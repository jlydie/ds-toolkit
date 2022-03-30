server = function(input, output, session) {
  
  ####### Global view #######
  
  data <- eventReactive(input$go, {
    inFile <- input$file
    if (is.null(inFile)) return(NULL)
    read.csv(inFile$datapath, 
             header = input$header,
             sep = input$sep)
  })

  output$fileSize <- renderText({ 
    paste("File size :",object.size(data()), "bytes") 
  })  
  
  output$colNum <- renderText({ 
    paste("Number of variables :", ncol(data())) 
  })
  
  output$rowNum <- renderText({ 
    paste("Number of observations :", nrow(data())) 
  })
  
  output$missingNum <- renderText({ 
    paste("Number of missing values :", sum(is.na(data()))) 
  })
  
  output$table <- renderDT({ data() },
                           options = list(scrollX = TRUE))
  
  ####### Data pre-processing - Data types #######
  
  data2 <- update_variables_server(
    id = "vars",
    data = reactive(data())
  )
  
  ####### Data pre-processing - Missing data #######
  
  output$plot_missing_vals_vim <- renderPlot({
    aggr(data2(), col=c('deepskyblue4','darkgoldenrod1'), numbers=TRUE, sortVars=TRUE, labels=names(data2()), 
         ylab=c("Histogram of missing data", "Pattern"))
  })
  
  url_mice <- a("mice (R package)", href="https://cran.r-project.org/web/packages/mice/mice.pdf")
  output$url_mice_ui <- renderUI({
    tagList("URL link:", url_mice)
  })
  
  imputed_data <- eventReactive(input$del_missing, {
    complete(mice(data2()[,1:length(data2())], m=5, seed = 123), 1)
  })
  
  
  ####### Data pre-processing - Normalization #######
  
  
  #normalize
  
  data3 <- eventReactive(input$normalize, {
    
    index_cols_norm = as.data.frame(which(sapply(imputed_data(), is.numeric)))[,1]

    if (input$select_method_normal == 1) {
      # standardization
      # norm1 = predict(preProcess(imputed_data()[,c(index_cols_norm)], method=c("center", "scale")), imputed_data()[,c(index_cols_norm)])
      # norm2 = subset(imputed_data(), select = -c(index_cols_norm))
      norm3 = cbind(subset(imputed_data(), select = -c(index_cols_norm)),
                    predict(preProcess(imputed_data()[,c(index_cols_norm)], method=c("center", "scale")), imputed_data()[,c(index_cols_norm)]))
    } else {
      # min-max
      # norm1 = predict(preProcess(imputed_data()[,c(index_cols_norm)], method=c("range")), imputed_data()[,c(index_cols_norm)])
      # norm2 = subset(imputed_data(), select = -c(index_cols_norm))
      norm3 = cbind(subset(imputed_data(), select = -c(index_cols_norm)), 
                    predict(preProcess(imputed_data()[,c(index_cols_norm)], method=c("range")), imputed_data()[,c(index_cols_norm)]))
    }
  })
  
  output$show_normalized <- renderPrint({
    #Sys.sleep(2)
    summary(data3())
  })
  
  
  ####### Data pre-processing - Imbalanced  Data #######
  
  output$chooseTarget_imbalanced <- renderUI({
    req(data3())
    selectInput("target_imb", "Target variable", names(data3()), selected = names(data3()[, c(1)]))
  })
  
  output$tab_target <- renderTable({
    req(data3())
    
    data.frame(
      Modality = names(table(data3()[[input$target_imb]])),
      Count = c(table(data3()[[input$target_imb]])),
      Proportion = paste(round(100 * prop.table(table(data3()[[input$target_imb]]))), "%"),
      row.names = NULL
    )
  })
  
  train <- reactive({
    req(data3())
    data3()[sort(sample(nrow(data3()), nrow(data3())*input$slider_train)),]
  })
  
  test <- reactive({
    #req(data3())
    data3()[-sort(sample(nrow(data3()), nrow(data3())*input$slider_train)),]
  })
  
  train2 <- eventReactive(input$balance, {
    train_target <- grep(input$target_imb, colnames(train()))
    if (input$select_method_imbalance == 1) {
      train2 <- downSample(x = subset(train(), select = - c(train_target)), y = train()[[input$target_imb]], yname = "class")
      
      
    } else if (input$select_method_imbalance == 2) {
      train2 <- upSample(x = subset(train(), select = - c(train_target)), y = train()[[input$target_imb]], yname = "class")
      
    } else {
      train2 <- ROSE(train()[[input$target_imb]] ~ ., data  = train())$data                       
    }
  })
  
  output$tab_target_after <- renderTable({
    req(train2())

    data.frame(
      Modality = names(table(train2()[[input$target_imb]])),
      Count = c(table(train2()[[input$target_imb]])),
      Proportion = paste(round(100 * prop.table(table(train2()[[input$target_imb]]))), "%"),
      row.names = NULL
    )
  })

  
  ####### Exploratory analysis : univariate ######
  
  data_stats <- reactive(imputed_data())

  # character vars
  
  observeEvent(data_stats(), updateSelectInput(session, "uni", 
                                          choices=append(colnames(data_stats()[ , unlist(lapply(data_stats(), is.factor))]),
                                                         colnames(data_stats()[ , unlist(lapply(data_stats(), is.character))]))))

  observeEvent(data_stats(), updateSelectInput(session, "uni2", choices=colnames(data_stats()[ , unlist(lapply(data_stats(), is.numeric))])))
  
  ## plot quali
  output$plot_uni_quali <- renderPlotly({
    p<- ggplot(data_stats(), aes(!!input$uni)) + geom_bar(fill="#9EB25D",alpha = 0.7, color = "#9EB25D") + theme_light()
    ggplotly(p)
  })
  
  ##table count and % 
  output$tab_eff <- function() {
    b<-data.frame(table(data_stats()[[input$uni]])) %>% merge(data.frame(round(prop.table(table(data_stats()[[input$uni]])),2)), by ="Var1")
    colnames(b)<- c("Modality", "Count", "%")
    
    b %>%
      kbl() %>%
      kable_styling(bootstrap_options = "bordered")  %>%
      column_spec(1, bold = T, width = "10em", color = "white",background = "#D7E2B2") %>%
      column_spec(2, width = "10em",background = "white")
  }
  
  ## plot quanti
  output$plot_uni_quanti <- renderPlotly({
    p<- ggplot(data_stats(), aes(y=!!input$uni2)) + geom_boxplot(fill="#900C3F",alpha = 0.5) + theme_light()
    ggplotly(p)
  })
  
  ##summary 
  output$summary <- function() {
    
    a<- data.frame(unclass(summary(data_stats()[[input$uni2]],digits=3)), check.names = FALSE, stringsAsFactors = FALSE)
    colnames(a) <- "Value"
    
    a %>%
      kbl() %>%
      kable_styling(bootstrap_options = "bordered") %>%
      column_spec(1, bold = T, width = "10em", color = "white",background = "#DC9AB4") %>%
      column_spec(2, width = "10em",background = "white")
  }
  
  ###################### Exploratory analysis : bivariate ###################### 
  ####### Analysis between two categorical variables #######
  
  observeEvent(data_stats(), updateSelectInput(session, "char_var_1", choices=append(colnames(data_stats()[ , unlist(lapply(data_stats(), is.factor))]),
                                                                                       colnames(data_stats()[ , unlist(lapply(data_stats(), is.character))]))))
  observeEvent(data_stats(), updateSelectInput(session, "char_var_2", choices=append(colnames(data_stats()[ , unlist(lapply(data_stats(), is.factor))]),
                                                                                       colnames(data_stats()[ , unlist(lapply(data_stats(), is.character))]))))
  
  ### Plot ###
  pltqual <- eventReactive(input$run_buttonqualqual, {
    plt<-ggplot(data = data_stats()) + 
      geom_mosaic(aes(x = product(!!sym(input$char_var_1)), fill = !!sym(input$char_var_2)), na.rm=TRUE) + theme_mosaic() +
      labs(x="First categorical variable", y = "Second categorical variable")
    ggplotly(plt)
    
  })
  
  output$plot_qualqual <- renderPlotly({
    pltqual()
  })
  
  ### Cross tab ###
  
  cross <- eventReactive(input$run_buttonqualqual, {
    as.data.frame.matrix(table(data_stats()[[input$char_var_1]], data_stats()[[input$char_var_2]]))
  })
  
  
  output$crosstab <- renderTable(cross(), striped=TRUE, bordered = TRUE,rownames = T)
  
  ### Cross tab row ###
  
  crossr <- eventReactive(input$run_buttonqualqual, {
    as.data.frame.matrix(lprop(table(data_stats()[[input$char_var_1]], data_stats()[[input$char_var_2]])))
  })
  
  
  output$crosstabr <- renderTable(crossr(), striped=TRUE, bordered = TRUE,rownames = T)
  
  ### Cross tab column###
  
  crossc <- eventReactive(input$run_buttonqualqual, {
    as.data.frame.matrix(cprop(table(data_stats()[[input$char_var_1]], data_stats()[[input$char_var_2]])))
  })
  
  
  output$crosstabc <- renderTable(crossc(), striped=TRUE, bordered = TRUE,rownames = T)
  
  ### Chi deux test###
  
  chid <- eventReactive(input$run_buttonqualqual, {
    chisq.test(table(data_stats()[[input$char_var_1]], data_stats()[[input$char_var_2]]),simulate.p.value = TRUE)
  })
  
  output$chideux <- renderPrint(
    chid()
    
  )
  
  ### Chi deux residuals ###
  
  
  chidr <- eventReactive(input$run_buttonqualqual, {
    as.data.frame.matrix(chisq.residuals(table(data_stats()[[input$char_var_1]], data_stats()[[input$char_var_2]])))
  })
  
  
  output$chideuxres <- renderTable(chidr(), striped=TRUE, bordered = TRUE,rownames = T)
  
  ####### Analysis between a quantitative variable and a categorical variable #######
  ### Plot ###
  
  observeEvent(data_stats(), updateSelectInput(session, "num_var", choices=colnames(data_stats()[ , unlist(lapply(data_stats(), is.numeric))])))
  observeEvent(data_stats(), updateSelectInput(session, "char_var", choices=append(colnames(data_stats()[ , unlist(lapply(data_stats(), is.factor))]),
                                                                                     colnames(data_stats()[ , unlist(lapply(data_stats(), is.character))]))))
  
  box <- eventReactive(input$run_buttonquanqual, {
    p<-  ggplot(data_stats(),
                aes(x = !!input$char_var, y = !!input$num_var)) + geom_boxplot(fill="#9EB25D",alpha = 0.5,na.rm = TRUE)+scale_x_discrete(na.translate = FALSE) + theme_light()
    
    ggplotly(p)
  })
  
  
  output$boxplot_quanqual <- renderPlotly({
    box()
  })
  
  ### TAPPLY ###
  
  tap <- eventReactive(input$run_buttonquanqual, {
    data.frame(Mean = tapply(data_stats()[[input$num_var]], data_stats()[[input$char_var]], mean,na.rm = TRUE),
               Variance = tapply(data_stats()[[input$num_var]], data_stats()[[input$char_var]], var,na.rm = TRUE),
               Q1 = tapply(data_stats()[[input$num_var]], data_stats()[[input$char_var]],quantile,probs=0.25,na.rm = TRUE),
               Median = tapply(data_stats()[[input$num_var]], data_stats()[[input$char_var]], median,na.rm = TRUE),
               Q3 = tapply(data_stats()[[input$num_var]], data_stats()[[input$char_var]],quantile,probs=0.75,na.rm = TRUE))
  })
  
  
  output$tapply <- renderTable(tap(), striped=TRUE, bordered = TRUE,rownames = T)
  
  
  
  ####### Analysis between two quantitative variables #######
  ### Plot ###
  observeEvent(data_stats(), updateSelectInput(session, "num_var_1", choices=colnames(data_stats()[ , unlist(lapply(data2(), is.numeric))])))
  observeEvent(data_stats(), updateSelectInput(session, "num_var_2", choices=colnames(data_stats()[ , unlist(lapply(data2(), is.numeric))])))
  
  plt <- eventReactive(input$run_buttonquanquan, {
    p<-  ggplot(data_stats(),
                aes_string(x = input$num_var_1, y = input$num_var_2)) + geom_point(color = "#9EB25D")+ theme_light()
    
    ggplotly(p)
  })
  
  
  output$plot_quanquan <- renderPlotly({
    plt()
  })
  
  ### Pearson ###
  
  pears <- eventReactive(input$run_buttonquanquan, {
    cor.test(
      data_stats()[[input$num_var_1]],
      data_stats()[[(input$num_var_2)]],method="pearson")
  })
  
  output$combined_summary_table <- renderPrint(
    pears()
  )
  
  
  ####### Logistic Regression #######
  
  train_target <- reactive({
    train_target <- noquote(input$target_imb)
  })
  
  output$test <- renderText(
    train_target()
  )
  
  # reg_log_full_model <- eventReactive(input$run_reg_log, {
  #   req(train2())
  #   glm(input$target_train~. , train2(), family = input$sel_reg_log_family)
  # })
  
  reg_log_full_model <- reactive({
    req(train2())
    glm(class~. , train2(), family = "binomial") #input$sel_reg_log_family
  })
  
  glm_model <- eventReactive(input$activate_reg_log, {
    
    if (input$select_log_reg == "backward") {
      glm_model <- stepAIC(reg_log_full_model(), direction = "backward", trace = FALSE)
      
    } else if (input$select_log_reg == "forward") {
      glm_model <- stepAIC(reg_log_full_model(), direction = "forward", trace = FALSE)
      
    } else if (input$select_log_reg == "both") {
      glm_model <- stepAIC(reg_log_full_model(), direction = "both", trace = FALSE)
      
    } else {
      glm_model <- reg_log_full_model()                   
    }
  })
  
  output$summary_reglog <- renderPrint({
    summary(glm_model())
  })
  
  output$plot_roc <- renderPlot({
    glm.probs <- predict(glm_model(), newdata = test(), type='response')
    rocit_obj <- rocit(score = glm.probs, class = test()$class)
    plot(rocit_obj)
  })
  
  glm.probs <- reactive({
    req(test())
    glm_model() %>% predict(test(), type = "response")
  })
  
  predicted.classes = reactive(as.factor(ifelse(glm.probs() > 0.5, 2, 1)))
  exp_reg_log = reactive(as.factor(test()$class))
  
  output$cf_reg_log <- renderPrint({
    req(test())
    caret::confusionMatrix(predicted.classes(), exp_reg_log())
  })
  
  output$var_imp_reg_log <- renderPlot({
    V = caret::varImp(glm_model())
    ggplot(V, aes(x=reorder(rownames(V),Overall), y=Overall)) +
      geom_point(color="blue", size=4, alpha=0.6)+
      geom_segment( aes(x=rownames(V), xend=rownames(V), y=0, yend=Overall), color='blue') +
      xlab('Variables')+
      ylab('Overall Importance')+
      theme_light() +
      coord_flip() 
  })
  
  ####### SVM  #######
  #### SVM confusion matrix ###
  
  output$svmacc <- renderPrint({
    classifierL = svm(train2()$class ~ ., data = train2(), kernel = input$noyau, cost=input$cost)
    ypred=predict(classifierL,test())
    confusionMatrix(ypred,as.factor(test()$class))  
  })
  
  #### SVM confusion matrix ###

  output$svmind <- renderTable({
    classifierL2 = svm(class ~ ., data = train2(), kernel = input$noyau, cost=input$cost)
    ypred2=predict(classifierL2,test())
    matconf=table(ypred2,test()$class)
    matconf
    #prec=matconf[1,1]/sum(matconf[1,1:2])
    #recall=matconf[1,1]/sum(matconf[1:2,1])
    #fscore=2*recall*prec / (recall+prec )
    #Precision <- c(prec)
    #Recall <- c(recall)
    #Fscore <- c(fscore)
    #dat=data.frame(Precision, Recall, Fscore)
    #dat
  })
  
  
  ### Decision trees 
  
  control <- reactive(rpart.control(minsplit = input$minsplit))

  rpart_model <- reactive(rpart(class~., data = train(), method = "class", parms = list(split=input$critere),
                      control=control()))
  
  output$decision_tree <- renderPlot({
    pruned_model <- prune(rpart_model(), input$cp)
    rpart.plot(pruned_model)
  })
  

  typeColNum <- reactive(grep("class", names(train())))
  
  
  output$accuracy_DT <- renderPrint({

    rpart_predict <- predict(rpart_model(), test()[, -typeColNum()], type = "class")
    confusionMatrix(rpart_predict, test()$class)

  })
  

  ####### Random Forest #######
  
  model_rf <- reactive({
    req(train2())
    metric = "Accuracy"
    mtry <- sqrt(ncol(train2()[,-1]))
    tunegrid <- expand.grid(.mtry=mtry)
    control <- trainControl(method="repeatedcv", number=input$number, repeats=input$repeats)
    rf_caret <- caret::train(train2()$class~., data=train2(), method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
  })
  
  prediction_rf <- reactive( predict(model_rf(), test()) ) 
  exp_rf = reactive(as.factor(test()$class))
  
  output$mat_conf_rf <- renderPrint({
    req(test())
    caret::confusionMatrix(prediction_rf(), exp_rf())
  }) 
  
  output$var_imp_rf <- renderPlot({
    V = caret::varImp(model_rf())$importance
    ggplot(V, aes(x=reorder(rownames(V),Overall), y=Overall)) +
      geom_point(color="purple", size=4, alpha=0.6)+
      geom_segment(aes(x=rownames(V), xend=rownames(V), y=0, yend=Overall), 
                   color='purple') +
      xlab('Variables')+
      ylab('Overall Importance')+
      theme_light() +
      coord_flip() 
  })
    
}
