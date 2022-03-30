library(shiny)
library(shinythemes)
library(DT)
library(datamods)
library(plotly)
library(ggthemes)
library(data.table)
library(skimr)
library(shinycssloaders)
library(ggmosaic)
library(questionr)
library(dplyr)
library(kableExtra)
library(mice) # missing data
library(VIM) # missing data plot
library(caret)
library(ROSE) # ROSE subsampling
# Log Reg
library(MASS)
library(ROCR) # ROC curve
library(ROCit)
# SVM
library(e1071)
# DT
library(rpart)
library(rpart.plot)
# RF
library(randomForest)
library(mlbench)

library(reactlog)
reactlog_enable()
#shiny::reactlogShow() only for testing, once app has closed run in the console

navbarPage("DS ToolKit", theme = shinytheme("flatly"),
           navbarMenu("Data",
                      tabPanel("Global View",
                               sidebarPanel(
                                 # radioButtons("file_choice", label = "File Choice",
                                 #              choiceNames = c("Own dataset",
                                 #                              "Test dataset (Hepatitis)"),
                                 #              choiceValues = c(
                                 #                "own_file", "test_data")
                                 #              ),
                                 fileInput(inputId = "file", label = "Upload a CSV or text file",
                                           accept = c("text/csv", 
                                                      "text/comma-separated-values",
                                                      "text/plain",
                                                      ".csv")
                                 ),
                                 checkboxInput("header", "with headers", TRUE),
                                 radioButtons("sep", "Separator",
                                              choices = c(Comma = ",",
                                                          Semicolon = ";",
                                                          Tab = "\t"),
                                              selected = ",",
                                              inline = TRUE),
                                 actionButton(inputId = "go", label = "Load"),
                                 br(),
                                 HTML('<center><img src="med-circle.png" height = 150, width = 150></center>'),
                                 br(),
                                 h4("File informations"),
                                 textOutput("fileSize"),
                                 textOutput("colNum"),
                                 textOutput("rowNum"),
                                 textOutput("missingNum")
                               ),
                               mainPanel(
                                 DTOutput("table")%>% withSpinner(color = "#21857c")
                               )
                      ),
                      tabPanel("Data Pre-processing",
                               sidebarPanel(width = 3,
                                 tags$div(
                                   tags$p("In this tab, you will be able to :"),
                                   tags$ul(
                                     tags$li("Change the already existing data types,"), 
                                     tags$li("Deal with missing data by imputing suitable values regarding the data type and its distribution,"), 
                                     tags$li("Handle outliers,"),
                                     tags$li("Normalize by adjusting the scale of a variable to a standard one,"),
                                     tags$li("Imbalanced Dataset")
                                   )
                                 ),
                                 tags$div(
                                   tags$p("Please make sure to navigate through the tabs on the left in the right order. The opposite will occur the app to crash and therefore can not be used to its full purpose."),
                                   tags$b("Data types > Missing Data > Outliers > Normalisation > Imbalanced Dataset")
                                 ),
                                 br(),
                                 tags$div(
                                   sliderInput("slider_train", label = "Choose the train set proportion (the remaining part will be used for testing).", min = 0.1, 
                                               max = 0.9, value = 0.6)
                                 )
                               ),
                               mainPanel(
                                 tabsetPanel(
                                   tabPanel("Data types",
                                            br(),
                                            update_variables_ui("vars")
                                   ),
                                   tabPanel("Missing Data",
                                            br(),
                                            h4("Overview of the missing data"),
                                            p("Missing data will be imputed using mice package."),
                                            uiOutput("url_mice_ui"),
                                            br(),
                                            plotOutput("plot_missing_vals_vim"),
                                            br(),
                                            actionButton(inputId = "del_missing", label = "Process Missing Values")
                                   ),
                                   tabPanel("Normalisation",
                                            br(),
                                            selectInput("select_method_normal", 
                                                        label = "Select the method for normalization",
                                                        choices = list("Standardization (Z-Score)" = 1, 
                                                                       "Min-max scaling" = 2),
                                                        selected = 1
                                                        ),
                                            actionButton(inputId = "normalize", label = "Normalize Data"),
                                            br(),
                                            br(),
                                            verbatimTextOutput("show_normalized")
                                            ),
                                   tabPanel("Imbalanced Dataset",
                                            br(),
                                            tags$p("Verify if your data is imbalanced or not."),
                                            fluidRow(
                                              column(4,
                                                     uiOutput("chooseTarget_imbalanced")
                                                     ),
                                              column(5,
                                                     tableOutput("tab_target")
                                                     )
                                            ),
                                            br(),
                                            fluidRow(
                                              column(4,
                                                     selectInput("select_method_imbalance", 
                                                                 label = "Select the method to balance proportion of your target variable in the train set",
                                                                 choices = list("Undersampling" = 1, 
                                                                                "Oversampling" = 2,
                                                                                "ROSE" = 3
                                                                 )),
                                                     actionButton(inputId = "balance", label = "Balance Data")
                                              ),
                                              column(5,
                                                     tableOutput("tab_target_after")
                                              )
                                            ),
                                            br(),
                                            #uiOutput("chooseTarget_train")
                                            )
                                   )
                               )
                      )
           ),
           navbarMenu("Exploratory analysis",
                      tabPanel("Univariate analysis",
                               fluidRow(
                                 column(6,
                                        varSelectInput('uni','Select a categorical variable',data()),
                                        tabsetPanel(type = "tabs",
                                                    tabPanel("Plot", plotlyOutput('plot_uni_quali')%>% withSpinner(color = "#21857c")),
                                                    tabPanel("Table", tableOutput("tab_eff")%>% withSpinner(color = "#21857c"))
                                        ),
                                 ),
                                 column(6,
                                        varSelectInput('uni2','Select a numeric variable',data()),
                                        tabsetPanel(type = "tabs",
                                                    tabPanel("Plot", plotlyOutput('plot_uni_quanti')%>%withSpinner(color = "#21857c")),
                                                    tabPanel("Summary", tableOutput("summary")%>%withSpinner(color = "#21857c"))
                                        )
                                 )
                               )
                      ),
                      "----",
                      'Bivariate analysis', 
                      tabPanel("Quali-Quali",
                               titlePanel("Analysis between two categorical variables"),
                               br(),
                               sidebarLayout(
                                 sidebarPanel(
                                   br(),
                                   varSelectInput('char_var_1','Select a first categorical variable',data()),
                                   br(),
                                   br(),
                                   varSelectInput('char_var_2','Select a second categorical variable',data()),
                                   br(),
                                   br(),
                                   actionButton("run_buttonqualqual", "Run Analysis", icon = icon("play")),
                                   br(),
                                   br(),
                                   br(),
                                 ),
                                 mainPanel(
                                   tabsetPanel(
                                     tabPanel(
                                       title = "Plot",
                                       br(),
                                       fluidRow(
                                         column(width = 12, strong("MOSAIC PLOT"))
                                       ),
                                       br(),
                                       plotlyOutput("plot_qualqual")
                                     ),
                                     tabPanel(
                                       title = "Cross-table",
                                       br(),
                                       fluidRow(
                                         column(width = 3, strong("CROSS TABLE")),
                                         column(width = 5, strong("CROSS TABLE - ROW PERCENTAGE")),
                                         column(width = 4, strong("CROSS TABLE - COLUMN PERCENTAGE")),
                                         
                                       ),
                                       br(),
                                       fluidRow(
                                         column(width = 3, tableOutput("crosstab")),
                                         column(width = 5, tableOutput("crosstabr")),
                                         column(width = 4, tableOutput("crosstabc")),
                                       ) 
                                     ),
                                     tabPanel(
                                       title = "Chi-squared test",
                                       br(),
                                       fluidRow(
                                         column(width = 9, strong("CHI SQUARED TEST")),
                                         column(width = 3, strong("CHI SQUARED RESIDUALS"))
                                         
                                       ),
                                       br(),
                                       fluidRow(
                                         column(width = 9, verbatimTextOutput("chideux")),
                                         column(width = 3, tableOutput("chideuxres")),
                                         
                                       )
                                     ),
                                     
                                   )
                                 )
                               )
                      ),
                      tabPanel("Quali-Quanti",
                               titlePanel("Analysis between a quantitative variable and a categorical variable "),
                               br(),
                               sidebarLayout(
                                 sidebarPanel(
                                   br(),
                                   varSelectInput('num_var','Select a numeric variable',data()),
                                   br(),
                                   br(),
                                   varSelectInput('char_var','Select a categorical variable',data()),
                                   br(),
                                   br(),
                                   actionButton("run_buttonquanqual", "Run Analysis", icon = icon("play")),
                                   br(),
                                   br(),
                                   br(),
                                 ),
                                 mainPanel(
                                   tabsetPanel(
                                     tabPanel(
                                       title = "Plot",
                                       br(),
                                       fluidRow(
                                         column(width = 12, strong("BOXPLOT"))
                                       ),
                                       plotlyOutput("boxplot_quanqual")
                                     ),
                                     tabPanel(
                                       title = "Indicators",
                                       br(),
                                       fluidRow(
                                         column(width = 12, strong("TAPPLY FUNCTION"))
                                       ),
                                       br(),
                                       fluidRow(
                                         column(width = 12,tableOutput("tapply"))
                                       )
                                     )
                                   )
                                 )
                               )
                      ),
                      tabPanel("Quanti-Quanti",
                               titlePanel("Analysis between two quantitative variables"),
                               br(),
                               sidebarLayout(
                                 sidebarPanel(
                                   br(),
                                   varSelectInput('num_var_1','Select a first numeric variable',data()),
                                   br(),
                                   br(),
                                   varSelectInput('num_var_2','Select a second numeric variable',data()),
                                   br(),
                                   br(),
                                   actionButton("run_buttonquanquan", "Run Analysis", icon = icon("play")),
                                   br(),
                                   br(),
                                   br(),
                                 ),
                                 mainPanel(
                                   tabsetPanel(
                                     tabPanel(
                                       title = "Plot",
                                       br(),
                                       fluidRow(
                                         column(width = 12, strong("SCATTER PLOT")
                                                )
                                       ),
                                       plotlyOutput("plot_quanquan")
                                     ),
                                     tabPanel(
                                       title = "Indicators",
                                       br(),
                                       fluidRow(
                                         column(width = 12, strong("PEARSON CORRELATION COEFFICIENT")
                                                )
                                       ),
                                       br(),
                                       fluidRow(
                                         column(width = 12,
                                                verbatimTextOutput("combined_summary_table")
                                                )
                                       )
                                     )
                                   )
                                 )
                               )
                      )
           ),
           
           navbarMenu("Supervised Classification",
                      tabPanel("Logistic Regression",
                               sidebarPanel(width = 3,
                                            
                                            textOutput("test"),
                                            
                                            # selectInput("sel_reg_log_family", 
                                            #             label = "Select the family for logistic regression",
                                            #             choices = list("backward",
                                            #                            "forward",
                                            #                            "both",
                                            #                            "none")
                                            # ),
                                            # textInput("sel_reg_log_family", 
                                            #           label = "Write the family to use for logistic regression"),
                                            # p('Example : binomial(link = "logit")
                                            #   gaussian(link = "identity")'),
                                            #actionButton(inputId = "run_reg_log", label = "Run"),
                                            # br(),
                                            # br(),
                                 selectInput("select_log_reg", 
                                             label = "Select the method for variable selection by AIC",
                                             choices = list("backward",
                                                            "forward",
                                                            "both",
                                                            "none")
                                 ),
                                 p("if none is chosen, all variables will be included in the model."),
                                 br(),
                                 actionButton(inputId = "activate_reg_log", label = "Update Model")
                               ),
                               mainPanel(
                                 tabsetPanel(
                                   tabPanel("Model Summary",
                                            verbatimTextOutput("summary_reglog")
                                   ),
                                   tabPanel("ROC Curve & other metrics",
                                            h4("ROC Curve"),
                                            plotOutput('plot_roc'),
                                            verbatimTextOutput("cf_reg_log"),
                                            plotOutput("var_imp_reg_log")
                                            )
                                 )
                               )
                      ), 
                      tabPanel("SVM",
                               sidebarPanel(
                                 sliderInput("cost", "Cost :",
                                             min = 0, max = 20, value = 0.5
                                 ),
                                 br(),
                                 radioButtons("noyau", label = "Kernel :",
                                              choices = list("linear" = "linear", "radial" = "radial"), 
                                              selected = "gini")
                               ), 
                               mainPanel(
                                 
                                 tabsetPanel(type = "tabs",
                                             tabPanel("Confusion Matrix", verbatimTextOutput("svmacc")),
                                             tabPanel("Indicators", tableOutput("svmind"))
                                 )
                                 
                               )
                      ), 
                      tabPanel("Decision Trees", 
                               
                               sidebarPanel(
                                 sliderInput("cp", "Complexity adjustement :",
                                             min = 0, max = 1, value = 0.01
                                 ),
                                 
                                 sliderInput("minsplit", "Minsplit :",
                                             min = 0, max = 20, value = 5
                                 ),
                                 
                                 radioButtons("critere", label = "Splitting index",
                                              choices = list("gini" = "gini", "information" = "information"), 
                                              selected = "gini")
                               ), 
                               mainPanel(
                                 
                                 tabsetPanel(type = "tabs",
                                             tabPanel("Plot", plotOutput("decision_tree")%>% withSpinner(color = "#21857c")),
                                             tabPanel("Evaluation",verbatimTextOutput("accuracy_DT"))
                                             )
                                 
                                 )
                               ),
                      tabPanel("Random Forests",
                               sidebarPanel(
                                 h3('Tuning'),
                                 
                                 h4("Cross Validation parameters"),
                                 sliderInput("number", "Number of K-fold :",
                                             min = 1, max = 10, value = 5
                                 ),
                                 sliderInput("repeats", "Repeating (x times) :",
                                             min = 1, max = 10, value = 3
                                 )
                               ),
                               mainPanel(
                                 tabsetPanel(
                                   tabPanel("Metrics",
                                            verbatimTextOutput("mat_conf_rf")
                                   ),
                                   tabPanel("Variable Importance",
                                            plotOutput("var_imp_rf")
                                   )
                                 )
                               )
                      )
           )
           
)
