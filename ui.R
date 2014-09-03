
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(pageWithSidebar(  
  headerPanel("Linear Regression Analysis"),
  sidebarPanel(
    conditionalPanel(condition="input.conditionedPanels == 'Data'",       
                     wellPanel(           
                       h4("Uploading Files"),
                       fileInput('file1', 'Choose a File',
                                 accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv', '.RData', '.rdata')),
                       radioButtons('type', 'File type:', c(csv='csv', delim='delim', RData='rdat'), inline=TRUE),
                       textInput("missing",  "In uploaded file missing value is:", value = ""),
                       selectInput("datainfo",  h4("Data summary by:"), c("structure", "summary")),
                       numericInput("obs", h4("Number of observations to view:"), 10),
                       h4("Manipulate"),
                       wellPanel(tabsetPanel(tabPanel("Variable", value = 11,
                                                      uiOutput("datafactor_ui"),
                                                      uiOutput("datacalcu_ui")),
                                             tabPanel("Data", value = 12,
                                                      h5("Data reshape"),
                                                      radioButtons("rsformat", "Format to", c("Long" = "long", "Wide" = "wide"), inline=TRUE),
                                                      #textInput("rsdid",  "Identifier variables:", value = ""),
                                                      uiOutput("datareshapeIDvar_ui"),
                                                      helpText("Variable names or column numbers in the data frame."),
                                                      textInput("rsdvar",  "Time Variable name:", value = ""),
                                                      helpText("e.g. time, day, week, year..."),
                                                      textInput("rsdval",  "Response variable name:", value = ""),
                                                      checkboxInput("rsdna", "Remove missing?", value = FALSE),
                                                      helpText(a("Click Here to get more help", href="http://seananderson.ca/2013/10/19/reshape.html")),
                                                      actionButton("reshaperun", "Run")                                                      
                                             ),                                                                                                    
                                             id = "manipulatePanel"),
                                 downloadButton('savedata', 'Save Updated Data')
                       )
                     )
    ), # end of conditional panel data
    
    conditionalPanel(condition="input.conditionedPanels == 'EDA'", 
                     wellPanel(
                       h4("Exploratory Data Analysis"),
                       checkboxInput("hideDataInfo1", "Hide data info?", value = FALSE),
                       tabsetPanel(tabPanel("Plot", value = 13, 
                                            selectInput("plottype1", "Plot Type", c("Scatter, Box or Summary", "Histogram", "Matrix")),
                                            uiOutput("edaplot_ui")),             
                                   tabPanel("Tabulate", value = 14,
                                            uiOutput("edatabulate_ui")),                                    
                                   id = "EDAPanel"))                       
    ), # end of conditional panel EDA
    
    conditionalPanel(condition="input.conditionedPanels == 'Model'",
                     h4("Regression Model"),
                     checkboxInput("hideDataInfo2", "Hide data info?", value = FALSE),
                     #"Data subset",
                     #uiOutput("subset_ui"),
                     selectInput("modletype", "Model Type", c("lm", "glm", "lme, lmer, gls", "glmer")),
                     uiOutput("model_ui"),
                     uiOutput("subset_ui")
    ), # end of conditional panel Model
    
    conditionalPanel(condition="input.conditionedPanels == 'Model Comparison'",
                     h4("Model Comparison"),
                     uiOutput("modelcomparison_ui")
    ), # end of conditional panel Model Comparison
    
    conditionalPanel(condition="input.conditionedPanels == 'Predicted Means'",
                     h4("Predicted Means"),
                     uiOutput("predictmean_ui")
    ), # end of conditional panel Predicted Means
    
    h4("Download knitr Reports"),
    radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'), inline = TRUE), 
    downloadButton('downloadReport')
    
  ),# end of sidebarPanel
  
  mainPanel(
    tabsetPanel(
      tabPanel("Data", 
               h4("Data Information"),
               verbatimTextOutput("datastr"),           
               h4("Data View"),
               dataTableOutput(outputId="view")
      ), # end of Data tab panel
      
      tabPanel("EDA",
               h4("Data Information"),
               verbatimTextOutput("datastr1"),
               
               tabsetPanel(type="pills", 
                           tabPanel("Data Plot", value = 15,
                                    plotOutput('dataplotout')),
                           tabPanel("Tabulate Output", value = 16,
                                    verbatimTextOutput("tabu")),                     
                           id = "edatabsetPanel")
      ), # end of EDA tab panel
      
      tabPanel("Model",
               h4("Data Information"),
               verbatimTextOutput("datastr2"),
               tabsetPanel(type="pills", 
                           tabPanel("Modeling Results", value = 17,
                                    h4("ANOVA"),
                                    tableOutput("anova"),
                                    h4("Model Summary"),
                                    verbatimTextOutput("summary"),
                                    h4("Confidence interval (95%)"),
                                    verbatimTextOutput("confint"),
                                    h4("Drop one"),
                                    verbatimTextOutput("drop1")),
                           tabPanel("Residual Plots", value = 18,
                                    plotOutput("residualplot")),                     
                           id = "modeltabsetPanel")                              
      ), # end of Model tab panel
      
      tabPanel("Model Comparison",
               h4("Model Comparison"),
               verbatimTextOutput("anova2"),
               h4("Model summary for a selected model"),
               verbatimTextOutput("summary1"), 
               h4("ANOVA for a selected nodel"),
               tableOutput("anova1"),
               h4("Residual plot for a selected model"), 
               plotOutput("residualplot1")                                                 
      ), # end of ModelComparison tab panel
      
      tabPanel("Predicted Means",
               tabsetPanel(type="pills", tabPanel("Predicted Means", value = 19,
                                                  verbatimTextOutput("predictm"),
                                                  plotOutput('predictmeansmplot')),
                           tabPanel("Multiple Comparisons", value = 110,
                                    verbatimTextOutput("predictmcp"),
                                    plotOutput('predictmeansPMplot')),
                           tabPanel("Covariate Means Plot", value = 19,
                                    plotOutput('predictmeanscovplot')),
                           id = "predictmeanoutPanel")
      ),# end of Predicted Means tab panel
      
      id = "conditionedPanels"                
    )# end of tabset panel
  ) # end of main panel 
))
Enter file contents here
