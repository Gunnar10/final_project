library(shiny)
library(shinydashboard)
# Define UI for application


dashboardPage(
  dashboardHeader(title = "Fish App"),
  dashboardSidebar(
    sidebarMenu(
                menuItem("About", tabName = "about"),
                
                menuItem("Data Exploration", tabName = "data"),
                
                menuItem("Modeling", tabName = "model", startExpanded = FALSE,
                         menuSubItem("Modeling Info", tabName = "infoModel"),
                         
                         menuSubItem("Model Fitting", tabName = "fitModel"),
                         
                         menuSubItem("Prediction", tabName = "predict")
                         )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "about",
              fluidPage(
                box(h1("This is the about tab."))
              )
      ),
      tabItem(tabName = "data",
              fluidRow(
                box(radioButtons(inputId = "plotType", 
                                 label = "Select the Type of Plot.",
                                 choiceNames = c("Violin Plot", "Histogram", "Scatter Plot", "Stacked Bar"),
                                 choiceValues = c("violin", "hist", "scatter", "bar")),
                    selectInput(inputId = "fish",
                                label = "Fish Type",
                                choices = c("DOLPHIN", "WAHOO", "KING MACKEREL", 
                                            "SPANISH MACKEREL", "GROUPER", "SPOT",
                                            "COBIA", "RED SNAPPER")
                                ),
                    conditionalPanel(condition = "input.plotType == 'violin'",
                                     checkboxInput("reg", h5("Add Region to Plot"))
                                     ),
                    conditionalPanel(condition = "input.plotType == 'hist'",
                                     sliderInput("bin", "Bin Width", 
                                                 min = 0,
                                                 max = 1,
                                                 value = 0.5)
                                     ),
                    conditionalPanel(condition = "input.plotType == 'scatter'",
                                     checkboxInput("area", h5("Add Area to Plot"))
                                     )
                    ),
                box(plotOutput("plot"))
              ),
              fluidRow(
                box(radioButtons(inputId = "sumType", 
                                 label = "Select the Type of Summary.",
                                 choiceNames = c("Mean", "Median", "Minimum", "Maximum"),
                                 choiceValues = c("mean", "median", "min", "max")),
                    selectInput(inputId = "var",
                                label = "Select Variable",
                                choices = c("kg", "cm")),
                    selectInput(inputId = "group",
                                label = "Select Grouping",
                                choices = c("subReg", "area"))
                  ),
                box(
                  dataTableOutput("table")
                )
              )
      ),
      tabItem(tabName = "infoModel",
              fluidRow(
                box(h1("This is the modeling information tab."))
              )
      ),
      tabItem(tabName = "fitModel",
                fluidRow(
                  box(numericInput(inputId = "trainSplit",
                                  label = "Specify the Training Data Percentage",
                                  min = 0.01,
                                  max = 0.99,
                                  value = 0.5
                                  ),
                      selectInput(inputId = "regVars",
                                  label = "Select the Linear Model's Predictor Variables.",
                                  choices = c("name", "subReg",
                                              "area", "cm", "month"),
                                  multiple = TRUE
                                    ),
                      actionButton(inputId = "train", label = "Train the Model")
                      ),
                  box(verbatimTextOutput("regTestResults"))
                ),
              
                fluidRow(
                  box(numericInput(inputId = "trainSplitRF",
                                   label = "Specify the Training Data Percentage",
                                   min = 0.01,
                                   max = 0.99,
                                   value = 0.5),
                      selectInput(inputId = "rfVars",
                                    label = "Select the Random Forest's Predictor Variables.",
                                    choices = c("subReg", "area",
                                                "kg", "cm", "month"),
                                    multiple = TRUE),
                      actionButton(inputId = "trainRF", label = "Train the Model")
                  ),
                  box(verbatimTextOutput("rfModel"),
                      plotOutput("rfPlot")
                      )
                ),
      ),
      tabItem(tabName = "predict",
              fluidRow(
                box(h1("This is the modeling prediction tab."))
              )
      )
    )
  )
)


