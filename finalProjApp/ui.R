#Load packages
library(shiny)
library(shinydashboard)
library(tidyverse)

#read in the Data
data_1 <- read_csv("size_20221.csv")
data_2 <- read_csv("size_20222.csv")
data_3 <- read_csv("size_20223.csv")
data_4 <- read_csv("size_20224.csv")
data_5 <- read_csv("size_20225.csv")
data_6 <- read_csv("size_20226.csv")

#Clean the data
data_1 <- data_1 %>% 
  select(common, SUB_REG, AREA_X, WGT, wgt_imp, LNGTH, lngth_imp, WAVE)

data_2 <- data_2 %>% 
  select(common, SUB_REG, AREA_X, WGT, wgt_imp, LNGTH, lngth_imp, WAVE)

data_3 <- data_3 %>% 
  select(common, SUB_REG, AREA_X, WGT, wgt_imp, LNGTH, lngth_imp, WAVE)

data_4 <- data_4 %>% 
  select(common, SUB_REG, AREA_X, WGT, wgt_imp, LNGTH, lngth_imp, WAVE)

data_5 <- data_5 %>% 
  select(common, SUB_REG, AREA_X, WGT, wgt_imp, LNGTH, lngth_imp, WAVE)

data_6 <- data_6 %>% 
  select(common, SUB_REG, AREA_X, WGT, wgt_imp, LNGTH, lngth_imp, WAVE)

fishData <- rbind(data_1, data_2, data_3, data_4, data_5, data_6) %>%
  filter(common %in% c("DOLPHIN", "WAHOO", "KING MACKEREL", 
                       "SPANISH MACKEREL", "GAG", "BLACK GROUPER", 
                       "RED GROUPER", "SPOT", "COBIA", "RED SNAPPER") & 
           wgt_imp == 0 & 
           lngth_imp == 0)

fishData <- fishData %>%
  transmute(name = as.factor(case_when(common == "GAG" ~ "GROUPER",
                                       common == "BLACK GROUPER" ~ "GROUPER",
                                       common == "RED GROUPER" ~ "GROUPER",
                                       common == "DOLPHIN" ~ "DOLPHIN",
                                       common == "WAHOO" ~ "WAHOO",
                                       common == "KING MACKEREL" ~ "KING MACKEREL",
                                       common == "SPANISH MACKEREL" ~ "SPANISH MACKEREL",
                                       common == "SPOT" ~ "SPOT",
                                       common == "COBIA" ~ "COBIA",
                                       common == "RED SNAPPER" ~ "RED SNAPPER")),
            
            subReg = as.factor(case_when(SUB_REG == 4 ~ "north atlantic",
                                         SUB_REG == 5 ~ "mid atlantic",
                                         SUB_REG == 6 ~ "south atlantic",
                                         SUB_REG == 7 ~ "gulf")),
            
            area = as.factor(case_when(AREA_X == 1 ~ "inshore",
                                       AREA_X == 2 ~ "offshore",
                                       AREA_X == 3 ~ "inshore",
                                       AREA_X == 4 ~ "offshore",
                                       AREA_X == 5 ~ "inland")),
            kg = WGT, 
            cm = round(LNGTH / 10, 0),
            month = as.factor(case_when(WAVE == 1 ~ "jan/feb",
                                        WAVE == 2 ~ "march/april",
                                        WAVE == 3 ~ "may/june",
                                        WAVE == 4 ~ "july/aug",
                                        WAVE == 5 ~ "sept/oct",
                                        WAVE == 6 ~ "nov/dec"))
  ) %>% drop_na()

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
                  box(sliderInput(inputId = "trainSplit",
                                  label = "Specify the Training Data Percentage",
                                  min = 0.10,
                                  max = 0.90,
                                  step = 0.10,
                                  value = 0.1
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
                  box(sliderInput(inputId = "trainSplitRF",
                                   label = "Specify the Training Data Percentage",
                                   min = 0.10,
                                   max = 0.90,
                                   step = 0.10,
                                   value = 0.1),
                      
                      selectInput(inputId = "rfVars",
                                    label = "Select the Random Forest's Predictor Variables.",
                                    choices = c("subReg", "area",
                                                "kg", "cm", "month"),
                                    multiple = TRUE),
                      
                      sliderInput(inputId = "mtry",
                                   label = "Specify the Tune Grid Parameter",
                                   min = 2,
                                   max = 10,
                                   step = 1,
                                   value = 2),
                      
                      sliderInput(inputId = "fold",
                                   label = "Specify the Number of Folds",
                                   min = 2,
                                   max = 10,
                                   step = 1,
                                   value = 2),
                      
                      actionButton(inputId = "trainRF", label = "Train the Model")
                  ),
                  
                  box(verbatimTextOutput("rfModel"),
                      plotOutput("rfPlot")
                      )
                  )
              ),
      
      tabItem(tabName = "predict",
              fluidRow(
                box("Select the input variables for the Linear Model",
                    selectInput(inputId = "namePred",
                                label = "Fish Type",
                                choices = unique(fishData$name)
                                ),
                    selectInput(inputId = "subRegPred",
                                label = "Sub Region",
                                choices = unique(fishData$subReg)
                                ),
                    selectInput(inputId = "areaPred",
                                label = "Area",
                                choices = unique(fishData$area)
                                ),
                    selectInput(inputId = "cmPred",
                                label = "Lenght in cm",
                                choices = unique(fishData$cm)
                                ),
                    selectInput(inputId = "monthPred",
                                label = "Months",
                                choices = unique(fishData$month)
                                ),
                    actionButton("lmPred", "Predict Fish Weight")
                    ),
                box("Select the input values for the Random Forest",
                    selectInput(inputId = "subRegPredRf",
                                label = "Sub Region",
                                choices = unique(fishData$subReg)
                    ),
                    selectInput(inputId = "areaPredRf",
                                label = "Area",
                                choices = unique(fishData$area)
                    ),
                    selectInput(inputId = "kgPredRf",
                                label = "Weight in kg",
                                choices = unique(fishData$kg)
                                ),
                    selectInput(inputId = "cmPredRf",
                                label = "Lenght in cm",
                                choices = unique(fishData$cm)
                    ),
                    selectInput(inputId = "monthPredRf",
                                label = "Months",
                                choices = unique(fishData$month)
                    ),
                    actionButton("rfPred", "Predict Fish Type")
                    ),
                box(verbatimTextOutput("linearPredOutput"),
                    verbatimTextOutput("rfPredOutput")
                    )
                )
              )
      )
  )
)

