#Load packages
library(shiny)
library(shinydashboard)
library(tidyverse)

#read in the Data
data_1 <- read_csv("size_20221.csv", show_col_types = FALSE)
data_2 <- read_csv("size_20222.csv", show_col_types = FALSE)
data_3 <- read_csv("size_20223.csv", show_col_types = FALSE)
data_4 <- read_csv("size_20224.csv", show_col_types = FALSE)
data_5 <- read_csv("size_20225.csv", show_col_types = FALSE)
data_6 <- read_csv("size_20226.csv", show_col_types = FALSE)

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
  dashboardHeader(title = "Fish Prediction App"),
  dashboardSidebar(
    #Create Tabs.
    sidebarMenu(
                menuItem("About", tabName = "about"),
                
                menuItem("Data Exploration", tabName = "data"),
                #create subtabs.
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
                img(src = "640x427-Mahimahi-NOAAFisheries.png", height = 600, width = 600),
                h1("About"),
                p("With this app, you can model the weight of a fish in kg with a linear model, and you predict the type of fish caught with a random   forest model. On the data exploration page you can view and update plots and data summaries based on variables the user can select.  On the model Fitting subtab you can select the variables to be used in a linear model and a random forest model then you can run the models to obtain performance. On the prediction subtab you can select the variable values for a linear and random forest model to obtain predictions."),
                br(),
                h1("Data"),
                p("The data used in this app is from the Recreation Fishing Data page on NOAA's website, ", a(href = "https://www.fisheries.noaa.gov/recreational-fishing-data/recreational-fishing-data-downloads", "found here"), ". The file used is the Size data from 2022. The data contains one record per fish caught by the interviewer that took the survey. The variables used in this app are the type of fish, the sub region the fish was caught, the area the fish was caught, the length on the fish in cm, the weight of the fish in kg, and the 2 month range the fish was caught in. Please read the MRIP_Read_Me.pdf and the MRIP_Survey_Variables.xls for more detailed information, both can be ", a(href = "https://www.st.nmfs.noaa.gov/st1/recreational/MRIP_Survey_Data/", "found here"), " along with the .csv files from 2022 used.")
                )
      ),
      #EDA Tab.
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
      #Modeling sub tabs.
      tabItem(tabName = "infoModel",
              fluidPage(
                h1("Model Info")
                p("")
              )
      ),
      #Fitting regression.
      tabItem(tabName = "fitModel",
                fluidRow(
                  box(sliderInput(inputId = "trainSplit",
                                  label = "Specify the Training Data Percentage",
                                  min = 0.05,
                                  max = 0.95,
                                  step = 0.05,
                                  value = 0.75
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
              #Fitting Random Forest.
                fluidRow(
                  box(sliderInput(inputId = "trainSplitRF",
                                   label = "Specify the Training Data Percentage",
                                   min = 0.05,
                                   max = 0.95,
                                   step = 0.05,
                                   value = 0.75),
                      
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
      #prediction tab.
      tabItem(tabName = "predict",
              fluidRow(
                box("Select the input values for the Linear Model",
                    #Select fish var.
                    selectInput(inputId = "namePred",
                                label = "Fish Type",
                                choices = unique(fishData$name)
                                ),
                    #select Region Var.
                    selectInput(inputId = "subRegPred",
                                label = "Sub Region",
                                choices = unique(fishData$subReg)
                                ),
                    #select Area var.
                    selectInput(inputId = "areaPred",
                                label = "Area",
                                choices = unique(fishData$area)
                                ),
                    #select length var.
                    numericInput(inputId = "cmPred",
                                 label = "Lenght in cm",
                                 value = 50
                                 ),
                    #Select month var.
                    selectInput(inputId = "monthPred",
                                label = "Months",
                                choices = unique(fishData$month)
                                ),
                    #select training data split.
                    sliderInput(inputId = "predSplit",
                                label = "Specify Training Data Split",
                                min = 0.05,
                                max = 0.95,
                                step = 0.05,
                                value = 0.75),
                    #button to run the model
                    actionButton("lmPred", "Predict Fish Weight")
                    ),
                #Select variables for rf.
                box("Select the input values for the Random Forest",
                    #select region var.
                    selectInput(inputId = "subRegPredRf",
                                label = "Sub Region",
                                choices = unique(fishData$subReg)
                    ),
                    #Select area var.
                    selectInput(inputId = "areaPredRf",
                                label = "Area",
                                choices = unique(fishData$area)
                    ),
                    #select weight var.
                    numericInput(inputId = "kgPredRf",
                                 label = "Weight in kg",
                                 value = 10
                                 ),
                    #Select length var.
                    numericInput(inputId = "cmPredRf",
                                 label = "Lenght in cm",
                                 value = 50
                                 ),
                    #Select month var.
                    selectInput(inputId = "monthPredRf",
                                label = "Months",
                                choices = unique(fishData$month)
                    ),
                    #select training data split.
                    sliderInput(inputId = "predSplitRf",
                                label = "Specify Training Data Split",
                                min = 0.05,
                                max = 0.95,
                                step = 0.05,
                                value = 0.75),
                    #button to run rf.
                    actionButton("rfPred", "Predict Fish Type")
                    ),
                #Output.
                box(verbatimTextOutput("linearPredOutput"),
                    verbatimTextOutput("rfPredOutput")
                    )
                )
              )
      )
  )
)

