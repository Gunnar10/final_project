#Load Packages
library(shiny)
library(tidyverse)
library(caret)

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
            cm = LNGTH / 10, 
            month = as.factor(case_when(WAVE == 1 ~ "jan/feb",
                                        WAVE == 2 ~ "march/april",
                                        WAVE == 3 ~ "may/june",
                                        WAVE == 4 ~ "july/aug",
                                        WAVE == 5 ~ "sept/oct",
                                        WAVE == 6 ~ "nov/dec"))
  ) %>% drop_na()

trainIndex <- createDataPartition(fishData$kg, p = 0.8, list = FALSE)
fishTrain <- fishData[trainIndex, ]
fishTest <- fishData[-trainIndex, ]

fit <- train(kg ~ ., data = fishTrain,
             method = "lm",
             trControl = trainControl(method = "cv", number = 10))
fit

pred <- predict(fit, newdata = fishTest)
postResample(pred,obs = fishTest$kg)

fit2 <- train(kg ~ name + cm + area, data = fishTrain,
              method = "lm",
              trControl = trainControl(method = "cv", number = 10))

pred <- predict(fit2, newdata = fishTest)
postResample(pred,obs = fishTest$kg)
fit2
data.frame(t(fit$results), t(fit2$results))

# Define server logic
function(input, output, session) {

    output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')

    })
    
    output$table <- renderTable({
      head(fishData)
    })

}
