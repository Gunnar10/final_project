library(shiny)
library(shinydashboard)
# Define UI for application


dashboardPage(
  dashboardHeader(title = "Fish App"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "about"),
      menuItem("Data Exploration", tabName = "data"),
      menuItem("Modeling", tabName = "model")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "about",
              fluidRow(
                box(h1("This is the about tab."))
              )
      ),
      tabItem(tabName = "data",
              fluidRow(
                box(radioButtons(inputId = "plotType", 
                                 label = "Select the Type of Plot.",
                                 selected = character(0),
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
                                     )
                    ),
                box(plotOutput("plot"))
              )
      ),
      tabItem(tabName = "model",
              fluidRow(
                box(h1("This is the modeling tab."))
              )
      )
    )
  )
)


