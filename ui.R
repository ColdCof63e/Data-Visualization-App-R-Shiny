library(shiny); 
library(dplyr); 
library(ggplot2)

ui <- fluidPage(
  titlePanel("Financial Market Datasets 2024 Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("region", "Region",
                  choices = c("All","North America","Europe","Other"),
                  selected = "All"),
      selectInput("xvar", "X variable",
                  choices = c("Interest_Rate_Percent",
                              "GDP_Growth_Rate_Percent",
                              "Exchange_Rate_USD",
                              "Inflation_Rate_Percent",
                              "Unemployment_Rate_Percent",
                              "Bond_Yield_10Y_Percent"),
                  selected = "Interest_Rate_Percent"),
      selectInput("yvar", "Y variable",
                  choices = c("Daily_Change_Percent","Index_Value"),
                  selected = "Daily_Change_Percent"),
      checkboxInput("show_labels", "Show country labels", FALSE)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("EDA", plotOutput("scatter"), plotOutput("boxplot")),
        tabPanel("Risk", plotOutput("riskplot")),
        tabPanel("FX Linkage", plotOutput("fxplot")),
        tabPanel("Summary", tableOutput("summarytbl"))
      )
    )
  )
)

