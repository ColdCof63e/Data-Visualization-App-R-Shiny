server <- function(input, output, session){
  
  # Validate data presence and required columns
  req(exists("finance"), is.data.frame(finance))
  required_cols <- c(
    "Country","Date","Stock_Index","Index_Value","Daily_Change_Percent",
    "GDP_Growth_Rate_Percent","Inflation_Rate_Percent","Interest_Rate_Percent",
    "Unemployment_Rate_Percent","Exchange_Rate_USD","Bond_Yield_10Y_Percent",
    "Credit_Rating","Political_Risk_Score","Banking_Sector_Health"
  )
  req(all(required_cols %in% names(finance)))
  
  data_react <- reactive({
    d <- finance
    if ("region" %in% names(d) && isTruthy(input$region) && input$region != "All") {
      d <- d %>% filter(region == input$region)
      if (nrow(d) == 0) d <- finance
    }
    d
  })
  
  # Scatter with optional labels
  output$scatter <- renderPlot({
    req(input$xvar, input$yvar)
    d <- data_react()
    req(nrow(d) > 0, input$xvar %in% names(d), input$yvar %in% names(d))
    
    p <- ggplot(d, aes(x = .data[[input$xvar]], y = .data[[input$yvar]])) +
      geom_point(color = "#4E79A7", size = 3) +
      geom_smooth(method = "lm", se = FALSE, color = "#E15759") +
      labs(title = "Scatter with Trend", x = input$xvar, y = input$yvar)
    
    if (isTRUE(input$show_labels) && "Country" %in% names(d)) {
      p <- p + ggrepel::geom_text_repel(aes(label = .data[["Country"]]), size = 3)
    }
    p
  })
  
  # Box plot: returns by Credit_Rating
  output$boxplot <- renderPlot({
    d <- data_react()
    req(nrow(d) > 0, all(c("Credit_Rating","Daily_Change_Percent") %in% names(d)))
    ggplot(d, aes(x = .data[["Credit_Rating"]], y = .data[["Daily_Change_Percent"]])) +
      geom_boxplot(fill = "#76B7B2") +
      labs(title = "Returns by Rating", x = "Credit_Rating", y = "Daily_Change_Percent")
  })
  
  # Risk plot: Political_Risk_Score vs Daily_Change_Percent, colored by Banking_Sector_Health
  output$riskplot <- renderPlot({
    d <- data_react()
    req(nrow(d) > 0, all(c("Political_Risk_Score","Daily_Change_Percent","Banking_Sector_Health") %in% names(d)))
    ggplot(d, aes(x = .data[["Political_Risk_Score"]], y = .data[["Daily_Change_Percent"]],
                  color = .data[["Banking_Sector_Health"]])) +
      geom_point(size = 3) +
      geom_smooth(method = "lm", se = FALSE, color = "grey30") +
      labs(title = "Political Risk vs Returns", x = "Political_Risk_Score", y = "Daily_Change_Percent")
  })
  
  # FX plot: Exchange_Rate_USD vs Daily_Change_Percent
  output$fxplot <- renderPlot({
    d <- data_react()
    req(nrow(d) > 0, all(c("Exchange_Rate_USD","Daily_Change_Percent") %in% names(d)))
    ggplot(d, aes(x = .data[["Exchange_Rate_USD"]], y = .data[["Daily_Change_Percent"]])) +
      geom_point(color = "#EDC948", size = 3) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = "USD FX vs Stock Returns", x = "Exchange_Rate_USD", y = "Daily_Change_Percent")
  })
  
  # Summary KPIs
  output$summarytbl <- renderTable({
    d <- data_react()
    req(nrow(d) > 0)
    d %>%
      summarize(
        mean_return    = mean(.data[["Daily_Change_Percent"]], na.rm = TRUE),
        mean_inflation = mean(.data[["Inflation_Rate_Percent"]], na.rm = TRUE),
        mean_gdp       = mean(.data[["GDP_Growth_Rate_Percent"]], na.rm = TRUE),
        n              = dplyr::n()
      )
  })
}
