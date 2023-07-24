library(shiny)


# Define server logic required to draw a histogram
function(input, output, session) {

observeEvent(input$latest_block, {
  latest <- get_latest_block()
  
  updateNumericInput(session, inputId = "budget", value = 100)
  updateNumericInput(session, inputId = "denominate", value = 1)
  updateNumericInput(session, inputId = "from_block", value = (latest$height-10000))
  updateNumericInput(session, inputId = "to_block", value = latest$height)
  
})

 # ez swap table (with default) ---- 
ez_tbl <- reactiveVal(starter_op$trades)  

output$ez_swap_tbl <- renderReactable({
  swp_tbl <- ez_tbl()[, c("block_number","amount0_adjusted", "amount1_adjusted","price")]
  swp_tbl$action <- ifelse(swp_tbl$amount0_adjusted < 0, "Sell ETH", "Buy ETH")
  swp_tbl <- swp_tbl[, c(1,5,2,3,4)]
  colnames(swp_tbl) <- c("Block","Action", "WBTC", "ETH", "Price")
  reactable(
    swp_tbl[, 2:5]
  )
})

# Optimization Results (with default)----
results <- reactiveVal(starter_op)

ar1 <- eventReactive(results(), {
  
  calc_forecast(optim_result = results(), budget = input$budget)
  
} )

# Cards ----
output$start_ <- renderUI({
  start_card(position_details = results()$position_details, budget = input$budget, xname = "WBTC", yname = "ETH")
})

output$end_ <- renderUI({
  end_card(strategy_details = results()$strategy_details, xname = "WBTC", yname = "ETH")
})

output$forecast_ <- renderUI({
  forecast_card(ar1(), budget = input$budget, xname = "WBTC", yname = "ETH")
})

# Plots ----

output$price_plot <- renderPlotly({
  
  price_lower <- round(tick_to_price(results()$position_details$tick_lower, 1e10), 3)
  price_upper <- round(tick_to_price(results()$position_details$tick_upper, 1e10), 3)
  forecast_low <- round(ar1()$price_lower, 3)
  forecast_high <- round(ar1()$price_upper, 3)
  plot_price_lines(ez_tbl(), price_lower, price_upper, forecast_low, forecast_high)

  })

output$grid_plot <- renderPlotly({
  grid_fit(results()$init_grid, results()$init_sv, "ETH","ETH/BTC", "ETH")
})

output$plane_plot <- renderPlotly({
  plane_fit(results()$init_grid, results()$init_sv, "ETH","ETH/BTC", "ETH")
})

# SQL Query ----

output$sql <- renderUI({
  div(class = "sql-code", sql_highlight(query_txt(input$from_block, input$to_block)))
})

# On Submit ----
observeEvent(input$submit, {
  # Generate EZ Trade 
ez_tbl_url <- paste0(secret_url,
         "make_ez?from_block=",
         input$from_block,
         "&to_block=",
         input$to_block)
eurl <<- ez_tbl_url

 withProgress(expr = {
   
  incProgress(amount = 0.1, message = "Request sent to Flipside", 
              detail = "Calling Quicknode/LiveQuery")
  ez <- httr::POST(ez_tbl_url)
  
  if(ez$status_code != 200){
    incProgress(amount = -0.1, message = "Retrying request")
    Sys.sleep(5)
    ez <- httr::POST(ez_tbl_url)
  } 
  
  if(ez$status_code == 200){
    incProgress(amount = 0.5, message = "Trades Table Complete")
    ez <- do.call(rbind.data.frame, httr::content(ez))
    ez_tbl(ez)
    ez_ <<- ez
    
    incProgress(amount = 0.1, message = "Requesting Optimal Range")
    op <- uni_optimize(trades = ez, 
                 budget = input$budget, 
                 denominate = 1)
    results(op)
    op_ <<- op
    
  } else {
    stop("There's been an issue with the table")
  }
 })
 
})

}
