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

 # ez swap table 
ez_tbl <- reactiveVal(data.frame())  
results <- reactiveVal(list())

output$ez_swap_tbl <- renderReactable({
  reactable(ez_tbl())
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
                 denominate = input$denominate)
    results(op)
    op_ <<- op
    
  } else {
    stop("There's been an issue with the table")
  }
 })
 
})

}
