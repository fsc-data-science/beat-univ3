
library(shiny)

fluidPage(
  
  tags$head(
    title = "Flipside Data Science",
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css?family=Questrial"),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css?family=Inter")
  ),
  withTags({
    header(class="top-banner",
           section(
             a(class="fs-logo", href="https://www.flipsidecrypto.com", img(src = "FLIPSIDE-BRAND-1-WHITE.png", width = "75%")),
             section(class="socials",
                     a(class="twitter", target = "_blank", href="https://twitter.com/flipsidecrypto", "Twitter"),
                     a(class="linkedin", target = "_blank", href="https://www.linkedin.com/company/flipside-crypto", "LinkedIn"),
                     a(class="discord", target = "_blank", href="https://discord.com/invite/ZmU3jQuu6W", "Discord"),
                     a(href="https://next.flipsidecrypto.xyz/", target = "_blank", "Explore our data!")
             )
           )
    )
  }),
  # centered main app ----
  div(class = "main-app",
  hr(class = "break-line"),
  fluidRow(
    column(3, 
           # user inputs budget, from_block, to_block
           div(class = "input-bar",
               div(class = "latest-block", actionButton(inputId = "latest_block", label = "Real Time")),
               numericInput(inputId = "budget", label = "Budget",
                            value = 100, min = 1, max = 1000, step = 1),
               numericInput(inputId = "from_block", label = "From Block", 
                            value = 16000000, min = 13000000, step = 1),   
               numericInput(inputId = "to_block", label = "To Block",
                            value = 16010000, min = 13001000, step = 1),
               div(class = "submit-btn", actionButton(inputId = "submit", label = "Submit"))
           ),
          HTML(
          "<h2>Pipeline</h2>
          <p>2 Flipside offerings are combined to create this demo</p>
            <li>Compass API: our production RPC API for the same Studio SQL experience in your environment.            </li>
            <li>LiveQuery: A structured approach to querying nodes (and other external data sources) 
            using bring-your-own credentials to combine real-time data with Flipside curations.</li>
            "
              ),
          reactableOutput("ez_swap_tbl")
           ),
    column(6, 
           div(class = 'intro-text',
               br(),
               p(class = 'app-title', "Beating Uniswap v3"),
               p("Uniswap v3 enables concentrated liquidity.
                   Users create automated strategies to support traders using the Uniswap DEX. The more 
                   accurately users forecast price ranges the higher % of trading fees they earn."),
               p("This tool calculates the 'perfect' range for known trades allowing users
                     to benchmark their performance given:"),
               HTML("
         <li>A Budget (e.g. 100 ETH)</li>
         <li>The trades over a time period</li>
         <br>
         "
               ),
           hr(),
           fluidRow(plotlyOutput("price_plot"),
                    hr(),
                    fluidRow(
                      column(4, uiOutput("start_")),
                      column(4, uiOutput("end_")),
                      column(4, uiOutput("forecast_"))
                    ),
                    fluidRow(
                      column(2, ""),
                      column(8, plotlyOutput("grid_plot")),
                      column(2, "")
                    )
           ))
    ),
    column(3, 
           uiOutput('sql')
    )
  )
  ) # main app 
) # page 




