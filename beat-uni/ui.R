
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
  hr(class = "break-line"),
  # centered main app 
div(class = "main-app",
    # user inputs budget, denominate, from_block, to_block
    div(class = "input-bar",
          div(class = "latest-block", actionButton(inputId = "latest_block", label = "Real Time")),
          fluidRow(
            column(4, numericInput(inputId = "budget", label = "Budget",
                                   value = 100, min = 1, max = 1000, step = 1)),
            column(4, numericInput(inputId = "from_block", label = "From Block", 
                                   value = 16000000, min = 13000000, step = 1)),   
            column(4, numericInput(inputId = "to_block", label = "To Block",
                                   value = 16010000, min = 13001000, step = 1)
                   )
          ),
          div(class = "submit-btn", actionButton(inputId = "submit", label = "Submit"))  
    ),
    tabsetPanel(
      tabPanel(title = "Welcome", 
        div(class = 'intro-text',
            br(),
            p("Beating Uniswap v3"),
            p("Uniswap v3 allows for concentrated liquidity in automatic market making.
        The more correct you are about the price range a token trades at 
        the higher % of trading fees you can earn."),
            p("This tool generates the 'perfect' position given a set of trades for liquidity
         providers to benchmark their performance given:"),
            HTML("
         <li>A Budget</li>
         <li>Which token to optimize for</li>
         <li>The trades that occured (using Quicknode if data is real-time)</li>
         <br>
         "
            ),
           div(class = 'uni-img', tags$img(src = "uniswap_v3_example.png", width = "750px"))
            
        )
        ),
      tabPanel(title = "Pipeline",
          hr(),
          HTML(
            "
          <p>2 Flipside offerings are combined to create this demo</p>
            <li>Compass API: our production RPC API for the same Studio SQL experience in your environment. 
            <ul><li>Alternatively, Enterprise Data Shares (available 
            within Snowflake Marketplace) enable wholesale access to our entire database in your own Snowflake
            instance.</li></ul>
            </li>
            <li>LiveQuery: A structured approach to querying nodes (and other external data sources) 
            using bring-your-own credentials to combine real-time data with Flipside curations.</li>
            <ul><li>Here, we call Quicknode to grab the latest trades 
            in the ETH-WBTC 0.3% Pool on Ethereum Mainnet.</li></ul>
            <ul><li>Our curations for *all* Uni v3 tables are lagged ~15min. Here, we trade
            the lag (< 12 second latency) by focusing on trades for a single pool, ignoring 
            all other transactions.</li></ul>
            "
          )
      ),
      tabPanel(title = "Result",
               hr(),
               uiOutput("start_"),
               uiOutput("end_"),
               reactableOutput("ez_swap_tbl")
               )
    )
)
)




