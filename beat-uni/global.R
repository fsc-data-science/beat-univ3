library(shiny)
library(jsonlite)
library(httr)
library(reactable)
library(plotly)
library(uniswap)
library(akima)

options(scipen = 99) # don't reformat large numbers

# read default parameter optimization from save 
# to load app w/ visuals 
starter_op <- readRDS("op_16m10k_base.rds")
starter_op$init_sv[starter_op$init_sv > 0] <- -99

get_latest_block <- function(){
  latest_info <- fromJSON('https://api.blockcypher.com/v1/eth/main')
  return(latest_info)
}

secret_url <- readLines("secret_url.txt")

# Optimize ----
uni_optimize <- function(trades, budget, denominate, p1 = 0, p2 = 0, 
                         decimal_x = 1e8, decimal_y = 1e18, fee = 0.003){
  budget <- as.numeric(budget)
  denominate <- as.numeric(denominate)
  
  p1 <- as.numeric(p1)
  p2 <- as.numeric(p2)
  
  if(length(p1) == 0 | p1 == '' | p1 == 0 | is.null(p1)){
    p1 <- NULL
  }
  if(length(p2) == 0 | p2 == '' | p2 == 0 | is.null(p2)){
    p2 <- NULL
  }
  
  decimal_x <- as.numeric(decimal_x)
  decimal_y <- as.numeric(decimal_y)
  fee <- as.numeric(fee)
  
  
  decimal_adjustment <- max(c(decimal_y/decimal_x, decimal_x/decimal_y))
  
  
  required_colnames <- c("tick","liquidity","amount0_adjusted","amount1_adjusted")
  
  if( mean( required_colnames %in% colnames(trades) ) != 1 ){
    stop("Need the following columns: tick, liquidity, amount0_adjusted, amount1_adjusted")
  }
  
  if(is.null(p1)){
    p1 <- tick_to_price(trades$tick[1], decimal_adjustment = decimal_adjustment)
  }
  if(is.null(p2)){
    p2 <- tick_to_price(tail(trades$tick,1), decimal_adjustment = decimal_adjustment)
  }
  
  
  paramz <- list(
    budget, denominate, p1, p2, decimal_x, decimal_y, fee
  )
  
  # Use naive search to get close-enough initial parameters for optimization
  low_price <- (1:9)/10*p1
  amount_1 <- c(1, budget*(1:9)/10)
  
  grid <- expand.grid(x = amount_1, y = low_price)
  
  sv <- lapply(1:nrow(grid), function(j){
    tryCatch({
      calculate_profit(
        params = c(grid[j,1], grid[j,2]),
        budget = budget, p1 = p1, p2 = p2, trades = trades,
        decimal_x = decimal_x, decimal_y = decimal_y, fee = fee,
        denominate = denominate,
        in_optim = TRUE)
    }, error = function(e){return(0)})
  })
  
  sv <- unlist(sv)
  
  # initialize using naive search min
  init_params <- as.numeric(grid[which.min(sv), 1:2])
  
  # lower_bounds(amount1 = 0.01 * budget, p1 = 0.09 * current price)
  # upper_bounds(amount1 = .99 * budget, p1 = 0.99 * current price)
  lower_bounds <- c(0.01*budget, 0.09*p1)
  upper_bounds <- c(.99*budget, 0.99*p1)
  
  # in_optim = TRUE provides *only* -1*strategy value for optimization
  # (-1 b/c algorithm looks for minimums and we want maximum)
  
  result <- optim(init_params,
                  calculate_profit,
                  method = "L-BFGS-B",
                  lower = lower_bounds,
                  upper = upper_bounds,
                  budget = budget, p1 = p1, p2 = p2, trades = trades,
                  decimal_x = decimal_x, decimal_y = decimal_y, fee = fee,
                  denominate = denominate, in_optim = TRUE)
  
  # in_optim = FALSE provides full audit of calculation
  profit = calculate_profit(params = result[[1]],
                            budget = budget, p1 = p1, p2 = p2, trades = trades,
                            decimal_x = decimal_x, decimal_y = decimal_y, fee = fee,
                            denominate = denominate,
                            in_optim = FALSE)
  
  # gmp bigz cannot be serialized for http returns
  profit$position$liquidity <- as.numeric(profit$position$liquidity)
  
  ret <- list(
    init_sv = sv,
    init_grid = grid,
    trades = trades,
    p1 = p1,
    p2 = p2,
    init_params = init_params,
    result_par = result$par,
    result_warn = result$message,
    position_details = profit$position,
    strategy_details = profit$strategy_value
  )
  
  return(ret)
  
}

# ---- 

# Price Chart ---- 

# Position Cards ----

start_card <- function(position_details, budget, xname = "WBTC", yname = "ETH"){
  
  svg_string <- {
      '
      <svg xmlns="http://www.w3.org/2000/svg" width="200" height="200" viewBox="0 0 175 175">
  <!-- Background -->
  <rect width="100%" height="100%" fill="#10151A"></rect>
  <!-- Table Outline -->
  <rect x="0" y="0" width="175" height="175" fill="none" stroke="#FFFFFF" stroke-width="2"></rect>
  <!-- Table Headers -->
  <text x="50" y="20" fill="#FFFFFF" font-size="20" font-family="Arial, sans-serif">START</text>
  <!-- Table Rows -->
  <text x="10" y="50" fill="#FFFFFF" font-size="16" font-family="Arial, sans-serif">WBTC:</text>
  <text x="150" y="50" fill="#FFFFFF" font-size="16" font-family="Arial, sans-serif" text-anchor = "end">TOKEN0AMOUNT</text>
  <text x="10" y="70" fill="#FFFFFF" font-size="16" font-family="Arial, sans-serif">ETH:</text>
  <text x="150" y="70" fill="#FFFFFF" font-size="16" font-family="Arial, sans-serif" text-anchor = "end">TOKEN1AMOUNT</text>
  <text x="10" y="90" fill="#FFFFFF" font-size="16" font-family="Arial, sans-serif">Prices (ETH/BTC):</text>
  <text x="10" y="110" fill="#FFFFFF" font-size="16" font-family="Arial, sans-serif">Lower:</text>
  <text x="150" y="110" fill="#FFFFFF" font-size="16" font-family="Arial, sans-serif" text-anchor = "end">PRICELOW</text>
  <text x="10" y="130" fill="#FFFFFF" font-size="16" font-family="Arial, sans-serif">Upper:</text>
  <text x="150" y="130" fill="#FFFFFF" font-size="16" font-family="Arial, sans-serif" text-anchor = "end">PRICEHIGH</text>
  <text x="40" y="160" fill="#FFFFFF" font-size="20" font-family="Arial, sans-serif">Value: BUDGET</text>
</svg>
'
  }
  
  svg_string <- gsub("TOKEN0AMOUNT", round(position_details$x, 4), svg_string)
  svg_string <- gsub("TOKEN1AMOUNT", round(position_details$y, 4), svg_string)
  svg_string <- gsub("PRICELOW", round(tick_to_price(position_details$tick_lower, 1e10), 3), svg_string)
  svg_string <- gsub("PRICEHIGH", round(tick_to_price(position_details$tick_upper, 1e10), 3), svg_string)
  svg_string <- gsub("BUDGET", budget, svg_string)
  
    HTML(svg_string)
}

end_card <- function(strategy_details, xname = "WBTC", yname = "ETH"){
  svg_string <- {
    '
      <svg xmlns="http://www.w3.org/2000/svg" width="200" height="200" viewBox="0 0 175 175">
  <!-- Background -->
  <rect width="100%" height="100%" fill="#10151A"></rect>
  <!-- Table Outline -->
  <rect x="0" y="0" width="175" height="175" fill="none" stroke="#FFFFFF" stroke-width="2"></rect>
  <!-- Table Headers -->
  <text x="60" y="20" fill="#FFFFFF" font-size="20" font-family="Arial, sans-serif">END</text>
  <!-- Table Rows -->
  <text x="10" y="50" fill="#FFFFFF" font-size="16" font-family="Arial, sans-serif">WBTC:</text>
  <text x="150" y="50" fill="#FFFFFF" font-size="16" font-family="Arial, sans-serif" text-anchor = "end">TOKEN0AMOUNT</text>
  <text x="10" y="70" fill="#FFFFFF" font-size="16" font-family="Arial, sans-serif">ETH:</text>
  <text x="150" y="70" fill="#FFFFFF" font-size="16" font-family="Arial, sans-serif" text-anchor = "end">TOKEN1AMOUNT</text>
  <text x="10" y="90" fill="#FFFFFF" font-size="16" font-family="Arial, sans-serif">Fee Revenue:</text>
  <text x="10" y="110" fill="#FFFFFF" font-size="16" font-family="Arial, sans-serif">WBTC:</text>
  <text x="150" y="110" fill="#FFFFFF" font-size="16" font-family="Arial, sans-serif" text-anchor = "end">FEEBTC</text>
  <text x="10" y="130" fill="#FFFFFF" font-size="16" font-family="Arial, sans-serif">ETH:</text>
  <text x="150" y="130" fill="#FFFFFF" font-size="16" font-family="Arial, sans-serif" text-anchor = "end">FEEETH</text>
  <text x="40" y="160" fill="#FFFFFF" font-size="20" font-family="Arial, sans-serif">Value: STRAT_VALUE</text>
</svg>
'
  }
  
  svg_string <- gsub("TOKEN0AMOUNT", round(strategy_details$balances$token0, 4), svg_string)
  svg_string <- gsub("TOKEN1AMOUNT", round(strategy_details$balances$token1, 4), svg_string)
  svg_string <- gsub("FEEBTC", round(strategy_details$fees$amount0_fees, 3), svg_string)
  svg_string <- gsub("FEEETH", round(strategy_details$fees$amount1_fees, 3), svg_string)
  svg_string <- gsub("STRAT_VALUE", round(strategy_details$value, 2), svg_string)
  
  HTML(svg_string)
  
}

calc_forecast <- function(optim_result, budget){
  
  p1 = optim_result$p1
  p2 = optim_result$p2
  
  p_low = tick_to_price(tick = optim_result$position_details$tick_lower, 1e10)
  if(p_low >= p2){
    p_low = (p_low / p1) * p2
  }
  
  a1 = optim_result$position_details$y
  a0 = (budget-a1) / p2
  
  ar1_position <- uniswap::price_all_tokens(x = a0, y = a1, sqrtpx96 = price_to_sqrtpx96(p2, F, 1e10),
                                            decimal_x = 1e8, decimal_y = 1e18, 
                                            tick_lower = get_closest_tick(p_low, 1, 1e10)$tick,
                                            tick_upper = NULL)
  return(ar1_position)
}

forecast_card <- function(ar1_position, budget, xname = "WBTC", yname = "ETH"){
 
  
  svg_string <- {
    '
      <svg xmlns="http://www.w3.org/2000/svg" width="200" height="200" viewBox="0 0 175 175">
  <!-- Background -->
  <rect width="100%" height="100%" fill="#10151A"></rect>
  <!-- Table Outline -->
  <rect x="0" y="0" width="175" height="175" fill="none" stroke="#FFFFFF" stroke-width="2"></rect>
  <!-- Table Headers -->
  <text x="40" y="20" fill="#FFFFFF" font-size="20" font-family="Arial, sans-serif">FORECAST</text>
  <!-- Table Rows -->
  <text x="10" y="50" fill="#FFFFFF" font-size="16" font-family="Arial, sans-serif">WBTC:</text>
  <text x="150" y="50" fill="#FFFFFF" font-size="16" font-family="Arial, sans-serif" text-anchor = "end">TOKEN0AMOUNT</text>
  <text x="10" y="70" fill="#FFFFFF" font-size="16" font-family="Arial, sans-serif">ETH:</text>
  <text x="150" y="70" fill="#FFFFFF" font-size="16" font-family="Arial, sans-serif" text-anchor = "end">TOKEN1AMOUNT</text>
  <text x="10" y="90" fill="#FFFFFF" font-size="16" font-family="Arial, sans-serif">Prices (ETH/BTC):</text>
  <text x="10" y="110" fill="#FFFFFF" font-size="16" font-family="Arial, sans-serif">Lower:</text>
  <text x="150" y="110" fill="#FFFFFF" font-size="16" font-family="Arial, sans-serif" text-anchor = "end">PRICELOW</text>
  <text x="10" y="130" fill="#FFFFFF" font-size="16" font-family="Arial, sans-serif">Upper:</text>
  <text x="150" y="130" fill="#FFFFFF" font-size="16" font-family="Arial, sans-serif" text-anchor = "end">PRICEHIGH</text>
  <text x="40" y="160" fill="#FFFFFF" font-size="20" font-family="Arial, sans-serif">Budget: BUDGET</text>
</svg>
'
  }
  
  svg_string <- gsub("TOKEN0AMOUNT", round(ar1_position$amount_x, 4), svg_string)
  svg_string <- gsub("TOKEN1AMOUNT", round(ar1_position$amount_y, 4), svg_string)
  svg_string <- gsub("PRICELOW", round(ar1_position$price_lower, 3), svg_string)
  svg_string <- gsub("PRICEHIGH", round(ar1_position$price_upper, 3), svg_string)
  svg_string <- gsub("BUDGET", budget, svg_string)
  
  HTML(svg_string)
}

# Block Price Chart ----

plot_price <- function(trades){
plot_ly() %>% 
    add_trace(data = trades, 
        x = ~block_number, y = ~price, 
        size = ~abs(amount1_adjusted), type = "scatter",
        text = paste0(
          "Block #", format(trades$block_number, big.mark = ","),
          "\nPrice: ", trades$price, 
          "\nETH Size:", abs(trades$amount1_adjusted)),
        hoverinfo = "text",
        mode = "markers+lines") %>% 
    layout(
      title = list(text = "Trades (Price & ETH Volume)", y = 0.95),
      xaxis = list(title = "Block #"),
      yaxis = list(title = "Price (ETH/BTC)", 
                   range = c(0.995*min(trades$price), 
                             1.005*max(trades$price)))
    )
}
# Block Price w/ Rectangle ----

plot_price_lines <- function(trades, price_low, price_high, forecast_low, forecast_high){
  
  minx <- min(trades$block_number)
  maxx <- max(trades$block_number)
  miny <- 0.99 * min(price_low, min(trades$price), min(forecast_low))
  maxy <- 1.01 * max(price_high, max(trades$price), max(forecast_high))
  
  plot_ly() %>% 
    add_trace(x = c(minx, maxx), y = c(price_high, price_high),
              type = "scatter", mode = "lines",name = paste0("Range: ", price_high),
              line = list(dash = "dash", color = "blue")) %>%
    add_trace(x = c(minx, maxx), y = c(price_low, price_low),
              type = "scatter", mode = "lines", name = paste0("Range: ", price_low),
              line = list(dash = "dash", color = "blue")) %>% 
    add_trace(x = c(minx, maxx), y = c(forecast_high, forecast_high),
              type = "scatter", mode = "lines",name = paste0("Forecast: ", forecast_high),
              line = list(dash = "dash", color = "lightblue")) %>%
    add_trace(x = c(minx, maxx), y = c(forecast_low, forecast_low),
              type = "scatter", mode = "lines", name = paste0("Forecast: ", forecast_low),
              line = list(dash = "dash", color = "lightblue")) %>% 
    add_trace(data = trades, 
          x = ~block_number, y = ~price, name = "Trades & Volume",
          size = ~abs(amount1_adjusted), type = "scatter",
          text = paste0(
            "Block #", format(trades$block_number, big.mark = ","),
            "\nPrice: ", trades$price, 
            "\nETH Vol:", abs(trades$amount1_adjusted)),
          hoverinfo = "text",
          mode = "markers+lines") %>% 
    layout(
      title = list(text = "Trades (Price & ETH Volume)", y = 0.95),
      xaxis = list(title = "Block #"),
      yaxis = list(title = "Price (ETH/BTC)", 
                   range = c(miny, maxy))
    )
  
}

# Plane 3D Viz ---- 
plane_fit <- function(init_grid, sv, denom_label, price_label, allo_label){
  
  init_field <- cbind(init_grid, sv)
  colnames(init_field) <- c("allocation","low_price","strategy_value")
  x <- init_field$allocation
  y <- init_field$low_price
  z <- init_field$strategy_value
  
  # Create a data frame
  data <- data.frame(x = x, y = y, z = z)
  
  # Fit a plane using Multivariate Regression
  model <- lm(z ~ x + y, data = data)
  
  # Obtain the estimated coefficients
  a <- coef(model)[1]
  b <- coef(model)[2]
  c <- coef(model)[3]
  
  # Generate a grid of x and y values
  x_range <- range(x)
  y_range <- range(y)
  x_grid <- seq(x_range[1], x_range[2], length.out = 50)
  y_grid <- seq(y_range[1], y_range[2], length.out = 50)
  grid <- expand.grid(x = x_grid, y = y_grid)
  
  # Calculate the corresponding z values using the estimated plane equation
  z_grid <- matrix(a + b * grid$x + c * grid$y, nrow = length(x_grid), byrow = TRUE)
  
  # Generate the 3D scatter plot
  plot_ly(data = data) %>%
    add_trace(name = "Estimated Profit (Strategy Value)",
              x = ~x,
              y = ~y,
              z = ~z,
              mode = "markers",
              marker = list(
                size = 3,
                color = ~z,
                colorscale = "Viridis"
              )
    ) %>%
    add_surface(name = "Best Fit Plane",
                x = x_grid,
                y = y_grid,
                z = z_grid,
                colorscale = "Viridis",
                showscale = FALSE
    ) %>% 
    layout(showlegend = TRUE,
           legend = list(orientation = "h", x = 0, y = -0.2),
           scene = list(
             xaxis = list(title = paste0("Allocation (",denom_label,")")),
             yaxis = list(title = paste0("Low Price (", price_label, ")")),
             zaxis = list(title = paste0("Strategy Value (",allo_label,")"),
                          range = c(0,1.1*max(z)))
           )
    )
  
}

#plane_fit(init_grid, sv,"ETH","ETH/BTC","ETH")

# Grid 3D Viz ----

grid_fit <- function(init_grid, sv, denom_label, price_label, allo_label){
  
  init_field <- cbind(init_grid, sv*-1)
  colnames(init_field) <- c("allocation","low_price","strategy_value")
  x <- init_field$allocation
  y <- init_field$low_price
  z <- init_field$strategy_value
  
  data = cbind(x,y,z)
  
  # Define the grid for interpolation
  grid_x <- sort(unique(x))
  grid_y <- sort(unique(y))
  
  # Perform bilinear interpolation to estimate z values on the grid
  interp_z <- interp(x, y, z, xo = grid_x, yo = grid_y, linear = FALSE)$z
  
  
  # Create a 3D plot with Plotly
  plot_ly() %>%
    add_trace(name = "Estimated Profit (Strategy Value)",
              x = x,
              y = y,
              z = z,
              mode = "markers",
              marker = list(
                size = 3,
                color = z,
                colorscale = "Jet"
              )) %>% 
    add_surface(
      x = grid_x,
      y = grid_y,
      z = interp_z,
      colorscale = "Cividis",
      showscale = FALSE
    ) %>% 
    layout(showlegend = TRUE,
           legend = list(orientation = "h", x = 0, y = -0.2),
           scene = list(
             xaxis = list(title = paste0("Allocation (",denom_label,")")),
             yaxis = list(title = paste0("Low Price (", price_label, ")")),
             zaxis = list(title = paste0("Strategy Value (",allo_label,")"),
                          range = c(0,1.1*max(z)))
           )
    )
  
}

# grid_fit(init_grid, sv,"ETH","ETH/BTC","ETH")


# Query-Text Format ----

sql_highlight <- function(sql_code) {
  # Define the SQL keywords and their associated CSS classes
  keywords <- list(
    keyword = c("SELECT", "FROM", "WHERE", "GROUP BY", "ORDER BY", "HAVING", "JOIN", "INNER JOIN", "LEFT JOIN", "RIGHT JOIN", "FULL JOIN", "OUTER JOIN", "ON", "AS", "AND", "OR", "NOT", "IS", "NULL", "IN", "BETWEEN", "EXISTS", "LIKE", "LIMIT", "DISTINCT"),
    func_ = c("COUNT", "SUM", "AVG", "MIN", "MAX"),
    operator = c("=", "<", ">", "<=", ">=", "<>", "!=")
  )
  
  sql_code <- gsub("<", "&lt;", sql_code)
  sql_code <- gsub(">", "&gt;", sql_code)
  
  # Replace SQL keywords with HTML span elements containing CSS classes
  for (k in names(keywords)) {
    for (w in keywords[[k]]) {
      sql_code <- gsub(paste0("\\b", w, "\\b"), paste0("<span class='", k, "'>", w, "</span>"), sql_code, ignore.case = TRUE)
    }
  }
  sql_code <- gsub("<span class='operator'><</span<span class='operator'>></span>","<",sql_code)
  
  html <- htmltools::HTML(sql_code)
  
  return(html)
}

query_txt <- function(from_block, to_block){
 
  query <- { 
    " 
  -- Gather relevant user inputs
  -- as CTE within Flipside Studio
  with inputs AS (
    SELECT '0xcbcdf9626bc03e24f779434178a73a0b4bad62ed' as contract_address,
    __FROM_BLOCK__ as from_block, concat('0x',trim(to_char(from_block,'XXXXXXXXXX'))) as hex_block_from,
    __TO_BLOCK__ as to_block, concat('0x',trim(to_char(to_block,'XXXXXXXXXX'))) as hex_block_to,
    ARRAY_CONSTRUCT('0xc42079f94a6350d7e6235f29174924f928cc2ac818eb64fed8004e115fbcca67', NULL, NULL
    ) as event_topic_param
  ),
  
  -- Get key pool details 
  -- from Flipside dim_dex_liquidity_pools
  pool_details AS (
    select POOL_NAME, TOKEN1_SYMBOL, TOKEN1_DECIMALS, 
    TOKEN0_SYMBOL, TOKEN0_DECIMALS,
    ABS(TOKEN1_DECIMALS - TOKEN0_DECIMALS) as decimal_adjustment
    from ethereum.core.dim_dex_liquidity_pools
    where pool_address = (select lower(contract_address) from inputs)
  ),
  
  -- Structure node request as JSON within Flipside Data Studio
  create_rpc_request as (
    SELECT contract_address, from_block, to_block,
    livequery.utils.udf_json_rpc_call(
      'eth_getLogs',
      [{ 'address': contract_address,
        'fromBlock': hex_block_from,
        'toBlock': hex_block_to,
        'topics': event_topic_param }]
    ) AS rpc_request
    FROM
    inputs),
  
  -- POST request to Node with LiveQuery 
  base AS (
    SELECT livequery.live.udf_api(
      'POST', -- method
      '{eth-mainnet-url}', -- url
      {},  -- default header
      rpc_request, -- data
      'charlie-quicknode' -- Unique registered secret
    ) AS api_call
    FROM create_rpc_request
  ),
  
  -- Clean the response in Data Studio 
  res AS (
    SELECT
    t.value:transactionHash::string as tx_hash,
    t.value:address::string as address,
    t.value:blockNumber::string as block_number,
    regexp_substr_all(SUBSTR(t.value:data, 3, len(t.value:data)), '.{64}') as data
    from base,
    LATERAL FLATTEN(input => api_call:data:result) t
  )
  
  -- Format to match Flipside Ethereum Uniswap v3 Schema
  SELECT
  address as pool_address,
  tx_hash,
  ethereum.public.udf_hex_to_int(block_number) as block_number,
  ethereum.public.udf_hex_to_int('s2c',data[0]::STRING)::FLOAT/POW(10,(select TOKEN0_DECIMALS from pool_details)) as amount0_adjusted,
  ethereum.public.udf_hex_to_int('s2c',data[1]::STRING)::FLOAT/POW(10,(select TOKEN1_DECIMALS from pool_details)) as amount1_adjusted,
  ethereum.public.udf_hex_to_int(data[2]) as sqrtPX96,
  POWER(sqrtPX96 / POWER(2, 96), 2)/(POWER(10, (SELECT decimal_adjustment from pool_details))) as price,
  ethereum.public.udf_hex_to_int(data[3]) as liquidity,
  ethereum.public.udf_hex_to_int(data[4]) as tick
  FROM res
  "
  }

  query <- gsub('__FROM_BLOCK__', from_block, query)
  query <- gsub('__TO_BLOCK__', to_block, query)
  
  return(query)
}

preformatted_cell_renderer <- function(value) {
  x = htmltools::tags$div(class = "sql-code",
                          style = "white-space: pre-wrap;", sql_highlight(value))
  htmltools::HTML(as.character(x))
}



