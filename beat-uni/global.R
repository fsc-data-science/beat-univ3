library(shiny)
library(jsonlite)
library(httr)
library(reactable)
library(uniswap)
options(scipen = 99) # don't reformat large numbers


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
    init_sv,
    init_grid,
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
  
  init_field <- cbind(init_grid, sv)
  colnames(init_field) <- c("allocation","low_price","strategy_value")
  x <- init_field$allocation
  y <- init_field$low_price
  z <- init_field$strategy_value
  
  # Define the grid for interpolation
  grid_x <- sort(unique(x))
  grid_y <- sort(unique(y))
  
  # Perform bilinear interpolation to estimate z values on the grid
  interp_z <- interp(x, y, z, xo = grid_x, yo = grid_y, linear = FALSE)$z
  
  
  # Create a 3D plot with Plotly
  plot_ly(data = data) %>%
    add_trace(name = "Estimated Profit (Strategy Value)",
              x = ~x,
              y = ~y,
              z = ~z,
              mode = "markers",
              marker = list(
                size = 3,
                color = ~z,
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
           scene = list(
             xaxis = list(title = paste0("Allocation (",denom_label,")")),
             yaxis = list(title = paste0("Low Price (", price_label, ")")),
             zaxis = list(title = paste0("Strategy Value (",allo_label,")"),
                          range = c(0,1.1*max(z)))
           )
    )
  
}

# grid_fit(init_grid, sv,"ETH","ETH/BTC","ETH")
