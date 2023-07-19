library(shiny)
library(jsonlite)
library(httr)
library(reactable)
options(scipen = 99) # don't reformat large numbers


get_latest_block <- function(){
  latest_info <- fromJSON('https://api.blockcypher.com/v1/eth/main')
  return(latest_info)
}

secret_url <- readLines("secret_url.txt")
