library(rvest)
library(dplyr)
library(keras)
library(tensorflow)
library(tidyverse)
library(glue)
library(forcats)
library(timetk)
library(tidyquant)
library(tibbletime)
library(cowplot)
library(recipes)
library(rsample)
library(yardstick)
library(tfruns)
library(datasets)
library(lubridate)

# Importing the libraries


# Function to scrape Lotto Max numbers
scrape_lotto_max_numbers <- function(url) {
  # Read HTML content from the URL
  webpage <- read_html(url)
  
  # Find the table containing the numbers
  numbers_table <- html_nodes(webpage, ".balls")
  
  # Extract data from table rows
  numbers <- numbers_table %>%
    html_nodes(".ball") %>%
    html_text() %>%
    matrix(ncol = 8, byrow = TRUE, dimnames = list(NULL, c("0", "1", "2", "3", "4", "5", "6", "Bonus"))) %>%
    as.data.frame()
  
  return(numbers)
}
#function to scrape jackpot numbers
scrape_lotto_max_jackpot <- function(url){
  webpage <- read_html(url)
  jackpot_table <- html_nodes(webpage, "td.jackpot")
  
  jackpot <- jackpot_table %>%
    html_text() %>%
    matrix(ncol = 1, byrow = TRUE, dimnames = list(NULL, c("Jackpot"))) %>%
    as.data.frame()
  return(jackpot)
}
#function to scrape jackpot dates
scrape_lotto_max_dates <- function(url){
  webpage <- read_html(url)
  date_table <- html_nodes(webpage, ".colour")
  
  dates <- date_table %>%
    html_text() %>%
    matrix(ncol = 1, byrow = TRUE, dimnames = list(NULL, c("Date"))) %>%
    as.data.frame()
  for (i in 1:ncol(dates)){
    dates[,i] <- gsub("[\r\n\t]", "", dates[,i])
  }
  return(dates)
}
# Function to save Lotto Max numbers to CSV
save_to_csv <- function(numbers1, jackpots1, lottodates1, filename) {

  datad <- cbind.data.frame(lottodates1,numbers1,jackpots1)
 
  clean <- read.csv(filename, stringsAsFactors = FALSE)
  clean[] <- lapply(datad, function(x) gsub("[\r\n\t]", "", x))
  print(datad)
  write.csv(datad, file = filename, row.names = FALSE)
  return(datad)
  
  
}

# Example usage
url <- "https://www.lottomaxnumbers.com/past-numbers"
numbers <- scrape_lotto_max_numbers(url)
jackpots <- scrape_lotto_max_jackpot(url)
lottodates <- scrape_lotto_max_dates(url)

print(lottodates)
print(numbers)
print(jackpots)

save_to_csv(numbers,jackpots,lottodates, "lottomax_numbers.csv")
#lottodate2 <- as.Date(as.character(lottomax_numbers$Date), "%m %d %y" )
#print(lottodate2)
lottodatea <- mdy(lottomax_numbers$Date)
glimpse(lottodatea)
tk_tbl(lottomax_numbers) %>%
mutate(index = lottodatea) %>%
as_tbl_time(index = index)



