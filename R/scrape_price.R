library(rvest)
library(dplyr)
library(stringr)

# URL of the website to scrape
url <- "https://openai.com/pricing"

# Read the HTML content of the webpage
webpage <- read_html(url)

# Use CSS selectors to scrape all rows in the pricing table
rows <- webpage %>%
  html_nodes("#gpt-4-turbo > div:nth-child(1) > div:nth-child(1) > div:nth-child(2) > div:nth-child(1) > div:nth-child(1) > div:nth-child(1) > table:nth-child(1) > tbody:nth-child(1) > tr")

# Function to extract model and price information from a row
extract_info <- function(row){
  models <- row %>% html_nodes("td:nth-child(1) > span") %>% html_text()
  Input_price <- row %>% html_nodes("td:nth-child(2)") %>% html_text()
  Output_price <- row %>% html_nodes("td:nth-child(3)") %>% html_text()


if (str_detect(Input_price, "M")) {
  Input_price_d <- 1e6
} else if (str_detect(Input_price, "B")) {
  Input_price_d <- 1e9
} else if (str_detect(Input_price, "T")) {
  Input_price_d <- 1e12
} else {
  Input_price_d <- as.numeric(str_extract(Input_price, "/\\s*(\\d+)"))
}

if (str_detect(Output_price, "M")) {
  Output_price_d <- 1e6
} else if (str_detect(Output_price, "B")) {
  Output_price_d <- 1e9
} else if (str_detect(Output_price, "T")) {
  Output_price_d <- 1e12
} else {
  Output_price_d <- as.numeric(str_extract(Output_price, "/\\s*(\\d+)"))
}

# Extract only the numeric part of the price and divide by 1M
Input_price <- as.numeric(str_extract(Input_price, "\\d+\\.\\d+")) / Input_price_d
Output_price <- as.numeric(str_extract(Output_price, "\\d+\\.\\d+")) / Output_price_d

data.frame(Model = models, Input = Input_price, Output = Output_price)
}

# Apply function to each row and combine results into a data frame
data <- do.call(rbind, lapply(rows, extract_info))
print(data)

# Apply function to each row
lapply(rows, extract_info)

# Use CSS selectors to scrape all rows in the new table
rows_gpt_4_turbo <- webpage %>%
  html_nodes("#gpt-4 > div:nth-child(1) > div:nth-child(1) > div:nth-child(2) > div:nth-child(1) > div:nth-child(1) > div:nth-child(1) > table:nth-child(1) > tbody:nth-child(1) > tr")

# Apply function to each row and combine results into a data frame
data_gpt_4_turbo <- do.call(rbind, lapply(rows_gpt_4_turbo, extract_info))
print(data_gpt_4_turbo)

# Create variables for the input and output price for each model in the new table
models_gpt_4_turbo <- data_gpt_4_turbo$Model
Input_price_gpt_4_turbo <- data_gpt_4_turbo$Input
Output_price_gpt_4_turbo <- data_gpt_4_turbo$Output

# Use CSS selectors to scrape all rows in the new table
rows_gpt_3_5_turbo <- webpage %>%
  html_nodes("#gpt-3-5-turbo > div:nth-child(1) > div:nth-child(1) > div:nth-child(2) > div:nth-child(1) > div:nth-child(1) > div:nth-child(1) > table:nth-child(1) > tbody:nth-child(1) > tr")

# Apply function to each row and combine results into a data frame
data_gpt_3_5_turbo <- do.call(rbind, lapply(rows_gpt_3_5_turbo, extract_info))
print(data_gpt_3_5_turbo)

# Create variables for the input and output price for each model in the new table
models_gpt_3_5_turbo <- data_gpt_3_5_turbo$Model
Input_price_gpt_3_5_turbo <- data_gpt_3_5_turbo$Input
Output_price_gpt_3_5_turbo <- data_gpt_3_5_turbo$Output



