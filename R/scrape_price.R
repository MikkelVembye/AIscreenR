# URL of the website to scrape
url <- "https://openai.com/pricing"

# Read the HTML content of the webpage
webpage <- rvest::read_html(url)

# Function to extract model and price information from a row
extract_info <- function(row){
  models <- row %>% rvest::html_nodes("td:nth-child(1) > span") %>% rvest::html_text()
  Input_price <- row %>% rvest::html_nodes("td:nth-child(2)") %>% rvest::html_text()
  Output_price <- row %>% rvest::html_nodes("td:nth-child(3)") %>% rvest::html_text()

  Input_price_d <- ifelse(stringr::str_detect(Input_price, "M"), 1e6, ifelse(stringr::str_detect(Input_price, "B"), 1e9, ifelse(stringr::str_detect(Input_price, "T"), 1e12, as.numeric(stringr::str_extract(Input_price, "/\\s*(\\d+)")))))
  Output_price_d <- ifelse(stringr::str_detect(Output_price, "M"), 1e6, ifelse(stringr::str_detect(Output_price, "B"), 1e9, ifelse(stringr::str_detect(Output_price, "T"), 1e12, as.numeric(stringr::str_extract(Output_price, "/\\s*(\\d+)")))))

  # Extract only the numeric part of the price and divide by 1M
  Input_price <- as.numeric(stringr::str_extract(Input_price, "\\d+\\.\\d+")) / Input_price_d
  Output_price <- as.numeric(stringr::str_extract(Output_price, "\\d+\\.\\d+")) / Output_price_d

  data.frame(Model = models, Input = Input_price, Output = Output_price)
}

# Function to scrape and process a table
scrape_table <- function(css_selector) {
  rows <- webpage %>% rvest::html_nodes(css_selector)
  data <- do.call(rbind, lapply(rows, extract_info))
  # Remove the first row
  data <- data[-1, ]
  # Create variables for the input and output price for each model
  for(i in 1:nrow(data)) {
    assign(paste0("Input_price_", stringr::str_replace_all(data$Model[i], "-", "_")), data$Input[i], envir = .GlobalEnv)
    assign(paste0("Output_price_", stringr::str_replace_all(data$Model[i], "-", "_")), data$Output[i], envir = .GlobalEnv)
  }
}

# CSS selectors for the tables
selectors <- c("#gpt-4-turbo > div:nth-child(1) > div:nth-child(1) > div:nth-child(2) > div:nth-child(1) > div:nth-child(1) > div:nth-child(1) > table:nth-child(1) > tbody:nth-child(1) > tr",
               "#gpt-4 > div:nth-child(1) > div:nth-child(1) > div:nth-child(2) > div:nth-child(1) > div:nth-child(1) > div:nth-child(1) > table:nth-child(1) > tbody:nth-child(1) > tr",
               "#gpt-3-5-turbo > div:nth-child(1) > div:nth-child(1) > div:nth-child(2) > div:nth-child(1) > div:nth-child(1) > div:nth-child(1) > table:nth-child(1) > tbody:nth-child(1) > tr")

# Scrape and process each table
lapply(selectors, scrape_table)
