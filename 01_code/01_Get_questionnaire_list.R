# WOCAT: COST AND BENEFIT DATABASE
# GET QUESTIONNAIRES
# Extracts list of questionnaires from the API
library(here)
library(httr)
library(jsonlite)
library(tidyverse)

## API key (DELETE BEFORE YOU SHARE THE CODE)
apikey   <- 
## API request string
url_base <- "https://qcat.wocat.net/en"
endpoint <- "/api/v2/questionnaires/"
url_req  <-  paste0(url_base,endpoint)

## apikey = API key (required)

## Raw data
raw      <- GET(url_req, add_headers(Authorization=apikey))
raw$status_code
raw.results <- rawToChar(raw$content)   # Change unicode to characters
clean  <- fromJSON(raw.results)         # Make it readable
questionnaire <- clean$results
## Next endpoint
if (clean[["next"]]!=""){
  url_req <- clean[["next"]]
} else {
  url_req <- 0
}


## Loop over all pages to get full list of questionnaires
while (url_req != 0){
  # Raw data
  raw      <- GET(url_req, add_headers(Authorization=apikey))
  raw$status_code
  raw.results <- rawToChar(raw$content)   # Change unicode to characters
  clean       <- fromJSON(raw.results)         # Make it readable
  questionnaire <- rbind(questionnaire,clean$results)
  # Next endpoint
  if (clean[["next"]]!=""){
    url_req <- clean[["next"]]
  } else {
    url_req <- 0
  }
}

## Convert data to save as csv
questionnaire <- as_tibble(questionnaire)   # because csv_read is faster
set_lists_to_chars <- function(x) {         # function to convert any list to character
  if(class(x) == 'list') {
    y <- paste(unlist(x[1]), sep='', collapse=', ')
  } else {
    y <- x 
  }
  return(y)
}
questionnaire<-data.frame(lapply(questionnaire, set_lists_to_chars), stringsAsFactors = F)
## Save data in csv file
filename = here::here("02_raw_data","01_Questionnaires.csv")
write_csv(questionnaire, filename)
rm(clean, raw)