# GET ALL DATA FROM THE SLM TECHNOLOGIES QUESTIONNAIRES
library(here)
library(httr)
library(jsonlite)
library(tidyverse)
if(!exists("questionnaire")){
  filename = here::here("02_raw_data","01_Questionnaires.csv")
  questionnaire<-read_csv(filename)
}

## Keep only technologies questionnaires
techs <- questionnaire %>% 
  filter(str_detect(code,"tech")) %>% 
  extract(code, c("type", "codenum"), "([[:alnum:]]+)_([[:alnum:]]+)", remove=FALSE) # seperates code to type and number

## Function to extract all data
f_get_data <- function(apikey,techs,qn){
  ### API request string
  url_base <- "https://qcat.wocat.net"
  endpoint <- techs$details[qn]   # This indexes the relevant endpoint stored in details row
  url_req  <-  paste0(url_base,endpoint)
  
  ### Raw data
  resp         <- GET(url_req, add_headers(Authorization=apikey))
  ### Cleaning
  cont_raw     <- content(resp)              # Contents of the API response
  data_raw     <- enframe(unlist(cont_raw))  # Creates a very long unnested tibble
  
  ### Dots in the name col in data_raw seperate out potential columns
  ### Identify the name with most number of dots
  rgx_split <- "\\."
  n_cols_max <-
    data_raw %>%
    pull(name) %>% 
    str_split(rgx_split) %>% 
    map_dbl(~length(.)) %>% 
    max()
  n_cols_max
  n_cols_max=n_cols_max+10 # overcompensating for any potential issues with number of cols
  
  ### Seperated columns
  nms_sep <- paste0("name", 1:n_cols_max)
  data_sep <-
    data_raw %>% 
    separate(name, into = nms_sep, sep = rgx_split, fill = "right")
}

## Get the data
### First questionnaire
qnum = 1
data_raw <- f_get_data(apikey,techs,qnum) %>% 
  mutate(q_code=techs$code[qnum])
### Loop over the rest of the questionnaires
for(qnum in 2:nrow(techs)) {
  d_raw <- f_get_data(apikey,techs,qnum) %>% 
    mutate(q_code=techs$code[qnum])
  data_raw <- data_raw %>% bind_rows(d_raw)
}

# Save raw data
data_raw <- data_raw[colSums(!is.na(data_raw)) > 0]
filename = here::here("02_raw_data","02_Tech_data_raw.rds")
saveRDS(data_raw,filename)