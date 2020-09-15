# WOCAT: COST AND BENEFIT DATABASE
# FINAL CLEAN SPREADSHEET
library(here)
library(tidyverse)
library(lubridate)
#library(sjlabelled)
library(labelled)
library(codebook)
# MERGE ALL DATASETS
filename = here::here("03_processed_data","03_Data_general.rds")
Data_general<-readRDS(filename)
filename = here::here("03_processed_data","03_Dictionary_general.rds")
Dictionary_general<-readRDS(filename)
data_clean = Data_general
dictionary = Dictionary_general

filename = here::here("03_processed_data","04_Data_cost.rds")
Data_general<-readRDS(filename)
filename = here::here("03_processed_data","04_Dictionary_cost.rds")
Dictionary_general<-readRDS(filename)
data_clean = data_clean %>% full_join(Data_general, by="qid")
dictionary = dictionary %>% bind_rows(Dictionary_general)

filename = here::here("03_processed_data","05_Data_impact.rds")
Data_general<-readRDS(filename)
filename = here::here("03_processed_data","05_Dictionary_impact.rds")
Dictionary_general<-readRDS(filename)
data_clean = data_clean %>% full_join(Data_general, by="qid")
dictionary = dictionary %>% bind_rows(Dictionary_general)

rm(Data_general,Dictionary_general)
# 1.0 Some general checks
## Check if there are duplicate observations
duplicate <- data_clean %>% 
  group_by(qid) %>% 
  mutate(ind=row_number()) %>% 
  filter(ind==2)

## Check if there are duplicate variables
duplicate <- dictionary %>% 
  group_by(variable) %>% 
  mutate(ind=row_number()) %>% 
  filter(ind==2)

dictionary <- dictionary %>%   group_by(variable) %>% 
  mutate(ind=row_number()) %>% 
  filter(ind==1) %>% select(-ind)
## Check if names in data correspond with names in dictionary
a_vars   <- as_tibble(colnames(data_clean)) %>% rename(variable=value)
dictionary   <- dictionary %>% right_join(a_vars, by="variable")  %>% #filter(is.na(question_number)) %>% 
  mutate(label=ifelse(str_detect(variable,"seasonal_temperature"),
                      str_replace(variable,"incrdecr","change"),
                      label)) %>% 
  mutate(label=ifelse(str_detect(variable,"seasonal_rainfall"),
                      str_replace(variable,"incrdecr","change"),
                      label)) %>% 
  mutate(label=ifelse(str_detect(variable,"_est_"),
                      str_c(label," (establishment)"),
                      label)) %>% 
  mutate(label=ifelse(str_detect(variable,"_maint_"),
                      str_c(label," (maintenance)"),
                      label)) 

## Remove "tech" from variable name
dictionary   <- dictionary %>% 
  mutate(variable=str_replace(variable,"tech_",""))
data_clean   <- data_clean %>%
  set_names(~stringr::str_replace_all(.,"tech_", ""))

#-----------------------------------------------------------------------------------------------------------------
# CONVERT COSTS TO CONSTANT USD
library(readxl)
filename = here::here("02_raw_data","IPD_countres_UNSTAT.xlsx")
data<-read_excel(filename)
deflator <- data %>% filter(Measure=="Implicit Price Deflator - NC") %>% 
  gather("year","Measure",5:53) %>% 
  rename(deflator=Measure, country=Country) %>% 
  group_by(country) %>% 
  mutate(base=ifelse(year==2015,deflator,NA)) %>% 
  fill(base, .direction = "downup") %>% 
  mutate(adj_price=base/deflator) %>% 
  mutate(year=as.numeric(year))

filename = here::here("02_raw_data","Exchange_rates_UNSTAT.xlsx")
data<-read_excel(filename)
exrate <- data %>% filter(Measure=="IMF based exchange rate") %>% 
  gather("year","Measure",5:53) %>% 
  rename(exrate=Measure, country=Country) %>% 
  group_by(country) %>% 
  mutate(exchange_rate=ifelse(year==2015,exrate,NA)) %>% 
  fill(exchange_rate, .direction = "downup") %>% 
  ungroup() %>% 
  mutate(year=as.numeric(year))

data_clean <- data_clean %>% 
  mutate(year=ifelse(is.na(year_documentation),implementation_year,year_documentation)) %>% 
  mutate(year=ifelse(year>2018,2018,year)) %>% 
  mutate(country=ifelse(str_detect(country,"Tanzania"),"U.R. of Tanzania: Mainland",country)) %>% 
  mutate(country=ifelse(str_detect(country,"Bolivia"),"Bolivia (Plurinational State of)",country)) %>% 
  mutate(country=ifelse(str_detect(country,"Lao"),"Lao People's DR",country)) %>% 
  mutate(country=ifelse(str_detect(country,"Iran"),"Iran (Islamic Republic of)",country)) %>% 
  left_join(deflator, by=c("country","year")) %>% 
  left_join(exrate, by=c("country","year"))

## Cost in 2015 prices
a_vars <- dictionary %>% 
  filter(str_detect(variable,"total_cost")&
           !str_detect(variable,"base")&
           !str_detect(variable,"percentage")&
           !str_detect(variable,"costbenefit")&
           !str_detect(variable,"remaining"))

data_clean <- data_clean %>% 
  mutate(across(contains("total_cost")&
                  !contains("base")&
                  !contains("percentage")&
                  !contains("costbenefit")&
                  !contains("remaining")
                , as.numeric)) %>%
  mutate(across(contains("total_cost")&
                  !contains("base")&
                  !contains("percentage")&
                  !contains("costbenefit")&
                  !contains("remaining")
                , ~. * adj_price)) #%>%
  # select(qid,country,year_documentation,currency,exchange_rate
  #        input_exchange_rate,input_est_total_costs,input_est_total_costs_usd,
  #        input_maint_total_costs,input_maint_total_costs_usd) %>% 
  # filter(qid==1306)


## Convert to US dollars using 2015 exchange rates
data_clean <- data_clean %>% 
  mutate(exch_rate=ifelse(input_est_total_costs_usd==input_est_total_costs,
                          1,NA)) %>% 
  mutate(exch_rate=ifelse(is.na(exch_rate)&
                            input_est_total_costs_usd==input_est_total_costs,
                          1,exch_rate)) %>% 
  mutate(exch_rate=ifelse(is.na(exch_rate)&
                            currency=="USD",
                          1,exch_rate)) %>% 
  mutate(exch_rate=ifelse(is.na(exch_rate)&
                            currency!="USD",
                          exchange_rate,exch_rate)) %>% 
  mutate(exch_rate=ifelse(is.na(exch_rate)&
                            is.na(currency),
                          exchange_rate,exch_rate)) %>% 
  mutate(exch_rate=ifelse(is.na(exch_rate)&
                            !is.na(input_exchange_rate),
                          input_exchange_rate,exch_rate)) %>% 
  mutate(exch_rate=as.numeric(exch_rate)) %>% 
  mutate(across(contains("total_cost")&
                !contains("base")&
                !contains("percentage")&
                !contains("costbenefit")&
                !contains("remaining")&
                !contains("usd")
              , ~. / exch_rate)) 

# CLEAN AREA
data_clean <- data_clean %>% 
  mutate(area_hectare=ifelse(is.na(area_hectare)&!is.na(spread_area_precise),
                             spread_area_precise,area_hectare)) %>% 
  mutate(area_hectare=as.numeric(area_hectare))
# REMOVE UNNECESSARY VARIABLES
data_clean <- data_clean %>% 
  select(-Currency.x, -Currency.y, -CountryID.x, -CountryID.y, 
         -exrate, -exchange_rate, -year)

# ONSITE IMPACT FACTOR VARIABLES
add_impact_labels <- function(x) {
  val_labels(x) <- c("Very negative (-50 to -100%)" = -3,
                     "Negative (-20 to -50%)" = -2,
                     "Slightly negative (-5 to -20%)" = -1,
                     "Negligible impact" = 0,
                     "Slightly positive (+5 to 20%)" = 1,
                     "Positive (+20 to 50%)" = 2,
                     "Very positive (+50 - 100%)" = 3)
  x
}
data_clean <- data_clean %>%
  mutate(across(contains("costbenefit"),as.factor)) %>% 
  mutate(across(contains("impacts"),add_impact_labels))
  
# RECODE AS FACTORS
# data_clean <- data_clean %>% 
#   mutate(across(contains("costbenefit")|
#                   contains("sensitivity")|
#                   contains("incrdcr")
#                 ,as_factor))

## Set variable labels
#data_clean <- set_label(data_clean, label = dictionary$label)
var_label(data_clean) <- dictionary %>% select(variable, label) %>% dict_to_list()
var_label(data_clean) <- list(
  deflator = "GDP deflator (current year) (UNSTATS)",
  base = "GDP deflator (base year 2015) (UNSTATS)",
  exch_rate = "Exchange rate (UNSTATS)"
)
# SAVE
filename = here::here("04_output","06_Data_final.rds")
saveRDS(data_clean,filename)
filename = here::here("04_output","06_Data_final.csv")
write_csv(data_clean,filename)
filename = here::here("04_output","06_Dictionary_final.rds")
saveRDS(dictionary,filename)
filename = here::here("04_output","06_Dictionary_final.csv")
write_csv(dictionary,filename)