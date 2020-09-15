# WOCAT: COST AND BENEFIT DATABASE
# EXTRACT COST VARIABLES
library(here)
library(tidyverse)
library(lubridate)
if(!exists("data_raw")){
  filename = here::here("02_raw_data","02_Tech_data_raw.rds")
  data_raw<-readRDS(filename)
}
filename = here::here("01_code","00_Functions.R")
source(filename)
# if(!exists("f_input_cost_data")) {
#   filename = here::here("01_code","00_Functions.R")
#   source(filename)
# }
# SECTION 4. Technical specifications, implementation activities, inputs, and costs
data_raw <- data_raw %>% mutate(qid=q_code) %>% 
  mutate(qid=as.numeric(str_replace(qid,"technologies_",""))) %>% 
  mutate(name5=str_replace(name5,"tech__","")) %>% 
  mutate(name5=str_replace(name5,"__","_"))
## Questionnaire id
data_clean <- data_raw %>% 
  select(qid) %>% unique 

dictionary <- tribble(
  ~question_number,~variable,~label,~notes,
  "0","Qid","Questionnaire ID number", ""
)

# 4.2 General information regarding the calculation of inputs and costs
a <- data_raw %>% filter(name5=="4_3") %>% select(name9) %>% unique()

a_value <- data_raw %>% 
  filter(name9=="tech_cost_calculation_base"&str_detect(name11,"value")) %>% 
  select(qid,name9,value) %>% 
  spread(name9,value) %>% 
  mutate(tech_cost_calculation_base=as_factor(tech_cost_calculation_base))

a_label <- data_raw %>% 
  filter(name9=="tech_cost_calculation_base"&name11=="key") %>% 
  select(name5,name9,value) %>% unique() %>% 
  mutate(notes="") %>% 
  rename(question_number=name5, variable=name9, label=value)

data_clean <- data_clean %>% full_join(a_value,by="qid")
dictionary <- dictionary %>% bind_rows(a_label)

## Tech area size
a_value <- data_raw %>% 
  filter(name9=="tech_perarea_size"|name9=="tech_area_unit_conversion") %>% 
  filter(name11=="value") %>% 
  select(qid,name9,value) %>% 
  spread(name9,value) %>% 
  mutate(area=str_extract(tech_perarea_size, "[0-9]+(?:.[0-9]+)?")) %>% 
  mutate(area=str_replace(area,",",".")) %>% 
  mutate(tech_perarea_size=str_to_lower(tech_perarea_size)) %>% 
  mutate(
    unit=ifelse(
    str_detect(tech_perarea_size,"hect")|
      str_detect(tech_perarea_size,"ha")|
      str_detect(tech_perarea_size," h")|
      str_detect(tech_perarea_size,"на")|
      str_detect(tech_perarea_size,"гектар")|
      str_detect(tech_perarea_size,"га"),
    "hectare",NA),
    unit=ifelse(
      str_detect(tech_perarea_size,"acre"),
      "acre",unit),
    unit=ifelse(
      str_detect(tech_perarea_size,"km2")|
        str_detect(tech_perarea_size,"km²"),
      "km2",unit),
    unit=ifelse(
      (str_detect(tech_perarea_size,"square meter")|
        str_detect(tech_perarea_size," m2"))|
        str_detect(tech_perarea_size,"squre meter"),
      "m2",unit),
    unit=ifelse(
      (str_detect(tech_perarea_size,"metros lineales")|
         str_detect(tech_perarea_size,"linear meters")),
      "linear meters",unit),
    area=ifelse(area=="5-6",5.5,area) ,
    area=ifelse(area=="50m2",50,area) ,
    area=ifelse(qid==2843,2000,area),
    area=ifelse(qid==2949,360,area),
    area=ifelse(qid==2886,30*40,area),
    area=ifelse(qid==2140,30*15,area),
    area=ifelse(qid==3165,200,area),
    area=ifelse(qid==2284,5*15,area),
    area=ifelse(qid==1890,14000,area),
    area=ifelse(qid==3318,75,area),
    area=ifelse(qid==4329,0.99,area),
    area=ifelse(qid==2900,1,area),
    area=ifelse(qid==1195,36,area),
    unit=ifelse(qid==1195,"m2",unit),
    area=as.double(area),
    area_hectare = ifelse(unit=="hectare",area,NA),
    area_hectare = ifelse(unit=="acre",area/2.471,area_hectare),
    area_hectare = ifelse(unit=="km2",area*100,area_hectare),
    area_hectare = ifelse(unit=="m2",area/10000,area_hectare),
    area_hectare = ifelse(unit=="linear meters",area/10000,area_hectare),
    area_hectare = ifelse(str_detect(tech_perarea_size,"decimal"),area/247,area_hectare),
    area_hectare = ifelse(str_detect(tech_perarea_size,"dhurs"),area/590.7,area_hectare),
    area_hectare = ifelse(str_detect(tech_perarea_size,"rai"),area/6.25,area_hectare),
    area_hectare = ifelse(str_detect(tech_perarea_size,"x")&is.na(area_hectare),area/1000,area_hectare),
    area_hectare = ifelse(str_detect(tech_area_unit_conversion,"a")&is.na(area_hectare),area/1000,area_hectare),
    area_hectare = ifelse(qid==4010,1,area_hectare),
    area_hectare = ifelse(qid==4281,15.43,area_hectare),
    area_hectare = ifelse(qid==1832,area/10000,area_hectare),
    ) %>% 
  select(qid,area_hectare)

a_label <- tribble(
  ~question_number, ~variable, ~label,
  "tech__3__2", "area_hectare", "Total size of area (in hectares)",
) %>% 
  mutate(question_number="4_3",notes="") 

data_clean <- data_clean %>% full_join(a_value,by="qid")
dictionary <- dictionary %>% bind_rows(a_label)

## per Technology unit: specify unit
a_value <- data_raw %>% 
  filter(name9=="tech_input_perunit_unit"|name9=="tech_input_perunit_volume") %>% 
  filter(name11=="value") %>% 
  select(qid,name9,value) %>% 
  spread(name9,value)

a_label <- data_raw %>% 
  filter((name9=="tech_input_perunit_unit"|name9=="tech_input_perunit_volume")&name11=="key") %>% 
  select(name5,name9,value) %>% unique() %>% 
  mutate(notes="") %>% 
  rename(question_number=name5, variable=name9, label=value)

data_clean <- data_clean %>% full_join(a_value,by="qid")
dictionary <- dictionary %>% bind_rows(a_label) %>% filter(label!="Specify volume, length, etc. (if relevant)")

## currency used for cost calculations
a_value <- data_raw %>% 
  filter((name9=="country"&name5=="1_1")|name9=="tech_input_dollar"|
           name9=="tech_input_national_currency"|
           name9=="tech_input_exchange_rate") %>% 
  filter(str_detect(name11,"value")) %>% 
  select(qid,name9,value) %>% 
  spread(name9,value) %>% 
  mutate(currency=ifelse(!is.na(tech_input_dollar),"USD",tech_input_national_currency)) %>% 
  select(-tech_input_national_currency, -tech_input_dollar, -country)

a_label <- data_raw %>% 
  filter((name9=="tech_input_national_currency"|
            name9=="tech_input_exchange_rate")&name11=="key") %>% 
  select(name5,name9,value) %>% unique() %>% 
  mutate(name9=ifelse(name9=="tech_input_national_currency","currency",name9)) %>% 
  mutate(value = ifelse(name9=="currency","Currency used for cost calculations",value)) %>% 
  mutate(notes="") %>% 
  rename(question_number=name5, variable=name9, label=value)
data_clean <- data_clean %>% full_join(a_value,by="qid") 
dictionary <- dictionary %>% 
  bind_rows(a_label) %>% 
  group_by(variable) %>% 
  mutate(ind=row_number()) %>% 
  filter(ind==1) %>% 
  select(-ind) %>% ungroup()

## average wage
a_value <- data_raw %>% 
  filter(name9=="tech_input_average_wage") %>% 
  filter(name11=="value") %>% 
  select(qid,name9,value) %>% 
  spread(name9,value) %>% 
  mutate(tech_input_average_wage=str_extract(tech_input_average_wage, "[0-9]+(?:.[0-9]+)?"))

a_label <- data_raw %>% 
  filter((name9=="tech_input_average_wage")&name11=="key") %>% 
  select(name5,name9,value) %>% unique() %>% 
  mutate(notes="") %>% 
  rename(question_number=name5, variable=name9, label=value)

data_clean <- data_clean %>% full_join(a_value,by="qid") 
dictionary <- dictionary %>% 
  bind_rows(a_label) %>% 
  group_by(variable) %>% 
  mutate(ind=row_number()) %>% 
  filter(ind==1) %>% 
  select(-ind) %>% ungroup()
 
# 4.4 Costs of inputs needed for establishment
### Labour
a1 <- data_raw %>%  filter(name7=="tech_qg_36")
a_value <- f_cost(a1,"labor","tech_input_est_total_costs_pi") 
a_label <- f_cost_lable(a1,"labor","tech_input_est_total_costs_pi")
data_clean <- data_clean %>% full_join(a_value,by="qid") 
dictionary <- dictionary %>% bind_rows(a_label)

a_value <- f_cost(a1,"labor","tech_input_est_percentage_costs") 
a_label <- f_cost_lable(a1,"labor","tech_input_est_percentage_costs")
data_clean <- data_clean %>% full_join(a_value,by="qid") 
dictionary <- dictionary %>% bind_rows(a_label)

### Equipment
a1 <- data_raw %>%  filter(name7=="tech_qg_37")
a_value <- f_cost(a1,"equipment","tech_input_est_total_costs_pi") 
a_label <- f_cost_lable(a1,"equipment","tech_input_est_total_costs_pi")
data_clean <- data_clean %>% full_join(a_value,by="qid") 
dictionary <- dictionary %>% bind_rows(a_label)

a_value <- f_cost(a1,"equipment","tech_input_est_percentage_costs") 
a_label <- f_cost_lable(a1,"equipment","tech_input_est_percentage_costs")
data_clean <- data_clean %>% full_join(a_value,by="qid") 
dictionary <- dictionary %>% bind_rows(a_label)

### Plant material
a1 <- data_raw %>%  filter(name7=="tech_qg_38")
a_value <- f_cost(a1,"plant_material","tech_input_est_total_costs_pi") 
a_label <- f_cost_lable(a1,"plant_material","tech_input_est_total_costs_pi")
data_clean <- data_clean %>% full_join(a_value,by="qid") 
dictionary <- dictionary %>% bind_rows(a_label)

a_value <- f_cost(a1,"plant_material","tech_input_est_percentage_costs") 
a_label <- f_cost_lable(a1,"plant_material","tech_input_est_percentage_costs")
data_clean <- data_clean %>% full_join(a_value,by="qid") 
dictionary <- dictionary %>% bind_rows(a_label)

### Fertilizers 
a1 <- data_raw %>%  filter(name7=="tech_qg_218")
a_value <- f_cost(a1,"fertilizers","tech_input_est_total_costs_pi") 
a_label <- f_cost_lable(a1,"fertilizers","tech_input_est_total_costs_pi")
data_clean <- data_clean %>% full_join(a_value,by="qid") 
dictionary <- dictionary %>% bind_rows(a_label)

a_value <- f_cost(a1,"fertilizers","tech_input_est_percentage_costs") 
a_label <- f_cost_lable(a1,"fertilizers","tech_input_est_percentage_costs")
data_clean <- data_clean %>% full_join(a_value,by="qid") 
dictionary <- dictionary %>% bind_rows(a_label)

### Construction
a1 <- data_raw %>%  filter(name7=="tech_qg_39")
a_value <- f_cost(a1,"construction","tech_input_est_total_costs_pi") 
a_label <- f_cost_lable(a1,"construction","tech_input_est_total_costs_pi")
data_clean <- data_clean %>% full_join(a_value,by="qid") 
dictionary <- dictionary %>% bind_rows(a_label)

a_value <- f_cost(a1,"construction","tech_input_est_percentage_costs") 
a_label <- f_cost_lable(a1,"construction","tech_input_est_percentage_costs")
data_clean <- data_clean %>% full_join(a_value,by="qid") 
dictionary <- dictionary %>% bind_rows(a_label)

### Other
a1 <- data_raw %>%  filter(name7=="tech_qg_40")
a_value <- f_cost(a1,"other","tech_input_est_total_costs_pi") 
a_label <- f_cost_lable(a1,"other","tech_input_est_total_costs_pi")
data_clean <- data_clean %>% full_join(a_value,by="qid") 
dictionary <- dictionary %>% bind_rows(a_label)

a_value <- f_cost(a1,"other","tech_input_est_percentage_costs") 
a_label <- f_cost_lable(a1,"other","tech_input_est_percentage_costs")
data_clean <- data_clean %>% full_join(a_value,by="qid") 
dictionary <- dictionary %>% bind_rows(a_label)

### Total costs for establishment of the Technology	
a1 <- data_raw %>%  filter(name7=="tech_qg_222")
a_value <- f_single_data(a1,"4_5","tech_input_est_total_costs")
a_label <- f_single_dict(a1,"4_5","tech_input_est_total_costs")
dictionary <- dictionary %>% bind_rows(a_label)
data_clean <- data_clean %>% full_join(a_value,by="qid")

### If land user bore less than 100% of costs, indicate who covered the remaining costs	
a1 <- data_raw %>%  filter(name7=="tech_qg_93")
a_value <- f_single_data(a1,"4_5","tech_input_est_remaining_costs")
a_label <- f_single_dict(a1,"4_5","tech_input_est_remaining_costs")
dictionary <- dictionary %>% bind_rows(a_label)
data_clean <- data_clean %>% full_join(a_value,by="qid")

### Total costs for establishment of the Technology in USD	
a1 <- data_raw %>%  filter(name7=="tech_qg_232")
a_value <- f_single_data(a1,"4_5","tech_input_est_total_costs_usd")
a_label <- f_single_dict(a1,"4_5","tech_input_est_total_costs_usd")
dictionary <- dictionary %>% bind_rows(a_label)
data_clean <- data_clean %>% full_join(a_value,by="qid")

## 4.6 Costs of inputs and recurrent activities needed for maintenance (per year)
### Labour
a1 <- data_raw %>%  filter(name7=="tech_qg_45")
a_value <- f_cost(a1,"labor","tech_input_maint_total_costs_pi") 
a_label <- f_cost_lable(a1,"labor","tech_input_maint_total_costs_pi")
data_clean <- data_clean %>% full_join(a_value,by="qid") 
dictionary <- dictionary %>% bind_rows(a_label)

a_value <- f_cost(a1,"labor","tech_input_maint_percentage_costs") 
a_label <- f_cost_lable(a1,"labor","tech_input_maint_percentage_costs")
data_clean <- data_clean %>% full_join(a_value,by="qid") 
dictionary <- dictionary %>% bind_rows(a_label)

### Equipment
a1 <- data_raw %>%  filter(name7=="tech_qg_46")
a_value <- f_cost(a1,"equipment","tech_input_maint_total_costs_pi") 
a_label <- f_cost_lable(a1,"equipment","tech_input_maint_total_costs_pi")
data_clean <- data_clean %>% full_join(a_value,by="qid") 
dictionary <- dictionary %>% bind_rows(a_label)

a_value <- f_cost(a1,"equipment","tech_input_maint_percentage_costs") 
a_label <- f_cost_lable(a1,"equipment","tech_input_maint_percentage_costs")
data_clean <- data_clean %>% full_join(a_value,by="qid") 
dictionary <- dictionary %>% bind_rows(a_label)

### Plant material
a1 <- data_raw %>%  filter(name7=="tech_qg_47")
a_value <- f_cost(a1,"plant_material","tech_input_maint_total_costs_pi") 
a_label <- f_cost_lable(a1,"plant_material","tech_input_maint_total_costs_pi")
data_clean <- data_clean %>% full_join(a_value,by="qid") 
dictionary <- dictionary %>% bind_rows(a_label)

a_value <- f_cost(a1,"plant_material","tech_input_maint_percentage_costs") 
a_label <- f_cost_lable(a1,"plant_material","tech_input_maint_percentage_costs")
data_clean <- data_clean %>% full_join(a_value,by="qid") 
dictionary <- dictionary %>% bind_rows(a_label)

### Fertilizers 
a1 <- data_raw %>%  filter(name7=="tech_qg_219")
a_value <- f_cost(a1,"fertilizers","tech_input_maint_total_costs_pi") 
a_label <- f_cost_lable(a1,"fertilizers","tech_input_maint_total_costs_pi")
data_clean <- data_clean %>% full_join(a_value,by="qid") 
dictionary <- dictionary %>% bind_rows(a_label)

a_value <- f_cost(a1,"fertilizers","tech_input_maint_percentage_costs") 
a_label <- f_cost_lable(a1,"fertilizers","tech_input_maint_percentage_costs")
data_clean <- data_clean %>% full_join(a_value,by="qid") 
dictionary <- dictionary %>% bind_rows(a_label)

### Construction
a1 <- data_raw %>%  filter(name7=="tech_qg_48")
a_value <- f_cost(a1,"construction","tech_input_maint_total_costs_pi") 
a_label <- f_cost_lable(a1,"construction","tech_input_maint_total_costs_pi")
data_clean <- data_clean %>% full_join(a_value,by="qid") 
dictionary <- dictionary %>% bind_rows(a_label)

a_value <- f_cost(a1,"construction","tech_input_maint_percentage_costs") 
a_label <- f_cost_lable(a1,"construction","tech_input_maint_percentage_costs")
data_clean <- data_clean %>% full_join(a_value,by="qid") 
dictionary <- dictionary %>% bind_rows(a_label)

### Other
a1 <- data_raw %>%  filter(name7=="tech_qg_49")
a_value <- f_cost(a1,"other","tech_input_maint_total_costs_pi") 
a_label <- f_cost_lable(a1,"other","tech_input_maint_total_costs_pi")
data_clean <- data_clean %>% full_join(a_value,by="qid") 
dictionary <- dictionary %>% bind_rows(a_label)

a_value <- f_cost(a1,"other","tech_input_maint_percentage_costs") 
a_label <- f_cost_lable(a1,"other","tech_input_maint_percentage_costs")
data_clean <- data_clean %>% full_join(a_value,by="qid") 
dictionary <- dictionary %>% bind_rows(a_label)

### Total costs for mntablishment of the Technology	
a1 <- data_raw %>%  filter(name7=="tech_qg_223")
a_value <- f_single_data(a1,"4_7","tech_input_maint_total_costs")
a_label <- f_single_dict(a1,"4_7","tech_input_maint_total_costs")
dictionary <- dictionary %>% bind_rows(a_label)
data_clean <- data_clean %>% full_join(a_value,by="qid")

### If land user bore less than 100% of costs, indicate who covered the remaining costs	
a1 <- data_raw %>%  filter(name7=="tech_qg_52")
a_value <- f_single_data(a1,"4_7","tech_input_maint_remaining_costs")
a_label <- f_single_dict(a1,"4_7","tech_input_maint_remaining_costs")
dictionary <- dictionary %>% bind_rows(a_label)
data_clean <- data_clean %>% full_join(a_value,by="qid")

### Total costs for mntablishment of the Technology in USD	
a1 <- data_raw %>%  filter(name7=="tech_qg_233")
a_value <- f_single_data(a1,"4_7","tech_input_maint_total_costs_usd")
a_label <- f_single_dict(a1,"4_7","tech_input_maint_total_costs_usd")
dictionary <- dictionary %>% bind_rows(a_label)
data_clean <- data_clean %>% full_join(a_value,by="qid")

# Explore cost comments
a_value <- data_raw %>% 
  filter(name5=="4_7"|name5=="4_5") %>% 
  mutate(family_labor=ifelse(str_detect(value,"family"),1,0)) %>% 
  group_by(qid) %>% 
  summarise(family_labor=sum(family_labor))

a_label <- tribble(
  ~question_number, ~variable, ~label,
  "tech__3__2", "family_labor", "Uses family labor",
) %>% 
  mutate(question_number="4_5",notes="") 
dictionary <- dictionary %>% bind_rows(a_label)
data_clean <- data_clean %>% full_join(a_value,by="qid")

a1 <- data_raw %>% filter(name5=="4_8") 
a_value <- f_single_data(a1,"4_8","tech_input_determinate_factors")
a_label <- f_single_dict(a1,"4_8","tech_input_determinate_factors")
dictionary <- dictionary %>% bind_rows(a_label)
data_clean <- data_clean %>% full_join(a_value,by="qid")


# Impute currency
data_clean <-   data_clean %>% 
  mutate(currency=ifelse(is.na(currency)&
                                         tech_input_est_total_costs==tech_input_est_total_costs_usd,
                                       "USD",currency)) %>% 
  mutate(currency=ifelse(is.na(currency)&
                           tech_input_maint_total_costs==tech_input_maint_total_costs_usd,
                         "USD",currency)) %>% 
  mutate(tech_input_exchange_rate	=ifelse(is.na(tech_input_exchange_rate	)&
                           tech_input_est_total_costs==tech_input_est_total_costs_usd,
                         1,tech_input_exchange_rate	)) %>% 
  mutate(tech_input_exchange_rate	=ifelse(is.na(tech_input_exchange_rate	)&
                           tech_input_maint_total_costs==tech_input_maint_total_costs_usd,
                         1,tech_input_exchange_rate	))


# SAVE DATA
filename = here::here("03_processed_data","04_Data_cost.rds")
saveRDS(data_clean,filename)
filename = here::here("03_processed_data","04_Dictionary_cost.rds")
saveRDS(dictionary,filename)
