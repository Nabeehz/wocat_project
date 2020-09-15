# WOCAT: COST AND BENEFIT DATABASE
# EXTRACT GENERAL INFO AND CLASSIFICATION VARIABLES
library(here)
library(tidyverse)
library(lubridate)
if(!exists("data_raw")){
  filename = here::here("02_raw_data","02_Tech_data_raw.rds")
  data_raw<-readRDS(filename)
}

# SECTION 1: GENERAL INFORMATION
data_raw <- data_raw %>% mutate(qid=q_code) %>% 
  mutate(qid=as.numeric(str_replace(qid,"technologies_",""))) %>% 
  mutate(name5=str_replace(name5,"tech__","")) %>% 
  mutate(name5=str_replace(name5,"__","_"))
  
## Questionnaire id
data_clean <- data_raw %>% 
  select(qid) %>% unique 

dictionary <- tribble(
  ~question_number,~variable,~label,~notes,
  "0","qid","Questionnaire ID number", ""
)

## 1.1 Technology name
a_value <- data_raw %>% 
  filter(name9=="name"&name11=="value") %>% 
  select(qid,name9,value) %>% 
  spread(name9,value)

a_label <- data_raw %>% 
  filter(name9=="name"&name11=="key") %>% 
  select(name5,name9,value) %>% unique() %>% 
  mutate(notes="") %>% 
  rename(question_number=name5, variable=name9, label=value)

data_clean <- data_clean %>% full_join(a_value,by="qid")
dictionary <- dictionary %>% bind_rows(a_label)

## 1.4 Declaration on sustainability of the described Technology
a_value <- data_raw %>% 
  filter(name9=="tech_sustainability"&name11=="value") %>% 
  select(qid,name9,value) %>% 
  spread(name9,value) %>% 
  rename(not_SLM=tech_sustainability) %>% 
  mutate(not_SLM=as_factor(not_SLM))

a_label <- data_raw %>% 
  filter(name9=="tech_sustainability"&name11=="key") %>% 
  select(name5,name9,value) %>% unique()%>% 
  mutate(notes="") %>% 
  rename(question_number=name5, variable=name9, label=value) %>% 
  mutate(variable="not_SLM")

data_clean <- data_clean %>% full_join(a_value,by="qid")
dictionary <- dictionary %>% bind_rows(a_label)
#-----
a_value <- data_raw %>% 
  filter(name9=="tech_sustainability_indicate"&name11=="value") %>% 
  select(qid,name9,value) %>% 
  spread(name9,value) %>% rename(not_SLM_comments=tech_sustainability_indicate)

a_label <- data_raw %>% 
  filter(name9=="tech_sustainability"&name11=="key") %>% 
  select(name5,name9,value) %>% unique()%>% 
  mutate(notes="") %>% 
  rename(question_number=name5, variable=name9, label=value) %>% 
  mutate(variable="not_SLM_comments") %>% 
  mutate(label=str_c(label," (comments)"))

data_clean <- data_clean %>% full_join(a_value,by="qid")
dictionary <- dictionary %>% bind_rows(a_label)

## 2.1 Short description of the Technology 
a_value <- data_raw %>% 
  filter(name9=="tech_definition"&name11=="value") %>% 
  select(qid,name9,value) %>% spread(name9,value)

a_label <- data_raw %>% 
  filter(name9=="tech_definition"&name11=="key") %>% 
  select(name5,name9,value) %>% unique()%>% 
  mutate(notes="") %>% 
  rename(question_number=name5, variable=name9, label=value)
data_clean <- data_clean %>% full_join(a_value,by="qid")
dictionary <- dictionary %>% bind_rows(a_label)
## 2.5 Country/ region/ locations where the Technology has been applied and which are covered by this assessment
a_value <- data_raw %>% 
  filter((name9=="country"|
            name9=="state_province"|
            name9=="further_location")
         &name11=="value"&name5=="2_5") %>% 
  select(qid,name9,value) %>% spread(name9,value)

a_label <- data_raw %>% 
  filter((name9=="country"|
            name9=="state_province"|
            name9=="further_location")&name5=="2_5"&name11=="key") %>% 
  select(name5,name9,value) %>% unique()%>% 
  mutate(notes="") %>% 
  rename(question_number=name5, variable=name9, label=value)


data_clean <- data_clean %>% full_join(a_value,by="qid")
dictionary <- dictionary %>% bind_rows(a_label)

## Number of sites considered/ analysed in the documentation of this Technology
a_value <- data_raw %>% 
  filter((name9=="tech_sites_considered")
         &name11=="values"&name5=="2_5") %>% 
  select(qid,name9,value) %>% spread(name9,value)  %>% 
  mutate(tech_sites_considered=as_factor(tech_sites_considered))

a_label <- data_raw %>% 
  filter((name9=="tech_sites_considered")&name5=="2_5"&name11=="key") %>% 
  select(name5,name9,value) %>% unique()%>% 
  mutate(notes="") %>% 
  rename(question_number=name5, variable=name9, label=value)

data_clean <- data_clean %>% full_join(a_value,by="qid")
dictionary <- dictionary %>% bind_rows(a_label)

## Specify the spread of the Technology:
a_value <- data_raw %>% 
  filter((name9=="tech_spread_tech")
         &name11=="values"&name5=="2_5") %>% 
  select(qid,name9,value) %>% spread(name9,value)  %>% 
  mutate(tech_spread_tech=as_factor(tech_spread_tech))

a_label <- data_raw %>% 
  filter((name9=="tech_spread_tech")&name5=="2_5"&name11=="key") %>% 
  select(name5,name9,value) %>% unique()%>% 
  mutate(notes="") %>% 
  rename(question_number=name5, variable=name9, label=value)

data_clean <- data_clean %>% full_join(a_value,by="qid")
dictionary <- dictionary %>% bind_rows(a_label)


### If the Technology is evenly spread over an area, specify area covered (in km2):
a_value <- data_raw %>% 
  filter((name9=="tech_spread_area_precise")
         &name11=="value"&name5=="2_5") %>% 
  select(qid,name9,value) %>% spread(name9,value) %>% 
  mutate(tech_spread_area_precise=as.numeric(tech_spread_area_precise))

a_label <- data_raw %>% 
  filter((name9=="tech_spread_area_precise")&name5=="2_5"&name11=="key") %>% 
  select(name5,name9,value) %>% unique()%>% 
  mutate(notes="") %>% 
  rename(question_number=name5, variable=name9, label=value)

data_clean <- data_clean %>% full_join(a_value,by="qid")
dictionary <- dictionary %>% bind_rows(a_label)

a_value <- data_raw %>% 
  filter((name9=="tech_spread_area")
         &name11=="values"&name5=="2_5") %>% 
  select(qid,name9,value) %>% spread(name9,value) %>% 
  mutate(tech_spread_area=as_factor(tech_spread_area))

a_label <- data_raw %>% 
  filter((name9=="tech_spread_area")&name5=="2_5"&name11=="key") %>% 
  select(name5,name9,value) %>% unique()%>% 
  mutate(notes="") %>% 
  rename(question_number=name5, variable=name9, label=value)

data_clean <- data_clean %>% full_join(a_value,by="qid")
dictionary <- dictionary %>% bind_rows(a_label)

### Is/ are the technology site(s) located in a permanently protected area? 
a_value <- data_raw %>% 
  filter((name9=="tech_location_protected")
         &name11=="value"&name5=="2_5") %>% 
  select(qid,name9,value) %>% spread(name9,value) %>% 
  mutate(tech_location_protected=as_factor(tech_location_protected))

a_label <- data_raw %>% 
  filter((name9=="tech_location_protected")&name5=="2_5"&name11=="key") %>% 
  select(name5,name9,value) %>% unique()%>% 
  mutate(notes="") %>% 
  rename(question_number=name5, variable=name9, label=value)

data_clean <- data_clean %>% full_join(a_value,by="qid")
dictionary <- dictionary %>% bind_rows(a_label)

a_value <- data_raw %>% 
  filter((name9=="tech_location_protected_specify")
         &name11=="value"&name5=="2_5") %>% 
  select(qid,name9,value) %>% spread(name9,value) 

a_label <- data_raw %>% 
  filter((name9=="tech_location_protected_specify")&name5=="2_5"&name11=="key") %>% 
  select(name5,name9,value) %>% unique()%>% 
  mutate(notes="") %>% 
  rename(question_number=name5, variable=name9, label=value)

data_clean <- data_clean %>% full_join(a_value,by="qid")
dictionary <- dictionary %>% bind_rows(a_label)

## 2.6 Date of implementation
a_value <- data_raw %>% 
  filter(name5=="2_6"&str_detect(name11,"value")) %>% 
  select(qid,name9,value) %>% spread(name9,value) %>% 
  mutate(tech_implementation_year=as.integer(tech_implementation_year)) %>% 
  mutate(tech_implementation_decades=as_factor(tech_implementation_decades))

a_label <- data_raw %>% 
  filter(name5=="2_6"&name11=="key") %>% 
  select(name5,name9,value) %>% unique()%>% 
  mutate(notes="") %>% 
  rename(question_number=name5, variable=name9, label=value)

data_clean <- data_clean %>% full_join(a_value,by="qid")
dictionary <- dictionary %>% bind_rows(a_label)

# SECTION 7 Date of documentation
a_value <- data_raw %>% 
  filter((name9=="date_documentation"|name9=="tech_date_comments")
         &name11=="value"&name5=="7_1") %>% 
  select(qid,name9,value) %>% spread(name9,value) %>% 
  mutate(year_documentation=
           if_else(str_detect(date_documentation,"/"),
                                    format(as.Date(date_documentation, format="%d/%m/%Y"),"%Y"),
                   date_documentation)) %>% 
  mutate(year_documentation=ifelse(date_documentation=="2012/2018",2018,year_documentation)) %>% 
  mutate(year_documentation=ifelse(date_documentation=="2013-2017 ",2017,year_documentation)) %>% 
  mutate(year_documentation=ifelse(is.na(year_documentation),2018,year_documentation)) %>% ## warning: guess for remainin two
  mutate(year_documentation=as.integer(year_documentation)) 

a_label <- data_raw %>% 
  filter((name9=="date_documentation"|name9=="tech_date_comments")&
           name11=="key"&name5=="7_1") %>% 
  select(name5,name9,value) %>% unique()%>% 
  add_row(name5="7_1",name9="year_documentation",value="Documentation year") %>% 
  mutate(notes="") %>% 
  rename(question_number=name5, variable=name9, label=value)

data_clean <- data_clean %>% full_join(a_value,by="qid")
dictionary <- dictionary %>% bind_rows(a_label)

# SECTION 3:  CLASSIFICATION OF SLM TECHNOLOGY
## 3.2 Current land use type(s) where the Technology is applied
a_value <- data_raw %>% 
  filter(name5=="3_2") %>% 
  filter(str_detect(value,"tech_lu")) %>% 
  mutate(tech_lu_cropland=if_else(str_detect(value,"tech_lu_cropland"),1,0)) %>% 
  mutate(tech_lu_grazingland=if_else(str_detect(value,"tech_lu_grazingland"),1,0)) %>%
  mutate(tech_lu_forest=if_else(str_detect(value,"tech_lu_forest"),1,0)) %>% 
  mutate(tech_lu_settlements=if_else(str_detect(value,"tech_lu_settlements"),1,0)) %>% 
  mutate(tech_lu_waterways=if_else(str_detect(value,"tech_lu_waterways"),1,0)) %>% 
  mutate(tech_lu_mines=if_else(str_detect(value,"tech_lu_mines"),1,0)) %>% 
  mutate(tech_lu_unproductive=if_else(str_detect(value,"tech_lu_unproductive"),1,0)) %>% 
  mutate(tech_lu_protected=if_else(str_detect(value,"tech_lu_protected"),1,0)) %>% 
  mutate(tech_lu_other=if_else(str_detect(value,"tech_lu_other"),1,0)) %>% 
  select(qid,starts_with("tech_lu", ignore.case = TRUE)) %>% 
  group_by(qid) %>% 
  summarize_if(is.numeric,sum,na.rm = TRUE) %>% 
  set_names(~stringr::str_replace_all(.,"tech_lu_", "current_landuse_")) %>% 
  mutate(across(contains("current_landuse"),as_factor))

a_label <- tribble(
    ~question_number, ~variable, ~label,
    "tech__3__2", "tech_lu_cropland", "Current land use: cropland",
    "tech__3__2", "tech_lu_grazingland", "Current land use: grazing",
    "tech__3__2", "tech_lu_forest", "Current land use: forest",
    "tech__3__2", "tech_lu_settlements", "Current land use: settlements",
    "tech__3__2", "tech_lu_waterways", "Current land use: waterways",
    "tech__3__2", "tech_lu_mines", "Current land use: mines",
    "tech__3__2", "tech_lu_unproductive", "Current land use: unproductive",
    "tech__3__2", "tech_lu_protected", "Current land use: potected areas",
    "tech__3__2", "tech_lu_other", "Current land use: other"
  ) %>% 
  mutate(question_number="3_2",variable=str_replace(variable,"tech_lu","current_landuse"),notes="") 

data_clean <- data_clean %>% full_join(a_value,by="qid")
dictionary <- dictionary %>% bind_rows(a_label)

## 3.4 Water supply for the land on which the Technology is applied
a_value <- data_raw %>% 
  filter((name9=="tech_watersupply"|name9=="tech_watersupply_other"|name9=="tech_watersupply_comments")
         &str_detect(name11,"value")&name5=="3_3") %>% 
  select(qid,name9,value) %>% spread(name9,value) %>% 
  mutate(tech_watersupply=as_factor(tech_watersupply))

a_label <- data_raw %>% 
  filter((name9=="tech_watersupply"|name9=="tech_watersupply_other"|name9=="tech_watersupply_comments")&
           name11=="key"&name5=="3_3") %>% 
  select(name5,name9,value) %>% unique()%>% 
  mutate(notes="") %>% 
  rename(question_number=name5, variable=name9, label=value)

data_clean <- data_clean %>% full_join(a_value,by="qid")
dictionary <- dictionary %>% bind_rows(a_label)

## 3.6 SLM measures comprising the Technology
a_value <- data_raw %>% 
  filter(name5=="3_6"&str_detect(value,"tech_measures_")) %>% 
  mutate(
    slm_measure_agronomic = if_else(str_detect(value,"tech_measures_agronomic"),1,0) ,
    slm_measure_vegetative = if_else(str_detect(value,"tech_measures_vegetative"),1,0) ,
    slm_measure_management = if_else(str_detect(value,"tech_measures_management"),1,0) ,
    slm_measure_structural = if_else(str_detect(value,"tech_measures_structural"),1,0)
  ) %>% 
  select(qid,starts_with("slm_measure_", ignore.case = TRUE)) %>% 
  group_by(qid) %>% 
  summarize_if(is.numeric,sum,na.rm = TRUE) %>% 
  mutate(across(contains("slm_measure_"),as_factor)) 

a_label <- tribble(
  ~question_number, ~variable, ~label,
  "3_6", "slm_measure_agronomic", "SLM measures comprising the Technology: agronomic",
  "3_6", "slm_measure_vegetative", "SLM measures comprising the Technology: vegetative",
  "3_6", "slm_measure_management", "SLM measures comprising the Technology: management",
  "3_6", "slm_measure_structural", "SLM measures comprising the Technology: structural",
) %>% 
  mutate(notes="") 

data_clean <- data_clean %>% full_join(a_value,by="qid")
dictionary <- dictionary %>% bind_rows(a_label)

## 3.7 Main types of land degradation addressed by the Technology
a <- data_raw %>% filter(name5=="3_7") %>% select(name9) %>% unique()

a_value <- data_raw %>% 
  filter(name5=="3_7"&str_detect(value,"degradation_")) %>% 
  mutate(
    degradation_erosion_water = if_else(str_detect(value,"degradation_erosion_water"),1,0) ,
    degradation_degradation_biological = if_else(str_detect(value,"degradation_biological"),1,0) ,
    degradation_erosion_wind = if_else(str_detect(value,"degradation_erosion_wind"),1,0) ,
    degradation_physical = if_else(str_detect(value,"degradation_physical"),1,0) ,
    degradation_water = if_else(str_detect(value,"degradation_water"),1,0) ,
    degradation_chemical = if_else(str_detect(value,"degradation_chemical"),1,0) ,
    degradation_other = if_else(str_detect(value,"degradation_other"),1,0) 
  ) %>% 
  select(qid,starts_with("degradation_", ignore.case = TRUE)) %>% 
  group_by(qid) %>% 
  summarize_if(is.numeric,sum,na.rm = TRUE) %>% 
  mutate(across(contains("degradation_"),as_factor)) 

a_label <- tribble(
  ~question_number, ~variable, ~label,
  "3_7", "degradation_erosion_water", "Type of degradation addressed: soil erosion by water",
  "3_7", "degradation_degradation_biological", "Type of degradation addressed: biological degradation",
  "3_7", "degradation_erosion_wind", "Type of degradation addressed: soil erosion by wind",
  "3_7", "degradation_physical", "Type of degradation addressed: physical soil degradation",
  "3_7", "degradation_water", "Type of degradation addressed: water degradation",
  "3_7", "degradation_chemical", "Type of degradation addressed: chemical soil degradation",
  "3_7", "degradation_other", "Type of degradation addressed: other"
) %>% 
  mutate(notes="") 
data_clean <- data_clean %>% full_join(a_value,by="qid")
dictionary <- dictionary %>% bind_rows(a_label)

## 3.8 Prevention, reduction, or restoration of land degradation 
a <- data_raw %>% filter(name5=="3_8") %>% select(name9) %>% unique()
a_value <- data_raw %>% 
  filter(name5=="3_8"&name9=="tech_prevention"&str_detect(name11,"value")) %>% 
  mutate(
    goal_reduce_land_degradation = ifelse(value=="reduce land degradation",1,0) ,
    goal_rehabilitate_degraded_land = ifelse(value=="restore/ rehabilitate severely degraded land",1,0) ,
    goal_prevent_land_degradation = ifelse(value=="prevent land degradation",1,0) ,
    goal_adapt_to_land_degradation = ifelse(value=="adapt to land degradation",1,0) ,
    goal_land_degradation_na = ifelse(value=="not applicable",1,0) 
  ) %>% 
  select(qid,starts_with("goal", ignore.case = TRUE)) %>% 
  group_by(qid) %>% 
  summarize_if(is.numeric,sum,na.rm = TRUE) %>% 
  mutate(across(contains("goal"),as_factor)) 

a_label <- tribble(
  ~question_number, ~variable, ~label,
  "3_8", "goal_reduce_land_degradation", "Goal: reduce land degradation",
  "3_8", "goal_rehabilitate_degraded_land", "Goal: restore/ rehabilitate severely degraded land",
  "3_8", "goal_prevent_land_degradation", "Goal: prevent land degradation",
  "3_8", "goal_adapt_to_land_degradation", "Goal: adapt to land degradation",
  "3_8", "goal_land_degradation_na", "Land degradation not applicable",
) %>% 
  mutate(notes="") 
data_clean <- data_clean %>% full_join(a_value,by="qid")
dictionary <- dictionary %>% bind_rows(a_label)

# SAVE DATA
filename = here::here("03_processed_data","03_Data_general.rds")
saveRDS(data_clean,filename)
filename = here::here("03_processed_data","03_Dictionary_general.rds")
saveRDS(dictionary,filename)
# a <- data_clean %>% 
#   filter(not_SLM=="Yes")
