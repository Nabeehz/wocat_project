# WOCAT: COST AND BENEFIT DATABASE
# EXTRACT IMAPCT VARIABLES
library(here)
library(tidyverse)
library(lubridate)
if(!exists("data_raw")){
  filename = here::here("02_raw_data","02_Tech_data_raw.rds")
  data_raw<-readRDS(filename)
}
filename = here::here("01_code","00_Functions.R")
source(filename)

## Questionnaire id
data_raw <- data_raw %>% mutate(qid=q_code) %>% 
  mutate(qid=as.numeric(str_replace(qid,"technologies_",""))) %>% 
  mutate(name5=str_replace(name5,"tech__","")) %>% 
  mutate(name5=str_replace(name5,"__","_"))

data_clean <- data_raw %>% 
  select(qid) %>% unique 

dictionary <- tribble(
  ~question_number,~variable,~label,~notes,
  "0","Qid","Questionnaire ID number", ""
)

# 6.1 On-site impacts the Technology has shown
## Impact levels
a_value <- data_raw %>% 
  filter(str_detect(name13,"tech_impacts_")&
           !str_detect(name13,"quant")&
           !str_detect(name13,"specify")&
           !str_detect(name13,"comment")&
           !str_detect(name13,"other")&
           name15=="value") %>% 
  select(qid,name13,value) %>% 
  mutate(value=as_factor(value)) %>% 
  mutate(name13=str_replace(name13,"tech_","onsite_")) %>% 
  spread(name13,value) 

a_label <- data_raw %>% 
  filter(str_detect(name13,"tech_impacts_")&
           !str_detect(name13,"quant")&
           !str_detect(name13,"specify")&
           !str_detect(name13,"comment")&
           !str_detect(name13,"other")&
           name14=="label") %>% 
  select(name5,name13,value) %>% unique() %>% 
  mutate(name13=str_replace(name13,"tech_","onsite_")) %>% 
  mutate(notes="") %>% 
  rename(question_number=name5, variable=name13, label=value) %>% 
  mutate(label=str_to_sentence(str_c(label,": onsite impacts")))

data_clean <- data_clean %>% full_join(a_value,by="qid")
dictionary <- dictionary %>% bind_rows(a_label)

# 6.2 Off-site impacts the Technology has shown
a_value <- data_raw %>% 
  filter(str_detect(name9,"tech_impacts_")&
           !str_detect(name9,"quant")&
           !str_detect(name9,"specify")&
           !str_detect(name9,"comment")&
           !str_detect(name9,"other")&
           name11=="value") %>% 
  select(qid,name9,value) %>% 
  mutate(value=as_factor(value)) %>% 
  mutate(name9=str_replace(name9,"tech_","offsite_")) %>% 
  spread(name9,value) 

a_label <- data_raw %>% 
  filter(str_detect(name9,"tech_impacts_")&
           !str_detect(name9,"quant")&
           !str_detect(name9,"specify")&
           !str_detect(name9,"comment")&
           !str_detect(name9,"other")&
           name10=="label") %>% 
  select(name5,name9,value) %>% unique() %>% 
  mutate(name9=str_replace(name9,"tech_","offsite_")) %>% 
  mutate(notes="") %>% 
  rename(question_number=name5, variable=name9, label=value) %>% 
  mutate(label=str_to_sentence(str_c(label,": offsite impacts")))
data_clean <- data_clean %>% full_join(a_value,by="qid")
dictionary <- dictionary %>% bind_rows(a_label)

# 6.3 Exposure and sensitivity of the Technology to gradual climate change and climate-related extremes/
#     disasters (as perceived by land users) 
a0 <- data_raw %>% 
  filter(str_detect(name5,"6_3"))

a1 <- a0 %>% 
  filter(name12=="label") %>%  
  select(name11,value) %>% unique %>% 
  rename(events=value)

a0 <- a0 %>% full_join(a1, by="name11") %>% 
  mutate(name13=str_replace(name13,"tech_exposure",str_replace(events," ","_"))) %>% 
  mutate(label=str_c(str_to_sentence(events),": ",value))


a_value <- a0 %>% 
  filter(!str_detect(name13,"season")) %>% 
  filter(!str_detect(name13,"specify")) %>% 
  filter(!str_detect(name13,"comments")) %>% 
  filter(!str_detect(name13,"other")) %>% 
  filter(name15=="value") %>% 
  select(qid,name13,value) %>% 
  mutate(value=as_factor(value)) %>% 
  group_by(qid) %>% 
  spread(name13,value)

a_label <- a0 %>% 
  filter(!str_detect(name13,"season")) %>% 
  filter(!str_detect(name13,"specify")) %>% 
  filter(!str_detect(name13,"comments")) %>% 
  filter(!str_detect(name13,"other")) %>% 
  filter(name15=="key") %>% 
  select(name5,name13,label) %>% 
  unique() %>% 
  rename(question_number=name5,variable=name13)
data_clean <- data_clean %>% full_join(a_value,by="qid")
dictionary <- dictionary %>% bind_rows(a_label)

### Seasonal temperature
a2 <- a0 %>% 
  filter(str_detect(name13,"seasonal_temperature_season")&name15=="value") %>% 
  group_by(qid,name13) %>% 
  mutate(ind=row_number()) %>% 
  ungroup() %>% 
  select(qid,ind,value) %>% 
  rename(season=value)

a3 <- a0 %>% 
  filter(name13=="seasonal_temperature_incrdecr" & name15=="value") %>% 
  group_by(qid,name13) %>% 
  mutate(ind=row_number()) %>% 
  ungroup() 

a_value <- a2 %>% full_join(a3, by=c("qid","ind")) %>% 
  mutate(name13=str_c(name13,"_",season)) %>% 
  select(qid,name13,value) %>% 
  filter(value!="") %>% 
  group_by(qid) %>% 
  mutate(ind=row_number()) %>% 
  filter(ind==1) %>% select(-ind) %>% 
  mutate(value=as_factor(value)) %>% 
  spread(name13,value)

data_clean <- data_clean %>% full_join(a_value,by="qid")

a3 <- a0 %>% 
  filter(name13=="seasonal_temperature_sensitivity" & name15=="value") %>% 
  group_by(qid,name13) %>% 
  mutate(ind=row_number()) %>% 
  ungroup() 

a_value <- a2 %>% full_join(a3, by=c("qid","ind")) %>% 
  mutate(name13=str_c(name13,"_",season)) %>% 
  select(qid,name13,value) %>% 
  filter(value!="") %>% 
  group_by(qid) %>% 
  mutate(ind=row_number()) %>% 
  filter(ind==1) %>% 
  select(-ind) %>% 
  mutate(value=as_factor(value)) %>% 
  spread(name13,value)

data_clean <- data_clean %>% full_join(a_value,by="qid")

a_label <- a0 %>%
  filter(name15=="key"&!str_detect(name13,"_season")&str_detect(label,"Seasonal")) %>%
  select(name5,name13,label) %>%
  unique %>%
  rename(question_number=name5,variable=name13) %>%
  filter(!str_detect(label,"climatic"))

dictionary <- dictionary %>% bind_rows(a_label)

### Seasonal rainfall
a2 <- a0 %>% 
  filter(str_detect(name13,"seasonal_rainfall_season")&name15=="value") %>% 
  group_by(qid,name13) %>% 
  mutate(ind=row_number()) %>% 
  ungroup() %>% 
  select(qid,ind,value) %>% 
  rename(season=value)

a3 <- a0 %>% 
  filter(name13=="seasonal_rainfall_incrdecr" & name15=="value") %>% 
  group_by(qid,name13) %>% 
  mutate(ind=row_number()) %>% 
  ungroup() 

a_value <- a2 %>% full_join(a3, by=c("qid","ind")) %>% 
  mutate(name13=str_c(name13,"_",season)) %>% 
  select(qid,name13,value) %>% 
  filter(value!="") %>% 
  group_by(qid) %>% 
  mutate(ind=row_number()) %>% 
  filter(ind==1) %>% select(-ind) %>% 
  mutate(value=as_factor(value)) %>% 
  spread(name13,value)

data_clean <- data_clean %>% full_join(a_value,by="qid")

a3 <- a0 %>% 
  filter(name13=="seasonal_rainfall_sensitivity" & name15=="value") %>% 
  group_by(qid,name13) %>% 
  mutate(ind=row_number()) %>% 
  ungroup() 

a_value <- a2 %>% full_join(a3, by=c("qid","ind")) %>% 
  mutate(name13=str_c(name13,"_",season)) %>% 
  select(qid,name13,value) %>% 
  filter(value!="") %>% 
  group_by(qid) %>% 
  mutate(ind=row_number()) %>% 
  filter(ind==1) %>% 
  select(-ind) %>% 
  mutate(value=as_factor(value)) %>% 
  spread(name13,value)

data_clean <- data_clean %>% full_join(a_value,by="qid")

a_label <- a0 %>%
  filter(name15=="key"&!str_detect(name13,"_season")&str_detect(label,"Seasonal")) %>%
  select(name5,name13,label) %>%
  unique %>%
  rename(question_number=name5,variable=name13) %>%
  filter(!str_detect(label,"climatic"))

dictionary <- dictionary %>% bind_rows(a_label)

# 6.4 Cost-benefit analysis
a0 <- data_raw %>% 
  filter(str_detect(name5,"6_4"))

a1 <- a0 %>% 
  filter(name7=="tech_qg_181")

a_value <- f_single_data(a1,"6_4","tech_costbenefit_est_short")
a_label <- f_single_dict(a1,"6_4","tech_costbenefit_est_short") %>% 
  mutate(label=str_c(label," in comparison with establishment costs"))
data_clean <- data_clean %>% full_join(a_value,by="qid")
dictionary <- dictionary %>% bind_rows(a_label)

a_value <- f_single_data(a1,"6_4","tech_costbenefit_est_long")
a_label <- f_single_dict(a1,"6_4","tech_costbenefit_est_long") %>% 
  mutate(label=str_c(label," in comparison with establishment costs"))

data_clean <- data_clean %>% full_join(a_value,by="qid")
dictionary <- dictionary %>% bind_rows(a_label)

a1 <- a0 %>% 
  filter(name7=="tech_qg_182")

a_value <- f_single_data(a1,"6_4","tech_costbenefit_est_short") %>% 
  rename(tech_costbenefit_mnt_short=tech_costbenefit_est_short)
a_label <- f_single_dict(a1,"6_4","tech_costbenefit_est_short") %>% 
  mutate(label=str_c(label," in comparison with maintenance costs")) %>% 
  mutate(variable="tech_costbenefit_mnt_short")

data_clean <- data_clean %>% full_join(a_value,by="qid")
dictionary <- dictionary %>% bind_rows(a_label)

a_value <- f_single_data(a1,"6_4","tech_costbenefit_est_long") %>% 
  rename(tech_costbenefit_mnt_long=tech_costbenefit_est_long)
a_label <- f_single_dict(a1,"6_4","tech_costbenefit_est_long") %>% 
  mutate(label=str_c(label," in comparison with maintenance costs")) %>% 
  mutate(variable="tech_costbenefit_mnt_long")


data_clean <- data_clean %>% full_join(a_value,by="qid")
dictionary <- dictionary %>% bind_rows(a_label)

# 6.5 Adoption of the Technology
a0 <- data_raw %>% 
  filter(str_detect(name5,"6_5"))

a_value <- f_single_data(a0,"6_5","tech_adoption_percentage")
a_label <- f_single_dict(a0,"6_5","tech_adoption_percentage")
data_clean <- data_clean %>% full_join(a_value,by="qid")
dictionary <- dictionary %>% bind_rows(a_label)

a_value <- f_single_data(a0,"6_5","tech_adoption_quantify")
a_label <- f_single_dict(a0,"6_5","tech_adoption_quantify")
data_clean <- data_clean %>% full_join(a_value,by="qid")
dictionary <- dictionary %>% bind_rows(a_label)

a_value <- f_single_data(a0,"6_5","tech_adoption_spontaneously")
a_label <- f_single_dict(a0,"6_5","tech_adoption_spontaneously")  %>%  slice(1)
data_clean <- data_clean %>% full_join(a_value,by="qid")
dictionary <- dictionary %>% bind_rows(a_label)

# 6.6 Adaptation
a0 <- data_raw %>% 
  filter(str_detect(name5,"6_6")) # %>% select(name7,name9) %>% unique()



a_value <- f_single_data(a0,"6_6","tech_adaptation_yes")
a_label <- f_single_dict(a0,"6_6","tech_adaptation_yes")
data_clean <- data_clean %>% full_join(a_value,by="qid")
dictionary <- dictionary %>% bind_rows(a_label)

a_value <- f_single_data(a0,"6_6","tech_adaptation_modification")
a_label <- f_single_dict(a0,"6_6","tech_adaptation_modification")
data_clean <- data_clean %>% full_join(a_value,by="qid")
dictionary <- dictionary %>% bind_rows(a_label)


# AGGREGATE IMPACT
a_value <- data_clean %>% 
  select(qid, contains("onsite_impacts_")) %>% 
  mutate(across(contains("onsite_impacts_"),as.double)) %>% 
  mutate(average_onsite_impact=(sum = rowMeans(.[2:68],na.rm=TRUE))) %>% 
  select(qid,average_onsite_impact)


a_label <- tribble(
  ~question_number, ~variable, ~label,
  "tech__3__2", "average_onsite_impact", "Average of all onsite impact scores recorded",
) %>% 
  mutate(question_number="6_1",notes="") 

data_clean <- data_clean %>% full_join(a_value,by="qid")
dictionary <- dictionary %>% bind_rows(a_label)

# dictionary <- dictionary %>% 
#   group_by(variable) %>% 
#   mutate(ind=row_number()) %>% 
#   filter(ind==1)

# SAVE DATA
filename = here::here("03_processed_data","05_Data_impact.rds")
saveRDS(data_clean,filename)
filename = here::here("03_processed_data","05_dictionary_impact.rds")
saveRDS(dictionary,filename)