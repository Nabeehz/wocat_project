# FUNCTIONS USED IN EXTRACTING AND CLEANING DATA
## Extract single response variables
f_single_data <- function(datamat,secno,vname){
  a_data <- datamat %>% 
    filter(name5==secno & name9==vname) %>% 
    filter(str_detect(name11,"value")) %>% 
    select(qid,name9,value) %>% 
    spread(name9,value)
}
f_single_dict <- function(datamat,secno,vname){
  a_dict <- datamat %>% 
    filter(name5==secno & name9==vname) %>% 
    filter(name10=="label") %>% 
    select(name5,name9,value) %>% unique() %>% 
    mutate(notes="") %>% 
    rename(question_number=name5, variable=name9, label=value)
}

## Aggregate total input costs and cost shares (used in "04_Cost_clean_SLM_tech.R")
f_cost <- function(a1,inp,cost_type){
  a1 %>% 
    filter(name9==cost_type&name11=="value") %>% 
    group_by(qid) %>% 
    mutate(value=ifelse(str_detect(name9,"percentage"),
                        mean(as.numeric(value)),
                        sum(as.numeric(value))
                        ),
           name9=str_c(name9,"_",inp),
           name9=ifelse(str_detect(name9,"pi"),str_replace(name9,"_pi",""),name9),
           ind=row_number()) %>% 
    filter(ind==1) %>%
    select(qid,name9,value) %>% 
    spread(name9,value) 
}

f_cost_lable <- function(a1,inp,cost_type){
  a1 %>% 
    filter(name9==cost_type&name11=="key") %>% 
    mutate(name9=str_c(name9,"_",inp),
           name9=ifelse(str_detect(name9,"pi"),str_replace(name9,"_pi",""),name9),
           value=ifelse(str_detect(value,"per input"),str_replace(value," per input",""),value),
           value=str_c(value,": ",inp)) %>%
    select(name5,name9,value) %>% unique() %>% 
    mutate(notes="") %>% 
    rename(question_number=name5, variable=name9, label=value)
}

f_input_cost_share_data <- function(a1,input,ctype){
  a_data <- a1 %>% 
    filter(name9=="tech_input_est_percentage_costs"|name9=="tech_input_maint_percentage_costs") %>% 
    mutate(name9=str_c("input_cost_",ctype,"_share_",input)) %>% 
    filter(name11=="value") %>% 
    group_by(qid,name9) %>% 
    mutate(value=as.numeric(value)) %>% 
    summarise(value=max(value)) %>% 
    ungroup() %>% 
    select(qid,name9,value) %>% 
    spread(name9,value)
}
f_input_cost_share_dict <- function(a1,input,ctype){
  a_dict <- a1 %>% 
    filter((name9=="tech_input_est_percentage_costs"|name9=="tech_input_maint_percentage_costs")&name11=="key") %>% 
    mutate(name9=str_c("input_cost_",ctype,"_share_",input)) %>% 
    select(-qid) %>% 
    unique %>% 
    mutate(notes="") %>% 
    rename(question_number=name5,variable=name9,label=value) %>% 
    mutate(label=str_c("Share of input cost borne by land users: ",input," (",ctype,")"))
}