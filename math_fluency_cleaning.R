# math_fluency_cleaning.R
# ------------------------------------
# August 2020
# Maintainer: Ethan Roy <ethanroy395@gmail.com>
#
#
# For functions, please provide the following information
# (these are examples).
# INPUTS
#   Raw SEA data files:
#     t1_SEA_All_correctednamesdemo_rmdups_2020-02-28.csv
#     t2_SEA_All_correctednamesdemo_rmdups_2020-02-28.csv
#     t3_SEA_All_correctednamesdemo_rmdups_2020-02-28.csv
#     t4_SEA_All_correctednamesdemo_rmdups_2020-02-28.csv
#
# OUTPUTS
#   cleaned data file
#     sea_math_fluency_filtered_MM/DD/YYYY.csv
#

# Cleaning guidelines can be found here: https://docs.google.com/spreadsheets/u/1/d/1r_MRbPByufP67MrklKdCzEmRmoTXvZUV7LXEgiYXo4U/edit?ts=5f15c057#gid=739343058
# But generally, we only included reaction times for:
#     -correct items,
#     -singe digit answers
#     -rt's greater than 200 ms
#     -within 3 MAD (ref. Leys et al. 2013)
#
#     Since we are trying to capture speed/accuracy metrics, I'm leaving
#     incorrect RT's in this cleaned dataset. To only work with correct
#     trials, please filter on 'answered_correctly' 
#     (i.e. cleaned_data %>% filter(answered_correctly == 1) in dplyr
#

rm(list=ls())


# Load Packages
library(tidyverse)
library(dplyr)
library(purrr)

summarise <- dplyr::summarise
select <- dplyr::select
map <- purrr::map

# Set working directory 
# We should probably find a way to host the data on Box or something
# and load the data from there for both simplicity's sake and also to 
# ensure the most up-to-date data are loaded

setwd("/Users/Ethan/Documents/Stanford/EdNeuro/UCSF/most updated data")

# Load Raw Data
  t0_sea <- read_csv("t1_SEA_All_correctednamesdemo_rmdups_2020-02-28.csv",
                   col_types = cols(rt = col_character(),
                                    grade = col_character(),
                                    time = col_character(),
                                    trial_onset = col_character()))
t1_sea <- read_csv("t2_SEA_All_correctednamesdemo_rmdups_2020-02-28.csv",
                   col_types = cols(rt = col_character(),
                                    grade = col_character(),
                                    time = col_character(),
                                    trial_onset = col_character(),
                                    ipad_number = col_character()))
t2_sea <- read_csv("t3_SEA_All_correctednamesdemo_rmdups_2020-02-28.csv",
                   col_types = cols(rt = col_character(),
                                    grade = col_character(),
                                    time = col_character(),
                                    trial_onset = col_character(),
                                    iPad_ID = col_character()))
t3_sea <- read_csv("t4_SEA_All_correctednamesdemo_rmdups_2020-02-28.csv",
                   col_types = cols(rt = col_character(),
                                    grade = col_character(),
                                    time = col_character(),
                                    trial_onset = col_character(),
                                    ipad_number = col_character(),
                                    iPad_ID = col_character()))



# Add a few variables and select only the variables of interset
t0_sea_r <- t0_sea %>% 
  mutate(timepoint = "t0_Fall2016",timepoint_int = 0) %>%
  select(pid, timepoint, module, response, correct_response, rt, 
         question_text, question_id, question_text,age_years,age_months,playcount, Grade,block_type,false_type,operation_type,switch_by_operation_type,carrying,digit_load,answer_size,trial_onset,current_time_ms,button_response_1,button_response_2,button_response_3,button_response_4,button_response_submit,correct_button,previous_correct_button) %>% 
  mutate(grade = as.character(Grade)) %>% select(-Grade)

t1_sea_r <- t1_sea %>% 
  mutate(timepoint = "t1_Spring2017",timepoint_int = 1) %>%
  select(pid, timepoint, module, response, correct_response, rt, 
         question_text, question_id, question_text,age_years,age_months,playcount, Grade,block_type,false_type,operation_type,switch_by_operation_type,carrying,digit_load,answer_size,trial_onset,current_time_ms,button_response_1,button_response_2,button_response_3,button_response_4,button_response_submit,correct_button,previous_correct_button) %>% 
  mutate(grade = as.character(Grade)) %>% select(-Grade)

t2_sea_r <- t2_sea %>% 
  mutate(timepoint = "t2_Fall2017",timepoint_int = 2) %>%
  select(pid, timepoint, module, response, correct_response, rt, 
         question_text, question_id, question_text,age_years,age_months,playcount, Grade,block_type,false_type,operation_type,switch_by_operation_type,carrying,digit_load,answer_size,trial_onset,current_time_ms,button_response_1,button_response_2,button_response_3,button_response_4,button_response_submit,correct_button,previous_correct_button) %>% 
  mutate(grade = as.character(Grade)) %>% select(-Grade)

t3_sea_r <- t3_sea %>% 
  mutate(timepoint = "t3_Spring2018",
         timepoint_int = 3,
         age_months = age_years * 12) %>%
  select(pid, timepoint, module, response, correct_response, rt, 
         question_text, question_id, question_text,age_years,age_months,playcount, Grade,block_type,false_type,operation_type,switch_by_operation_type,carrying,digit_load,answer_size,trial_onset,current_time_ms,button_response_1,button_response_2,button_response_3,button_response_4,button_response_submit,correct_button,previous_correct_button) %>% 
  mutate(grade = as.character(Grade)) %>% select(-Grade)


# join the data into one big raw structure
sea_raw <- t0_sea_r %>%
  bind_rows(t1_sea_r) %>%
  bind_rows(t2_sea_r) %>%
  bind_rows(t3_sea_r)


### Add Individual Level Predictors to Raw Data

# setwd('/Users/Ethan/Documents/Stanford/EdNeuro/UCSF/data-selected/')
# 
# abcd_or = read.csv('criticalValuesALL_averaged_data_consentcheck_2019-02-01 (1) (1).csv')
# 
# colnames(abcd_or) <- gsub(".", "_", colnames(abcd_or), fixed = TRUE)
# colnames(abcd_or) <- tolower(colnames(abcd_or))
# 
# names(abcd_or)
# 
# abcd <- abcd_or %>%
#   mutate(time = case_when(time_point == "T1_Fall2016" ~ 0,
#                           time_point == "T2_Spring2017" ~ 1,
#                           time_point == "T3_Fall2017" ~ 2,
#                           time_point == "T4_Spring2018" ~ 3),
#          language_fluency = ifelse(language_fluency == "English Only", 
#                                    "1. English Only", language_fluency),
#          grade_x = ifelse(is.na(grade_x), grade_y, grade_x),
#          grade_x = ifelse(is.na(grade_x), grade, grade_x)) %>%
#   select(pid, sex,
#          language_fluency,
#          low_income,
#          parent_ed_lvl,
#          taskswitch_rt_mean_correct_switch,
#          stroop_rcs_overall,
#          flanker_rcs_overall
#   ) 
# 
# sea_raw =  join(sea_raw,abcd, by="pid")
# 
# 
# 
# sea_raw$taskswitch_rt_mean_correct_switch = as.numeric(sea_raw$taskswitch_rt_mean_correct_switch)/1000
# 
# sea_raw$stroop_rcs_overall = as.numeric(sea_raw$stroop_rcs_overall)
# sea_raw$flanker_rcs_ocerall = as.numeric(sea_raw$flanker_rcs_overall)


### Looks like we have averages on various EF measures
### as well as Academic performance (grades and standardized test)
### and things such as language status, parental education, income level
### special education classifications



### Filter Math Fluency Data and add derived metrics ###


# Filter trials that are less than 200ms and recode correct answer based on question_text
# since the correct_response column was messed up in raw data files
sea_math_fluency = sea_raw %>% 
  mutate(rt = as.numeric(rt)) %>% 
  filter(module == "MATH_FLUENCY",rt>200) %>% 
  mutate(operands = purrr::map(str_extract_all(question_text,"\\d"),as.numeric)) %>% 
  mutate(correct_response = case_when(
    operation_type == 'addition' ~ as.numeric(map(operands, max))+as.numeric(map(operands, min)),
    operation_type == 'subtraction' ~ as.numeric(map(operands, max))-as.numeric(map(operands, min)),
    operation_type == 'multiplication' ~as.numeric(map(operands, max))*as.numeric(map(operands, min)))) %>%
  ungroup() %>% 
  mutate(operand_distance = as.numeric(map(operands, max))-as.numeric(map(operands, min))) %>% ungroup() %>% 
  select(-operands)

sea_math_fluency = sea_math_fluency %>% 
  distinct(pid,question_text, timepoint, .keep_all = T)
### some of the time points double recorded answers for some reason
### this makes sure that only unique responses are present



## Add initial age and time since firstgameplay
temp = sea_math_fluency %>% select(pid,age_months,playcount) %>% filter(playcount==1) %>% mutate(initial_age = age_months)

sea_math_fluency$initial_age <- temp$initial_age[match(sea_math_fluency$pid,temp$pid)]

sea_math_fluency= sea_math_fluency%>%
  mutate(time_since_firstgameplay=age_months-initial_age)
### For some reason a couple participants get younger -- should they be dropped?


# select math fluency data and add vars for
# correct/incorrect answers, cohort, fluency type, rule type, and set size
# error size, first button press RT, and time between button presses
sea_math_fluency = sea_math_fluency %>% 
  mutate(answered_correctly = response == correct_response, 
         answered_incorrectly = response != correct_response,
         rt = as.numeric(rt),
         cohort = case_when(grade == "3" ~ 0,
                            grade == "4" ~ 0,
                            grade == "5" ~ 1,
                            grade == "6" ~ 1,
                            grade == "7" ~ 2,
                            grade == "8" ~ 2,
         ),
         trial_onset=as.numeric(trial_onset))%>% 
  mutate(fluency_type = case_when(str_detect(question_text, "\\+ 10") ~ "No-Rule",      
                                  str_detect(question_text, "\\- 1") ~ "Rule",          
                                  str_detect(question_text, "\\+ 1") ~ "Rule",
                                  str_detect(question_text, "\\- 0") ~ "Rule",
                                  str_detect(question_text, "\\+ 0") ~ "Rule",
                                  str_detect(question_text, "1 \\+") ~ "Rule",
                                  str_detect(question_text, "1 \\-") ~ "Rule",
                                  str_detect(question_text, "10 \\+") ~ "No-Rule",
                                  str_detect(question_text, "0 \\+") ~ "Rule",
                                  str_detect(question_text, "1 \\+ 1") ~ "Rule",
                                  str_detect(question_text, "1 \\- 1") ~ "Rule",
                                  str_detect(question_text, "2 \\+ 2") ~ "Rule",
                                  str_detect(question_text, "2 \\- 2") ~ "Rule",
                                  str_detect(question_text, "3 \\+ 3") ~ "Rule",
                                  str_detect(question_text, "3 \\- 3") ~ "Rule",
                                  str_detect(question_text, "4 \\+ 4") ~ "Rule",
                                  str_detect(question_text, "4 \\- 4") ~ "Rule",
                                  str_detect(question_text, "5 \\+ 5") ~ "Rule",
                                  str_detect(question_text, "5 \\- 5") ~ "Rule",
                                  str_detect(question_text, "6 \\+ 6") ~ "Rule",
                                  str_detect(question_text, "6 \\- 6") ~ "Rule",
                                  str_detect(question_text, "7 \\+ 7") ~ "Rule",
                                  str_detect(question_text, "7 \\- 7") ~ "Rule",
                                  str_detect(question_text, "8 \\+ 8") ~ "Rule",
                                  str_detect(question_text, "8 \\- 8") ~ "Rule",
                                  str_detect(question_text, "9 \\+ 9") ~ "Rule",
                                  str_detect(question_text, "9 \\- 9") ~ "Rule",
                                  str_detect(question_text, "\\* 1") ~ "Rule",
                                  str_detect(question_text, "1 \\*") ~ "Rule",
                                  str_detect(question_text, "\\* 0") ~ "Rule",
                                  str_detect(question_text, "0 \\*") ~ "Rule",
                                  TRUE ~ "No-Rule"),
         error_size = abs(as.numeric(correct_response)-as.numeric(response)),
         ## What should the cutoff be? 4 or 5?
         set_size = case_when(as.numeric(correct_response) <= 4 ~"small",
                              as.numeric(correct_response) > 4 ~ "large"),
         firstButtonRT =  as.numeric(button_response_1) - as.numeric(trial_onset),
         timeBetweenButtons=as.numeric(button_response_submit)-as.numeric(button_response_1)) %>% 
         filter(module == "MATH_FLUENCY",firstButtonRT>200) 


# Arrange the data by pid, timepoint, and question ID for coding task switching 
sea_math_fluency = sea_math_fluency%>% 
  arrange(pid, timepoint,question_id)


#pull out max operand
sea_math_fluency = sea_math_fluency %>% 
  mutate(max_operand = map(str_extract_all(question_text,"\\d"),as.numeric)) %>% 
  rowwise() %>% 
  mutate(max_operand = max(unlist(max_operand))) %>% ungroup()

# and max operand size
sea_math_fluency = sea_math_fluency%>%
  mutate(max_operand_size = case_when(as.numeric(max_operand) <= 4 ~"small",
                                      as.numeric(max_operand) > 4 ~ "large"))

## did participant have to task switch between operation?
sea_math_fluency = sea_math_fluency %>%
  group_by(pid, timepoint) %>% 
  mutate(operation_switch = ifelse(operation_type==lag(operation_type),F,T)) %>% 
  ungroup()

## did participant have to task switch between rule/no-rule?
sea_math_fluency = sea_math_fluency %>%
  group_by(pid, timepoint) %>% 
  mutate(fluency_switch = ifelse(fluency_type==lag(fluency_type),F,T))%>% 
  ungroup()

#set size task switching
sea_math_fluency = sea_math_fluency%>%
  group_by(pid, timepoint) %>% 
  mutate(setSize_switch = ifelse(set_size==lag(set_size),F,T)) %>% 
  ungroup()


#drop all double digit/negative answers ane multiplication
# we end up throwing out ~40% of the unique questions with this
# and about 15% of the total questions
sea_math_fluency = sea_math_fluency %>%
  # filter(as.numeric(correct_response)<10 & as.numeric(correct_response)>=0) %>%
  filter(as.numeric(correct_response)>=0) %>%
  filter(operation_type != 'multiplication')%>%
  # filter(button_response_2 == 'None')%>%
  filter(question_text != "7 + 10") %>% ## for some reason 7 + 10 is coded as =7
  filter(question_text != "10 + 8") %>% ## same for 10 + 8
  filter(question_text != "2 + 10") %>% 
  ungroup()


## Column for how many times they've taken the test
sea_math_fluency = sea_math_fluency  %>% group_by(pid) %>% 
  mutate(times_taken = match(timepoint,sort(unique(timepoint)))) 


### Filter out only correct answers and remove outliers ###

# only look at correct data
sea_math_fluency_correct_rt = sea_math_fluency %>% filter(answered_correctly==T)


# look at how many questions each person answered correctly at each timepoint
correct_per_timepoint = sea_math_fluency_correct_rt %>% group_by(pid,timepoint) %>% 
  summarise(count = n())

table(correct_per_timepoint$count)
# big range here 1-98...What should the cutoff be? Going to start at 5 to be consistent w/ UCSF

# dataframe to label timepoints within participants that had an insufficient 
# number of trials (in this case < 5)
sufficient_total = sea_math_fluency%>% group_by(pid,timepoint) %>% 
  summarise(count = n()) %>% 
  mutate(insufficient = ifelse(count<5,1,0))

table(sufficient_total$insufficient)
# looks like 1 timepoints will be dropped due to insufficient correct responses

# Filter out timepoints where individuals did not answer sufficient number of questions
sea_math_fluency_filtered = sea_math_fluency %>%
  inner_join(sufficient_total) %>%
  filter(insufficient == 0) 


# label outliers based who are outside of 3 MADs based on grade x timepoint
# First group on grade and timepoint to find median at each grade x timepoint
# then group at individual and timepoint to find median for each individual x timepoint
# then compare individual and grade timepoint medians and flag those that are outside 3 MADs
# as outliers 
outliers_rt <- sea_math_fluency_filtered %>% group_by(grade,timepoint) %>%
  summarise(pid = pid,
            grade = grade,
            median_rt_grade_time = median(firstButtonRT, na.rm = T),
            mad_rt_grade_time = mad(firstButtonRT, na.rm = T),
            firstButtonRT = firstButtonRT) %>%
  group_by(pid,timepoint,median_rt_grade_time,mad_rt_grade_time) %>%
  summarise(median_ind_rt = median(firstButtonRT)) %>%
  mutate(outlier_rt = ifelse(median_ind_rt < (median_rt_grade_time + (mad_rt_grade_time*3))
                          & median_ind_rt > (median_rt_grade_time - (mad_rt_grade_time*3)), 0, 1))

# outliers_rt <- sea_math_fluency_filtered %>% group_by(grade,timepoint) %>% 
#   summarise(pid = pid, 
#             grade = grade,
#             median_rt_grade_time = median(rt, na.rm = T),
#             mad_rt_grade_time = mad(rt, na.rm = T),
#             rt = rt) %>% 
#   group_by(pid,timepoint,median_rt_grade_time,mad_rt_grade_time) %>% 
#   summarise(median_ind_rt = median(rt)) %>% 
#   mutate(outlier_rt = ifelse(median_ind_rt < (median_rt_grade_time + (mad_rt_grade_time*3))
#                              & median_ind_rt > (median_rt_grade_time - (mad_rt_grade_time*3)), 0, 1))

prop.table(table(outliers_rt$outlier_rt)) * 100
table(outliers_rt$outlier_rt)
outliers_rt %>% filter(outlier_rt==1) %>% group_by(timepoint) %>% summarise(count=n())
# Looks like 29 indiviudals will be excluded across all four timepoints
# t0: 4
# t1: 7
# t2: 6
# t3: 12

outliers_acc <- sea_math_fluency_filtered %>% group_by(pid,grade,timepoint) %>% 
  summarise(tot_corr = sum(answered_correctly,na.rm=T),
            tot=n(),
            acc=tot_corr/tot) %>% 
  group_by(grade,timepoint) %>% 
  dplyr::summarise(pid = pid, 
                   grade = grade,
                   median_acc_grade_time = median(acc, na.rm = T),
                   mad_acc_grade_time = mad(acc, na.rm = T),
                   acc = acc) %>% 
  group_by(pid,timepoint,median_acc_grade_time,mad_acc_grade_time) %>% 
  dplyr::summarise(median_ind_acc = median(acc)) %>% 
  mutate(outlier_acc = ifelse(median_ind_acc < (median_acc_grade_time + (mad_acc_grade_time*3))
                              & median_ind_acc > (median_acc_grade_time - (mad_acc_grade_time*3)), 0, 1))

prop.table(table(outliers_acc$outlier_acc)) * 100
table(outliers_acc$outlier_acc)
outliers_acc %>% filter(outlier_acc==1) %>% group_by(timepoint) %>% summarise(count=n())
# Looks like 233 indiviudals will be excluded across all four timepoints for accuracy
# t0: 62
# t1: 53
# t2: 68
# t3: 49

# Filter out the outliers
sea_math_fluency_filtered <- sea_math_fluency_filtered %>% inner_join(outliers_rt) %>% filter(outlier_rt == 0) 
sea_math_fluency_filtered <- sea_math_fluency_filtered %>% inner_join(outliers_acc) %>% filter(outlier_acc == 0) 


#Save cleaned dataset to current directory
write_csv(sea_math_fluency_filtered, "sea_math_fluency_cleaned.csv")





