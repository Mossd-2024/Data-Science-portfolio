# Plan for machine learning model for predicting participant FU completions
# Limitations:
  # 1 predicting FU2 and beyond from FU1 predictions
  # 2 Predicting on the same sample
  # 3 small sample size for machine learning algorithm, will adjust accordingly, but not ideal for cross-validation
      # SVM might perform well with less issues however


# Plan
  # Make DF with all important variables, download
  # Wrangle to tidy format and clean everything
  # extract features from cleaned data
  # design workflows and tuning for SVM, RF, and Regularized regression
  # compare performance metrics to FU1 testing set 
      # Need to see how correlated the FU completions are (correlations matrix)
  # compare performance metrics to completed FU2
  # maybe run another model where people are repeated for each completed FU, then we'll have a larger sample, see performance
  # decide on fitted model
  # write code (probably another script) to pipe upcoming participants in and give a prediction for copmleting FU
  # Have Todd find an idea for what to do for participants at risk of not completing FU


# Packages
library(tidymodels)
library(multilevelmod)
library(tidyverse)
library(lubridate)
library(stringr)
library(plotly)
library(ggnewscale)
library(qualtRics)


# Loading in Data


# screeners

qualtrics_api_credentials(api_key = "placeholder", # to protect data security 
                          base_url = "uwartsandsciences.az1.qualtrics.com",
                          install = TRUE, overwrite=TRUE)

readRenviron("~/.Renviron")


#Import directory of survey names from Qualtrics
surveys <- all_surveys() 

#Set study start date
study_start <- "2020-08-01" #Set to first day of true data collection

###Import Variables from each screener

screener_1 <- fetch_survey(surveyID = surveys$id[which(surveys$name == "Screener Part 1 - Contact Info - Zoom")], #Select survey by name in directory - REPLACE w/correct name
                           verbose = FALSE,
                           force_request = TRUE, #Overwrite last version imported
                           start_date = study_start, #Only import responses after study start date  
                           time_zone = 'America/Los_Angeles')


screener_2 <- fetch_survey(surveyID = surveys$id[which(surveys$name == "Screener Part 2 - Eligibility")], #Select survey by name in directory - REPLACE w/correct name
                           verbose = FALSE,
                           force_request = TRUE, #Overwrite last version imported
                           start_date = study_start, #Only import responses after study start date  
                           time_zone = 'America/Los_Angeles')



df_screener <- screener_1 %>% 
  left_join(screener_2, by = "ID") # need to capitalize then paste first and last names, then see if we can join together



# getting data from the storage platform redcap


token <- "placeholder" # to protect data security
url <- "https://redcap.iths.org/api/"
formData <- list("token"=token,
                 content='report',
                 format='csv',
                 report_id='211992',
                 csvDelimiter='',
                 rawOrLabel='raw',
                 rawOrLabelHeaders='raw',
                 exportCheckboxLabel='false',
                 returnFormat='csv'
)
response <- httr::POST(url, body = formData, encode = "form")
df1 <- httr::content(response)

df_redcap <- df1%>% 
  mutate(across(starts_with("payw"), ~ if_else(. < 16, ./22, ((.-5)/22))),
         wk1 = ymd(surveystart) +7,
         wk2 = ymd(surveystart) +14,
         wk3 = ymd(surveystart) +21,
         wk4 = ymd(surveystart) +28,
         wk5 = ymd(surveystart) +35,
         wk6 = ymd(surveystart) +42,
         wk7 = ymd(surveystart) +49,
         wk8 = ymd(surveystart) +56,
         fu1_date = ymd(surveystart) +181,
         fu2_date = ymd(surveystart) +365,
         fu3_date = ymd(surveystart) +547,
         fu4_date = ymd(surveystart) +729,
         across(ends_with("finished"), ~ if_else(is.na(.), FALSE, .)),
         payw0amt = 1) %>% 
  rename("wk0" = surveystart) %>% 
  rename_at(vars(starts_with("wk")), ~ paste(.,"date", sep = "_")) %>% 
  rename_at(vars(starts_with("payw")), ~ paste("wk", str_sub(., 5,-4),"_completion", sep = "")) %>% 
  rename_at(vars(ends_with("finished")), ~ paste("fu", str_sub(.,3, -9), "_completion", sep = "")) %>% 
  mutate(fu1_completion = ifelse(fu1_date > as.Date("2023-07-26"), NA_integer_, fu1_completion),
         fu2_completion = ifelse(fu2_date > as.Date("2023-07-26"), NA_integer_, fu2_completion),
         fu3_completion = ifelse(fu3_date > as.Date("2023-07-26"), NA_integer_, fu3_completion),
         fu4_completion = ifelse(fu4_date > as.Date("2023-07-26"), NA_integer_, fu4_completion)) # if sent after today's date, make NA


# joining, will join via phone number, only one participant different phone number


df_redcap <- df_redcap %>% mutate(phone = str_replace_all(phone, "[\\(\\)\\s-]+", ""), 
                                  phone = if_else(pid == "010322120803", "3606609093", phone),
                                  phone = if_else(pid == "113020115926", "2066416826", phone)) # incorrectly saved numbers
df_screener <- df_screener %>% mutate(phone = str_replace_all(phone, "[\\(\\)\\s-]+", ""))

# filtering to include only pids and Removing duplicates for multiple screeners in screening database
df_screener <- df_screener[which(df_screener$phone %in% unique(df_redcap$phone)),] %>% 
  distinct(phone, .keep_all = T)



df_join <- df_redcap %>% left_join(df_screener, by = "phone") 






# Wrangling, cleaning


# what are all the variables of interest?
  # completion rate for each week (dif model for just one total completion rate)
  # survey start month, year
  # difference of days from screener to EMA start  
  # race 
  # gender
  # vegetables, fruits, activity, strength, soda (reverse-coded)
  # cell.type
  # source of advertisement
  # country of origin
  # month of birth
  # county
  # occupation
  # education
  # cigarettes/ecigs 
  # alcohol
  # marijuana
  # tried to quit
  # binge drinking
  # sleeptime, sleepweekday, sleepweekend
  # stress



# let's remove scammers

scammer_pid_list <- as.character(c(011922102974, 011922123156, 012022120935, 012221143796, 020321151933, 
  021721164753, 031622113064, 062121150029, 072021153777, 072821152857, 
  101121140697, 110421115162, 111521160320,
  111621164314, 111921133160, 111921154303, 112221160383, 112321142084, 
  112321170522, 112421115863, 112421141531, 120621140230))


df_join <- df_join[which(!df_join$pid %in% scammer_pid_list),]
df_join <- df_join[which(df_join$pid != "[pid]"),]
df_join <- df_join[which(df_join$firstname != "Test"),]
df_join <- df_join[which(df_join$lastname != "Test"),]
df_join <- df_join[which(df_join$lastname != "Patel"),]
df_join <- df_join[which(df_join$pid != "100820142764"),]
df_join <- df_join[which(df_join$Progress.y > 33),] #removing people who didn't complete screener



# let's see how much missing data for each variable

  # weekly completion data
sum(is.na(df_join$wk1_completion))
sum(is.na(df_join$wk2_completion))
sum(is.na(df_join$wk3_completion))
sum(is.na(df_join$wk4_completion))
sum(is.na(df_join$wk5_completion))
sum(is.na(df_join$wk6_completion))
sum(is.na(df_join$wk7_completion))
sum(is.na(df_join$wk8_completion)) # 14 participants do not have completion data

df_join[which(is.na(df_join$wk8_completion)),] # many are withdrawn participants some just don't have any

  # Survey start
sum(is.na(df_join$wk0_date))#0
sum(is.na(df_join$StartDate.x))#0



  # race
sum(is.na(df_join$race_1) &
      is.na(df_join$race_2) &
      is.na(df_join$race_3) &
      is.na(df_join$race_4) &
      is.na(df_join$race_5) &
      is.na(df_join$race_6) &
      is.na(df_join$race_7) &
      is.na(df_join$race_8) &
      is.na(df_join$race_9) &
      is.na(df_join$race_10) &
      is.na(df_join$race_12) &
      is.na(df_join$race_13)) #0



  # gender
sum(is.na(df_join$gender_1) &
      is.na(df_join$gender_8) &
      is.na(df_join$gender_9) &
      is.na(df_join$gender_10) &
      is.na(df_join$gender_11) &
      is.na(df_join$gender_12) &
      is.na(df_join$gender_13) &
      is.na(df_join$gender_14) &
      is.na(df_join$gender_15) &
      is.na(df_join$gender_16) &
      is.na(df_join$gender_16_TEXT)) # also 0









# recruitment source

sum(is.na(df_join$Q20_1) &
      is.na(df_join$Q20_2) &
      is.na(df_join$Q20_3) &
      is.na(df_join$Q20_4) &
      is.na(df_join$Q20_5) &
      is.na(df_join$Q20_6) &
      is.na(df_join$Q20_7) &
      is.na(df_join$Q20_8) &
      is.na(df_join$Q20_9) &
      is.na(df_join$Q20_10) &
      is.na(df_join$Q20_11) &
      is.na(df_join$Q20_13) &
      is.na(df_join$Q20_14) &
      is.na(df_join$Q20_15) &
      is.na(df_join$Q20_8_TEXT)) # also 0





# Occupation

sum(is.na(df_join$occupation_1) &
      is.na(df_join$occupation_2) &
      is.na(df_join$occupation_3) &
      is.na(df_join$occupation_4) &
      is.na(df_join$occupation_5) &
      is.na(df_join$occupation_6) &
      is.na(df_join$occupation_7)) # 2 missing, 

df_join[which(is.na(df_join$occupation_1) &
                is.na(df_join$occupation_2) &
                is.na(df_join$occupation_3) &
                is.na(df_join$occupation_4) &
                is.na(df_join$occupation_5) &
                is.na(df_join$occupation_6) &
                is.na(df_join$occupation_7)),] # likely students




# Feature extraction




# what are all the variables of interest?



df1<- df_join %>% 
  mutate(start_year = str_sub(wk0_date, 1,4),
         start_month = as.numeric(str_sub(wk0_date, 6,7)),
         start_season = case_when(start_month == 1 |
                                    start_month == 2 |
                                    start_month == 12 ~ "Winter",
                                  start_month == 3 |
                                    start_month == 4 |
                                    start_month == 5 ~ "Spring",
                                  start_month == 6 |
                                    start_month == 7 |
                                    start_month == 8 ~ "Summer",
                                  start_month == 9 |
                                    start_month == 10 |
                                    start_month == 11 ~ "Winter"),
         screener_EMA_delay = as.numeric(as.Date(wk0_date) - as.Date(StartDate.x)),
         across(contains("race"), ~ !is.na(.)), 
         race = case_when(race_1 == T & 
                   race_2 == F & 
                   race_5 == F & 
                   race_3 == F &
                   race_4 == F &
                   race_6 == F &
                   race_8 == F &
                   race_8 == F &
                   race_9 == F &
                   race_10 == F &
                   race_12 == F &
                   race_13 == F ~ "White",
                 race_2 == T &
                   race_1 == F &
                   race_5 == F & 
                   race_3 == F &
                   race_4 == F &
                   race_6 == F &
                   race_7 == F &
                   race_8 == F &
                   race_9 == F &
                   race_10 == F &
                   race_12 == F &
                   race_13 == F ~ "Hispanic",
                 race_3 == T &
                   race_1 == F & 
                   race_7 == F &
                   race_8 == F &
                   race_9 == F &
                   race_12 == F &
                   race_13 == F  ~ "Asian",
                 race_4 == T &
                   race_1 == F & 
                   race_7 == F &
                   race_8 == F &
                   race_9 == F &
                   race_12 == F &
                   race_13 == F  ~ "Asian",
                 race_5 == T &
                   race_1 == F & 
                   race_7 == F &
                   race_8 == F &
                   race_9 == F &
                   race_12 == F &
                   race_13 == F  ~ "Asian",
                 race_6 == T &
                   race_1 == F & 
                   race_7 == F &
                   race_8 == F &
                   race_9 == F &
                   race_12 == F &
                   race_13 == F  ~ "Asian",
                 race_7 == T &
                   race_2 == F & 
                   race_5 == F & 
                   race_3 == F &
                   race_4 == F &
                   race_6 == F &
                   race_1 == F &
                   race_8 == F &
                   race_9 == F &
                   race_10 == F &
                   race_12 == F &
                   race_13 == F ~ "other",
                 race_8 == T &
                   race_2 == F & 
                   race_5 == F & 
                   race_3 == F &
                   race_4 == F &
                   race_6 == F &
                   race_7 == F &
                   race_1 == F &
                   race_10 == F &
                   race_12 == F &
                   race_13 == F ~ "Black",
                 race_9 == T &
                   race_2 == F & 
                   race_5 == F & 
                   race_3 == F &
                   race_4 == F &
                   race_6 == F &
                   race_7 == F &
                   race_1 == F &
                   race_10 == F &
                   race_12 == F &
                   race_13 == F ~ "Black",
                 race_10 == T &
                   race_1 == F & 
                   race_7 == F &
                   race_8 == F &
                   race_9 == F &
                   race_12 == F &
                   race_13 == F  ~ "Asian",
                 race_12 == T ~ "other",
                 T ~ "other")) %>% 
  relocate(contains("race"), .before = pid) %>% 
  mutate(newvar = rowSums(.[1:11]),
         race = if_else(newvar >= 2, "multiracial", race)) %>% 
  relocate(contains("gender"), .before = race_1) %>%
  mutate(across(contains("gender"), ~ !is.na(.))) %>% 
  mutate(newvar = rowSums(.[3:11]),
         gender = case_when(newvar >= 1 ~ "other",
                            gender_1 == 1 ~ "Female",
                            gender_8 == 1 ~ "Male",
                            T ~ "other"),
         education = as.numeric(education),
         current_student = if_else(!is.na(occupation_4) |
                                     !is.na(occupation_5) |
                                     !is.na(occupation_6) |
                                     !is.na(occupation_7), 1, 0),
         currently_working = if_else(!is.na(occupation_2) |
                                       !is.na(occupation_3), 1, 0),
         soda = factor(soda, levels = rev(levels(df_join$soda))),
         across(c(vegetables, fruit, soda), as.numeric)) %>% 
         mutate(healthy_eating = rowMeans(select(., vegetables, fruit, soda)),
                across(c(activity, strength), ~ as.numeric(str_sub(.,1,1)))) %>% 
        mutate(exercise = rowMeans(select(., activity, strength)),
               across(c(cigarettes, 
                        ecigs, 
                        alcohol, 
                        marijuana, 
                        binge, 
                        stress, 
                        sleeptime, 
                        sleepweekday, 
                        sleepweekend), as.numeric)) %>% 
  mutate(sleep_diff = sleepweekend-sleepweekday,
         source = case_when(!is.na(Q20_1) |
                              !is.na(Q20_2) |
                              !is.na(Q20_3) |
                              !is.na(Q20_11) ~ "social_media",
                              !is.na(Q20_9) |
                              !is.na(Q20_10) ~ "school",
                              !is.na(Q20_3) ~ "flyer",
                              !is.na(Q20_6) ~ "friend",
                              !is.na(Q20_7) ~ "craigslist",
                              !is.na(Q20_4) ~ "research_match",
                              !is.na(Q20_5) ~ "newspaper",
                              !is.na(Q20_15) ~ "student_email",
                              !is.na(Q20_8) ~ "other",
                            T ~ NA_character_))
  

df1<- df1%>% select(pid,
                    race,
                    gender,
                    contains("completion"),
                    contains("date"),
                    age,
                    education,
                    cigarettes,
                    ecigs,
                    alcohol,
                    binge,
                    marijuana,
                    sleeptime,
                    sleepweekday,
                    sleepweekend,
                    stress,
                    start_year,
                    start_month,
                    start_season,
                    screener_EMA_delay,
                    current_student,
                    currently_working,
                    healthy_eating,
                    exercise,
                    sleep_diff) %>% select(-wk0_completion)





# Four dataframes, nested or unested, averaged weekly EMA, or broken up by week

df_average_fu <- df1%>% mutate(follow_up_ave = rowMeans(select(., contains("fu")), na.rm = T)) %>% select(-contains("fu"))



df_long <- df1%>% select(-contains("date")) %>% 
  pivot_longer(cols = contains("fu"), names_to = "fu_num", values_to = "completed_fu") %>%
  mutate(fu_num = factor(fu_num, levels = c("fu1_completion", "fu2_completion", "fu3_completion", "fu4_completion"))) %>% 
  filter(!is.na(completed_fu)) %>% 
         mutate(completed_fu = factor(as.numeric(completed_fu)))# almost 2,000 observations

  
  
df_average_ema <- df1%>% mutate(ema_ave = rowMeans(select(., contains("wk")), na.rm = T),
                                ema_ave = if_else(is.nan(ema_ave), 0, ema_ave),
                                follow_up_ave = rowMeans(select(., contains("fu")), na.rm = T)) %>% 
                          select(-contains(c("wk", "fu")))



df_long_average_ema <- df1%>% mutate(ema_ave = rowMeans(select(., contains("wk")), na.rm = T),
                                     ema_ave = if_else(is.nan(ema_ave), 0, ema_ave)) %>% 
  select(-contains(c("wk"))) %>% 
  pivot_longer(cols = contains("fu"), names_to = "fu_num", values_to = "completed_fu")%>%
  mutate(fu_num = factor(fu_num, levels = c("fu1_completion", "fu2_completion", "fu3_completion", "fu4_completion"))) %>% 
  filter(!is.na(completed_fu)) %>% 
  mutate(completed_fu = factor(as.numeric(completed_fu)))





#missing data?

for (i in names(df1)) {
  print(paste(sum(is.na(df1[,i])), i, sep = " ---- "))    
} # need to impute for healthy_eating and exercise, probably for week 1 -8 completion




# FU completion correlations

df_join[, c("fu1_completion", "fu2_completion", "fu3_completion", "fu4_completion")] %>% na.omit() %>% 
  cor() 

# fairly correlated but not greatly
# can do one model that's OLS regression and average out the completion rates (potentially enough sample size)
# can do one algorithm for each (not enough sample size)
# can have multiple rows for each FU (may break assumptions, but could have predictive power)





#RUN THE "fu_model_and_tuning.R" SCRIPT FOR TESTING MODELS
  # WILL GATHER OBJECTS FROM THIS SCRIPT

  
