# Implimentation code for Machine learning algorithm on pt FU forecasts

# look at previous file for how I made the data frame objects
source("learning and processing.R")


fu_final_fit <- readRDS("fu_machine_learning_algorithm.rds")


df_upcoming_fus <- df1 %>% pivot_longer(cols = contains("fu"), 
                     names_to = c("fu_num", ".value"),
                     values_to = "fu_date",
                     names_sep = "_") %>%
  mutate(fu_num = paste(fu_num, "completion", sep = "_"),
         fu_num = factor(fu_num, levels = c("fu1_completion", "fu2_completion", "fu3_completion", "fu4_completion"))) %>% 
  mutate(completion = factor(as.numeric(completion))) %>% 
  rename(completed_fu = completion) %>% 
  filter(date >= today() & date <= today() + 30) 
  
# Now we just need to fit with best model, gather predictions, and wrangle the data into a csv file to upload monthly



upcoming_fu_predictions <- df_upcoming_fus %>% 
  cbind(predict(fu_final_fit, test, type = "prob")) %>% 
  mutate(Will_Complete = if_else(.pred_1 > .9, "Likely", "Not likely"),
         fu_num = str_sub(fu_num, 1,3)) %>% 
  rename(date_sent = date) %>%
  select(pid,
         fu_num,
         date_sent,
         Will_Complete) %>% 
  arrange(fu_num)

write.csv(upcoming_fu_predictions, "upcoming_fu_predictions.csv")
