library(tidyverse)
library(lubridate)
library(DescTools)

df <- read_csv("RadLabBillingLog.csv")

sum(df$Debit) #151,230 should be total signals

df %>% group_by(TransactionType, Debit) %>% 
  count()
# dif types of transactions, 149,813 of 151,230 (99.1%) were from Daily Signals
    #1,422 of daily signals were sent twice (total 2844 signals, mostly from four pids = $284.40 spent)
# 598 signals were from sent verification message
# 119 signal were sent as PARTICIPANTMESSAGE, unsure what this is, could be when we manually send baseline
# 93 signals were sent "SURVEYMESSAGE" unsure exactly what this means

df %>% group_by(Debit) %>% 
  count()


# 1477 had 2 signals in  single transaction?
# double checked with Paresh, this happens when the text is greater than 160 characters, 
# some could be from new tuesday text

df_debit2 <- df %>% filter(Debit == 2)

unique(df_debit2$TransactionDesc)
unique(df_debit2$TransactionType)
unique(df_debit2$TransactionDate)

unique(df$TransactionType)

debit_2 <- intersect(unique(df_debit2$TransactionType), unique(df$TransactionType))
only_debit_1 <- setdiff(unique(df$TransactionType), unique(df_debit2$TransactionType))

df %>% group_by(Debit, TransactionType) %>% count()
#registering a participant and automatic verification verification was only charged with one signal, 
  # but the others have at least 2 signals purchased in one row, need to check if it's from multiple purchases in short timeframe
  # Alright checked with Paresh, with messages greater than 160 characters are sent two signals/2  were debited from our account.


df %>% group_by(ID) %>% count() #ID is study ID, not PID


# Making PID column
df_PID <- df %>%  
  mutate(PID = str_sub(TransactionDesc, -12,-1),
         TransactionDesc = str_sub(TransactionDesc, 1,-14),
         StartDate = mdy(str_sub(PID, 1,6)))

unique(df_PID$TransactionDesc)


df_PID %>% group_by(PID) %>% count() %>% #signals were sent to PIDs for multiple reasons
  group_by(n) %>% count() # most folks got three different types of notifications, up to 6


df_PID %>% group_by(TransactionDesc) %>% count() 
# how many people were sent each type of message
# It looks like about all pts were sent Daily signals, registration, and verification text. 
# about 34 were manually sent verifications, and either 47 and/or 54 participants were sent their baseline via SS.




#when, and for what type of signals did people get two signals instead of one

df %>% 
     mutate(PID = str_sub(TransactionDesc, -12,-1),
                      TransactionDesc = str_sub(TransactionDesc, 1,-14),
                       StartDate = mdy(str_sub(PID, 1,6))) %>% 
     filter(Debit == 2) %>%  group_by(StartDate, TransactionDesc, PID) %>% count() %>% view()


PIDs_sent_double <- c("100520153706", "011521150025", "050421133020", "063021132119")
# PIDs 100520153706, 011521150025, 050421133020, and 063021132119 were sent two daily signals at once 
                                            # between 250 and 450 times, and only PIDS who had this.  

df %>% 
  mutate(PID = str_sub(TransactionDesc, -12,-1),
         TransactionDesc = str_sub(TransactionDesc, 1,-14),
         StartDate = mdy(str_sub(PID, 1,6))) %>% 
  filter(Debit == 2) %>%  group_by(StartDate, TransactionDesc, PID) %>% count() %>% 
  filter(n > 2 | TransactionDesc == "DAILY SIGNAL SMS WAS SENT FOR PID:")

# weird because these participants were onbaorded months apart, but less 2 signal transactions the later they were onboarded
    # can take a look at when they started getting 2 signal transactions.


#first lets see how much of the 2 signal transcations were due to these PIDs
sum(test$n) #1422 (since this is two signals, this means this accounts for 1422 signals we weren't expecting to pay for ($142.20))
nrow(df_debit2)
#1422 of the #1477 (96.3%) of the double transactions were from these four PIDs Daily surveys



df %>% 
  mutate(PID = str_sub(TransactionDesc, -12,-1),
         TransactionDesc = str_sub(TransactionDesc, 1,-14),
         StartDate = mdy(str_sub(PID, 1,6))) %>% 
  filter(PID == "100520153706" |
         PID == "011521150025" |
         PID == "050421133020" |
         PID == "063021132119") %>%
  filter(Debit == 1)
# looks like these four have always gotten 2 signals. Unsure why the more recent PIDs have less signals,
      # they've all completed their EMA period.
  

# how many signals are spent on a single PID
PID_total_signals <- df %>% 
  mutate(PID = str_sub(TransactionDesc, -12,-1),
         TransactionDesc = str_sub(TransactionDesc, 1,-14),
         StartDate = mdy(str_sub(PID, 1,6))) %>% 
  group_by(PID) %>% 
  summarise(PID_total_signals = sum(Debit)) %>% 
  filter(PID_total_signals < 600)


min(PID_total_signals$PID_total_signals) # 1
max(PID_total_signals$PID_total_signals) # 898; need to take a look at those sent a lot of signals
hist(PID_total_signals$PID_total_signals) 
median(PID_total_signals$PID_total_signals) # 267.5 median signals
mean(PID_total_signals$PID_total_signals) # 239.9
sd(PID_total_signals$PID_total_signals) # 132.6
# after 3/25/21, estimate the number of signals we should have spent via daily signals (look for how many reminder texts),
  # the estimate for each PID should change after the tuesday signals on 8/16/22
  # maybe get number of roughly how much we spend on a single PID, winsorizing the tails. (with SD)



# how any signals were sent before the launch of project sms 8/3/2020

test <- df %>% 
  mutate(PID = str_sub(TransactionDesc, -12,-1),
         TransactionDesc = str_sub(TransactionDesc, 1,-14),
         StartDate = mdy(str_sub(PID, 1,6)))

test %>% filter(TransactionDate < as.Date("2020-08-03")) %>% summarise(total = sum(Debit)) 
#185 signals sent before project SMS stardate

test %>% filter(TransactionDate < as.Date("2020-08-03")) %>% group_by(TransactionDesc) %>% count()
PIDs_before_launch_df <- test %>% filter(TransactionDate < as.Date("2020-08-03")) %>% group_by(PID) %>% count()
PIDs_before_launch <- PIDs_before_launch_df$PID


#how many signals were spent in each month, fix the code below
test <- df %>% 
      mutate(PID = str_sub(TransactionDesc, -12,-1),
                         TransactionDesc = str_sub(TransactionDesc, 1,-14),
                         StartDate = mdy(str_sub(PID, 1,6)))
test %>%
  group_by(PID, StartDate) %>% 
  summarize(dailysignals = sum(Debit)) %>% 
  ggplot(aes(x = StartDate, y = dailysignals)) +
  geom_line(size = 3)
  

# Look at PIDs_before_launch and PIDs_sent_double

PIDs_before_launch #no data on redcap, Kevin said they were likely test PIDs, makes sense because only in march and april 2020

PIDs_sent_double 

# 100520153706 this is actually paresh, and was sent 448 double signals (tot = 896)
# 011521150025 nothing out of the ordinary with this pt, sent 390 double signals (tot = 780)
# 050421133020 nothing out of the ordinary with this pt, sent 327 double signals (tot = 654)
# 063021132119 nothing out of the ordinary with this pt, sent 257 double signals (tot = 514)




#progression of signals sent for PID: example - 010721132161 (before adding tuesday surveys)
df_PID %>% filter(PID == "010721132161",
                  TransactionType == "SURVEYDAILYSIG") %>%
  group_by(TransactionDate) %>% count() %>% 
  mutate(weekday = wday(TransactionDate, label = T)) %>% 
  group_by(weekday) %>% 
  summarize(average = mean(n, na.rm = T),
            min = min(n, na.rm = T),
            max = max(n, na.rm = T))

#progression of signals sent for PID: example - 030220113135 (after adding tuesday surveys (8-16-22))
df_PID %>% filter(PID == "092422105226",
                  TransactionType == "SURVEYDAILYSIG") %>% 
  group_by(TransactionDate) %>% count() %>% 
  mutate(weekday = wday(TransactionDate, label = T)) %>% 
  group_by(weekday) %>% 
  summarize(average = mean(n, na.rm = T),
            min = min(n, na.rm = T),
            max = max(n, na.rm = T))
#looks like folks weren't consistently sent the appropriate amount of signals, we knew this from pts accounts


# alright, now lets look at total folks
# on thurs-sun:5 surveys a day, times 4 (survey sms then 3 reminder sms for each), so expect 20 signals for each survey
# on monday and tuesday: 1 survey a day, times 3, so should expect 3 signals
wday_df_PID <- df_PID %>% filter(StartDate >= as.Date("2020-08-01"),
                  TransactionType == "SURVEYDAILYSIG") %>% 
  group_by(TransactionDate, PID) %>% count() %>% 
  mutate(weekday = wday(TransactionDate, label = T)) %>% 
  group_by(weekday) 

wday_df_PID %>% 
  summarize(average = mean(n, na.rm = T),
            ave_sd = sd(n, na.rm = T),
            min = min(n, na.rm = T),
            max = max(n, na.rm = T))

# thurs-sun send about 9 give or take 3 signals a day


# let's look at the distributions of each day

week_days <- c("Mon", "Tue", "Thu", "Fri", "Sat", "Sun")
weekday_analysis <- list(NULL)

hist_day_fun <- function(x) {
  
test_df <- df_PID %>% filter(StartDate >= as.Date("2020-08-01"),
                             TransactionType == "SURVEYDAILYSIG") %>% 
    group_by(TransactionDate, PID) %>% count() %>% 
    mutate(weekday = wday(TransactionDate, label = T)) %>% 
    filter(weekday == x)

weekday_analysis[[x]] <- hist(test_df$n, main = paste("Histogram of" , x))  

}

weekday_analysis <- lapply(week_days, hist_day_fun)



#lets make a table looking at the different counts/props for each day

wday_count_df <- df_PID %>% filter(StartDate >= as.Date("2020-08-01"),
                                   TransactionType == "SURVEYDAILYSIG") %>% 
  group_by(TransactionDate, PID) %>% count() %>% 
  mutate(weekday = wday(TransactionDate, label = T)) %>%
  group_by(weekday, n) %>% 
  count() %>% 
  pivot_wider(names_from = weekday, values_from = nn)


wday_prop_df <- wday_count_df %>% 
  mutate(across(everything(), ~ 100*(./sum(wday_count_df$., na.rm = T))),
         across(everything(), ~ round(.,2)))


(wday_prop_graph <- wday_prop_df %>% 
  rename(number_of_signals = n) %>% 
  pivot_longer(-contains("number"), names_to = "day", values_to = "Proportion") %>% 
  mutate(day = factor(day, levels = week_days),
         expected_signals = case_when(day == "Mon" |
                                      day == "Tue" ~ 3,
                                      day == "Thu" |
                                      day == "Fri" |
                                      day == "Sat" |
                                      day == "Sun" ~ 15,
         T ~ NA_real_),
         wday_mean = case_when(day == "Mon" ~ mean(wday_df_PID[which(wday_df_PID$weekday == "Mon"),]$n, na.rm = T),
                               day == "Tue" ~ mean(wday_df_PID[which(wday_df_PID$weekday == "Tue"),]$n, na.rm = T),
                               day == "Thu" ~ mean(wday_df_PID[which(wday_df_PID$weekday == "Thu"),]$n, na.rm = T),
                               day == "Fri" ~ mean(wday_df_PID[which(wday_df_PID$weekday == "Fri"),]$n, na.rm = T),
                               day == "Sat" ~ mean(wday_df_PID[which(wday_df_PID$weekday == "Sat"),]$n, na.rm = T),
                               day == "Sun" ~ mean(wday_df_PID[which(wday_df_PID$weekday == "Sun"),]$n, na.rm = T)),
         wday_sd = case_when(day == "Mon" ~ sd(wday_df_PID[which(wday_df_PID$weekday == "Mon"),]$n, na.rm = T),
                             day == "Tue" ~ sd(wday_df_PID[which(wday_df_PID$weekday == "Tue"),]$n, na.rm = T),
                             day == "Thu" ~ sd(wday_df_PID[which(wday_df_PID$weekday == "Thu"),]$n, na.rm = T),
                             day == "Fri" ~ sd(wday_df_PID[which(wday_df_PID$weekday == "Fri"),]$n, na.rm = T),
                             day == "Sat" ~ sd(wday_df_PID[which(wday_df_PID$weekday == "Sat"),]$n, na.rm = T),
                             day == "Sun" ~ sd(wday_df_PID[which(wday_df_PID$weekday == "Sun"),]$n, na.rm = T))) %>% 
  group_by(day) %>% 
  mutate(day_mean = mean(expected_signals, na.rm = T)) %>% 
  ggplot(aes(x = number_of_signals, y = Proportion, group = day))+
  geom_line(aes(color = day), size = 1.5) +
  geom_line(aes(x = expected_signals), color = "red", size = 1.5) +
  geom_line(aes(x = wday_mean), color = "blue", size = 1.5) +
  geom_ribbon(aes(xmin = wday_mean-(2*wday_sd), xmax = wday_mean+(2*wday_sd)), fill = "aquamarine4", alpha = .3) +
  geom_ribbon(aes(xmin = wday_mean-wday_sd, xmax = wday_mean+wday_sd), fill = "darkcyan", alpha = .4) +
  facet_wrap(~ day) +
  labs(caption = "Note: red line denotes expected signals to be sent per PID, blue shade is average +/- 1 sd, lighter shade is average +/- 2 sd."))




# pid aggregates
weekday_df <- wday_df_PID %>% 
  group_by(PID, weekday) %>% 
  summarise(n = mean(n, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(weekday) %>% 
  summarise(std = sd(n, na.rm = T),
            n = mean(n, na.rm = T)) %>% 
  ungroup()


wday_df_PID %>% 
  group_by(PID, weekday) %>% 
  summarise(n = mean(n, na.rm = T)) %>%
  ungroup() %>% 
  ggplot(aes(x = weekday, y = n, group = PID)) + 
  geom_line(size = 1, alpha = .05) +
  geom_line(data = weekday_df, aes(x= weekday, y = n, group = 1), 
            color = "deeppink4", alpha = .6, size = 3) +
  geom_ribbon(data = weekday_df, aes(ymin = n-(2*std), 
                                     ymax = n+(2*std),
                                     group = 1), 
              fill = "darksalmon", alpha = .3) +
  geom_point(data = weekday_df, aes(x= weekday, y = n, group = 1), size = 3.5, color = "darkslateblue") +
  geom_label(data = weekday_df, aes(label = round(n,2), group = 1)) +
  labs(title = "Average Signals Received for Each Weekday",
       caption = "Note: only subset were given tuesday surveys and averages were determined with that.\n Pink spread is one sd away from grouped mean") +
  xlab("Day of Week") +
  ylab("Number of Signals")






  


# expecting 25 signals a day for thu-sun and 5 for mon, 4 for tue (exception), 109*8 = 872 for one pt, 872*502 = 437,744, 
#if only 3 signals per survey, then should expect 20 for thu-sun, and 4 for mon, 3 for tue (exception), 87*8 = 696 for one pt, 696*502 = 349,392

# separate function from when we started doing 3 reminders instead of 2. OCT 2020, double check if we get reminder texts at 20, 40, 55


# if we expect 4 signals per survey we'd have about 10,000 less than what we have spent on dailysignals (149,813 - 134,352)
  # weird because a lot of folks aren't getting all daily signals,
# if we expect 3 signals per survey we'd have about 34,832 less than what we have spent on dailysignals (134,352 - 99,520)



#some signals sent on wednesday

df_PID %>% 
  mutate(weekday = wday(TransactionDate, label = T)) %>%
  filter(weekday == "Wed") # these were not daily signals


# how many daily signals were sent to each PID,

total_sig_PID <- df_PID %>% 
  filter(TransactionType == "SURVEYDAILYSIG",
         Debit == 1,
         StartDate >= as.Date("2020-08-01")) %>% #146,802 were sent single signals
  group_by(PID, StartDate) %>% 
  count() %>% arrange(StartDate)

total_sig_PID %>%  ungroup() %>% summarise(std = sd(n, na.rm = T),
                          n = mean(n, na.rm = T))
# average pid was sent 272 signals throughout EMA period, give or take 105

# Dist
hist(total_sig_PID$n)

# sum
sum(total_sig_PID$n)
#146,969



#maybe final step is to get metric for how often someone was not sent the expected amount of signals


# if we are expecting 2 reminders per survey in addition to survey signal, 
# what percent of people received at least as much signals as were exppected (tuesday is exception of 2 reminders)

wday_prop_df <- ungroup(wday_prop_df)

sum(wday_prop_df[which(wday_prop_df$n >=15),"Sun"], na.rm = T) #   5.09%
sum(wday_prop_df[which(wday_prop_df$n >=15),"Thu"], na.rm = T) #   2.52%
sum(wday_prop_df[which(wday_prop_df$n >=15),"Fri"], na.rm = T) #   3.84%
sum(wday_prop_df[which(wday_prop_df$n >=15),"Sat"], na.rm = T) #   5.2%

  



# Check how many people did not receive monday or tuesday surveys when they were expected to


#mondays/tuesdays receiving no signals

mon_tue_df <- wday_df_PID %>% 
  ungroup() %>% 
  mutate(StartDate = mdy(str_sub(PID, 1,6)),
         surveystart = case_when(wday(StartDate, label = T) == "Fri" ~ StartDate + 6,
                                 wday(StartDate, label = T) == "Sat" ~ StartDate + 5,
                                 wday(StartDate, label = T) == "Sun" ~ StartDate + 4,
                                 wday(StartDate, label = T) == "Mon" ~ StartDate + 3,
                                 wday(StartDate, label = T) == "Tue" ~ StartDate + 2,
                                 wday(StartDate, label = T) == "Wed" ~ StartDate + 1,
                                 T ~ StartDate),
         wk_num = case_when(TransactionDate >= surveystart & TransactionDate < surveystart +7     ~ 1,
                            TransactionDate >= surveystart+7 & TransactionDate <  surveystart +14 ~ 2,
                            TransactionDate >= surveystart+14 & TransactionDate < surveystart +21 ~ 3,
                            TransactionDate >= surveystart+21 & TransactionDate < surveystart +28 ~ 4,
                            TransactionDate >= surveystart+28 & TransactionDate < surveystart +35 ~ 5,
                            TransactionDate >= surveystart+35 & TransactionDate < surveystart +42 ~ 6,
                            TransactionDate >= surveystart+42 & TransactionDate < surveystart +49 ~ 7,
                            TransactionDate >= surveystart+49 & TransactionDate < surveystart +56 ~ 8,
                            TransactionDate >= surveystart+56 & TransactionDate < surveystart +63 ~ 9,
                            TransactionDate >= surveystart+63 & TransactionDate < surveystart +70 ~ 10, #some folks went up to 10 weeks for some reason
                            T ~ NA_real_)) %>% 
    select(surveystart, n, weekday, PID, wk_num) %>% 
    group_by(PID, wk_num) %>%  
    pivot_wider(everything(), names_from = weekday, values_from = n) 


#mon proportions
mon_df_count <- mon_tue_df %>% 
  group_by(Mon) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(Mon = if_else(!is.na(Mon), as.numeric(Mon), 0))

mon_df_prop <- mon_df_count %>% 
  mutate(perc = round(100*(n/sum(mon_df_count$n)),2)) 
#thankfully only 3.96% of monday surveys were not sent (still a lot though - 156 surveys)


#Tue proportions 
tue_df_count <- mon_tue_df %>% 
  filter(surveystart >= as.Date("2022-08-16")) %>% 
  group_by(Tue) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(Tue = if_else(!is.na(Tue), as.numeric(Tue), 0))

tue_df_prop <- tue_df_count %>% 
  mutate(perc = round(100*(n/sum(tue_df_count$n)),2)) # .99% of Tuesday surveys were not sent (3 surveys)




# Make line graph on how much the reminders increase total signals costed.

sig_weekday_count <- c(5+1+1+5+5+5)

Sig_estimate_graph <- 
  data.frame(PID_count = 1:532,
             Actual_sig_sent = total_sig_PID$n) %>% 
  mutate(reminder_0 = PID_count*(8*1*sig_weekday_count),
         reminder_1 = PID_count*(8*2*sig_weekday_count),
         reminder_2 = PID_count*(8*3*sig_weekday_count),
         reminder_3 = PID_count*(8*4*sig_weekday_count))


Sig_estimate_graph$cumulative_rem_sent <- sapply(1:532, function(x) {
                                                 sum(total_sig_PID[1:x,"n"], na.rm = T)})


Sig_estimate_graph <- Sig_estimate_graph %>%
  select(-Actual_sig_sent) %>% 
  pivot_longer(contains("rem"), names_to = "Reminder", values_to = "Estimate") %>%
  mutate(Reminder = factor(Reminder, levels = c("reminder_3", 
                                                "reminder_2",
                                                "reminder_1",
                                                "reminder_0",
                                                "cumulative_rem_sent")))

Sig_estimate_graph %>% 
  ggplot(aes(x = PID_count, y = Estimate, group = Reminder)) +
    geom_line(aes(color = Reminder), size = 2.5) +
    scale_color_manual(values = c("goldenrod4","goldenrod4", "gold3", "gold1","black")) +
  geom_point(
    data = Sig_estimate_graph[which(Sig_estimate_graph$PID_count == 532),],
    size = 6) +
  geom_label(data = Sig_estimate_graph[which(Sig_estimate_graph$PID_count == 532),],
             aes(label = paste(str_sub(as.character(Estimate), 1,-4), ",", 
                               str_sub(as.character(Estimate), -3,-1), sep = ""))) +
  labs(title = "Cumulative Expected vs. Real Total Signals",
       subtitle = "Grouped by Expected Signals Sent depending on Number of Reminders Pre-Specified",
       caption = "Black line denotes actual signals received by Project SMS Participants") +
  xlab("Onboarded Participants") +
  ylab("Signals Sent")


# amount of signals spent each month of study.

month_year_df_PID <- df_PID %>% 
  filter(TransactionType == "SURVEYDAILYSIG",
         Debit == 1,
         StartDate >= as.Date("2020-08-01")) %>% 
  mutate(month.num = as.numeric(str_sub(as.Date(TransactionDate),6,7)),
         month = factor(month.abb[month.num], levels = month.abb),
         year = as.numeric(str_sub(as.Date(TransactionDate),1,4))) %>% 
  group_by(month, year) %>% 
  count() %>% arrange(year, month) %>% 
  mutate(date = factor(paste(month,year, sep = " "))) 



month_year_graph <- month_year_df_PID %>% 
  ggplot(aes(x = reorder(date, year), y = n, group = 1)) +
  geom_polygon(size = 1.5, alpha = .6, color = "darkblue", fill = "lightblue4") +
  geom_text(aes(group = 1, label = paste("(",as.character(n),")", 
                                         sep = "")), 
            size = 3, angle = 90, hjust = -1, fontface = "bold") +
  geom_hline(yintercept = mean(month_year_df_PID$n), color = "darkred", alpha = .5, size = 2)+
  geom_hline(yintercept = median(month_year_df_PID$n), color = "orange4", alpha = .5, size = 2)+
  theme(axis.text.x = element_text(angle = 90)) +
  ylim(0,14000) +
  labs(title = "Number of Signals Spent Each Month for Daily Signals",
          caption = "Note: Red line represents average signals spent per month at roughly 4,900 signals, orange line represents median at 3,900") +
  ylab("Number of Signals Received by Participants") +
  xlab("Month of Study")
  

# How many reminders per average

mon_tue_df <- mon_tue_df %>% 
  mutate(across(c("Thu", "Fri", "Sat", "Sun"), ~ ./5),
         across(c("Thu", "Fri", "Sat", "Sun", "Mon", "Tue"), 
                ~ if_else(is.na(.), 0, as.numeric(.))))
mon_tue_df %>% 
  ungroup() %>% 
  summarise(across(c("Thu", "Fri", "Sat", "Sun", "Mon", "Tue"), ~ 
                     paste(round(mean(.),2), "(",round(sd(.), 2),")", sep = "")))

       


hist(mon_tue_df$Thu)
hist(mon_tue_df$Fri)
hist(mon_tue_df$Sat)
hist(mon_tue_df$Sun)
hist(mon_tue_df$Mon)     


# how many folks got 1 reminder or less on average

wday_prop_df %>% 
  pivot_longer(-n, values_to = "prop", names_to = "weekday") %>% 
  filter(n < 11) %>% group_by(weekday) %>% 
  summarise(one_or_less = sum(prop, na.rm = T))

sum(mon_df_prop[1:3, "perc"])
sum(tue_df_prop[1:3, "perc"])

#Tuesday
wday_count_df <- df_PID %>% filter(StartDate >= as.Date("2022-08-16"),
                                   TransactionType == "SURVEYDAILYSIG") %>% 
  group_by(TransactionDate, PID) %>% count() %>% 
  mutate(weekday = wday(TransactionDate, label = T)) %>%
  group_by(weekday, n) %>% 
  count() %>% 
  pivot_wider(names_from = weekday, values_from = nn)


wday_prop_df <- wday_count_df %>% 
  mutate(across(everything(), ~ 100*(./sum(wday_count_df$., na.rm = T))),
         across(everything(), ~ round(.,2)),
         )

