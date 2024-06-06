# Coping Motives Paper Analysis
library(readr)
library(lubridate)
library(ltm)
library(psych)
library(tidyverse)
library(janitor)
library(lm.beta)
library(MASS)
library(betareg)


# Script plan
  #1 pull in Data
  #2 Grab just the variables I'll be working with within these data
  #3 Merge both samples into EMA and Baseline, manipulate variables
  #4 Get alphas and omegas for each scale we're using (jonas had done this for each sample, should we do it again for full sample?)
  #5 Look at scales and Items of main variables (look for skew, missing data, potential influence (outliers))
      #A: histograms of Baseline variables, then descriptives 
      #B: histograms of EMA's at the momentary level, then descriptives
      #C: Double check validity of dependent variables (double check not intoxicated when not using)
      #D: aggregate EMA data to Participant level
      #E: get histograms and descriptives for participant level
  #6 Make graphs/tables for important stats (race, gender, weekday usage (alc and mar), age)
  #7 Merge baseline and EMA into signle dataset

#1: import data

  #Run pilot.sms.processing.R, pilot.sms.download.R, and pilot.sms.reliability.R and save to working directory for reproducible code

pilot1.baseline <- read_csv("pilot1.baseline.processed.scales.csv")
pilot2.baseline <- read_csv("pilot2.baseline.processed.scales.csv")
pilot1.ema <- read_csv("pilot1.ema.processed.scales.csv")
pilot2.ema <- read_csv("pilot2.ema.processed.scales.csv")





#2: What variables are we working with?
  # baseline Marijuana Coping motives (MMQ)
  # baseline Alcohol Coping motives (DMQ)
  # Baseline CERQ.
  # EMA er (emotion regulation)
  # baseline Emotion reactivity (ERS)
  # Baseline UPPS. (premeditation, negativeurgency, positiveurgency)
  # Baseline PROMIS. (ANX, DEP)
  # Baseline Alcohol use, 
  # EMA Alcohol use,
  # Baseline Marijuana use 
  # EMA Marijuana use

#baselines
pilot1.baseline <- pilot1.baseline %>% 
  dplyr::select(contains(c("StartDate",
                           "age",
                           "PID",
                    "coping", 
                    "CERQ", #Will need to calculate dsiengagement ratio
                    "ERS", 
                    "UPPS",
                    "PROMIS.ANX", 
                    "PROMIS.DEP", 
                    "DDTQ.typ.use.", #DMU3 and DDQ (DDQ.typ.drinkingdays) need to be manipulated to week level
                    "DDQ.typ.drinks.",
                    "DOB.",
                    "gender.",
                    "race.")),
         -contains(c("mother","father")),
         "MMQ1",
         "MMQ4",
         "MMQ6",
         "MMQ15",
         "MMQ17",
         "DMQ1",
         "DMQ4",
         "DMQ6",
         "DMQ15",
         "DMQ17",
         "DMU3")

pilot2.baseline <- pilot2.baseline %>% 
  dplyr::select(contains(c("StartDate","PID",
                           "age",
                    "coping", 
                    "CERQ", #Will need to calculate dsiengagement ratio
                    "ERS", 
                    "UPPS",
                    "PROMIS.ANX", 
                    "PROMIS.DEP", 
                    "DDTQ.typ.use.", #DMU3 and DDQ (DDQ.typ.drinkingdays) need to be manipulated to week level
                    "DDQ.typ.drinks.",
                    "DOB.",
                    "gender.",
                    "race.")),
         -contains(c("mother","father")),
         "MMQ1",
         "MMQ4",
         "MMQ6",
         "MMQ15",
         "MMQ17",
         "DMQ1",
         "DMQ4",
         "DMQ6",
         "DMQ15",
         "DMQ17",
         "DMU3")


#EMAs
pilot1.ema <- pilot1.ema %>% 
  dplyr::select(contains(c("PID",
                    "version",
                    "er.",
                    "Date",
                    "alc.quant",
                    "alc.intox",
                    "mar.quantjoint", #code to use/non-use
                    "mar.intox")),
         -contains(c("neg",
                     "pos",
                     "all",
                     "ratio"))) #check if anyone was high without putting in joints (problem response)


pilot2.ema <- pilot2.ema %>% 
  dplyr::select(contains(c("PID",
                    "version",
                    "er.",
                    "Date",
                    "alc.quant",
                    "alc.intox",
                    "mar.quantjoint", #code to use/non-use
                    "mar.intox")),
         -contains(c("neg",
                     "pos",
                     "all",
                     "ratio",
                     "dinner")))






#3: Merging two pilot baselines
pilot1.baseline.variables <- names(pilot1.baseline)
pilot2.baseline.variables <- names(pilot2.baseline)
pilot2.baseline[pilot1.baseline.variables[!(pilot1.baseline.variables %in% colnames(pilot2.baseline))]] = NA
pilot.baseline <- pilot1.baseline %>% rbind(pilot2.baseline)



#3b: Merge EMAs

pilot.ema <- pilot1.ema %>% rbind(pilot2.ema)


### 3c: Manipulating variables

#EMA
pilot.ema <- pilot.ema %>% 
  mutate(alc.quant.b = if_else(alc.quant > 0 |
                               alc.intox > 10 & alc.intoxhours > 0,
                               1,0),
         mar.quantjoint.b = if_else(mar.quantjoint > 0 |
                                    mar.intox > 10 & mar.intoxhours > 0,
                                    1,0),
         er.disengagement = rowSums(dplyr::select(., 
                                           er.avoidance, 
                                           er.distraction, 
                                           er.rumination, 
                                           er.suppression), na.rm = T)/
           rowSums(dplyr::select(.,starts_with("er."), -er.none), na.rm = T))  #Momentary disengagement proportion of total er use


#Baseline
pilot.baseline <- pilot.baseline %>% 
  mutate(ERS.full = (ERS.sensitivity + ERS.persistence + ERS.intensity)/3,
         across(starts_with("DDQ"), ~ if_else(. > 0, 1,.)),
         across(starts_with("DDTQ"), ~ if_else(.> 0, 1,.))) %>% #binary use/non-use
  mutate(DDQ.typ.drinks.week = rowSums(dplyr::select(.,starts_with("DDQ")), na.rm = T), #adding up days on typical week
         DDTQ.typ.use.week = rowSums(dplyr::select(.,starts_with("DDTQ")), na.rm = T), #same with weed
         CERQ.disengagement = rowSums(dplyr::select(.,CERQ.selfblame, 
                                             CERQ.otherblame,
                                             CERQ.rumination,
                                             CERQ.catastrophizing), na.rm = T)/ #sum disengagement/sum all ER strategies 
           rowSums(dplyr::select(.,contains("CERQ.")), na.rm = T))
 







### 4: Alphas and Omegas for each scale

  #scales as objects
DMQ.coping <- c("DMQ1", "DMQ4", "DMQ6", "DMQ15", "DMQ17")
MMQ.coping <- c("MMQ1", "MMQ4", "MMQ6", "MMQ15", "MMQ17")
CERQ.selfblame <- c("CERQ1", "CERQ10", "CERQ19", "CERQ28")
CERQ.otherblame <- c("CERQ9", "CERQ18", "CERQ27", "CERQ36")
CERQ.rumination <- c("CERQ3", "CERQ12", "CERQ21", "CERQ30")
CERQ.catastrophizing <- c("CERQ8", "CERQ17", "CERQ26", "CERQ35")
CERQ.perspective <- c("CERQ7", "CERQ16", "CERQ25", "CERQ34")
CERQ.reappraisal <- c("CERQ6", "CERQ15", "CERQ24", "CERQ33")
CERQ.acceptance <- c("CERQ2", "CERQ20", "CERQ29", "CERQ11")
CERQ.planning <- c("CERQ5", "CERQ14", "CERQ23", "CERQ32")
CERQ.refocusing <- c("CERQ4", "CERQ13", "CERQ22", "CERQ31")
ERS.sensitivity <- c("ERS2", "ERS5", "ERS7", "ERS9", "ERS12", "ERS13", "ERS14", "ERS15", "ERS16", "ERS18")
ERS.intensity <- c("ERS3", "ERS4", "ERS6", "ERS17", "ERS19", "ERS20", "ERS21")
ERS.persistence <- c("ERS1", "ERS8", "ERS10", "ERS11")
UPPS.positiveurgency <- c("UPPS5", "UPPS10", "UPPS15", "UPPS20", "UPPS25", "UPPS30", "UPPS35", "UPPS40", "UPPS45", "UPPS49", "UPPS52", "UPPS54", "UPPS57", "UPPS59")
UPPS.negativeurgency <- c("UPPS2", "UPPS7", "UPPS12", "UPPS17", "UPPS22", "UPPS29", "UPPS34", "UPPS39", "UPPS44", "UPPS50", "UPPS53", "UPPS58")
UPPS.premeditation <- c("UPPS1", "UPPS6", "UPPS11", "UPPS16", "UPPS21", "UPPS28", "UPPS33", "UPPS38", "UPPS43", "UPPS48", "UPPS55")
PROMIS.ANX.T <- c("PROMIS.ANX1", "PROMIS.ANX4", "PROMIS.ANX5", "PROMIS.ANX19", "PROMIS.ANX20", "PROMIS.ANX22","PROMIS.ANX27", "PROMIS.ANX28")
PROMIS.DEP.T <- c("PROMIS.DEP1", "PROMIS.DEP2", "PROMIS.DEP3", "PROMIS.DEP5","PROMIS.DEP10", "PROMIS.DEP15", "PROMIS.DEP19", "PROMIS.DEP21")
scales <- list(DMQ.coping, MMQ.coping, CERQ.selfblame, CERQ.otherblame, CERQ.rumination,
            CERQ.catastrophizing, CERQ.perspective, CERQ.reappraisal, CERQ.acceptance, CERQ.planning,
            CERQ.refocusing, ERS.sensitivity, ERS.intensity, ERS.persistence, UPPS.positiveurgency,
            UPPS.negativeurgency, UPPS.premeditation, PROMIS.ANX.T, PROMIS.DEP.T)
scales.c <- c("DMQ.coping", "MMQ.coping", "CERQ.selfblame"," CERQ.otherblame", "CERQ.rumination",
               "CERQ.catastrophizing", "CERQ.perspective", "CERQ.reappraisal", "CERQ.acceptance", "CERQ.planning",
               "CERQ.refocusing", "ERS.sensitivity", "ERS.intensity", "ERS.persistence", "UPPS.positiveurgency",
               "UPPS.negativeurgency", "UPPS.premeditation", "PROMIS.ANX.T", "PROMIS.DEP.T")


#for loop to get alpha and omega for each scale
  # ends in a dataframe with one column being each scale, one column for alpha, and another for omega.

scales_df <- data.frame(Scale = c("Alpha", "Omega"))

for(i in 1:length(scales)) {
  
  new_col <- c(round(as.numeric(ltm::cronbach.alpha(pilot.baseline[,scales[[i]]], na.rm=T)[1]),3),
               round(as.numeric(psych::omega(pilot.baseline[,scales[[i]]], 
                                             fm = "ml",
                                             nfactors = 1,
                                             poly = T)[4]),3)) 
#I assume we want the total Omega? not Hierarchical or asymptotic?                      
  scales_df[ , i+1] <- new_col                     
  colnames(scales_df)[i+1] <- scales.c[i]    
}


#Dataframe of reliabilities for Alphas and Omegas: alphas all above .7, and omegas all above .8

(reliabilities.df <- as.data.frame(t(scales_df)) %>% 
  tibble::rownames_to_column(.) %>% 
  row_to_names(row_number = 1))







### 5: Descriptives of variables

## 5a: Baseline
  #use view() for easier viewing of descriptives
(baseline.describ <- as.data.frame(psych::describe(pilot.baseline)))
  



  #Histograms
hist(pilot.baseline$DMQ.coping) #right skewed
hist(pilot.baseline$MMQ.coping) #Very right skewed
hist(pilot.baseline$CERQ.selfblame)
hist(pilot.baseline$CERQ.otherblame)
hist(pilot.baseline$CERQ.rumination)
hist(pilot.baseline$CERQ.catastrophizing) #Right Skewed
hist(pilot.baseline$CERQ.perspective)
hist(pilot.baseline$CERQ.reappraisal)
hist(pilot.baseline$CERQ.acceptance)
hist(pilot.baseline$CERQ.planning)
hist(pilot.baseline$CERQ.refocusing) #Right Skewed
hist(pilot.baseline$CERQ.disengagement) #Pretty normally distributed
hist(pilot.baseline$ERS.sensitivity)
hist(pilot.baseline$ERS.intensity)
hist(pilot.baseline$ERS.persistence)
hist(pilot.baseline$ERS.full) #averaged accross subscales 
hist(pilot.baseline$UPPS.positiveurgency) #Right Skewed
hist(pilot.baseline$UPPS.premeditation) #Left Skewed
hist(pilot.baseline$UPPS.negativeurgency)
hist(pilot.baseline$PROMIS.ANX.T) #Right Skewed
hist(pilot.baseline$PROMIS.DEP.T) #Right Skewed
hist(pilot.baseline$DDTQ.typ.use.week) # Not many people say they use in DDTQ
hist(pilot.baseline$DMU3) #heavily right skewed but at least folks say that they use.
hist(pilot.baseline$DDQ.typ.drinks.mon) #recode to weekly amount then look at distribution
hist(pilot.baseline$DDQ.typ.drinks.tues)
hist(pilot.baseline$DDQ.typ.drinks.wed)
hist(pilot.baseline$DDQ.typ.drinks.thurs)
hist(pilot.baseline$DDQ.typ.drinks.fri)
hist(pilot.baseline$DDQ.typ.drinks.sat)
hist(pilot.baseline$DDQ.typ.drinks.sun)
hist(pilot.baseline$DDQ.typ.drinks.week) #DEPENDENT VARIABLE, Average 2-3 days (expected) little right skewed.
hist(pilot.baseline$DOB.year) #recode to get age at time of baseline
hist(pilot.baseline$MMQ1) #All except one motive item is right skewed
hist(pilot.baseline$MMQ4)
hist(pilot.baseline$MMQ6)
hist(pilot.baseline$MMQ15)
hist(pilot.baseline$MMQ17)
hist(pilot.baseline$DMQ1)
hist(pilot.baseline$DMQ4)
hist(pilot.baseline$DMQ6)
hist(pilot.baseline$DMQ15) #Only motives item not right skewed
hist(pilot.baseline$DMQ17)




## 5b: Pilot
(ema.describ <- as.data.frame(psych::describe(pilot.ema)))

  #survey version proportion
pilot.ema %>% 
  group_by(version) %>% 
  count() 
  #mostly morning, midday and night, all three about 2.5 times as much as afternoon and evening.

## Histograms

#EMAs unaggregated by PID
hist(pilot.ema$er.acceptance)
hist(pilot.ema$er.avoidance)
hist(pilot.ema$er.distraction)
hist(pilot.ema$er.none) #somewhat equal proportion of use/non-use of stategies
hist(pilot.ema$er.problemsolve)
hist(pilot.ema$er.reframing)
hist(pilot.ema$er.rumination)
hist(pilot.ema$er.suppression)
hist(pilot.ema$alc.quant.b) #heavily right skewed 
hist(pilot.ema$mar.intoxhours) #heavily right skewed
hist(pilot.ema$mar.quantjoint) #Heavily right skewed
hist(pilot.ema$er.disengagement) #Momentary level (not participant level)
#lots of full disengagement or no disengagement use (unsure what to do for no use at all, probably just NA)




###########################################################

### 5c: some quick double Checks before I greatly change EMA table
# occasions where mar.quantjoint = 0 but mar.intox > 0 
pilot.ema %>% 
  filter(mar.quantjoint == 0 & mar.intox != 0, #331 observations; 133 PIDs
         mar.quantjoint == 0 & mar.intoxhours != 0) %>% # 32 observations; #19 PIDs
  dplyr::select(PID, mar.quantjoint, mar.intox, mar.intoxhours)

# same with alcohol

pilot.ema %>% 
  filter(alc.quant == 0 & alc.intox != 0, #141 observations; 75 PIDs
         alc.quant == 0 & alc.intoxhours != 0) %>% #17 observations; 16 PIDs
  dplyr::select(PID, alc.quant, alc.intox, alc.intoxhours) 

#Why is this the case? Should we use intoxication instead? 


## Also, what about folks reporting use/non-use several times in a day 
#(If any report twice a day doesn't work with my code currently and need to look at proportional use on the day level)

pilot.ema %>% group_by(StartDate, PID) %>% 
  summarise(alc.quant.b = sum(!is.na(alc.quant.b), na.rm = T), 
            mar.quantjoint.b = sum(!is.na(mar.quantjoint.b), na.rm = T)) %>% 
  filter(alc.quant.b > 1 | mar.quantjoint.b > 1)


#It looks like no one reporting use/non-use more than once a day? Should be okay for averaging then?

##############################################################








### 6: Distribution Tables (gender, age, race, weekday(avg mar and alc use on each day))
  
# Weekday Distribution of EMA participation (total number of EMAs on specific day/total number of EMAs taken)

weekday_df <- pilot.ema %>% 
  mutate(weekday = wday(Date-1, label = T)) %>% #date minus one because survey asks for previous days' use, and date comes from StartDate variable
  group_by(weekday) %>% 
  count() 
weekday_df %>% 
  mutate(perc = n/sum(weekday_df$n)) %>% 
  ggplot(aes(x = weekday, y = perc)) +
  geom_bar(stat = "identity", position = "dodge")
  


  #Weekday distribution of average drinks 

pilot.ema %>% 
  mutate(weekday = wday(Date-1, label = T)) %>%
  group_by(weekday) %>% 
  summarise(alc.quant.sd = sd(alc.quant, na.rm = T),
            alc.quant = mean(alc.quant, na.rm = T),
            mar.quantjoint.sd = sd(mar.quantjoint, na.rm = T),
            mar.quantjoint = mean(mar.quantjoint, na.rm = T)) %>% 
  ggplot(aes(x = weekday, y = alc.quant)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = alc.quant-alc.quant.sd, ymax = alc.quant+alc.quant.sd))



  #Weekday distribution of average estimate marijuana joints

pilot.ema %>% 
  mutate(weekday = wday(Date-1, label = T)) %>%
  group_by(weekday) %>% 
  summarise(alc.quant.sd = sd(alc.quant, na.rm = T),
            alc.quant = mean(alc.quant, na.rm = T),
            mar.quantjoint.sd = sd(mar.quantjoint, na.rm = T),
            mar.quantjoint = mean(mar.quantjoint, na.rm = T)) %>% 
  ggplot(aes(x = weekday, y = mar.quantjoint)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mar.quantjoint-mar.quantjoint.sd, ymax = mar.quantjoint+mar.quantjoint.sd))


# Age Distribution
pilot.baseline %>% 
  mutate(startdate = as.Date(str_sub(as.character(PID),1,6), format = "%m%d%y"),
         DOB.month.year = as.Date(paste(DOB.year, "01", DOB.month, sep = ""), 
                                  format = "%Y%d%m"),   #not exact birthday because don't know day of month
         age = as.duration(interval(DOB.month.year,startdate)) %/% as.duration(years(1))) %>% 
  group_by(age) %>% 
  count() %>% 
  mutate(n = n/nrow(pilot.baseline))
           

#Gender Distribution (proportion of sample belonging to each identity)
pilot.baseline %>% 
  dplyr::select(-gender.text) %>% 
  pivot_longer(cols = contains("gender."), 
               names_to = "Gender_ID",
               values_to = "Gender_T_F")%>% 
  dplyr::select(PID, contains("Gender_")) %>% 
  mutate(Gender_T_F = if_else(is.na(Gender_T_F), 0, Gender_T_F)) %>% 
  group_by(Gender_ID) %>% 
  summarise(Perc_of_sample = mean(Gender_T_F))


#Race Distribution

pilot.baseline %>% 
  pivot_longer(cols = contains("race."), 
               names_to = "race_ID",
               values_to = "race_T_F")%>% 
  dplyr::select(PID, contains("race_")) %>% 
  mutate(race_T_F = if_else(is.na(race_T_F), 0, race_T_F)) %>% 
  group_by(race_ID) %>% 
  summarise(Perc_of_sample = mean(race_T_F))

# Aggregating EMAs and merging with baseline





# how many EMAs report quant but not intox or intox hours for alc and mar. look into each category and see if other data is weird


# Mar
  #no quant, some intox or intox hours
pilot.ema %>% 
  filter(mar.quantjoint == 0 & mar.intoxhours != 0 | 
         mar.quantjoint == 0 & mar.intox != 0) %>%
  dplyr::select(contains(c("mar", "PID"))) %>% group_by(PID) %>% count() %>% arrange(desc(n))

  #no quant, some intoxhours
pilot.ema %>% 
  filter(mar.quantjoint == 0 & mar.intoxhours != 0) %>% dplyr::select(contains(c("mar", "PID"))) %>% group_by(PID) %>% count() %>% arrange(desc(n))
pilot.ema %>% 
  filter(mar.quantjoint == 0 & mar.intoxhours != 0) %>% 
  summarise(intoxhours = median(mar.intoxhours, na.rm = T))

  #no quant, some intox
pilot.ema %>% 
  filter(mar.quantjoint == 0 & mar.intox != 0) %>% dplyr::select(contains(c("mar", "PID"))) %>% group_by(PID) %>% count() %>% arrange(desc(n))
pilot.ema %>% 
  filter(mar.quantjoint == 0 & mar.intox != 0) %>%
  summarise(intox = median(mar.intox, na.rm = T))

    #no quant, intox > 10
pilot.ema %>% 
  filter(mar.quantjoint == 0 & mar.intox > 10) %>% dplyr::select(contains(c("mar", "PID"))) %>% group_by(PID) %>% count() %>% arrange(desc(n))

pilot.ema %>% 
  filter(mar.quantjoint == 0 & mar.intox > 10 & mar.intoxhours == 0)  %>% dplyr::select(contains(c("mar", "PID"))) %>% group_by(PID) %>% count() %>% arrange(desc(n))


    # 339 EMAs (134 PIDs) where someone reported no quant, but some intox or intoxhours, mostly intox (331 EMAs, 133 PIDs), some intox hours (40 EMAs,24 PIDs)
    # most folks without quant have low intox and intox hours (median intox = 2.26, median intoxhours = 1)
      # 18 EMAs (12 PIDs) where report greater than 10 intox, but report no use. 11 EMAs (7 PIDs) where report no use, no intoxhours, but >10 intox 
      #I'm afraid some folks accidentally clicked the intox slider, should I look into the responses where report intox, but nothing else? Or report high intox without anything else.
          #Could also be heavy users that stay high after heavy use, but not sure since alc less people than mar, but still a lot.

# Thinking of only including folks who report more than 10 intox, any intox hours, or any quant, thoughts?


#alc
#no quant, some intox or intox hours
pilot.ema %>% 
  filter(alc.quant == 0 & alc.intoxhours != 0 | 
           alc.quant == 0 & alc.intox != 0) %>%
 dplyr::select(contains(c("alc", "PID"))) %>% group_by(PID) %>% count() %>% arrange(desc(n))

#no quant, some intoxhours
pilot.ema %>% 
  filter(alc.quant == 0 & alc.intoxhours != 0) %>% dplyr::select(contains(c("alc", "PID"))) %>% group_by(PID) %>% count() %>% arrange(desc(n))
pilot.ema %>% 
  filter(alc.quant == 0 & alc.intoxhours != 0) %>% 
  summarise(intoxhours = median(alc.intoxhours, na.rm = T))

#no quant, some intox
pilot.ema %>% 
  filter(alc.quant == 0 & alc.intox != 0) %>% dplyr::select(contains(c("alc", "PID"))) %>% group_by(PID) %>% count() %>% arrange(desc(n))
pilot.ema %>% 
  filter(alc.quant == 0 & alc.intox != 0) %>%
  summarise(intox = median(alc.intox, na.rm = T))


#no quant, intox > 10
pilot.ema %>% 
  filter(alc.quant == 0 & alc.intox > 10) %>% dplyr::select(contains(c("alc", "PID"))) %>% group_by(PID) %>% count() %>% arrange(desc(n))

pilot.ema %>% 
  filter(alc.quant == 0 & alc.intox > 10 & alc.intoxhours == 0)  %>% dplyr::select(contains(c("alc", "PID"))) %>% group_by(PID) %>% count() %>% arrange(desc(n))



    # 145 EMAs (77 PIDs) where someone reported no quant, but some intox or intoxhours, mostly intox (141 EMAs, 75 PIDs), some intox hours (21 EMAs, 18 PIDs)
    # most folks without quant have low intox and intox hours (median intox = 2.26, median intoxhours = 1, why exact same as mar? )
    #I'm afraid some folks accidentally clicked the intox slider, should I look into the responses where report intox, but nothing else?  High intox (>10) and nothing else?


#Double checking DMU3 and DDTQ and DDQ measures


pilot.baseline %>% 
  filter(DMU3 == 0 & DDTQ.typ.use.week > 0) %>% 
  dplyr::select(DMU3, contains("DDTQ")) #Two pts, one makes sense, other reports lots of use in DDTQ, not in DMU3 (should remove?)

pilot.baseline %>% 
  filter(DMU3 > 0 & DDTQ.typ.use.week == 0) %>% 
  dplyr::select(DMU3, contains("DDTQ")) #all folks reporting 0 for DDTQ report using less than one day a week in DMU3

pilot.baseline %>% 
  filter(is.na(DMU3)) %>% 
  dplyr::select(DMU3, contains("DDTQ")) # all NAs in DMU3 report no use in DDTQ


hist(pilot.baseline$DDTQ.typ.use.week) #Still right skewed but looking a lot prettier
hist(pilot.baseline$DDQ.typ.drinks.week) # a little more normally distributed now, still technically right skewed.  

pilot.baseline %>% 
  dplyr::select(DMU3, DDTQ.typ.use.week) %>% 
  na.omit() %>% 
  cor() #r = .92 likely won't make a difference using one or the other for analysis, but can do more to double check if needed.



#Merge ema and baseline datasets


# EMA's aggregated by PID

#data wrangling

pilot.ema.use.day <- pilot.ema %>% 
  group_by(PID) %>% 
  mutate(er.disengagement = mean(er.disengagement, na.rm = T)) %>% 
  group_by(PID, Date) %>% 
  tidyr::fill(alc.quant.b, mar.quantjoint.b, .direction = "downup") %>% 
  dplyr::select(alc.quant.b, mar.quantjoint.b, er.disengagement) %>% 
  summarise_all(mean, na.rm = T) %>%
  ungroup() %>% 
  group_by(PID) %>% 
  summarise_all(mean, na.rm = T) 
#This dataframe is better for merging



pilot.ema.participant <- pilot.ema %>% 
  group_by(PID) %>% 
  summarise(alc.quant.b = mean(alc.quant.b, na.rm = T),
            alc.quant = mean(alc.quant, na.rm = T),
            mar.quantjoint.b = mean(mar.quantjoint.b, na.rm = T),
            mar.quantjoint = mean(mar.quantjoint, na.rm = T),
            er.disengagement = mean(er.disengagement, na.rm = T))

#Histograms

hist(pilot.ema.use.day$alc.quant.b) #right skewed trending downards towards roof
hist(pilot.ema.use.day$mar.quantjoint.b) #severely right skewed
hist(pilot.ema.use.day$er.disengagement) 






df <- pilot.baseline %>% left_join(pilot.ema.use.day, by = "PID") %>% 
  dplyr::select(age, 
                PROMIS.DEP.T,
                PROMIS.ANX.T,
                contains(c(".coping", 
                         "CERQ.dis", 
                         "ERS.full", 
                         "nt.b", 
                         "er.disen", 
                         ".week",
                         "PID",
                         "gender.",
                         "race.",
                         "UPPS.")),
                -UPPS.sensationseeking,
                -UPPS.perseverance) %>% 
  mutate(white_nonwhite = case_when(race.hispanic == 1 & is.na(race.white) | #I realize we already coded race and gender, but I already wrote this so it's staying unless it's worse
                                      race.eastasian == 1 & is.na(race.white) |
                                      race.southeastasian == 1 & is.na(race.white) |
                                      race.pacificislander == 1 & is.na(race.white) |
                                      race.southasian == 1 & is.na(race.white) |
                                      race.amerindian == 1 & is.na(race.white) |
                                      race.africanamer == 1 & is.na(race.white) |
                                      race.african == 1 & is.na(race.white) |
                                      race.middleeastern == 1 & is.na(race.white) |
                                      race.other == 1 & is.na(race.white) |
                                      race.mixed == 1 & is.na(race.white) ~ 0, 
                                    race.white == 1 & is.na(race.eastasian) |
                                      race.white == 1 & is.na(race.southeastasian) |
                                      race.white == 1 & is.na(race.pacificislander) |
                                      race.white == 1 & is.na(race.southasian) |
                                      race.white == 1 & is.na(race.amerindian) |
                                      race.white == 1 & is.na(race.africanamer) |
                                      race.white == 1 & is.na(race.middleeastern) |
                                      race.white == 1 & is.na(race.other) |
                                      race.white == 1 & is.na(race.mixed) |
                                      race.white == 1 & is.na(race.african) |
                                      race.white == 1 & is.na(race.hispanic) ~ 1,
                                    TRUE ~ NA_real_),
         gender = case_when(gender.male == 1 ~ "Male",
                            gender.female == 1 ~ "Female",
                            !is.na(gender.intersex) |
                            !is.na(gender.genderqueer) |
                            !is.na(gender.transgender) |
                            !is.na(gender.nongendered) |
                            !is.na(gender.other) ~ "Other",
                                    TRUE ~ NA_character_)) %>% 
  dplyr::select(-contains(c("gender.", "race."))) %>% 
  filter(gender != "Other") #Only one person identified as an "other" gender, will remove because it breaks Negative binomial models

#ISSUES:
 #18 NAs for EMA disengagement, take a look at them to see what's up since this is a calculated variable

er.na <- df %>% filter(is.na(er.disengagement)) %>% dplyr::select(PID) %>% pull()

pilot.ema %>% filter(PID %in% er.na)


# it looks like these are all ema's where folks didn't report any er strategies at all. Makes sense, participants still have an aggregate er.disengagement

# use days variables check.

df %>% filter(alc.quant.b == 0 | alc.quant.b == 1)
df %>% filter(mar.quantjoint.b == 0, mar.quantjoint.b == 1)
df %>% filter(alc.quant.b == 0 & 
              mar.quantjoint.b == 0 & 
              DDQ.typ.drinks.week == 0 & 
              DDTQ.typ.use.week == 0)

# Also took a look at use distributions, lots of no use at any EMA day and use during all EMA days (N = 302) 
  #49 pts drank no days, and 143 participants used weed on no EMA days
  #14 participants drank on all days, 25 participants used weed on all days
  #17 participants reported no use at all in emas or in DDQ and DDTQ. Is this our intended sample? Should we remove if they had no use?



# Take a look to see if anyone had no use at all.





# Data Analysis - maybe move to another Script.
  # Correlation table
  # in Psych can do pairs.panel
  # H1a first try to do Global ER and Global coping for both alc and mar
  # H1b EMA ER with Global Coping
  # Coping with ERS, UPPS: Neg/Pos Urg, Anxiety, Depression
  # Coping UPPS: Premeditation (opposite)


pairs.panels(df[,c("DMQ.coping", 
                   "MMQ.coping", 
                   "CERQ.disengagement",
                   "ERS.full",
                   "UPPS.negativeurgency",
                   "UPPS.positiveurgency",
                   "UPPS.premeditation",
                   "er.disengagement",
                   "PROMIS.DEP.T",
                   "PROMIS.ANX.T")],
             stars = T,
             cex.cor = 2)


# According to this we would have demo, ERS, pos.urg, Anxiety and depression for full DMQ model, 
# According to this we would have demo, Anxiety and depression for full MMQ model. However, ANX and DEP are highly correlated (separate models for both? see if any difference) 

  # We said we would include disengagement in second model, but did not specify if disengagement needed to 
  # be above a certain threshold. I'm leaning towards including one or both 
  # (they were moderately correlated around .3***), but would there be any issues doing this?

# let's add stadardized variables for our scales: CERQ, ERS, pos.urg, 
  # will only be using standardized version of PROMIS scales

df$ERS.full.s <- as.numeric(scale(df$ERS.full))
df$UPPS.positiveurgency.s <- as.numeric(scale(df$UPPS.positiveurgency)) 



#ad hoc ideas
  # if wanted, can compare Disengagement if same strategy as an idea
  # compare ema and baseline disengagment significantly.


#let's fit the models, starting with bivariate regressions with use regressed on Coping motives 



#bivariate regression 4 types: EMA, Global use Mar and Alc
  # 1. Alc Global
  # 2. Alc EMA
  # 3. Mar Global
  # 4. Mar EMA

#1. Global Coping Motives on Global Alc  

m0.alc.base.normal <- glm(DDQ.typ.drinks.week ~ DMQ.coping, data = df)
# m0.alc.base.nb <- glm.nb(DDQ.typ.drinks.week ~ DMQ.coping, data =  df, control = glm.control(maxit = 1000)) 
  # currently not fitting returning error with package update
m0.alc.base.pois <- glm(DDQ.typ.drinks.week ~ DMQ.coping, data =  df, "poisson")
anova(m0.alc.base.normal, m0.alc.base.pois)
AIC(m0.alc.base.normal, m0.alc.base.pois)


   # pois/nb explain data slightly more than gaussian, but not more than 2. Siding with gaussian for parsimony.

#standardized formula
m0.alc.base.normal.s <- glm(DDQ.typ.drinks.week ~ as.numeric(scale(DMQ.coping)), data = df)


summary(m0.alc.base.normal)
summary(m0.alc.base.normal.s)

plot(m0.alc.base.normal)

    # Significant UN-standardized effect of .25 
    # Significant standardized effect of .2



#Taking a look at the graphs to see how well the models fit

#first unstandardized

# Fitting model to data frame so we can graph.
NEWDATA <- data.frame(DMQ.coping = seq(1,5, length.out = nobs(m0.alc.base.normal))) #nobs function to get rightn number of observations
pred <- predict(m0.alc.base.normal, se.fit=T, newdata = NEWDATA) 

# CI function
make_ci <- function(pred){
  
  # fit, lower, and upper CI
  fit <- pred$fit
  lower <- fit - 1.96*pred$se.fit
  upper <- fit + 1.96*pred$se.fit
  
  return(data.frame(fit, lower, upper))
}

normal.graph.0 <- make_ci(pred)

normal.graph.0 <- data.frame(fit = normal.graph.0$fit, 
                             upper = normal.graph.0$upper, 
                             lower = normal.graph.0$lower,
                             DMQ.coping = seq(1,5, length.out = nobs(m0.alc.base.normal)))

normal.graph.0 <- df %>%
  filter(!is.na(DMQ.coping)) %>% 
  mutate(fit = DDQ.typ.drinks.week) %>% 
  ggplot(., aes(x = DMQ.coping, y = fit))+
  geom_point()+
  geom_jitter(alpha = .25)+
  geom_line(size = 1, data = normal.graph.0, aes(x = DMQ.coping, y = fit)) +
  geom_ribbon(data = normal.graph.0, aes(ymin = lower, ymax = upper), alpha= .2, fill = "gray4") +
  geom_vline(xintercept =  2.10, color = "red", size = 1) +
  geom_vline(xintercept = c(2.10-0.9786804, 2.10+0.9786804), color = "blue", size = 1) +  
  ylab("Weekly Predicted Use Days") +
  theme(legend.position = "none") +
  ggtitle("m0.alc.base.normal")


#Now Standardized Graph



# Fitting model to data frame so we can graph.
NEWDATA <- data.frame(DMQ.coping = seq(-1.2,3.1, length.out = nobs(m0.alc.base.normal.s))) #nobs function to get rightn number of observations
pred <- predict(m0.alc.base.normal.s, se.fit=T, newdata = NEWDATA) 

# CI function
make_ci <- function(pred){
  
  # fit, lower, and upper CI
  fit <- pred$fit
  lower <- fit - 1.96*pred$se.fit
  upper <- fit + 1.96*pred$se.fit
  
  return(data.frame(fit, lower, upper))
}

normal.graph.0.s <- make_ci(pred)

normal.graph.0.s <- data.frame(fit = normal.graph.0.s$fit, 
                             upper = normal.graph.0.s$upper, 
                             lower = normal.graph.0.s$lower,
                             DMQ.coping = seq(-1.2,3.1, length.out = nobs(m0.alc.base.normal.s)))

normal.graph.0.s <- df %>%
  filter(!is.na(DMQ.coping)) %>% 
  mutate(fit = DDQ.typ.drinks.week,
         DMQ.coping = as.numeric(scale(DMQ.coping))) %>% 
  ggplot(., aes(x = DMQ.coping, y = fit))+
  geom_point()+
  geom_jitter(alpha = .25)+
  geom_line(size = 1, data = normal.graph.0.s, aes(x = DMQ.coping, y = fit)) +
  geom_ribbon(data = normal.graph.0.s, aes(ymin = lower, ymax = upper), alpha= .2, fill = "gray4") +
  ylab("Weekly Predicted Use Days") +
  theme(legend.position = "none") +
  ggtitle("m0.alc.base.normal.s")




#2. Global Coping Motives on Momentary Alc

m0.alc.ema.binomial <- glm(alc.quant.b ~ DMQ.coping, data = df, family = "binomial")
m0.alc.ema.binomial.s <- glm(formula = alc.quant.b ~ as.numeric(scale(DMQ.coping)), 
                             family = "binomial", data = df)
exp(coef(summary(m0.alc.ema.binomial))[2])
exp(coef(summary(m0.alc.ema.binomial.s))[2])
    # Non-significant UN-standardized effect of .098 log odds (1.1 odds)
    # Non-significant standardized effect of .093 log odds (1.1 odds)


#Unstandardized graph

# Fitting model to data frame so we can graph.
NEWDATA <- data.frame(DMQ.coping = seq(1,5, length.out = nobs(m0.alc.ema.binomial))) #nobs function to get rightn number of observations
pred <- predict(m0.alc.ema.binomial, se.fit=T, newdata = NEWDATA, type = "link") 

# CI function
make_ci <- function(pred){
  
  # fit, lower, and upper CI
  fit <- pred$fit
  lower <- fit - 1.96*pred$se.fit
  upper <- fit + 1.96*pred$se.fit
  
  return(data.frame(exp(fit), exp(lower), exp(upper)))
}

binomial.alc.graph.0 <- make_ci(pred)

binomial.alc.graph.0 <- data.frame(fit = binomial.alc.graph.0$exp.fit., 
                               upper = binomial.alc.graph.0$exp.upper., 
                               lower = binomial.alc.graph.0$exp.lower.,
                               DMQ.coping = seq(1,5, length.out = nobs(m0.alc.ema.binomial)))
                               
binomial.alc.graph.0 <- df %>%
  filter(!is.na(DMQ.coping) & !is.na(alc.quant.b)) %>% 
  mutate(fit = alc.quant.b) %>% 
  ggplot(., aes(x = DMQ.coping, y = fit))+
  geom_point()+
  geom_jitter(alpha = .25)+
  geom_line(size = 1, data = binomial.alc.graph.0, aes(x = DMQ.coping, y = fit)) +
  geom_ribbon(data = binomial.alc.graph.0, aes(ymin = lower, ymax = upper), alpha= .2, fill = "gray4") +
  geom_vline(xintercept =  2.10, color = "red", size = 1) +
  geom_vline(xintercept = c(2.10-0.9786804, 2.10+0.9786804), color = "blue", size = 1) +  
  labs(caption = "Note: Red line represents mean, blue line represents \n 1 std from mean or boundary of x-axis. Min response on X-axis is 1") +
  ylab("Proportion of Use Days in Study") +
  ylim(0,1)+
  theme(legend.position = "none") +
  ggtitle("m0.alc.ema.binomial")

#NOTE:variance is too wide around 3.5-5 on the x-axis so ggplot stops the ribbons there, 
  #Can set higher y boundaries to fix, but doesn't have clean 0,1 range that I want.



#Standardized graph

# Fitting model to data frame so we can graph.
NEWDATA <- data.frame(DMQ.coping = seq(-1.2,3.2, length.out = nobs(m0.alc.ema.binomial.s))) #nobs function to get rightn number of observations
pred <- predict(m0.alc.ema.binomial.s, se.fit=T, newdata = NEWDATA, type = "link") 

# CI function
make_ci <- function(pred){
  
  # fit, lower, and upper CI
  fit <- pred$fit
  lower <- fit - 1.96*pred$se.fit
  upper <- fit + 1.96*pred$se.fit
  
  return(data.frame(exp(fit), exp(lower), exp(upper)))
}

binomial.alc.graph.0.s <- make_ci(pred)

binomial.alc.graph.0.s <- data.frame(fit = binomial.alc.graph.0.s$exp.fit., 
                                   upper = binomial.alc.graph.0.s$exp.upper., 
                                   lower = binomial.alc.graph.0.s$exp.lower.,
                                   DMQ.coping = seq(-1.2,3.1, length.out = nobs(m0.alc.ema.binomial.s)))



binomial.alc.graph.0.s <- df %>%
  filter(!is.na(DMQ.coping) & !is.na(alc.quant.b)) %>% 
  mutate(fit = alc.quant.b,
         DMQ.coping = as.numeric(scale(DMQ.coping))) %>% 
  ggplot(., aes(x = DMQ.coping, y = fit))+
  geom_point()+
  geom_jitter(alpha = .25)+
  geom_line(size = 1, data = binomial.alc.graph.0.s, aes(x = DMQ.coping, y = fit)) +
  geom_ribbon(data = binomial.alc.graph.0.s, aes(ymin = lower, ymax = upper), alpha= .2, fill = "gray4") +
  ylab("Proportion of Use Days in Study") +
  theme(legend.position = "none") +
  ggtitle("m0.alc.ema.binomial.s")





#3. Global Coping Motives on Global Mar  

m0.mar.base.normal <- glm(DDTQ.typ.use.week ~ MMQ.coping, data = df)
m0.mar.base.nb <- glm.nb(DDTQ.typ.use.week ~ MMQ.coping, data = df, control = glm.control(maxit = 1000))
m0.mar.base.pois <- glm(DDTQ.typ.use.week ~ MMQ.coping, data = df, "poisson")
anova(m0.mar.base.normal, m0.mar.base.nb, m0.mar.base.pois)
AIC(m0.mar.base.normal, m0.mar.base.nb, m0.mar.base.pois)

    # Negative binomial explains data better than pois/normal. Siding with Negative Binomial.
    # What assumptions do we need to check for Negative binomial?
      # Poisson - Predictor needs to independently predict outcome, one drinking day should not influence the nexts (can just be transparent about this?)
      # NB/Pois - majority of sqrd standardized residuals need to be within 0-1.5
      # Dispersion value of 1.27, so not exactly 1, but it seems NB explains more data anyway.
      # Explanation of NB distribution seems a little different from our distribution, more appropriate with Poisson, however want to check. Will it still be the same interpretation?
      # if interpretation is same, then side with NB, if not, Poisson (residuals fit and look better for NB)

#standardized
m0.mar.base.nb.s <- glm.nb(DDTQ.typ.use.week ~ as.numeric(scale(MMQ.coping)), data = df, control = glm.control(maxit = 1000))


summary(m0.mar.base.nb)
summary(m0.mar.base.nb.s)

    # Significant UN-standardized effect of .54 
    # Significant standardized effect of .53


#Taking a look at the graphs to see how well the models fit

#first unstandardized

# Fitting model to data frame so we can graph.
NEWDATA <- data.frame(MMQ.coping = seq(1,5, length.out = nobs(m0.mar.base.nb))) #nobs function to get rightn number of observations
pred <- predict(m0.mar.base.nb, se.fit=T, newdata = NEWDATA, type = "response") 

# CI function
make_ci <- function(pred){
  
  # fit, lower, and upper CI
  fit <- pred$fit
  lower <- fit - 1.96*pred$se.fit
  upper <- fit + 1.96*pred$se.fit
  
  return(data.frame(fit, lower, upper))
}

nb.graph.0 <- make_ci(pred)

nb.graph.0 <- data.frame(fit = nb.graph.0$fit, 
                            upper = nb.graph.0$upper, 
                            lower = nb.graph.0$lower,
                            MMQ.coping = seq(1,5, length.out = nobs(m0.mar.base.nb)))

nb.graph.0 <- df %>%
  filter(!is.na(MMQ.coping)) %>% 
  mutate(fit = DDTQ.typ.use.week) %>% 
  ggplot(., aes(x = MMQ.coping, y = fit))+
  geom_point()+
  geom_jitter(alpha = .25)+
  geom_line(size = 1, data = nb.graph.0, aes(x = MMQ.coping, y = fit)) +
  ylim(0,7)+
  geom_ribbon(data = nb.graph.0, aes(ymin = lower, ymax = upper), alpha= .2, fill = "gray4") +
  geom_vline(xintercept =  1.71, color = "red", size = 1,) +
  geom_vline(xintercept = c(1, 1.71+0.9786804), color = "blue", size = 1) +  
  labs(caption = "Note: Red line represents mean, blue line represents \n 1 std from mean or boundary of x-axis. Min response on X-axis is 1") +
  ylab("Weekly Predicted Use Days") +
  theme(legend.position = "none") +
  ggtitle("nb.graph.0")





#Standardized

# Fitting model to data frame so we can graph.
NEWDATA <- data.frame(MMQ.coping = seq(-.73,3.36, length.out = nobs(m0.mar.base.nb.s))) #nobs function to get rightn number of observations
pred <- predict(m0.mar.base.nb.s, se.fit=T, newdata = NEWDATA, type = "response") 

# CI function
make_ci <- function(pred){
  
  # fit, lower, and upper CI
  fit <- pred$fit
  lower <- fit - 1.96*pred$se.fit
  upper <- fit + 1.96*pred$se.fit
  
  return(data.frame(fit, lower, upper))
}

nb.graph.0.s <- make_ci(pred)

nb.graph.0.s <- data.frame(fit = nb.graph.0.s$fit, 
                         upper = nb.graph.0.s$upper, 
                         lower = nb.graph.0.s$lower,
                         MMQ.coping = seq(-.73,3.36, length.out = nobs(m0.mar.base.nb.s)))


nb.graph.0.s <- df %>%
  filter(!is.na(MMQ.coping)) %>% 
  mutate(fit = DDTQ.typ.use.week,
         MMQ.coping = as.numeric(scale(MMQ.coping))) %>% 
  ggplot(., aes(x = MMQ.coping, y = fit))+
  geom_point()+
  geom_jitter(alpha = .25)+
  geom_line(size = 1, data = nb.graph.0.s, aes(x = MMQ.coping, y = fit)) +
  ylim(0,7)+
  geom_ribbon(data = nb.graph.0.s, aes(ymin = lower, ymax = upper), alpha= .2, fill = "gray4") +
  ylab("Weekly Predicted Use Days") +
  theme(legend.position = "none") +
  ggtitle("nb.graph.0.s")


#4. Global Coping Motives on Momentary Mar

m0.mar.ema.binomial <- glm(mar.quantjoint.b ~ MMQ.coping, data = df, family = "binomial") 
m0.mar.ema.binomial.s <- glm(mar.quantjoint.b ~ as.numeric(scale(MMQ.coping)), data = df, family = "binomial") 
exp(coef(summary(m0.mar.ema.binomial))[2])
exp(coef(summary(m0.mar.ema.binomial.s))[2])

    # Significant UN-standardized effect of .75 log odds (2.12 odds)
    # Significant standardized effect of .74 log odds (2.1 odds)


#Unstandardized graph

# Fitting model to data frame so we can graph.

NEWDATA <- data.frame(MMQ.coping = seq(1,5, length.out = nobs(m0.mar.ema.binomial))) #nobs function to get rightn number of observations
pred <- predict(m0.mar.ema.binomial, se.fit=T, newdata = NEWDATA, type = "link") 

# CI function
make_ci <- function(pred){
  
  # fit, lower, and upper CI
  fit <- pred$fit
  lower <- fit - 1.96*pred$se.fit
  upper <- fit + 1.96*pred$se.fit
  
  return(data.frame(exp(fit), exp(lower), exp(upper)))
}

binomial.mar.graph.0 <- make_ci(pred)

binomial.mar.graph.0 <- data.frame(fit = binomial.mar.graph.0$exp.fit., 
                                   upper = binomial.mar.graph.0$exp.upper., 
                                   lower = binomial.mar.graph.0$exp.lower.,
                                   MMQ.coping = seq(1,5, length.out = nobs(m0.mar.ema.binomial)))

binomial.mar.graph.0 <- df %>%
  filter(!is.na(MMQ.coping) & !is.na(mar.quantjoint.b)) %>% 
  mutate(fit = mar.quantjoint.b) %>% 
  ggplot(., aes(x = MMQ.coping, y = fit))+
  geom_point()+
  geom_jitter(alpha = .25)+
  geom_line(size = 1, data = binomial.mar.graph.0, aes(x = MMQ.coping, y = fit)) +
  geom_ribbon(data = binomial.mar.graph.0, aes(ymin = lower, ymax = upper), alpha= .2, fill = "gray4") +
  geom_vline(xintercept =  1.71, color = "red", size = 1) +
  geom_vline(xintercept = c(1, 1.71+0.9786804), color = "blue", size = 1) +  
  labs(caption = "Note: Red line represents mean, blue line represents \n 1 std from mean or boundary of x-axis. Min response on X-axis is 1") +
  ylab("Proportion of Use Days in Study") +
  ylim(0,1)+
  theme(legend.position = "none") +
  ggtitle("m0.mar.ema.binomial")

#NOTE:variance is too wide around 3.5-5 on the x-axis so ggplot stops the ribbons there, 
#Can set higher y boundaries to fix, but doesn't have clean 0,1 range that I want.



#Standardized graph

# Fitting model to data frame so we can graph.
NEWDATA <- data.frame(MMQ.coping = seq(-.73,3.4, length.out = nobs(m0.mar.ema.binomial.s))) #nobs function to get rightn number of observations
pred <- predict(m0.mar.ema.binomial.s, se.fit=T, newdata = NEWDATA, type = "link") 

# CI function
make_ci <- function(pred){
  
  # fit, lower, and upper CI
  fit <- pred$fit
  lower <- fit - 1.96*pred$se.fit
  upper <- fit + 1.96*pred$se.fit
  
  return(data.frame(exp(fit), exp(lower), exp(upper)))
}

binomial.mar.graph.0.s <- make_ci(pred)

binomial.mar.graph.0.s <- data.frame(fit = binomial.mar.graph.0.s$exp.fit., 
                                     upper = binomial.mar.graph.0.s$exp.upper., 
                                     lower = binomial.mar.graph.0.s$exp.lower.,
                                     MMQ.coping = seq(-.73,3.4, length.out = nobs(m0.mar.ema.binomial.s)))



binomial.mar.graph.0.s <- df %>%
  filter(!is.na(MMQ.coping) & !is.na(mar.quantjoint.b)) %>% 
  mutate(fit = mar.quantjoint.b,
         MMQ.coping = as.numeric(scale(MMQ.coping))) %>% 
  ggplot(., aes(x = MMQ.coping, y = fit))+
  geom_point()+
  geom_jitter(alpha = .25)+
  geom_line(size = 1, data = binomial.mar.graph.0.s, aes(x = MMQ.coping, y = fit)) +
  geom_ribbon(data = binomial.mar.graph.0.s, aes(ymin = lower, ymax = upper), alpha= .2, fill = "gray4") +
  ylab("Proportion of Use Days in Study") +
  ylim(0,1) +
  theme(legend.position = "none") +
  ggtitle("m0.mar.ema.binomial.s")



# Time for demographics: Step 2 models, demographics and Coping motives



#Multiple regression 4 types: EMA, Global use Mar and Alc
# 1. Alc Global
# 2. Alc EMA
# 3. Mar Global
# 4. Mar EMA



# 1. Alc Gobal

m1.alc.base.normal <- glm(DDQ.typ.drinks.week ~ DMQ.coping + age + gender + white_nonwhite, data = df)
m1.alc.base.normal.s <- glm(DDQ.typ.drinks.week ~ as.numeric(scale(DMQ.coping)) + 
                                                  age + gender + white_nonwhite, data = df)


summary(m1.alc.base.normal)
summary(m1.alc.base.normal.s)

# Significant Un-standardized effect of .332
# Significant Standardized effect of .313

AIC(m0.alc.base.normal, m1.alc.base.normal) #m1 explain data better than null model


# Graphs
#first unstandardized

# Fitting model to data frame so we can graph.
NEWDATA <- data.frame(DMQ.coping = seq(1,5, length.out = nobs(m1.alc.base.normal)),
                      age = mean(df$age, na.rm = T), 
                      gender = "Female", 
                      white_nonwhite = mean(df$white_nonwhite, na.rm = T)) #nobs function to get rightn number of observations
pred <- predict(m1.alc.base.normal, se.fit=T, newdata = NEWDATA) 

# CI function
make_ci <- function(pred){
  
  # fit, lower, and upper CI
  fit <- pred$fit
  lower <- fit - 1.96*pred$se.fit
  upper <- fit + 1.96*pred$se.fit
  
  return(data.frame(fit, lower, upper))
}

normal.graph.1 <- make_ci(pred)

normal.graph.1 <- data.frame(fit = normal.graph.1$fit, 
                             upper = normal.graph.1$upper, 
                             lower = normal.graph.1$lower,
                             DMQ.coping = seq(1,5, length.out = nobs(m1.alc.base.normal)))

normal.graph.1 <- df %>%
  filter(!is.na(DMQ.coping)) %>% 
  mutate(fit = DDQ.typ.drinks.week) %>% 
  ggplot(., aes(x = DMQ.coping, y = fit))+
  geom_point()+
  geom_jitter(alpha = .25)+
  geom_line(size = 1, data = normal.graph.1, aes(x = DMQ.coping, y = fit)) +
  geom_ribbon(data = normal.graph.1, aes(ymin = lower, ymax = upper), alpha= .2, fill = "gray4") +
  geom_vline(xintercept =  2.10, color = "red", size = 1) +
  geom_vline(xintercept = c(2.10-0.9786804, 2.10+0.9786804), color = "blue", size = 1) +  
  ylim(0,7) +
  labs(caption = "Note: Red line represents mean, blue line represents \n 1 std from mean or boundary of x-axis. Min response on X-axis is 1") +
  ylab("Weekly Predicted Use Days") +
  theme(legend.position = "none") +
  ggtitle("m1.alc.base.normal")


#Now Standardized Graph



# Fitting model to data frame so we can graph.
NEWDATA <- data.frame(DMQ.coping = seq(-1.2,3.1, length.out = nobs(m1.alc.base.normal.s)),
                      age = mean(df$age, na.rm = T), 
                      gender = "Male", 
                      white_nonwhite = mean(df$white_nonwhite, na.rm = T)) #nobs function to get right number of observations
pred <- predict(m1.alc.base.normal.s, se.fit=T, newdata = NEWDATA) 

# CI function
make_ci <- function(pred){
  
  # fit, lower, and upper CI
  fit <- pred$fit
  lower <- fit - 1.96*pred$se.fit
  upper <- fit + 1.96*pred$se.fit
  
  return(data.frame(fit, lower, upper))
}

normal.graph.1.s <- make_ci(pred)

normal.graph.1.s <- data.frame(fit = normal.graph.1.s$fit, 
                               upper = normal.graph.1.s$upper, 
                               lower = normal.graph.1.s$lower,
                               DMQ.coping = seq(-1.2,3.1, length.out = nobs(m1.alc.base.normal.s)))

normal.graph.1.s <- df %>%
  filter(!is.na(DMQ.coping)) %>% 
  mutate(fit = DDQ.typ.drinks.week,
         DMQ.coping = as.numeric(scale(DMQ.coping))) %>% 
  ggplot(., aes(x = DMQ.coping, y = fit))+
  geom_point()+
  geom_jitter(alpha = .25)+
  geom_line(size = 1, data = normal.graph.1.s, aes(x = DMQ.coping, y = fit)) +
  geom_ribbon(data = normal.graph.1.s, aes(ymin = lower, ymax = upper), alpha= .2, fill = "gray4") +
  ylab("Weekly Predicted Use Days") +
  theme(legend.position = "none") +
  ggtitle("m1.alc.base.normal.s")





#2. Global Coping Motives on Momentary Alc

m1.alc.ema.binomial <- glm(alc.quant.b ~ DMQ.coping + age + gender + white_nonwhite, data = df, family = "binomial")
m1.alc.ema.binomial.s <- glm(alc.quant.b ~ as.numeric(scale(DMQ.coping)) + age + 
                             gender + white_nonwhite, data = df, family = "binomial")

exp(coef(summary(m1.alc.ema.binomial))[2])
exp(coef(summary(m1.alc.ema.binomial.s))[2])


# Non-significant UN-standardized effect of .167 log odds (1.18 odds)
# Non-significant standardized effect of .157 log odds (1.17 odds)


  # Just like baseline, effect size increases, but this stays nonsignificant

AIC(m0.alc.ema.binomial, m1.alc.ema.binomial) #just over 2 AIC units difference between null and m1


#Unstandardized graph

# Fitting model to data frame so we can graph.
NEWDATA <- data.frame(DMQ.coping = seq(1,5, length.out = nobs(m1.alc.ema.binomial)),
                      age = mean(df$age, na.rm = T), 
                      gender = "Male", 
                      white_nonwhite = mean(df$white_nonwhite, na.rm = T)) #nobs function to get rightn number of observations
pred <- predict(m1.alc.ema.binomial, se.fit=T, newdata = NEWDATA, type = "link") 

# CI function
make_ci <- function(pred){
  
  # fit, lower, and upper CI
  fit <- pred$fit
  lower <- fit - 1.96*pred$se.fit
  upper <- fit + 1.96*pred$se.fit
  
  return(data.frame(exp(fit), exp(lower), exp(upper)))
}

binomial.alc.graph.1 <- make_ci(pred)

binomial.alc.graph.1 <- data.frame(fit = binomial.alc.graph.1$exp.fit., 
                                   upper = binomial.alc.graph.1$exp.upper., 
                                   lower = binomial.alc.graph.1$exp.lower.,
                                   DMQ.coping = seq(1,5, length.out = nobs(m1.alc.ema.binomial)))

binomial.alc.graph.1 <- df %>%
  filter(!is.na(DMQ.coping) & !is.na(alc.quant.b)) %>% 
  mutate(fit = alc.quant.b) %>% 
  ggplot(., aes(x = DMQ.coping, y = fit))+
  geom_point()+
  geom_jitter(alpha = .25)+
  geom_line(size = 1, data = binomial.alc.graph.1, aes(x = DMQ.coping, y = fit)) +
  geom_ribbon(data = binomial.alc.graph.1, aes(ymin = lower, ymax = upper), alpha= .2, fill = "gray4") +
  geom_vline(xintercept =  2.10, color = "red", size = 1) +
  geom_vline(xintercept = c(2.10-0.9786804, 2.10+0.9786804), color = "blue", size = 1) +  
  labs(caption = "Note: Red line represents mean, blue line represents \n 1 std from mean or boundary of x-axis. Min response on X-axis is 1") +
  ylab("Proportion of Use Days in Study") +
  ylim(0,1)+
  theme(legend.position = "none") +
  ggtitle("m1.alc.ema.binomial")

#NOTE:variance is too wide around 3.5-5 on the x-axis so ggplot stops the ribbons there, 
#Can set higher y boundaries to fix, but doesn't have clean 0,1 range that I want.



#Standardized graph

# Fitting model to data frame so we can graph.
NEWDATA <- data.frame(DMQ.coping = seq(-1.2,3.2, length.out = nobs(m1.alc.ema.binomial.s)),
                      age = mean(df$age, na.rm = T), 
                      gender = "Male", 
                      white_nonwhite = mean(df$white_nonwhite, na.rm = T)) #nobs function to get rightn number of observations
pred <- predict(m1.alc.ema.binomial.s, se.fit=T, newdata = NEWDATA, type = "link") 

# CI function
make_ci <- function(pred){
  
  # fit, lower, and upper CI
  fit <- pred$fit
  lower <- fit - 1.96*pred$se.fit
  upper <- fit + 1.96*pred$se.fit
  
  return(data.frame(exp(fit), exp(lower), exp(upper)))
}

binomial.alc.graph.1.s <- make_ci(pred)

binomial.alc.graph.1.s <- data.frame(fit = binomial.alc.graph.1.s$exp.fit., 
                                     upper = binomial.alc.graph.1.s$exp.upper., 
                                     lower = binomial.alc.graph.1.s$exp.lower.,
                                     DMQ.coping = seq(-1.2,3.1, length.out = nobs(m1.alc.ema.binomial.s)))



binomial.alc.graph.1.s <- df %>%
  filter(!is.na(DMQ.coping) & !is.na(alc.quant.b)) %>% 
  mutate(fit = alc.quant.b,
         DMQ.coping = as.numeric(scale(DMQ.coping))) %>% 
  ggplot(., aes(x = DMQ.coping, y = fit))+
  geom_point()+
  geom_jitter(alpha = .25)+
  geom_line(size = 1, data = binomial.alc.graph.1.s, aes(x = DMQ.coping, y = fit)) +
  geom_ribbon(data = binomial.alc.graph.1.s, aes(ymin = lower, ymax = upper), alpha= .2, fill = "gray4") +
  ylim(0,1) +
  ylab("Proportion of Use Days in Study") +
  theme(legend.position = "none") +
  ggtitle("m1.alc.ema.binomial.s")








#3. Global Coping Motives on Global Mar  

m1.mar.base.nb <- glm.nb(DDTQ.typ.use.week ~ MMQ.coping + age + gender + white_nonwhite, data = df, control = glm.control(maxit = 1000)) 
m1.mar.base.nb.s <- glm.nb(DDTQ.typ.use.week ~ as.numeric(scale(MMQ.coping)) + age + 
                             gender + white_nonwhite, data = df, control = glm.control(maxit = 1000))


exp(coef(summary(m1.mar.base.nb)))
exp(coef(summary(m1.mar.base.nb.s)))

# Significant UN-standardized effect of .58 log odds (1.79 odds)
# Significant UN-standardized effect of .57 log odds (1.77 odds)



# Graphs
#first unstandardized

# Fitting model to data frame so we can graph.

NEWDATA <- data.frame(MMQ.coping = seq(1,5, length.out = nobs(m1.mar.base.nb)),
                      age = mean(df$age, na.rm = T), 
                      gender = "Male", 
                      white_nonwhite = mean(df$white_nonwhite, na.rm = T)) #nobs function to get rightn number of observations
pred <- predict(m1.mar.base.nb, se.fit=T, newdata = NEWDATA, type = "response") 

# CI function
make_ci <- function(pred){
  
  # fit, lower, and upper CI
  fit <- pred$fit
  lower <- fit - 1.96*pred$se.fit
  upper <- fit + 1.96*pred$se.fit
  
  return(data.frame(fit, lower, upper))
}

nb.graph.1 <- make_ci(pred)

nb.graph.1 <- data.frame(fit = nb.graph.1$fit, 
                         upper = nb.graph.1$upper, 
                         lower = nb.graph.1$lower,
                         MMQ.coping = seq(1,5, length.out = nobs(m1.mar.base.nb)))

nb.graph.1 <- df %>%
  filter(!is.na(MMQ.coping)) %>% 
  mutate(fit = DDTQ.typ.use.week) %>% 
  ggplot(., aes(x = MMQ.coping, y = fit))+
  geom_point()+
  geom_jitter(alpha = .25)+
  geom_line(size = 1, data = nb.graph.1, aes(x = MMQ.coping, y = fit)) +
  ylim(0,7)+
  geom_ribbon(data = nb.graph.1, aes(ymin = lower, ymax = upper), alpha= .2, fill = "gray4") +
  geom_vline(xintercept =  1.71, color = "red", size = 1,) +
  geom_vline(xintercept = c(1, 1.71+0.9786804), color = "blue", size = 1) +  
  labs(caption = "Note: Red line represents mean, blue line represents \n 1 std from mean or boundary of x-axis. Min response on X-axis is 1") +
  ylab("Weekly Predicted Use Days") +
  theme(legend.position = "none") +
  ggtitle("nb.graph.1")





#Standardized

# Fitting model to data frame so we can graph.
NEWDATA <- data.frame(MMQ.coping = seq(-.73,3.36, length.out = nobs(m1.mar.base.nb.s)),
                      age = mean(df$age, na.rm = T), 
                      gender = "Male", 
                      white_nonwhite = mean(df$white_nonwhite, na.rm = T)) #nobs function to get rightn number of observations
pred <- predict(m1.mar.base.nb.s, se.fit=T, newdata = NEWDATA, type = "response") 

# CI function
make_ci <- function(pred){
  
  # fit, lower, and upper CI
  fit <- pred$fit
  lower <- fit - 1.96*pred$se.fit
  upper <- fit + 1.96*pred$se.fit
  
  return(data.frame(fit, lower, upper))
}

nb.graph.1.s <- make_ci(pred)

nb.graph.1.s <- data.frame(fit = nb.graph.1.s$fit, 
                           upper = nb.graph.1.s$upper, 
                           lower = nb.graph.1.s$lower,
                           MMQ.coping = seq(-.73,3.36, length.out = nobs(m1.mar.base.nb.s)))


nb.graph.1.s <- df %>%
  filter(!is.na(MMQ.coping)) %>% 
  mutate(fit = DDTQ.typ.use.week,
         MMQ.coping = as.numeric(scale(MMQ.coping))) %>% 
  ggplot(., aes(x = MMQ.coping, y = fit))+
  geom_point()+
  geom_jitter(alpha = .25)+
  geom_line(size = 1, data = nb.graph.1.s, aes(x = MMQ.coping, y = fit)) +
  ylim(0,7)+
  geom_ribbon(data = nb.graph.1.s, aes(ymin = lower, ymax = upper), alpha= .2, fill = "gray4") +
  ylab("Weekly Predicted Use Days") +
  theme(legend.position = "none") +
  ggtitle("nb.graph.1.s")


#4. Global Coping Motives on Momentary Mar

m1.mar.ema.binomial <- glm(mar.quantjoint.b ~ MMQ.coping + age + gender + white_nonwhite, data = df, family = "binomial") # find citations at some point for proportion logistic #Error: In eval(family$initialize) : non-integer #successes in a binomial glm! ## think it should be okay
m1.mar.ema.binomial.s <- glm(mar.quantjoint.b ~ as.numeric(scale(MMQ.coping)) + age + 
                             gender + white_nonwhite, data = df, family = "binomial")


exp(coef(summary(m1.mar.ema.binomial)))
exp(coef(summary(m1.mar.ema.binomial.s)))

# Significant UN-standardized effect of .833 log odds (2.3 odds)
# Significant standardized effect of .816 log odds (2.26)


#Unstandardized graph

# Fitting model to data frame so we can graph.

NEWDATA <- data.frame(MMQ.coping = seq(1,5, length.out = nobs(m1.mar.ema.binomial)),
                      age = mean(df$age, na.rm = T), 
                      gender = "Male", 
                      white_nonwhite = mean(df$white_nonwhite, na.rm = T)) #nobs function to get rightn number of observations
pred <- predict(m1.mar.ema.binomial, se.fit=T, newdata = NEWDATA, type = "link") 

# CI function
make_ci <- function(pred){
  
  # fit, lower, and upper CI
  fit <- pred$fit
  lower <- fit - 1.96*pred$se.fit
  upper <- fit + 1.96*pred$se.fit
  
  return(data.frame(exp(fit), exp(lower), exp(upper)))
}

binomial.mar.graph.1 <- make_ci(pred)

binomial.mar.graph.1 <- data.frame(fit = binomial.mar.graph.1$exp.fit., 
                                   upper = binomial.mar.graph.1$exp.upper., 
                                   lower = binomial.mar.graph.1$exp.lower.,
                                   MMQ.coping = seq(1,5, length.out = nobs(m1.mar.ema.binomial)))

binomial.mar.graph.1 <- df %>%
  filter(!is.na(MMQ.coping) & !is.na(mar.quantjoint.b)) %>% 
  mutate(fit = mar.quantjoint.b) %>% 
  ggplot(., aes(x = MMQ.coping, y = fit))+
  geom_point()+
  geom_jitter(alpha = .25)+
  geom_line(size = 1, data = binomial.mar.graph.1, aes(x = MMQ.coping, y = fit)) +
  geom_ribbon(data = binomial.mar.graph.1, aes(ymin = lower, ymax = upper), alpha= .2, fill = "gray4") +
  geom_vline(xintercept =  1.71, color = "red", size = 1) +
  geom_vline(xintercept = c(1, 1.71+0.9786804), color = "blue", size = 1) +  
  labs(caption = "Note: Red line represents mean, blue line represents \n 1 std from mean or boundary of x-axis. Min response on X-axis is 1") +
  ylab("Proportion of Use Days in Study") +
  ylim(0,1)+
  theme(legend.position = "none") +
  ggtitle("m1.mar.ema.binomial")

#NOTE:variance is too wide around 3.5-5 on the x-axis so ggplot stops the ribbons there, 
#Can set higher y boundaries to fix, but doesn't have clean 0,1 range that I want.



#Standardized graph

# Fitting model to data frame so we can graph.
NEWDATA <- data.frame(MMQ.coping = seq(-.73,3.4, length.out = nobs(m1.mar.ema.binomial.s)),
                      age = mean(df$age, na.rm = T), 
                      gender = "Male", 
                      white_nonwhite = mean(df$white_nonwhite, na.rm = T)) #nobs function to get rightn number of observations
pred <- predict(m1.mar.ema.binomial.s, se.fit=T, newdata = NEWDATA, type = "link") 

# CI function
make_ci <- function(pred){
  
  # fit, lower, and upper CI
  fit <- pred$fit
  lower <- fit - 1.96*pred$se.fit
  upper <- fit + 1.96*pred$se.fit
  
  return(data.frame(exp(fit), exp(lower), exp(upper)))
}

binomial.mar.graph.1.s <- make_ci(pred)

binomial.mar.graph.1.s <- data.frame(fit = binomial.mar.graph.1.s$exp.fit., 
                                     upper = binomial.mar.graph.1.s$exp.upper., 
                                     lower = binomial.mar.graph.1.s$exp.lower.,
                                     MMQ.coping = seq(-.73,3.4, length.out = nobs(m1.mar.ema.binomial.s)))



binomial.mar.graph.1.s <- df %>%
  filter(!is.na(MMQ.coping) & !is.na(mar.quantjoint.b)) %>% 
  mutate(fit = mar.quantjoint.b,
         MMQ.coping = as.numeric(scale(MMQ.coping))) %>% 
  ggplot(., aes(x = MMQ.coping, y = fit))+
  geom_point()+
  geom_jitter(alpha = .25)+
  geom_line(size = 1, data = binomial.mar.graph.1.s, aes(x = MMQ.coping, y = fit)) +
  geom_ribbon(data = binomial.mar.graph.1.s, aes(ymin = lower, ymax = upper), alpha= .2, fill = "gray4") +
  ylab("Proportion of Use Days in Study") +
  ylim(0,1) +
  theme(legend.position = "none") +
  ggtitle("m1.mar.ema.binomial.s")



# Now for demographics and Emotional Disengagement: Step 3 models, demographics, Coping motives, and ER: Disengagement



#Multiple regression 4 types: EMA, Global use Mar and Alc
# 1. Alc Global
# 2. Alc EMA
# 3. Mar Global
# 4. Mar EMA


### Should we include both global and momentary disengagement for all models or keep only the appropriate measure for the model?
    ### i.e. use CERQ for global and er.disengagement for momentary, or include both in all models?

    ### they're correlated at .28***, but one might argue that one depends on the other, so I think it's fine to only include the appropriate one

# 1. Alc Gobal

m2.alc.base.normal <- glm(DDQ.typ.drinks.week ~ DMQ.coping + 
                            age + 
                            gender + 
                            white_nonwhite + 
                            CERQ.disengagement, data = df)


m2.alc.base.normal.s <-  glm(DDQ.typ.drinks.week ~ as.numeric(scale(DMQ.coping)) + 
                                                   age + gender + white_nonwhite + 
                                                   CERQ.disengagement, data = df)


summary(m2.alc.base.normal) 
summary(m2.alc.base.normal.s) 



AIC(m2.alc.base.normal, m1.alc.base.normal) #m2 1.5 AIC difference, m1 predicts better than m2


# Graphs
#first unstandardized

# Fitting model to data frame so we can graph.
NEWDATA <- data.frame(DMQ.coping = seq(1,5, length.out = nobs(m2.alc.base.normal)),
                      age = mean(df$age, na.rm = T), 
                      gender = "Female", 
                      white_nonwhite = mean(df$white_nonwhite, na.rm = T),
                      CERQ.disengagement = mean(df$CERQ.disengagement, na.rm = T)) #nobs function to get rightn number of observations
pred <- predict(m2.alc.base.normal, se.fit=T, newdata = NEWDATA) 

# CI function
make_ci <- function(pred){
  
  # fit, lower, and upper CI
  fit <- pred$fit
  lower <- fit - 1.96*pred$se.fit
  upper <- fit + 1.96*pred$se.fit
  
  return(data.frame(fit, lower, upper))
}

normal.graph.2 <- make_ci(pred)

normal.graph.2 <- data.frame(fit = normal.graph.2$fit, 
                             upper = normal.graph.2$upper, 
                             lower = normal.graph.2$lower,
                             DMQ.coping = seq(1,5, length.out = nobs(m2.alc.base.normal)))

normal.graph.2 <- df %>%
  filter(!is.na(DMQ.coping)) %>% 
  mutate(fit = DDQ.typ.drinks.week) %>% 
  ggplot(., aes(x = DMQ.coping, y = fit))+
  geom_point()+
  geom_jitter(alpha = .25)+
  geom_line(size = 1, data = normal.graph.2, aes(x = DMQ.coping, y = fit)) +
  geom_ribbon(data = normal.graph.2, aes(ymin = lower, ymax = upper), alpha= .2, fill = "gray4") +
  geom_vline(xintercept =  2.10, color = "red", size = 1) +
  geom_vline(xintercept = c(2.10-0.9786804, 2.10+0.9786804), color = "blue", size = 1) +  
  ylim(0,7) +
  labs(caption = "Note: Red line represents mean, blue line represents \n 1 std from mean or boundary of x-axis. Min response on X-axis is 1") +
  ylab("Weekly Predicted Use Days") +
  theme(legend.position = "none") +
  ggtitle("m2.alc.base.normal")


#Now Standardized Graph



# Fitting model to data frame so we can graph.
NEWDATA <- data.frame(DMQ.coping = seq(-1.2,3.1, length.out = nobs(m2.alc.base.normal.s)),
                      age = mean(df$age, na.rm = T), 
                      gender = "Male", 
                      white_nonwhite = mean(df$white_nonwhite, na.rm = T),
                      CERQ.disengagement = mean(df$CERQ.disengagement, na.rm = T)) #nobs function to get right number of observations
pred <- predict(m2.alc.base.normal.s, se.fit=T, newdata = NEWDATA) 

# CI function
make_ci <- function(pred){
  
  # fit, lower, and upper CI
  fit <- pred$fit
  lower <- fit - 1.96*pred$se.fit
  upper <- fit + 1.96*pred$se.fit
  
  return(data.frame(fit, lower, upper))
}

normal.graph.2.s <- make_ci(pred)

normal.graph.2.s <- data.frame(fit = normal.graph.2.s$fit, 
                               upper = normal.graph.2.s$upper, 
                               lower = normal.graph.2.s$lower,
                               DMQ.coping = seq(-1.2,3.1, length.out = nobs(m2.alc.base.normal.s)))

normal.graph.2.s <- df %>%
  filter(!is.na(DMQ.coping)) %>% 
  mutate(fit = DDQ.typ.drinks.week,
         DMQ.coping = as.numeric(scale(DMQ.coping))) %>% 
  ggplot(., aes(x = DMQ.coping, y = fit))+
  geom_point()+
  geom_jitter(alpha = .25)+
  geom_line(size = 1, data = normal.graph.2.s, aes(x = DMQ.coping, y = fit)) +
  geom_ribbon(data = normal.graph.2.s, aes(ymin = lower, ymax = upper), alpha= .2, fill = "gray4") +
  ylab("Weekly Predicted Use Days") +
  theme(legend.position = "none") +
  ggtitle("m2.alc.base.normal.s")




#2. Global Coping Motives on Momentary Alc

m2.alc.ema.binomial <- glm(alc.quant.b ~ DMQ.coping + age + gender + white_nonwhite + er.disengagement, data = df, family = "binomial")
m2.alc.ema.binomial.s <- glm(alc.quant.b ~ as.numeric(scale(DMQ.coping)) + 
                                          age + gender + white_nonwhite + 
                                          er.disengagement, data = df, family = "binomial")




exp(coef(summary(m2.alc.ema.binomial))[2])
exp(coef(summary(m2.alc.ema.binomial.s))[2])


# Non-significant UN-standardized effect of .17 log odds (1.86 odds)
# Non-significant Standardized effect of .169 log odds (1.84 odds)


# Just like baseline, effect size increases, but this stays nonsignificant

AIC(m2.alc.ema.binomial, m1.alc.ema.binomial) #just over 2 AIC units difference between m1 and m2, m1 better explains data




#Unstandardized graph

# Fitting model to data frame so we can graph.
NEWDATA <- data.frame(DMQ.coping = seq(1,5, length.out = nobs(m2.alc.ema.binomial)),
                      age = mean(df$age, na.rm = T), 
                      gender = "Male", 
                      white_nonwhite = mean(df$white_nonwhite, na.rm = T),
                      er.disengagement = mean(df$er.disengagement, na.rm = T)) #nobs function to get rightn number of observations
pred <- predict(m2.alc.ema.binomial, se.fit=T, newdata = NEWDATA, type = "link") 

# CI function
make_ci <- function(pred){
  
  # fit, lower, and upper CI
  fit <- pred$fit
  lower <- fit - 1.96*pred$se.fit
  upper <- fit + 1.96*pred$se.fit
  
  return(data.frame(exp(fit), exp(lower), exp(upper)))
}

binomial.alc.graph.2 <- make_ci(pred)

binomial.alc.graph.2 <- data.frame(fit = binomial.alc.graph.2$exp.fit., 
                                   upper = binomial.alc.graph.2$exp.upper., 
                                   lower = binomial.alc.graph.2$exp.lower.,
                                   DMQ.coping = seq(1,5, length.out = nobs(m2.alc.ema.binomial)))

binomial.alc.graph.2 <- df %>%
  filter(!is.na(DMQ.coping) & !is.na(alc.quant.b)) %>% 
  mutate(fit = alc.quant.b) %>% 
  ggplot(., aes(x = DMQ.coping, y = fit))+
  geom_point()+
  geom_jitter(alpha = .25)+
  geom_line(size = 1, data = binomial.alc.graph.2, aes(x = DMQ.coping, y = fit)) +
  geom_ribbon(data = binomial.alc.graph.2, aes(ymin = lower, ymax = upper), alpha= .2, fill = "gray4") +
  geom_vline(xintercept =  2.10, color = "red", size = 1) +
  geom_vline(xintercept = c(2.10-0.9786804, 2.10+0.9786804), color = "blue", size = 1) +  
  labs(caption = "Note: Red line represents mean, blue line represents \n 1 std from mean or boundary of x-axis. Min response on X-axis is 1") +
  ylab("Proportion of Use Days in Study") +
  ylim(0,1)+
  theme(legend.position = "none") +
  ggtitle("m2.alc.ema.binomial")

#NOTE:variance is too wide around 3.5-5 on the x-axis so ggplot stops the ribbons there, 
#Can set higher y boundaries to fix, but doesn't have clean 0,1 range that I want.



#Standardized graph

# Fitting model to data frame so we can graph.
NEWDATA <- data.frame(DMQ.coping = seq(-1.2,3.2, length.out = nobs(m2.alc.ema.binomial.s)),
                      age = mean(df$age, na.rm = T), 
                      gender = "Male", 
                      white_nonwhite = mean(df$white_nonwhite, na.rm = T),
                      er.disengagement = mean(df$er.disengagement, na.rm = T)) #nobs function to get rightn number of observations
pred <- predict(m2.alc.ema.binomial.s, se.fit=T, newdata = NEWDATA, type = "link") 

# CI function
make_ci <- function(pred){
  
  # fit, lower, and upper CI
  fit <- pred$fit
  lower <- fit - 1.96*pred$se.fit
  upper <- fit + 1.96*pred$se.fit
  
  return(data.frame(exp(fit), exp(lower), exp(upper)))
}

binomial.alc.graph.2.s <- make_ci(pred)

binomial.alc.graph.2.s <- data.frame(fit = binomial.alc.graph.2.s$exp.fit., 
                                     upper = binomial.alc.graph.2.s$exp.upper., 
                                     lower = binomial.alc.graph.2.s$exp.lower.,
                                     DMQ.coping = seq(-1.2,3.1, length.out = nobs(m2.alc.ema.binomial.s)))



binomial.alc.graph.2.s <- df %>%
  filter(!is.na(DMQ.coping) & !is.na(alc.quant.b)) %>% 
  mutate(fit = alc.quant.b,
         DMQ.coping = as.numeric(scale(DMQ.coping))) %>% 
  ggplot(., aes(x = DMQ.coping, y = fit))+
  geom_point()+
  geom_jitter(alpha = .25)+
  geom_line(size = 1, data = binomial.alc.graph.2.s, aes(x = DMQ.coping, y = fit)) +
  geom_ribbon(data = binomial.alc.graph.2.s, aes(ymin = lower, ymax = upper), alpha= .2, fill = "gray4") +
  ylab("Proportion of Use Days in Study") +
  ylim(0,1) +
  theme(legend.position = "none") +
  ggtitle("m2.alc.ema.binomial.s")






#3. Global Coping Motives on Global Mar  

m2.mar.base.nb <- glm.nb(DDTQ.typ.use.week ~ MMQ.coping + age + gender + white_nonwhite + CERQ.disengagement, data = df, control = glm.control(maxit = 1000)) 
m2.mar.base.nb.s <- glm.nb(DDTQ.typ.use.week ~ as.numeric(scale(MMQ.coping)) + 
                                               age + gender + white_nonwhite + 
                                               CERQ.disengagement, 
                           data = df, control = glm.control(maxit = 1000)) 



summary(m2.mar.base.nb)
summary(m2.mar.base.nb.s)
# Significant UN-standardized effect of .59 
# Significant Standardized effect of .57 


AIC(m2.mar.base.nb, m1.mar.base.nb) #just over 1 AIC units difference between m1 and m2, m1 better explains data


# Graphs
#first unstandardized

# Fitting model to data frame so we can graph.

NEWDATA <- data.frame(MMQ.coping = seq(1,5, length.out = nobs(m2.mar.base.nb)),
                      age = mean(df$age, na.rm = T), 
                      gender = "Male", 
                      white_nonwhite = mean(df$white_nonwhite, na.rm = T),
                      CERQ.disengagement = mean(df$CERQ.disengagement, na.rm = T)) #nobs function to get rightn number of observations
pred <- predict(m2.mar.base.nb, se.fit=T, newdata = NEWDATA, type = "response") 

# CI function
make_ci <- function(pred){
  
  # fit, lower, and upper CI
  fit <- pred$fit
  lower <- fit - 1.96*pred$se.fit
  upper <- fit + 1.96*pred$se.fit
  
  return(data.frame(fit, lower, upper))
}

nb.graph.2 <- make_ci(pred)

nb.graph.2 <- data.frame(fit = nb.graph.2$fit, 
                         upper = nb.graph.2$upper, 
                         lower = nb.graph.2$lower,
                         MMQ.coping = seq(1,5, length.out = nobs(m2.mar.base.nb)))

nb.graph.2 <- df %>%
  filter(!is.na(MMQ.coping)) %>% 
  mutate(fit = DDTQ.typ.use.week) %>% 
  ggplot(., aes(x = MMQ.coping, y = fit))+
  geom_point()+
  geom_jitter(alpha = .25)+
  geom_line(size = 1, data = nb.graph.2, aes(x = MMQ.coping, y = fit)) +
  ylim(0,7)+
  geom_ribbon(data = nb.graph.2, aes(ymin = lower, ymax = upper), alpha= .2, fill = "gray4") +
  geom_vline(xintercept =  1.71, color = "red", size = 1,) +
  geom_vline(xintercept = c(1, 1.71+0.9786804), color = "blue", size = 1) +  
  labs(caption = "Note: Red line represents mean, blue line represents \n 1 std from mean or boundary of x-axis. Min response on X-axis is 1") +
  ylab("Weekly Predicted Use Days") +
  theme(legend.position = "none") +
  ggtitle("nb.graph.2")





#Standardized

# Fitting model to data frame so we can graph.
NEWDATA <- data.frame(MMQ.coping = seq(-.73,3.36, length.out = nobs(m2.mar.base.nb.s)),
                      age = mean(df$age, na.rm = T), 
                      gender = "Male", 
                      white_nonwhite = mean(df$white_nonwhite, na.rm = T),
                      CERQ.disengagement = mean(df$CERQ.disengagement, na.rm = T)) #nobs function to get rightn number of observations
pred <- predict(m2.mar.base.nb.s, se.fit=T, newdata = NEWDATA, type = "response") 

# CI function
make_ci <- function(pred){
  
  # fit, lower, and upper CI
  fit <- pred$fit
  lower <- fit - 1.96*pred$se.fit
  upper <- fit + 1.96*pred$se.fit
  
  return(data.frame(fit, lower, upper))
}

nb.graph.2.s <- make_ci(pred)

nb.graph.2.s <- data.frame(fit = nb.graph.2.s$fit, 
                           upper = nb.graph.2.s$upper, 
                           lower = nb.graph.2.s$lower,
                           MMQ.coping = seq(-.73,3.36, length.out = nobs(m2.mar.base.nb.s)))


nb.graph.2.s <- df %>%
  filter(!is.na(MMQ.coping)) %>% 
  mutate(fit = DDTQ.typ.use.week,
         MMQ.coping = as.numeric(scale(MMQ.coping))) %>% 
  ggplot(., aes(x = MMQ.coping, y = fit))+
  geom_point()+
  geom_jitter(alpha = .25)+
  geom_line(size = 1, data = nb.graph.2.s, aes(x = MMQ.coping, y = fit)) +
  ylim(0,7)+
  geom_ribbon(data = nb.graph.2.s, aes(ymin = lower, ymax = upper), alpha= .2, fill = "gray4") +
  ylab("Weekly Predicted Use Days") +
  theme(legend.position = "none") +
  ggtitle("nb.graph.2.s")


#4. Global Coping Motives on Momentary Mar

m2.mar.ema.binomial <- glm(mar.quantjoint.b ~ MMQ.coping + age + gender + white_nonwhite + er.disengagement, data = df, family = "binomial") # find citations at some point for proportion logistic #Error: In eval(family$initialize) : non-integer #successes in a binomial glm! ## think it should be okay
m2.mar.ema.binomial.s <- glm(mar.quantjoint.b ~ as.numeric(scale(MMQ.coping)) + 
                                                age + gender + white_nonwhite + 
                                                er.disengagement, data = df, family = "binomial") # find citations at some point for proportion logistic #Error: In eval(family$initialize) : non-integer #successes in a binomial glm! ## think it should be okay



exp(coef(summary(m2.mar.ema.binomial))[2])
exp(coef(summary(m2.mar.ema.binomial.s))[2])

# Significant UN-standardized effect of .86 log odds (2.36 odds)
# Significant standardized effect of .84 log odds (2.32 odds)

AIC(m2.mar.ema.binomial, m1.mar.ema.binomial) #just under 1 AIC units difference between m1 and m2, m1 better explains data


#Unstandardized graph

# Fitting model to data frame so we can graph.

NEWDATA <- data.frame(MMQ.coping = seq(1,5, length.out = nobs(m2.mar.ema.binomial)),
                      age = mean(df$age, na.rm = T), 
                      gender = "Male", 
                      white_nonwhite = mean(df$white_nonwhite, na.rm = T),
                      er.disengagement = mean(df$er.disengagement, na.rm = T)) #nobs function to get rightn number of observations
pred <- predict(m2.mar.ema.binomial, se.fit=T, newdata = NEWDATA, type = "link") 

# CI function
make_ci <- function(pred){
  
  # fit, lower, and upper CI
  fit <- pred$fit
  lower <- fit - 1.96*pred$se.fit
  upper <- fit + 1.96*pred$se.fit
  
  return(data.frame(exp(fit), exp(lower), exp(upper)))
}

binomial.mar.graph.2 <- make_ci(pred)

binomial.mar.graph.2 <- data.frame(fit = binomial.mar.graph.2$exp.fit., 
                                   upper = binomial.mar.graph.2$exp.upper., 
                                   lower = binomial.mar.graph.2$exp.lower.,
                                   MMQ.coping = seq(1,5, length.out = nobs(m2.mar.ema.binomial)))

binomial.mar.graph.2 <- df %>%
  filter(!is.na(MMQ.coping) & !is.na(mar.quantjoint.b)) %>% 
  mutate(fit = mar.quantjoint.b) %>% 
  ggplot(., aes(x = MMQ.coping, y = fit))+
  geom_point()+
  geom_jitter(alpha = .25)+
  geom_line(size = 1, data = binomial.mar.graph.2, aes(x = MMQ.coping, y = fit)) +
  geom_ribbon(data = binomial.mar.graph.2, aes(ymin = lower, ymax = upper), alpha= .2, fill = "gray4") +
  geom_vline(xintercept =  1.71, color = "red", size = 1) +
  geom_vline(xintercept = c(1, 1.71+0.9786804), color = "blue", size = 1) +  
  labs(caption = "Note: Red line represents mean, blue line represents \n 1 std from mean or boundary of x-axis. Min response on X-axis is 1") +
  ylab("Proportion of Use Days in Study") +
  ylim(0,1)+
  theme(legend.position = "none") +
  ggtitle("m2.mar.ema.binomial")

#NOTE:variance is too wide around 3.5-5 on the x-axis so ggplot stops the ribbons there, 
#Can set higher y boundaries to fix, but doesn't have clean 0,1 range that I want.



#Standardized graph

# Fitting model to data frame so we can graph.
NEWDATA <- data.frame(MMQ.coping = seq(-.73,3.4, length.out = nobs(m2.mar.ema.binomial.s)),
                      age = mean(df$age, na.rm = T), 
                      gender = "Male", 
                      white_nonwhite = mean(df$white_nonwhite, na.rm = T),
                      er.disengagement = mean(df$er.disengagement, na.rm = T)) #nobs function to get rightn number of observations
pred <- predict(m2.mar.ema.binomial.s, se.fit=T, newdata = NEWDATA, type = "link") 

# CI function
make_ci <- function(pred){
  
  # fit, lower, and upper CI
  fit <- pred$fit
  lower <- fit - 1.96*pred$se.fit
  upper <- fit + 1.96*pred$se.fit
  
  return(data.frame(exp(fit), exp(lower), exp(upper)))
}

binomial.mar.graph.2.s <- make_ci(pred)

binomial.mar.graph.2.s <- data.frame(fit = binomial.mar.graph.2.s$exp.fit., 
                                     upper = binomial.mar.graph.2.s$exp.upper., 
                                     lower = binomial.mar.graph.2.s$exp.lower.,
                                     MMQ.coping = seq(-.73,3.4, length.out = nobs(m2.mar.ema.binomial.s)))



binomial.mar.graph.2.s <- df %>%
  filter(!is.na(MMQ.coping) & !is.na(mar.quantjoint.b)) %>% 
  mutate(fit = mar.quantjoint.b,
         MMQ.coping = as.numeric(scale(MMQ.coping))) %>% 
  ggplot(., aes(x = MMQ.coping, y = fit))+
  geom_point()+
  geom_jitter(alpha = .25)+
  geom_line(size = 1, data = binomial.mar.graph.2.s, aes(x = MMQ.coping, y = fit)) +
  geom_ribbon(data = binomial.mar.graph.2.s, aes(ymin = lower, ymax = upper), alpha= .2, fill = "gray4") +
  ylab("Proportion of Use Days in Study") +
  ylim(0,1) +
  theme(legend.position = "none") +
  ggtitle("m2.mar.ema.binomial.s")






# Finally for similar constructs, demographics and Emotional Disengagement: Step 4 models, demographics, Coping motives, and ER: Disengagement



#Multiple regression 4 types: EMA, Global use Mar and Alc
# 1. Alc Global
# 2. Alc EMA
# 3. Mar Global
# 4. Mar EMA




# 1. Alc Gobal

m3.alc.base.normal <- glm(DDQ.typ.drinks.week ~ DMQ.coping + age + gender + white_nonwhite + CERQ.disengagement + PROMIS.ANX.T + PROMIS.DEP.T + ERS.full + UPPS.positiveurgency, data = df)
m3.alc.base.normal.s <- glm(DDQ.typ.drinks.week ~ as.numeric(scale(DMQ.coping)) + 
                              age + gender + white_nonwhite + 
                              CERQ.disengagement + 
                              PROMIS.ANX.T + 
                              PROMIS.DEP.T + 
                              ERS.full.s + 
                              UPPS.positiveurgency.s, data = df)


# Only anxiety and depression were correlated enough to include as theoretically related constructs.

summary(m3.alc.base.normal) 
summary(m3.alc.base.normal.s)

# Non-significant UN-standardized effect of .166
# Non-significant Standardized effect of .126




AIC(m3.alc.base.normal, m2.alc.base.normal, m1.alc.base.normal) #m3 is a bit of a difference, m3 predicts better than m2 and m1,
# Not much of a difference with disengagement, but a bit when including theoretically similar constructs.



# Graphs
#first unstandardized

# Fitting model to data frame so we can graph.
NEWDATA <- data.frame(DMQ.coping = seq(1,5, length.out = nobs(m3.alc.base.normal)),
                      age = mean(df$age, na.rm = T), 
                      gender = "Female", 
                      white_nonwhite = mean(df$white_nonwhite, na.rm = T),
                      CERQ.disengagement = mean(df$CERQ.disengagement, na.rm = T),
                      PROMIS.ANX.T = mean(df$PROMIS.ANX.T, na.rm = T),
                      PROMIS.DEP.T = mean(df$PROMIS.DEP.T, na.rm = T),
                      ERS.full = mean(df$ERS.full, na.rm = T),
                      UPPS.positiveurgency = mean(df$UPPS.positiveurgency, na.rm = T)) #nobs function to get rightn number of observations
pred <- predict(m3.alc.base.normal, se.fit=T, newdata = NEWDATA) 

# CI function
make_ci <- function(pred){
  
  # fit, lower, and upper CI
  fit <- pred$fit
  lower <- fit - 1.96*pred$se.fit
  upper <- fit + 1.96*pred$se.fit
  
  return(data.frame(fit, lower, upper))
}

normal.graph.3 <- make_ci(pred)

normal.graph.3 <- data.frame(fit = normal.graph.3$fit, 
                             upper = normal.graph.3$upper, 
                             lower = normal.graph.3$lower,
                             DMQ.coping = seq(1,5, length.out = nobs(m3.alc.base.normal)))

normal.graph.3 <- df %>%
  filter(!is.na(DMQ.coping)) %>% 
  mutate(fit = DDQ.typ.drinks.week) %>% 
  ggplot(., aes(x = DMQ.coping, y = fit))+
  geom_point()+
  geom_jitter(alpha = .25)+
  geom_line(size = 1, data = normal.graph.3, aes(x = DMQ.coping, y = fit)) +
  geom_ribbon(data = normal.graph.3, aes(ymin = lower, ymax = upper), alpha= .2, fill = "gray4") +
  geom_vline(xintercept =  2.10, color = "red", size = 1) +
  geom_vline(xintercept = c(2.10-0.9786804, 2.10+0.9786804), color = "blue", size = 1) +  
  ylim(0,7) +
  labs(caption = "Note: Red line represents mean, blue line represents \n 1 std from mean or boundary of x-axis. Min response on X-axis is 1") +
  ylab("Weekly Predicted Use Days") +
  theme(legend.position = "none") +
  ggtitle("m3.alc.base.normal")


#Now Standardized Graph



# Fitting model to data frame so we can graph.
NEWDATA <- data.frame(DMQ.coping = seq(-1.2,3.1, length.out = nobs(m3.alc.base.normal.s)),
                      age = mean(df$age, na.rm = T), 
                      gender = "Male", 
                      white_nonwhite = mean(df$white_nonwhite, na.rm = T),
                      CERQ.disengagement = mean(df$CERQ.disengagement, na.rm = T),
                      PROMIS.ANX.T = mean(df$PROMIS.ANX.T, na.rm = T),
                      PROMIS.DEP.T = mean(df$PROMIS.DEP.T, na.rm = T),
                      ERS.full.s = mean(df$ERS.full.s, na.rm = T),
                      UPPS.positiveurgency.s = mean(df$UPPS.positiveurgency.s, na.rm = T)) #nobs function to get right number of observations
pred <- predict(m3.alc.base.normal.s, se.fit=T, newdata = NEWDATA) 

# CI function
make_ci <- function(pred){
  
  # fit, lower, and upper CI
  fit <- pred$fit
  lower <- fit - 1.96*pred$se.fit
  upper <- fit + 1.96*pred$se.fit
  
  return(data.frame(fit, lower, upper))
}

normal.graph.3.s <- make_ci(pred)

normal.graph.3.s <- data.frame(fit = normal.graph.3.s$fit, 
                               upper = normal.graph.3.s$upper, 
                               lower = normal.graph.3.s$lower,
                               DMQ.coping = seq(-1.2,3.1, length.out = nobs(m3.alc.base.normal.s)))

normal.graph.3.s <- df %>%
  filter(!is.na(DMQ.coping)) %>% 
  mutate(fit = DDQ.typ.drinks.week,
         DMQ.coping = as.numeric(scale(DMQ.coping))) %>% 
  ggplot(., aes(x = DMQ.coping, y = fit))+
  geom_point()+
  geom_jitter(alpha = .25)+
  geom_line(size = 1, data = normal.graph.3.s, aes(x = DMQ.coping, y = fit)) +
  geom_ribbon(data = normal.graph.3.s, aes(ymin = lower, ymax = upper), alpha= .2, fill = "gray4") +
  ylab("Weekly Predicted Use Days") +
  theme(legend.position = "none") +
  ggtitle("m3.alc.base.normal.s")




#2. Global Coping Motives on Momentary Alc

m3.alc.ema.binomial <- glm(alc.quant.b ~ DMQ.coping + age + gender + white_nonwhite + er.disengagement + PROMIS.ANX.T + PROMIS.DEP.T + ERS.full + UPPS.positiveurgency, data = df, family = "binomial")
m3.alc.ema.binomial.s <- glm(alc.quant.b ~ as.numeric(scale(DMQ.coping)) + 
                                           age + gender + white_nonwhite + 
                                           er.disengagement + 
                                           PROMIS.ANX.T + 
                                           PROMIS.DEP.T + 
                                           ERS.full.s + 
                                           UPPS.positiveurgency.s, data = df, family = "binomial")



exp(coef(summary(m3.alc.ema.binomial))[2])
exp(coef(summary(m3.alc.ema.binomial.s))[2])

# Non-significant UN-standardized effect of .177 log odds (1.19 odds)
# Non-significant Standardized effect of .167 (1.18)




AIC(m3.alc.ema.binomial, m2.alc.ema.binomial, m1.alc.ema.binomial) #m3 really explains data compared to other models



#Unstandardized graph

# Fitting model to data frame so we can graph.
NEWDATA <- data.frame(DMQ.coping = seq(1,5, length.out = nobs(m3.alc.ema.binomial)),
                      age = mean(df$age, na.rm = T), 
                      gender = "Male", 
                      white_nonwhite = mean(df$white_nonwhite, na.rm = T),
                      er.disengagement = mean(df$er.disengagement, na.rm = T),
                      PROMIS.ANX.T = mean(df$PROMIS.ANX.T, na.rm = T),
                      PROMIS.DEP.T = mean(df$PROMIS.DEP.T, na.rm = T),
                      ERS.full = mean(df$ERS.full, na.rm = T),
                      UPPS.positiveurgency = mean(df$UPPS.positiveurgency, na.rm = T)) #nobs function to get rightn number of observations
pred <- predict(m3.alc.ema.binomial, se.fit=T, newdata = NEWDATA, type = "link") 

# CI function
make_ci <- function(pred){
  
  # fit, lower, and upper CI
  fit <- pred$fit
  lower <- fit - 1.96*pred$se.fit
  upper <- fit + 1.96*pred$se.fit
  
  return(data.frame(exp(fit), exp(lower), exp(upper)))
}

binomial.alc.graph.3 <- make_ci(pred)

binomial.alc.graph.3 <- data.frame(fit = binomial.alc.graph.3$exp.fit., 
                                   upper = binomial.alc.graph.3$exp.upper., 
                                   lower = binomial.alc.graph.3$exp.lower.,
                                   DMQ.coping = seq(1,5, length.out = nobs(m3.alc.ema.binomial)))

binomial.alc.graph.3 <- df %>%
  filter(!is.na(DMQ.coping) & !is.na(alc.quant.b)) %>% 
  mutate(fit = alc.quant.b) %>% 
  ggplot(., aes(x = DMQ.coping, y = fit))+
  geom_point()+
  geom_jitter(alpha = .25)+
  geom_line(size = 1, data = binomial.alc.graph.3, aes(x = DMQ.coping, y = fit)) +
  geom_ribbon(data = binomial.alc.graph.3, aes(ymin = lower, ymax = upper), alpha= .2, fill = "gray4") +
  geom_vline(xintercept =  2.10, color = "red", size = 1) +
  geom_vline(xintercept = c(2.10-0.9786804, 2.10+0.9786804), color = "blue", size = 1) +  
  labs(caption = "Note: Red line represents mean, blue line represents \n 1 std from mean or boundary of x-axis. Min response on X-axis is 1") +
  ylab("Proportion of Use Days in Study") +
  ylim(0,1)+
  theme(legend.position = "none") +
  ggtitle("m3.alc.ema.binomial")

#NOTE:variance is too wide around 3.5-5 on the x-axis so ggplot stops the ribbons there, 
#Can set higher y boundaries to fix, but doesn't have clean 0,1 range that I want.



#Standardized graph

# Fitting model to data frame so we can graph.
NEWDATA <- data.frame(DMQ.coping = seq(-1.2,3.2, length.out = nobs(m3.alc.ema.binomial.s)),
                      age = mean(df$age, na.rm = T), 
                      gender = "Male", 
                      white_nonwhite = mean(df$white_nonwhite, na.rm = T),
                      er.disengagement = mean(df$er.disengagement, na.rm = T),
                      PROMIS.ANX.T = mean(df$PROMIS.ANX.T, na.rm = T),
                      PROMIS.DEP.T = mean(df$PROMIS.DEP.T, na.rm = T),
                      ERS.full.s = mean(df$ERS.full.s, na.rm = T),
                      UPPS.positiveurgency.s = mean(df$UPPS.positiveurgency.s, na.rm = T)) #nobs function to get rightn number of observations
pred <- predict(m3.alc.ema.binomial.s, se.fit=T, newdata = NEWDATA, type = "link") 

# CI function
make_ci <- function(pred){
  
  # fit, lower, and upper CI
  fit <- pred$fit
  lower <- fit - 1.96*pred$se.fit
  upper <- fit + 1.96*pred$se.fit
  
  return(data.frame(exp(fit), exp(lower), exp(upper)))
}

binomial.alc.graph.3.s <- make_ci(pred)

binomial.alc.graph.3.s <- data.frame(fit = binomial.alc.graph.3.s$exp.fit., 
                                     upper = binomial.alc.graph.3.s$exp.upper., 
                                     lower = binomial.alc.graph.3.s$exp.lower.,
                                     DMQ.coping = seq(-1.2,3.1, length.out = nobs(m3.alc.ema.binomial.s)))



binomial.alc.graph.3.s <- df %>%
  filter(!is.na(DMQ.coping) & !is.na(alc.quant.b)) %>% 
  mutate(fit = alc.quant.b,
         DMQ.coping = as.numeric(scale(DMQ.coping))) %>% 
  ggplot(., aes(x = DMQ.coping, y = fit))+
  geom_point()+
  geom_jitter(alpha = .25)+
  geom_line(size = 1, data = binomial.alc.graph.3.s, aes(x = DMQ.coping, y = fit)) +
  geom_ribbon(data = binomial.alc.graph.3.s, aes(ymin = lower, ymax = upper), alpha= .2, fill = "gray4") +
  ylab("Proportion of Use Days in Study") +
  ylim(0,1) +
  theme(legend.position = "none") +
  ggtitle("m3.alc.ema.binomial.s")









#3. Global Coping Motives on Global Mar  

m3.mar.base.nb <- glm.nb(DDTQ.typ.use.week ~ MMQ.coping + age + gender + white_nonwhite + CERQ.disengagement + PROMIS.ANX.T + PROMIS.DEP.T, data = df, control = glm.control(maxit = 1000)) 
m3.mar.base.nb.s <- glm.nb(DDTQ.typ.use.week ~ as.numeric(scale(MMQ.coping)) + 
                                               age + gender + white_nonwhite + 
                                               CERQ.disengagement + 
                                               PROMIS.ANX.T + 
                                               PROMIS.DEP.T, data = df, control = glm.control(maxit = 1000)) 



exp(coef(summary(m3.mar.base.nb))[2])
exp(coef(summary(m3.mar.base.nb.s))[2])


# Significant UN-standardized effect of .63 log odds (1.87 odds ratio) 
# Significant Standardized effect of .61 (1.84)


AIC(m3.mar.base.nb, m2.mar.base.nb, m1.mar.base.nb)  #m3 really explains data compared to other models



# Graphs
#first unstandardized

# Fitting model to data frame so we can graph.

NEWDATA <- data.frame(MMQ.coping = seq(1,5, length.out = nobs(m3.mar.base.nb)),
                      age = mean(df$age, na.rm = T), 
                      gender = "Male", 
                      white_nonwhite = mean(df$white_nonwhite, na.rm = T),
                      CERQ.disengagement = mean(df$CERQ.disengagement, na.rm = T),
                      PROMIS.ANX.T = mean(df$PROMIS.ANX.T),
                      PROMIS.DEP.T = mean(df$PROMIS.DEP.T)) #nobs function to get rightn number of observations
pred <- predict(m3.mar.base.nb, se.fit=T, newdata = NEWDATA, type = "response") 

# CI function
make_ci <- function(pred){
  
  # fit, lower, and upper CI
  fit <- pred$fit
  lower <- fit - 1.96*pred$se.fit
  upper <- fit + 1.96*pred$se.fit
  
  return(data.frame(fit, lower, upper))
}

nb.graph.3 <- make_ci(pred)

nb.graph.3 <- data.frame(fit = nb.graph.3$fit, 
                         upper = nb.graph.3$upper, 
                         lower = nb.graph.3$lower,
                         MMQ.coping = seq(1,5, length.out = nobs(m3.mar.base.nb)))

nb.graph.3 <- df %>%
  filter(!is.na(MMQ.coping)) %>% 
  mutate(fit = DDTQ.typ.use.week) %>% 
  ggplot(., aes(x = MMQ.coping, y = fit))+
  geom_point()+
  geom_jitter(alpha = .25)+
  geom_line(size = 1, data = nb.graph.3, aes(x = MMQ.coping, y = fit)) +
  ylim(0,7)+
  geom_ribbon(data = nb.graph.3, aes(ymin = lower, ymax = upper), alpha= .2, fill = "gray4") +
  geom_vline(xintercept =  1.71, color = "red", size = 1,) +
  geom_vline(xintercept = c(1, 1.71+0.9786804), color = "blue", size = 1) +  
  labs(caption = "Note: Red line represents mean, blue line represents \n 1 std from mean or boundary of x-axis. Min response on X-axis is 1") +
  ylab("Weekly Predicted Use Days") +
  theme(legend.position = "none") +
  ggtitle("nb.graph.3")





#Standardized

# Fitting model to data frame so we can graph.
NEWDATA <- data.frame(MMQ.coping = seq(-.73,3.36, length.out = nobs(m3.mar.base.nb.s)),
                      age = mean(df$age, na.rm = T), 
                      gender = "Male", 
                      white_nonwhite = mean(df$white_nonwhite, na.rm = T),
                      CERQ.disengagement = mean(df$CERQ.disengagement, na.rm = T),
                      PROMIS.ANX.T = mean(df$PROMIS.ANX.T),
                      PROMIS.DEP.T = mean(df$PROMIS.DEP.T)) #nobs function to get rightn number of observations
pred <- predict(m3.mar.base.nb.s, se.fit=T, newdata = NEWDATA, type = "response") 

# CI function
make_ci <- function(pred){
  
  # fit, lower, and upper CI
  fit <- pred$fit
  lower <- fit - 1.96*pred$se.fit
  upper <- fit + 1.96*pred$se.fit
  
  return(data.frame(fit, lower, upper))
}

nb.graph.3.s <- make_ci(pred)

nb.graph.3.s <- data.frame(fit = nb.graph.3.s$fit, 
                           upper = nb.graph.3.s$upper, 
                           lower = nb.graph.3.s$lower,
                           MMQ.coping = seq(-.73,3.36, length.out = nobs(m3.mar.base.nb.s)))


nb.graph.3.s <- df %>%
  filter(!is.na(MMQ.coping)) %>% 
  mutate(fit = DDTQ.typ.use.week,
         MMQ.coping = as.numeric(scale(MMQ.coping))) %>% 
  ggplot(., aes(x = MMQ.coping, y = fit))+
  geom_point()+
  geom_jitter(alpha = .25)+
  geom_line(size = 1, data = nb.graph.3.s, aes(x = MMQ.coping, y = fit)) +
  ylim(0,7)+
  geom_ribbon(data = nb.graph.3.s, aes(ymin = lower, ymax = upper), alpha= .2, fill = "gray4") +
  ylab("Weekly Predicted Use Days") +
  theme(legend.position = "none") +
  ggtitle("nb.graph.3.s")






#4. Global Coping Motives on Momentary Mar

m3.mar.ema.binomial <- glm(mar.quantjoint.b ~ MMQ.coping + age + gender + white_nonwhite + er.disengagement + PROMIS.ANX.T + PROMIS.DEP.T, data = df, family = "binomial") # find citations at some point for proportion logistic #Error: In eval(family$initialize) : non-integer #successes in a binomial glm! ## think it should be okay
m3.mar.ema.binomial.s <- glm(mar.quantjoint.b ~ as.numeric(scale(MMQ.coping)) + 
                                              age + gender + white_nonwhite + 
                                              er.disengagement + 
                                              PROMIS.ANX.T + 
                                              PROMIS.DEP.T, data = df, family = "binomial")



exp(coef(summary(m1.mar.ema.binomial))[2])
exp(coef(summary(m1.mar.ema.binomial.s))[2])

# Significant UN-standardized effect of .83 log odds (2.3 odds)
# Significant standardized effect of .82 log odds (2.26 odds)

AIC(m3.mar.ema.binomial, m2.mar.ema.binomial, m1.mar.ema.binomial) #m3 really explains data compared to other models


#Unstandardized graph

# Fitting model to data frame so we can graph.

NEWDATA <- data.frame(MMQ.coping = seq(1,5, length.out = nobs(m3.mar.ema.binomial)),
                      age = mean(df$age, na.rm = T), 
                      gender = "Male", 
                      white_nonwhite = mean(df$white_nonwhite, na.rm = T),
                      er.disengagement = mean(df$er.disengagement, na.rm = T),
                      PROMIS.ANX.T = mean(df$PROMIS.ANX.T),
                      PROMIS.DEP.T = mean(df$PROMIS.DEP.T)) #nobs function to get rightn number of observations
pred <- predict(m3.mar.ema.binomial, se.fit=T, newdata = NEWDATA, type = "link") 

# CI function
make_ci <- function(pred){
  
  # fit, lower, and upper CI
  fit <- pred$fit
  lower <- fit - 1.96*pred$se.fit
  upper <- fit + 1.96*pred$se.fit
  
  return(data.frame(exp(fit), exp(lower), exp(upper)))
}

binomial.mar.graph.3 <- make_ci(pred)

binomial.mar.graph.3 <- data.frame(fit = binomial.mar.graph.3$exp.fit., 
                                   upper = binomial.mar.graph.3$exp.upper., 
                                   lower = binomial.mar.graph.3$exp.lower.,
                                   MMQ.coping = seq(1,5, length.out = nobs(m3.mar.ema.binomial)))

binomial.mar.graph.3 <- df %>%
  filter(!is.na(MMQ.coping) & !is.na(mar.quantjoint.b)) %>% 
  mutate(fit = mar.quantjoint.b) %>% 
  ggplot(., aes(x = MMQ.coping, y = fit))+
  geom_point()+
  geom_jitter(alpha = .25)+
  geom_line(size = 1, data = binomial.mar.graph.3, aes(x = MMQ.coping, y = fit)) +
  geom_ribbon(data = binomial.mar.graph.3, aes(ymin = lower, ymax = upper), alpha= .2, fill = "gray4") +
  geom_vline(xintercept =  1.71, color = "red", size = 1) +
  geom_vline(xintercept = c(1, 1.71+0.9786804), color = "blue", size = 1) +  
  labs(caption = "Note: Red line represents mean, blue line represents \n 1 std from mean or boundary of x-axis. Min response on X-axis is 1") +
  ylab("Proportion of Use Days in Study") +
  ylim(0,1)+
  theme(legend.position = "none") +
  ggtitle("m3.mar.ema.binomial")

#NOTE:variance is too wide around 3.5-5 on the x-axis so ggplot stops the ribbons there, 
#Can set higher y boundaries to fix, but doesn't have clean 0,1 range that I want.



#Standardized graph

# Fitting model to data frame so we can graph.
NEWDATA <- data.frame(MMQ.coping = seq(-.73,3.4, length.out = nobs(m2.mar.ema.binomial.s)),
                      age = mean(df$age, na.rm = T), 
                      gender = "Male", 
                      white_nonwhite = mean(df$white_nonwhite, na.rm = T),
                      er.disengagement = mean(df$er.disengagement, na.rm = T),
                      PROMIS.ANX.T = mean(df$PROMIS.ANX.T),
                      PROMIS.DEP.T = mean(df$PROMIS.DEP.T)) #nobs function to get rightn number of observations
pred <- predict(m3.mar.ema.binomial.s, se.fit=T, newdata = NEWDATA, type = "link") 

# CI function
make_ci <- function(pred){
  
  # fit, lower, and upper CI
  fit <- pred$fit
  lower <- fit - 1.96*pred$se.fit
  upper <- fit + 1.96*pred$se.fit
  
  return(data.frame(exp(fit), exp(lower), exp(upper)))
}

binomial.mar.graph.3.s <- make_ci(pred)

binomial.mar.graph.3.s <- data.frame(fit = binomial.mar.graph.3.s$exp.fit., 
                                     upper = binomial.mar.graph.3.s$exp.upper., 
                                     lower = binomial.mar.graph.3.s$exp.lower.,
                                     MMQ.coping = seq(-.73,3.4, length.out = nobs(m3.mar.ema.binomial.s)))



binomial.mar.graph.3.s <- df %>%
  filter(!is.na(MMQ.coping) & !is.na(mar.quantjoint.b)) %>% 
  mutate(fit = mar.quantjoint.b,
         MMQ.coping = as.numeric(scale(MMQ.coping))) %>% 
  ggplot(., aes(x = MMQ.coping, y = fit))+
  geom_point()+
  geom_jitter(alpha = .25)+
  geom_line(size = 1, data = binomial.mar.graph.3.s, aes(x = MMQ.coping, y = fit)) +
  geom_ribbon(data = binomial.mar.graph.3.s, aes(ymin = lower, ymax = upper), alpha= .2, fill = "gray4") +
  ylab("Proportion of Use Days in Study") +
  ylim(0,1) +
  theme(legend.position = "none") +
  ggtitle("m3.mar.ema.binomial.s")





# Table for organizing info
    # Maybe two, one for alcohol and one for Marijuana


# Global/momentary, distribution/Model Type, covariates, Effect size, SE, P-value, AIC

fun_model_df <- function(x) {

    
  # MMQ: effect size
CM_effect_size <- round(summary(x)$coefficients[2],3)
  
  # SE
SE <- round(summary(x)$coefficients[,2][[2]],3)
  
  # P-value
P_val <- round(summary(x)$coefficients[,4][[2]],3)
  
  #AIC
AIC_val <- round(AIC(x),3)
  
  
  #BIC
BIC_val <- round(BIC(x),3)

  #model_family
model_family <- as.character(summary(x)$family[1],3)


  #Model name
model_name <- deparse(substitute(x))



model_df <- data.frame(model_name, CM_effect_size, SE, P_val, AIC_val, BIC_val, model_family)
  
return(model_df)

}


(alc_results_df <- fun_model_df(m0.alc.base.normal) %>% 
  rbind(fun_model_df(m1.alc.base.normal)) %>% 
  rbind(fun_model_df(m2.alc.base.normal)) %>% 
  rbind(fun_model_df(m3.alc.base.normal)) %>% 
  rbind(fun_model_df(m0.alc.ema.binomial)) %>% 
  rbind(fun_model_df(m1.alc.ema.binomial)) %>% 
  rbind(fun_model_df(m2.alc.ema.binomial)) %>% 
  rbind(fun_model_df(m3.alc.ema.binomial))) 



(mar_results_df <- fun_model_df(m0.mar.base.nb) %>% 
  rbind(fun_model_df(m1.mar.base.nb)) %>% 
  rbind(fun_model_df(m2.mar.base.nb)) %>% 
  rbind(fun_model_df(m3.mar.base.nb)) %>% 
  rbind(fun_model_df(m0.mar.ema.binomial)) %>% 
  rbind(fun_model_df(m1.mar.ema.binomial)) %>% 
  rbind(fun_model_df(m2.mar.ema.binomial)) %>% 
  rbind(fun_model_df(m3.mar.ema.binomial)))


(alc_s_results_df <- fun_model_df(m0.alc.base.normal.s) %>% 
    rbind(fun_model_df(m1.alc.base.normal.s)) %>% 
    rbind(fun_model_df(m2.alc.base.normal.s)) %>% 
    rbind(fun_model_df(m3.alc.base.normal.s)) %>% 
    rbind(fun_model_df(m0.alc.ema.binomial.s)) %>% 
    rbind(fun_model_df(m1.alc.ema.binomial.s)) %>% 
    rbind(fun_model_df(m2.alc.ema.binomial.s)) %>% 
    rbind(fun_model_df(m3.alc.ema.binomial.s)))

(mar_s_results_df <- fun_model_df(m0.mar.base.nb.s) %>% 
    rbind(fun_model_df(m1.mar.base.nb.s)) %>% 
    rbind(fun_model_df(m2.mar.base.nb.s)) %>% 
    rbind(fun_model_df(m3.mar.base.nb.s)) %>% 
    rbind(fun_model_df(m0.mar.ema.binomial.s)) %>% 
    rbind(fun_model_df(m1.mar.ema.binomial.s)) %>% 
    rbind(fun_model_df(m2.mar.ema.binomial.s)) %>% 
    rbind(fun_model_df(m3.mar.ema.binomial.s)))




#Normal plots
normal.graph.0.s
normal.graph.1.s
normal.graph.2.s
normal.graph.3.s


#taking a look at the nb plots
nb.graph.0.s
nb.graph.1.s
nb.graph.2.s
nb.graph.3.s



#logistic regression on proportion of use days (Alc, EMA)
binomial.alc.graph.0.s
binomial.alc.graph.1.s
binomial.alc.graph.2.s
binomial.alc.graph.3.s



#logistic regression on proportion of use days (Mar, EMA)
binomial.mar.graph.0.s
binomial.mar.graph.1.s
binomial.mar.graph.2.s
binomial.mar.graph.3.s



df_global_alc <- data.frame(x = c("Step 1", "Step 2", "Step 3", "Step 4"),
                            estimate = c(0.251, 0.308, 0.321, 0.300),
                            se = c(.072, .073, .076, .08)) %>% 
                        mutate(xmin. = estimate - 2*se,
                        xmax. = estimate + 2*se)


df_ema_alc <- data.frame(x = c("Step 1", "Step 2", "Step 3", "Step 4"),
                            estimate = c(.093, 0.157, 0.170, 0.144),
                            se = c(.126, .132, .138, .157)) %>% 
                      mutate(xmin. = estimate - 2*se,
                      xmax. = estimate + 2*se)


df_global_mar <- data.frame(x = c("Step 1", "Step 2", "Step 3", "Step 4"),
                         estimate = c(.526, 0.566, 0.571, 0.565),
                         se = c(.063, .066, .068, .073)) %>% 
                      mutate(xmin. = estimate - 2*se,
                      xmax. = estimate + 2*se)



df_ema_mar <- data.frame(x = c("Step 1", "Step 2", "Step 3", "Step 4"),
                         estimate = c(.741, 0.803, 0.830, 0.863),
                         se = c(.126, .132, .138, .157)) %>% 
  mutate(xmin. = estimate - 2*se,
         xmax. = estimate + 2*se)



df_global_alc %>% 
  mutate(sig = if_else(xmin. < 0, 0, 1),
         order = 5-as.numeric(str_sub(x, 6,-1))) %>% 
  ggplot(aes(y = reorder(x,order), x = estimate)) +
  geom_vline(xintercept = 0, size = 1.5, color = "gray8", alpha = .25) +
  geom_linerange(aes(xmin = xmin., xmax = xmax.), size = 1.5, color = "orange2", alpha = .6)+
  geom_point(color = "orange4", size = 2.5) +
  xlim(-.2,.5) +
  ylab("model") +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank()) 


  
df_ema_alc %>% 
  mutate(sig = if_else(xmin. < 0, 0, 1),
         order = 5-as.numeric(str_sub(x, 6,-1))) %>% 
  ggplot(aes(y = reorder(x,order), x = estimate)) +
  geom_vline(xintercept = 0, size = 1.5, color = "gray8", alpha = .25) +
  geom_linerange(aes(xmin = xmin., xmax = xmax.), size = 1.5, color = "orange2", alpha = .6)+
  geom_point(color = "orange4", size = 2.5) +
  ylab("model") +
  xlim(-.2,.5) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank())



df_global_mar %>% 
  mutate(sig = if_else(xmin. < 0, 0, 1),
         order = 5-as.numeric(str_sub(x, 6,-1))) %>% 
  ggplot(aes(y = reorder(x,order), x = estimate)) +
  geom_vline(xintercept = 0, size = 1.5, color = "gray8", alpha = .25) +
  geom_linerange(aes(xmin = xmin., xmax = xmax.), size = 1.5, color = "green2", alpha = .6)+
  geom_point(color = "green4", size = 2.5) +
  ylab("model") +
  xlim(0,1.2) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank()) 


df_ema_mar %>% 
  mutate(sig = if_else(xmin. < 0, 0, 1),
         order = 5-as.numeric(str_sub(x, 6,-1))) %>% 
  ggplot(aes(y = reorder(x,order), x = estimate)) +
  geom_vline(xintercept = 0, size = 1.5, color = "gray8", alpha = .25) +
  geom_linerange(aes(xmin = xmin., xmax = xmax.), size = 1.5, color = "green2", alpha = .6)+
  geom_point(color = "green4", size = 2.5) +
  ylab("model") +
  xlim(0,1.2) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank()) 


