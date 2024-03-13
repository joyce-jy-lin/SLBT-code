---
  title: "Creating merged dataset for SLBT Tooth Analyses -- with QC"
author: "Joyce adapted from Ruby Hickman"
date: "2024-01-05"
output: html_document
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

Load required packages 
```{r, results="hide"}
library(data.table)
library(tidyverse)
library(compareGroups)
library(ggplot2)
library(kableExtra)
library(gtools)
library(Hmisc)
```

## Read in data 
```{r}
## updated as of 10/4/23 with new prenatal and postnatal averages added in after sonya's work over the summer to identify neonatal line locations 
## updated 1/5/24 to create separate dataset for the QCed data 
## QCed AVERAGES 
tooth <- fread("/Volumes/shared/EOME/Weisskopf/Saint_Louis_Baby_Teeth_Study/Individuals/Joyce/Tooth data/tooth_15IQRclean_75.csv")
#tooth <- fread("/Volumes/shared/EOME/Weisskopf/Saint_Louis_Baby_Teeth_Study/Individuals/Joyce/Tooth data/tooth15IQR_verifieddnt.csv")
#tooth <- fread("/Volumes/shared/EOME/Weisskopf/Saint_Louis_Baby_Teeth_Study/Individuals/Joyce/Tooth data/tooth_nl.csv")
surv <- read_csv("/Volumes/shared/EOME/Weisskopf/Saint_Louis_Baby_Teeth_Study/Individuals/Ruby/datasets_creation_qc/slbt_survey_20230616.csv")
tmb <- read_csv("/Volumes/shared/EOME/Weisskopf/Saint_Louis_Baby_Teeth_Study/Data 2020/TestMyBrain/output/tmb_tidy_current.csv")
glimpse(tmb)
rphp <- read_csv("/Volumes/shared/EOME/Weisskopf//Saint_Louis_Baby_Teeth_Study/Data 2020/Teeth Files from RPHP/Teeth Cards with Tooth IDs/teeth_cards_data_subset/tooth_data_sex.dob.breastfed.toothtype.csv")

names(rphp)[names(rphp) == 'Tooth Type'] <- 'Tooth_Type'
names(rphp)[names(rphp) == 'Year Lost'] <- 'Year_Lost'
describe(rphp$tooth_type_clean)

rphp_df<-rphp%>%drop_na(study_id)
describe(rphp_df$tooth_type_clean)
```

## Do a little data cleaning 
```{r}
# summary(tooth$pb208_ppm_mean)
length(unique(tooth$study_id))
length(unique(tooth$Pbmean))
## joyce used different naming conventions 
summary(tooth$Pbmean)
summary(is.na(tooth$Pbmean))
tooth_old <- tooth

## take the values we care about (means) and drop enamel measurements 
tooth<-subset(tooth, group_name== "EDJ")
length(unique(tooth$Pbmean))

tmb_core <-
  tmb %>% select(
    'study_id',
    'DSM_score',
    'GCPT_score_outcomes_dprime',
    'MOT_score_outcomes_correct',
    'VerPAT_score_outcomes_accuracy',
    'VisPAT_score_outcomes_accuracy',
    'VocP_score_outcomes_accuracy'
  )
tmb_core <- unique(tmb_core)
```

## Merge datasets 
```{r}
i1 <- tooth %>% left_join(surv, by = "study_id")
i2 <- i1 %>% left_join(tmb_core, by = "study_id") %>% 
  filter(!is.na(study_id))
dat <-
  i2 %>% left_join(rphp, by = "study_id") 

#now since all spot values should be gone, can just use unique to select one row per person (all should be the same for each person )
length(unique(dat$study_id))
dat_ <- unique(dat$study_id)


# Check! 
glimpse(dat)
summary(dat$Pbmean)
summary(i2$Pbmean)
sum(is.na(dat$Pbmean))

# ### ok idk what is going on here so we gonna drop
# dat[is.na(dat$study_id),]
# tooth[is.na(tooth$study_id),]
# dat <- dat %>% filter(!is.na(study_id))
```


## Create cognition scores 
```{r}
## Check the cognition scores for missingness 
## digit-symbol matching
table(dat$DSM_score, useNA="always") ##all good after fixing the study_id problem

## Who is missing? 
dat[is.na(dat$DSM_score), c("study_id")]

## TEMP: remove missing people 
# dat <- dat %>% filter(!is.na(DSM_score))

## create z-scores 
dat <- dat %>%
  mutate(
    DSM_score_z = (DSM_score - mean(DSM_score, na.rm = T)) / sd(DSM_score, na.rm =
                                                                  T),
    GCPT_score_outcomes_dprime_z = (
      GCPT_score_outcomes_dprime - mean(GCPT_score_outcomes_dprime, na.rm = T)
    ) / sd(GCPT_score_outcomes_dprime, na.rm = T),
    
    MOT_score_outcomes_correct_z = (
      MOT_score_outcomes_correct - mean(MOT_score_outcomes_correct, na.rm = T)
    ) / sd(MOT_score_outcomes_correct, na.rm = T),
    
    VerPAT_score_outcomes_accuracy_z = (
      VerPAT_score_outcomes_accuracy - mean(VerPAT_score_outcomes_accuracy, na.rm =
                                              T)
    ) / sd(VerPAT_score_outcomes_accuracy, na.rm = T),
    
    VisPAT_score_outcomes_accuracy_z = (
      VisPAT_score_outcomes_accuracy - mean(VisPAT_score_outcomes_accuracy, na.rm =
                                              T)
    ) / sd(VisPAT_score_outcomes_accuracy, na.rm = T),
    
    VocP_score_outcomes_accuracy_z = (
      VocP_score_outcomes_accuracy - mean(VocP_score_outcomes_accuracy, na.rm =
                                            T)
    ) / sd(VocP_score_outcomes_accuracy, na.rm = T)
  )

### DSM_score_z
ggplot(dat, aes(x=DSM_score_z)) + 
  geom_histogram(color="white", fill="blue")

### GCPT_score_outcomes_dprime_z
ggplot(dat, aes(x=GCPT_score_outcomes_dprime_z)) + 
  geom_histogram(color="white", fill="blue")

### MOT_score_outcome_correct_z
ggplot(dat, aes(x=MOT_score_outcomes_correct_z)) + 
  geom_histogram(color="white", fill="blue") 

### VisPAT_score_outcomes_accuracy_z
ggplot(dat, aes(x=VisPAT_score_outcomes_accuracy_z)) + 
  geom_histogram(color="white", fill="blue") 


### VocP_score_outcomes_accuracy_z
ggplot(dat, aes(x=VocP_score_outcomes_accuracy_z)) + 
  geom_histogram(color="white", fill="blue")


### VerPAT_score_outcomes_accuracy_z
ggplot(dat, aes(x=VerPAT_score_outcomes_accuracy_z)) + 
  geom_histogram(color="white", fill="blue")


## averaged overall z-score 
dat$TMB_composite_Z <- (dat$DSM_score_z + 
                          dat$GCPT_score_outcomes_dprime_z + 
                          dat$MOT_score_outcomes_correct_z + 
                          dat$VisPAT_score_outcomes_accuracy_z + 
                          dat$VocP_score_outcomes_accuracy_z + 
                          dat$VerPAT_score_outcomes_accuracy_z)/6

summary(dat$TMB_composite_Z)
sum(is.na(dat$TMB_composite_Z))

```

### Hannah code for age things: 

**Create age at cognitive test variable**
  Convert the date of taking survey/cog test into useable date. 
```{r}
#make ID.date into date format 
dat$survey_date <- as.Date(dat$ID.date, "%m/%d/%Y")
#print(dat$survey_date)

# dat$survey_mony <- format(as.Date(dat$survey_date), "%Y-%m")
# print(dat$survey_mony)
``` 

Birth Year and Month
```{r}
#make birth month and year into one variable
#first month to just numbers from SLBT survey data
dat$birth_month2 <- ifelse(dat$bth_mon=="1: Jan", "01",
                           ifelse(dat$bth_mon=="2: Feb", "02",
                                  ifelse(dat$bth_mon=="3: Mar", "03",
                                         ifelse(dat$bth_mon=="4: Apr", "04",
                                                ifelse(dat$bth_mon=="5: May", "05",
                                                       ifelse(dat$bth_mon=="6: Jun", "06",
                                                              ifelse(dat$bth_mon=="7: Jul", "07",
                                                                     ifelse(dat$bth_mon=="8: Aug", "08",
                                                                            ifelse(dat$bth_mon=="9: Sep", "09",
                                                                                   ifelse(dat$bth_mon=="10: Oct", "10",
                                                                                          ifelse(dat$bth_mon=="11: Nov", "11",
                                                                                                 ifelse(dat$bth_mon=="12: Dec", "12", NA))))))))))))

#check changes to birth month 
#dat$birth_month2 #all numbers


```


Create an age variable with the SNAP survey variables only 
```{r}
#Birthdate from SNAP survey
dat$birth_day_clean <- ifelse(is.na(dat$birth_day_clean), 1, dat$birth_day_clean)
summary(dat$birth_day_clean)

dat$birth_date_snap <- paste(dat$birth_yr, dat$birth_month_clean, dat$birth_day_clean, sep="-")
head(dat$birth_date_snap)

table(dat$birth_date_snap)
#dat$birth_date_snap

dat$birth_date_snap1 <- as.Date(dat$birth_date_snap, "%Y-%m-%d") 
#dat$birth_date_snap1

dat$age <- trunc(as.numeric(difftime(dat$survey_date, dat$birth_date_snap1, units = "days")) / 365.25)
summary(dat$age) # one missing - need to figure out why - because missing a birth day - is it appropriate to enter 1 as their day so that this will run and give us an approx age 
```
dat_<-dat[!duplicated(dat$study_id), ]
describe(dat_$age)

## Save/load merged dataset 
```{r}
write_csv(dat, "/Volumes/shared/EOME/Weisskopf/Saint_Louis_Baby_Teeth_Study/Individuals/Joyce/Tooth data/merged_data15IQR_ENL.csv")

# dat <- read_csv("/Volumes/Saint_Louis_Baby_Teeth_Study/Individuals/Ruby/datasets_creation_qc/merged_slbt_20231004.csv")
```
# add census variables for merge
cen <- read_csv("/Volumes/shared/EOME/Weisskopf/Saint_Louis_Baby_Teeth_Study/Individuals/Joyce/SLBT/ISEE24/censusvars.csv")
cendat <- dat %>% left_join(cen, by = "study_id")
write_csv(cendat, "/Volumes/shared/EOME/Weisskopf/Saint_Louis_Baby_Teeth_Study/Individuals/Joyce/SLBT/ISEE24/merged_data15IQR_cen.csv")


