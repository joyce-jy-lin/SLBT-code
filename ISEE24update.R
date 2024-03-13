## ISEE 24 updated 2/26/24
# Deciduous tooth Pb and BMI
library(data.table)
library(tidyverse)
library(Hmisc)
library(car)
library(patchwork)

#snp=read.csv("S:\\Saint_Louis_Baby_Teeth_Study\\Individuals\\Ian\\Teeth_Metals\\full_tmb_geocoded_data_IT.csv", header=T,sep=",",as.is=T)

# Merge with survey data/census -----------------------------------------------------------------
tooth<- read_csv('/Volumes/shared/EOME/Weisskopf/Saint_Louis_Baby_Teeth_Study/Individuals/Joyce/Tooth data/tooth15IQR_verifieddnt.csv') #this file created from "RawDNT_verif.R" using batch merged raw tooth data and NL_HSRP_record.csv for tooth notes about dnt past enamel and NL
length(unique(tooth$tooth_id))
length(unique(tooth$study_id.x))
surv <- read_csv("/Volumes/shared/EOME/Weisskopf/Saint_Louis_Baby_Teeth_Study/Individuals/Ruby/datasets_creation_qc/slbt_survey_20230616.csv")
tmb <- read_csv("/Volumes/shared/EOME/Weisskopf/Saint_Louis_Baby_Teeth_Study/Data 2020/TestMyBrain/output/tmb_tidy_current.csv")
rphp <- read_csv("/Volumes/shared/EOME/Weisskopf//Saint_Louis_Baby_Teeth_Study/Data 2020/Teeth Files from RPHP/Teeth Cards with Tooth IDs/teeth_cards_data_subset/tooth_data_sex.dob.breastfed.toothtype.csv")
cen <- read_csv("/Volumes/shared/EOME/Weisskopf/Saint_Louis_Baby_Teeth_Study/Individuals/Joyce/SLBT/ISEE24/censusvars.csv")

names(rphp)[names(rphp) == 'Tooth Type'] <- 'Tooth_Type'
names(rphp)[names(rphp) == 'Year Lost'] <- 'Year_Lost'
describe(rphp$tooth_type_clean)
tooth_old <- tooth

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

#create new columns for Pb observations with dnt verified spots and 15 IQR outliers removed
tooth<- tooth %>% mutate(pb=pb208_ppm)
tooth<-tooth %>% mutate_at(vars(c("pb")), funs(replace(., Pbout == 1, NA)))
describe(tooth$pb208_ppm)
describe(tooth$pb)
tooth<-tooth %>% mutate_at(vars(c("pb")), funs(replace(., DNTpastENL == 1, NA)))
describe(tooth$pb)


## Merge datasets 
tooth <- tooth %>% rename(study_id = study_id.x)
i1 <- tooth %>% left_join(surv, by = "study_id")
i2 <- i1 %>% left_join(tmb_core, by = "study_id") %>% 
  filter(!is.na(study_id))
dat <-
  i2 %>% left_join(rphp, by = "study_id") 
dat <- dat %>% left_join(cen, by = "study_id")

## create prenatal and postnatal variables 
dat_nl <- dat %>% 
  mutate(
    prenatal = case_when(
      # study_id %in% c(100695, 131485, 156650, 192630) ~ NA_integer_, ## for now excluding the worst offenders for bad DEF (having < -400 days), potentially ok for pre and postnatal if its just the tooth misidentified
      nl_usability == 1 & spot <= nl_start_from_cusp_dent ~ 1, # if the NL was IDed well and spot number is before the NL counting from the Cusp --> Yes Prenatal 
      # adding on 2 spots further from the NL by Sonya due to Felicitas saying NL is farther down # In joyce's new data, she accounted for this in raw data so no need to add in code
      nl_usability == 1 & spot >= nl_start_from_cusp_dent ~ 0, # if the NL was IDed well and spot number is AFTER the NL counting from the Cusp --> Yes Prenatal 
      def > 0 ~ 0, # positive DEF --> NOT prenatal
      def < 0 ~ 1, # negative DEF --> prenatal 
      TRUE ~ NA_integer_
    ),
    postnatal = 1 - prenatal
  )

dat_nl %>% 
  filter(study_id %in% c(100695, 131485, 156650, 192630)) %>% 
  {table(.$prenatal, useNA = "always")}
table(dat_nl$prenatal, useNA = "always") 
table(dat_nl$postnatal, useNA = "always") ## these should be opposites 

dat_nl <- dat_nl %>% 
  mutate(newpp = case_when(
    prenatal == 1 ~ "prenatal",
    prenatal == 0 ~ "postnatal"))

## checking things went as expected 
describe(dat_nl$newpp)

#Create tooth averages ----------
# temp_avg <- dat_nl %>% group_by(study_id) %>% 
#   summarise(across(
#     .cols = ends_with("_ppm"),
#     .fns = list(mean = mean), na.rm = TRUE,
#     .names = "{col}_{fn}"))

temp_avg <- dat_nl %>% group_by(study_id) %>% 
  summarise(across(
    .cols = ends_with("pb"),
    .fns = list(mean = mean), na.rm = TRUE,
    .names = "{col}_{fn}"))

# Postnatal ## 
# Restrict dataset to postnatal spots
postnat <- dat_nl %>% filter(postnatal == 1)
length(unique(postnat$study_id))

# Calculate averages 
# temp_avg_post <- postnat %>% group_by(study_id) %>% 
#   summarise(across(
#     .cols = ends_with("_ppm"),
#     .fns = list(mean = mean), na.rm = TRUE,
#     .names = "{col}_{fn}_post"))

temp_avg_post <- postnat %>% group_by(study_id) %>% 
  summarise(across(
    .cols = ends_with("pb"),
    .fns = list(mean = mean), na.rm = TRUE,
    .names = "{col}_{fn}_post"))

temp_avg_post

## Prenatal ## 
prenat <- dat_nl %>% filter(prenatal == 1)
length(unique(prenat$study_id))
# temp_avg_pre <- prenat %>% group_by(study_id) %>% 
#   summarise(across(
#     .cols = ends_with("_ppm"),
#     .fns = list(mean = mean), na.rm = TRUE,
#     .names = "{col}_{fn}_pre"))

temp_avg_pre <- prenat %>% group_by(study_id) %>% 
  summarise(across(
    .cols = ends_with("pb"),
    .fns = list(mean = mean), na.rm = TRUE,
    .names = "{col}_{fn}_pre"))
temp_avg_pre

temp_merge <- temp_avg %>% left_join(temp_avg_pre, by="study_id")
averages <- temp_merge %>% left_join(temp_avg_post, by="study_id")

# summary(averages$pb208_ppm_mean_pre)
# sum(!is.na(averages$pb208_ppm_mean_pre))
# summary(averages$pb208_ppm_mean_post)
# sum(!is.na(averages$pb208_ppm_mean_post))

summary(averages$pb_mean_pre)
sum(!is.na(averages$pb_mean_pre))
summary(averages$pb_mean_post)
sum(!is.na(averages$pb_mean_post))

df <- dat_nl %>% left_join(averages, by="study_id")
length(unique(df$study_id))
df_ <- df[!duplicated(df$study_id),]

## Create cognition scores ----------------------------------
## create z-scores 
dat <- df %>%
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
## averaged overall z-score 
dat$TMB_composite_Z <- (dat$DSM_score_z + 
                          dat$GCPT_score_outcomes_dprime_z + 
                          dat$MOT_score_outcomes_correct_z + 
                          dat$VisPAT_score_outcomes_accuracy_z + 
                          dat$VocP_score_outcomes_accuracy_z + 
                          dat$VerPAT_score_outcomes_accuracy_z)/6

summary(dat$TMB_composite_Z)
sum(is.na(dat$TMB_composite_Z))

### Hannah code for age things--------------------------
#Create age at cognitive test variable**
#Convert the date of taking survey/cog test into useable date. 
#make ID.date into date format 
dat$survey_date <- as.Date(dat$ID.date, "%m/%d/%Y")
#print(dat$survey_date)

# dat$survey_mony <- format(as.Date(dat$survey_date), "%Y-%m")
# print(dat$survey_mony)

#Birth Year and Month
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

#Create an age variable with the SNAP survey variables only 
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

## two level variable for dad education 
table(dat$dad_edu, useNA = "always")
dat <- dat %>% 
  mutate(dad_edu_two = factor(
    case_when(
      dad_edu == "1: Middle school or less" |
        dad_edu == "2: Some high school"  |
        dad_edu == "3: High school graduate" |
        dad_edu == "4: Associate degree" 
      ~ "Less than college",
      dad_edu == "5: Some college" |
        dad_edu == "6: College graduate" | 
        dad_edu == "7: More than college" 
      ~ "College or more"
    ),
    levels = c("Less than college", "College or more")
  ))
table(dat$dad_edu_two, useNA = "always")

## two level var for mom education
table(dat$mom_edu, useNA = "always")
dat <- dat %>% 
  mutate(mom_edu_two = factor(
    case_when(
      mom_edu == "1: Middle school or less" |
        mom_edu == "2: Some high school"  |
        mom_edu == "3: High school graduate" |
        mom_edu == "4: Associate degree" 
      ~ "Less than college",
      mom_edu == "5: Some college" |
        mom_edu == "6: College graduate" | 
        mom_edu == "7: More than college" 
      ~ "College or more"
    ),
    levels = c("Less than college", "College or more")
  ))
table(dat$mom_edu_two, useNA = "always")

## parental education: college degree 
table(dat$dad_edu_two, dat$mom_edu_two)
dat <- dat %>%
  mutate(parent_edu = factor(
    case_when(
      dat$mom_edu_two == "Less than college" &
        dat$dad_edu_two == "Less than college" ~ "No degree",
      dat$mom_edu_two == "Less than college" &
        dat$dad_edu_two == "College or more" ~ "One degree",
      dat$dad_edu_two == "Less than college" &
        dat$mom_edu_two == "College or more" ~ "One degree",
      dat$mom_edu_two == "College or more" &
        dat$dad_edu_two == "College or more" ~ "Both degree",
    ),
    levels = c("No degree", "One degree", "Both degree")
  ))
table(dat$parent_edu)

# collapse birthweight into 3 cats, low, normal, high
table(dat$birth_lb)
dat <- dat %>% 
  mutate(bweight = factor(
    case_when(
      birth_lb == "Extremely Low" |
        birth_lb == "Very Low"  |
        birth_lb == "Low" 
      ~ "Low",
      birth_lb == "Normal" ~ "Normal",
      birth_lb == "High" ~ "High"
    ),
    levels = c("Low", "Normal", "High")
  ))
table(dat$bweight)

# collapse econ12 into 3 cats, low, normal, high
table(dat$econ_12)
dat <- dat %>% 
  mutate(econ12_ = factor(
    case_when(
      econ_12 == "Very poor" |
        econ_12 == "Poor" 
      ~ "Poor",
      econ_12 == "Very well off"|
        econ_12=="Well-off" ~ "Well off",
      econ_12 == "Average" ~ "Average"
    ),
    levels = c("Average", "Poor", "Well off")
  ))
table(dat$econ12_)

### Race/ethnicity variables------------------------------
table(dat$race_wh_sp, useNA = "always") ## did they specify White 
table(dat$race_bl_sp, useNA = "always") ## did they specify Black 
table(dat$race_as_sp, useNA = "always") ## did they specify Asian 
table(dat$race_ai_sp, useNA = "always") ## did they specify American Indian/Alaska Native
table(dat$race_pi_sp, useNA = "always") ## did they specify Native Hawaiian/Pacific Islander
table(dat$race_his, useNA = "always") ## do they ID as Hispanic 
## some crosstabs 
table(dat$race_wh_sp, dat$race_bl_sp, useNA = "always")
table(dat$race_wh_sp, dat$race_as_sp, useNA = "always") ## some overlap here 
table(dat$race_wh_sp, dat$race_ai_sp, useNA = "always") ## overlap here
table(dat$race_wh_sp, dat$race_his, useNA = "always") ## and overlap here 

## White non-hispanic vs. all other 
dat <- dat %>%
  mutate(race_white = ifelse(
    race_wh_sp == "1: White" & ## if they ID as white
      race_his == 0 & ## and do not ID as hispanic 
      is.na(race_as_sp) & 
      is.na(race_ai_sp) &
      is.na(race_bl_sp), 
    "Non-Hispanic White", 
    "Other"
  ))
table(dat$race_white, useNA = "always")

## weights ----------------------
table(dat$batch)

## cutoff date for batches 1-3 == 9/15/21 23:59:59
cutoff_date <- as.POSIXct("9/15/21 23:59:59", format="%m/%d/%y %H:%M:%S")

# calc IPW based on batch and survey 1 start date
dat <- dat %>%
  mutate(
    # create surv start date
    survdate = as.POSIXct(paste(ID.date, ID.start), format = "%m/%d/%Y %H:%M:%S"),
    # calc IPW
    ipw = case_when(
      ## for those in batch 1-3 (simple random) all get the same weight
      batch <= 3 ~ 1 / (250 / 1055), 
      ## for those in batch 4 who were also eligible for batch 1 sampling 
      batch == 4 &
        survdate < cutoff_date ~ 1 / ((250 / 1055) + (150 / 339) * (1 - (250 /
                                                                           1055))),
      ## for those in batch 4 who were NOT eligible for batch 1 sampling 
      batch == 4 & survdate > cutoff_date ~ 1 / (150 / 339),
      TRUE ~ NA_integer_
    )
  ) # %>% select(batch, ipw, survdate, ID.date, ID.start) # if you want to view just vars in question

sum(dat$batch <= 3)
count(dat, ipw)
count(dat, batch)

dat_<-dat[!duplicated(dat$study_id), ]

#Exploratory vis ---------------
# mean Pb pre vs post natal
#dattt <- dat %>% group_by(study_id) %>% mutate(Pb_pp = case_when(
#newpp =="prenatal" ~ (mean(pb208_ppm[newpp=="prenatal"], na.rm=TRUE)),
#newpp =="postnatal"~ (mean(pb208_ppm[newpp=="postnatal"], na.rm=TRUE))))

#make new dataset in long format for visualization
datlong<-dat_ %>% pivot_longer(cols=c('pb_mean', 'pb_mean_pre', "pb_mean_post"),
                    names_to='pbtype',
                    values_to='concentration') 

datlong$batch<-as.factor(datlong$batch)

plot1<- ggplot(datlong, aes(concentration, fill = pbtype)) + 
  geom_histogram(alpha = 0.5, aes(y = ..count..), position = 'identity')
plot1


# dat_$batch<-as.factor(dat_$batch)
# Pbwhole<-dat_ %>% ggplot(aes(x = pb_mean))+#, fill=batch)) + 
#   geom_histogram()+
#   theme_bw()+
#   labs(x = "Tooth average Pb Concentration (ppm)")
# Pbwhole
# 
# Pbprenat<-dat_ %>% ggplot(aes(x = pb_mean_pre)) + 
#   geom_histogram()+
#   theme_bw()+
#   labs(x = "Prenatalaverage Pb Concentration (ppm)")
# Pbprenat
# 
# Pbpostnat<-dat_ %>% ggplot(aes(x = pb_mean_post)) + 
#   geom_histogram()+
#   theme_bw()+
#   labs(x = "Postnatal average Pb Concentration (ppm)")
# 
# Pbwhole + Pbprenat + Pbpostnat + plot_layout(ncol=3)

Pb<-dat_ %>% ggplot(aes(y = pb_mean)) + 
  geom_boxplot()+
  theme_bw()+ylim(0,13)+
  labs(y = "Whole tooth (ppm)")

Pbpre<-dat_ %>% ggplot(aes(y = pb_mean_pre)) + 
  geom_boxplot()+
  theme_bw()+ylim(0,13)+
  labs(y = "Prenatal (ppm)")

Pbpost<-dat_ %>% ggplot(aes(y = pb_mean_post)) + 
  geom_boxplot()+
  theme_bw()+ylim(0,13)+
  labs(y = "Postnatal (ppm)")

library(patchwork)
Pb + Pbpre + Pbpost + plot_layout(ncol=3)

#vis distribution of BMI
hist(dat_$bmi)# 1 person with BMI 60
i1 <- which.max(dat_$bmi)
dat_$bmi[i1]
dat_[i1, c("study_id", "bmi","height_in","weight_lb", "sex", "age")]

dat_$batch<-as.factor(dat_$batch)
describe(dat_$batch)

describe(dat_$hbp_dx)
## examine associations between tooth Pb and BMI ----------------------
#check covariates
describe(dat_$age) # 3 missing
describe(dat_$race_white) #12 missing
describe(dat_$sex) #1 missing
describe(dat_$parent_edu) #13 missing
describe(dat_$econ12_) #1 missing
describe(dat_$bweight) #20 missing

#create missing indicators
dat_mi <- dat_ %>%
  mutate(race_white_mi = factor(
    if_else(is.na(race_white), "Missing", race_white),
    levels = c("Non-Hispanic White", "Other", "Missing")),
    parent_edu_mi = factor(
      if_else(is.na(parent_edu), "Missing", parent_edu),
      levels = c("No degree", "One degree", "Both degree", "Missing")),
    bweight_mi = factor(
      if_else(is.na(bweight), "Missing", bweight),
      levels = c("Low", "Normal","High","Missing")
    )
  )


# BMI categories instead of continuous
describe(dat_$bmi)
#all cats
dat_<- dat_ %>% mutate(bmi_cat = case_when(bmi < 18.5 ~ 'underweight',
                                           bmi >= 18.5 & bmi<25 ~ 'healthy',
                                           bmi >=25 & bmi <30 ~ 'overweight',
                                           bmi >=30 ~ 'obese'))
describe(dat_$bmi_cat)

#collapsed cats
dat_<- dat_ %>% mutate(bmi_cat = case_when(bmi < 25 ~ 'normal',
                                           bmi >=25 ~ 'overweight'))

describe(dat_$bmi_cat)
dat_<- dat_ %>% mutate(bmi_cat = case_when(bmi < 25 ~ 0,
                                           bmi >=25 ~ 1))

class(dat_$bmi_cat)


# gam
library(mgcv)
mod_ps <-
  gam(
    bmi ~ s(pb_mean, fx = FALSE, bs = "cr") + age + sex + race_white + parent_edu + econ_12,
    data = dat_,
    weights = ipw
  )
plot(mod_ps)
summary(mod_ps)

## 1 Knot (2)
mod_k2 <-
  gam(
    bmi ~ s(pb_mean, fx = T, k = 2, bs = "cr") + age + sex + race_white + parent_edu + econ_12,
    data = dat_,
    weights = ipw
  )
plot(mod_k2)
summary(mod_k2)


# Linear regressions whole tooth Pb and BMI ----------------------------------------------------
#scale Pbmean_dent by quartile
hist(dat_$pb_mean)
dat_<-dat_%>% mutate(pb_mean_iqr = pb_mean/IQR(pb_mean))
hist(dat_$pb_mean_iqr)
hist(dat_$bmi)

model1<- glm(bmi~(pb_mean_iqr)+age+race_white + sex +  econ12_+ parent_edu, data = dat_, weights = ipw)
summary(model1)#aic 2933.8
confint(model1)
plot(model1)

model2<- glm(bmi~log(pb_mean_iqr)+age+race_white + sex +  econ12_+ parent_edu, data = dat_, weights = ipw)
summary(model2)#aic 2935.1
exp(coef(model2))
exp(confint(model2))
plot(model2)

summary(model1$residuals)
plot(model1,which=2)
ggplot(mapping=aes(sample=(dat_$pb208_ppm_mean)))+
  stat_qq()+stat_qq_line()

#additionally adjusting for census tract med househould income
model3<- glm(bmi~(pb_mean_iqr) +age+race_white + sex + econ12_ + medInc60+ parent_edu, data = dat_, weights = ipw)
summary(model3)
(confint(model3))

# check Pb-bmi association with 15IQR removed tooth average with missing data indicators
model1_<- glm(bmi~log(pb208_ppm_mean) + age +race_white_mi + sex + econ12_ + bweight_mi+ parent_edu_mi, data = dat_mi, weights = ipw) # additional covariates: childhood ses?
summary(model1_)#aic 2673.9
vif(model1_)
plot(model1_)
confint(model1_)


#cvd check --------------------
dat$hbp_dx
dat_$hbp_dx
describe(dat_$hbp_dx)
class(dat_$hbp_dx)
dat_$hbp_dx <- as.factor(dat_$hbp_dx)
dat_<- dat_ %>% mutate(hbp = case_when(hbp_dx =="2: Yes, and have NOT been prescribed medication" ~ 1,
                                       hbp_dx == "1: Yes, and have been prescribed medication" ~ 1,
                                       hbp_dx == "0: No" ~0))
describe(dat_$hbp)

modela <- glm(hbp ~(pb_mean_iqr) + sex+ age + race_white + econ12_ + parent_edu,family=binomial(link='logit'),data=dat_, weights = ipw)
summary(modela) #aic 1752.4
(confint(modela))
exp(cbind(OR = coef(modela), confint(modela)))

modelb <- glm(hbp ~(pb_mean_pre_iqr) + sex+ age + race_white + econ12_ + parent_edu,family=binomial(link='logit'),data=dat_, weights = ipw)
summary(modelb)
exp(cbind(OR = coef(modelb), confint(modelb)))

modelc <- glm(hbp ~(pb_mean_post_iqr) + sex+ age + race_white + econ12_ + parent_edu,family=binomial(link='logit'),data=dat_,  weights = ipw)
summary(modelc)
exp(cbind(OR = coef(modelc), confint(modelc)))

modelc <- glm(hbp ~pb_mean_post_iqr + pb_mean_pre_iqr + sex+ age + race_white + econ12_ + parent_edu,family=binomial(link='logit'),data=dat_,  weights = ipw)
summary(modelc)
exp(cbind(OR = coef(modelc), confint(modelc)))

# pre/post natal for 15IQR tooth
hist(dat_$pb208_ppm_mean)

#check correlation between pre and post natal concentration and whole tooth
corel <- dat_[ , c("pb_mean_pre", "pb_mean_post", "pb_mean")]
cor(na.omit(corel))

dat_<-dat_%>% mutate(pb_mean_pre_iqr = pb_mean_pre/IQR(pb_mean_pre, na.rm=TRUE))
dat_<-dat_%>% mutate(pb_mean_post_iqr = pb_mean_post/IQR(pb_mean_post, na.rm=TRUE))

model1pre<- glm(bmi~ (pb_mean_pre_iqr)+ age + sex + race_white+econ12_ +parent_edu, data = dat_, weights = ipw) # additional covariates: childhood ses?
summary(model1pre)
confint(model1pre)
plot(model1pre)

model1post<- glm(bmi~ (pb_mean_post_iqr) +age + sex+race_white + econ12_+ parent_edu, data = dat_, weights=ipw) # additional covariates: childhood ses?
summary(model1post)
confint(model1post)

#plot with pre and post together in same model
model1pre<- glm(bmi~ (pb_mean_pre_iqr)+ (pb_mean_post_iqr)+ age + sex + race_white+econ12_ +parent_edu, data = dat_, weights = ipw) # additional covariates: childhood ses?
summary(model1pre)
(cbind(est = coef(model1pre), confint(model1pre)))
plot(model1pre)

library(car)

#BMI categories of overweight or not 
model2 <- glm(bmi_cat ~(pb_mean_iqr) + sex+ age + race_white + econ12_ + parent_edu,family=binomial(link='logit'),data=dat_, weights = ipw)
summary(model2)
exp(cbind(OR = coef(model2), confint(model2)))

model2 <- glm(bmi_cat ~log(pb_mean_pre_iqr) + sex+ age + race_white + econ12_ + parent_edu,family=binomial(link='logit'),data=dat_, weights = ipw)
summary(model2)
exp(cbind(OR = coef(model2), confint(model2)))

model2 <- glm(bmi_cat ~(pb_mean_post_iqr) + sex+ age + race_white + econ12_ + parent_edu,family=binomial(link='logit'),data=dat_, weights = ipw)
summary(model2)
exp(cbind(OR = coef(model2), confint(model2)))

#pre and post in same model
model2 <- glm(bmi_cat ~(pb_mean_post_iqr) + pb_mean_pre_iqr+ sex+ age + race_white + econ12_ + parent_edu,family=binomial(link='logit'),data=dat_, weights = ipw)
summary(model2)
exp(cbind(OR = coef(model2), confint(model2)))

#addition of median household income census tract-------------------------------
#to continuous bmi models
model1<- glm(bmi~ (pb_mean_iqr) + age + sex + race_white +medInc60 +econ12_+parent_edu, data = dat_, weights = ipw) # additional covariates: childhood ses?
summary(model1)
(confint(model1))

model1pre_<- glm(bmi~ (pb_mean_pre_iqr) + age + sex + race_white +medInc60 +econ12_+parent_edu, data = dat_, weights = ipw) # additional covariates: childhood ses?
summary(model1pre_)
(confint(model1pre_))

model1post_<- glm(bmi~ (pb_mean_post_iqr) + age + sex + race_white +medInc60 +econ12_+parent_edu, data = dat_, weights = ipw) # additional covariates: childhood ses?
summary(model1post_)
(confint(model1post_))

model1post_<- glm(bmi~ (pb_mean_post_iqr) + pb_mean_pre_iqr + age + sex + race_white +medInc60 +econ12_+parent_edu, data = dat_, weights = ipw) # additional covariates: childhood ses?
summary(model1post_)
(confint(model1post_))

#to cat bmi models
model2 <- glm(bmi_cat ~(pb_mean_iqr) + sex+ age + race_white + medInc60+ econ12_ + parent_edu,family=binomial(link='logit'),data=dat_, weights = ipw)
summary(model2)
exp(cbind(OR = coef(model2), confint(model2)))

model2 <- glm(bmi_cat ~(pb_mean_pre_iqr) + sex+ age + race_white + medInc60+ econ12_ + parent_edu,family=binomial(link='logit'),data=dat_, weights = ipw)
summary(model2)
exp(cbind(OR = coef(model2), confint(model2)))

model2 <- glm(bmi_cat ~(pb_mean_post_iqr) + sex+ age + race_white + medInc60+ econ12_ + parent_edu,family=binomial(link='logit'),data=dat_, weights = ipw)
summary(model2)
exp(cbind(OR = coef(model2), confint(model2)))

model2 <- glm(bmi_cat ~pb_mean_pre_iqr+ pb_mean_post_iqr + sex+ age + race_white + medInc60+ econ12_ + parent_edu,family=binomial(link='logit'),data=dat_, weights = ipw)
summary(model2)
exp(cbind(OR = coef(model2), confint(model2)))

# add medinc60 to cvd
modela <- glm(hbp ~(pb_mean_iqr) + sex+ medInc60+ age + race_white + econ12_ + parent_edu,family=binomial(link='logit'),data=dat_, weights = ipw)
summary(modela)
exp(cbind(OR = coef(modela), confint(modela)))

modelb <- glm(hbp ~(pb_mean_pre_iqr) + sex+ medInc60+ age + race_white + econ12_ + parent_edu,family=binomial(link='logit'),data=dat_, weights = ipw)
summary(modelb)
exp(cbind(OR = coef(modelb), confint(modelb)))

modelc <- glm(hbp ~log(pb_mean_post_iqr) + sex+ medInc60+ age + race_white + econ12_ + parent_edu,family=binomial(link='logit'),data=dat_,  weights = ipw)
summary(modelc)
exp(cbind(OR = coef(modelc), confint(modelc)))

modeld <- glm(hbp ~(pb_mean_post_iqr)+ pb_mean_pre_iqr + sex+ medInc60+ age + race_white + econ12_ + parent_edu,family=binomial(link='logit'),data=dat_,  weights = ipw)
summary(modeld)
exp(cbind(OR = coef(modeld), confint(modeld)))

# check if association persists after removing high prenatal value study ID 202680
hist(dat_$bmi)
datt_<-dat_ %>% filter(study_id!=144825)
hist(datt_$bmi)

model1<- glm(bmi~ (pb_mean_iqr) + age + sex +race_white+ econ12_+ parent_edu, data = datt_, weights = ipw) # additional covariates: childhood ses?
summary(model1)#aic: 2497.3
exp(coef(model1))
exp(confint(model1))
plot(model1)
model1<- glm(bmi~ (pb_mean_pre_iqr) + age + sex +race_white+ econ12_ + parent_edu, data = datt_, weights = ipw) # additional covariates: childhood ses?
summary(model1)#aic:1784
confint(model1)
plot(model1)
model1<- glm(bmi~ (pb_mean_post_iqr) + age + sex +race_white+ econ12_ + parent_edu, data = datt_, weights = ipw) # additional covariates: childhood ses?
summary(model1)
confint(model1)

#categorical
model2 <- glm(bmi_cat ~(pb_mean_iqr) + sex+ age + race_white+ econ12_ + parent_edu,family=binomial(link='logit'),data=datt_, weights = ipw)
summary(model2)
exp(coef(model2))
exp(confint(model2))
model2 <- glm(bmi_cat ~(pb_mean_pre_iqr) + sex+ age +race_white + econ12_ + parent_edu,family=binomial(link='logit'),data=datt_, weights = ipw)
summary(model2)
model2 <- glm(bmi_cat ~(pb_mean_post_iqr) + sex+ age + race_white + econ12_ + parent_edu,family=binomial(link='logit'),data=datt_, weights = ipw)
summary(model2)


#and remove extreme pb outliers
#remove extreme lead tooth average concentrations?
hist(dat_$pb_mean)
i1 <- which.max(dat_$pb_mean)
dat_$pb_mean[i1]
dat_[i1, c("study_id", "bmi", "sex", "age","pb_mean", "pb_mean_pre","pb_mean_post", "prenatal")]

i1 <- which.max(datt_$pb_mean)
datt_$pb_mean[i1]
datt_[i1, c("study_id", "bmi", "sex", "age","pb_mean", "pb_mean_pre","pb_mean_post", "prenatal")]

datt_<-dat_ %>% filter(study_id!=202680)
datt_<-datt_ %>% filter(study_id!=177385)
hist(dat_$pb_mean)
hist(datt_$pb_mean)

model1<- glm(bmi~ (pb_mean_iqr) + age + sex+race_white+ econ12_+ parent_edu, data = datt_, weights = ipw) # additional covariates: childhood ses?
summary(model1)#aic: 2497.3
exp(coef(model1))
exp(confint(model1))
plot(model1)

model1<- glm(bmi~ (pb_mean_iqr) + age + home_ses+ sex+race_white+ econ12_+ parent_edu, data = dat_ses, weights = ipw) # additional covariates: childhood ses?
summary(model1)#aic: 2497.3
exp(coef(model1))
exp(confint(model1))
plot(model1)

model1<- glm(bmi~ (pb_mean_iqr) + age + sex+race_white+ econ12_+ parent_edu, data = dat_ses, weights = ipw) # additional covariates: childhood ses?
summary(model1)#aic: 2497.3


model1<- glm(bmi~ (pb_mean_pre_iqr) + age + sex +race_white+econ12_ + parent_edu, data = datt_, weights = ipw) # additional covariates: childhood ses?
summary(model1)#aic:1784
exp(coef(model1))
exp(confint(model1))
plot(model1)
model1<- glm(bmi~ (pb_mean_post_iqr) + age + sex +race_white+ econ12_ + parent_edu, data = datt_, weights = ipw) # additional covariates: childhood ses?
summary(model1)
confint(model1)

#categorigal bmi
model2 <- glm(bmi_cat ~(pb_mean_iqr) + sex+age +race_white + econ12_ + parent_edu,family=binomial(link='logit'),data=datt_, weights = ipw)
summary(model2)
exp(cbind(OR = coef(model2), confint(model2)))

model2 <- glm(bmi_cat ~(pb_mean_pre_iqr) + sex+ age +race_white + econ12_ + parent_edu,family=binomial(link='logit'),data=datt_, weights = ipw)
summary(model2)
model2 <- glm(bmi_cat ~(pb_mean_post_iqr) + sex+age +race_white + econ12_ + parent_edu,family=binomial(link='logit'),data=datt_, weights = ipw)
summary(model2)

#check compiled ses xinye score for additional ses adjustment
describe(dat_$kitch_12_sp)# kitch_12_sp	Q1301a when you were 12 did your home have a kitchen	"Yes, for my family's use only", "Yes, shared with others", "No, did not have"
#99.4% yes, for family use only
describe(dat_$water_12_sp)# water_12_sp	Q1301b when you were 12 did your home have hot water	"No, did not have", "Yes, for my family's use only", "Yes, shared with others"
#99% yes family use only
describe(dat_$toilet_12_sp)# toilet_12_sp	Q1301d when you were 12 did your home have an indoor flush toilet	"No, did not have", "Yes, for my family's use only", "Yes, shared with others"
#99% yes family use only
describe(dat_$bath_12_sp)# bath_12_sp	Q1301e when you were 12 did your home have a bathtub	"No, did not have", "Yes, for my family's use only", "Yes, shared with others"
#99% yes family use only
describe(dat_$wash_12_sp)# wash_12_sp	Q1301f when you were 12 did your home have a washing machine	"No, did not have", "Yes, for my family's use only", "Yes, shared with others"
#96%yes family only
describe(dat_$dry_12_sp)# dry_12_sp	Q1301g when you were 12 did your home have a laundry dryer	"No, did not have", "Yes, for my family's use only", "Yes, shared with others"
#93% yes family only
describe(dat_$tel_12_sp)# tel_12_sp	Q1301h when you were 12 did your home have a telephone	"No, did not have", "Yes, for my family's use only", "Yes, shared with others"
#99% yes family only

#better variability in responses below
describe(dat_$bed_12)# bed_12	Q1300 when you were 12 how many bedrooms did your house have	numerical (0-10+)
describe(dat_$frzr_12_sp)# frzr_12_sp	Q1301c when you were 12 did your home have a freezer	"No, did not have", "Yes, for my family's use only", "Yes, shared with others"
#62% yes, 37% no
describe(dat_$hometv_12_sp)# hometv_12_sp	Q1302 when you were 12, how many of the following did your home have: television set	"None", "One", "Two or more"
describe(dat_$homerad_12_sp)# homerad_12_sp	Q1302 when you were 12, how many of the following did your home have: radio	"None", "One", "Two or more"
describe(dat_$homeac_12_sp)# homeac_12_sp	Q1302 when you were 12, how many of the following did your home have:  air conditioning units (central air == "two or more") 	"None", "One", "Two or more"
describe(dat_$homecar_12_sp)# homecar_12_sp	Q1302 when you were 12, how many of the following did your home have: automobiles	"None", "One", "Two or more"
describe(dat_$econ_12)# econ_12	Q1303 how would you describe your familyÍs financial situation when you were a very young child	"Average", "Poor", "Very poor", "Very well off", "Well-off"

# add additional SES variables individually into model

modelses<- glm(bmi~(pb_mean_iqr)+age+race_white + medInc60+ bed_12 + frzr_12_sp+hometv_12_sp + homerad_12_sp + homeac_12_sp + homecar_12_sp +sex +  econ12_+ parent_edu, data = dat_, weights = ipw)
summary(modelses)
(confint(modelses))

modelses<- glm(bmi~(pb_mean_pre_iqr)+age+race_white + medInc60 +bed_12 + frzr_12_sp+hometv_12_sp + homerad_12_sp + homeac_12_sp + homecar_12_sp +sex +  econ12_+ parent_edu, data = dat_, weights = ipw)
summary(modelses)
(confint(modelses))

modelses<- glm(bmi~(pb_mean_post_iqr)+age+race_white +medInc60+ bed_12 + frzr_12_sp+hometv_12_sp + homerad_12_sp + homeac_12_sp + homecar_12_sp +sex +  econ12_+ parent_edu, data = dat_, weights = ipw)
summary(modelses)
(confint(modelses))

modelses<- glm(bmi~pb_mean_post_iqr + pb_mean_pre_iqr+medInc60 +age+race_white + bed_12 + frzr_12_sp+hometv_12_sp + homerad_12_sp + homeac_12_sp + homecar_12_sp +sex +  econ12_+ parent_edu, data = dat_, weights = ipw)
summary(modelses)
(confint(modelses))

# cat bmi
model2 <- glm(bmi_cat ~(pb_mean_iqr) + sex+age + bed_12 + frzr_12_sp+hometv_12_sp + homerad_12_sp + homeac_12_sp + homecar_12_sp+race_white + econ12_ + parent_edu,family=binomial(link='logit'),data=dat_, weights = ipw)
summary(model2)
exp(cbind(OR = coef(model2), confint(model2)))

model2 <- glm(bmi_cat ~(pb_mean_pre_iqr) + sex+ bed_12 + frzr_12_sp+hometv_12_sp + homerad_12_sp + homeac_12_sp + homecar_12_sp+ age +race_white + econ12_ + parent_edu,family=binomial(link='logit'),data=dat_, weights = ipw)
summary(model2)
exp(cbind(OR = coef(model2), confint(model2)))

model2 <- glm(bmi_cat ~(pb_mean_post_iqr) + sex+ bed_12 + frzr_12_sp+hometv_12_sp + homerad_12_sp + homeac_12_sp + homecar_12_sp+age +race_white + econ12_ + parent_edu,family=binomial(link='logit'),data=dat_, weights = ipw)
summary(model2)
exp(cbind(OR = coef(model2), confint(model2)))

model2 <- glm(bmi_cat ~pb_mean_post_iqr + pb_mean_pre_iqr + medInc60+ sex+ bed_12 + frzr_12_sp+hometv_12_sp + homerad_12_sp + homeac_12_sp + homecar_12_sp+age +race_white + econ12_ + parent_edu,family=binomial(link='logit'),data=dat_, weights = ipw)
summary(model2)
exp(cbind(OR = coef(model2), confint(model2)))

# cat cvd
model2 <- glm(hbp ~(pb_mean_iqr) + sex+medInc60+age+ bed_12 + frzr_12_sp+hometv_12_sp + homerad_12_sp + homeac_12_sp + homecar_12_sp+race_white + econ12_ + parent_edu,family=binomial(link='logit'),data=dat_, weights = ipw)
summary(model2)
exp(cbind(OR = coef(model2), confint(model2)))

model2 <- glm(hbp ~(pb_mean_pre_iqr) + sex+medInc60+ bed_12 + frzr_12_sp+hometv_12_sp + homerad_12_sp + homeac_12_sp + homecar_12_sp+ age +race_white + econ12_ + parent_edu,family=binomial(link='logit'),data=dat_, weights = ipw)
summary(model2)
exp(cbind(OR = coef(model2), confint(model2)))

model2 <- glm(hbp ~(pb_mean_post_iqr) + sex+medInc60+ bed_12 + frzr_12_sp+hometv_12_sp + homerad_12_sp + homeac_12_sp + homecar_12_sp+age +race_white + econ12_ + parent_edu,family=binomial(link='logit'),data=dat_, weights = ipw)
summary(model2)
exp(cbind(OR = coef(model2), confint(model2)))

model2 <- glm(hbp ~(pb_mean_post_iqr) + pb_mean_pre_iqr + medInc60 + bed_12 + frzr_12_sp+hometv_12_sp + homerad_12_sp + homeac_12_sp + homecar_12_sp+age +race_white + econ12_ + parent_edu,family=binomial(link='logit'),data=dat_, weights = ipw)
summary(model2)
exp(cbind(OR = coef(model2), confint(model2)))

model2 <- glm(hbp ~(pb_mean_post_iqr) + pb_mean_pre_iqr +age +race_white + econ12_ + parent_edu,family=binomial(link='logit'),data=dat_, weights = ipw)
summary(model2)
exp(cbind(OR = coef(model2), confint(model2)))

library(car)
vif(model2)

#check correlation of ses variables
corel <- dat_[ , c("econ12_", "medInc60", "bed_12", "frzr_12_sp", "hometv_12_sp", "homerad_12_sp", "homeac_12_sp", "homecar_12_sp")]
#need to think of visual way to assess correlation for factor variables



# compile SES home factors into single categorical variable
dat_ses<- dat_ %>% mutate(home_car = case_when(homecar_12_sp == "0: None"~ 0,
                                              homecar_12_sp == "1: One" ~ 1,
                                              homecar_12_sp == "2: Two or more" ~ 2)) 

dat_ses<- dat_ses %>% mutate(home_ac = case_when(homeac_12_sp == "0: None"~ 0,
                                               homeac_12_sp == "1: One" ~ 1,
                                               homeac_12_sp == "2: Two or more" ~ 2)) 

dat_ses<- dat_ses %>% mutate(home_rad = case_when(homerad_12_sp == "0: None"~ 0,
                                                 homerad_12_sp == "1: One" ~ 1,
                                                 homerad_12_sp == "2: Two or more" ~ 2)) 

dat_ses<- dat_ses %>% mutate(home_tv = case_when(hometv_12_sp == "0: None"~ 0,
                                                 hometv_12_sp == "1: One" ~ 1,
                                                 hometv_12_sp == "2: Two or more" ~ 2)) 

dat_ses<- dat_ses %>% mutate(home_frzr = case_when(frzr_12_sp == "0: No, did not have"~ 0,
                                                 frzr_12_sp == "1: Yes, for my family's use only" ~ 2,
                                                 frzr_12_sp == "2: Yes, shared with others" ~ 1)) 

dat_ses<- dat_ses %>% mutate(home_dry = case_when(dry_12_sp =="0: No, did not have"~ 0,
                                               dry_12_sp == "1: Yes, for my family's use only" ~ 2,
                                               dry_12_sp == "2: Yes, shared with others" ~ 1)) 

dat_ses<- dat_ses %>% mutate(home_wash = case_when(wash_12_sp == "0: No, did not have"~ 0,
                                                  wash_12_sp == "1: Yes, for my family's use only" ~ 2,
                                                  wash_12_sp == "2: Yes, shared with others" ~ 1)) 

dat_ses<-dat_ses %>% mutate(home_ses = home_wash+ home_dry+ home_frzr+home_tv + home_rad+ home_ac +home_car)

hist(dat_ses$home_ses)
median(dat_ses$home_ses, na.rm=T)
quantile(dat_ses$home_ses, probs=c(0.25, .50, .75), na.rm=TRUE)


describe(dat_$kitch_12_sp)# kitch_12_sp	Q1301a when you were 12 did your home have a kitchen	"Yes, for my family's use only", "Yes, shared with others", "No, did not have"
#99.4% yes, for family use only
describe(dat_$water_12_sp)# water_12_sp	Q1301b when you were 12 did your home have hot water	"No, did not have", "Yes, for my family's use only", "Yes, shared with others"
#99% yes family use only
describe(dat_$toilet_12_sp)# toilet_12_sp	Q1301d when you were 12 did your home have an indoor flush toilet	"No, did not have", "Yes, for my family's use only", "Yes, shared with others"
#99% yes family use only
describe(dat_$bath_12_sp)# bath_12_sp	Q1301e when you were 12 did your home have a bathtub	"No, did not have", "Yes, for my family's use only", "Yes, shared with others"
#99% yes family use only
describe(dat_$wash_12_sp)# wash_12_sp	Q1301f when you were 12 did your home have a washing machine	"No, did not have", "Yes, for my family's use only", "Yes, shared with others"
#96%yes family only
describe(dat_$dry_12_sp)# dry_12_sp	Q1301g when you were 12 did your home have a laundry dryer	"No, did not have", "Yes, for my family's use only", "Yes, shared with others"
#93% yes family only
describe(dat_$tel_12_sp)# tel_12_sp	Q1301h when you were 12 did your home have a telephone	"No, did not have", "Yes, for my family's use only", "Yes, shared with others"
#99% yes family only

#better variability in responses below
describe(dat_$bed_12)# bed_12	Q1300 when you were 12 how many bedrooms did your house have	numerical (0-10+)
describe(dat_$frzr_12_sp)# frzr_12_sp	Q1301c when you were 12 did your home have a freezer	"No, did not have", "Yes, for my family's use only", "Yes, shared with others"
#62% yes, 37% no
describe(dat_$hometv_12_sp)# hometv_12_sp	Q1302 when you were 12, how many of the following did your home have: television set	"None", "One", "Two or more"
describe(dat_$homerad_12_sp)# homerad_12_sp	Q1302 when you were 12, how many of the following did your home have: radio	"None", "One", "Two or more"
describe(dat_$homeac_12_sp)# homeac_12_sp	Q1302 when you were 12, how many of the following did your home have:  air conditioning units (central air == "two or more") 	"None", "One", "Two or more"
describe(dat_$homecar_12_sp)# homecar_12_sp	Q1302 when you were 12, how many of the following did your home have: automobiles	"None", "One", "Two or more"
describe(dat_$econ_12)# econ_12	Q1303 how would you describe your familyÍs financial situation when you were a very young child	"Average", "Poor", "Very poor", "Very well off", "Well-off"



# check impact of smoking
model1<- glm(bmi~pb208_ppm_mean_iqr+ever_smk+age+race_white + sex +  econ_12+ bweight+ parent_edu, data = dat_, weights = ipw)
summary(model1)

model1<- glm(bmi~pb208_ppm_mean_pre_iqr+ever_smk+age+race_white + sex +  econ_12+ bweight+ parent_edu, data = dat_, weights = ipw)
summary(model1)


# Create table 1
library(table1)
table1(~ sex + age + race_white + econ_12+ bmi_cat+ parent_edu + medInc60, data=dat_)


# Other outcomes-------------
# Tooth Pb and birthweight
mod4<- glm(bweight~(pb208_ppm_mean_iqr) + age+race_white + sex +  econ_12+ medInc60 + parent_edu, data = dat_, weights = ipw)
mod5<- glm(bweight~(pb208_ppm_mean_pre_iqr) + age+race_white + sex +  econ_12+ medInc60+ parent_edu, data = dat_, weights = ipw)
mod6<- glm(bweight~(pb208_ppm_mean_post_iqr) + age+race_white + sex +  econ_12+ medInc60+ parent_edu, data = dat_, weights = ipw)

describe(dat_$bweight)

# BMI and cognition

modela <- glm(hbp ~(pb_mean) + sex+ age + race_white+ parent_edu,family=binomial(link='logit'),data=dat_, weights = ipw)
summary(modela)

mod4<- glm(bmi~(pb_mean) + age+race_white + sex+ parent_edu, data = dat_, weights = ipw)
summary(mod4)

mod4<- glm(TMB_composite_Z~(bmi) + age+race_white + sex+ parent_edu, data = dat_, weights = ipw)
summary(mod4)

mod4<- glm(TMB_composite_Z~(hbp) + age+race_white + sex + parent_edu, data = dat_, weights = ipw)
summary(mod4)

mod4<- glm(TMB_composite_Z~(hbp)+ log(pb_mean) + age+race_white + sex + parent_edu, data = dat_, weights = ipw)
summary(mod4)

#lead and cognition
mod4<- glm(TMB_composite_Z~log(pb_mean) + age+race_white + sex + parent_edu, data = dat_, weights = ipw)
summary(mod4)#aic: 747.9
exp(cbind(OR=coef(mod5), confint(mod5)))
plot(mod4)

mod4<- glm(TMB_composite_Z~ log(pb_mean_post)+ log(pb_mean_pre)+ age+race_white + sex + parent_edu, data = dat_, weights = ipw)
summary(mod4)#aic: 747.9
plot(mod4)

mod4<- glm(TMB_composite_Z~ log(pb_mean_pre)+ age+race_white + sex + parent_edu, data = dat_, weights = ipw)
summary(mod4)#aic: 747.9
plot(mod4)

mod5<- glm(TMB_composite_Z~log(pb_mean) + hbp+ age+race_white + sex + parent_edu, data = dat_, weights = ipw)
summary(mod5)#aic: 750.57
exp(cbind(coef(mod5), confint(mod5)))

#summarize missing nl data across batches
nl_NA<-dat_ %>%
  group_by(batch) %>%
  summarise(missing = sum(is.na(prenatal)), n = n())
nl_NA

b123<- dat_ %>% filter(batch != 4)
describe(b123$prenatal)

# compile IDs with missing NL data in separate file
missNA<-dat_ %>% 
  group_by(study_id) %>% 
  filter(all(is.na(prenatal))) %>% 
  pull(study_id)
view(missNA)



#creating shortened cleaned dataset
cleantooth<- df[, c("study_id", "tooth_id.x","stl.", "group_name", "def", "BATCH","BATCH_COUNT", "tooth_type", "spot","low_ca_r","pb208_ppm", "pb208_below_lod", "pb208_ppm_lod_howell", "max_DNTspot", "max_ENLspot", "dent", "spots_pENL","DNTpastENL", "nl_usability","nl_start_from_cusp_dent", "Pbout", "pb", "pb_mean","pb_mean_pre", "pb_mean_post")]
t103945 <- cleantooth%>% filter(study_id == 103945)
clipr::write_clip(t103945)


# bmi and neuro 






