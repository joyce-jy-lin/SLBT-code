### Compiling tooth metal batches into full tooth dataset
# Based off Ruby's "tooth_dataset_20231019.Rmd file
# 11/16/2023

library(tidyverse)
library(ggplot2)

#Load data
## Batch 1, updated as of 9/22/23 for the teeth/spots that had study id matching issues (previously as of 8/16 for the weird tooth)
b1 <-  read_excel("/Volumes/shared/EOME/Weisskopf/Saint_Louis_Baby_Teeth_Study/Data 2020/Tooth Analysis/Dartmouth Post-ICP-MS Processing/Output/SLBT_Tooth_Data1_2023_09_22.xlsx", 
                  sheet ="proc3_flags")

b1_old <-  read_excel("/Volumes/shared/EOME/Weisskopf/Saint_Louis_Baby_Teeth_Study/Data 2020/Tooth Analysis/Dartmouth Post-ICP-MS Processing/Output/SLBT_Tooth_Data1_2023_08_17.xlsx", 
                      sheet="proc3_flags")
class(b1_old$hg202_ppm_lod_howell_mean)

## Batches 1-3 (DON'T TAKE BATCH ONE FROM THIS FILE THOUGH!)
b123 <-  read_excel("/Volumes/shared/EOME/Weisskopf/Saint_Louis_Baby_Teeth_Study/Data 2020/Tooth Analysis/Dartmouth Post-ICP-MS Processing/Output/SLBT_Tooth_Data_All_2023_03_09.xlsx", 
                  sheet = "proc3_flags_all")
class(b123$hg202_ppm_lod_howell_mean) 
table(b123$hg202_ppm_lod_howell_mean, useNA = "always")


## Batch 4.1 
b4 <- read_excel("/Volumes/shared/EOME/Weisskopf/Saint_Louis_Baby_Teeth_Study/Data 2020/Tooth Analysis/Dartmouth Post-ICP-MS Processing/Output/SLBT_Tooth_Data4_2023_04_14.xlsx", 
               sheet = "proc3_flags")

## Batch 4.2 
b4_2 <- read_excel("/Volumes/shared/EOME/Weisskopf/Saint_Louis_Baby_Teeth_Study/Data 2020/Tooth Analysis/Dartmouth Post-ICP-MS Processing/Output/SLBT_Tooth_Data4_part2_2023_08_01.xlsx", 
                 sheet = "proc3_flags")

## Batch 4.3 
b4_3 <- read_excel("/Volumes/shared/EOME/Weisskopf/Saint_Louis_Baby_Teeth_Study/Data 2020/Tooth Analysis/Dartmouth Post-ICP-MS Processing/Output/SLBT_Tooth_Data4_part3_2023_12_19.xlsx", 
                   sheet="proc3_flags")
                 
## Dataset with neonatal line spot number 
#nl <- read.csv("/Volumes/shared/EOME/Weisskopf/Saint_Louis_Baby_Teeth_Study/Individuals/Ruby/datasets_creation_qc/hsrp_prog_nl_swapped.csv", header = T)

#### CHECK DATASETS BRIEFLY ####
## updated batch 1 data 
length(unique(b1$study_id)) 
length(unique(b1$tooth_id)) 
sum(is.na(b1$study_id)) 
glimpse(b1) 

length(unique(b4_2$study_id)) 
length(unique(b4_2$tooth_id)) 
sum(is.na(b4_2$study_id)) 

length(unique(b4_3$study_id)) 
length(unique(b4_3$tooth_id)) 
sum(is.na(b4_2$study_id)) 

## fix issue: hg202_ppm_lod_howell_mean had some excel calculation errors 
class(b1$hg202_ppm_lod_howell_mean)
sum(b1$hg202_ppm_lod_howell_mean == "#NUM!")
sum(b1_old$hg202_ppm_lod_howell_mean == "#NUM!") #was this the same in the previous dataset? 
b1$hg202_ppm_lod_howell_mean <- as.double(ifelse(b1$hg202_ppm_lod_howell_mean == "#NUM!",
                                                 NA,
                                                 b1$hg202_ppm_lod_howell_mean))
table(b1$hg202_ppm_lod_howell_mean, useNA = "ifany")
## is this restricted to the hg or with other metals too? 
class(b1$pb208_ppm_lod_howell_mean) 

## figuring out what is going on with the NA IDs in batch 1 data 
# table(tooth_full$stl., useNA = "always")
#   View(tooth_full[is.na(tooth_full$stl.),])
#   unique(tooth_full[is.na(tooth_full$stl.),"session_name"])
#   unique(tooth_full[, c("session_name", "study_id")])
#   
# table(b1$stl., useNA = "always")
#     
# unique(tooth_full[is.na(tooth_full$stl.),"session_name"]) %in% b123$session_name

## batches 1-3 data 
length(unique(b123$study_id)) ## this should match with what you know of how many individuals are in the dataset 
glimpse(b123) ## what are the variables in this dataset?  
table(b123$flag_issue, b123$batch, useNA = "always") ## Zn_Ba_issue indicated for some teeth, including all of Batch 3 

## check batch 4.1 
length(unique(b4$study_id)) 
glimpse(b4) 
table(b4$group_name, useNA = "always") ## importantly in batch 4 onward now "group_name" matters for separating EDJ (dentine) and ENL (enamel) measurements which have different averaging times 

## check batch 4.2
length(unique(b4_2$study_id))
table(b4_2$group_name, useNA = "always")

## check batch 4.3
length(unique(b4_3$study_id))
table(b4_3$group_name, useNA = "always")


## check nl dataset 
#glimpse(nl)
#length(unique(nl$study_id)) ## there are duplicates at least based on study ID (as multiple teeth may have been )
#   n_occur <- data.frame(table(nl$study_id))
#   n_occur[n_occur$Freq > 1,] ## 54 study IDs are duplicated at least once 
# length(unique(nl[,c("study_id","TOOTH.ID")]))
# sum(duplicated(nl[,c("study_id", "TOOTH.ID")])) ## 40 are duplicate rows (could be less than 40 if some are duplicated ,more than once)
#sum(duplicated(nl[,c("study_id", "TOOTH.ID", "STL.")])) ## no duplicates here: use this to match up with the tooth metals data 


#### MERGE DATASETS TOGETHER ####
## first drop b1 from the B1-3 (outdated) dataset
table(b123$batch) 
b2_3 <- b123 %>% 
  filter(batch != 1 & batch != 1.3) ## keep ones that aren't in batch 1 or 1.3

## fixing issue with class for stl. which rarely has a letter in it 
b1<-b1 %>% 
  rename(
    stl. = `stl#`)
b2_3<-b2_3 %>% 
  rename(
    stl. = `stl#`)
b4<-b4 %>% 
  rename(
    stl. = `stl#`)
b4_2<-b4_2 %>% 
  rename(
    stl. = `stl#`)
b4_3<-b4_3 %>% 
  rename(
    stl. = `stl#`)

class(b1$stl.)
class(b2_3$stl.)
class(b4$stl.)
class(b4_2$stl.)
class(b4_3$stl.)
b1$stl. <- as.character(b1$stl.)
b2_3$stl. <- as.character(b2_3$stl.)
b4_2$stl. <- as.character(b4_2$stl.)
b4_3$stl. <- as.character(b4_3$stl.)

## join batch 1 (re-ran) and batch 2 and 3 together 
tooth_i1 <- full_join(b1, b2_3)

## join batch 4.1 to batches 1-3
tooth_i2 <- full_join(tooth_i1, b4)

## join batch 4.2 to batches 1-4. 
tooth_i3 <- full_join(tooth_i2, b4_2)

## join batch 4.3 to batches 1-4. 
tooth_full <- full_join(tooth_i3, b4_3)


#correct wrong study id in raw data (only needed if using raw tooth data tooth_nl) -----------
tooth_full$study_id_incorrect <- tooth_full$study_id 
tooth_full$tooth_id_incorrect <- tooth_full$tooth_id 
tooth_full$stl._incorrect <- tooth_full$stl. 
tooth_full <- tooth_full %>% 
  mutate(study_id = if_else(study_id_incorrect == 194063, 213870, study_id), 
         study_id = if_else(study_id_incorrect == 121555, 212155, study_id),
         tooth_id = if_else(study_id_incorrect == 146080, 21877, tooth_id),
         stl. = if_else(study_id_incorrect == 146080, "246164", stl.))

# Prelim QC ---------------------------------------------
## Set low calcium spots to NA ####
table(tooth_full$low_ca_r, tooth_full$batch, useNA = "always")
tooth_noloca <- tooth_full %>%
  mutate(across(
    ends_with("ppm"),
    ~ case_when(low_ca_r == "low" ~ NA_integer_, 
                TRUE ~ .x) ## TRUE is a way of saying "else" in case_when (weird, I know)
  ))

## Zn/Ba
table(tooth_noloca$flag_issue, useNA = "always")
tooth_noloca_zb <- tooth_noloca %>%
  mutate(across(
    c(zn66_ppm, ba137_ppm),
    ~ case_when(flag_issue == "Zn_Ba_issue" ~ NA_integer_, 
                TRUE ~ .x) ## TRUE is a way of saying "else" in case_when (weird, I know)
  ))

length(unique(tooth_full$study_id))
length(unique(tooth_full$tooth_id))

write_csv(tooth_noloca_zb, "/Volumes/shared/EOME/Weisskopf/Saint_Louis_Baby_Teeth_Study/Individuals/Joyce/Tooth data/tooth_b1_4_3.csv")



