# making new NL line record sheet that includes old sonya sheet data and new batch data from HSRP
library(tidyverse)
library(Hmisc)
nl <- read.csv("/Volumes/shared/EOME/Weisskopf/Saint_Louis_Baby_Teeth_Study/Individuals/Ruby/datasets_creation_qc/hsrp_prog_nl_swapped.csv", 
               header = T)
hsrp<-read.csv("/Volumes/shared/EOME/Weisskopf/Saint_Louis_Baby_Teeth_Study/Individuals/Joyce/SLBT/tooth_imaging/hsrpnotes.csv")

nl<- nl %>% 
  rename(tooth_id = `TOOTH.ID`,
         BATCH_COUNT=`BATCH.CNT`,
         nl_usability = `USABILITY`)

hsrp<- hsrp %>% 
  rename(tooth_id = tooth_id.x)

nl_sub <- nl %>% select("study_id", "tooth_id", "STL.", "max_dentine_spot", "nl_start_from_cusp_dent", 
                        "nl_usability","BATCH", "BATCH_COUNT","COMMENTS")

#merge
hsrp_nl <- left_join(hsrp, nl_sub, 
                    by = c("study_id", "tooth_id", "STL.", "BATCH","BATCH_COUNT"))

write.csv(hsrp_nl,file='/Volumes/shared/EOME/Weisskopf/Saint_Louis_Baby_Teeth_Study/Individuals/Joyce/Tooth data/NL_HSRP_record.csv')
