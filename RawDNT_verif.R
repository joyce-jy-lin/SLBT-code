# Data cleaning for teeth that have been imaged to check for dentin spots past enamel

library(data.table)
library(tidyverse)
library(Hmisc)
library(trelliscopejs)
library(plotly)
library(ggplot2)

#load batch 1-4 raw data merged prog 3 flags versions
raw<-read.csv("/Volumes/shared/EOME/Weisskopf/Saint_Louis_Baby_Teeth_Study/Individuals/Joyce/Tooth data/tooth_b1_4_3.csv")
#load tooth imaging notes for crown dentin verification
#note<-read.csv("/Volumes/shared/EOME/Weisskopf/Saint_Louis_Baby_Teeth_Study/Individuals/Joyce/SLBT/tooth_imaging/hsrpnotes.csv")
note<-read.csv("/Volumes/shared/EOME/Weisskopf/Saint_Louis_Baby_Teeth_Study/Individuals/Joyce/Tooth data/NL_HSRP_record.csv")

length(unique(raw$study_id))
length(unique(raw$tooth_id))

b4<- raw %>% filter(batch==4)
length(unique(b4$tooth_id))
#taking out obs with low Ca dropout
metals <- c( "pb208_ppm", "mn55_ppm", "cu63_ppm", "zn66_ppm","mg26_ppm","al27_ppm","hg202_ppm",
             "as75_ppm","cd111_ppm", "cr53_ppm", "cr52_ppm","fe56_ppm","ba137_ppm","li7_ppm","na23_ppm","ni60_ppm","sn118_ppm","sr88_ppm")
main<-raw %>% mutate(across(c(metals), ~replace(., low_ca_r == "low", NA)))

# remove first two and last two spots on each tooth for edge effect
#make NA if spot = 1 or 2
#main<-main %>% mutate(across(c(metals), ~replace(., spot == 1, NA)))
#main<-main %>% mutate(across(c(metals), ~replace(., spot == 2, NA)))
# replace with NA if highest or second highest spot value
#main<-main %>% group_by(study_id)%>% mutate(across(c(metals), ~replace(., spot == max(spot), NA)))
#main<-main %>% group_by(study_id)%>% mutate(across(c(metals), ~replace(., spot == max(spot)-1, NA)))

#vis
T86461<-subset(main, tooth_id ==86461)
p3<- T86461 %>% ggplot(., aes(x=spot, y=pb208_ppm))+ geom_point()
p3 #still need outlier removal IQR or SD

#merge with imaging notes
merged <- main %>% left_join(note, by = "tooth_id","study_id")
length(unique(merged$tooth_id))

dat<-merged %>%select(-contains(c("mean", "cps", "sd",".y","incorrect")))

df<-dat %>% group_by(tooth_id, group_name) %>% mutate(maxspot = max(spot))
EDJ<-subset(df, group_name =="EDJ")
ENL<-subset(df, group_name =="ENL")
EDJ <- EDJ %>% rename(max_DNTspot = maxspot)
ENL <- ENL %>% rename(max_ENLspot = maxspot)
ENL<-ENL %>% select (-c(spot))

newdf = merge(EDJ, ENL[,c("max_ENLspot", "tooth_id")], by="tooth_id", all.x = TRUE)

#check
length(unique(newdf$tooth_id))
b4<-subset(newdf, batch ==4)
b1<-subset(newdf, batch ==1)
# create new column indicating number of dentine spots to include (dentine spots covered by enamel)
newdat<-newdf%>% mutate(dent = max_DNTspot - spots_pENL) #for batches 1-3 with no ENL measured, count from imaging

newdat_ <-newdat %>% 
  mutate(
    DNTpastENL = case_when(
      spot> max_ENLspot ~ "1",
      spot>dent ~ "1",
      spot<=max_ENLspot ~"0",
      spot<=dent ~"0",
      TRUE ~ NA_character_))

length(unique(newdat_$tooth_id))
b1<-subset(newdat_, batch ==1)
b4<-subset(newdat_, batch ==4)
#check
newdat_$DNTpastENL<-as.double(newdat_$DNTpastENL)
newdat_unique<-newdat_ %>% distinct()
length(unique(newdat_unique$tooth_id))
b4<-subset(newdat_unique, batch ==4)
describe(newdat_unique$DNTpastENL)

#check
T86461<-subset(newdat_unique, tooth_id ==1959)

write.csv(newdat_unique,file='/Volumes/shared/EOME/Weisskopf/Saint_Louis_Baby_Teeth_Study/Individuals/Joyce/Tooth data/rawtooth_dntverifiedspots_4_3.csv')


# vis after removing tooth root spots in dentin
newdat_unique<-read.csv("/Volumes/shared/EOME/Weisskopf/Saint_Louis_Baby_Teeth_Study/Individuals/Joyce/Tooth data/rawtooth_dntverifiedspots_4_3.csv")

dnt<-subset(newdat_unique, DNTpastENL ==0)
T86461_<-subset(dnt, tooth_id ==86461)
p4<- T86461_ %>% ggplot(., aes(x=spot, y=pb208_ppm))+ geom_point()
p4

#vis all teeth interactively
dnt<-subset(newdat_unique, DNTpastENL ==0)
length(unique(dnt$tooth_id))
datt <- dnt[c('tooth_id', 'study_id.x', 'spot', 'batch', 'pb208_ppm', 'BATCH','BATCH_COUNT')]
p<-datt %>% 
  ggplot(., aes(x = spot, y = pb208_ppm)) + 
  geom_point() +
  geom_vline(xintercept = 0, linetype = "dotted") +
  facet_trelliscope(~study_id.x, ncol = 3, scales = "free") +
  theme_bw()
p

# Start outlier removal for metals ----------------------------------
# need a method to check LODS here for metals with many neg tooth averages

# 3SD approach
dnt_<-dnt %>% group_by(study_id.x) %>%
  arrange(start_time_clean,spot) %>%
  mutate(pb_mean = mean(pb208_ppm, na.rm=T)) %>%
  as.data.frame()

dnt_<-dnt_ %>% group_by(study_id.x) %>%
  arrange(start_time_clean,spot) %>%
  mutate(pb_3sd = 3*sd(pb208_ppm, na.rm=T)) %>%
  as.data.frame()

dnt_<- dnt_ %>% mutate(Pb3SD = case_when(pb208_ppm >= pb_mean+pb_3sd~ '1',
                                     pb208_ppm <= pb_mean-pb_3sd ~ '1',
                                     .default ='0')) 


describe(dnt_$Pb3SD)
dnt_outrem<-dnt_ %>% mutate_at(vars(c("pb208_ppm")), funs(replace(., Pb3SD == 1, NA)))
length(unique(dnt_outrem$tooth_id))

#interactive vis to see if 3SD outlier method has handled all the outliers
dat_dnt_outrem <- dnt_outrem[c('tooth_id', 'study_id.x', 'spot', 'batch', 'pb208_ppm', 'BATCH','BATCH_COUNT')]

p1<-dat_dnt_outrem %>% 
  ggplot(., aes(x = spot, y = pb208_ppm)) + 
  geom_point() +
  geom_vline(xintercept = 0, linetype = "dotted") +
  facet_trelliscope(~study_id.x, ncol = 3, scales = "free") +
  theme_bw()
p1 # 3SD outlier removal not sufficient

# 15 IQR outlier identification instead ---------------------------------------------------
dntIQR<-newdat_unique %>% group_by(study_id.x, DNTpastENL) %>%
  mutate(Pb15IQR = 1.5*IQR(pb208_ppm, na.rm=TRUE),
         Mn15IQR = 1.5*IQR(mn55_ppm, na.rm=TRUE),
         Cu15IQR = 1.5*IQR(cu63_ppm, na.rm=TRUE),
         Mg15IQR = 1.5*IQR(mg26_ppm, na.rm=TRUE),
         Zn15IQR = 1.5*IQR(zn66_ppm, na.rm=TRUE),
         Hg15IQR = 1.5*IQR(hg202_ppm, na.rm=TRUE),
         Al15IQR = 1.5*IQR(al27_ppm, na.rm=TRUE),
         Cr15IQR = 1.5*IQR(cr52_ppm, na.rm=TRUE),
         Ba15IQR = 1.5*IQR(ba137_ppm, na.rm=TRUE),
         Li15IQR = 1.5*IQR(li7_ppm, na.rm=TRUE),
         Na15IQR = 1.5*IQR(na23_ppm, na.rm=TRUE),
         Sr15IQR = 1.5*IQR(sr88_ppm, na.rm=TRUE),#Low detect metals below
         Ni15IQR = 1.5*IQR(ni60_ppm, na.rm=TRUE),
         Fe15IQR = 1.5*IQR(fe56_ppm, na.rm=TRUE),
         As15IQR = 1.5*IQR(as75_ppm, na.rm=TRUE),
         Cd15IQR = 1.5*IQR(cd111_ppm, na.rm=TRUE),
         Sn15IQR = 1.5*IQR(sn118_ppm, na.rm=TRUE)) %>%
  as.data.frame()
length(unique(dntIQR$tooth_id))


dntIQR<-dntIQR %>% group_by(study_id.x, DNTpastENL) %>%
  mutate(PbQ1 = quantile(pb208_ppm, probs=0.25, na.rm=TRUE),
         PbQ3 = quantile(pb208_ppm, probs=0.75, na.rm=TRUE),
         MnQ1 = quantile(mn55_ppm, probs=0.25, na.rm=TRUE),
         MnQ3 = quantile(mn55_ppm, probs=0.75, na.rm=TRUE),
         CuQ1 = quantile(cu63_ppm, probs=0.25, na.rm=TRUE),
         CuQ3 = quantile(cu63_ppm, probs=0.75, na.rm=TRUE),
         MgQ1 = quantile(mg26_ppm, probs=0.25, na.rm=TRUE),
         MgQ3 = quantile(mg26_ppm, probs=0.75, na.rm=TRUE),
         ZnQ1 = quantile(zn66_ppm, probs=0.25, na.rm=TRUE),
         ZnQ3 = quantile(zn66_ppm, probs=0.75, na.rm=TRUE),
         HgQ1 = quantile(hg202_ppm, probs=0.25, na.rm=TRUE),
         HgQ3 = quantile(hg202_ppm, probs=0.75, na.rm=TRUE),
         AlQ1 = quantile(al27_ppm, probs=0.25, na.rm=TRUE),
         AlQ3 = quantile(al27_ppm, probs=0.75, na.rm=TRUE),
         CrQ1 = quantile(cr52_ppm, probs=0.25, na.rm=TRUE),
         CrQ3 = quantile(cr52_ppm, probs=0.75, na.rm=TRUE),
         BaQ1 = quantile(ba137_ppm, probs=0.25, na.rm=TRUE),
         BaQ3 = quantile(ba137_ppm, probs=0.75, na.rm=TRUE),
         LiQ1 = quantile(li7_ppm, probs=0.25, na.rm=TRUE),
         LiQ3 = quantile(li7_ppm, probs=0.75, na.rm=TRUE),
         NaQ1 = quantile(na23_ppm, probs=0.25, na.rm=TRUE),
         NaQ3 = quantile(na23_ppm, probs=0.75, na.rm=TRUE),
         SrQ1 = quantile(sr88_ppm, probs=0.25, na.rm=TRUE),
         SrQ3 = quantile(sr88_ppm, probs=0.75, na.rm=TRUE),
         NiQ1 = quantile(ni60_ppm, probs=0.25, na.rm=TRUE),#low detection metals below
         NiQ3 = quantile(ni60_ppm, probs=0.75, na.rm=TRUE),
         FeQ1 = quantile(fe56_ppm, probs=0.25, na.rm=TRUE),
         FeQ3 = quantile(fe56_ppm, probs=0.75, na.rm=TRUE),
         AsQ1 = quantile(as75_ppm, probs=0.25, na.rm=TRUE),
         AsQ3 = quantile(as75_ppm, probs=0.75, na.rm=TRUE),
         CdQ1 = quantile(cd111_ppm, probs=0.25, na.rm=TRUE),
         CdQ3 = quantile(cd111_ppm, probs=0.75, na.rm=TRUE),
         SnQ1 = quantile(sn118_ppm, probs=0.25, na.rm=TRUE),
         SnQ3 = quantile(sn118_ppm, probs=0.75, na.rm=TRUE)) %>%
  as.data.frame()

dntIQR<- dntIQR %>% mutate(Pbout = case_when(pb208_ppm <= (PbQ1 - Pb15IQR) ~'1',
                                     pb208_ppm >= (PbQ3 + Pb15IQR) ~ '1',
                                     .default ='0')) 
dntIQR<- dntIQR %>% mutate(Mnout = case_when(mn55_ppm <= (MnQ1 - Mn15IQR) ~'1',
                                     mn55_ppm >= (MnQ3 + Mn15IQR) ~ '1',
                                     .default ='0')) 
dntIQR<- dntIQR %>% mutate(Cuout = case_when(cu63_ppm <= (CuQ1 - Cu15IQR) ~'1',
                                     cu63_ppm >= (CuQ3 + Cu15IQR) ~ '1',
                                     .default ='0')) 
dntIQR<- dntIQR %>% mutate(Mgout = case_when(mg26_ppm <= (MgQ1 - Mg15IQR) ~'1',
                                     mg26_ppm >= (MgQ3 + Mg15IQR) ~ '1',
                                     .default ='0')) 
dntIQR<- dntIQR %>% mutate(Znout = case_when(zn66_ppm <= (ZnQ1 - Zn15IQR) ~'1',
                                     zn66_ppm >= (ZnQ3 + Zn15IQR) ~ '1',
                                     .default ='0')) 
dntIQR<- dntIQR %>% mutate(Hgout = case_when(hg202_ppm <= (HgQ1 - Hg15IQR) ~'1',
                                     hg202_ppm >= (HgQ3 + Hg15IQR) ~ '1',
                                     .default ='0')) 
dntIQR<- dntIQR %>% mutate(Alout = case_when(al27_ppm <= (AlQ1 - Al15IQR) ~'1',
                                     al27_ppm >= (AlQ3 + Al15IQR) ~ '1',
                                     .default ='0')) 
dntIQR<- dntIQR %>% mutate(Crout = case_when(cr52_ppm <= (CrQ1 - Cr15IQR) ~'1',
                                     cr52_ppm >= (CrQ3 + Cr15IQR) ~ '1',
                                     .default ='0')) 
dntIQR<- dntIQR %>% mutate(Baout = case_when(ba137_ppm <= (BaQ1 - Ba15IQR) ~'1',
                                     ba137_ppm >= (BaQ3 + Ba15IQR) ~ '1',
                                     .default ='0')) 
dntIQR<- dntIQR %>% mutate(Liout = case_when(li7_ppm <= (LiQ1 - Li15IQR) ~'1',
                                     li7_ppm >= (LiQ3 + Li15IQR) ~ '1',
                                     .default ='0')) 
dntIQR<- dntIQR %>% mutate(Naout = case_when(na23_ppm <= (NaQ1 - Na15IQR) ~'1',
                                     na23_ppm >= (NaQ3 + Na15IQR) ~ '1',
                                     .default ='0')) 
dntIQR<- dntIQR %>% mutate(Seout = case_when(sr88_ppm <= (SrQ1 - Sr15IQR) ~'1',
                                     sr88_ppm >= (SrQ3 + Sr15IQR) ~ '1',
                                     .default ='0')) 
dntIQR<- dntIQR %>% mutate(Niout = case_when(ni60_ppm <= (NiQ1 - Ni15IQR) ~'1',
                                     ni60_ppm >= (NiQ3 + Ni15IQR) ~ '1',
                                     .default ='0')) 
dntIQR<- dntIQR %>% mutate(Feout = case_when(fe56_ppm <= (FeQ1 - Fe15IQR) ~'1',
                                     fe56_ppm >= (FeQ3 + Fe15IQR) ~ '1',
                                     .default ='0')) 
dntIQR<- dntIQR %>% mutate(Asout = case_when(as75_ppm <= (AsQ1 - As15IQR) ~'1',
                                     as75_ppm >= (AsQ3 + As15IQR) ~ '1',
                                     .default ='0')) 
dntIQR<- dntIQR %>% mutate(Cdout = case_when(cd111_ppm <= (CdQ1 - Cd15IQR) ~'1',
                                     cd111_ppm >= (CdQ3 + Cd15IQR) ~ '1',
                                     .default ='0')) 
dntIQR<- dntIQR %>% mutate(Snout = case_when(sn118_ppm <= (SnQ1 - Sn15IQR) ~'1',
                                     sn118_ppm >= (SnQ3 + Sn15IQR) ~ '1',
                                     .default ='0')) 

#make new dataset with column indicating potential 15IQR outlier
write.csv(dntIQR,file='/Volumes/shared/EOME/Weisskopf/Saint_Louis_Baby_Teeth_Study/Individuals/Joyce/Tooth data/tooth15IQR_verifieddnt.csv')

#summarize number of outliers
outsum<- dntIQR %>% 
  group_by(DNTpastENL) %>% 
 count(Pbout)



#Checks for 5IQR suitability and new means when subsetting to just DNTpastENL==0
# 5 IQR outlier identification instead ---------------------------------------------------
dnt<-subset(newdat_unique, DNTpastENL ==0)
dntIQR<-dnt %>% group_by(study_id.x) %>%
  mutate(Pb15IQR = 10*IQR(pb208_ppm, na.rm=TRUE),
         Mn15IQR = 10*IQR(mn55_ppm, na.rm=TRUE),
         Cu15IQR = 10*IQR(cu63_ppm, na.rm=TRUE),
         Mg15IQR = 10*IQR(mg26_ppm, na.rm=TRUE),
         Zn15IQR = 10*IQR(zn66_ppm, na.rm=TRUE),
         Hg15IQR = 10*IQR(hg202_ppm, na.rm=TRUE),
         Al15IQR = 10*IQR(al27_ppm, na.rm=TRUE),
         Cr15IQR = 10*IQR(cr52_ppm, na.rm=TRUE),
         Ba15IQR = 10*IQR(ba137_ppm, na.rm=TRUE),
         Li15IQR = 10*IQR(li7_ppm, na.rm=TRUE),
         Na15IQR = 10*IQR(na23_ppm, na.rm=TRUE),
         Sr15IQR = 10*IQR(sr88_ppm, na.rm=TRUE),#Low detect metals below
         Ni15IQR = 10*IQR(ni60_ppm, na.rm=TRUE),
         Fe15IQR = 10*IQR(fe56_ppm, na.rm=TRUE),
         As15IQR = 10*IQR(as75_ppm, na.rm=TRUE),
         Cd15IQR = 10*IQR(cd111_ppm, na.rm=TRUE),
         Sn15IQR = 10*IQR(sn118_ppm, na.rm=TRUE)) %>%
  as.data.frame()
length(unique(dntIQR$tooth_id))


dntIQR<-dntIQR %>% group_by(study_id.x) %>%
  mutate(PbQ1 = quantile(pb208_ppm, probs=0.25, na.rm=TRUE),
         PbQ3 = quantile(pb208_ppm, probs=0.75, na.rm=TRUE),
         MnQ1 = quantile(mn55_ppm, probs=0.25, na.rm=TRUE),
         MnQ3 = quantile(mn55_ppm, probs=0.75, na.rm=TRUE),
         CuQ1 = quantile(cu63_ppm, probs=0.25, na.rm=TRUE),
         CuQ3 = quantile(cu63_ppm, probs=0.75, na.rm=TRUE),
         MgQ1 = quantile(mg26_ppm, probs=0.25, na.rm=TRUE),
         MgQ3 = quantile(mg26_ppm, probs=0.75, na.rm=TRUE),
         ZnQ1 = quantile(zn66_ppm, probs=0.25, na.rm=TRUE),
         ZnQ3 = quantile(zn66_ppm, probs=0.75, na.rm=TRUE),
         HgQ1 = quantile(hg202_ppm, probs=0.25, na.rm=TRUE),
         HgQ3 = quantile(hg202_ppm, probs=0.75, na.rm=TRUE),
         AlQ1 = quantile(al27_ppm, probs=0.25, na.rm=TRUE),
         AlQ3 = quantile(al27_ppm, probs=0.75, na.rm=TRUE),
         CrQ1 = quantile(cr52_ppm, probs=0.25, na.rm=TRUE),
         CrQ3 = quantile(cr52_ppm, probs=0.75, na.rm=TRUE),
         BaQ1 = quantile(ba137_ppm, probs=0.25, na.rm=TRUE),
         BaQ3 = quantile(ba137_ppm, probs=0.75, na.rm=TRUE),
         LiQ1 = quantile(li7_ppm, probs=0.25, na.rm=TRUE),
         LiQ3 = quantile(li7_ppm, probs=0.75, na.rm=TRUE),
         NaQ1 = quantile(na23_ppm, probs=0.25, na.rm=TRUE),
         NaQ3 = quantile(na23_ppm, probs=0.75, na.rm=TRUE),
         SrQ1 = quantile(sr88_ppm, probs=0.25, na.rm=TRUE),
         SrQ3 = quantile(sr88_ppm, probs=0.75, na.rm=TRUE),
         NiQ1 = quantile(ni60_ppm, probs=0.25, na.rm=TRUE),#low detection metals below
         NiQ3 = quantile(ni60_ppm, probs=0.75, na.rm=TRUE),
         FeQ1 = quantile(fe56_ppm, probs=0.25, na.rm=TRUE),
         FeQ3 = quantile(fe56_ppm, probs=0.75, na.rm=TRUE),
         AsQ1 = quantile(as75_ppm, probs=0.25, na.rm=TRUE),
         AsQ3 = quantile(as75_ppm, probs=0.75, na.rm=TRUE),
         CdQ1 = quantile(cd111_ppm, probs=0.25, na.rm=TRUE),
         CdQ3 = quantile(cd111_ppm, probs=0.75, na.rm=TRUE),
         SnQ1 = quantile(sn118_ppm, probs=0.25, na.rm=TRUE),
         SnQ3 = quantile(sn118_ppm, probs=0.75, na.rm=TRUE)) %>%
  as.data.frame()

dntIQR<- dntIQR %>% mutate(Pbout = case_when(pb208_ppm <= (PbQ1 - Pb15IQR) ~'1',
                                             pb208_ppm >= (PbQ3 + Pb15IQR) ~ '1',
                                             .default ='0')) 
dntIQR<- dntIQR %>% mutate(Mnout = case_when(mn55_ppm <= (MnQ1 - Mn15IQR) ~'1',
                                             mn55_ppm >= (MnQ3 + Mn15IQR) ~ '1',
                                             .default ='0')) 
dntIQR<- dntIQR %>% mutate(Cuout = case_when(cu63_ppm <= (CuQ1 - Cu15IQR) ~'1',
                                             cu63_ppm >= (CuQ3 + Cu15IQR) ~ '1',
                                             .default ='0')) 
dntIQR<- dntIQR %>% mutate(Mgout = case_when(mg26_ppm <= (MgQ1 - Mg15IQR) ~'1',
                                             mg26_ppm >= (MgQ3 + Mg15IQR) ~ '1',
                                             .default ='0')) 
dntIQR<- dntIQR %>% mutate(Znout = case_when(zn66_ppm <= (ZnQ1 - Zn15IQR) ~'1',
                                             zn66_ppm >= (ZnQ3 + Zn15IQR) ~ '1',
                                             .default ='0')) 
dntIQR<- dntIQR %>% mutate(Hgout = case_when(hg202_ppm <= (HgQ1 - Hg15IQR) ~'1',
                                             hg202_ppm >= (HgQ3 + Hg15IQR) ~ '1',
                                             .default ='0')) 
dntIQR<- dntIQR %>% mutate(Alout = case_when(al27_ppm <= (AlQ1 - Al15IQR) ~'1',
                                             al27_ppm >= (AlQ3 + Al15IQR) ~ '1',
                                             .default ='0')) 
dntIQR<- dntIQR %>% mutate(Crout = case_when(cr52_ppm <= (CrQ1 - Cr15IQR) ~'1',
                                             cr52_ppm >= (CrQ3 + Cr15IQR) ~ '1',
                                             .default ='0')) 
dntIQR<- dntIQR %>% mutate(Baout = case_when(ba137_ppm <= (BaQ1 - Ba15IQR) ~'1',
                                             ba137_ppm >= (BaQ3 + Ba15IQR) ~ '1',
                                             .default ='0')) 
dntIQR<- dntIQR %>% mutate(Liout = case_when(li7_ppm <= (LiQ1 - Li15IQR) ~'1',
                                             li7_ppm >= (LiQ3 + Li15IQR) ~ '1',
                                             .default ='0')) 
dntIQR<- dntIQR %>% mutate(Naout = case_when(na23_ppm <= (NaQ1 - Na15IQR) ~'1',
                                             na23_ppm >= (NaQ3 + Na15IQR) ~ '1',
                                             .default ='0')) 
dntIQR<- dntIQR %>% mutate(Seout = case_when(sr88_ppm <= (SrQ1 - Sr15IQR) ~'1',
                                             sr88_ppm >= (SrQ3 + Sr15IQR) ~ '1',
                                             .default ='0')) 
dntIQR<- dntIQR %>% mutate(Niout = case_when(ni60_ppm <= (NiQ1 - Ni15IQR) ~'1',
                                             ni60_ppm >= (NiQ3 + Ni15IQR) ~ '1',
                                             .default ='0')) 
dntIQR<- dntIQR %>% mutate(Feout = case_when(fe56_ppm <= (FeQ1 - Fe15IQR) ~'1',
                                             fe56_ppm >= (FeQ3 + Fe15IQR) ~ '1',
                                             .default ='0')) 
dntIQR<- dntIQR %>% mutate(Asout = case_when(as75_ppm <= (AsQ1 - As15IQR) ~'1',
                                             as75_ppm >= (AsQ3 + As15IQR) ~ '1',
                                             .default ='0')) 
dntIQR<- dntIQR %>% mutate(Cdout = case_when(cd111_ppm <= (CdQ1 - Cd15IQR) ~'1',
                                             cd111_ppm >= (CdQ3 + Cd15IQR) ~ '1',
                                             .default ='0')) 
dntIQR<- dntIQR %>% mutate(Snout = case_when(sn118_ppm <= (SnQ1 - Sn15IQR) ~'1',
                                             sn118_ppm >= (SnQ3 + Sn15IQR) ~ '1',
                                             .default ='0')) 

#summarize number of outliers
outsum_<- dntIQR %>% 
  count(Pbout)


#new dataset with metal specific NAs where metal ext = 1
newdat<-dntIQR %>% mutate_at(vars(c("pb208_ppm")), funs(replace(., Pbout == 1, NA)))
newdat<-newdat %>% mutate_at(vars(c("mn55_ppm")), funs(replace(., Mnout == 1, NA)))
newdat<-newdat %>% mutate_at(vars(c("cu63_ppm")), funs(replace(., Cuout == 1, NA)))
newdat<-newdat %>% mutate_at(vars(c("mg26_ppm")), funs(replace(., Mgout == 1, NA)))
newdat<-newdat %>% mutate_at(vars(c("zn66_ppm")), funs(replace(., Znout == 1, NA)))
newdat<-newdat %>% mutate_at(vars(c("al27_ppm")), funs(replace(., Alout == 1, NA)))
newdat<-newdat %>% mutate_at(vars(c("hg202_ppm")), funs(replace(., Hgout == 1, NA)))
newdat<-newdat %>% mutate_at(vars(c("cr52_ppm")), funs(replace(., Crout == 1, NA)))
newdat<-newdat %>% mutate_at(vars(c("ba137_ppm")), funs(replace(., Baout == 1, NA)))
newdat<-newdat %>% mutate_at(vars(c("li7_ppm")), funs(replace(., Liout == 1, NA)))
newdat<-newdat %>% mutate_at(vars(c("na23_ppm")), funs(replace(., Naout == 1, NA)))
newdat<-newdat %>% mutate_at(vars(c("sr88_ppm")), funs(replace(., Srout == 1, NA)))
newdat<-newdat %>% mutate_at(vars(c("ni60_ppm")), funs(replace(., Niout == 1, NA)))
newdat<-newdat %>% mutate_at(vars(c("fe56_ppm")), funs(replace(., Feout == 1, NA)))
newdat<-newdat %>% mutate_at(vars(c("as75_ppm")), funs(replace(., Asout == 1, NA)))
newdat<-newdat %>% mutate_at(vars(c("cd111_ppm")), funs(replace(., Cdout == 1, NA)))
newdat<-newdat %>% mutate_at(vars(c("sn118_ppm")), funs(replace(., Snout == 1, NA)))

#interactive vis to check 15IQR outlier removal
newdat_outrem <- newdat[c('tooth_id', 'study_id.x', 'spot', 'batch', 'pb208_ppm', 'BATCH','BATCH_COUNT')]

p2<-newdat_outrem %>% 
  ggplot(., aes(x = spot, y = pb208_ppm)) + 
  geom_point() +
  geom_vline(xintercept = 0, linetype = "dotted") +
  facet_trelliscope(~study_id.x, ncol = 3, scales = "free") +
  theme_bw()
p2


# Create new ppm means using only dentin spots covered by enamel
mean_newdat<- newdat %>% group_by(study_id.x, DNTpastENL)%>% mutate(Pbmean_dnt = mean(pb208_ppm, na.rm=TRUE))
mean_newdat<- mean_newdat %>% group_by(study_id.x, DNTpastENL)%>% mutate(Znmean_dnt = mean(zn66_ppm, na.rm=TRUE))
mean_newdat<- mean_newdat %>% group_by(study_id.x, DNTpastENL)%>% mutate(Cumean_dnt = mean(cu63_ppm, na.rm=TRUE))

mean_newdat_<-mean_newdat[!duplicated(mean_newdat$study_id.x), ]

#where DNTpastENL=1 are dentin spots past enamel and DNTpastENL=0 dentin spots covered by enamel
#for tooth averaging, take average from only dentin spots covered by enamel

vDNT<-subset(mean_newdat, DNTpastENL ==0)
describe(vDNT$Pbmean_dnt)
length(unique(mean_newdat$tooth_id))


#check nl summary in new data
nldat<-newdat_unique%>% filter(nl_usability==1)
length(unique(nldat$tooth_id))#181 with NL from batch 4


# Vis various metals interactively
dnt15<-subset(newdat, DNTpastENL ==0)
length(unique(dnt15$tooth_id))
datt <- dnt15[c('tooth_id', 'study_id.x', 'spot', 'BATCH','BATCH_COUNT', 'pb208_ppm', 'pb208_ppm_lod_howell', 'pb208_ppm_int2se','zn66_ppm', 'zn66_ppm_lod_howell', 'zn66_ppm_int2se','cu63_ppm', 'cu63_ppm_lod_howell', 'cu63_ppm_int2se')    ]
p<-datt %>% 
  ggplot(., aes(x = spot, y = cu63_ppm)) + 
  geom_point() +
  geom_vline(xintercept = 0, linetype = "dotted") +
  facet_trelliscope(~study_id.x, ncol = 3, scales = "free") +
  theme_bw()
p

#Check which teeth have whole tooth negative concentrations


