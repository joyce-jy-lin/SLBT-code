## Outlier removal using IQR method
# 11/28/2023
library(data.table)
library(tidyverse)
library(ggplot2)
library(psych)
library(compareGroups)
library(glue)
library(Hmisc)

main<-fread("/Volumes/shared/EOME/Weisskopf/Saint_Louis_Baby_Teeth_Study/Individuals/Joyce/Tooth data/tooth_b1_4.csv")
metals <- c( "pb208_ppm", "mn55_ppm", "cu63_ppm", "zn66_ppm","mg26_ppm","al27_ppm","hg202_ppm",
             "as75_ppm","cd111_ppm", "cr53_ppm", "cr52_ppm","fe56_ppm","ba137_ppm","li7_ppm","na23_ppm","ni60_ppm","sn118_ppm","sr88_ppm")
#taking out low ca obs
main1<-main %>% mutate(across(c(metals), ~replace(., low_ca_r == "low", NA)))
#remove first two and last two spots (make NA)
#make NA if spot = 1 or 2
main1<-main1 %>% mutate(across(c(metals), ~replace(., spot == 1, NA)))
main1<-main1 %>% mutate(across(c(metals), ~replace(., spot == 2, NA)))
# replace with NA if highest or second highest spot value
main1<-main1 %>% group_by(study_id)%>% mutate(across(c(metals), ~replace(., spot == max(spot), NA)))
main1<-main1 %>% group_by(study_id)%>% mutate(across(c(metals), ~replace(., spot == max(spot)-1, NA)))

## correct two mis-entered study IDs in the tooth dataset 
main1$study_id_incorrect <- main1$study_id 
main1$tooth_id_incorrect <- main1$tooth_id 
main1$stl._incorrect <- main1$stl. 
main1 <- main1 %>% 
  mutate(study_id = if_else(study_id_incorrect == 194063, 213870, study_id), 
         study_id = if_else(study_id_incorrect == 121555, 212155, study_id),
         tooth_id = if_else(study_id_incorrect == 146080, 21877, tooth_id),
         stl. = if_else(study_id_incorrect == 146080, "246164", stl.))
length(unique(main1$study_id))
length(unique(main1$tooth_id))

#vis
p1<- main1 %>% ggplot(., aes(x=spot, y=hg202_ppm, label=tooth_id))+ geom_text()
p1

p2<- newdat %>% ggplot(., aes(x=spot, y=cu63_ppm, label=tooth_id))+ geom_text()
p2

Tf47644<-subset(main1, tooth_id ==47644)
p1<- Tf47644 %>% ggplot(., aes(x=spot, y=hg202_ppm))+ geom_point()
p1

T47644<-subset(newdat, tooth_id ==47644)
p2<- T47644 %>% ggplot(., aes(x=spot, y=hg202_ppm))+ geom_point()
p2

EDJ<-subset(T54759, group_name =="EDJ")
IQR(EDJ$pb208_ppm)

#IQR outlier identification using 1.5* IQR approach -----------
df<-main1 %>% group_by(tooth_id, group_name) %>%
  mutate(Pb1.5IQR = 1.5*IQR(pb208_ppm, na.rm=TRUE),
         Mn1.5IQR = 1.5*IQR(mn55_ppm, na.rm=TRUE),
         Cu1.5IQR = 1.5*IQR(cu63_ppm, na.rm=TRUE),
         Mg1.5IQR = 1.5*IQR(mg26_ppm, na.rm=TRUE),
         Zn1.5IQR = 1.5*IQR(zn66_ppm, na.rm=TRUE),
         Hg1.5IQR = 1.5*IQR(hg202_ppm, na.rm=TRUE),
         Al1.5IQR = 1.5*IQR(al27_ppm, na.rm=TRUE),
         Cr521.5IQR = 1.5*IQR(cr52_ppm, na.rm=TRUE),
         Cr531.5IQR = 1.5*IQR(cr53_ppm, na.rm=TRUE),
         Ba1.5IQR = 1.5*IQR(ba137_ppm, na.rm=TRUE),
         Li1.5IQR = 1.5*IQR(li7_ppm, na.rm=TRUE),
         Na1.5IQR = 1.5*IQR(na23_ppm, na.rm=TRUE),
         Sr1.5IQR = 1.5*IQR(sr88_ppm, na.rm=TRUE),#Low detect metals below
         Ni1.5IQR = 1.5*IQR(ni60_ppm, na.rm=TRUE),
         Fe1.5IQR = 1.5*IQR(fe56_ppm, na.rm=TRUE),
         As1.5IQR = 1.5*IQR(as75_ppm, na.rm=TRUE),
         Cd1.5IQR = 1.5*IQR(cd111_ppm, na.rm=TRUE),
         Sn1.5IQR = 1.5*IQR(sn118_ppm, na.rm=TRUE)) %>%
  as.data.frame()


df<-df %>% group_by(tooth_id, group_name) %>%
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
         Cr52Q1 = quantile(cr52_ppm, probs=0.25, na.rm=TRUE),
         Cr52Q3 = quantile(cr52_ppm, probs=0.75, na.rm=TRUE),
         Cr53Q1 = quantile(cr53_ppm, probs=0.25, na.rm=TRUE),
         Cr53Q3 = quantile(cr53_ppm, probs=0.75, na.rm=TRUE),
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

df<- df %>% mutate(Pbout = case_when(pb208_ppm <= (PbQ1 - Pb1.5IQR) ~'1',
                                      pb208_ppm >= (PbQ3 + Pb1.5IQR) ~ '1',
                                      .default ='0')) 
df<- df %>% mutate(Mnout = case_when(mn55_ppm <= (MnQ1 - Mn1.5IQR) ~'1',
                                     mn55_ppm >= (MnQ3 + Mn1.5IQR) ~ '1',
                                     .default ='0')) 
df<- df %>% mutate(Cuout = case_when(cu63_ppm <= (CuQ1 - Cu1.5IQR) ~'1',
                                     cu63_ppm >= (CuQ3 + Cu1.5IQR) ~ '1',
                                     .default ='0')) 
df<- df %>% mutate(Mgout = case_when(mg26_ppm <= (MgQ1 - Mg1.5IQR) ~'1',
                                     mg26_ppm >= (MgQ3 + Mg1.5IQR) ~ '1',
                                     .default ='0')) 
df<- df %>% mutate(Znout = case_when(zn66_ppm <= (ZnQ1 - Zn1.5IQR) ~'1',
                                     zn66_ppm >= (ZnQ3 + Zn1.5IQR) ~ '1',
                                     .default ='0')) 
df<- df %>% mutate(Hgout = case_when(hg202_ppm <= (HgQ1 - Hg1.5IQR) ~'1',
                                     hg202_ppm >= (HgQ3 + Hg1.5IQR) ~ '1',
                                     .default ='0')) 
df<- df %>% mutate(Alout = case_when(al27_ppm <= (AlQ1 - Al1.5IQR) ~'1',
                                     al27_ppm >= (AlQ3 + Al1.5IQR) ~ '1',
                                     .default ='0')) 
df<- df %>% mutate(Cr52out = case_when(cr52_ppm <= (Cr52Q1 - Cr521.5IQR) ~'1',
                                     cr52_ppm >= (Cr52Q3 + Cr521.5IQR) ~ '1',
                                     .default ='0')) 
df<- df %>% mutate(Cr53out = case_when(cr53_ppm <= (Cr53Q1 - Cr531.5IQR) ~'1',
                                     cr53_ppm >= (Cr53Q3 + Cr531.5IQR) ~ '1',
                                     .default ='0')) 
df<- df %>% mutate(Baout = case_when(ba137_ppm <= (BaQ1 - Ba1.5IQR) ~'1',
                                     ba137_ppm >= (BaQ3 + Ba1.5IQR) ~ '1',
                                     .default ='0')) 
df<- df %>% mutate(Liout = case_when(li7_ppm <= (LiQ1 - Li1.5IQR) ~'1',
                                     li7_ppm >= (LiQ3 + Li1.5IQR) ~ '1',
                                     .default ='0')) 
df<- df %>% mutate(Naout = case_when(na23_ppm <= (NaQ1 - Na1.5IQR) ~'1',
                                     na23_ppm >= (NaQ3 + Na1.5IQR) ~ '1',
                                     .default ='0')) 
df<- df %>% mutate(Seout = case_when(sr88_ppm <= (SrQ1 - Sr1.5IQR) ~'1',
                                     sr88_ppm >= (SrQ3 + Sr1.5IQR) ~ '1',
                                     .default ='0')) 
df<- df %>% mutate(Niout = case_when(ni60_ppm <= (NiQ1 - Ni1.5IQR) ~'1',
                                     ni60_ppm >= (NiQ3 + Ni1.5IQR) ~ '1',
                                     .default ='0')) 
df<- df %>% mutate(Feout = case_when(fe56_ppm <= (FeQ1 - Fe1.5IQR) ~'1',
                                     fe56_ppm >= (FeQ3 + Fe1.5IQR) ~ '1',
                                     .default ='0')) 
df<- df %>% mutate(Asout = case_when(as75_ppm <= (AsQ1 - As1.5IQR) ~'1',
                                     as75_ppm >= (AsQ3 + As1.5IQR) ~ '1',
                                     .default ='0')) 
df<- df %>% mutate(Cdout = case_when(cd111_ppm <= (CdQ1 - Cd1.5IQR) ~'1',
                                     cd111_ppm >= (CdQ3 + Cd1.5IQR) ~ '1',
                                     .default ='0')) 
df<- df %>% mutate(Snout = case_when(sn118_ppm <= (SnQ1 - Sn1.5IQR) ~'1',
                                     sn118_ppm >= (SnQ3 + Sn1.5IQR) ~ '1',
                                     .default ='0')) 

#check number of obs identified as outlier for each metal 
describe(df$Pbout)

#make new datasets with outlier values NA --------------------------------------------------
#1 create dataset with rows for which all metal ext = 1, observe spot numbers for these rows
#outliers<-subset(df, Pbout ==1 & Mnout ==1 & Cuout==1 & Mgout==1 & Znout==1 & Alout ==1 & Hgout==1 & Crout ==1)
#only 62 obs where all metals are marked as outliers, doenst appear to be first or last few spots per tooth

#new dataset with metal specific NAs where metal ext = 1
newdat<-df %>% mutate_at(vars(c("pb208_ppm")), funs(replace(., Pbout == 1, NA)))
newdat<-newdat %>% mutate_at(vars(c("mn55_ppm")), funs(replace(., Mnout == 1, NA)))
newdat<-newdat %>% mutate_at(vars(c("cu63_ppm")), funs(replace(., Cuout == 1, NA)))
newdat<-newdat %>% mutate_at(vars(c("mg26_ppm")), funs(replace(., Mgout == 1, NA)))
newdat<-newdat %>% mutate_at(vars(c("zn66_ppm")), funs(replace(., Znout == 1, NA)))
newdat<-newdat %>% mutate_at(vars(c("al27_ppm")), funs(replace(., Alout == 1, NA)))
newdat<-newdat %>% mutate_at(vars(c("hg202_ppm")), funs(replace(., Hgout == 1, NA)))
newdat<-newdat %>% mutate_at(vars(c("cr52_ppm")), funs(replace(., Cr52out == 1, NA)))
newdat<-newdat %>% mutate_at(vars(c("cr53_ppm")), funs(replace(., Cr53out == 1, NA)))
newdat<-newdat %>% mutate_at(vars(c("ba137_ppm")), funs(replace(., Baout == 1, NA)))
newdat<-newdat %>% mutate_at(vars(c("li7_ppm")), funs(replace(., Liout == 1, NA)))
newdat<-newdat %>% mutate_at(vars(c("na23_ppm")), funs(replace(., Naout == 1, NA)))
newdat<-newdat %>% mutate_at(vars(c("sr88_ppm")), funs(replace(., Srout == 1, NA)))
newdat<-newdat %>% mutate_at(vars(c("ni60_ppm")), funs(replace(., Niout == 1, NA)))
newdat<-newdat %>% mutate_at(vars(c("fe56_ppm")), funs(replace(., Feout == 1, NA)))
newdat<-newdat %>% mutate_at(vars(c("as75_ppm")), funs(replace(., Asout == 1, NA)))
newdat<-newdat %>% mutate_at(vars(c("cd111_ppm")), funs(replace(., Cdout == 1, NA)))
newdat<-newdat %>% mutate_at(vars(c("sn118_ppm")), funs(replace(., Snout == 1, NA)))


#manual check
noout<-subset(df, Hgout ==0)
describe(noout$hg202_ppm)

#plot
p1<- noout %>% ggplot(., aes(x=spot, y=hg202_ppm,label=tooth_id))+ geom_text()
p1 

T47644<- filter(noout, tooth_id ==47644)
p1<- T47644 %>% ggplot(., aes(x=spot, y=hg202_ppm))+ geom_point()
p1 


## Same for LOD to update tooth mean LODs -------------------------------------------------------------
#IQR outlier identification using 1.5* IQR approach 
newdat<-newdat %>% group_by(tooth_id, group_name) %>%
  mutate(lPb1.5IQR = 1.5*IQR(pb208_ppm_lod_howell, na.rm=TRUE),
         lMn1.5IQR = 1.5*IQR(mn55_ppm_lod_howell, na.rm=TRUE),
         lCu1.5IQR = 1.5*IQR(cu63_ppm_lod_howell, na.rm=TRUE),
         lMg1.5IQR = 1.5*IQR(mg26_ppm_lod_howell, na.rm=TRUE),
         lZn1.5IQR = 1.5*IQR(zn66_ppm_lod_howell, na.rm=TRUE),
         lHg1.5IQR = 1.5*IQR(hg202_ppm_lod_howell, na.rm=TRUE),
         lAl1.5IQR = 1.5*IQR(al27_ppm_lod_howell, na.rm=TRUE),
         lCr521.5IQR = 1.5*IQR(cr52_ppm_lod_howell, na.rm=TRUE),
         lCr531.5IQR = 1.5*IQR(cr53_ppm_lod_howell, na.rm=TRUE),
         lBa1.5IQR = 1.5*IQR(ba137_ppm_lod_howell, na.rm=TRUE),
         lLi1.5IQR = 1.5*IQR(li7_ppm_lod_howell, na.rm=TRUE),
         lNa1.5IQR = 1.5*IQR(na23_ppm_lod_howell, na.rm=TRUE),
         lSr1.5IQR = 1.5*IQR(sr88_ppm_lod_howell, na.rm=TRUE),#Low detect metals below
         lNi1.5IQR = 1.5*IQR(ni60_ppm_lod_howell, na.rm=TRUE),
         lFe1.5IQR = 1.5*IQR(fe56_ppm_lod_howell, na.rm=TRUE),
         lAs1.5IQR = 1.5*IQR(as75_ppm_lod_howell, na.rm=TRUE),
         lCd1.5IQR = 1.5*IQR(cd111_ppm_lod_howell, na.rm=TRUE),
         lSn1.5IQR = 1.5*IQR(sn118_ppm_lod_howell, na.rm=TRUE)) %>%
  as.data.frame()


newdat<-newdat %>% group_by(tooth_id, group_name) %>%
  mutate(lPbQ1 = quantile(pb208_ppm_lod_howell, probs=0.25, na.rm=TRUE),
         lPbQ3 = quantile(pb208_ppm_lod_howell, probs=0.75, na.rm=TRUE),
         lMnQ1 = quantile(mn55_ppm_lod_howell, probs=0.25, na.rm=TRUE),
         lMnQ3 = quantile(mn55_ppm_lod_howell, probs=0.75, na.rm=TRUE),
         lCuQ1 = quantile(cu63_ppm_lod_howell, probs=0.25, na.rm=TRUE),
         lCuQ3 = quantile(cu63_ppm_lod_howell, probs=0.75, na.rm=TRUE),
         lMgQ1 = quantile(mg26_ppm_lod_howell, probs=0.25, na.rm=TRUE),
         lMgQ3 = quantile(mg26_ppm_lod_howell, probs=0.75, na.rm=TRUE),
         lZnQ1 = quantile(zn66_ppm_lod_howell, probs=0.25, na.rm=TRUE),
         lZnQ3 = quantile(zn66_ppm_lod_howell, probs=0.75, na.rm=TRUE),
         lHgQ1 = quantile(hg202_ppm_lod_howell, probs=0.25, na.rm=TRUE),
         lHgQ3 = quantile(hg202_ppm_lod_howell, probs=0.75, na.rm=TRUE),
         lAlQ1 = quantile(al27_ppm_lod_howell, probs=0.25, na.rm=TRUE),
         lAlQ3 = quantile(al27_ppm_lod_howell, probs=0.75, na.rm=TRUE),
         lCr52Q1 = quantile(cr52_ppm_lod_howell, probs=0.25, na.rm=TRUE),
         lCr52Q3 = quantile(cr52_ppm_lod_howell, probs=0.75, na.rm=TRUE),
         lCr53Q1 = quantile(cr53_ppm_lod_howell, probs=0.25, na.rm=TRUE),
         lCr53Q3 = quantile(cr53_ppm_lod_howell, probs=0.75, na.rm=TRUE),
         lBaQ1 = quantile(ba137_ppm_lod_howell, probs=0.25, na.rm=TRUE),
         lBaQ3 = quantile(ba137_ppm_lod_howell, probs=0.75, na.rm=TRUE),
         lLiQ1 = quantile(li7_ppm_lod_howell, probs=0.25, na.rm=TRUE),
         lLiQ3 = quantile(li7_ppm_lod_howell, probs=0.75, na.rm=TRUE),
         lNaQ1 = quantile(na23_ppm_lod_howell, probs=0.25, na.rm=TRUE),
         lNaQ3 = quantile(na23_ppm_lod_howell, probs=0.75, na.rm=TRUE),
         lSrQ1 = quantile(sr88_ppm_lod_howell, probs=0.25, na.rm=TRUE),
         lSrQ3 = quantile(sr88_ppm_lod_howell, probs=0.75, na.rm=TRUE),
         lNiQ1 = quantile(ni60_ppm_lod_howell, probs=0.25, na.rm=TRUE),#low detection metals below
         lNiQ3 = quantile(ni60_ppm_lod_howell, probs=0.75, na.rm=TRUE),
         lFeQ1 = quantile(fe56_ppm_lod_howell, probs=0.25, na.rm=TRUE),
         lFeQ3 = quantile(fe56_ppm_lod_howell, probs=0.75, na.rm=TRUE),
         lAsQ1 = quantile(as75_ppm_lod_howell, probs=0.25, na.rm=TRUE),
         lAsQ3 = quantile(as75_ppm_lod_howell, probs=0.75, na.rm=TRUE),
         lCdQ1 = quantile(cd111_ppm_lod_howell, probs=0.25, na.rm=TRUE),
         lCdQ3 = quantile(cd111_ppm_lod_howell, probs=0.75, na.rm=TRUE),
         lSnQ1 = quantile(sn118_ppm_lod_howell, probs=0.25, na.rm=TRUE),
         lSnQ3 = quantile(sn118_ppm_lod_howell, probs=0.75, na.rm=TRUE)) %>%
  as.data.frame()

newdat<- newdat %>% mutate(lPbout = case_when(pb208_ppm_lod_howell <= (lPbQ1 - lPb1.5IQR) ~'1',
                                     pb208_ppm_lod_howell >= (lPbQ3 + lPb1.5IQR) ~ '1',
                                     .default ='0')) 
newdat<- newdat %>% mutate(lMnout = case_when(mn55_ppm_lod_howell <= (lMnQ1 - lMn1.5IQR) ~'1',
                                     mn55_ppm_lod_howell >= (lMnQ3 + lMn1.5IQR) ~ '1',
                                     .default ='0')) 
newdat<- newdat %>% mutate(lCuout = case_when(cu63_ppm_lod_howell <= (lCuQ1 - lCu1.5IQR) ~'1',
                                     cu63_ppm_lod_howell >= (lCuQ3 + lCu1.5IQR) ~ '1',
                                     .default ='0')) 
newdat<- newdat %>% mutate(lMgout = case_when(mg26_ppm_lod_howell <= (lMgQ1 - lMg1.5IQR) ~'1',
                                     mg26_ppm_lod_howell >= (lMgQ3 + lMg1.5IQR) ~ '1',
                                     .default ='0')) 
newdat<- newdat %>% mutate(lZnout = case_when(zn66_ppm_lod_howell <= (lZnQ1 - lZn1.5IQR) ~'1',
                                     zn66_ppm_lod_howell >= (lZnQ3 + lZn1.5IQR) ~ '1',
                                     .default ='0')) 
newdat<- newdat %>% mutate(lHgout = case_when(hg202_ppm_lod_howell <= (lHgQ1 - lHg1.5IQR) ~'1',
                                     hg202_ppm_lod_howell >= (lHgQ3 + lHg1.5IQR) ~ '1',
                                     .default ='0')) 
newdat<- newdat %>% mutate(lAlout = case_when(al27_ppm_lod_howell <= (lAlQ1 - lAl1.5IQR) ~'1',
                                     al27_ppm_lod_howell >= (lAlQ3 + lAl1.5IQR) ~ '1',
                                     .default ='0')) 
newdat<- newdat %>% mutate(lCr52out = case_when(cr52_ppm_lod_howell <= (lCr52Q1 - lCr521.5IQR) ~'1',
                                     cr52_ppm_lod_howell >= (lCr52Q3 + lCr521.5IQR) ~ '1',
                                     .default ='0')) 
newdat<- newdat %>% mutate(lCr53out = case_when(cr53_ppm_lod_howell <= (lCr53Q1 - lCr531.5IQR) ~'1',
                                              cr53_ppm_lod_howell >= (lCr53Q3 + lCr531.5IQR) ~ '1',
                                              .default ='0')) 
newdat<- newdat %>% mutate(lBaout = case_when(ba137_ppm_lod_howell <= (lBaQ1 - lBa1.5IQR) ~'1',
                                     ba137_ppm_lod_howell >= (lBaQ3 + lBa1.5IQR) ~ '1',
                                     .default ='0')) 
newdat<- newdat %>% mutate(lLiout = case_when(li7_ppm_lod_howell <= (lLiQ1 - lLi1.5IQR) ~'1',
                                     li7_ppm_lod_howell >= (lLiQ3 + lLi1.5IQR) ~ '1',
                                     .default ='0')) 
newdat<- newdat %>% mutate(lNaout = case_when(na23_ppm_lod_howell <= (lNaQ1 - lNa1.5IQR) ~'1',
                                     na23_ppm_lod_howell >= (lNaQ3 + lNa1.5IQR) ~ '1',
                                     .default ='0')) 
newdat<- newdat %>% mutate(lSrout = case_when(sr88_ppm_lod_howell <= (lSrQ1 - lSr1.5IQR) ~'1',
                                     sr88_ppm_lod_howell >= (lSrQ3 + lSr1.5IQR) ~ '1',
                                     .default ='0')) 
newdat<- newdat %>% mutate(lNiout = case_when(ni60_ppm_lod_howell <= (lNiQ1 - lNi1.5IQR) ~'1',
                                     ni60_ppm_lod_howell >= (lNiQ3 + lNi1.5IQR) ~ '1',
                                     .default ='0')) 
newdat<- newdat %>% mutate(lFeout = case_when(fe56_ppm_lod_howell <= (lFeQ1 - lFe1.5IQR) ~'1',
                                     fe56_ppm_lod_howell >= (lFeQ3 + lFe1.5IQR) ~ '1',
                                     .default ='0')) 
newdat<- newdat %>% mutate(lAsout = case_when(as75_ppm_lod_howell <= (lAsQ1 - lAs1.5IQR) ~'1',
                                     as75_ppm_lod_howell >= (lAsQ3 + lAs1.5IQR) ~ '1',
                                     .default ='0')) 
newdat<- newdat %>% mutate(lCdout = case_when(cd111_ppm_lod_howell <= (lCdQ1 - lCd1.5IQR) ~'1',
                                     cd111_ppm_lod_howell >= (lCdQ3 + lCd1.5IQR) ~ '1',
                                     .default ='0')) 
newdat<- newdat %>% mutate(lSnout = case_when(sn118_ppm_lod_howell <= (lSnQ1 - lSn1.5IQR) ~'1',
                                     sn118_ppm_lod_howell >= (lSnQ3 + lSn1.5IQR) ~ '1',
                                     .default ='0')) 

#check number of obs identified as outlier for each metal 
describe(newdat$Pbout)

#make new datasets with outlier values NA --------------------------------------------------
#new columns with new tooth mean LODs
newdat<- newdat %>% mutate( PbLOD = pb208_ppm_lod_howell,
                            MnLOD = mn55_ppm_lod_howell,
                            CuLOD = cu63_ppm_lod_howell,
                            MgLOD = mg26_ppm_lod_howell,
                            ZnLOD = zn66_ppm_lod_howell,
                            HgLOD = hg202_ppm_lod_howell,
                            AlLOD = al27_ppm_lod_howell,
                            Cr52LOD = cr52_ppm_lod_howell,
                            Cr53LOD = cr53_ppm_lod_howell,
                            BaLOD = ba137_ppm_lod_howell,
                            LiLOD = li7_ppm_lod_howell,
                            NaLOD = na23_ppm_lod_howell,
                            SrLOD = sr88_ppm_lod_howell,
                            NiLOD = ni60_ppm_lod_howell,
                            FeLOD = fe56_ppm_lod_howell,
                            AsLOD = as75_ppm_lod_howell,
                            CdLOD = cd111_ppm_lod_howell,
                            SnLOD = sn118_ppm_lod_howell)

newdat<-newdat %>% mutate_at(vars(c("PbLOD")), funs(replace(., lPbout == 1, NA)))
newdat<-newdat %>% mutate_at(vars(c("MnLOD")), funs(replace(., lMnout == 1, NA)))
newdat<-newdat %>% mutate_at(vars(c("CuLOD")), funs(replace(., lCuout == 1, NA)))
newdat<-newdat %>% mutate_at(vars(c("MgLOD")), funs(replace(., lMgout == 1, NA)))
newdat<-newdat %>% mutate_at(vars(c("ZnLOD")), funs(replace(., lZnout == 1, NA)))
newdat<-newdat %>% mutate_at(vars(c("AlLOD")), funs(replace(., lAlout == 1, NA)))
newdat<-newdat %>% mutate_at(vars(c("HgLOD")), funs(replace(., lHgout == 1, NA)))
newdat<-newdat %>% mutate_at(vars(c("Cr52LOD")), funs(replace(., lCr52out == 1, NA)))
newdat<-newdat %>% mutate_at(vars(c("Cr53LOD")), funs(replace(., lCr53out == 1, NA)))
newdat<-newdat %>% mutate_at(vars(c("BaLOD")), funs(replace(., lBaout == 1, NA)))
newdat<-newdat %>% mutate_at(vars(c("LiLOD")), funs(replace(., lLiout == 1, NA)))
newdat<-newdat %>% mutate_at(vars(c("NaLOD")), funs(replace(., lNaout == 1, NA)))
newdat<-newdat %>% mutate_at(vars(c("SrLOD")), funs(replace(., lSrout == 1, NA)))
newdat<-newdat %>% mutate_at(vars(c("NiLOD")), funs(replace(., lNiout == 1, NA)))
newdat<-newdat %>% mutate_at(vars(c("FeLOD")), funs(replace(., lFeout == 1, NA)))
newdat<-newdat %>% mutate_at(vars(c("AsLOD")), funs(replace(., lAsout == 1, NA)))
newdat<-newdat %>% mutate_at(vars(c("CdLOD")), funs(replace(., lCdout == 1, NA)))
newdat<-newdat %>% mutate_at(vars(c("SnLOD")), funs(replace(., lSnout == 1, NA)))


## mean LOD
newdat<- newdat %>% group_by(tooth_id, group_name)%>% mutate(PbLODmean = mean(PbLOD, na.rm=TRUE),
                                                             MnLODmean = mean(MnLOD, na.rm=TRUE),
                                                             CuLODmean = mean(CuLOD, na.rm=TRUE),
                                                             MgLODmean = mean(MgLOD, na.rm=TRUE),
                                                             ZnLODmean = mean(ZnLOD, na.rm=TRUE),
                                                             HgLODmean = mean(HgLOD, na.rm=TRUE),
                                                             AlLODmean = mean(AlLOD, na.rm=TRUE),
                                                             Cr52LODmean = mean(Cr52LOD, na.rm=TRUE),
                                                             Cr53LODmean = mean(Cr53LOD, na.rm=TRUE),
                                                             BaLODmean = mean(BaLOD, na.rm=TRUE),
                                                             LiLODmean = mean(LiLOD, na.rm=TRUE),
                                                             NaLODmean = mean(NaLOD, na.rm=TRUE),
                                                             SrLODmean = mean(SrLOD, na.rm=TRUE),
                                                             NiLODmean = mean(NiLOD, na.rm=TRUE),
                                                             FeLODmean = mean(FeLOD, na.rm=TRUE),
                                                             AsLODmean = mean(AsLOD, na.rm=TRUE),
                                                             CdLODmean = mean(CdLOD, na.rm=TRUE),
                                                             SnLODmean = mean(SnLOD, na.rm=TRUE))



newdat<- newdat %>% mutate(pb_belowLOD = case_when(pb208_ppm <= (PbLODmean) ~'1',
                                                        .default ='0')) 
newdat<- newdat %>% mutate(mn_belowLOD = case_when(mn55_ppm <= (MnLODmean) ~'1',
                                                           .default ='0')) 
newdat<- newdat %>% mutate(al_belowLOD = case_when(al27_ppm <= (AlLODmean) ~'1',
                                                           .default ='0')) 
newdat<- newdat %>% mutate(as_belowLOD = case_when(as75_ppm <= (AsLODmean) ~'1',
                                                           .default ='0')) 
newdat<- newdat %>% mutate(cd_belowLOD = case_when(cd111_ppm <= (CdLODmean) ~'1',
                                                           .default ='0')) 
newdat<- newdat %>% mutate(cr52_belowLOD = case_when(cr52_ppm <= (Cr52LODmean) ~'1',
                                                             .default ='0')) 
newdat<- newdat %>% mutate(cr53_belowLOD = case_when(cr53_ppm <= (Cr53LODmean) ~'1',
                                                             .default ='0')) 
newdat<- newdat %>% mutate(cu_belowLOD = case_when(cu63_ppm <= (CuLODmean) ~'1',
                                                           .default ='0')) 
newdat<- newdat %>% mutate(fe_belowLOD = case_when(fe56_ppm <= (FeLODmean) ~'1',
                                                           .default ='0')) 
newdat<- newdat %>% mutate(hg_belowLOD = case_when(hg202_ppm <= (HgLODmean) ~'1',
                                                           .default ='0')) 
newdat<- newdat %>% mutate(mg_belowLOD = case_when(mg26_ppm <= (MgLODmean) ~'1',
                                                           .default ='0')) 
newdat<- newdat %>% mutate(ba_belowLOD = case_when(ba137_ppm <= (BaLODmean) ~'1',
                                                           .default ='0')) 
newdat<- newdat %>% mutate(li_belowLOD = case_when(li7_ppm <= (LiLODmean) ~'1',
                                                           .default ='0')) 
newdat<- newdat %>% mutate(na_belowLOD = case_when(na23_ppm <= (NaLODmean) ~'1',
                                                           .default ='0')) 
newdat<- newdat %>% mutate(ni_belowLOD = case_when(ni60_ppm <= (NiLODmean) ~'1',
                                                           .default ='0')) 
newdat<- newdat %>% mutate(sn_belowLOD = case_when(sn118_ppm <= (SnLODmean) ~'1',
                                                           .default ='0')) 
newdat<- newdat %>% mutate(sr_belowLOD = case_when(sr88_ppm <= (SrLODmean) ~'1',
                                                           .default ='0')) 
newdat<- newdat %>% mutate(zn_belowLOD = case_when(zn66_ppm <= (ZnLODmean) ~'1',
                                                           .default ='0')) 

write.csv(newdat,file='/Volumes/shared/EOME/Weisskopf/Saint_Louis_Baby_Teeth_Study/Individuals/Joyce/tooth_15IQRremoved.csv')


#add column for whole tooth mean ppm
newdat<- newdat %>% group_by(tooth_id, group_name)%>% mutate(Pbmean = mean(pb208_ppm, na.rm=TRUE),
                                                             Mnmean = mean(mn55_ppm, na.rm=TRUE),
                                                             Cumean = mean(cu63_ppm, na.rm=TRUE),
                                                             Mgmean = mean(mg26_ppm, na.rm=TRUE),
                                                             Znmean = mean(zn66_ppm, na.rm=TRUE),
                                                             Hgmean = mean(hg202_ppm, na.rm=TRUE),
                                                             Almean = mean(al27_ppm, na.rm=TRUE),
                                                             Cr52mean = mean(cr52_ppm, na.rm=TRUE),
                                                             Cr53mean = mean(cr53_ppm, na.rm=TRUE),
                                                             Bamean = mean(ba137_ppm, na.rm=TRUE),
                                                             Limean = mean(li7_ppm, na.rm=TRUE),
                                                             Namean = mean(na23_ppm, na.rm=TRUE),
                                                             Srmean = mean(sr88_ppm, na.rm=TRUE),
                                                             Nimean = mean(ni60_ppm, na.rm=TRUE),
                                                             Femean = mean(fe56_ppm, na.rm=TRUE),
                                                             Asmean = mean(as75_ppm, na.rm=TRUE),
                                                             Cdmean = mean(cd111_ppm, na.rm=TRUE),
                                                             Snmean = mean(sn118_ppm, na.rm=TRUE))


# create new dataset with only relevant columns
cleaned <- newdat[c("start_time_clean","tooth_id","group_name", "study_id","stl.", "def","batch", "sample","spot","ca43_cps",
                    "pb208_ppm","Pbmean", "PbLOD","PbLODmean", "pb_belowLOD", "mn55_ppm","Mnmean", "MnLOD","MnLODmean", "mn_belowLOD", "cu63_ppm","Cumean", "CuLOD","CuLODmean", "cu_belowLOD",
                    "mg26_ppm","Mgmean", "MgLOD","MgLODmean", "mg_belowLOD", "zn66_ppm","Znmean", "ZnLOD","ZnLODmean", "zn_belowLOD", "hg202_ppm","Hgmean", "HgLOD","HgLODmean", "hg_belowLOD",
                    "al27_ppm","Almean","AlLOD","AlLODmean", "al_belowLOD","cr52_ppm","Cr52mean","Cr52LOD","Cr52LODmean", "cr52_belowLOD","cr53_ppm","Cr53mean","Cr53LOD","Cr53LODmean", "cr53_belowLOD", "ba137_ppm","Bamean", "BaLOD","BaLODmean", "ba_belowLOD",
                    "li7_ppm","Limean", "LiLOD","LiLODmean", "li_belowLOD", "na23_ppm","Namean", "NaLOD","NaLODmean", "na_belowLOD","sr88_ppm","Srmean", "SrLOD","SrLODmean", "sr_belowLOD",
                    "ni60_ppm","Nimean","NiLOD","NiLODmean", "ni_belowLOD", "fe56_ppm","Femean","FeLOD","FeLODmean", "fe_belowLOD", "as75_ppm","Asmean","AsLOD","AsLODmean", "as_belowLOD",
                    "cd111_ppm","Cdmean", "CdLOD","CdLODmean", "cd_belowLOD", "sn118_ppm", "Snmean", "SnLOD","SnLODmean", "sn_belowLOD")]


write.csv(cleaned,file='/Volumes/shared/EOME/Weisskopf/Saint_Louis_Baby_Teeth_Study/Individuals/Joyce/Tooth data/tooth_15IQRclean.csv')


# Tooth average below spot 75 ------------------------
#create new dataset with tooth averaging only up to spot 75
cleaned75<-cleaned %>% mutate_at(vars(c("pb208_ppm")), funs(replace(., spot >= 75, NA)))
cleaned75<-cleaned75 %>% mutate_at(vars(c("mn55_ppm")), funs(replace(., spot >= 75, NA)))
cleaned75<-cleaned75 %>% mutate_at(vars(c("cu63_ppm")), funs(replace(., spot >= 75, NA)))
cleaned75<-cleaned75 %>% mutate_at(vars(c("mg26_ppm")), funs(replace(., spot >= 75, NA)))
cleaned75<-cleaned75 %>% mutate_at(vars(c("zn66_ppm")), funs(replace(., spot >= 75, NA)))
cleaned75<-cleaned75 %>% mutate_at(vars(c("hg202_ppm")), funs(replace(., spot >= 75, NA)))
cleaned75<-cleaned75 %>% mutate_at(vars(c("al27_ppm")), funs(replace(., spot >= 75, NA)))
cleaned75<-cleaned75 %>% mutate_at(vars(c("cr52_ppm")), funs(replace(., spot >= 75, NA)))
cleaned75<-cleaned75 %>% mutate_at(vars(c("cr53_ppm")), funs(replace(., spot >= 75, NA)))
cleaned75<-cleaned75 %>% mutate_at(vars(c("ba137_ppm")), funs(replace(., spot >= 75, NA)))
cleaned75<-cleaned75 %>% mutate_at(vars(c("li7_ppm")), funs(replace(., spot >= 75, NA)))
cleaned75<-cleaned75 %>% mutate_at(vars(c("na23_ppm")), funs(replace(., spot >= 75, NA)))
cleaned75<-cleaned75 %>% mutate_at(vars(c("sr88_ppm")), funs(replace(., spot >= 75, NA)))
cleaned75<-cleaned75 %>% mutate_at(vars(c("ni60_ppm")), funs(replace(., spot >= 75, NA)))
cleaned75<-cleaned75 %>% mutate_at(vars(c("fe56_ppm")), funs(replace(., spot >= 75, NA)))
cleaned75<-cleaned75 %>% mutate_at(vars(c("as75_ppm")), funs(replace(., spot >= 75, NA)))
cleaned75<-cleaned75 %>% mutate_at(vars(c("cd111_ppm")), funs(replace(., spot >= 75, NA)))
cleaned75<-cleaned75 %>% mutate_at(vars(c("sn118_ppm")), funs(replace(., spot >= 75, NA)))

cleaned75<- cleaned75 %>% group_by(tooth_id, group_name)%>% mutate(Pbmean75 = mean(pb208_ppm, na.rm=TRUE),
                                                                   Mnmean75 = mean(mn55_ppm, na.rm=TRUE),
                                                                   Cumean75 = mean(cu63_ppm, na.rm=TRUE),
                                                                   Mgmean75 = mean(mg26_ppm, na.rm=TRUE),
                                                                   Znmean75 = mean(zn66_ppm, na.rm=TRUE),
                                                                   Hgmean75 = mean(hg202_ppm, na.rm=TRUE),
                                                                   Almean75 = mean(al27_ppm, na.rm=TRUE),
                                                                   Cr52mean75 = mean(cr52_ppm, na.rm=TRUE),
                                                                   Cr53mean75 = mean(cr53_ppm, na.rm=TRUE),
                                                                   Bamean75 = mean(ba137_ppm, na.rm=TRUE),
                                                                   Limean75 = mean(li7_ppm, na.rm=TRUE),
                                                                   Namean75 = mean(na23_ppm, na.rm=TRUE),
                                                                   Srmean75 = mean(sr88_ppm, na.rm=TRUE),
                                                                   Nimean75 = mean(ni60_ppm, na.rm=TRUE),
                                                                   Femean75 = mean(fe56_ppm, na.rm=TRUE),
                                                                   Asmean75 = mean(as75_ppm, na.rm=TRUE),
                                                                   Cdmean75 = mean(cd111_ppm, na.rm=TRUE),
                                                                   Snmean75 = mean(sn118_ppm, na.rm=TRUE))

length(unique(cleaned75$Pbmean75))
cleaned75_<-subset(cleaned75, group_name== "EDJ")
length(unique(cleaned75_$Pbmean75))

write.csv(cleaned75,file='/Volumes/shared/EOME/Weisskopf/Saint_Louis_Baby_Teeth_Study/Individuals/Joyce/Tooth data/tooth_15IQRclean_75.csv')

############# ------------------------------------------

main_ <-main %>% group_by(group_name, tooth_id, batch) %>% 
  summarise_at(.vars = vars(hg202_ppm, pb208_ppm, mn55_ppm, al27_ppm, as75_ppm, ba137_ppm, cd111_ppm, cr52_ppm, cu63_ppm, fe56_ppm, hg202_ppm, li7_ppm, mg26_ppm, na23_ppm, ni60_ppm, sn118_ppm, sr88_ppm, zn66_ppm),
               .funs = c(mean="mean"), na.rm=TRUE)

cleaned_ <-cleaned %>% group_by(group_name, tooth_id, batch) %>% 
  summarise_at(.vars = vars(hg202_ppm, pb208_ppm, mn55_ppm, al27_ppm, as75_ppm, ba137_ppm, cd111_ppm, cr52_ppm, cu63_ppm, fe56_ppm, hg202_ppm, li7_ppm, mg26_ppm, na23_ppm, ni60_ppm, sn118_ppm, sr88_ppm, zn66_ppm),
               .funs = c(mean="mean"), na.rm=TRUE)

cleaned_$batch<- as.factor(cleaned_$batch)
main_$batch<- as.factor(main_$batch)

p1<-ggplot(cleaned_, aes(x=group_name, y=cu63_ppm_mean, fill=batch)) + 
  geom_boxplot()
p1

p1<-ggplot(main_, aes(x=group_name, y=hg202_ppm_mean, fill=batch)) + 
  geom_boxplot()
p1

p1<-ggplot(cleaned_, aes(x=group_name, y=al27_ppm_mean, fill=batch)) + 
  geom_boxplot()
p1

cleaned_avg <-dat %>% group_by(group_name, tooth_id, batch) %>% 
  summarise_at(.vars = vars(hg202_ppm, pb208_ppm, mn55_ppm, al27_ppm, as75_ppm, ba137_ppm, cd111_ppm, cr52_ppm, cu63_ppm, fe56_ppm, hg202_ppm, li7_ppm, mg26_ppm, na23_ppm, ni60_ppm, sn118_ppm, sr88_ppm, zn66_ppm),
               .funs = c(mean="mean"), na.rm=TRUE)


# check
length(unique(cleaned$study_id))
length(unique(cleaned$tooth_id))
describe(cleaned$study_id)

