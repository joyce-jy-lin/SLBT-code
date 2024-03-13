# RPHP dedup by date

library(readxl)
library(tidyverse)
library(Hmisc)

rphp <- Hmiscrphp <- read_excel("/Volumes/shared/EOME/Weisskopf/Saint_Louis_Baby_Teeth_Study/Individuals/Joyce/RPHP dedup/RPHP9_allIDs_02.01.2023.xlsx", sheet ="r")

#renaming vars
rphp_dat <- rename(rphp, 
                   "stl.num" = `St. Louis Number`,
                   "last_name" = `Last Name`,	
                   "first_name" = `First Name`,
                   "middle_initial" = `Middle Initial`,	
                   "parent_name" = `Parent Name`,
                   "street_add" = `Street Address`,
                   "city"= `City` ,	
                   "state" = `State`,
                   "zip" = `Zip`,
                   "sex" = `Sex`,
                   "mother_carried" = `Where Mother Carried`,
                   "first_year_live" = `Where Lived First Year`,
                   "birth_year" = `Birth Year`,
                   "birth_month" = `Birth Month`,
                   "birth_day" = `Birth Date`,
                   "year_lost"= `Year Lost`)

rphp_dup <- rphp_dat[duplicated(rphp_dat[ , c("last_name","middle_initial","first_name","parent_name","street_add","birth_year","birth_day","birth_month")]), ]
rphp_dedup <- rphp_dat[!duplicated(rphp_dat[ , c("last_name","middle_initial","first_name","parent_name","street_add","birth_year","birth_day","birth_month")]), ]

##birth month
rphp_dedup$birth_month<-as.factor(rphp_dedup$birth_month)
rphp_dedup <- rphp_dedup %>%
  mutate(birth_month = fct_collapse(birth_month,
                                    NULL = c("X", "!","M","F","19","18","101","121"),
                                    "1" = c("1"),
                                    "2" = c("2"),
                                    "3" = c("3"),
                                    "4" = c("4","44"),
                                    "5" = c("5"),
                                    "6" = c("6","`6"),
                                    "7" = c("7"),
                                    "8" = c("8"),
                                    "9" = c("9"),
                                    "10" = c("10"),
                                    "11" = c("11"),
                                    "12" = c("12","21")
  )) 
rphp_dedup$birth_month <-as.numeric(levels(rphp_dedup$birth_month))[rphp_dedup$birth_month] #month factor --> numeric
describe(rphp_dedup$birth_month)

##birth day  
rphp_dedup$birth_day<-as.factor(rphp_dedup$birth_day)
rphp_dedup <- rphp_dedup %>%
  mutate(birth_day = fct_collapse(birth_day,
                                  NULL = c("xx","tren","?","32","39","45","71","112","115","121", "224","290","298","221","37"),
                                  "1" = c("1"),
                                  "2" = c("2"),
                                  "3" = c("3","33"),
                                  "4" = c("4","40"),
                                  "5" = c("5","55"),
                                  "6" = c("6"),
                                  "7" = c("7"),
                                  "8" = c("8","88"),
                                  "9" = c("9"),
                                  "10" = c("10"),
                                  "11" = c("11"),
                                  "12" = c("12"),
                                  "13" = c("13"),
                                  "14" = c("14"),
                                  "15" = c("15"),
                                  "16" = c("16"),
                                  "17" = c("17"),
                                  "18" = c("18","81"),
                                  "19" = c("19"),
                                  "20" = c("20"),
                                  "21" = c("21"),
                                  "22" = c("22"),
                                  "23" = c("23"),
                                  "24" = c("24"),
                                  "25" = c("25"),
                                  "26" = c("26"),
                                  "27" = c("27"),
                                  "28" = c("28"),
                                  "29" = c("29"),
                                  "30" = c("30"),
                                  "31" = c("31")
  )) #%>% count(birth_day)


rphp_dedup$birth_day <-as.numeric(levels(rphp_dedup$birth_day))[rphp_dedup$birth_day] #day factor --> numeric
describe(rphp_dedup$birth_day)

##birth year  
rphp_dedup$birth_year<-as.factor(rphp_dedup$birth_year)
rphp_dedup <- rphp_dedup %>%
  mutate(birth_year = fct_collapse(birth_year,
                                   NULL = c("xx","3","5","6","7","14","18","20","24","28","32","66","67","68","69","70","71","77","87","90","556","557","1929","19589","1968","1966","1969","1980","1985","1995"),
                                   "36" = c("36","1936"),
                                   "37" = c("37","1937"),
                                   "38" = c("38"),
                                   "40" = c("40"),
                                   "41" = c("41"),
                                   "42" = c("42"),
                                   "43" = c("43","34"),
                                   "44" = c("44","1944"),
                                   "45" = c("45","1945"),
                                   "46" = c("46","1946"),
                                   "46" = c("47","1947"),
                                   "48" = c("48","1948"),
                                   "49" = c("49","1949","94"),
                                   "50" = c("50","1950","1050"),
                                   "51" = c("51","1951"),
                                   "52" = c("52","1952"),
                                   "53" = c("53","53?","1953"),
                                   "54" = c("54","1954","11954"),
                                   "55" = c("55","1955","1855"),
                                   "56" = c("56","1956","560","956"),
                                   "57" = c("57","1957","578"),
                                   "58" = c("58","1958"),
                                   "59" = c("59","1959","1860"),
                                   "60" = c("60","1960"),
                                   "61" = c("61","1961"),
                                   "62" = c("62","1962"),
                                   "63" = c("63","1963"),
                                   "64" = c("64","1964"),
                                   "65" = c("65","650"),
  ))# %>% count(birth_year)
rphp_dedup$birth_year <-as.numeric(levels(rphp_dedup$birth_year))[rphp_dedup$birth_year] #day factor --> numeric
describe(rphp_dedup$birth_year)

#make new column of combined DOB day.month.year
rphp_dedup<- unite(rphp_dedup, col='full_dob', c('birth_month', 'birth_day', 'birth_year'), sep='.')
describe(rphp_dedup$full_dob)

dobcount <- rphp_dedup %>%
  group_by(full_dob) %>%
  summarise(Count = n())     

#find number of full_dobs with more than 1 observation
repeateddobcount<-dobcount%>%filter(Count!=1)

#append full_dob count to rphp_dedup by full_dob
rphp_dedup_dobcount <- rphp_dedup %>% left_join(dobcount, by = "full_dob")

#find duplicated observations sharing full_dob, last_name
rphp_dedup_fulldoblastnamedup <- rphp_dedup_dobcount[duplicated(rphp_dedup_dobcount[ , c("last_name","full_dob")]), ]
# 21576 dups by full_dob and last name

#find duplicated observations sharing full_dob, last_name, first_name
rphp_dedup_fulldobfirstlastnamedup <- rphp_dedup_dobcount[duplicated(rphp_dedup_dobcount[ , c("first_name", "last_name","full_dob")]), ]
#18373 dups by full_dob, first name, last name - these are exact name matches, same person could have put in different first names by different obs ie. used nickname

#create new dataset of deduped by full_dob, last name, and first name, could still include duplicates of people using nicknames for each visit
rphp_dedup_firstlastfulldob <- rphp_dedup_dobcount[!duplicated(rphp_dedup_dobcount[ , c("last_name","first_name","full_dob")]), ]
#62058
#save to csv - these are deduped based on people with same dob, last name and exact first name, still includes people who used nicknames, misspellings, twins
write.csv(rphp_dedup_firstlastfulldob,file='/Volumes/shared/EOME/Weisskopf/Saint_Louis_Baby_Teeth_Study/Individuals/Joyce/RPHPdedup_L1.csv')
# 
# #create new dataset of deduped by full_dob, last name, could remove twins
# rphp_dedup_lastfulldob <- rphp_dedup_dobcount[!duplicated(rphp_dedup_dobcount[ , c("last_name","full_dob")]), ]
# #58855
# write.csv(rphp_dedup_lastfulldob,file='/Volumes/shared/EOME/Weisskopf/Saint_Louis_Baby_Teeth_Study/Individuals/Joyce/dedup_notwins.csv')

#create new dataset of deduped by full_dob, first name, city, could erroneously identify two different people with same first name and bday as duplicate same person
rphp_dedup_firstfulldob <- rphp_dedup_dobcount[!duplicated(rphp_dedup_dobcount[ , c("first_name","full_dob","city")]), ]
#61729

#create new dataset of deduped by full_dob, parent name
rphp_dedup_fulldobparent <- rphp_dedup_dobcount[!duplicated(rphp_dedup_dobcount[ , c("full_dob","parent_name")]), ]
#68666

#create new dataset of deduped by full_dob, address
rphp_dedup_fulldobstreet <- rphp_dedup_dobcount[!duplicated(rphp_dedup_dobcount[ , c("full_dob","street_add")]), ]
#65810

#create new dataset of deduped by full_dob, last name, city, mother carried city
rphp_dedup_doblastcitymother <- rphp_dedup_dobcount[!duplicated(rphp_dedup_dobcount[ , c("last_name","city", "mother_carried","full_dob")]), ]
#64803

#find intersecting rows across all different dedup datasets
common_rows <- generics::intersect(rphp_dedup_lastfulldob,rphp_dedup_fulldobparent) #55473
common_rows_ <- generics::intersect(common_rows,rphp_dedup_fulldobstreet) #51956, This one likely excludes twins
#more inclusive option potentially catching twins (this one probably better)
morecommon_rows <- generics::intersect(rphp_dedup_firstlastfulldob,rphp_dedup_firstfulldob)#58991
morecommon_rows_ <- generics::intersect(morecommon_rows,rphp_dedup_fulldobparent) #55443
morecommon_rows_1 <- generics::intersect(morecommon_rows_,rphp_dedup_fulldobstreet) #51636
morecommon_rows_2 <- generics::intersect(morecommon_rows_1,rphp_dedup_doblastcitymother) #51226
write.csv(morecommon_rows_2,file='/Volumes/shared/EOME/Weisskopf/Saint_Louis_Baby_Teeth_Study/Individuals/Joyce/RPHPdedup_L2.csv')


#More aggressive dedup that allows same person to have different DOB, potential misentering of birthdays
rphp_dedup_nodob <- rphp_dedup_dobcount[!duplicated(rphp_dedup_dobcount[ , c("last_name","first_name","city", "mother_carried")]), ]
#65178
morecommon_rows_3 <- generics::intersect(morecommon_rows_2,rphp_dedup_nodob) 
write.csv(morecommon_rows_3,file='/Volumes/shared/EOME/Weisskopf/Saint_Louis_Baby_Teeth_Study/Individuals/Joyce/RPHPdedup_L3.csv')
#49750

#check number of individuals with same dob after dedup
newdobcount <- morecommon_rows_2 %>%
  group_by(full_dob) %>%
  summarise(Count = n())     

#append full_dob count to rphp_dedup by full_dob
morecommon_rows_2_dobcount <- morecommon_rows_2 %>% left_join(newdobcount, by = "full_dob")
