.libPaths( c( "/root/R/x86_64-pc-linux-gnu-library/4.4/" , .libPaths() ) )
library(groundhog) 
set.groundhog.folder('/root/R_groundhog2/')
groundhog.library(c(
"data.table" ,  
"dplyr" , 
"magrittr" , 
"lubridate" , 
"readxl" , 
"ggplot2" , 
"forcats" , 
"stringr" ) , '2024-05-01')

# library(tidyr)

overlaps <- function(x, y) { c(
length(setdiff(x,y)) ,
length(intersect(x,y)) ,
length(setdiff(y,x)) 
)}


setwd("/research")
clarity_root <- '/early_act3/'


## Population included
main_pop <- read_xlsx("Template4TECTONICSData_Req21Mar2023.xlsx", sheet="Population_v2") %>% unique
all_ids <- fread('2023_07_TECTONICS_PaperData/2023_07_21_IDsGalore.csv', sep="^", quote="") ##PAT_MRN_ID AN_52_ENC_CSN_ID AN_53_ENC_CSN_ID AN_3_ENC_CSN_ID INPATIENT_DATA_ID_52_INTRAOP INPATIENT_DATA_ID_3_HSP AN_PROC_NAME AN_START_DATETIME AN_STOP_DATETIME Anesthesia_Duration CASE_CLASS_NM SERVICE_NM LOG_ID ASA_SCORE_C EMERG_STATUS AN_TYPE

all_ids[ ,SERVICE_int := fcase(
  SERVICE_NM %chin% c('Acute Critical Care Surgery','General Surgery','Trauma'), 1 ,
  SERVICE_NM == 'Cardiothoracic' , 2 ,
  SERVICE_NM == 'Colorectal' , 3 ,
  SERVICE_NM %chin% c('Anesthesiology', 'Dental', 'Gastroenterology', 'Minor Procedures', 'Pain Management', 'Pulmonary','Transplant Hepatology', 'Cardiovascular', 'Radiation Oncology', 'Interventional Radiology', 'Wound'), 4 ,
  SERVICE_NM == 'Hepatobiliary' , 5 ,
  SERVICE_NM == 'Minimally Invasive Surgery' , 6 ,
  SERVICE_NM %chin% c('Neurosurgery', 'Neurosurgery Spine'), 7 ,
  SERVICE_NM == 'Obstetrics / Gynecology' , 8 ,
  SERVICE_NM == 'Oncology' , 9 ,
  SERVICE_NM == 'Ophthalmology' , 4 ,
  SERVICE_NM %chin% c('Orthopaedic Spine', 'Orthopaedics', 'Podiatry Foot / Ankle'), 11 ,
  SERVICE_NM %chin% c('Otolaryngology', 'Oral / Maxillofacial'), 12 ,
  SERVICE_NM == 'Plastics' , 13 ,
  SERVICE_NM == 'Transplant' , 14 ,
  SERVICE_NM == 'Urology' , 15 ,
  SERVICE_NM == 'Vascular' , 16 
  )]

all_ids[ ,SERVICE_collapsed := fcase(
  SERVICE_NM %chin% c('Acute Critical Care Surgery','General Surgery','Trauma'), "General and Trauma" ,
  SERVICE_NM == 'Cardiothoracic' , "Cardiothoracic" ,
  SERVICE_NM == 'Colorectal' , "Colorectal" ,
  SERVICE_NM %chin% c('Anesthesiology', 'Dental', 'Gastroenterology', 'Minor Procedures', 'Pain Management', 'Pulmonary','Transplant Hepatology', 'Cardiovascular', 'Radiation Oncology', 'Interventional Radiology', 'Wound'), "Misc procedures, GI, Ophthalmology" ,
  SERVICE_NM == 'Hepatobiliary' , "General and Trauma"  ,
  SERVICE_NM == 'Minimally Invasive Surgery' , "General and Trauma"  ,
  SERVICE_NM %chin% c('Neurosurgery', 'Neurosurgery Spine'), "Neurosurgery" ,
  SERVICE_NM == 'Obstetrics / Gynecology' , "Gynecology" ,
  SERVICE_NM == 'Oncology' , "General and Trauma"  ,
  SERVICE_NM == 'Ophthalmology' , "Misc procedures, GI, Ophthalmology" ,
  SERVICE_NM %chin% c('Orthopaedic Spine', 'Orthopaedics', 'Podiatry Foot / Ankle'), "Orthopaedics" ,
  SERVICE_NM %chin% c('Otolaryngology', 'Oral / Maxillofacial'), "Otolaryngology" ,
  SERVICE_NM == 'Plastics' , "Plastics" ,
  SERVICE_NM == 'Transplant' , "Transplant" ,
  SERVICE_NM == 'Urology' , "Urology" ,
  SERVICE_NM == 'Vascular' , "Vascular" 
  )]

## these are all cancelled cases 
main_pop  %<>% filter( (SurgicalRecordID %in% all_ids$LOG_ID))



consort_list <- list()
consort_list[["Surgeries in period"]] <- uniqueN(  all_ids$LOG_ID) 
consort_list[["Weekday cases"]] <- all_ids [  !(weekdays(AN_START_DATETIME, abbr = TRUE) %chin% c("Sun", "Sat")  ) , .(LOG_ID)] %>% unlist %>% uniqueN
consort_list[["Weekend cases"]] <- all_ids [  (weekdays(AN_START_DATETIME, abbr = TRUE) %chin% c("Sun", "Sat")  ) , .(LOG_ID)] %>% unlist %>% uniqueN

# 
# ## days not staffed
# temp <- bind_rows(
# main_pop %>% filter(LastAction  %>% local_time %>% as.numeric %>% divide_by(60*60) %>% is_greater_than( 10) %>% not ) ,
# main_pop %>% filter(FirstAction  %>% local_time %>% as.numeric %>% divide_by(60*60)  %>% is_less_than( 12)  %>% not ) ,
# main_pop %>% mutate(active_time = difftime(LastAction,FirstAction, units="hours") %>% as.numeric ) %>% filter(active_time<=2) %>% select(!one_of("active_time"))
# ) %>% unique
# 
# setdiff(all_ids [  (weekdays(AN_DATE, abbr = TRUE) %chin% c("Sun", "Sat")  ) , .(LOG_ID)]$LOG_ID
# 
# consort_list[["ACT not staffed"]] <- temp %>% uniqueN
# 

# main_pop  %<>% filter( (SurgicalRecordID %in% join_mrns$SurgicalRecordID)) %>% filter(weekdays(Date) %in% c('Friday'  ,  'Monday' , 'Thursday' ,  'Tuesday' , 'Wednesday')   )
main_pop %<>% filter(LastAction  %>% local_time %>% as.numeric %>% divide_by(60*60) %>% is_greater_than( 10) )
main_pop %<>% filter(FirstAction  %>% local_time %>% as.numeric %>% divide_by(60*60)  %>% is_less_than( 12)  )
main_pop %<>% mutate(active_time = difftime(LastAction,FirstAction, units="hours") %>% as.numeric ) %>% filter(active_time>2)

## no AnStart in main pop  
temp <- main_pop %>% distinct(SurgicalRecordID, .keep_all=TRUE) %>% 
  left_join(all_ids[, .( SurgicalRecordID=LOG_ID,AN_START_DATETIME)], by="SurgicalRecordID" ) %>% 
  filter( !(weekdays(AN_START_DATETIME, abbr = TRUE) %chin% c("Sun", "Sat")  )  ) %>% 
  filter(  as.numeric(local_time(AN_START_DATETIME, units="hours" ) ) > fifelse( weekdays(AN_START_DATETIME, abbr = TRUE) == "Wed", 8-0.75 , 7-0.75 ) ) %>% 
  filter( as.numeric(local_time(AN_START_DATETIME, units="hours" ) ) < 16) %>% 
  pull("SurgicalRecordID") 
  
consort_list[["Case Started in ACT Hours"]] <- temp %>% uniqueN

consort_list[["Case Started outside ACT Hours"]] <-  consort_list[["Weekday cases"]] - consort_list[["Case Started in ACT Hours"]]




# main_pop %<>% group_by(Date) %>% mutate(ncase=uniqueN(OR_Abbrev) )  %>% ungroup %>% filter(ncase > 15) 
# main_pop[["ncase"]] <- NULL
# main_pop[["active_time"]] <- NULL

## target population
join_mrns <- main_pop %>% select(MRN, SurgicalRecordID, Date, isTreatment, DoB, OR_Abbrev) %>% data.table %>% unique  %>% setorder(SurgicalRecordID, MRN)

## the 2 duplicates by SurgicalRecordID are MRN merges where the lower number is prefered
join_mrns <- join_mrns[ join_mrns[ , .I[1], by="SurgicalRecordID"]$V1 ]

## this starts with no duplicates
join_mrns <- all_ids[, .( SurgicalRecordID=LOG_ID, AN_START_DATETIME, AN_STOP_DATETIME, SERVICE_collapsed, emergency=EMERG_STATUS=="1")] [join_mrns  , on="SurgicalRecordID"]

join_mrns <- join_mrns[ as.numeric(local_time(AN_START_DATETIME, units="hours" ) ) > fifelse( weekdays(AN_START_DATETIME, abbr = TRUE) == "Wed", 8-0.75 , 7-0.75 )] [ as.numeric(local_time(AN_START_DATETIME, units="hours" ) ) < 16 ] [!(weekdays(AN_START_DATETIME, abbr = TRUE) %chin% c("Sun", "Sat")  ) ]

join_mrns[, Age:=floor( as.numeric(difftime(AN_START_DATETIME, DoB, units="days"))/365.25) ]

consort_list[["NonAdult"]] <-  join_mrns[Age < 18]$SurgicalRecordID%>% uniqueN

join_mrns <- join_mrns[Age >= 18]
consort_list[["Adult"]] <-  join_mrns$SurgicalRecordID%>% uniqueN

## update to randomization
new_random <- read_xlsx("/research/2023_07_TECTONICS_PaperData/2024_06_28_SQL01T_Randomizaiton_Post_2020_05_13.xlsx", col_types="text") %>% data.table
join_mrns <- unique(new_random[, .(SurgicalRecordID=as.integer(Log_Id),RANDOMIZATION_SQL01T)])[join_mrns, on="SurgicalRecordID", nomatch=NA]

## these are almost always in agreement
# join_mrns[, late_period := Date >= as.Date("2020-05-13")]
# join_mrns[, .(is.na(RANDOMIZATION_SQL01T), late_period, )  ] %>% table(useNA='a') 
# join_mrns[is.na(RANDOMIZATION_SQL01T) & late_period==TRUE,  ] ## AK is tracking these 11 non-matches
# join_mrns[,late_period:=NULL]

join_mrns[, oldTx := isTreatment %>% as.integer]
join_mrns[, isTreatment := fcoalesce(as.integer(RANDOMIZATION_SQL01T), oldTx ) ]
join_mrns[, RANDOMIZATION_SQL01T:= NULL ]

## Filtering to avoid repeats on the same patient
pure_r_window <- function(x) {
  ## two common cases where all are in the same interval
  x2 <- c(TRUE, rep(FALSE, times=length(x)-1))
  if(length(x) >1) {
  if( x[length(x)]- x[1] > 30) {
  ## another common case
  if(min(diff(x)) > 30) {
    x2 <- rep(TRUE, length(x) )
  } else {
  ## the hard case
    x <- x - min(x)
    currentinterval <-0
    for( i in seq_along(x)) {
      if(x[i]  - currentinterval > 30) {
        currentinterval <- x[i]
        x2[i] <- TRUE
      }
      
  }
  }
  }
  }
  return(x2)

}
setorder(join_mrns, MRN, Date)

join_mrns [ ,window_start:=pure_r_window(as.numeric(difftime(Date, min(Date) , units="days") ))  , by="MRN"]

 
consort_list[["Index Case"]] <- sum(join_mrns$window_start == TRUE)
consort_list[["Excluded subsequent case"]] <- sum(join_mrns$window_start == FALSE)

join_mrns[, DoB:=NULL]
join_mrns[, Age:=NULL]

join_mrns %>% fwrite("/research/intermediates/population.csv")

join_mrns[, cluster_id := fct_cross(as_factor(OR_Abbrev), as_factor(as.character(Date)) ) ]

consort_list[["Intervention clusters"]] <- join_mrns[isTreatment==1 & window_start==TRUE, .(cluster_id)] %>% uniqueN
consort_list[["Control clusters"]] <- join_mrns[isTreatment==0 & window_start==TRUE, .(cluster_id)] %>% uniqueN

consort_list[["Intervention participants"]] <- join_mrns[isTreatment==1 & window_start==TRUE, .(SurgicalRecordID)] %>% uniqueN
consort_list[["Control participants"]] <- join_mrns[isTreatment==0 & window_start==TRUE, .(SurgicalRecordID)] %>% uniqueN


consort_list %>% unlist %>% as.data.frame %>% write.csv("/research/outputs/consort.txt")





#########
## enrollment summary pictures
#########

roughen_date <- function(x) round(year(x) + (month(x)-1)/12,2)  

cases_by_month_summary <-  copy(join_mrns )[, roughdate := Date %>% roughen_date ][, .(UniqueCase=uniqueN(SurgicalRecordID) ), by=.(roughdate, isTreatment) ][roughdate>2019.25] [order(roughdate)]

(cases_by_month_summary%>% dcast(roughdate~isTreatment, value.var="UniqueCase"))  [, .(YearMonth = roughdate, Controls=`0`, Interv=`1`, Enrolled = `0`+`1`)] %>% fwrite("/research/outputs/enrollment.csv")
copy(cases_by_month_summary)[,isTreatment:=factor(isTreatment) ] %>% ggplot( aes(x=roughdate, y=UniqueCase, fill=isTreatment)) + geom_bar(stat = "identity", position="stack") + labs(x = "Date", y = "Count", title = "Enrollment by month") -> temp2
temp2 %>% ggsave(device="png", filename="/research/outputs/enrollment.png")

copy(cases_by_month_summary)[,isTreatment:=factor(isTreatment) ] %>% ggplot( aes(x=roughdate, y=UniqueCase, fill=isTreatment)) + geom_bar(stat = "identity", position="fill") + labs(x = "Date", y = "Fraction", title = "Enrollment by month") -> temp2
temp2 %>% ggsave(device="png", filename="/research/outputs/enrollment_frac.png")

#########
## covid summary picture for comparison
#########

covid_files <- c(
"/research/nyt/covid-19-data/rolling-averages/us-counties-2020.csv" , 
"/research/nyt/covid-19-data/rolling-averages/us-counties-2021.csv" ,
"/research/nyt/covid-19-data/rolling-averages/us-counties-2022.csv" ,
"/research/nyt/covid-19-data/rolling-averages/us-counties-2023.csv"
)
# myf <-  . %>% fread %>% extract( geoid == 'USA-29189') 
# myf <- 
covid_summary <-  covid_files %>% lapply(  FUN =function(x) {fread(x)[geoid == 'USA-29189'] } ) %>% rbindlist

covid_summary <- covid_summary[ between( as.Date(date), ymd("2019-07-01") ,  ymd("2023-02-01")) ] 
covid_summary [ , roughdate := roughen_date(date)]
covid_summary[, .(cases = mean(deaths_avg_per_100k, na.rm=T) ), by= 'roughdate'][order(roughdate)] %>% ggplot( aes(x=roughdate, y=cases)) + geom_bar(stat = "identity", position="stack") + labs(x = "Date", y = "Cases / 100k", title = "Covid-19 cases by month") -> temp2

temp2 %>% ggsave(device="png", filename="/research/outputs/covid.png")

