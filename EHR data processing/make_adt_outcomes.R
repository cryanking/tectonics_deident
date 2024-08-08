.libPaths( c( "/root/R/x86_64-pc-linux-gnu-library/4.4/" , .libPaths() ) )
library(groundhog) 
set.groundhog.folder('/root/R_groundhog2/')
groundhog.library(c(
"data.table" ,  
"dplyr" , 
"magrittr" , 
"lubridate" , 
"ggplot2" , 
"forcats" , 
"stringr" ) , '2024-05-01')
 


# library(tidyr)


roughen_date <- function(x) round(year(x) + (month(x)-1)/12,2)  

setwd("/research")
clarity_root <- '/early_act3/'


join_mrns <- fread("/research/intermediates/population.csv")
cases_by_month_summary <-  copy(join_mrns )[, roughdate := Date %>% roughen_date ][, .(UniqueCase=uniqueN(SurgicalRecordID) ), by=.(roughdate, isTreatment) ][roughdate>2019.25] [order(roughdate)]

an_start_stop <- join_mrns[,.(CurrentMRN=MRN, AnestStart=AN_START_DATETIME, AnestStop=AN_STOP_DATETIME, orlogid=SurgicalRecordID)][!is.na(AnestStart) & !is.na(AnestStop) ] %>% unique(by="orlogid")


clarity_death  <- fread("2023_07_TECTONICS_PaperData/2023_07_17_Clarity_Result_Set_DeathDates.csv", sep="^", quote="")[ , Death_Date:=  ymd_hms(Death_Date) ][!is.na(Death_Date), .(CurrentMRN, Death_Date)] %>% unique ## MRN, date

## join on dod for pmin of discharge and icu exit times

local_clarity_death <- unique(an_start_stop[,.(orlogid, CurrentMRN)])[clarity_death, on="CurrentMRN" , allow.cartesian=TRUE]
local_clarity_death[, CurrentMRN:=NULL] 

## there is incomplete overlap between the tectonics file and ACT3 (45074/64492), which may reflect criteria
###########################
## Hospital and ICU length of stay
###########################
## ACT3 main
# epic_new_events <- fread(paste0(clarity_root , 'Clarity Harper Result Set Operating Room Event Log.csv'))
# epic_new_events[, CPB_TIME := ymd_hm(CPB_TIME)]
# an_start_stop2 <- epic_new_events[ , .(CurrentMRN = min(CurrentMRN), AnestStart=min(CPB_TIME[Event=="Anesthesia Start"] ), AnestStop=min(CPB_TIME[Event=="Anesthesia Stop"] ) ) , by="orlogid"]
# an_start_stop2 <- an_start_stop[!is.na(AnestStart) & !is.na(AnestStop)]

epic_admits <- fread(paste0(clarity_root , 'Clarity Hogue Result Set ADT.csv'))

## the CSN is the "hospitalization" CSN, have to join on MRN for postops
## FIXED: this ends up with admit data for orlogids outside this query who have surgery in a another wave.
## To eliminate, need to explicitly drop cases outside the target times. Could also merge the 3 datasets first
epic_admits <- an_start_stop[AnestStart < ymd('2021-01-01')] [epic_admits, on="CurrentMRN", allow.cartesian=T, nomatch=NULL ]

## this is to catch patients who were in the hospital when the file generated
last_date <-  epic_admits[(ADT_EVENT=="Discharge")]$EFFECTIVE_TIME %>% max(na.rm=T)

## for SLCH specifically, remove intraop discharges
epic_admits[ , is_slch := grepl(DEPARTMENT_NAME, pattern="SLCH", fixed=TRUE )]
epic_admits[ , is_slch := any(is_slch) , by = "orlogid" ]
epic_admits[(is_slch & between(EFFECTIVE_TIME, AnestStart ,AnestStop) & ADT_EVENT =="Discharge"), ADT_EVENT:="Transfer Out" ]

## sorting on event is to place admit after discharge and transfer in after transfer out
setorderv(epic_admits , cols=c('orlogid', 'EFFECTIVE_TIME','ADT_EVENT'), order=c(1,1,-1) )
setkey(epic_admits , orlogid, EFFECTIVE_TIME)

epic_admits  [, prev_status :=  shift(ADT_EVENT, type="lag") , by = "orlogid" ]
## drop admit -> admit events (this effectively merges to the earlier of the two)
epic_admits  <- epic_admits [ !((prev_status %chin% c("Admission", "Transfer In")) & (ADT_EVENT %chin% c("Admission", "Transfer In"))) ]
## find the previous time
epic_admits [, prev_time :=  shift(EFFECTIVE_TIME, type="lag") , by = "orlogid" ]
## drop discharge -> admit within 4 hours; this is an internal transfer
epic_admits  [ which( (as.numeric(difftime(EFFECTIVE_TIME, prev_time, units="hours")) < 4) & (prev_status == "Discharge") & (ADT_EVENT %chin% c("Admission", "Transfer In"))) -1 , ADT_EVENT:="Transfer Out" ]
epic_admits  [(as.numeric(difftime(EFFECTIVE_TIME, prev_time, units="hours")) < 4) & (prev_status == "Discharge") & (ADT_EVENT== "Admission") , ADT_EVENT:="Transfer In" ]


## find the first discharge after an_stop, first admit after an_stop, last admit before an_stop
first_post_admit <- epic_admits [(EFFECTIVE_TIME > AnestStop + ddays(0.5)) & (ADT_EVENT=="Admission") , .( readmit=min(EFFECTIVE_TIME) ) , by="orlogid"]

# postop_los <- epic_admits[(ADT_EVENT=="Discharge") & (EFFECTIVE_TIME > AnestStop), .(discharge_y =  min(EFFECTIVE_TIME) ), by="orlogid" ]



preop_los <- epic_admits[(ADT_EVENT=="Admission") & (EFFECTIVE_TIME < AnestStop + dhours(1)), .(admit_y =   max(EFFECTIVE_TIME) ), by="orlogid" ]
discharge_time <- epic_admits [(EFFECTIVE_TIME > AnestStart + dhours(0.5) ) & (ADT_EVENT=="Discharge") , .( discharge_time=min(EFFECTIVE_TIME), AnestStop=first(AnestStop) ) , by="orlogid"]

discharge_time <- local_clarity_death[discharge_time, on="orlogid", nomatch=NA ]
discharge_time[, discharge_time:= pmax(AnestStop, pmin(discharge_time, Death_Date, na.rm=T) ) ] 
discharge_time[, Death_Date:=NULL]
discharge_time[, AnestStop:=NULL]

## finding the duration of preop ICU is more complicated than I want because of all the transfers to CT etc.
## however, because of the transfer out event to the OR can just search forr any ICU events

preop_ICU <- epic_admits [(EFFECTIVE_TIME > AnestStart -ddays(1) ) & (EFFECTIVE_TIME < AnestStop - dhours(1) ) ]
preop_ICU <- preop_ICU[, .(preop_ICU = any(grepl(DEPARTMENT_NAME, pattern="ICU"))), by="orlogid" ]

adt_data <- merge(merge(merge(merge(first_post_admit , discharge_time , all=T,  by="orlogid"), an_start_stop, all.x=T,  by="orlogid"), preop_ICU , all=TRUE,  by="orlogid"), preop_los,  by="orlogid", all=T)

adt_data[is.na(preop_ICU), preop_ICU := FALSE ]
adt_data[is.na(discharge_time) , discharge_time := pmin(last_date, AnestStart + ddays(90) ) ] 
adt_data [ , preop_los := as.numeric(difftime(AnestStart, admit_y, units="days"))]
adt_data [ , postop_los  := as.numeric(difftime(discharge_time, AnestStop, units="days"))]
adt_data [ , readmission_survival  := as.numeric(difftime(readmit,discharge_time, units="days"))]
adt_data [ readmission_survival < 0 , readmission_survival  := Inf]
adt_data[is.na(readmission_survival), readmission_survival := Inf]

## very slightly negative postop los are documentation errors
adt_data [between(postop_los, -3/24, 0), postop_los := 0]
# adt_data[is.na(postop_los) , postop_los :=  as.numeric(difftime(last_date, AnestStop, units="days"))]

adt_data <- adt_data[, .(preop_los,postop_los,readmission_survival,preop_ICU, orlogid)]


## this will pick up if the patient is already in the icu - first event is transfer out
postop_ICU <-  copy(epic_admits [(EFFECTIVE_TIME > AnestStop -dhours(1) ) ][grepl(DEPARTMENT_NAME, pattern="BJH\\s\\d")] )
setorder(postop_ICU, orlogid, EFFECTIVE_TIME)

## is the first unit an icu, regardless of how far in the future it is
postop_ICU_late <- postop_ICU[ postop_ICU[, .I[1], by="orlogid" ]$V1  , .(ICU=grepl(DEPARTMENT_NAME, pattern="ICU") , unit=DEPARTMENT_NAME, orlogid , admit_day=as.numeric(difftime(EFFECTIVE_TIME,AnestStop, units="days"  )) ) ]


##  in the next 7 days, the first ICU admission
postop_ICU <- postop_ICU[ (EFFECTIVE_TIME < AnestStop + ddays(7) )][grepl(DEPARTMENT_NAME, pattern="ICU") ]
postop_ICU <- postop_ICU[ postop_ICU[ , .I[1], by="orlogid" ]$V1,  .(unit=DEPARTMENT_NAME, orlogid, admit_day=as.numeric(difftime(EFFECTIVE_TIME,AnestStop, units="days"  )) )]
postop_ICU[, ICU2 :=TRUE]

postop_ICU_late <- postop_ICU_late[ICU == TRUE]
postop_ICU <- merge(postop_ICU, postop_ICU_late,  by="orlogid", all=T)
postop_ICU[, unit := fcoalesce(unit.x, unit.y) ]
postop_ICU[, ICU := fcoalesce(ICU | ICU2, FALSE) ]
postop_ICU[, admit_day := pmax(0., pmin(admit_day.x, admit_day.y, na.rm=TRUE)) ]
postop_ICU_status <- postop_ICU

## create ICU stays - in-out containing ICU
## filtering times
# discharge_time <- epic_admits [(EFFECTIVE_TIME > AnestStop) & (ADT_EVENT=="Discharge") , .( discharge_time=min(EFFECTIVE_TIME)  ) , by="orlogid"]

## postop events in ICUs
postop_ICU <-  copy(epic_admits [(EFFECTIVE_TIME > AnestStop -dhours(1) ) ][grepl(DEPARTMENT_NAME, pattern="BJH\\s\\d")] )
postop_ICU <- postop_ICU[ grep(DEPARTMENT_NAME, pattern="ICU")]

## during this stay
postop_ICU <- discharge_time[postop_ICU , on="orlogid", nomatch=NA]
postop_ICU[is.na(discharge_time) , discharge_time := pmin(last_date, AnestStart + ddays(30) ) ] 
postop_ICU <- postop_ICU[ EFFECTIVE_TIME <= discharge_time]


## if last event is not an out, create one
setkey(postop_ICU, orlogid, EFFECTIVE_TIME)
last_event <- postop_ICU[ postop_ICU[ , .I[.N], by="orlogid" ]$V1 ][ADT_EVENT %chin% c("Admission", "Transfer In") ]
last_event[ ,ADT_EVENT:="Transfer Out"] 
last_event[ ,EFFECTIVE_TIME:= discharge_time] 


if(nrow(last_event ) >0 ) {
  postop_ICU <- rbind(postop_ICU,last_event )
  setorder(postop_ICU, orlogid, EFFECTIVE_TIME)
}

## if there is no previous time, start at an stop
postop_ICU [is.na(prev_time), prev_time := AnestStop] 
postop_ICU [is.na(prev_status), prev_status := "Transfer In"] 

## set all times before anstop to zero
postop_ICU [ , EFFECTIVE_TIME := pmax(EFFECTIVE_TIME, AnestStop ) ] 
postop_ICU [ , prev_time := pmax(prev_time, AnestStop ) ] 

## merge stays within 1 day of each other - or just define the outcome as cumulative ICU time this admission. That's a lot easier.
iculos <- postop_ICU[ADT_EVENT %chin% c("Discharge", "Transfer Out")] [prev_status %chin% c("Admission", "Transfer In")] [, .(ICULoS = sum(as.numeric(difftime( EFFECTIVE_TIME, prev_time, units="hours") ) ) ), by="orlogid" ]

merged_adt <- iculos  %>%
merge( postop_ICU_status[, .(ICU, unit, admit_day, orlogid) ] , by="orlogid", all= TRUE) %>%
merge( adt_data[, .(postop_los,readmission_survival, orlogid) ] , by="orlogid", all= TRUE) 

merged_adt[is.na(ICU), ICU:=FALSE ]
merged_adt[is.na(ICULoS), ICULoS:=0. ]


merged_adt_act3 <- merged_adt #[join_mrns[, .(orlogid=SurgicalRecordID)] , on="orlogid", nomatch=NULL]

#############
## 2022 update
##############
# an_records <- fread("/mark2_act3/intermediates/an_records.csv")
admits <- fread("/mark2_act3/easy_hard_version/easyADT.csv", sep=';')
epic_admits <- an_start_stop[between(AnestStart, ymd('2021-01-01'), ymd('2022-03-01') ) ] [admits, on="CurrentMRN", allow.cartesian=T ]

# setnames(epic_admits, "an_start", "AnestStart" )
# setnames(epic_admits, "an_stop", "AnestStop" )

last_date <-  admits[(ADT_EVENT=="Discharge")]$EFFECTIVE_TIME %>% max(na.rm=T)

## for SLCH specifically, remove intraop discharges
epic_admits[ , is_slch := grepl(DEPARTMENT_NAME, pattern="SLCH", fixed=TRUE )]
epic_admits[ , is_slch := any(is_slch) , by = "orlogid" ]
epic_admits[(is_slch & between(EFFECTIVE_TIME, AnestStart ,AnestStop) & ADT_EVENT =="Discharge"), ADT_EVENT:="Transfer Out" ]

setorderv(epic_admits , cols=c('orlogid', 'EFFECTIVE_TIME','ADT_EVENT'), order=c(1,1,-1) )
setkey(epic_admits , orlogid, EFFECTIVE_TIME)

epic_admits  [, prev_status :=  shift(ADT_EVENT, type="lag") , by = "orlogid" ]
## drop admit -> admit events (this effectively merges to the earlier of the two)
epic_admits  <- epic_admits [ !((prev_status %chin% c("Admission", "Transfer In")) & (ADT_EVENT %chin% c("Admission", "Transfer In"))) ]
## find the previous time
epic_admits [, prev_time :=  shift(EFFECTIVE_TIME, type="lag") , by = "orlogid" ]
## drop discharge -> admit within 4 hours; this is an internal transfer
epic_admits  [ which( (as.numeric(difftime(EFFECTIVE_TIME, prev_time, units="hours")) < 4) & (prev_status == "Discharge") & (ADT_EVENT %chin% c("Admission", "Transfer In"))) -1 , ADT_EVENT:="Transfer Out" ]
epic_admits  [(as.numeric(difftime(EFFECTIVE_TIME, prev_time, units="hours")) < 4) & (prev_status == "Discharge") & (ADT_EVENT== "Admission") , ADT_EVENT:="Transfer In" ]

first_post_admit <- epic_admits [(EFFECTIVE_TIME > AnestStop + ddays(0.5)) & (ADT_EVENT=="Admission") , .( readmit=min(EFFECTIVE_TIME) ) , by="orlogid"]

# postop_los <- epic_admits[(ADT_EVENT=="Discharge") & (EFFECTIVE_TIME > AnestStop), .(discharge_y =  min(EFFECTIVE_TIME) ), by="orlogid" ]
preop_los <- epic_admits[(ADT_EVENT=="Admission") & (EFFECTIVE_TIME < AnestStop + dhours(1)), .(admit_y =   max(EFFECTIVE_TIME) ), by="orlogid" ]
discharge_time <- epic_admits [(EFFECTIVE_TIME > AnestStart + dhours(0.5) ) & (ADT_EVENT=="Discharge") , .( discharge_time=min(EFFECTIVE_TIME), AnestStop=first(AnestStop) ) , by="orlogid"]

discharge_time <- local_clarity_death[discharge_time, on="orlogid" ]
discharge_time[, discharge_time:= pmax(AnestStop, pmin(discharge_time, Death_Date, na.rm=T) ) ] 
discharge_time[, Death_Date:=NULL]
discharge_time[, AnestStop:=NULL]

preop_ICU <- epic_admits [(EFFECTIVE_TIME > AnestStart -ddays(1) ) & (EFFECTIVE_TIME < AnestStop - dhours(1) ) ]

preop_ICU <- preop_ICU[, .(preop_ICU = any(grepl(DEPARTMENT_NAME, pattern="ICU"))), by="orlogid" ]

adt_data <- merge(merge(merge(merge(first_post_admit , discharge_time , all=T,  by="orlogid"), an_start_stop, all.x=T,  by="orlogid"), preop_ICU , all=TRUE,  by="orlogid"), preop_los,  by="orlogid", all=T)

# setnames(adt_data, "an_start", "AnestStart" )
# setnames(adt_data, "an_stop", "AnestStop" )


adt_data[is.na(preop_ICU), preop_ICU := FALSE ]
adt_data [ , preop_los := as.numeric(difftime(AnestStart, admit_y, units="days"))]
adt_data[is.na(discharge_time) , discharge_time := pmin(last_date, AnestStart + ddays(90) ) ] 
adt_data [ , postop_los  := as.numeric(difftime(discharge_time, AnestStop, units="days"))]
adt_data [ , readmission_survival  := as.numeric(difftime(readmit,discharge_time, units="days"))]
adt_data[(readmission_survival < 0), readmission_survival := Inf]
adt_data[is.na(readmission_survival), readmission_survival := Inf]
# adt_data[is.na(postop_los) , postop_los :=  as.numeric(difftime(last_date, AnestStop, units="days"))]

adt_data [between(postop_los, -3/24, 0), postop_los := 0]

adt_data <- adt_data[, .(preop_los,postop_los,readmission_survival,preop_ICU, orlogid)]

postop_ICU <-  copy(epic_admits [(EFFECTIVE_TIME > AnestStop -dhours(1) ) ][grepl(DEPARTMENT_NAME, pattern="BJH\\s\\d")] )
setorder(postop_ICU, orlogid, EFFECTIVE_TIME)
postop_ICU_late <- postop_ICU[ postop_ICU[, .I[1], by="orlogid" ]$V1  , .(ICU=grepl(DEPARTMENT_NAME, pattern="ICU") , unit=DEPARTMENT_NAME, orlogid , admit_day=as.numeric(difftime(EFFECTIVE_TIME,AnestStop, units="days"  )) ) ]

postop_ICU <- postop_ICU[ (EFFECTIVE_TIME < AnestStop + ddays(7) )][grepl(DEPARTMENT_NAME, pattern="ICU") ]
postop_ICU <- postop_ICU[ postop_ICU[ , .I[1], by="orlogid" ]$V1,  .(unit=DEPARTMENT_NAME, orlogid, admit_day=as.numeric(difftime(EFFECTIVE_TIME,AnestStop, units="days"  )) )]
postop_ICU[, ICU2 :=TRUE]

postop_ICU_late <- postop_ICU_late[ICU == TRUE]
postop_ICU <- merge(postop_ICU, postop_ICU_late,  by="orlogid", all=T)
postop_ICU[, unit := fcoalesce(unit.x, unit.y) ]
postop_ICU[, ICU := fcoalesce(ICU | ICU2, FALSE) ]
postop_ICU[, admit_day := pmax(0., pmin(admit_day.x, admit_day.y, na.rm=TRUE)) ]
postop_ICU_status <- postop_ICU

## postop events in ICUs
postop_ICU <-  copy(epic_admits [(EFFECTIVE_TIME > AnestStop -dhours(1) ) ][grepl(DEPARTMENT_NAME, pattern="BJH\\s\\d")] )
postop_ICU <- postop_ICU[ grep(DEPARTMENT_NAME, pattern="ICU")]

## during this stay
postop_ICU <- discharge_time[postop_ICU , on="orlogid", nomatch=NA]
postop_ICU[is.na(discharge_time) , discharge_time := pmin(last_date, AnestStart + ddays(30) ) ] 
postop_ICU <- postop_ICU[ EFFECTIVE_TIME <= discharge_time]


## if last event is not an out, create one
setorder(postop_ICU, orlogid, EFFECTIVE_TIME)
last_event <- postop_ICU[ postop_ICU[ , .I[.N], by="orlogid" ]$V1 ][ADT_EVENT %chin% c("Admission", "Transfer In") ]
last_event[ ,ADT_EVENT:="Transfer Out"] 
last_event[ ,EFFECTIVE_TIME:= discharge_time] 

if(nrow(last_event ) >0 ) {
  postop_ICU <- rbind(postop_ICU,last_event )
  setorder(postop_ICU, orlogid, EFFECTIVE_TIME)
}

## if there is no previous time, start at an stop
postop_ICU [is.na(prev_time), prev_time := AnestStop] 
postop_ICU [is.na(prev_status), prev_status := "Transfer In"] 

## set all times before anstop to zero
postop_ICU [ , EFFECTIVE_TIME := pmax(EFFECTIVE_TIME, AnestStop ) ] 
postop_ICU [ , prev_time := pmax(prev_time, AnestStop ) ] 

## merge stays within 1 day of each other - or just define the outcome as cumulative ICU time this admission. That's a lot easier.
iculos <- postop_ICU[ADT_EVENT %chin% c("Discharge", "Transfer Out")] [prev_status %chin% c("Admission", "Transfer In")] [, .(ICULoS = sum(as.numeric(difftime( EFFECTIVE_TIME, prev_time, units="hours") ) ) ), by="orlogid" ]

merged_adt <- iculos  %>%
merge( postop_ICU_status[, .(ICU, unit, admit_day, orlogid) ] , by="orlogid", all= TRUE) %>%
merge( adt_data[, .(postop_los,readmission_survival, orlogid) ] , by="orlogid", all= TRUE) 

merged_adt[is.na(ICU), ICU:=FALSE ]
merged_adt[is.na(ICULoS), ICULoS:=0. ]

merged_adt_act3mk2 <- merged_adt #[join_mrns[, .(orlogid=SurgicalRecordID)] , on="orlogid", nomatch=NULL]


#############
## New cohort
#############

admits <- fread("/research/2023_11_02_Clarity_Result_Set_ADT.csv", sep='^')
epic_admits <- an_start_stop[admits, on="CurrentMRN", allow.cartesian=T ]

last_date <-  admits[(ADT_EVENT=="Discharge")]$EFFECTIVE_TIME %>% max(na.rm=T)

## for SLCH specifically, remove intraop discharges
epic_admits[ , is_slch := grepl(DEPARTMENT_NAME, pattern="SLCH", fixed=TRUE )]
epic_admits[ , is_slch := any(is_slch) , by = "orlogid" ]
epic_admits[(is_slch & between(EFFECTIVE_TIME, AnestStart ,AnestStop) & ADT_EVENT =="Discharge"), ADT_EVENT:="Transfer Out" ]

setorderv(epic_admits , cols=c('orlogid', 'EFFECTIVE_TIME','ADT_EVENT'), order=c(1,1,-1) )
setkey(epic_admits , orlogid, EFFECTIVE_TIME)


epic_admits  [, prev_status :=  shift(ADT_EVENT, type="lag") , by = "orlogid" ]
## drop admit -> admit events (this effectively merges to the earlier of the two)
epic_admits  <- epic_admits [ !((prev_status %chin% c("Admission", "Transfer In")) & (ADT_EVENT %chin% c("Admission", "Transfer In"))) ]
## find the previous time
epic_admits [, prev_time :=  shift(EFFECTIVE_TIME, type="lag") , by = "orlogid" ]
## drop discharge -> admit within 4 hours; this is an internal transfer
epic_admits  [ which( (as.numeric(difftime(EFFECTIVE_TIME, prev_time, units="hours")) < 4) & (prev_status == "Discharge") & (ADT_EVENT %chin% c("Admission", "Transfer In"))) -1 , ADT_EVENT:="Transfer Out" ]
epic_admits  [(as.numeric(difftime(EFFECTIVE_TIME, prev_time, units="hours")) < 4) & (prev_status == "Discharge") & (ADT_EVENT== "Admission") , ADT_EVENT:="Transfer In" ]


first_post_admit <- epic_admits [(EFFECTIVE_TIME > AnestStop + ddays(0.5)) & (ADT_EVENT=="Admission") , .( readmit=min(EFFECTIVE_TIME) ) , by="orlogid"]

# postop_los <- epic_admits[(ADT_EVENT=="Discharge") & (EFFECTIVE_TIME > AnestStop), .(discharge_y =  min(EFFECTIVE_TIME) ), by="orlogid" ]
preop_los <- epic_admits[(ADT_EVENT=="Admission") & (EFFECTIVE_TIME < AnestStop + dhours(1)), .(admit_y =   max(EFFECTIVE_TIME) ), by="orlogid" ]
discharge_time <- epic_admits [(EFFECTIVE_TIME > AnestStart + dhours(0.5) ) & (ADT_EVENT=="Discharge") , .( discharge_time=min(EFFECTIVE_TIME), AnestStop=first(AnestStop) ) , by="orlogid"]

discharge_time <- local_clarity_death[discharge_time, on="orlogid" ]
discharge_time[, discharge_time:= pmax(AnestStop, pmin(discharge_time, Death_Date, na.rm=T) ) ] 
discharge_time[, Death_Date:=NULL]
discharge_time[, AnestStop:=NULL]


preop_ICU <- epic_admits [(EFFECTIVE_TIME > AnestStart -ddays(1) ) & (EFFECTIVE_TIME < AnestStop - dhours(1) ) ]

preop_ICU <- preop_ICU[, .(preop_ICU = any(grepl(DEPARTMENT_NAME, pattern="ICU"))), by="orlogid" ]

adt_data <- merge(merge(merge(merge(first_post_admit , discharge_time , all=T,  by="orlogid"), an_start_stop, all.x=T,  by="orlogid"), preop_ICU , all=TRUE,  by="orlogid"), preop_los,  by="orlogid", all=T)



adt_data[is.na(preop_ICU), preop_ICU := FALSE ]
adt_data [ , preop_los := as.numeric(difftime(AnestStart, admit_y, units="days"))]
adt_data[is.na(discharge_time) , discharge_time := pmin(last_date, AnestStart + ddays(90) ) ] 
adt_data [ , postop_los  := as.numeric(difftime(discharge_time, AnestStop, units="days"))]
adt_data [ , readmission_survival  := as.numeric(difftime(readmit,discharge_time, units="days"))]
adt_data[(readmission_survival < 0), readmission_survival := Inf]
adt_data[is.na(readmission_survival), readmission_survival := Inf]
# adt_data[is.na(postop_los) , postop_los :=  as.numeric(difftime(last_date, AnestStop, units="days"))]
adt_data [between(postop_los, -3/24, 0), postop_los := 0]

adt_data <- adt_data[, .(preop_los,postop_los,readmission_survival,preop_ICU, orlogid)]

postop_ICU <-  copy(epic_admits [(EFFECTIVE_TIME > AnestStop -dhours(1) ) ][grepl(DEPARTMENT_NAME, pattern="BJH\\s\\d")] )
setorder(postop_ICU, orlogid, EFFECTIVE_TIME)
postop_ICU_late <- postop_ICU[ postop_ICU[, .I[1], by="orlogid" ]$V1  , .(ICU=grepl(DEPARTMENT_NAME, pattern="ICU") , unit=DEPARTMENT_NAME, orlogid , admit_day=as.numeric(difftime(EFFECTIVE_TIME,AnestStop, units="days"  )) ) ]

postop_ICU <- postop_ICU[ (EFFECTIVE_TIME < AnestStop + ddays(7) )][grepl(DEPARTMENT_NAME, pattern="ICU") ]
postop_ICU <- postop_ICU[ postop_ICU[ , .I[1], by="orlogid" ]$V1,  .(unit=DEPARTMENT_NAME, orlogid, admit_day=as.numeric(difftime(EFFECTIVE_TIME,AnestStop, units="days"  )) )]
postop_ICU[, ICU2 :=TRUE]

postop_ICU_late <- postop_ICU_late[ICU == TRUE]
postop_ICU <- merge(postop_ICU, postop_ICU_late,  by="orlogid", all=T)
postop_ICU[, unit := fcoalesce(unit.x, unit.y) ]
postop_ICU[, ICU := fcoalesce(ICU | ICU2, FALSE) ]
postop_ICU[, admit_day := pmax(0., pmin(admit_day.x, admit_day.y, na.rm=TRUE)) ]
postop_ICU_status <- postop_ICU

## postop events in ICUs
postop_ICU <-  copy(epic_admits [(EFFECTIVE_TIME > AnestStop -dhours(1) ) ][grepl(DEPARTMENT_NAME, pattern="BJH\\s\\d")] )
postop_ICU <- postop_ICU[ grep(DEPARTMENT_NAME, pattern="ICU")]

## during this stay
postop_ICU <- discharge_time[postop_ICU , on="orlogid", nomatch=NA]
postop_ICU[is.na(discharge_time) , discharge_time := pmin(last_date, AnestStart + ddays(30) ) ] 
postop_ICU <- postop_ICU[ EFFECTIVE_TIME <= discharge_time]


## if last event is not an out, create one
setorder(postop_ICU, orlogid, EFFECTIVE_TIME)
last_event <- postop_ICU[ postop_ICU[ , .I[.N], by="orlogid" ]$V1 ][ADT_EVENT %chin% c("Admission", "Transfer In") ]
last_event[ ,ADT_EVENT:="Transfer Out"] 
last_event[ ,EFFECTIVE_TIME:= discharge_time] 

if(nrow(last_event ) >0 ) {
  postop_ICU <- rbind(postop_ICU,last_event )
  setorder(postop_ICU, orlogid, EFFECTIVE_TIME)
}


## if there is no previous time, start at an stop
postop_ICU [is.na(prev_time), prev_time := AnestStop] 
postop_ICU [is.na(prev_status), prev_status := "Transfer In"] 

## set all times before anstop to zero
postop_ICU [ , EFFECTIVE_TIME := pmax(EFFECTIVE_TIME, AnestStop ) ] 
postop_ICU [ , prev_time := pmax(prev_time, AnestStop ) ] 

## merge stays within 1 day of each other - or just define the outcome as cumulative ICU time this admission. That's a lot easier.
iculos <- postop_ICU[ADT_EVENT %chin% c("Discharge", "Transfer Out")] [prev_status %chin% c("Admission", "Transfer In")] [, .(ICULoS = sum(as.numeric(difftime( EFFECTIVE_TIME, prev_time, units="hours") ) ) ), by="orlogid" ]

merged_adt <- iculos  %>%
merge( postop_ICU_status[, .(ICU, unit, admit_day, orlogid) ] , by="orlogid", all= TRUE) %>%
merge( adt_data[, .(postop_los,readmission_survival, orlogid) ] , by="orlogid", all= TRUE) 

merged_adt[is.na(ICU), ICU:=FALSE ]
merged_adt[is.na(ICULoS), ICULoS:=0. ]

adt_tectonics <- merged_adt #[join_mrns[, .(orlogid=SurgicalRecordID)] , on="orlogid", nomatch=NULL]



adt_outcomes_merged <- rbind(adt_tectonics, merged_adt_act3mk2,merged_adt_act3) 
adt_outcomes_merged %<>% unique(by="orlogid")
# adt_outcomes_merged <- adt_outcomes_merged[, .(ICULoS = max(ICULoS[is.finite(ICULoS)], na.rm=T), ICU=any(ICU), postop_los=max(postop_los[is.finite(postop_los)]), readmission_survival=max(readmission_survival[is.finite(readmission_survival)] ) ),  by="orlogid"]

adt_outcomes_merged[!is.finite(readmission_survival), readmission_survival:= 365]
adt_outcomes_merged[(readmission_survival>365), readmission_survival:= 365]
adt_outcomes_merged[!is.finite(postop_los), postop_los:= 0]



setnames(adt_outcomes_merged, "orlogid", "SurgicalRecordID")
adt_outcomes_merged %>% fwrite("/research/intermediates/adt_imports.csv")




temp <- merge(
   adt_outcomes_merged[join_mrns, on="SurgicalRecordID", nomatch=NULL] [, roughdate := Date %>% roughen_date ][, .(UniqueCaseWithLoS=uniqueN(SurgicalRecordID)), by=.(roughdate, isTreatment )][roughdate>2019.25][order(roughdate)] ,
   cases_by_month_summary, by=c("roughdate", "isTreatment"), all.y=TRUE
   )[ , UniqueCaseWithLoS := fifelse(is.na(UniqueCaseWithLoS), 0,  UniqueCaseWithLoS)][, fractionWLoS := round( UniqueCaseWithLoS/UniqueCase,4)][order(roughdate, isTreatment)] 

temp  %>%  fwrite("/research/outputs/LoS_presense.csv")


copy(temp)[,isTreatment:=factor(isTreatment) ] %>% ggplot( aes(x=roughdate, y=fractionWLoS,  group = isTreatment, fill =isTreatment)) + geom_bar(stat = "identity", width=0.05, position=position_identity(), alpha=0.5) + labs(x = "Date", y = "Fraction", title = "LOS present by month") -> temp2
temp2 %>% ggsave(device="png", filename="/research/outputs/LoS_present.png")

