
## this is for compatability with lsf
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


overlaps <- function(x, y) { c(
length(setdiff(x,y)) ,
length(intersect(x,y)) ,
length(setdiff(y,x)) 
)}

roughen_date <- function(x) round(year(x) + (month(x)-1)/12,2)  


setwd("/research")
clarity_root <- '/early_act3/'


join_mrns <- fread("/research/intermediates/population.csv")
cases_by_month_summary <-  copy(join_mrns )[, roughdate := Date %>% roughen_date ][, .(UniqueCase=uniqueN(SurgicalRecordID) ), by=.(roughdate, isTreatment) ][roughdate>2019.25] [order(roughdate)]

main_pop <- read_xlsx("Template4TECTONICSData_Req21Mar2023.xlsx", sheet="Population_v2") %>% unique
main_pop  %<>% filter( (SurgicalRecordID %in% join_mrns$SurgicalRecordID))

ages_only<- main_pop   %>% mutate(Age=floor(as.numeric(difftime(Date, DoB, units="days"))/365.25) ) %>% select(orlogid = SurgicalRecordID, Age) %>% data.table %>% unique

all_prop <- fread("/research/intermediate_propofol.csv")

## This is needed to find the next AN stop if it is not in the analytic set
all_ids <- fread('2023_07_TECTONICS_PaperData/2023_07_21_IDsGalore.csv', sep="^", quote="") ##PAT_MRN_ID AN_52_ENC_CSN_ID AN_53_ENC_CSN_ID AN_3_ENC_CSN_ID INPATIENT_DATA_ID_52_INTRAOP INPATIENT_DATA_ID_3_HSP AN_PROC_NAME AN_START_DATETIME AN_STOP_DATETIME Anesthesia_Duration CASE_CLASS_NM SERVICE_NM LOG_ID ASA_SCORE_C EMERG_STATUS AN_TYPE


###########################
## Merge flowsheets for
# - - fresh gas [ <= 2L/min sevo <= 1L/min others]  -> need maintanace period (intubation and extubation times) DONE
# - - Temperature DONE
# - - MAP > 60 mmHg (% time) DONE
# - - mean airway pressure < 30 cmH20 (% time) DONE

## original ACT3
## intubation detectors:
##  PIP (false negative on spont)
##  ETCO2 (false positive on mask)
##  Mode (? false negatives and positives)
##  MV / PIP chaos (false positive on bucking, preparing for extubation)
##  Consider PIP > 10, PIP - PEEP > 5, TV > 200, PEEP >2, ETCO2 > 23 -> very high likelihood



all_prop <- all_prop[exclude_low_max==TRUE]

## make stop windows for resp failure: next an case 
join_mrns <- rbind(join_mrns, all_ids[, .( SurgicalRecordID=LOG_ID, MRN=PAT_MRN_ID ,AN_START_DATETIME, AN_STOP_DATETIME)][!join_mrns, on='SurgicalRecordID'] , fill=TRUE)
setorder(join_mrns, MRN, AN_START_DATETIME)
join_mrns[, numc := .N, by="MRN"]
join_mrns[numc==1, resp_stop:=AN_STOP_DATETIME+ddays(30) ]
join_mrns[numc>1, resp_stop:= pmin(shift(AN_START_DATETIME, type="lead") - dhours(1), AN_STOP_DATETIME+ddays(30), na.rm=T) , by='MRN']


join_mrns[numc==1, resp_start:=  AN_STOP_DATETIME - dhours(48) ]
join_mrns[numc>1, resp_start:= pmax(shift(AN_STOP_DATETIME, type="lag") + dhours(1) , AN_STOP_DATETIME - dhours(48), na.rm=T) , by='MRN']

join_mrns[, numc := NULL]
join_mrns_resp <- copy(join_mrns) ## this one is different because of the "chain backwards" of respiratory failure has to use non-tectonics cases
join_mrns <- join_mrns[is.finite(isTreatment)]

#############3
## Read RASS separately
#############3

RASS_raw <- fread('/research/2023_11_02_Clarity_Result_Set_RASS.csv', sep='^')
RASS_raw[, LINE :=NULL] 
RASS_raw[, ENTRY_TIME :=NULL] 
RASS_raw[, dropval := as.numeric(MEAS_VALUE) < -3]
RASS_raw <- RASS_raw[is.finite(dropval)]
RASS_raw[, MEAS_VALUE :=NULL] 
setkey(RASS_raw, PAT_MRN_ID, RECORDED_TIME)
setnames(RASS_raw, 'PAT_MRN_ID', 'MRN')

# if(TRUE) {
all_ts <- list.files('/early_act3' , full.names=TRUE, pattern="Clarity\\sHogue\\sResult\\sSet\\sFlowsheets\\s\\d")
all_ts %<>% grep(pattern="csv$", value=TRUE)

temp_holder <- vector('list', length(all_ts))
map_holder<- vector('list', length(all_ts))
agent_holder<- vector('list', length(all_ts))
fgf_holder<- vector('list', length(all_ts))
vent_holder<- vector('list', length(all_ts))
vent_holder2<- vector('list', length(all_ts))
flow_name_holder <- vector('list', length(all_ts))
cam_holder<- vector('list', length(all_ts))
sample_count  <- vector('list', length(all_ts))
big_vent_time <- vector('list', length(all_ts))
map_holder_post<- vector('list', length(all_ts))
for(ts_index in seq_along (all_ts) ) {
#  ts_index <- 1
epic_flow_1 <- fread(all_ts[ts_index] )
# drop_holder[[ts_index]]  <- epic_flow_1[ FLOWSHEET_NAME %chin% c("Early Detection of Sepsis" , "Monitor Check" ) , .(FLOWSHEET_NAME, FLO_MEAS_ID, MEASURE_NAME)] %>% unique

epic_flow_1 <- epic_flow_1[ FLOWSHEET_NAME != "Early Detection of Sepsis"][ MEASURE_NAME!="Monitor Check"]
epic_flow_1 <- join_mrns_resp[epic_flow_1, on="MRN==CurrentMRN", allow.cartesian=TRUE, nomatch=NULL]

setnames(epic_flow_1  , 'AN_START_DATETIME', 'AnestStart')
setnames(epic_flow_1  , 'AN_STOP_DATETIME', 'AnestStop')
setnames(epic_flow_1  , 'SurgicalRecordID', 'orlogid')

epic_flow_1 <- epic_flow_1[abs(as.numeric( difftime(AnestStart, PERFORMED,  units="days"))) < 30 ]

flow_name_holder[[ts_index]] <- epic_flow_1[, .(FLO_MEAS_ID, MEASURE_NAME)] %>% unique

## first 2 outcomes use a broader timeframe


#########
##preop and postop vent -2 +7 day window
## duration may be positive if reoperated
#########

  ## this may miss - the last event is probably to change value to stand by
vent_on <- epic_flow_1[MEASURE_NAME %chin% c("Vent Status" ,"Vent Mode" , "Adult Vent Mode" , "Ventilator Patient" ) ]

## since I am only interested in pre and postop events, filter them out
## this will be wrong in the super rare case that someone was previously on vent, extubated in the OR, and then goes back to surgery in the next 48 hours.
vent_on <-vent_on[ FLOWSHEET_NAME != "Anesthesia Monitoring"]
 temp <- epic_flow_1[ FLOWSHEET_NAME %chin% c("Anesthesia Monitoring" ) , .(FLOWSHEET_NAME, FLO_MEAS_ID, MEASURE_NAME)] %>% unique
 
vent_off <- vent_on [   (MEASURE_NAME=="Vent Status" & (VALUE == "Stand-by")) |
  (MEASURE_NAME=="Vent Mode" & (VALUE %chin% c("OFF", "", "AVAPS", "CPAP","Stand-by") )) |
  (MEASURE_NAME=="Adult Vent Mode" & (VALUE %chin% c("OFF", "", "AVAPS", "CPAP","Stand-by" , "Spontaneous") )) |
  (MEASURE_NAME=="Ventilator Patient" & (VALUE == "No")) ]
   

vent_on <- vent_on [   (MEASURE_NAME=="Vent Status" & !(VALUE == "Stand-by")) |
  (MEASURE_NAME=="Vent Mode" & !(VALUE %chin% c("OFF", "", "AVAPS", "CPAP","Stand-by", "Spontaneous") )) |
  (MEASURE_NAME=="Adult Vent Mode" & !(VALUE %chin% c("OFF", "", "AVAPS", "CPAP","Stand-by") )) |
  (MEASURE_NAME=="Ventilator Patient" & (VALUE == "Yes")) ]

## from 2 days to 2 hours pre
## the shortest interval to a documented "off" state
vent_off_pre <- vent_off[ (AnestStart -dhours(2) > PERFORMED ) & ( resp_start < PERFORMED  )  ]  
vent_off_pre <- vent_off_pre [ , .(dc_time = min(as.numeric( difftime(AnestStart, PERFORMED,  units="days")) ) ) , by="orlogid"  ]

## from 2 days to pre
## occasionally preop oxygen for block documented with Spont, or vent is just documented as on
## the shortest interval to a documented "on" state


vent_pre <- vent_on[ (AnestStart-dhours(1) > PERFORMED ) & (resp_start < PERFORMED ) ][!(VALUE %chin% c("Spontaneous", "Manual")) , .(preop_vent=min(as.numeric( difftime(AnestStart, PERFORMED,  units="days")) ))  , by="orlogid" ]

## check if vent off occurs more recently than vent on
vent_pre <- merge(vent_pre , vent_off_pre , all.x=TRUE, by="orlogid")
vent_pre [ is.na(dc_time),dc_time:= -Inf]
vent_pre [ ,preop_vent_b := case_when(
(preop_vent <3 ) & (! is.finite(dc_time)) ~ TRUE ,
preop_vent < dc_time ~ TRUE ,
dc_time < preop_vent ~ FALSE ,
(! is.finite(preop_vent)) & (! is.finite(dc_time)) ~ FALSE ,
TRUE ~ FALSE ) ## this occurs when the two are present and identically equal
]

## however, that doesn't really occur postop
## maximum interval to an on event in first 30 days
## any events in >24 hours triggers the outcome
## Because of the PSV status during weaning, this rarely misses  an "off"
## Very rare for daily status documentation not to occur
## transform to (start stop) pairs first, with the rationale of ignoring short intervals that likely represent procedural sedation
##   - small enough to self-join within orlogid
##   - 
vent_on_post <- merge(
  vent_on[PERFORMED > AnestStop + dhours(2) & PERFORMED < resp_stop, .(start=PERFORMED, orlogid, AnestStop)] ,
  vent_off[PERFORMED > AnestStop & PERFORMED < resp_stop, .(stop=PERFORMED, orlogid, AnestStop2=AnestStop)] ,
  by=c("orlogid"), allow.cartesian=TRUE, all=TRUE)
  
  ## handle some edge cases: (1) only a start or stop -> mark an an stop (2) only a stop -> assume never stopped from OR (3) only a start -> assume greater than threshold
  vent_on_post[is.na(AnestStop) ,AnestStop:= AnestStop2 ][,AnestStop2:=NULL ]
#  [is.na(start),start:=AnestStop ]
#   [is.na(stop) , stop:= start + dhours(48)]  
  ## only interested in start -> stop
  vent_on_post <- vent_on_post[!is.na(start)] [start<fcoalesce( stop ,start + dhours(48) ) ]
  ## only interested in the first stop after each stop
  ## note that multiple straggling stops after a start are dropped
  ## its possible that a vent used for bpap without an on triggers?
  setkey(vent_on_post , orlogid, start, stop)
  vent_on_post<- vent_on_post[vent_on_post[ , .I[1] , by=.(orlogid, start) ]$V1 ]
  ## only interested in the earliest start before each stop
  vent_on_post<- vent_on_post[vent_on_post[ , .I[1] , by=.(orlogid, stop) ]$V1 ]
  vent_on_post[, maxtime := max( start, stop, na.rm=TRUE ), by=.(orlogid) ]
  vent_on_post[is.na(stop) , stop:= maxtime + dhours(48)] 
  vent_on_post[, duration:= difftime(stop, start, units="hours") %>% as.numeric ]
  vent_on_post <- vent_on_post[duration > 4]
  
  vent_post <- vent_on_post[ , .(postop_vent_duration = max(difftime(stop, AnestStop, units="days") %>% as.numeric ) , number_events = .N, longest_duration = max(duration)/24, earliest_start=min( difftime(start, AnestStop, units="days") %>% as.numeric ) ) , by="orlogid"]
  
vent_on <- merge(vent_post, vent_pre[,.(preop_vent_b, orlogid)], all=TRUE , by="orlogid")
vent_on[is.infinite(postop_vent_duration),postop_vent_duration:=0.]
vent_on[is.na(preop_vent_b),preop_vent_b:=FALSE]


tracheostomy_yes <- join_mrns[ vent_post[, .(orlogid)] %>% unique , .(MRN), on="SurgicalRecordID==orlogid"] %>% unique
tracheostomy_yes <- epic_flow_1[tracheostomy_yes, on="MRN", nomatch=NULL, allow.cartesian=TRUE][PERFORMED < AnestStop] [(VALUE %ilike% "tracheo[s]t"), .(orlogid)] %>% unique
# epic_flow_1[PERFORMED < AnestStop] [(VALUE %ilike% "tracheo[s]t")]

vent_on[tracheostomy_yes , preop_vent_b:= TRUE , on="orlogid"]

vent_holder2[[ts_index]]<- vent_on


## the rest of the outcomes don't need the ineligible cases to be calculated
epic_flow_1 <- unique(join_mrns[, .(orlogid=SurgicalRecordID)]) [epic_flow_1, on="orlogid", allow.cartesian=FALSE, nomatch=NULL]

#########
##preop and postop delirium
#########


CAM_outcome <- epic_flow_1[MEASURE_NAME=="Overall CAM-ICU"]
CAM_outcome <- CAM_outcome[ abs(as.numeric(difftime(PERFORMED , AnestStop, units="days"))) <7 ]

RASS_measurements <- setkey(unique(CAM_outcome[,.(MRN, AnestStop)]), MRN)[ RASS_raw, on="MRN", allow.cartesian=TRUE]
setnames(RASS_measurements,"RECORDED_TIME", "PERFORMED" )
RASS_measurements <- RASS_measurements[ abs(as.numeric(difftime(PERFORMED , AnestStop, units="days"))) <7 ]

## drop CAM measures whose nearest within 30 min RASS is < -3
## make drop windows, match on MRN 
setorder(RASS_measurements , MRN, PERFORMED)
RASS_measurements[, window_end := pmin( PERFORMED + dminutes(30) , shift(PERFORMED, type="lead") , na.rm=TRUE), by="MRN" ] 
RASS_measurements[, window_begin := pmax( PERFORMED - dminutes(30) , shift(PERFORMED, type="lag") , na.rm=TRUE), by="MRN" ] 
RASS_measurements_drop_windows <- RASS_measurements[dropval==TRUE , .(MRN, window_begin, window_end)]

CAM_outcome[, camid := .I] 
setkey(CAM_outcome, camid)

CAM_outcome_with_low_RASS <- RASS_measurements_drop_windows[ CAM_outcome , on="MRN", allow.cartesian=TRUE, nomatch=NULL]

if(nrow(CAM_outcome_with_low_RASS) > 0) {
  CAM_outcome_with_low_RASS <-  CAM_outcome_with_low_RASS [ data.table::between(PERFORMED, window_begin, window_end) ]
  setkey(CAM_outcome_with_low_RASS , camid)
  CAM_outcome <- CAM_outcome[ !CAM_outcome_with_low_RASS, on="camid"]
}



CAM_outcome_pre <- CAM_outcome[PERFORMED < AnestStop , .(preop_del = any(VALUE == "Positive"), pre_num_checked=.N ) , by="orlogid" ]
CAM_outcome_post <- CAM_outcome[PERFORMED >= AnestStop , .(postop_del = any(VALUE == "Positive"), post_num_checked=.N ) , by="orlogid" ]
CAM_outcome <- merge( CAM_outcome_pre , CAM_outcome_post, by="orlogid", all=TRUE)

CAM_outcome [is.na(post_num_checked), post_num_checked:=0L, ]
CAM_outcome [is.na(pre_num_checked), pre_num_checked:=0L, ]
CAM_outcome [post_num_checked==0, postop_del:=FALSE ]
CAM_outcome [pre_num_checked==0, preop_del:=FALSE ]
cam_holder[[ts_index]]<- CAM_outcome


## postop map

bp_only <- epic_flow_1[MEASURE_NAME %in% c( 'Arterial BP' , 'NIBP (Mean)' , 'NIBP', "MAP (mmHg)",  "Arterial Line MAP (mmHg)" , "ARTERIAL MAP (2)", "BP",  "Arterial Line BP", "ARTERIAL BP (2)", "ARTERIAL BP")][ (PERFORMED > AnestStop + dminutes(15)) & (PERFORMED < AnestStop + ddays(3))]


bp_only [ , MAP := NA_real_]
bp_only [MEASURE_NAME %in% c('NIBP (Mean)', "MAP (mmHg)", "Arterial Line MAP (mmHg)" , "ARTERIAL MAP (2)"), MAP := as.numeric(VALUE)]
bp_only [MEASURE_NAME %in% c( 'Arterial BP' , 'NIBP', "BP",  "Arterial Line BP", "ARTERIAL BP (2)", "ARTERIAL BP"), c("sbp", "dbp"):= tstrsplit(x=VALUE, "/", fixed=TRUE)]
bp_only [, sbp:=as.numeric(sbp)]
bp_only [, dbp:=as.numeric(dbp)]
bp_only [is.finite(sbp) & is.finite(dbp), MAP:= dbp*.66 + sbp*.34]
# bp_only [is.finite(sbp) & is.finite(dbp), PP:= sbp - dbp]
bp_only <- bp_only[is.finite(MAP)]
bp_only <- bp_only[MAP < 180]
bp_only <- bp_only[MAP > 20]
bp_only <- bp_only[ , .(MAP = min(MAP)), by=c("orlogid", "PERFORMED" ) ]

setorder(bp_only , orlogid, PERFORMED)
bp_only[ , min_time := min(PERFORMED) , by="orlogid"]

bp_only [, Time := as.numeric(difftime(PERFORMED, min_time, units="mins")) ]
bp_only [,deltaT := c(diff(Time), 0.) , by="orlogid" ] 
bp_only[, deltat2 := pmax(65-MAP,0)*deltaT ]

bp_only <- bp_only [  , .(low_map_time=sum(deltaT*(MAP< 60), na.rm=TRUE), total_map_time=max(Time, na.rm=TRUE) , low_map_auc = sum(deltat2, na.rm=T), twa_map=sum(deltaT*(MAP), na.rm=TRUE ) ), by="orlogid"]

map_holder_post[[ts_index]]<- bp_only 

## narrower time frame for all others
epic_flow_1 <-epic_flow_1[PERFORMED > AnestStart & PERFORMED < AnestStop - dminutes(6)] ## 6 minutes to avoid first PACU or ICU measure. This only doesn't work in patients who die on the table, which is very rare

sample_count[[ts_index]] <- epic_flow_1[, .N, by="orlogid"]

#########
## temp outcome - success
#########

temperature_measures <- c("Temp" ,"Core Temp" , "Temperature - Venous" , "Temperature - Arterial" ,  "Esophageal Temperature" , "Core (Body) Temperature")

temperature_intraop <- epic_flow_1[MEASURE_NAME %in% temperature_measures]
temperature_intraop [ , VALUE:=as.numeric(VALUE)]
temperature_intraop <- temperature_intraop [ is.finite(VALUE)]
temperature_intraop [ , VALUE:=fcase(VALUE > 48, (VALUE-32.)*5/9  , VALUE <= 48, VALUE) ]

## there is unfortunate overlap between the coldest rooms and hottest people in different units
temperature_intraop <- temperature_intraop [ VALUE > 34.] ## this is probe out of body
setorder(temperature_intraop, orlogid, -PERFORMED )
## generate last 10 observations
# temperature_intraop <- temperature_intraop[ , .SD[1:10, .( last_temp=max(VALUE, na.rm=TRUE))] , by="orlogid"]
temperature_intraop <- temperature_intraop[ , .( last_temp=max(VALUE[1:10], na.rm=TRUE), mean_temp=mean(VALUE, na.rm=TRUE)  ) , by="orlogid"]
temperature_intraop[ , temp_ever_measured := is.finite(last_temp)]
temperature_intraop[ , temp_good := last_temp >= 36.0]

temp_holder[[ts_index]] <- temperature_intraop

rm(temperature_intraop)


##########
## BP management
##########
bp_only <- epic_flow_1[MEASURE_NAME %in% c( 'Arterial BP' , 'NIBP (Mean)' , 'NIBP', "MAP (mmHg)",  "Arterial Line MAP (mmHg)" , "ARTERIAL MAP (2)", "BP",  "Arterial Line BP", "ARTERIAL BP (2)", "ARTERIAL BP")]
bp_only [ , MAP := NA_real_]
bp_only [MEASURE_NAME %in% c('NIBP (Mean)', "MAP (mmHg)", "Arterial Line MAP (mmHg)" , "ARTERIAL MAP (2)"), MAP := as.numeric(VALUE)]
bp_only [MEASURE_NAME %in% c( 'Arterial BP' , 'NIBP', "BP",  "Arterial Line BP", "ARTERIAL BP (2)", "ARTERIAL BP"), c("sbp", "dbp"):= tstrsplit(x=VALUE, "/", fixed=TRUE)]
bp_only [, sbp:=as.numeric(sbp)]
bp_only [, dbp:=as.numeric(dbp)]
bp_only [is.finite(sbp) & is.finite(dbp), MAP:= dbp*.66 + sbp*.34]
# bp_only [is.finite(sbp) & is.finite(dbp), PP:= sbp - dbp]
bp_only <- bp_only[is.finite(MAP)]
bp_only <- bp_only[MAP < 180]
bp_only <- bp_only[MAP > 20]
bp_only <- bp_only[ , .(MAP = min(MAP)), by=c("orlogid", "PERFORMED" ) ]

setorder(bp_only , orlogid, PERFORMED)
bp_only[ , min_time := min(PERFORMED) , by="orlogid"]

bp_only [, Time := as.numeric(difftime(PERFORMED, min_time, units="mins")) ]
bp_only [,deltaT := c(diff(Time), 0.) , by="orlogid" ] 
bp_only[, deltat2 := pmax(65-MAP,0)*deltaT ]

bp_only <- bp_only [  , .(low_map_time=sum(deltaT*(MAP< 60), na.rm=TRUE), total_map_time=max(Time, na.rm=TRUE)  , low_map_auc = sum(deltat2, na.rm=T) , twa_map=sum(deltaT*(MAP), na.rm=TRUE) ) , by="orlogid"]

map_holder[[ts_index]]<- bp_only 


########
## Airway pressures
## NOTE: we do not measure mean airway pressure!
########

tv_only <- epic_flow_1[MEASURE_NAME %in% c( 'Tidal (Observed)' , 'Tidal Volume Exp (mL)'), .(orlogid, PERFORMED, tv=as.numeric(VALUE) ) ][is.finite(tv)][, .(tv = min(tv)), , by=c("orlogid", "PERFORMED")]

tv_only <- merge(tv_only , epic_flow_1[MEASURE_NAME == 'PIP Observed' , .(orlogid, PERFORMED, pip=as.numeric(VALUE) ) ][is.finite(pip)] , by=c("orlogid", "PERFORMED"), all=TRUE)

tv_only <- merge(tv_only , epic_flow_1[MEASURE_NAME == 'ETCO2' , .(orlogid, PERFORMED, ETCO2=as.numeric(VALUE) ) ][is.finite(ETCO2)] , by=c("orlogid", "PERFORMED"), all=TRUE)

tv_only <- merge(tv_only , epic_flow_1[MEASURE_NAME == 'PEEP/CPAP' , .(orlogid, PERFORMED, PEEP=as.numeric(VALUE) ) ][is.finite(PEEP)] , by=c("orlogid", "PERFORMED"), all=TRUE)

setorder(tv_only, orlogid, PERFORMED)
## Derive likely intubation / extubation limits
## this definition excludes some time Spontaneous vent or very low pressure support; however, those patients are at very low risk for (low anesthetic, high pressures)
## This assumes that ventilation is a single event (i.e. it breaks on reintubation)
tv_only[, vent_likely := (PEEP > 2) & (pip - PEEP >= 3) & (ETCO2 > 20) & tv > 150 ]
tv_only <- tv_only[vent_likely == TRUE]
tv_only[, N := .N , by="orlogid"]
## look for 15 minutes on either side; likely enough to avoid mask vent pre and post
tv_only<-tv_only[N>30]

vent_times<- merge( 
   tv_only[vent_likely == TRUE , .SD[8, .(vent_start=PERFORMED, N) ], by="orlogid" ] ,
   tv_only[vent_likely == TRUE , .SD[.N-8, .(vent_stop=PERFORMED) ], by="orlogid" ] , by= "orlogid")

big_vent_time[[ts_index]] <- vent_times   

vent_times[, total_vent_time := difftime(vent_stop, vent_start, units="mins") %>% as.numeric]

vent_holder[[ts_index]] <- vent_times[ tv_only, on="orlogid", allow.cartesian=TRUE , nomatch=NULL ][data.table::between(PERFORMED, vent_start, vent_stop), .( pip_mean=mean( pip) , pip_good_frac=mean( pip < 30 ) )  , by= "orlogid" ]


##########
## volatile use - success
##########

# epic_flow_1$MEASURE_NAME %>% unique %>% grep(pattern='N2O', value=T) 

an_agents <- epic_flow_1[MEASURE_NAME %in% c('O2', 'N2O', 'Air')] 
an_agents[ , VALUE := as.numeric(VALUE)] 
an_agents <-an_agents[is.finite(VALUE), nomatch=NULL]

an_agents <- vent_times[ an_agents, on="orlogid", allow.cartesian=TRUE ][data.table::between(PERFORMED, vent_start, vent_stop)]

## do five minute blocks to smooth out variation
fgf <- an_agents [ , .(fgf=sum(VALUE , na.rm=TRUE) ), by = c('orlogid' , 'PERFORMED')]
# fgf [ , time_block := as.numeric(difftime(PERFORMED, vent_start, units='mins')) %/% 5 ]
# fgf <- fgf [, .(fgf = mean(fgf) ) , by=c("orlogid", "time_block")]
# 
## there are some rooms with maual fgf, where it is only defined at moments of change
## so locf between observations
setkey(fgf, orlogid, PERFORMED)
fgf[, delta := difftime(shift(PERFORMED, type="lead"), PERFORMED , units="mins") %>% as.numeric %>% fcoalesce(1) %>% as.integer , by="orlogid"] 
fgf[, oldI := .I]
fgf <- fgf[ rep(oldI, times=delta) ]
## this could be much faster by pre-computing the difference
setkey(fgf, orlogid, PERFORMED)
fgf[delta>1, PERFORMED := PERFORMED + dminutes(seq(.N)-1), by=.(orlogid, PERFORMED) ] 
fgf[,oldI :=NULL]
fgf[,delta :=NULL]

agent2 <- epic_flow_1[MEASURE_NAME %chin% c('Expired Desflurane' , 'Expired Isoflurane', 'Expired Sevoflurane', "N2O Expired") ]
agent2 <-vent_times[agent2 , on="orlogid", allow.cartesian=TRUE, nomatch=NULL]
agent2 <-agent2 [(PERFORMED < vent_stop) & (PERFORMED > vent_start ) ]
agent2[ , VALUE := as.numeric(VALUE)] 
agent2<-agent2[is.finite(VALUE)]

## get rid of very low values, as these are often trace gas at the beginning and end
agent2 <- agent2[ VALUE > fcase(MEASURE_NAME == 'Expired Desflurane', .5, MEASURE_NAME == 'Expired Isoflurane', 0.1,MEASURE_NAME ==  'Expired Sevoflurane' , 0.15, MEASURE_NAME ==  "N2O Expired", .1 )   ]

## it is easier to LOCF here than after the merge
setkey(agent2, orlogid,MEASURE_NAME, PERFORMED)
agent2[, delta := difftime(shift(PERFORMED, type="lead"), PERFORMED , units="mins") %>% as.numeric %>% fcoalesce(1) %>% as.integer , by=c("orlogid", "MEASURE_NAME")] 
agent2[, oldI := .I]
agent2 <- agent2[ rep(oldI, times=delta) ]
## this could be much faster by pre-computing the difference with a parallen seq (that exists somewhere), but it's not slow now
setkey(agent2, orlogid, PERFORMED, MEASURE_NAME)
agent2[delta>1, PERFORMED := PERFORMED + dminutes(seq(.N)-1), by=.(orlogid, PERFORMED, MEASURE_NAME) ] 
agent2[,oldI :=NULL]
agent2[,delta :=NULL]


fgf <- merge(fgf, agent2, by=c("orlogid", "PERFORMED"))

fgf[ VALUE > fcase(MEASURE_NAME == 'Expired Desflurane', 2, MEASURE_NAME == 'Expired Isoflurane', 0.3,MEASURE_NAME ==  'Expired Sevoflurane' , 0.6 )  , efficent := fgf <= fcase(MEASURE_NAME == 'Expired Desflurane', 1.01 , MEASURE_NAME == 'Expired Isoflurane', 1.01 ,MEASURE_NAME ==  'Expired Sevoflurane' , 2.01 ) ]

fgf_holder[[ts_index]] <- fgf[ is.finite(efficent), .(eff_flow = mean(efficent)) , by="orlogid"]

## if vent time is not included, add it with value greater ~ 1 mac (to start a jump, assuming induction drug had been given)
## actually, because I used the inequality, just always add
## using the select operator means that I don't have to worry about other fields
## using the select operator also means that TIVA / sedation cases aren't included here 
## I am a little worried that this messes up the fgf outcome for cases that start tiva, BUT I don't want to locf from this measure, and the fgf outcome drops the first 8 observations

## actually, easier to handle this at the end using the first observed agent time
# agent2 <- rbind(agent2 , agent2[agent2[ , .I[1], by="orlogid" ]$V1][, PERFORMED := vent_start][,VALUE:= fcase(MEASURE_NAME == 'Expired Desflurane', 6 , MEASURE_NAME == 'Expired Isoflurane', 1. ,MEASURE_NAME ==  'Expired Sevoflurane' , 2.01, MEASURE_NAME =="N2O Expired", .6   ) ] )

agent3<- agent2[ , .(mac=sum(VALUE / fcase(MEASURE_NAME == 'Expired Desflurane', 6.6 , MEASURE_NAME == 'Expired Isoflurane', 1.2 ,MEASURE_NAME ==  'Expired Sevoflurane' , 1.8, MEASURE_NAME =="N2O Expired", 1.04 ) , na.rm=TRUE) ), by = c('orlogid' , 'PERFORMED') ]
agent3<- agent3[is.finite(mac)]



## age adjustment
agent3 <- ages_only[agent3, on="orlogid", nomatch=NULL ][, mac := mac*10^ (-0.00269*(Age - 40) ) ]
## To get 15 minute block of no agent, take only the yes-agent times, vent+15 and diff them. 
## I need to add a "yes" time for each unique med observation within the relevant orlogid and vent times

local_prop <-vent_times[ all_prop[ orlogid %in% unique(vent_times$orlogid)   ], on="orlogid", nomatch=NULL][data.table::between(TAKEN_TIME, vent_start, vent_stop)][ , .(orlogid, PERFORMED=TAKEN_TIME, mac = 0.5)]

agent3 <- agent3[mac >= 0.3 ]
agent3[, Age := NULL] 

agent3 <- rbind(agent3, local_prop )

setkey(agent3, orlogid, PERFORMED)
agent3[, jump := as.numeric(difftime(shift(PERFORMED, type="lead"), PERFORMED,  units="mins" ) ) > 15, by="orlogid"]

#  join_mrns[temp[njumps>0], on="SurgicalRecordID==orlogid"] 
agent3 <- agent3[, .(minyes = min(PERFORMED), njumps = sum(jump, na.rm=T), minjump=min(PERFORMED[jump %in% TRUE]) ) , by="orlogid"]
agent3 <- vent_times[agent3 , on="orlogid", allow.cartesian=TRUE, nomatch=NULL]
agent3[ as.numeric(difftime(minyes, vent_start, units="min" ) ) >15, njumps := njumps +1 ]

agent_holder[[ts_index]]<- agent3[ , .( orlogid, njumps,  minjump) ]




}

## This subsequent query did not include measure names

used_pre_flow <- fread("/research2/ActFast_Intermediates/all_flowsheet_names.csv")
used_pre_flow <-unique(used_pre_flow[, .(MEASURE_NAME, FLO_MEAS_ID)] , by="FLO_MEAS_ID")

## ACT3 update
flow_files <- c(
"/mark2_act3/2023_02_Flowsheets_202101.csv", 
"/mark2_act3/2023_02_Flowsheets_202102.csv", 
"/mark2_act3/2023_02_Flowsheets_202103.csv", 
"/mark2_act3/2023_02_Flowsheets_202104.csv", 
"/mark2_act3/2023_02_Flowsheets_202105.csv", 
"/mark2_act3/2023_02_Flowsheets_202106.csv", 
"/mark2_act3/2023_02_Flowsheets_202107.csv", 
"/mark2_act3/2023_02_Flowsheets_202108.csv", 
"/mark2_act3/2023_02_Flowsheets_202109.csv", 
"/mark2_act3/2023_02_Flowsheets_2021Q4.csv",
"/mark2_act3/2023_02_Flowsheets_2022Q1.csv",
"/mark2_act3/2023_02_Flowsheets_2022Q2.csv",
"/mark2_act3/2023_02_Flowsheets_2022Q3.csv",
"/mark2_act3/2023_02_Flowsheets_2022Q4.csv",
"/mark2_act3/2023_11_Flowsheets_2023Q1.csv"
)
# used_pre_flow <-  fread("/research/used_flow_intra.csv")

temp_holder_up  <- vector('list', length(flow_files))
map_holder_up   <- vector('list', length(flow_files))
agent_holder_up <- vector('list', length(flow_files))
fgf_holder_up   <- vector('list', length(flow_files))
vent_holder_up  <- vector('list', length(flow_files))
vent_holder2_up  <- vector('list', length(flow_files))
cam_holder_up  <- vector('list', length(flow_files))
sample_count_up  <- vector('list', length(flow_files))
big_vent_time_up <- vector('list', length(flow_files))
map_holder_post_up<- vector('list', length(flow_files))



for(ts_index in seq_along (flow_files) ) {
epic_flow_1 <- fread(flow_files[ts_index], sep="^", quote="" )
epic_flow_1 <- used_pre_flow[epic_flow_1, on='FLO_MEAS_ID', nomatch=NULL]

setnames(epic_flow_1, "RECORDED_TIME", "PERFORMED")
setnames(epic_flow_1  , 'PAT_MRN_ID', 'CurrentMRN')
setnames(epic_flow_1  , 'MEAS_VALUE', 'VALUE')

epic_flow_1 <- join_mrns_resp[epic_flow_1, on="MRN==CurrentMRN", allow.cartesian=TRUE, nomatch=NULL]

setnames(epic_flow_1  , 'AN_START_DATETIME', 'AnestStart')
setnames(epic_flow_1  , 'AN_STOP_DATETIME', 'AnestStop')
setnames(epic_flow_1  , 'SurgicalRecordID', 'orlogid')

epic_flow_1 <- epic_flow_1[abs(as.numeric( difftime(AnestStart, PERFORMED,  units="days"))) < 30 ]



## first 2 outcomes use a broader timeframe

#########
##preop and postop vent -2 +7 day window
## duration may be positive if reoperated
#########

  ## this may miss - the last event is probably to change value to stand by
vent_on <- epic_flow_1[MEASURE_NAME %chin% c("Vent Status" ,"Vent Mode" , "Adult Vent Mode" , "Ventilator Patient" ) ]

## since I am only interested in pre and postop events, filter them out
## this will be wrong in the super rare case that someone was previously on vent, extubated in the OR, and then goes back to surgery in the next 48 hours.
# vent_on <-vent_on[ FLOWSHEET_NAME != "Anesthesia Monitoring"]
## Since this q doesn't have flow name, drop intraop vent modes
vent_on <-vent_on[  ! data.table::between(PERFORMED, AnestStart, AnestStop)]

vent_off <- vent_on [   (MEASURE_NAME=="Vent Status" & (VALUE == "Stand-by")) |
  (MEASURE_NAME=="Vent Mode" & (VALUE %chin% c("OFF", "", "AVAPS", "CPAP","Stand-by", "Spontaneous") )) |
  (MEASURE_NAME=="Adult Vent Mode" & (VALUE %chin% c("OFF", "", "AVAPS", "CPAP","Stand-by" , "Spontaneous") )) |
  (MEASURE_NAME=="Ventilator Patient" & (VALUE == "No")) ]
   

vent_on <- vent_on [   (MEASURE_NAME=="Vent Status" & !(VALUE == "Stand-by")) |
  (MEASURE_NAME=="Vent Mode" & !(VALUE %chin% c("OFF", "", "AVAPS", "CPAP","Stand-by", "Spontaneous") )) |
  (MEASURE_NAME=="Adult Vent Mode" & !(VALUE %chin% c("OFF", "", "AVAPS", "CPAP","Stand-by", "Spontaneous") )) |
  (MEASURE_NAME=="Ventilator Patient" & (VALUE == "Yes")) ]

## from 2 days to 2 hours pre
## the shortest interval to a documented "off" state
vent_off_pre <- vent_off[ (AnestStart -dhours(2) > PERFORMED ) & ( resp_start < PERFORMED  )  ]  
vent_off_pre <- vent_off_pre [ , .(dc_time = min(as.numeric( difftime(AnestStart, PERFORMED,  units="days")) ) ) , by="orlogid"  ]


## from 2 days to pre
## occasionally preop oxygen for block documented with Spont, or vent is just documented as on
## the shortest interval to a documented "on" state
vent_pre <- vent_on[ (AnestStart > PERFORMED ) & (resp_start < PERFORMED) ][!(VALUE %chin% c("Spontaneous", "Manual")) , .(preop_vent=min(as.numeric( difftime(AnestStart, PERFORMED,  units="days")) ))  , by="orlogid" ]

## check if vent off occurs more recently than vent on
vent_pre <- merge(vent_pre , vent_off_pre , all.x=TRUE, by="orlogid")
vent_pre [ is.na(dc_time),dc_time:= -Inf]
vent_pre [ ,preop_vent_b := case_when(
(preop_vent <3 ) & (! is.finite(dc_time)) ~ TRUE ,
preop_vent < dc_time ~ TRUE ,
dc_time < preop_vent ~ FALSE ,
(! is.finite(preop_vent)) & (! is.finite(dc_time)) ~ FALSE ,
TRUE ~ FALSE ) ## this occurs when the two are present and identically equal
]

## however, that doesn't really occur postop
## maximum interval to an on event in first 30 days
## any events in >24 hours triggers the outcome
## Because of the PSV status during weaning, this rarely misses  an "off"
## Very rare for daily status documentation not to occur
## transform to (start stop) pairs first, with the rationale of ignoring short intervals that likely represent procedural sedation
##   - small enough to self-join within orlogid
##   - isolated "offs" occur because someone checks "ventilator = no"
vent_on_post <- merge(
  vent_on[PERFORMED > AnestStop + dhours(2) & PERFORMED < resp_stop, .(start=PERFORMED, orlogid, AnestStop)] ,
  vent_off[PERFORMED > AnestStop & PERFORMED < resp_stop, .(stop=PERFORMED, orlogid, AnestStop2=AnestStop)] ,
  by=c("orlogid"), allow.cartesian=TRUE, all=TRUE)

  ## handle some edge cases: (1) only a start or stop -> mark an an stop (2) only a stop -> assume never stopped from OR (3) only a start -> assume greater than threshold
  vent_on_post[is.na(AnestStop) ,AnestStop:= AnestStop2 ][,AnestStop2:=NULL ]
#   [is.na(start),start:=AnestStop ]
#   [is.na(stop) , stop:= start + dhours(48)]  
  ## only interested in start -> stop
  vent_on_post <- vent_on_post[!is.na(start)] [start<fcoalesce( stop ,start + dhours(48) ) ]
  ## only interested in the first stop after each stop
  ## note that multiple straggling stops after a start are dropped
  ## its possible that a vent used for bpap without an on triggers?
  setkey(vent_on_post , orlogid, start, stop)
  vent_on_post<- vent_on_post[vent_on_post[ , .I[1] , by=.(orlogid, start) ]$V1 ]
  ## only interested in the earliest start before each stop
  vent_on_post<- vent_on_post[vent_on_post[ , .I[1] , by=.(orlogid, stop) ]$V1 ]
  vent_on_post[, maxtime := max( start, stop, na.rm=TRUE ), by=.(orlogid) ]
  vent_on_post[is.na(stop) , stop:= maxtime + dhours(48)] 
  vent_on_post[, duration:= difftime(stop, start, units="hours") %>% as.numeric ]
  vent_on_post <- vent_on_post[duration > 4]
  
  vent_post <- vent_on_post[ , .(postop_vent_duration = max(difftime(stop, AnestStop, units="days") %>% as.numeric ) , number_events = .N, longest_duration = max(duration)/24, earliest_start=min( difftime(start, AnestStop, units="days") %>% as.numeric ) ) , by="orlogid"]
 

vent_on <- merge(vent_post, vent_pre[,.(preop_vent_b, orlogid)], all=TRUE , by="orlogid")
vent_on[is.infinite(postop_vent_duration),postop_vent_duration:=0.]
vent_on[is.na(preop_vent_b),preop_vent_b:=FALSE]

tracheostomy_yes <- join_mrns[ vent_post[, .(orlogid)] %>% unique , .(MRN), on="SurgicalRecordID==orlogid"] %>% unique
tracheostomy_yes <- epic_flow_1[tracheostomy_yes, on="MRN", nomatch=NULL, allow.cartesian=TRUE][PERFORMED < AnestStop] [(VALUE %ilike% "tracheo[s]t"), .(orlogid)] %>% unique
# epic_flow_1[PERFORMED < AnestStop] [(VALUE %ilike% "tracheo[s]t")]

vent_on[tracheostomy_yes , preop_vent_b:= TRUE , on="orlogid"]

vent_holder2_up[[ts_index]]<- vent_on

## the rest of the outcomes don't need the ineligible cases to be calculated
epic_flow_1 <- unique(join_mrns[, .(orlogid=SurgicalRecordID)]) [epic_flow_1, on="orlogid", allow.cartesian=FALSE, nomatch=NULL]


#########
##preop and postop delirium
#########


CAM_outcome <- epic_flow_1[MEASURE_NAME=="Overall CAM-ICU"]
CAM_outcome <- CAM_outcome[ abs(as.numeric(difftime(PERFORMED , AnestStop, units="days"))) <7 ]

RASS_measurements <- setkey(unique(CAM_outcome[,.(MRN, AnestStop)]), MRN)[ RASS_raw, on="MRN", allow.cartesian=TRUE]
setnames(RASS_measurements,"RECORDED_TIME", "PERFORMED" )
RASS_measurements <- RASS_measurements[ abs(as.numeric(difftime(PERFORMED , AnestStop, units="days"))) <7 ]

## drop CAM measures whose nearest within 30 min RASS is < -3
## make drop windows, match on MRN 
setorder(RASS_measurements , MRN, PERFORMED)
RASS_measurements[, window_end := pmin( PERFORMED + dminutes(30) , shift(PERFORMED, type="lead") , na.rm=TRUE), by="MRN" ] 
RASS_measurements[, window_begin := pmax( PERFORMED - dminutes(30) , shift(PERFORMED, type="lag") , na.rm=TRUE), by="MRN" ] 
RASS_measurements_drop_windows <- RASS_measurements[dropval==TRUE , .(MRN, window_begin, window_end, dropval)]


CAM_outcome[, camid := .I] 
setkey(CAM_outcome, camid)

CAM_outcome_with_low_RASS <- RASS_measurements_drop_windows[ CAM_outcome , on="MRN", allow.cartesian=TRUE, nomatch=NULL]
if(nrow(CAM_outcome_with_low_RASS) > 0) {
  CAM_outcome_with_low_RASS <- CAM_outcome_with_low_RASS [ data.table::between(PERFORMED, window_begin, window_end) ]
  setkey(CAM_outcome_with_low_RASS , camid)
  CAM_outcome <- CAM_outcome[ !CAM_outcome_with_low_RASS, on="camid"]
}

CAM_outcome_pre <- CAM_outcome[PERFORMED < AnestStop , .(preop_del = any(VALUE == "Positive"), pre_num_checked=.N ) , by="orlogid" ]
CAM_outcome_post <- CAM_outcome[PERFORMED > AnestStop , .(postop_del = any(VALUE == "Positive"), post_num_checked=.N ) , by="orlogid" ]
CAM_outcome <- merge( CAM_outcome_pre , CAM_outcome_post, by="orlogid", all=TRUE)

CAM_outcome [is.na(post_num_checked), post_num_checked:=0L, ]
CAM_outcome [is.na(pre_num_checked), pre_num_checked:=0L, ]
CAM_outcome [post_num_checked==0, postop_del:=FALSE ]
CAM_outcome [pre_num_checked==0, preop_del:=FALSE ]
cam_holder_up[[ts_index]]<- CAM_outcome


## postop map

bp_only <- epic_flow_1[MEASURE_NAME %in% c( 'Arterial BP' , 'NIBP (Mean)' , 'NIBP', "MAP (mmHg)",  "Arterial Line MAP (mmHg)" , "ARTERIAL MAP (2)", "BP",  "Arterial Line BP", "ARTERIAL BP (2)", "ARTERIAL BP")][ (PERFORMED > AnestStop + dminutes(15)) & (PERFORMED < AnestStop + ddays(3))]


bp_only [ , MAP := NA_real_]
bp_only [MEASURE_NAME %in% c('NIBP (Mean)', "MAP (mmHg)", "Arterial Line MAP (mmHg)" , "ARTERIAL MAP (2)"), MAP := as.numeric(VALUE)]
bp_only [MEASURE_NAME %in% c( 'Arterial BP' , 'NIBP', "BP",  "Arterial Line BP", "ARTERIAL BP (2)", "ARTERIAL BP"), c("sbp", "dbp"):= tstrsplit(x=VALUE, "/", fixed=TRUE)]
bp_only [, sbp:=as.numeric(sbp)]
bp_only [, dbp:=as.numeric(dbp)]
bp_only [is.finite(sbp) & is.finite(dbp), MAP:= dbp*.66 + sbp*.34]
# bp_only [is.finite(sbp) & is.finite(dbp), PP:= sbp - dbp]
bp_only <- bp_only[is.finite(MAP)]
bp_only <- bp_only[MAP < 180]
bp_only <- bp_only[MAP > 20]
bp_only <- bp_only[ , .(MAP = min(MAP)), by=c("orlogid", "PERFORMED" ) ]

setorder(bp_only , orlogid, PERFORMED)
bp_only[ , min_time := min(PERFORMED) , by="orlogid"]

bp_only [, Time := as.numeric(difftime(PERFORMED, min_time, units="mins")) ]
bp_only [,deltaT := c(diff(Time), 0.) , by="orlogid" ] 
bp_only[, deltat2 := pmax(65-MAP,0)*deltaT ]

bp_only <- bp_only [  , .(low_map_time=sum(deltaT*(MAP< 60), na.rm=TRUE), total_map_time=max(Time, na.rm=TRUE) , low_map_auc = sum(deltat2, na.rm=T), twa_map=sum(deltaT*(MAP), na.rm=TRUE ) ), by="orlogid"]

map_holder_post_up[[ts_index]]<- bp_only 

#########
## temp outcome - success
#########
## narrower time frame
epic_flow_1 <-epic_flow_1[PERFORMED > AnestStart & PERFORMED < AnestStop - dminutes(6)]

sample_count_up[[ts_index]] <- epic_flow_1[, .N, by="orlogid"]


temperature_intraop <- epic_flow_1[MEASURE_NAME %in% temperature_measures]
temperature_intraop [ , VALUE:=as.numeric(VALUE)]
temperature_intraop <- temperature_intraop [ is.finite(VALUE)]
temperature_intraop [ , VALUE:=fcase(VALUE > 48, (VALUE-32.)*5/9  , VALUE <= 48, VALUE) ]

## there is unfortunate overlap between the coldest rooms and hottest people in different units
temperature_intraop <- temperature_intraop [ VALUE > 34.] ## this is probe out of body
setorder(temperature_intraop, orlogid, -PERFORMED )
## generate last 10 observations
temperature_intraop <- temperature_intraop[ , .( last_temp=max(VALUE[1:10], na.rm=TRUE), mean_temp=mean(VALUE, na.rm=TRUE)  ) , by="orlogid"]
# temperature_intraop <- temperature_intraop[ , .SD[1:10, .( last_temp=max(VALUE, na.rm=TRUE))] , by="orlogid"]
temperature_intraop[ , temp_ever_measured := is.finite(last_temp)]
temperature_intraop[ , temp_good := last_temp >= 36.0]

temp_holder_up[[ts_index]] <- temperature_intraop


##########
## BP management
##########
bp_only <- epic_flow_1[MEASURE_NAME %in% c( 'Arterial BP' , 'NIBP (Mean)' , 'NIBP', "MAP (mmHg)",  "Arterial Line MAP (mmHg)" , "ARTERIAL MAP (2)", "BP",  "Arterial Line BP", "ARTERIAL BP (2)", "ARTERIAL BP")]
bp_only [ , MAP := NA_real_]
bp_only [MEASURE_NAME %in% c('NIBP (Mean)', "MAP (mmHg)", "Arterial Line MAP (mmHg)" , "ARTERIAL MAP (2)"), MAP := as.numeric(VALUE)]
bp_only [MEASURE_NAME %in% c( 'Arterial BP' , 'NIBP', "BP",  "Arterial Line BP", "ARTERIAL BP (2)", "ARTERIAL BP"), c("sbp", "dbp"):= tstrsplit(x=VALUE, "/", fixed=TRUE)]
bp_only [, sbp:=as.numeric(sbp)]
bp_only [, dbp:=as.numeric(dbp)]
bp_only [is.finite(sbp) & is.finite(dbp), MAP:= dbp*.66 + sbp*.34]
# bp_only [is.finite(sbp) & is.finite(dbp), PP:= sbp - dbp]
bp_only <- bp_only[is.finite(MAP)]
bp_only <- bp_only[MAP < 180]
bp_only <- bp_only[MAP > 20]
bp_only <- bp_only[ , .(MAP = min(MAP)), by=c("orlogid", "PERFORMED" ) ]

setorder(bp_only , orlogid, PERFORMED)
bp_only[ , min_time := min(PERFORMED) , by="orlogid"]

bp_only [, Time := as.numeric(difftime(PERFORMED, min_time, units="mins")) ]
bp_only [,deltaT := c(diff(Time), 0.) , by="orlogid" ] 
bp_only[, deltat2 := pmax(65-MAP,0)*deltaT ]

bp_only <- bp_only [  , .(low_map_time=sum(deltaT*(MAP< 60), na.rm=TRUE), total_map_time=max(Time, na.rm=TRUE) , low_map_auc = sum(deltat2, na.rm=T), twa_map=sum(deltaT*(MAP), na.rm=TRUE ) ) , by="orlogid"]

map_holder_up[[ts_index]]<- bp_only 



########
## Airway pressures
########

tv_only <- epic_flow_1[MEASURE_NAME %in% c( 'Tidal (Observed)' , 'Tidal Volume Exp (mL)'), .(orlogid, PERFORMED, tv=as.numeric(VALUE) ) ][is.finite(tv)][, .(tv = min(tv)), , by=c("orlogid", "PERFORMED")]

tv_only <- merge(tv_only , epic_flow_1[MEASURE_NAME == 'PIP Observed' , .(orlogid, PERFORMED, pip=as.numeric(VALUE) ) ][is.finite(pip)] , by=c("orlogid", "PERFORMED"), all=TRUE)

tv_only <- merge(tv_only , epic_flow_1[MEASURE_NAME == 'ETCO2' , .(orlogid, PERFORMED, ETCO2=as.numeric(VALUE) ) ][is.finite(ETCO2)] , by=c("orlogid", "PERFORMED"), all=TRUE)

tv_only <- merge(tv_only , epic_flow_1[MEASURE_NAME == 'PEEP/CPAP' , .(orlogid, PERFORMED, PEEP=as.numeric(VALUE) ) ][is.finite(PEEP)] , by=c("orlogid", "PERFORMED"), all=TRUE)

setorder(tv_only, orlogid, PERFORMED)
## Derive likely intubation / extubation limits
tv_only[, vent_likely := (PEEP > 2) & (pip - PEEP >= 3) & (ETCO2 > 20) & tv > 150 ]
tv_only <- tv_only[vent_likely == TRUE]
tv_only[, N := .N , by="orlogid"]
## look for 15 minutes on either side; likely enough to avoid mask vent pre and post
tv_only<-tv_only[N>30]

vent_times<- merge( 
   tv_only[vent_likely == TRUE , .SD[8, .(vent_start=PERFORMED, N) ], by="orlogid" ] ,
   tv_only[vent_likely == TRUE , .SD[.N-8, .(vent_stop=PERFORMED) ], by="orlogid" ] , by= "orlogid")
   
big_vent_time_up[[ts_index]] <- vent_times   
vent_times[, total_vent_time := difftime(vent_stop, vent_start, units="mins") %>% as.numeric]

vent_holder_up[[ts_index]] <- vent_times[ tv_only, on="orlogid", allow.cartesian=TRUE , nomatch=NULL ][data.table::between(PERFORMED, vent_start, vent_stop), .( pip_mean=mean( pip) , pip_good_frac=mean( pip < 30 ) )  , by= "orlogid" ]


##########
## volatile use - success
##########

an_agents <- epic_flow_1[MEASURE_NAME %in% c('O2', 'N2O', 'Air')] 
an_agents[ , VALUE := as.numeric(VALUE)] 
an_agents <-an_agents[is.finite(VALUE), nomatch=NULL]

an_agents <- vent_times[ an_agents, on="orlogid", allow.cartesian=TRUE ][data.table::between(PERFORMED, vent_start, vent_stop)]

## do five minute blocks to smooth out variation
fgf <- an_agents [ , .(fgf=sum(VALUE , na.rm=TRUE) ), by = c('orlogid' , 'PERFORMED')]
# fgf [ , time_block := as.numeric(difftime(PERFORMED, vent_start, units='mins')) %/% 5 ]
# fgf <- fgf [, .(fgf = mean(fgf) ) , by=c("orlogid", "time_block")]
# 
## there are some rooms with maual fgf, where it is only defined at moments of change
## so locf between observations
setkey(fgf, orlogid, PERFORMED)
fgf[, delta := difftime(shift(PERFORMED, type="lead"), PERFORMED , units="mins") %>% as.numeric %>% fcoalesce(1) %>% as.integer , by="orlogid"] 
fgf[, oldI := .I]
fgf <- fgf[ rep(oldI, times=delta) ]
## this could be much faster by pre-computing the difference
setkey(fgf, orlogid, PERFORMED)
fgf[delta>1, PERFORMED := PERFORMED + dminutes(seq(.N)-1), by=.(orlogid, PERFORMED) ] 
fgf[,oldI :=NULL]
fgf[,delta :=NULL]

agent2 <- epic_flow_1[MEASURE_NAME %chin% c('Expired Desflurane' , 'Expired Isoflurane', 'Expired Sevoflurane', "N2O Expired") ]
agent2 <-vent_times[agent2 , on="orlogid", allow.cartesian=TRUE, nomatch=NULL]
agent2 <-agent2 [(PERFORMED < vent_stop) & (PERFORMED > vent_start ) ]
agent2[ , VALUE := as.numeric(VALUE)] 
agent2<-agent2[is.finite(VALUE)]

## get rid of very low values, as these are often trace gas at the beginning and end
agent2 <- agent2[ VALUE > fcase(MEASURE_NAME == 'Expired Desflurane', .5, MEASURE_NAME == 'Expired Isoflurane', 0.1,MEASURE_NAME ==  'Expired Sevoflurane' , 0.15, MEASURE_NAME ==  "N2O Expired", .1 )   ]

## it is easier to LOCF here than after the merge
setkey(agent2, orlogid,MEASURE_NAME, PERFORMED)
agent2[, delta := difftime(shift(PERFORMED, type="lead"), PERFORMED , units="mins") %>% as.numeric %>% fcoalesce(1) %>% as.integer , by=c("orlogid", "MEASURE_NAME")] 
agent2[, oldI := .I]
agent2 <- agent2[ rep(oldI, times=delta) ]
## this could be much faster by pre-computing the difference with a parallen seq (that exists somewhere), but it's not slow now
setkey(agent2, orlogid, PERFORMED, MEASURE_NAME)
agent2[delta>1, PERFORMED := PERFORMED + dminutes(seq(.N)-1), by=.(orlogid, PERFORMED, MEASURE_NAME) ] 
agent2[,oldI :=NULL]
agent2[,delta :=NULL]


fgf <- merge(fgf, agent2, by=c("orlogid", "PERFORMED"))

fgf[ VALUE > fcase(MEASURE_NAME == 'Expired Desflurane', 2, MEASURE_NAME == 'Expired Isoflurane', 0.3,MEASURE_NAME ==  'Expired Sevoflurane' , 0.6 )  , efficent := fgf <= fcase(MEASURE_NAME == 'Expired Desflurane', 1.01 , MEASURE_NAME == 'Expired Isoflurane', 1.01 ,MEASURE_NAME ==  'Expired Sevoflurane' , 2.01 ) ]

fgf_holder_up[[ts_index]] <- fgf[ is.finite(efficent), .(eff_flow = mean(efficent)) , by="orlogid"]

## if vent time is not included, add it with value greater ~ 1 mac (to start a jump, assuming induction drug had been given)
## actually, because I used the inequality, just always add
## using the select operator means that I don't have to worry about other fields
## using the select operator also means that TIVA / sedation cases aren't included here 
## I am a little worried that this messes up the fgf outcome for cases that start tiva, BUT I don't want to locf from this measure, and the fgf outcome drops the first 8 observations
## actually, it is easier to just handle this at the end using the minimum time with agent 
# agent2 <- rbind(agent2 , agent2[agent2[ , .I[1], by="orlogid" ]$V1][, PERFORMED := vent_start][,VALUE:= fcase(MEASURE_NAME == 'Expired Desflurane', 6 , MEASURE_NAME == 'Expired Isoflurane', 1. ,MEASURE_NAME ==  'Expired Sevoflurane' , 2.01, MEASURE_NAME =="N2O Expired", .6   ) ] )

agent3<- agent2[ , .(mac=sum(VALUE / fcase(MEASURE_NAME == 'Expired Desflurane', 6.6 , MEASURE_NAME == 'Expired Isoflurane', 1.2 ,MEASURE_NAME ==  'Expired Sevoflurane' , 1.8, MEASURE_NAME =="N2O Expired", 1.04 ) , na.rm=TRUE) ), by = c('orlogid' , 'PERFORMED') ]
agent3<- agent3[is.finite(mac)]



## age adjustment
agent3 <- ages_only[agent3, on="orlogid", nomatch=NULL ][, mac := mac*10^ (-0.00269*(Age - 40) ) ]
## To get 15 minute block of no agent, take only the yes-agent times, vent+15 and diff them. 
## I need to add a "yes" time for each unique med observation within the relevant orlogid and vent times

local_prop <-vent_times[ all_prop[ orlogid %in% unique(vent_times$orlogid)   ], on="orlogid", nomatch=NULL][data.table::between(TAKEN_TIME, vent_start, vent_stop)][ , .(orlogid, PERFORMED=TAKEN_TIME, mac = 0.5)]

agent3 <- agent3[mac >= 0.3 ]
agent3[, Age := NULL] 

agent3 <- rbind(agent3, local_prop )

setkey(agent3, orlogid, PERFORMED)
agent3[, jump := as.numeric(difftime(shift(PERFORMED, type="lead"), PERFORMED,  units="mins" ) ) > 15, by="orlogid"]

#  join_mrns[temp[njumps>0], on="SurgicalRecordID==orlogid"] 
agent3 <- agent3[, .(minyes = min(PERFORMED), njumps = sum(jump, na.rm=T), minjump=min(PERFORMED[jump %in% TRUE]) ) , by="orlogid"]
agent3 <- vent_times[agent3 , on="orlogid", allow.cartesian=TRUE, nomatch=NULL]
agent3[ as.numeric(difftime(minyes, vent_start, units="min" ) ) >15, njumps := njumps +1 ]

agent_holder_up[[ts_index]]<- agent3[ , .( orlogid, njumps,  minjump) ]


}

rm(agent3,
agent2,
fgf,
an_agents,
tv_only,
epic_flow_1,
bp_only
)

## take only the "best" entry 
for(ts_index in seq_along(sample_count)) {
  sample_count[[ts_index]][, set_index := 100+ ts_index]
}

for(ts_index in seq_along(sample_count_up)) {
  sample_count_up[[ts_index]][, set_index := 200+ ts_index]
}

sample_count_all <- rbindlist( c( sample_count,sample_count_up  ) ) %>% unique
setorder(sample_count_all, orlogid, -N)
sample_count_all <- sample_count_all[ sample_count_all[,.I[1], by="orlogid" ]$V1  ] 
## intersect all the below with samples where that is the "best" set
sample_count_all %>%unique(by="orlogid") %>% fwrite("/research/intermediates/flowsheet_counts.csv")

for(ts_index in seq_along(sample_count)) {
local_set <- sample_count_all[ set_index == 100+ ts_index]$orlogid %>% unique
temp_holder[[ts_index]] <- temp_holder[[ts_index]][orlogid %in% local_set]
map_holder[[ts_index]] <- map_holder[[ts_index]][orlogid %in% local_set]
fgf_holder[[ts_index]] <- fgf_holder[[ts_index]][orlogid %in% local_set]
vent_holder[[ts_index]] <- vent_holder[[ts_index]][orlogid %in% local_set]
vent_holder2[[ts_index]] <- vent_holder2[[ts_index]][orlogid %in% local_set]
agent_holder[[ts_index]] <- agent_holder[[ts_index]][orlogid %in% local_set]
cam_holder[[ts_index]] <- cam_holder[[ts_index]][orlogid %in% local_set]
big_vent_time[[ts_index]] <- big_vent_time[[ts_index]][orlogid %in% local_set]
map_holder_post[[ts_index]] <- map_holder_post[[ts_index]][orlogid %in% local_set]
}


for(ts_index in seq_along(sample_count_up)) {
local_set <- sample_count_all[ set_index == 200+ ts_index]$orlogid %>% unique
temp_holder_up[[ts_index]] <- temp_holder_up[[ts_index]][orlogid %in% local_set]
map_holder_up[[ts_index]] <- map_holder_up[[ts_index]][orlogid %in% local_set]
fgf_holder_up[[ts_index]] <- fgf_holder_up[[ts_index]][orlogid %in% local_set]
vent_holder_up[[ts_index]] <- vent_holder_up[[ts_index]][orlogid %in% local_set]
vent_holder2_up[[ts_index]] <- vent_holder2_up[[ts_index]][orlogid %in% local_set]
agent_holder_up[[ts_index]] <- agent_holder_up[[ts_index]][orlogid %in% local_set]
cam_holder_up[[ts_index]] <- cam_holder_up[[ts_index]][orlogid %in% local_set]
big_vent_time_up[[ts_index]] <- big_vent_time_up[[ts_index]][orlogid %in% local_set]
map_holder_post_up[[ts_index]] <- map_holder_post_up[[ts_index]][orlogid %in% local_set]
}




temp_holder <- rbindlist( c( temp_holder,temp_holder_up  ) ) %>% unique(by="orlogid")
map_holder <- rbindlist( c(map_holder , map_holder_up ) ) %>% unique(by="orlogid") 
fgf_holder <- rbindlist( c(fgf_holder , fgf_holder_up ) ) %>% unique(by="orlogid")
vent_holder <- rbindlist( c( vent_holder, vent_holder_up  ) ) %>% unique(by="orlogid")
vent_holder2 <- rbindlist( c( vent_holder2, vent_holder2_up  ) ) %>% unique(by="orlogid")
agent_holder <- rbindlist( c(agent_holder ,agent_holder_up  ) ) %>% unique(by="orlogid")
cam_holder <- rbindlist( c(cam_holder ,cam_holder_up  ) ) %>% unique(by="orlogid")
big_vent_time <- rbindlist( c(big_vent_time ,big_vent_time_up  ) ) %>% unique(by="orlogid")
map_holder_post <- rbindlist( c(map_holder_post ,map_holder_post_up  ) ) %>% unique(by="orlogid")


## eliminate ineligble cases, chain backwards
vent_holder3 <- join_mrns[, .(SurgicalRecordID, CurrentMRN=MRN, AN_START_DATETIME)][ vent_holder2, on="SurgicalRecordID==orlogid"]

vent_holder3[preop_vent_b == FALSE, post_ven_clarity := postop_vent_duration > 1]
vent_holder3 <- vent_holder3 [is.finite(post_ven_clarity)]

setorder(vent_holder3, CurrentMRN, AN_START_DATETIME)

## the lowest effort way to do this is to do a self join on the remaining instead of worrying about a linear time search; it's less than 1k

vent_holder3[ , nc := .N, by ="CurrentMRN"]
vent_holder3[ , dont_window := nc ==1]
# vent_holder3[ dont_window == FALSE, dont_window := max( difftime(AN_START_DATETIME, min(AN_START_DATETIME) ) ) <30 , by="CurrentMRN"] ## handle this case separately
vent_holder3[ dont_window == FALSE, dont_window := min( difftime(shift(AN_START_DATETIME, type="lead"), AN_START_DATETIME) , na.rm=T ) >30 , by="CurrentMRN"] ## handle this case separately

vent_holder3[ dont_window == FALSE, dont_window := difftime(shift(AN_START_DATETIME, type="lead"), AN_START_DATETIME)  >30 , by="CurrentMRN"] ## handle this case separately
vent_holder3[ is.na(dont_window), dont_window := FALSE]


vent_repair <-  vent_holder3[(nc>1) & (post_ven_clarity==TRUE), .(CurrentMRN, post_ven_clarity, AN_START_DATETIME)][vent_holder3[(dont_window==FALSE)& (post_ven_clarity==FALSE), .(CurrentMRN, SurgicalRecordID, post_ven_clarity, AN_START_DATETIME) ] , on="CurrentMRN", allow.cartesian=TRUE, nomatch=NULL]

vent_repair[ data.table::between(AN_START_DATETIME, i.AN_START_DATETIME, i.AN_START_DATETIME+ddays(29)),  i.post_ven_clarity := any(post_ven_clarity, na.rm=TRUE) , by="SurgicalRecordID"]

vent_repair<-vent_repair[ , .(chain_post_vent=any(i.post_ven_clarity) ), by="SurgicalRecordID"][chain_post_vent==TRUE, .(orlogid=SurgicalRecordID)]

vent_holder2[vent_repair, postop_vent_duration := pmax(postop_vent_duration, 10), on="orlogid" ]

cam_holder %>%unique(by="orlogid") %>% fwrite("/research/intermediates/intermediate_cam.csv")
vent_holder2 %>% unique(by="orlogid") %>% fwrite("/research/intermediates/intermediate_ventfail.csv")
temp_holder %>% unique(by="orlogid") %>% fwrite("/research/intermediates/intermediate_temperature.csv")
map_holder %>%unique(by="orlogid") %>% fwrite("/research/intermediates/intermediate_map.csv")
fgf_holder %>%unique(by="orlogid") %>% fwrite("/research/intermediates/intermediate_fgf.csv")
vent_holder %>%unique(by="orlogid") %>% fwrite("/research/intermediates/intermediate_vent.csv")
agent_holder %>%unique(by="orlogid") %>% fwrite("/research/intermediates/intermediate_agent.csv")
map_holder_post %>%unique(by="orlogid") %>% fwrite("/research/intermediates/intermediate_postop_hypo.csv")
big_vent_time%>% fwrite("/research/intermediates/vent_times.csv")

# } else {

# 
# cam_holder <- fread("/research/intermediates/intermediate_cam.csv")
# vent_holder2 <-fread("/research/intermediates/intermediate_ventfail.csv")
# temp_holder <- fread("/research/intermediates/intermediate_temperature.csv")
# map_holder <- fread("/research/intermediates/intermediate_map.csv")
# fgf_holder <- fread("/research/intermediates/intermediate_fgf.csv")
# vent_holder <- fread("/research/intermediates/intermediate_vent.csv")
# agent_holder <- fread("/research/intermediates/intermediate_agent.csv")


# }

temp <- merge(
   cam_holder[join_mrns, on="orlogid==SurgicalRecordID", nomatch=NULL] [, roughdate := Date %>% roughen_date ][, .(UniqueCaseWithTemp=uniqueN(orlogid)), by=.(roughdate, isTreatment )][roughdate>2019.25][order(roughdate)] ,
   cases_by_month_summary, by=c("roughdate", "isTreatment"))[, fractionWCAM := round( UniqueCaseWithTemp/UniqueCase,4)][order(roughdate, isTreatment)] 
   
temp  %>%  fwrite("/research/outputs/CAM_presense.csv")

temp <- merge(
   vent_holder2[join_mrns, on="orlogid==SurgicalRecordID", nomatch=NULL] [, roughdate := Date %>% roughen_date ][, .(UniqueCaseWithTemp=uniqueN(orlogid)), by=.(roughdate, isTreatment )][roughdate>2019.25][order(roughdate)] ,
   cases_by_month_summary, by=c("roughdate", "isTreatment"))[, fractionWRespF := round( UniqueCaseWithTemp/UniqueCase,4)][order(roughdate, isTreatment)] 
   
temp  %>%  fwrite("/research/outputs/RespF_presense.csv")


temp <- merge(
   temp_holder[join_mrns, on="orlogid==SurgicalRecordID", nomatch=NULL] [, roughdate := Date %>% roughen_date ][, .(UniqueCaseWithTemp=uniqueN(orlogid)), by=.(roughdate, isTreatment )][roughdate>2019.25][order(roughdate)] ,
   cases_by_month_summary, by=c("roughdate", "isTreatment"))[, fractionWTemp := round( UniqueCaseWithTemp/UniqueCase,4)][order(roughdate, isTreatment)] 
   
temp  %>%  fwrite("/research/outputs/temperature_presense.csv")


copy(temp)[,isTreatment:=factor(isTreatment) ] %>% ggplot( aes(x=roughdate, y=fractionWTemp,  group = isTreatment, fill =isTreatment)) + geom_bar(stat = "identity", width=0.05, position=position_identity(), alpha=0.5) + labs(x = "Date", y = "Fraction", title = "Temp measured by month") -> temp2
temp2 %>% ggsave(device="png", filename="/research/outputs/temp_present.png")

temp <- merge(
   map_holder[join_mrns, on="orlogid==SurgicalRecordID", nomatch=NULL] [, roughdate := Date %>% roughen_date ][, .(UniqueCaseWithTemp=uniqueN(orlogid)), by=.(roughdate, isTreatment )][roughdate>2019.25][order(roughdate)] ,
   cases_by_month_summary, by=c("roughdate", "isTreatment"))[, fractionWTemp := round( UniqueCaseWithTemp/UniqueCase,4)][order(roughdate, isTreatment)] 
   
temp  %>%  fwrite("/research/outputs/bp_presense.csv")


copy(temp)[,isTreatment:=factor(isTreatment) ] %>% ggplot( aes(x=roughdate, y=fractionWTemp,  group = isTreatment, fill =isTreatment)) + geom_bar(stat = "identity", width=0.05, position=position_identity(), alpha=0.5) + labs(x = "Date", y = "Fraction", title = "BP measured by month") -> temp2
temp2 %>% ggsave(device="png", filename="/research/outputs/bp_present.png")


temp <- merge(
   fgf_holder[join_mrns, on="orlogid==SurgicalRecordID", nomatch=NULL] [, roughdate := Date %>% roughen_date ][, .(UniqueCaseWithTemp=uniqueN(orlogid)), by=.(roughdate, isTreatment )][roughdate>2019.25][order(roughdate)] ,
   cases_by_month_summary, by=c("roughdate", "isTreatment"))[, fractionWTemp := round( UniqueCaseWithTemp/UniqueCase,4)][order(roughdate, isTreatment)] 
   
temp  %>%  fwrite("/research/outputs/fgf_presense.csv")


copy(temp)[,isTreatment:=factor(isTreatment) ] %>% ggplot( aes(x=roughdate, y=fractionWTemp,  group = isTreatment, fill =isTreatment)) + geom_bar(stat = "identity", width=0.05, position=position_identity(), alpha=0.5) + labs(x = "Date", y = "Fraction", title = "FGF measured by month") -> temp2
temp2 %>% ggsave(device="png", filename="/research/outputs/fgf_present.png")


temp <- merge(
   vent_holder[join_mrns, on="orlogid==SurgicalRecordID", nomatch=NULL] [, roughdate := Date %>% roughen_date ][, .(UniqueCaseWithTemp=uniqueN(orlogid)), by=.(roughdate, isTreatment )][roughdate>2019.25][order(roughdate)] ,
   cases_by_month_summary, by=c("roughdate", "isTreatment"))[, fractionWTemp := round( UniqueCaseWithTemp/UniqueCase,4)][order(roughdate, isTreatment)] 
   
temp  %>%  fwrite("/research/outputs/vent_presense.csv")


copy(temp)[,isTreatment:=factor(isTreatment) ] %>% ggplot( aes(x=roughdate, y=fractionWTemp,  group = isTreatment, fill =isTreatment)) + geom_bar(stat = "identity", width=0.05, position=position_identity(), alpha=0.5) + labs(x = "Date", y = "Fraction", title = "Airway Pressure measured by month") -> temp2
temp2 %>% ggsave(device="png", filename="/research/outputs/vent_present.png")

temp <- merge(
   agent_holder[join_mrns, on="orlogid==SurgicalRecordID", nomatch=NULL] [, roughdate := Date %>% roughen_date ][, .(UniqueCaseWithTemp=uniqueN(orlogid)), by=.(roughdate, isTreatment )][roughdate>2019.25][order(roughdate)] ,
   cases_by_month_summary, by=c("roughdate", "isTreatment"))[, fractionWTemp := round( UniqueCaseWithTemp/UniqueCase,4)][order(roughdate, isTreatment)] 
   
temp  %>%  fwrite("/research/outputs/agent_presense.csv")


copy(temp)[,isTreatment:=factor(isTreatment) ] %>% ggplot( aes(x=roughdate, y=fractionWTemp,  group = isTreatment, fill =isTreatment)) + geom_bar(stat = "identity", width=0.05, position=position_identity(), alpha=0.5) + labs(x = "Date", y = "Fraction", title = "Agent concentration measured by month") -> temp2
temp2 %>% ggsave(device="png", filename="/research/outputs/agent_present.png")

