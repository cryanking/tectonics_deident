## TODO:
## review brad's folder for  raw pre-2020.25 data -> distinct reporting in supplement, then merge for main if looks reasonably compatabile
## brad's folder big "saves" -> "newEntry" type responses (not alerts) -> add to contacts
## pull caseReviews (probably in Brad's raw data) -> non alert contacts, make sure to check for group and review sent --> there are control group reviews not sent

## expects that '/storage1/fs1/christopherking/Active/tectonics_paper/' is mounted as '/research/'
## this is because of dir compatability issues between regular docker and RIS. Because RIS runs as non-root and docker root by default, RIS doesn't like root owned dirs for package storage
.libPaths( c( "/root/R/x86_64-pc-linux-gnu-library/4.3/" , .libPaths() ) )

## groundhog is an alternative that is supposed to make the docker build a little more reproducable. It installs packages as of a fixed date. You can just run library(), there is nothing in here with significant change in package build over 2023
library(groundhog) 

## this is because I had issues with getting the docker build stage to consistently set non-root ownership of installed packages, so on RIS I copy the '/root/R_groundhog/' to '/root/R_groundhog2/' which is user-owned. On regular installs it will re-install these packages unless you are at the same version. For interactive use, you can skip this and just 
## library("data.table") ; library("dplyr") ; library("magrittr") ; library("lubridate")
set.groundhog.folder('/root/R_groundhog2/')

groundhog.library(c(
"data.table" , 
"dplyr" ,
"magrittr" ,
"lubridate" 
) , '2023-12-14')

## alerts accumulated per (case?). I think actually accumulated over unique op_sys+SurgicalRecordCSN based on the line
##             op_sys SurgicalRecordCSN  n_alts n_contactOR n_impact
## 27811:       <NA>              NULL    139           3        0

alert_data <- fread("/research/2023_07_TECTONICS_PaperData/TECTONICS_AlertReviews_perPatient.csv")
alert_data <- alert_data[SurgicalRecordCSN!="NULL"]
alert_data <- alert_data[!is.na(op_sys)]
# alert_data<- alert_data[Date >=lubridate::ymd("2019-07-01")]

## the list  of patients / cases passing inclusion filters
join_mrns <- fread("/research/intermediates/population.csv")
join_mrns[, Date:= as.Date(Date)]

## IDs that I don't normally use for matching
all_ids <- fread('/research/2023_07_TECTONICS_PaperData/2023_07_21_IDsGalore.csv', sep="^", quote="") ##PAT_MRN_ID AN_52_ENC_CSN_ID AN_53_ENC_CSN_ID AN_3_ENC_CSN_ID INPATIENT_DATA_ID_52_INTRAOP INPATIENT_DATA_ID_3_HSP AN_PROC_NAME AN_START_DATETIME AN_STOP_DATETIME Anesthesia_Duration CASE_CLASS_NM SERVICE_NM LOG_ID ASA_SCORE_C EMERG_STATUS AN_TYPE

## strangely, the op_sys field is the AN_3_ENC_CSN_ID, and the CSN field doesn't match any IDs we have. AN_3_ENC_CSN_ID is roughly a hospitalization (I think it's a surgery encounter)
# sapply( all_ids, function(x) length(intersect(alert_data$op_sys %>% as.character, x%>%as.character)) )
# sapply( all_ids, function(x) length(intersect(alert_data$SurgicalRecordCSN %>% as.character, x%>%as.character)) )

## match opsys to a log ID. this results in a few duplicates (multiple surgeries on a day), I will have to reformat the underlying data to fix, since only a date is given
alert_data <- all_ids[, .(op_sys=AN_3_ENC_CSN_ID, SurgicalRecordID=LOG_ID, AN_DATE)][ alert_data, on="op_sys", nomatch=NULL][Date == AN_DATE] 

## require that the alert is on someone who passed the inclusion filters
alert_data <- alert_data[SurgicalRecordID %in%  (join_mrns[window_start==TRUE]$SurgicalRecordID )]

## as a first pass (it is only 15) just merge the cases in the same day
alert_data[, joined_logs := .N > 1, by="SurgicalRecordID" ]
alert_data[joined_logs == TRUE, n_alts := sum(n_alts, na.rm=TRUE) ,  by="SurgicalRecordID"]
alert_data[joined_logs == TRUE, n_contactOR := sum(n_contactOR, na.rm=TRUE) ,  by="SurgicalRecordID"]
alert_data[joined_logs == TRUE, n_impact := sum(n_impact, na.rm=TRUE) ,  by="SurgicalRecordID"]
alert_data <- unique(alert_data, by="SurgicalRecordID")


## get the denominator
## note that this is a little small for the denominator, because it assumes that days with no replied-to-alerts were unstaffed and therefore excluded
denominators <- join_mrns[window_start==TRUE][ Date %in%  alert_data[n_alts >0]$Date, .(N=uniqueN(SurgicalRecordID)), by="isTreatment"]
setnames(denominators, "isTreatment", "TreatmentName")
denominators[ , TreatmentName := fcase(TreatmentName==1, "Telemedicine Group", TreatmentName==0, "Usual Care Group" ) ]

# alert_data[, .(n_c=sum((n_contactOR+n_impact) > 0), n_im=sum(n_impact>0) ), by="TreatmentName"]


alertcounts <- denominators[ alert_data[Date >=lubridate::ymd("2019-07-01"), .(n_c=sum((n_contactOR+n_impact) > 0), n_im=sum(n_impact>0) ), by="TreatmentName"] , on="TreatmentName"]

alertcounts[ , f_cont := (n_c/N) %>% round(3) %>% multiply_by(100)]
alertcounts[ , f_im := (n_im/N) %>% round(3) %>% multiply_by(100)]

# > alertcounts
#         TreatmentName     N  n_c n_im f_cont f_im
# 1: Telemedicine Group 31902 1638  555    5.1  1.7
# 2:   Usual Care Group 31851  103   22    0.3  0.1


## how many days is this?

length(setdiff(join_mrns$Date,  alert_data[Date >=lubridate::ymd("2019-07-01")][n_alts >0]$Date))
length(setdiff(join_mrns$Date,  alert_data[Date >=lubridate::ymd("2019-07-01")]$Date))

## are these in blocks?
missing_days <- setdiff(join_mrns$Date,  alert_data[Date >=lubridate::ymd("2019-07-01")][n_alts >0]$Date) %>% sort
missing_days  %>% diff

## yes, one big block and some sporadic days
as.IDate(c(18247 , 18393) )
# [1] "2019-12-17" "2020-05-11"

start_data_hole <- missing_days[ which(diff(missing_days) < 2)[1] ]                                                                                                    
stop_data_hole <- missing_days[missing_days > start_data_hole]                                                               
stop_data_hole <- stop_data_hole[ which(diff(stop_data_hole) > 5)[1]-1  ]                                                    
c(start_data_hole , stop_data_hole)  %>% as.IDate                                                                            
seq(from=start_data_hole, to=stop_data_hole) %>% as.IDate %>% weekdays -> temp
sum(!(temp %in% c("Saturday", "Sunday") ))

stop_data_hole-start_data_hole  

## alert counts for secondary analysis
alert_data2 <- fread("/research/2023_07_TECTONICS_PaperData/TECTONICS_AlertReview.csv")

alert_data2 <- alert_data2[SurgicalRecordCSN!="NULL"][!is.na(op_sys)]

nrow(alert_data2)
uniqueN(alert_data2$SurgicalRecordCSN)

## code alerts and responses
## significant, doesn't need contact: assess 1, 2
## significant, needs contact: assess 3, 4
## not sure: 6 NOTE: this is a huge fraction, will need to decide what to do with it
## not sig: 7, 8, 9
## very not sig (silence perm): 100

## earlier versions of the form graded the ACTs ability to respond:
## significant assessContact and helpful: 1 , 2 , 4 , 5
## significant assessContact and not helpful: 3 , 6, 7
## irr assessContact : 8

## these contacts imply something meaningful
## actionContact: 1, 2, 3
## irr actionContact: 4
## didComm 2, entry 1, enntry 2

## I accumulate these into one group (a) because it's easier to have the same function for each col (b) so I don't have to keep track of which goes in which col
relevant_alerts <- c(
paste("reaction", seq.int(8), sep="." ) ,
paste("assess", c(seq.int(4)), sep="." ) ,
paste("assessContact", seq.int(7), sep="." ) ,
paste("actionContact", seq.int(3), sep="." ) ,
paste("entry", seq.int(2), sep="." ) ,
paste("reasonContact", c(1,3,4,5), sep="." ) ,
'didComm.2' , 'didComm.1')

irrelevant_alerts <- c(
paste("assess", c(7,8,9,100), sep="." ) ,
paste("assessContact", 8, sep="." ) ,
paste("actionContact", 4, sep="." ) 
)

## and need for intervention
needed_intervention_alerts <- c(
paste("assess", c(3,4), sep="." ),
paste("assessContact", c(1,2,4,5), sep="." ) ,
paste("actionContact", seq.int(3), sep="." ) ,
paste("entry", seq.int(2), sep="." ) ,
'didComm.2', 'didComm.1'
)


## this is a "slow" way to do this, but seems to work fast enough on this scale (better would be to unlist, key the sum)
alert_data2[, splitrxn := strsplit(reaction, split=",")]
alert_data2[, splitass := strsplit(assessment, split=",")]
alert_data2[, splitcontant := strsplit(contact, split=",")]
alert_data2[, splitaction := strsplit(action, split=",")]

alert_data2[, sig_reaction := sapply(splitrxn, function(x){any(x %chin% relevant_alerts)} )]
alert_data2[, sig_asses := sapply(splitass, function(x){any(x %chin% relevant_alerts)} )]
alert_data2[, sig_contact := sapply(splitcontant, function(x){any(x %chin% relevant_alerts)} )]
alert_data2[, sig_action := sapply(splitaction, function(x){any(x %chin% relevant_alerts)} )]
alert_data2[, maybesig_assess := sapply(splitass, function(x){any(x %chin% "assess.6")} )]

alert_data2[, signon_reaction := sapply(splitrxn, function(x){any(x %chin% irrelevant_alerts)} )]
alert_data2[, signon_asses := sapply(splitass, function(x){any(x %chin% irrelevant_alerts)} )]
alert_data2[, signon_contact := sapply(splitcontant, function(x){any(x %chin% irrelevant_alerts)} )]
alert_data2[, signon_action := sapply(splitaction, function(x){any(x %chin% irrelevant_alerts)} )]


alert_data2[, overall_relevant := fcoalesce( as.logical(pmax(sig_reaction, sig_asses, sig_contact, sig_action  , na.rm=T)) & (!pmax(signon_reaction, signon_asses, signon_contact, signon_action  , na.rm=T) ), FALSE) ]


alert_data2[, int_reaction := sapply(splitrxn, function(x){any(x %chin% needed_intervention_alerts)} )]
alert_data2[, int_asses := sapply(splitass, function(x){any(x %chin% needed_intervention_alerts)} )]
alert_data2[, int_contact := sapply(splitcontant, function(x){any(x %chin% needed_intervention_alerts)} )]
alert_data2[, int_action := sapply(splitaction, function(x){any(x %chin% needed_intervention_alerts)} )]

alert_data2[, overall_interv := fcoalesce(pmax(int_reaction, int_asses, int_contact, int_action  , na.rm=T), 0L) ]
# alert_data2[ , .(int_reaction, int_asses, int_contact, int_action)] %>% sapply(FUN= .%>% sum(na.rm=T))
alert_data2[ , .(sig_reaction, sig_asses, sig_contact, sig_action)] %>% sapply(FUN= .%>% sum(na.rm=T))
alert_data2[ , .(int_reaction, int_asses, int_contact, int_action)] %>% sapply(FUN= .%>% sum(na.rm=T))

alert_data2[, isglucose := message %ilike% "glucose"]
alert_data2[, isglucose := isglucose | (message %ilike% "insulin")]
alert_data2[, isglucose := isglucose | (message %ilike% "glycem")]
alert_data2[, istemp := message %ilike% "tempera"]
alert_data2[, istemp := istemp | (message %ilike% "thermia")]
alert_data2[, istemp := istemp | (message %ilike% "fever")]
alert_data2[, isabx := message %ilike% "antibiot"]

## this data is all assessed (that's how it got into the db)
# alert_data2[, notassessed := (reaction == "") & (assessment == "") &(action == "") &(contact == "") ]

alert_data2[ , .(overall_relevant,overall_interv)] %>% table
alert_data2[ !(isglucose|isabx|istemp) , .(overall_relevant,overall_interv)] %>% table

## probably contacted
alert_data2[ , didContact := fcase(
 action %chin% c("noComm.1",""), FALSE, ## the blank is a small N, but free text occasionaly mentions contact
 sapply(splitaction, function(x){any(x %chin% c("didComm.1","didComm.2"))})==TRUE, TRUE,
 overall_interv > 0 & treatment ==1, TRUE ,
 sig_reaction > 0 , TRUE ,
 sapply(splitcontant, function(x){any(x %chin% paste("reasonContact", c(1,3,4,5), sep="." ))})==TRUE, TRUE,
 sapply(splitcontant, function(x){any(x %chin% paste("assessContact", c(1,2,4,5), sep="." ) )})==TRUE, TRUE,
 default=NA) ]
 
## inadequate
alert_data2[ , inad :=  sapply(splitass, function(x){any(x %chin% paste("assess", c(3,4), sep="." ))}) ]


alert_data2[,.(treatment, didContact)] %>% table(useNA='a')

## join before accumulating
alert_data2 <- all_ids[, .(op_sys=AN_3_ENC_CSN_ID, SurgicalRecordID=LOG_ID, AN_START_DATETIME, AN_STOP_DATETIME)][ alert_data2, on="op_sys", nomatch=NULL][ between(time,  AN_START_DATETIME, AN_STOP_DATETIME) ]

alert_data2 <- alert_data2[SurgicalRecordID %in%  (join_mrns[window_start==TRUE]$SurgicalRecordID )]


alert_per_person <- alert_data2[ , .(
  didContact=any(didContact, na.rm=T) , 
  isProtocolOnly = all(isglucose|isabx|istemp , na.rm=T) ,
  interv = any(inad, na.rm=T) , 
  overall_relevant = any(overall_relevant, na.rm=T) , 
  surgDate = first(na.omit(date(AN_START_DATETIME  ) ) ),
  treatment= mean(treatment, na.rm=T)) , by="SurgicalRecordID"] 
  
## note that there is one case with mixed treatment status
name_prepend <- function(x,pre)  set_names(x, paste0(pre, names(x)) ) 

alert_per_person [ , c(list(N=nrow(.SD)), lapply(.SD, . %>% mean(na.rm=T) %>% round(3)) %>% name_prepend(pre="mean_") , lapply(.SD, . %>% sum(na.rm=T))%>% name_prepend(pre="sum_") ) , by="treatment", .SDcols= c("didContact", "isProtocolOnly", "interv", "overall_relevant") ]

## add pseudorecords for cases not included

pseudo_records <- join_mrns[window_start==TRUE][ Date %in%  alert_per_person$surgDate][!alert_per_person , on="SurgicalRecordID" ][, .(SurgicalRecordID, treatment=isTreatment, surgDate=Date)] 

set(pseudo_records, j="interv", value=FALSE )
set(pseudo_records, j="overall_relevant", value=FALSE )
set(pseudo_records, j="isProtocolOnly", value=FALSE )
set(pseudo_records, j="didContact", value=FALSE )

rbind(alert_per_person,pseudo_records)[ , c(list(N=nrow(.SD)), lapply(.SD, . %>% mean(na.rm=T) %>% round(3)) %>% name_prepend(pre="mean_") , lapply(.SD, . %>% sum(na.rm=T))%>% name_prepend(pre="sum_") ) , by="treatment", .SDcols= c("didContact", "isProtocolOnly", "interv", "overall_relevant") ]

## merge "big saves"

saves <- read_xlsx("/research/2023_07_05_FINALSaveList.xlsx", col_types= c("date", "text" , "numeric", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text") )
setDT(saves)

# test_overlap <- function(d1, d2) { 
# resultm <- matrix(NA_real_, nrow=length(d1), ncol=length(d2))
# rownames(resultm ) <- names(d1)
# colnames(resultm ) <- names(d2)
# 
# for (x in seq_along(d1)) {
# for (y in seq_along(d2)) {
# resultm[x,y] <- length(intersect(d1[[x]] %>% as.character, d2[[y]]  %>% as.character )) 
# }
# }
# resultm
# }
# 
# test_overlap(saves[, .(AnesCase, SurgicalRecordCSN, MRN ) ] , all_ids )

saves[,AN_3_ENC_CSN_ID := bit64::as.integer64(AnesCase)]
missing_id_saves <- saves[!all_ids, on="AN_3_ENC_CSN_ID"]
saves <- all_ids[,.(AN_3_ENC_CSN_ID,SurgicalRecordID=LOG_ID )][saves, on="AN_3_ENC_CSN_ID", nomatch=NULL]

## some of these I can 3-way link
csn_map <- unique(alert_data2[, .(SurgicalRecordCSN, SurgicalRecordID)])
missing_id_saves[, SurgicalRecordCSN:= as.character(SurgicalRecordCSN)]
saves <- rbind(saves,  csn_map[missing_id_saves, on="SurgicalRecordCSN", nomatch=NULL])

missing_id_saves <- missing_id_saves[!csn_map , on="SurgicalRecordCSN"]

## this never works
# csn_map <- unique(all_ids[, .(MRN=as.character(PAT_MRN_ID), Date=AN_DATE, SurgicalRecordID=LOG_ID)])
# csn_map[missing_id_saves, on=.(MRN,Date), nomatch=NULL]


saves_ps_records <- saves[ , .(SurgicalRecordID, surgDate=Date %>% as.Date, treatment = treatment %>% as.integer , didContact=TRUE , interv=TRUE, isProtocolOnly=FALSE, overall_relevant=TRUE)] %>% unique 

pseudo_records <- rbind(
pseudo_records[!saves_ps_records, on="SurgicalRecordID"], 
saves_ps_records) %>% unique

rbind(alert_per_person,pseudo_records)[ , c(list(N=nrow(.SD)), lapply(.SD, . %>% mean(na.rm=T) %>% round(3)) %>% name_prepend(pre="mean_") , lapply(.SD, . %>% sum(na.rm=T))%>% name_prepend(pre="sum_") ) , by="treatment", .SDcols= c("didContact", "isProtocolOnly", "interv", "overall_relevant") ]

logins <- fread("/research/2023_07_TECTONICS_PaperData/20231219logins.csv")
logins$V3 %>% table
#  1  2  3  4  5  6  7  8  9 10 
# 70 71 61 80 68 38 14  9  6  2 
