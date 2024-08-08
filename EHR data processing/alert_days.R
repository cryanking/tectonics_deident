## TODO:
## review brad's folder for  raw pre-2020.25 data -> distinct reporting in supplement, then merge for main if looks reasonably compatabile
## brad's folder big "saves" -> "newEntry" type responses (not alerts) -> add to contacts
## pull caseReviews (probably in Brad's raw data) -> non alert contacts, make sure to check for group and review sent --> there are control group reviews not sent

## expects that '/storage1/fs1/christopherking/Active/tectonics_paper/' is mounted as '/research/'
## this is because of dir compatability issues between regular docker and RIS. Because RIS runs as non-root and docker root by default, RIS doesn't like root owned dirs for package storage
.libPaths( c( "/root/R/x86_64-pc-linux-gnu-library/4.4/" , .libPaths() ) )

## groundhog is an alternative that is supposed to make the docker build a little more reproducable. It installs packages as of a fixed date. You can just run library(), there is nothing in here with significant change in package build over 2023
library(groundhog) 

## this is because I had issues with getting the docker build stage to consistently set non-root ownership of installed packages, so on RIS I copy the '/root/R_groundhog/' to '/root/R_groundhog2/' which is user-owned. On regular installs it will re-install these packages unless you are at the same version. For interactive use, you can skip this and just 
## library("data.table") ; library("dplyr") ; library("magrittr") ; library("lubridate")
set.groundhog.folder('/root/R_groundhog2/')

groundhog.library(c(
"data.table" , 
"dplyr" ,
"magrittr" ,
"lubridate" ,
"readxl",
"stringr",
"ggplot2"
) , '2024-05-01')



## the list  of patients / cases passing inclusion filters
join_mrns <- fread("/research/intermediates/population.csv")
join_mrns[, Date:= as.Date(Date)]


## IDs that I don't normally use for matching
all_ids <- fread('/research/2023_07_TECTONICS_PaperData/2023_07_21_IDsGalore.csv', sep="^", quote="") ##PAT_MRN_ID AN_52_ENC_CSN_ID AN_53_ENC_CSN_ID AN_3_ENC_CSN_ID INPATIENT_DATA_ID_52_INTRAOP INPATIENT_DATA_ID_3_HSP AN_PROC_NAME AN_START_DATETIME AN_STOP_DATETIME Anesthesia_Duration CASE_CLASS_NM SERVICE_NM LOG_ID ASA_SCORE_C EMERG_STATUS AN_TYPE



## alerts accumulated per (case?). I think actually accumulated over unique op_sys+SurgicalRecordCSN based on the line
##             op_sys SurgicalRecordCSN  n_alts n_contactOR n_impact
## 27811:       <NA>              NULL    139           3        0

alert_data <- fread("/research/2023_07_TECTONICS_PaperData/TECTONICS_AlertReviews_perPatient.csv")
alert_data <- alert_data[SurgicalRecordCSN!="NULL"]
alert_data <- alert_data[!is.na(op_sys)]
# alert_data<- alert_data[Date >=lubridate::ymd("2019-07-01")]


## strangely, the op_sys field is the AN_3_ENC_CSN_ID, and the CSN field doesn't match any IDs we have. AN_3_ENC_CSN_ID is roughly a hospitalization (I think it's a surgery encounter)
# sapply( all_ids, function(x) length(intersect(alert_data$op_sys %>% as.character, x%>%as.character)) )
# sapply( all_ids, function(x) length(intersect(alert_data$SurgicalRecordCSN %>% as.character, x%>%as.character)) )

## match opsys to a log ID. this results in a few duplicates (multiple surgeries on a day), I will have to reformat the underlying data to fix, since only a date is given
alert_data <- all_ids[, .(op_sys=AN_3_ENC_CSN_ID, SurgicalRecordID=LOG_ID, AN_DATE)][ alert_data, on="op_sys", nomatch=NULL][Date == AN_DATE] 

## require that the alert is on someone who passed the inclusion filters
alert_data <- alert_data[SurgicalRecordID %in%  (join_mrns[window_start==TRUE]$SurgicalRecordID )]


## as a first pass (it is only 22) just merge the cases in the same day
alert_data[, joined_logs := .N > 1, by="SurgicalRecordID" ]
alert_data[joined_logs == TRUE, n_alts := sum(n_alts, na.rm=TRUE) ,  by="SurgicalRecordID"]
alert_data[joined_logs == TRUE, n_contactOR := sum(n_contactOR, na.rm=TRUE) ,  by="SurgicalRecordID"]
alert_data[joined_logs == TRUE, n_impact := sum(n_impact, na.rm=TRUE) ,  by="SurgicalRecordID"]
alert_data <- unique(alert_data, by="SurgicalRecordID")

## distinct patients
alert_data$SurgicalRecordID %>% uniqueN
# [1] 20919

## get the denominator
## note that this is a little small for the denominator, because it assumes that days with no replied-to-alerts were unstaffed and therefore excluded
denominators <- join_mrns[window_start==TRUE][ Date %in%  alert_data[n_alts >0]$Date, .(N=uniqueN(SurgicalRecordID)), by="isTreatment"]
setnames(denominators, "isTreatment", "TreatmentName")
denominators[ , TreatmentName := fcase(TreatmentName==1, "Telemedicine Group", TreatmentName==0, "Usual Care Group" ) ]

# alert_data[, .(n_c=sum((n_contactOR+n_impact) > 0), n_im=sum(n_impact>0) ), by="TreatmentName"]


alertcounts <- denominators[ alert_data[Date >=lubridate::ymd("2019-07-01"), .(num_contacted=sum((n_contactOR+n_impact) > 0), num_impactful=sum(n_impact>0) ), by="TreatmentName"] , on="TreatmentName"]

alertcounts[ , frac_contact := (num_contacted/N) %>% round(3) %>% multiply_by(100)]
alertcounts[ , frac_impact := (num_impactful/N) %>% round(3) %>% multiply_by(100)]

# > alertcounts
#         TreatmentName     N  n_c n_im f_cont f_im
# 1: Telemedicine Group 31249 1642  557    5.3  1.8
# 2:   Usual Care Group 32504  103   22    0.3  0.1

 fwrite(alertcounts,"/research/outputs/contact_rate_from_brad.csv")

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


## join before accumulating
alert_data2 <- all_ids[, .(op_sys=AN_3_ENC_CSN_ID, SurgicalRecordID=LOG_ID, AN_START_DATETIME, AN_STOP_DATETIME)][ alert_data2, on="op_sys", nomatch=NULL][ between(time,  AN_START_DATETIME, AN_STOP_DATETIME) ]

alert_data2 <- alert_data2[SurgicalRecordID %in%  (join_mrns[window_start==TRUE]$SurgicalRecordID )]

alert_data2[ , routine_alert := isglucose|isabx|istemp]
alert_data2[ , .(overall_relevant,overall_interv)] %>% table
alert_data2[ !(isglucose|isabx|istemp) , .(overall_relevant,overall_interv)] %>% table
alert_data2[,.(treatment, didContact)] %>% table(useNA='a')
alert_data2[, .N, by=c("routine_alert","overall_interv","overall_relevant","treatment")][order(treatment, routine_alert, overall_relevant, overall_interv)] %>% fwrite("/research/outputs/alert_interp_counts.csv")

alert_data2[routine_alert==FALSE,.(treatment, didContact)] %>% table(useNA='a')
#          didContact
# treatment FALSE  TRUE  <NA>
#      0    11029    43  2714
#      1    11201   819  1105


alert_per_person <- alert_data2[ , .(
  didContact=any(didContact, na.rm=T) , 
  isProtocolOnly = all(routine_alert , na.rm=T) ,
  interv = any(inad, na.rm=T) , 
  overall_relevant = any(overall_relevant, na.rm=T) , 
  surgDate = first(na.omit(date(AN_START_DATETIME  ) ) ),
  treatment= mean(treatment, na.rm=T),
  nalerts = uniqueN(id)
  ) , by="SurgicalRecordID"] 
  
## note that there is one case very early with mixed treatment status


name_prepend <- function(x,pre)  set_names(x, paste0(pre, names(x)) ) 

alert_per_person [ , c(list(N=nrow(.SD)), lapply(.SD, . %>% mean(na.rm=T) %>% round(3)) %>% name_prepend(pre="mean_") , lapply(.SD, . %>% sum(na.rm=T))%>% name_prepend(pre="sum_") ) , by="treatment", .SDcols= c("didContact", "isProtocolOnly", "interv", "overall_relevant") ]

## add pseudorecords for cases not included

pseudo_records <- join_mrns[window_start==TRUE][ Date %in%  alert_per_person$surgDate][!alert_per_person , on="SurgicalRecordID" ][, .(SurgicalRecordID, treatment=isTreatment, surgDate=Date)] 

set(pseudo_records, j="interv", value=FALSE )
set(pseudo_records, j="overall_relevant", value=FALSE )
set(pseudo_records, j="isProtocolOnly", value=FALSE )
set(pseudo_records, j="didContact", value=FALSE )
set(pseudo_records, j="nalerts", value=0L )

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

set(saves_ps_records, j="nalerts", value=0L )

pseudo_records <- rbind(
pseudo_records[!saves_ps_records, on="SurgicalRecordID"], 
saves_ps_records) %>% unique

#######
## Final estimated contact rates
#######
rbind(alert_per_person,pseudo_records)[ , c(list(N=nrow(.SD)), lapply(.SD, . %>% mean(na.rm=T) %>% round(3)) %>% name_prepend(pre="mean_") , lapply(.SD, . %>% sum(na.rm=T))%>% name_prepend(pre="sum_") ) , by="treatment", .SDcols= c("didContact", "isProtocolOnly", "interv", "overall_relevant") ] %>% fwrite("/research/outputs/individual_alert_interp_counts.csv")

## save the raw data for propensity
rbind(alert_per_person,pseudo_records) %>% fwrite("/research/intermediates/alert_per_person.csv")

roughen_date <- function(x) round(year(x) + (month(x)-1)/12,2)  

rbind(alert_per_person,pseudo_records) %>% mutate( roughdate = surgDate %>% roughen_date ) %>% mutate(roughdate = as.factor(roughdate ) ) %>% filter(treatment %in% c(0,1)) %>% group_by(roughdate, treatment) %>% summarize(fWeight = mean(didContact)  )  %>% ungroup %>% mutate(roughdate = as.numeric(as.character(roughdate)))  %>% arrange(roughdate, treatment)  %>% mutate(isTreatment = factor(treatment)) %>% mutate(fWeight = na_if(fWeight,fWeight> .3)) %>% ggplot( aes(x=roughdate, y=fWeight,  group = isTreatment, fill =isTreatment)) + geom_bar(stat = "identity", width=0.05, position=position_identity(), alpha=0.5) + labs(x = "Date", y = "Fraction", title = "Alert Contact by month") -> temp2
temp2 %>% ggsave(device="png", filename="/research/outputs/contact_present.png")
 


#######
## case reviews are in a separate file
#######



## more modern ACT3 epic era ... this extends into Tectonics 
early_review_db <- fread('/research2/Actfast_Alerts/2022_11_21_ReviewTables/2022_11_21_BJHMVDB03_ReviewTable.csv', sep="^", quote="")




## join before accumulating
early_case_reviews <- all_ids[, .(op_sys=AN_3_ENC_CSN_ID, SurgicalRecordID=LOG_ID, AN_START_DATETIME, AN_STOP_DATETIME)][ early_review_db[type=="CaseReview"], on="op_sys", nomatch=NULL][ between(time,  AN_START_DATETIME, AN_STOP_DATETIME) ]

early_case_reviews <- join_mrns[window_start==TRUE][, .(isTreatment, SurgicalRecordID)] [ early_case_reviews, on="SurgicalRecordID", nomatch=NULL]


early_case_reviews[ , c("message","assessment","action","contact","reaction","treatment","atom","status","sequence","id"   ) := NULL]

## extract the front structured data

early_case_reviews[, risk_score:= as.numeric(str_match(comment, pattern='riskScore\":(\\d+)')[,2] )  ]
early_case_reviews[, status:= as.integer(str_match(comment, pattern='status\":(\\d+)')[,2]) ]
early_case_reviews[, version:= as.integer(str_match(comment, pattern='version\":(\\d+)')[,2]) ]
early_case_reviews[, alertReviews:= as.integer(str_match(comment, pattern='alertReviews\":(\\d+)')[,2]) ]
early_case_reviews[, historyAltered:= as.integer(str_match(comment, pattern='historyAltered\":(\\d+)')[,2]) ]
## create the maximum difference in times to see if that is "touched"
setorder(early_case_reviews,SurgicalRecordID, -time ) 
early_case_reviews[, deltat := difftime(time, min(time, na.rm=T) , units="mins" ) %>% as.numeric , by="SurgicalRecordID"]
early_case_reviews[, ntouched := .N, by="SurgicalRecordID"]
early_case_reviews %>% unique(by="SurgicalRecordID") -> early_case_reviews

early_case_reviews[, .N, by=.(status, isTreatment)]
early_case_reviews[, .(status, isTreatment)] %>% table
#       isTreatment
# status    0    1
#      1   17   10
#      2  261  187
#      3   16 2418

early_case_reviews[, .(ntouched, isTreatment)] %>% table
early_case_reviews[isTreatment==1, .(.N, median(deltat, na.rm=T)) ,by=.(ntouched)][order(ntouched)]
## almost only treatment group case reviews, status 3 = sent or finished?


## after the new db in may 2020
new_era_reviews <- fread('/research2/Actfast_Alerts/tectonics_reviews_db2.csv', sep="^", quote="", encoding="Latin-1")
colnames(new_era_reviews) <- c('time', 'op_sys', 'treatment', 'atom', 'type', 'id', 'status', 'message', 'assessment', 'action', 'contact', 'reaction','comment', 'SurgicalRecordCSN')
## a lot more comment length, but does get truncated
new_era_reviews$comment %>% nchar %>% summary
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#    4236    4700    4723    4714    4738    4817

## can I get something meaningful out of these?
library(jsonlite)
safify <- function(inner_function) {
  return(function(...) {
    # Use tryCatch for error handling
    tryCatch({
      inner_function(...)
    }, error = function(error) {
      # Return an empty list on error
      list()
    })
  })
}

jsonifieddata <- lapply(new_era_reviews[, comment] %>% unlist ,  safify(fromJSON) )

## these errors are caused because AW (or whoever generated this JSON) didn't sanitize quotation marks in data.
this_set <- which(lengths(jsonifieddata) ==0)
for (i in seq_along(this_set)) {
## this is annoying in R, split using substring, then combine
  text <- new_era_reviews[this_set[i] , "comment" ] 
  quote_locations <- text %>% str_locate_all( "\"") %>% extract2(1) %>% extract(,1)
  valid_locations <- text %>% str_locate_all( "[,:{}]") %>% extract2(1) %>% extract(,1)
  quote_locations <- setdiff(quote_locations,c(valid_locations+1, valid_locations-1) )
  jsonifieddata[[this_set[i] ]] <- str_flatten(str_sub( text, start=c(1, quote_locations+1), end=c(quote_locations-1, nchar(text)) ) ) %>% fromJSON
}



extract_assess <- function(x){ cbind(data.frame( assessment=lapply(x[['assess']][["CompMinor"]], function(x) {x$value} ) ) , data.frame( assessment=lapply(x[['assess']][["CompMajor"]], function(x) {x$value} ) ) )  }
comp_assessments <- lapply(jsonifieddata, extract_assess  ) %>% rbindlist(fill=TRUE)

## the long version of the same extraction
# extract_assess <- function(x){ rbind(data.frame( assessment=sapply(x[['assess']][["CompMinor"]], function(x) {x$value} ) ) , data.frame( assessment=sapply(x[['assess']][["CompMajor"]], function(x) {x$value} ) ) )  }

extract_reco <- function(x){ data.frame( assessment=lapply(x[['assess']][["CompReco"]], function(x) {x$value} ) )  }
comp_rec <- lapply(jsonifieddata, extract_reco  ) %>% rbindlist(fill=TRUE)

# CompPost ## postop predictions

## meta data
## unbearably slow not vectorized
## there are some contacts that have multiple rows. I assume this reflects multiple reachouts
if( FALSE) {
fix_nill <- function(x) {if(length(x) == 0) {return(data.frame(type="none"))} else {return(x %>% unique %>% mutate(ncontact = n(), response = na_if(lubridate:::ymd_hms(response), lubridate:::ymd_hms("0001/01/01 00:00:00") ) , sent=na_if(lubridate:::ymd_hms(sent), lubridate:::ymd_hms("0001/01/01 00:00:00") ) ) %>% mutate(sent = max(sent, na.rm=TRUE), response=max(response, na.rm=TRUE) ) %>% slice_tail(n=1)) }  }
jsonifieddata %>% lapply( .%>% extract2("contact") %>% fix_nill ) -> contact_times
for(i in seq_along(contact_times)) { contact_times[[i]]$oldrow <- i }
contact_times <- contact_times %>% bind_rows 
contact_times %<>% mutate( sent = if_else(is.finite(sent), sent, lubridate:::ymd_hms(NA) ) , response = if_else(is.finite(response), response, lubridate:::ymd_hms(NA) ) , ncontact = coalesce(ncontact, 0))
} else {
jsonifieddata %>% lapply( .%>% extract2("contact") ) -> contacts_only
contacts_only[which(lengths(contacts_only) > 0)] %>% rbindlist -> tempc
contacts_only[which(lengths(contacts_only) > 0)] %>% sapply(nrow) -> tempa
tempc %>% set(j="oldrow2", value= rep( which(lengths(contacts_only) > 0), times=tempa) )
setkey(tempc, "oldrow2")
tempc[ , ncontact := .N, by="oldrow2" ]
tempc[ , response := na_if(lubridate:::ymd_hms(response), lubridate:::ymd_hms("0001/01/01 00:00:00") ) ]
tempc[ , sent:=na_if(lubridate:::ymd_hms(sent), lubridate:::ymd_hms("0001/01/01 00:00:00") ) ]
tempc[ , sent := max(sent, na.rm=TRUE), by="oldrow2"]
tempc[ , response := max(response, na.rm=TRUE), by="oldrow2"]
tempc <- unique(tempc, by="oldrow2")
tempc[!is.finite(sent), sent := NA]
tempc[!is.finite(response), response := NA]
rbind(tempc, data.table(type="none", sent=ymd_hms(NA_character_), response=ymd_hms(NA_character_) , ncontact=0, oldrow2=which(lengths(contacts_only)==0)) ) -> contact_times

setorder(contact_times, oldrow2)
rm(contacts_only, tempc, tempa)
setnames(contact_times, "oldrow2", "oldrow")
}







## are there ever text entries?

## wide version


extract_reco <- function(x){ 
  temp <- lapply(x[['assess']][["CompReco"]], function(x) {x$text} )
  temp <- temp[nchar(unlist(temp))>0]
  data.frame( temp  )  
}

comp_text <- lapply(jsonifieddata, extract_reco  ) 

# %>% lapply(function(x) { x$rname <- rownames(x); x[ nchar(x$assessment)>1 ,1, drop=FALSE] } ) 
myindex <- sapply(comp_text, . %>% nrow %>% is_greater_than(0) )


## add index (to non-empty)

for(i in which(myindex)) {comp_text[[i]] <-  comp_text[[i]]  %>% mutate(oldrow=i)}

comp_text <- comp_text[myindex] %>% bind_rows

## merge these by oldrow
full_join(contact_times, comp_text, by="oldrow")  %>% full_join(comp_rec %>% mutate( oldrow=row_number() ) , by="oldrow") -> joined_assess

## this almost never matters
joined_assess %<>% mutate(across(starts_with("Other.CompReco."), . %>% if_else(nchar(.)<3, NA_character_, .) ) )  %>%  mutate(firstText = coalesce( !!! select(.,starts_with("Other.CompReco.") ) ) ) 

joined_assess %<>% mutate(has_response = !is.na(response) )


# joined_assess %>% group_by(oldrow) %>% filter(n() >1)
# contact_times %>% group_by(oldrow) %>% filter(n() >1)


## drop some always empty fields (these are for alerts)
new_era_reviews[, treatment:= NULL]
new_era_reviews[, atom:= NULL]
new_era_reviews[, id:= NULL]
new_era_reviews[, status:= NULL]
new_era_reviews[, message:= NULL]
new_era_reviews[, assessment:= NULL]
new_era_reviews[, action:= NULL]
new_era_reviews[, contact:= NULL]
new_era_reviews[, reaction:= NULL]

## some things that I already wrote code to fetch directly instead of full json parsing (also helped on incomplete records due to truncation but these tended to be at the top)
new_era_reviews[, risk_score:= as.numeric(str_match(comment, pattern='riskScore\":(\\d+)')[,2] )  ]
new_era_reviews[, status:= as.integer(str_match(comment, pattern='status\":(\\d+)')[,2]) ]


joined_assess <- cbind(joined_assess, new_era_reviews %>% select(time, op_sys, type, SurgicalRecordCSN,risk_score,status))



## unique cases
new_era_reviews$op_sys %>% uniqueN
# [1] 12604
new_era_reviews[type=="CaseReview"][, uniqueN(op_sys)]

setDT(joined_assess)

late_case_reviews <- all_ids[, .(op_sys=AN_3_ENC_CSN_ID, SurgicalRecordID=LOG_ID, AN_START_DATETIME, AN_STOP_DATETIME)][ joined_assess, on="op_sys", nomatch=NULL][ between((time),  AN_START_DATETIME, AN_STOP_DATETIME) ]

late_case_reviews <- join_mrns[window_start==TRUE][, .(isTreatment, SurgicalRecordID)] [ late_case_reviews, on="SurgicalRecordID", nomatch=NULL]
late_case_reviews[, nreviewstages := .N, by="SurgicalRecordID"]


setorder(late_case_reviews, SurgicalRecordID, -time, -status)

last_review <- unique(late_case_reviews, by="SurgicalRecordID")
# last_review <- late_case_reviews[ late_case_reviews[, .I[1], by="SurgicalRecordID" ]$V1 ]
last_review[, deltat := as.numeric(difftime(response, sent, units="mins") ) ]


last_review[, has_sent := !is.na(sent)]
last_review[, .( has_sent, has_response, isTreatment)] %>% table
last_review[isTreatment==1, .( has_sent, type )] %>% table



last_review[, .(has_response, status)] %>% table

last_review[ ,.(status,isTreatment)] %>% table(useNA='a')


last_review[ ,.(has_response,isTreatment, type)] %>% table(useNA='a')

last_review[type=="email" & isTreatment==1, .(has_response, status)] %>% table(useNA='a')

## almost all completed evaluations have status = 2 regardless of group, very different from the complete set of reviews
## we now basically never see "email" semt type in control, and we never see having response times in control
## status 3 may actually be an intermediate step, the last review is rarely status 3
## treatment status now divides status = 3 (waiting for reply?)
## type == none almost perfectly agrees with missing sent time
late_case_reviews[ ,.(status=max(as.integer(status), na.rm=T),isTreatment=first(isTreatment)), by="SurgicalRecordID" ][ ,.(status,isTreatment)] %>% table(useNA='a')
#       isTreatment
# status    0    1 <NA>
#   1       2   36    0
#   2    1549 2764    0
#   3       0 6397    0
#   <NA>    0    0    0

joined_assess[, .(risk_score, type)] %>% table(useNA='a')

joined_assess[, .(has_response, type)] %>% table(useNA='a')


last_review[, .(risk_score=round(mean(as.numeric(risk_score), na.rm=T),1) ), by="type"] 


new_era_reviews[ ,.(status=max(as.integer(status), na.rm=T)), by="op_sys" ][ ,.(status)] %>% table
#    1    2    3 
#   44 5169 7391

## there are 1749 treatment group patients with hand-typed text and only 60 controls
last_review[, .(is.na(firstText), isTreatment) ] %>% table(useNA='a')


## many more edits
late_case_reviews[, .(nreviewstages, isTreatment, SurgicalRecordID)] %>% unique(by="SurgicalRecordID") %>% extract(, .(nreviewstages, isTreatment)) %>% table(useNA='a')
#              isTreatment
# nreviewstages    0    1 <NA>
#          1     612 1631    0
#          2     353 2357    0
#          3     466 1588    0
#          4      57 1402    0
#          5      26 1010    0
#          6      22  508    0
#          7       5  332    0
#          8       2  188    0
#          9       3   82    0
#          10      2   39    0
#          11      1   22    0
#          12      1   16    0
#          13      0    7    0
#          14      0    5    0
#          15      0    3    0
#          16      0    2    0
#          17      0    1    0
#          19      1    1    0
#          20      0    1    0
#          21      0    1    0
#          22      0    1    0
#          <NA>    0    0    0



## any insights into early period with the truncated data?
## no, not reliably. status is always 3, version is always 2. the number of touches and time to resolution climb together, but thaty won't give me a reliable marker (someone could respond quickly)
## the ntouched in control is almost never more than 2, but that may be just curosry reviews and not translate
early_case_reviews[, .(status, isTreatment)] %>% table %>% addmargins

## denominator in a similar fashion
early_case_reviews[, Date := date(AN_START_DATETIME)]

denominators_early_cr <- join_mrns[window_start==TRUE][ Date %in% unique(early_case_reviews$Date), .(N=uniqueN(SurgicalRecordID)), by="isTreatment"]

review_counts_early <- denominators_early_cr[ early_case_reviews[, .(num_reviewed=uniqueN(SurgicalRecordID), probably_contacted=sum(ntouched>2) ), by="isTreatment"] , on="isTreatment"]
setnames(review_counts_early, "isTreatment", "TreatmentName")
review_counts_early[ , TreatmentName := fcase(TreatmentName==1, "Telemedicine Group", TreatmentName==0, "Usual Care Group" ) ]

last_review[, Date := date(AN_START_DATETIME)]
denominators_late_cr <- join_mrns[window_start==TRUE][ Date %in% unique(last_review$Date), .(N=uniqueN(SurgicalRecordID)), by="isTreatment"]

review_counts_late <- denominators_late_cr[ last_review[, .(num_reviewed=uniqueN(SurgicalRecordID), probably_contacted=sum(type!="none"), has_reply=sum(has_response==TRUE) ), by="isTreatment"] , on="isTreatment"]

setnames(review_counts_late, "isTreatment", "TreatmentName")
review_counts_late[ , TreatmentName := fcase(TreatmentName==1, "Telemedicine Group", TreatmentName==0, "Usual Care Group" ) ]

review_counts_late %>% fwrite("/research/outputs/late_review_counts.csv")
review_counts_early %>% fwrite("/research/outputs/early_review_counts.csv")

## output a sheet of indicators for having a review done / being contacted for propensity analysis
last_review[, .(SurgicalRecordID,  isTreatment, AN_START_DATETIME, has_sent, has_response  )] %>% fwrite("/research/intermediates/review_marker.csv")

pseudo_records_reviews <- join_mrns[window_start==TRUE][ Date %in%  last_review$Date][!last_review , on="SurgicalRecordID" ][, .(SurgicalRecordID, isTreatment, AN_START_DATETIME, Date, has_sent = FALSE, has_response=FALSE )] 



## compare the number of case reviews


rbind(last_review[, .(SurgicalRecordID,  isTreatment, Date, AN_START_DATETIME, has_sent, has_response  )],pseudo_records_reviews) %>% mutate( roughdate = Date %>% roughen_date ) %>% mutate(roughdate = as.factor(roughdate ) ) %>% filter(isTreatment %in% c(0,1)) %>% group_by(roughdate, isTreatment) %>% summarize(fWeight = sum(has_sent)  )  %>% ungroup %>% mutate(roughdate = as.numeric(as.character(roughdate)))  %>% arrange(roughdate, isTreatment)  %>% mutate(isTreatment = factor(isTreatment)) %>% ggplot( aes(x=roughdate, y=fWeight,  group = isTreatment, fill =isTreatment)) + geom_bar(stat = "identity", width=0.05, position=position_identity(), alpha=0.5) + labs(x = "Date", y = "Sum", title = "Case Reviews Sent by month") -> temp2
temp2 %>% ggsave(device="png", filename="/research/outputs/review_present.png")
 




logins <- fread("/research/2023_07_TECTONICS_PaperData/20231219logins.csv")
logins$V3 %>% table
#  1  2  3  4  5  6  7  8  9 10 
# 70 71 61 80 68 38 14  9  6  2 
