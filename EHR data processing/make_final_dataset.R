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
"xgboost" , 
"stringr" , 
"boot" , 
'tidyr' , 
'openssl' , 
"effectsize" , 
"optmatch" ,
"gridExtra" ) , '2024-05-01')


newsalt <- ""
## code for salt generation removed

setwd("/research")
clarity_root <- '/early_act3/'
roughen_date <- function(x) round(year(x) + (month(x)-1)/12,2)  

na_false <- function(x) {fifelse(is.na(x), FALSE, x ) }
na_zero <- function(x) {fifelse(is.na(x), 0, x ) }

additional_consort_facts <- list()

join_mrns <- fread("/research/intermediates/population.csv")
cases_by_month_summary <-  copy(join_mrns )[, roughdate := Date %>% roughen_date ][, .(UniqueCase=uniqueN(SurgicalRecordID) ), by=.(roughdate, isTreatment) ][roughdate>2019.25] [order(roughdate)]


all_flow_data<- fread("/research/intermediates/flowsheet_counts.csv")

table1_data <- fread("/research/intermediates/individual_t1_data.csv")

all_redoses <- fread("/research/intermediate_abx.csv") %>% unique

cam_holder <- fread("/research/intermediates/intermediate_cam.csv")
vent_holder2 <-fread("/research/intermediates/intermediate_ventfail.csv")
temp_holder <- fread("/research/intermediates/intermediate_temperature.csv")
map_holder <- fread("/research/intermediates/intermediate_map.csv")
fgf_holder <- fread("/research/intermediates/intermediate_fgf.csv")
vent_holder <- fread("/research/intermediates/intermediate_vent.csv")
agent_holder <- fread("/research/intermediates/intermediate_agent.csv") 

lab_outcomes <- fread("/research/intermediates/lab_outcomes.csv") %>% unique
adt_outcomes_merged <- fread("/research/intermediates/adt_imports.csv") %>% unique

## the procedure word based mortality model for subset analysis
procedure_names <- fread('2023_07_TECTONICS_PaperData/2023_07_21_IDsGalore.csv', sep="^", quote="")  [, .(AN_PROC_NAME, SurgicalRecordID=LOG_ID)]

procedure_names <- procedure_names [ join_mrns[, .(SurgicalRecordID)] , on="SurgicalRecordID" , nomatch=NULL]

text_pipe <- . %>% tolower %>% 
 gsub(x=., pattern="dos:?\\s?[0-9\\\\\\/-]+", replacement=" " ) %>% 
 gsub(pattern="[[:punct:]]", replacement=" " ,.) %>%
 gsub(pattern="\\bdr\\s+\\w+", replacement="" ,.) %>%
 gsub(pattern="\\bunos\\s+\\w+", replacement="" ,.) %>%
 gsub(pattern="\\b\\w\\b", replacement="" ,.) %>%
 gsub(pattern="\\b\\d+\\b", replacement="" ,.) %>%
 gsub(pattern="\\s+", replacement=" " ,.) %>% 
 trimws 


procedure_names[, AN_PROC_NAME := AN_PROC_NAME %>% text_pipe ]
admission_vocab <- fread("/research/intermediates/word_indicies.csv") %>% unlist

## 
newwords <- procedure_names$AN_PROC_NAME %>% strsplit(split=" ")
newwords <-sapply(newwords, match, table=admission_vocab)
newwords <-sapply(newwords, na.omit)

newcols <- Matrix::sparseMatrix( i= rep(seq_along(newwords ), newwords %>% lengths ) , j= unlist(newwords), x=1)

prediction_model <- xgb.load('/research/intermediates/name_prediction.xgb' )
procedure_names[, predicted_mortality := predict(prediction_model,newcols) ]



## The overlap times
act_times <- read_xlsx("Template4TECTONICSData_Req21Mar2023.xlsx", sheet="Population_v2") 
act_times <- act_times %>% select(SurgicalRecordID, FirstAction, LastAction)
setDT(act_times)
act_times %<>% unique

## presence of case review (5-13-2020 and later) and alerts

last_review <- fread("/research/intermediates/review_marker.csv")
last_review[, Date := date(AN_START_DATETIME)]

join_mrns[, Date:= date(Date)]
pseudo_records_reviews <- join_mrns[window_start==TRUE][ Date %in%  last_review$Date][!last_review , on="SurgicalRecordID" ][, .(SurgicalRecordID, isTreatment, AN_START_DATETIME, Date, has_sent = FALSE, has_response=FALSE )] 
last_review <- rbind(last_review,pseudo_records_reviews )


alert_data <- fread("/research/intermediates/alert_per_person.csv")

## pull in alerts for intervention scoring - there are about 1000 not included, 134 are on otherwise eligible days with I guess just no case reviews. Ignore them.
# length(setdiff(alert_data[didContact==TRUE][treatment==1]$SurgicalRecordID, combined_data$SurgicalRecordID))
# length(setdiff(alert_data[didContact==TRUE][treatment==1][surgDate > ymd("2020-05-20")]$SurgicalRecordID, last_review$SurgicalRecordID))

# alert_data[!last_review, on="SurgicalRecordID"][didContact==TRUE][treatment==1]$surgDate %>% table

last_review[, combined_contact := has_sent]

last_review[ SurgicalRecordID %in% alert_data[didContact==TRUE][treatment==1][surgDate > ymd("2020-05-20")]$SurgicalRecordID, combined_contact := TRUE]

## merge t1 data and procedure for propensity matching
## do I need labs here? not obvious that they would have played a role
## lastreview is very short, 11k. It clearly does not have all the pseudo-records it needs
localt1data <- table1_data[, .(SurgicalRecordID, Age, ASA_PS, Diabetes, Service, PtClass,WEIGHT_IN_KG, HEIGHT_IN_INCHES,TOBACCO_USE,Sex,CVA_Stroke, HTN,PlannedAnesthesia,FunctionalCapacity,Dementia_MildCognitiveImpairment,Race,Ethnicity,AFib,Asthma, CAD, CancerCurrent,   CKD,  ESRD,  COPD,emergency  ) ][last_review ,on="SurgicalRecordID"]

## OHE things
encode_onehot <- function(x, colname_prefix = "", colname_suffix = "") {
  if (!is.factor(x)) {
      x <- as.factor(x)
  }
  encoding_matrix <- contrasts(x, contrasts = FALSE)
  encoded_data <- encoding_matrix[as.integer(x),]
  colnames(encoded_data) <- paste0(colname_prefix, colnames(encoded_data), colname_suffix)
  encoded_data
}

cbind(
  localt1data[,.(SurgicalRecordID, Age, ASA_PS, Diabetes,WEIGHT_IN_KG, HEIGHT_IN_INCHES,CVA_Stroke, HTN ,FunctionalCapacity,Dementia_MildCognitiveImpairment,AFib,Asthma, CAD, CancerCurrent,   CKD,  ESRD,  COPD )],
  encode_onehot(localt1data$Service, colname_prefix="Service") %>% data.table,
  encode_onehot(localt1data$Sex, colname_prefix="Sex") %>% data.table,
  encode_onehot(localt1data$PlannedAnesthesia, colname_prefix="PlannedAnesthesia") %>% data.table,
  encode_onehot(localt1data$Race, colname_prefix="Race") %>% data.table,
  encode_onehot(localt1data$Ethnicity, colname_prefix="Ethnicity") %>% data.table
) -> localt1data


colnames(newcols) <- paste0("bow", seq(ncol(newcols)))
## join words
cbind( localt1data, newcols[match(localt1data$SurgicalRecordID,  procedure_names$SurgicalRecordID),] %>% as.matrix %>% data.table)  -> localt1data

## subset to treatment and train propensity, require date in the range with records
training_data <- table1_data[SurgDate > ymd("2020-05-20"),.(isTreatment, SurgicalRecordID)][ localt1data, on="SurgicalRecordID", nomatch=NULL][isTreatment==1]

## labels

did_review <- last_review[training_data,on='SurgicalRecordID'][,.(has_sent,combined_contact,SurgicalRecordID)]

if( any(did_review$has_sent %>% is.na )) {error("missing outcome for propensity")}

training_data[, SurgicalRecordID := NULL]
training_data[, isTreatment := NULL]

## xgb for propensity score
xgdata <- xgb.DMatrix(data=training_data %>% as.matrix, info = list(label=did_review$has_sent))
local_fit <- xgb.cv(data=xgdata, tree_method ="approx" , nrounds=30L, params=list(objective='binary:logistic',  max_depth=6L ) , predictor="cpu_predictor" , nfold=5L, metrics=c("error", "auc")) 
## this found increasing over fitting and minimal improvement after 11 rounds
local_fit <- xgb.train(data=xgdata, tree_method ="approx" , nrounds=11L, params=list(objective='binary:logistic',  max_depth=6L ) , predictor="cpu_predictor" ) 

## a linear booster was much less effective
# local_fit <- xgb.train(data=xgdata,, nrounds=20L, params=list(booster="gblinear", objective='binary:logistic',  lambda=.5, alpha=.3 ) ) 
# local_fit <- xgb.cv(data=xgdata,nfold=5, nrounds=20L, params=list(booster="gblinear", objective='binary:logistic',  lambda=.05, alpha=.05 ) , metrics=c("error", "auc")) 

xgb.save(local_fit, "/research/intermediates/propensity.xgb")

xgdata2 <- xgb.DMatrix(data=training_data %>% as.matrix, info = list(label=did_review$combined_contact))
local_fit2<- xgb.train(data=xgdata2, tree_method ="approx" , nrounds=11L, params=list(objective='binary:logistic',  max_depth=6L ) , predictor="cpu_predictor" ) 




## predict on controls
## debatable whether should include matches earlier ... contact rates change, but benefit from more potential matches

# control_data <- table1_data[SurgDate > ymd("2020-05-20"),.(isTreatment, SurgicalRecordID)][ localt1data, on="SurgicalRecordID", nomatch=NULL][isTreatment==0]
control_data <- table1_data[,.(isTreatment, SurgicalRecordID)][ localt1data, on="SurgicalRecordID", nomatch=NULL][isTreatment==0]

control_index <- control_data$SurgicalRecordID

control_data[, SurgicalRecordID := NULL]
control_data[, isTreatment := NULL]

## propensity

treatment_scores <- predict(local_fit, xgdata)
treatment_scores2 <- predict(local_fit2, xgdata)
control_scores <- predict(local_fit, control_data %>% as.matrix)
control_scores2 <- predict(local_fit2, control_data %>% as.matrix)
treatment_scores <- treatment_scores[ did_review$has_sent == TRUE]
treatment_scores2 <- treatment_scores2[ did_review$combined_contact == TRUE]


## select only treatment patients with review
## plain optmatch even with caliper is very slow; I think it does not do anything clever with sorting and does all n^2 comparisons
## two options are to stratify by key variables or make strata in the scores.

combined_data <- data.table( 
  score=c(treatment_scores, control_scores) %>% qlogis, 
  outcome=rep(c(0,1), times=c(length(treatment_scores), length(control_scores)) ) , 
  surgServ = c(table1_data[did_review[has_sent==TRUE], on="SurgicalRecordID"]$Service ,  table1_data[match(control_index, SurgicalRecordID) ]$Service ) %>% factor,
  sex= c(table1_data[did_review[has_sent==TRUE], on="SurgicalRecordID"]$Sex ,  table1_data[match(control_index, SurgicalRecordID) ]$Sex ) %>% factor
  )

match.restriction <- exactMatch(outcome~sex+surgServ , data =combined_data)

#   glm.p <- glm(new_osa~predicted_osa, data=local.imputed, family=binomial())

# mnames <- paste0("ID",c(did_review[has_sent==TRUE]$SurgicalRecordID, control_index))
# combined_scores <- combined_data$score %>% set_names(mnames) %>% qlogis
 
temp <- match_on(x=outcome~score, data= combined_data , caliper=.35 , within=match.restriction ) #
temp2<- pairmatch(temp, controls=1, remove.unmatchables=TRUE, data=combined_data)

combined_data$is_matched <- temp2
combined_data$SurgicalRecordID <- c(  did_review[has_sent==TRUE]$SurgicalRecordID, control_index  )

#   temp <- margins(model=glm(ever_del ~ new_osa, family=binomial(), data=local.imputed)  , data =local.imputed , variable='new_osa', vce='delta')
#   unlist(summary(temp)[c('AME', 'SE')])    
    
    
combined_data2 <- data.table( 
  score=c(treatment_scores2, control_scores2) %>% qlogis, 
  outcome=rep(c(0,1), times=c(length(treatment_scores2), length(control_scores2)) ) , 
  surgServ = c(table1_data[did_review[combined_contact==TRUE], on="SurgicalRecordID"]$Service ,  table1_data[match(control_index, SurgicalRecordID) ]$Service ) %>% factor,
  sex= c(table1_data[did_review[combined_contact==TRUE], on="SurgicalRecordID"]$Sex ,  table1_data[match(control_index, SurgicalRecordID) ]$Sex ) %>% factor
  )

match.restriction <- exactMatch(outcome~sex+surgServ , data =combined_data2)

#   glm.p <- glm(new_osa~predicted_osa, data=local.imputed, family=binomial())

# mnames <- paste0("ID",c(did_review[has_sent==TRUE]$SurgicalRecordID, control_index))
# combined_scores <- combined_data$score %>% set_names(mnames) %>% qlogis

temp <- match_on(x=outcome~score, data= combined_data2 , caliper=.35 , within=match.restriction ) #
temp2<- pairmatch(temp, controls=1, remove.unmatchables=TRUE, data=combined_data2)

combined_data2$is_matched <- temp2
combined_data2$SurgicalRecordID <- c(  did_review[combined_contact==TRUE]$SurgicalRecordID, control_index  )

combined_data_matched <- join_mrns[,.(SurgicalRecordID)]
combined_data_matched[ SurgicalRecordID %in% combined_data2[!is.na(is_matched)]$SurgicalRecordID, combined_match:= TRUE  ]
combined_data_matched[ SurgicalRecordID %in% combined_data[!is.na(is_matched)]$SurgicalRecordID, is_matched:= TRUE  ]


rm(newcols)
rm(newwords)


###########################
## load outcomes 
###########################

main_death <- read_xlsx("Template4TECTONICSData_Req21Mar2023.xlsx", sheet="Mortality", col_types=c("numeric", "numeric", "numeric", "date", "numeric", "date"), na="NULL") ## MRN, CSN, orlogid, tx status, DoD
main_other_out <- read_xlsx("Template4TECTONICSData_Req21Mar2023.xlsx", sheet="Outcomes", skip=1) ## orlogid, CAM, Respiratory failure, AKI
main_cam <- main_other_out[, c(1,3)] %>% set_colnames(c("SurgicalRecordID", "CAM")) %>% mutate(CAM= case_when(CAM == "Positive" ~ TRUE, CAM == "Negative"~FALSE, TRUE~NA) )
main_resp <- main_other_out[, c(5,7)] %>% set_colnames(c("SurgicalRecordID", "RespF"))
main_aki <- main_other_out[, c(9,11)] %>% set_colnames(c("SurgicalRecordID", "AKI"))

clarity_death  <- fread("2023_07_TECTONICS_PaperData/2023_07_17_Clarity_Result_Set_DeathDates.csv", sep="^", quote="")[ , Death_Date:=  ymd_hms(Death_Date) ][!is.na(Death_Date), .(CurrentMRN, Death_Date)] %>% unique ## MRN, date

setnames(cam_holder, "orlogid", "SurgicalRecordID")
## switch to my variant of resp failure, which NAs trach and pre-procedure vent
vent_holder2[, vent_fail_flow := as.numeric(postop_vent_duration > 1)]
vent_holder2[is.na(vent_fail_flow), vent_fail_flow:=0] ## I reviewed 10 of these and they are all patients who have some kind of other vent data but were NA at the merge step because they didn't have qualifying postop. Some actually had mature trachs that didn't show up in history without preop vent
vent_holder2[preop_vent_b==TRUE, vent_fail_flow:=-1]


join_mrns %>% left_join(main_cam %>% unique, by="SurgicalRecordID") %>% 
 left_join(main_resp %>% unique, by="SurgicalRecordID") %>% 
 left_join(main_aki %>% unique, by="SurgicalRecordID") %>% 
 left_join(main_death %>% select(SurgicalRecordID, DoD) %>% mutate(DoD = if_else(is.na(DoD), ymd_hms("2100-12-1 0:0:0"), DoD )) %>% unique, by="SurgicalRecordID") -> main_outcomes

main_outcomes[, no_flo_data := !(SurgicalRecordID %in% unique(all_flow_data$orlogid) ) ]
## blank AKI outcome where ESRD or high baseline cr measured
## Alex and my's disagreements are almost always inconsequential otherwise

main_outcomes <- lab_outcomes[, .(SurgicalRecordID, exclude_aki, aki_grade)][main_outcomes , on="SurgicalRecordID", nomatch=NA]
setnames(main_outcomes, "AKI" , "oldAKI")
main_outcomes[, AKI := aki_grade > 0]
main_outcomes[exclude_aki==TRUE, AKI:=NA] 
main_outcomes[, aki_grade:=NULL]
additional_consort_facts[["AKI"]] <- main_outcomes[window_start==TRUE, .(not_eligible=sum(is.na(AKI)), eligible=sum(is.finite(AKI)) ), by='isTreatment']

main_outcomes <- vent_holder2[,.(SurgicalRecordID=orlogid,vent_fail_flow,preop_vent_b)] [main_outcomes, on="SurgicalRecordID" ]
main_outcomes[is.na(vent_fail_flow) , vent_fail_flow:=0]
main_outcomes[vent_fail_flow<0, vent_fail_flow:=NA]
main_outcomes[, vent_fail_flow:= as.logical(vent_fail_flow)]
main_outcomes[no_flo_data==TRUE, vent_fail_flow := NA ]
setnames(main_outcomes, "RespF", "oldRespF")
setnames(main_outcomes, "vent_fail_flow", "RespF")
additional_consort_facts[["RespF"]] <- main_outcomes[window_start==TRUE, .(no_flo_data=sum(no_flo_data) , not_eligible=sum(preop_vent_b, na.rm=T ), eligible=sum(is.finite(RespF) ), .N ), by='isTreatment']


## merge the other source of death data, which has a few differences. On manual review, when either is true was always true.
main_outcomes <- clarity_death [main_outcomes, on="CurrentMRN==MRN", allow.cartesian=TRUE ]

main_outcomes[, EarlyDeath := as.numeric(difftime(DoD, Date, units="days" ) )  ] 
main_outcomes[, EarlyDeath2 := as.numeric(difftime(Death_Date, Date, units="days" ) )  ] 

## this happens exactly once (a clear data entry error)
main_outcomes[EarlyDeath <0, EarlyDeath := NA ]
main_outcomes[EarlyDeath2 <0, EarlyDeath2 := NA ]

main_outcomes[ , EarlyDeath := (EarlyDeath <30) | (EarlyDeath2 < 30)]
main_outcomes[ , EarlyDeath2 := NULL]

main_outcomes[ is.na(EarlyDeath), EarlyDeath := FALSE]

## my and Alex's cam almost always agree, but send some for manual adjucation
main_outcomes <- cam_holder[,.(SurgicalRecordID,preop_del ,postop_del)  ] [main_outcomes, on="SurgicalRecordID" ]
main_outcomes[!(preop_del==TRUE), incident_del:=postop_del ] 
main_outcomes[no_flo_data==TRUE, incident_del := NA ]

additional_consort_facts[["CAM"]] <- main_outcomes[window_start==TRUE, .(no_flo_data=sum(no_flo_data) , not_eligible=sum(preop_del, na.rm=T ), no_measurements=sum( is.na(postop_del) & (!no_flo_data) & (!na_false(preop_del)) )  , eligible=sum(is.finite(incident_del) ) , .N), by='isTreatment']
setnames(main_outcomes, "CAM", "oldCAM")
setnames(main_outcomes, "incident_del", "CAM")

setnames(main_outcomes, "Date", "SurgDate")



if(FALSE) {
main_outcomes[preop_del == TRUE, oldCAM := NA]
main_outcomes[preop_vent_b == TRUE, oldRespF := NA]
main_outcomes[ , roughdate := AN_START_DATETIME %>% lubridate::quarter(x=., with_year=T)]
main_outcomes[ , .( newrate=mean(RespF, na.rm=T) %>% round(3), oldrate=mean(oldRespF, na.rm=T) %>% round(3), newN=sum(is.finite(RespF)) , oldN=sum(is.finite(oldRespF)) , newCAMrate=mean(CAM, na.rm=T)%>% round(3), oldCAMrate=mean(oldCAM, na.rm=T)%>% round(3) , newCAMN=sum(is.finite(CAM)) , oldCAMN=sum(is.finite(oldCAM))   ) , by="roughdate"][order(roughdate)]
}


sink("/research/outputs/consort2.txt")
print(additional_consort_facts)
sink()

secondary_merged <- all_redoses[, .(Abx_success= all(Abx_success) ) , by='SurgicalRecordID' ] %>% 
merge( temp_holder[ ,.(SurgicalRecordID= orlogid, temp_good, mean_temp) ], by="SurgicalRecordID", all=T ) %>%
merge( map_holder[ ,.(SurgicalRecordID= orlogid, hypotension_fraction = low_map_time/total_map_time, low_map_auc) ], by="SurgicalRecordID", all=T ) %>%
merge( fgf_holder[ ,.(SurgicalRecordID= orlogid, eff_flow=eff_flow>0.9) ], by="SurgicalRecordID", all=T ) %>%
merge( vent_holder[ ,.(SurgicalRecordID= orlogid, pip_mean, pip_good_frac) ], by="SurgicalRecordID", all=T ) %>%
merge( agent_holder[ ,.(SurgicalRecordID= orlogid, no_gaps = (njumps==0)) ], by="SurgicalRecordID", all=T ) %>%
merge( lab_outcomes %>% select(SurgicalRecordID, first_glucose , ratio_cre, aki_grade ) %>% mutate( first_glucose = (first_glucose>200), aki_grade  , ratio_cre=log(ratio_cre) ) %>% data.table , by="SurgicalRecordID", all=T ) %>%
merge( adt_outcomes_merged ,by="SurgicalRecordID", all=T ) %>% 
merge( main_outcomes[, .(EarlyDeath, SurgicalRecordID)],by="SurgicalRecordID", all=T )

## handle informative censoring and truncate outliers
secondary_merged[EarlyDeath == TRUE, ICULoS:= 30*24]
secondary_merged[EarlyDeath == TRUE, postop_los:= 90]
secondary_merged[, postop_los:= pmin(90,postop_los) ]
secondary_merged[, ICULoS:= pmin(30*24,ICULoS) ]
secondary_merged[, EarlyDeath:=NULL]
secondary_merged[!is.finite(readmission_survival), readmission_survival:=365]




####
## main analysis: exclude repeats within 30 days
####

## propensity matched
# 
# prop_data_set <- merge(combined_data_matched , main_outcomes, by="SurgicalRecordID") %>% merge(secondary_merged, by="SurgicalRecordID", all.x=TRUE )
# setDT(prop_data_set)
# prop_data_set[, ICU := ICU %>% na_false]
# prop_data_set[, ICULoS := ICULoS %>% na_zero]
# prop_data_set[, postop_los := postop_los %>% na_zero]
# prop_data_set[, cluster_id := fct_cross(as_factor(OR_Abbrev), as_factor(as.character(SurgDate)) ) ]
# setkey(prop_data_set, SurgDate, OR_Abbrev)
# prop_data_set[, thisq := lubridate::quarter(SurgDate, with_year=T)  ]
# prop_data_set[, treatment := fcase( isTreatment==0, "Control", isTreatment==1, "Treatment" )  ]
# prop_data_set[ , sumOuts := na_false(CAM) + na_false(as.logical(RespF) ) + na_false(as.logical(AKI) ) + na_false(EarlyDeath) ]
# prop_data_set[ , CAM2 := na_false(CAM) ]
# # glm(EarlyDeath ~ isTreatment, data=prop_data_set) %>% summary
# prop_data_set %>% fwrite("/research/outputs/perprotocol_review_data.csv")


# primary_data_set <- data.table(table1_data %>% select(SurgicalRecordID, MRN=CurrentMRN, OR_Abbrev)%>% unique) [ main_outcomes, on="SurgicalRecordID"]
primary_data_set <- merge(main_outcomes, table1_data [, .(SurgicalRecordID, Age_high = Age >= 65, ASA_high= ASA_PS>= 3, Diabetes, Service) ] , by="SurgicalRecordID", all.x=TRUE) %>% merge(secondary_merged, by="SurgicalRecordID", all.x=TRUE )
primary_data_set[, ICU := ICU %>% na_false]
primary_data_set[, ICULoS := ICULoS %>% na_zero]
primary_data_set[, postop_los := postop_los %>% na_zero]

# primary_data_set[, EarlyDeath := as.numeric(difftime(DoD, SurgDate, units="days" ) ) < 30 ] 

primary_data_set[, cluster_id := fct_cross(as_factor(OR_Abbrev), as_factor(as.character(SurgDate)) ) ]
primary_data_set[, thisq := lubridate::quarter(SurgDate, with_year=T)  ]
primary_data_set[, treatment := fcase( isTreatment==0, "Control", isTreatment==1, "Treatment" )  ]
primary_data_set[ , sumOuts := na_false(CAM) + na_false(as.logical(RespF) ) + na_false(as.logical(AKI) ) + na_false(EarlyDeath) ]
primary_data_set[ , CAM2 := na_false(CAM) ]
primary_data_set <- merge(primary_data_set, combined_data_matched[,.(SurgicalRecordID, is_matched=!is.na(is_matched), combined_match= !is.na(combined_match) )], all.x=TRUE, by="SurgicalRecordID")

# setorder(primary_data_set, MRN, SurgDate)
# primary_data_set [ ,window_start:=pure_r_window(as.numeric(difftime(SurgDate, min(SurgDate) , units="days") ))  , by="MRN"]
# primary_data_set <- primary_data_set[window_start == TRUE]
setkey(primary_data_set, SurgDate, OR_Abbrev)

primary_data_set[, first_glucose_restr := fifelse(Diabetes==FALSE, NA, first_glucose)  ]

primary_data_set <- procedure_names[, .(predicted_mortality, SurgicalRecordID)] [primary_data_set, on="SurgicalRecordID", nomatch=NA]
primary_data_set[, predicted_risk_high := predicted_mortality > quantile(x=predicted_mortality, probs=.75, na.rm=TRUE) ]
primary_data_set$predicted_mortality %>% summary %>% round(4) %>% as.list %>% data.frame %>% write.csv("/research/intermediates/risk_quantiles.csv")

primary_data_set <- act_times [primary_data_set, on="SurgicalRecordID", nomatch=NA]
primary_data_set[, overlap:= as.numeric( difftime(pmin(AN_STOP_DATETIME, LastAction) , pmax(AN_START_DATETIME, FirstAction), units="mins") ) / as.numeric( difftime(AN_STOP_DATETIME,AN_START_DATETIME, units="mins") )   ] 

primary_data_set[, overlap_high := overlap > 0.5]



primary_data_set[, .(CurrentMRN, SurgicalRecordID,OR_Abbrev, SurgDate, isTreatment,SERVICE_collapsed, ## real ids
EarlyDeath, Death_Date = pmin(Death_Date, DoD, na.rm=T), RespF, oldRespF, CAM, oldCAM, postop_del, preop_del, AKI, exclude_aki, aki_grade, ratio_cre,  ## primary outcomes with supporting / alternative
postop_los, Abx_success, temp_good, mean_temp, hypotension_fraction, no_gaps, low_map_auc, eff_flow, pip_mean, pip_good_frac , first_glucose, ICU, ICULoS, readmission_survival,   ## secondary outcomes
window_start, Age_high, ASA_high, predicted_risk_high, overlap_high, Diabetes     ## subanalysis state
, is_matched, combined_match, emergency)] %>% fwrite("/research/outputs/identified_final_data.csv")

## map to anonymous IDs
## ## ## ## 
## code section deleted from public repo
## ## ## ## 


primary_data_set[ , .(MRN_encoded, orlogid_encoded,cluster_encoded , CurrentMRN, SurgicalRecordID,OR_Abbrev, SurgDate,  cluster_id) ] %>% fwrite("/research/outputs/reidentify_data.csv")

dei_data <- primary_data_set[, .(MRN_encoded, orlogid_encoded,cluster_encoded,SurgDate_encoded,OR_Abbrev_encoded, isTreatment,SERVICE_collapsed, ##  ids
EarlyDeath,  RespF, CAM, AKI,   ## primary outcomes with supporting / alternative
postop_los=round(postop_los), Abx_success, temp_good, mean_temp=round(mean_temp,1), hypotension_fraction=hypotension_fraction %>% round(3), no_gaps, low_map_auc=round(low_map_auc), eff_flow=eff_flow %>% round(2), pip_mean=pip_mean %>% round , pip_good_frac= pip_good_frac %>% round(2) , first_glucose, ICU, ICULoS = (ICULoS/24) %>% round(1), readmission_survival = readmission_survival %>% round,  aki_grade, ratio_cre = ratio_cre %>% round(2), ## secondary outcomes
window_start, Age_high, ASA_high, predicted_risk_high, overlap_high, Diabetes     ## subanalysis state
, is_matched, combined_match,
emergency)] 

setorder(dei_data, isTreatment, MRN_encoded, orlogid_encoded)

dei_data %>% fwrite("/research/outputs/deidentified_final_data.csv")



set.seed(101)
primary_data_set[, randorder:= runif(.N)]

setorder(primary_data_set, EarlyDeath, CAM, RespF, AKI, randorder)
primary_data_set[, randorder:= NULL]

primary_data_set[ primary_data_set[, .I[seq_len(min(.N, 3)) ] , by=.(EarlyDeath, CAM, RespF, AKI)]$V1 ] %>% fwrite("/research/outputs/random_verify.csv")


table1_data[Race == "", Race:="Unknown or Other" ]

table1_data <- primary_data_set[, .(SurgicalRecordID,window_start)][table1_data, on="SurgicalRecordID"]
 
 table1_sum <- table1_data %>% filter(window_start==TRUE)  %>% mutate( roughdate = SurgDate %>% roughen_date ) %>% mutate(roughdate = as.factor(roughdate ) ) %>% group_by(roughdate, isTreatment) %>% summarize(fEval = mean(!is.na(EvalDate)) ,fWeight = mean(!is.na(WEIGHT_IN_KG))  )  %>% ungroup %>% mutate(roughdate = as.numeric(as.character(roughdate)))  %>% arrange(roughdate, isTreatment) 
  table1_sum %>%  fwrite("/research/outputs/t1_presense.csv")
table1_sum %>% mutate(isTreatment = factor(isTreatment)) %>% ggplot( aes(x=roughdate, y=fEval,  group = isTreatment, fill =isTreatment)) + geom_bar(stat = "identity", width=0.05, position=position_identity(), alpha=0.5) + labs(x = "Date", y = "Fraction", title = "CPAP by month") -> temp2
temp2 %>% ggsave(device="png", filename="/research/outputs/CPAP_present.png")
 
table1_sum %>% mutate(isTreatment = factor(isTreatment)) %>% ggplot( aes(x=roughdate, y=fWeight,  group = isTreatment, fill =isTreatment)) + geom_bar(stat = "identity", width=0.05, position=position_identity(), alpha=0.5) + labs(x = "Date", y = "Fraction", title = "Weight by month") -> temp2
temp2 %>% ggsave(device="png", filename="/research/outputs/Weight_present.png")
 
table1_data %<>% mutate(FunctionalCapacity  = FunctionalCapacity %>% add(1L) %>% factor(levels = c(1:4), labels=c("<4 METs", "4-6 METs", "6-10 METs", ">10 METs" ), ordered=TRUE ) %>% addNA %>% fct_recode(`Unknown or Unable to Assess`=NA_character_) )


table1_data %<>% mutate(Sex  = Sex %>% case_match("F"~"Female", "M"~"Male", .default="Unknown or Other" ) ) 
 
#  smartdata [SDE == "EPIC#37999" , Value := dplyr::recode( Value, "ambulates with assistance only"="1" , "<4 METs"="2" , "4-6 METs"="3", "6-10 METs"="4", ">10 METs"="5", "cannot ambulate" ="1" , .default=NA_character_ ) ]

#  table1_data %>% group_by(isTreatment) %>% summarize(across( one_of("Age", "ASA_PS", "AFib", 'Anemia' ,'Asthma','CAD','CancerCurrent','CKD','ESRD', 'COPD', 'Dementia_MildCognitiveImpairment', 'Diabetes', 'CVA_Stroke' ,'VTE', 'OSA', 'HTN') , list(mean= . %>% mean(na.rm=T) , sd= . %>% sd(na.rm=T) )) )
binary_t1 <- table1_data %>% filter(window_start==TRUE) %>% mutate(across( one_of("AFib", 'Anemia' ,'Asthma','CAD','CancerCurrent','CKD','ESRD', 'COPD', 'Dementia_MildCognitiveImpairment', 'Diabetes', 'CVA_Stroke' ,'VTE', 'OSA', 'HTN') , function(x) {if_else(is.na(x), FALSE, x )} ) ) %>% mutate(across( one_of("AFib", 'Anemia' ,'Asthma','CAD','CancerCurrent','CKD','ESRD', 'COPD', 'Dementia_MildCognitiveImpairment', 'Diabetes', 'CVA_Stroke' ,'VTE', 'OSA', 'HTN', "emergency") , function(x) {if_else(is.na(EvalDate), NA, x )} ) )  %>% group_by(isTreatment) %>% summarize(across( one_of("AFib", 'Anemia' ,'Asthma','CAD','CancerCurrent','CKD','ESRD', 'COPD', 'Dementia_MildCognitiveImpairment', 'Diabetes', 'CVA_Stroke' ,'VTE', 'OSA', 'HTN', "emergency") , function(x) { paste0(sum(x, na.rm=T), "/" , sum(!is.na(x) ) , " (",round(mean(x, na.rm=T) ,3)*100,"%)" ) }  ) ) %>% t

binary_t1_all <- table1_data %>% filter(window_start==TRUE) %>% mutate(across( one_of("AFib", 'Anemia' ,'Asthma','CAD','CancerCurrent','CKD','ESRD', 'COPD', 'Dementia_MildCognitiveImpairment', 'Diabetes', 'CVA_Stroke' ,'VTE', 'OSA', 'HTN', "emergency") , function(x) {if_else(is.na(x), FALSE, x )} ) ) %>% mutate(across( one_of("AFib", 'Anemia' ,'Asthma','CAD','CancerCurrent','CKD','ESRD', 'COPD', 'Dementia_MildCognitiveImpairment', 'Diabetes', 'CVA_Stroke' ,'VTE', 'OSA', 'HTN', "emergency") , function(x) {if_else(is.na(EvalDate), NA, x )} ) )  %>% summarize(across( one_of("AFib", 'Anemia' ,'Asthma','CAD','CancerCurrent','CKD','ESRD', 'COPD', 'Dementia_MildCognitiveImpairment', 'Diabetes', 'CVA_Stroke' ,'VTE', 'OSA', 'HTN', "emergency") , function(x) { paste0(sum(x, na.rm=T), "/" , sum(!is.na(x) ) , " (",round(mean(x, na.rm=T) ,3)*100,"%)" ) }  ) ) %>% t %>% as_tibble(rownames="variable") %>% set_colnames(c("variable", "all") )


phi_print <- function(x) { paste0(round(x[1,1] ,2) %>% sprintf(fmt="%0.2f"), " (", round(x[1,3] ,2)  %>% sprintf(fmt="%0.2f"), " , ", round(x[1,4] ,2) %>% pmax(.01), ")") }
binary_t1 <- binary_t1[-1,] %>% as_tibble(rownames="variable") %>% set_colnames(c("variable", "Control", "Interv"))

binary_t1_eff <- table1_data %>% filter(window_start==TRUE) %>% mutate(across( one_of("AFib", 'Anemia' ,'Asthma','CAD','CancerCurrent','CKD','ESRD', 'COPD', 'Dementia_MildCognitiveImpairment', 'Diabetes', 'CVA_Stroke' ,'VTE', 'OSA', 'HTN', "emergency") , function(x) {if_else(is.na(x), FALSE, x )} ) ) %>% mutate(across( one_of("AFib", 'Anemia' ,'Asthma','CAD','CancerCurrent','CKD','ESRD', 'COPD', 'Dementia_MildCognitiveImpairment', 'Diabetes', 'CVA_Stroke' ,'VTE', 'OSA', 'HTN', "emergency") , function(x) {if_else(is.na(EvalDate), NA, x )} ) ) %>% summarize(across( one_of("AFib", 'Anemia' ,'Asthma','CAD','CancerCurrent','CKD','ESRD', 'COPD', 'Dementia_MildCognitiveImpairment', 'Diabetes', 'CVA_Stroke' ,'VTE', 'OSA', 'HTN', "emergency") , function(x) {bind_cols(a=x,b=isTreatment) %>% table %>% phi(alternative="two.sided") %>% phi_print}    ) ) %>% t %>% as_tibble(rownames="variable") %>% set_colnames(c("variable", "eff"))

binary_t1<- left_join(binary_t1, binary_t1_eff, by="variable") %>% left_join( binary_t1_all, by="variable")


cont_t1 <-  table1_data %>% filter(window_start==TRUE)  %>% filter(window_start==TRUE)  %>% group_by(isTreatment) %>% summarize(across( one_of( "Age") , function(x){ paste0(round(mean(x, na.rm=T), 1) , " (",round(sd(x, na.rm=T) ,1),")" ) }   )  ) %>% t

cont_t1_all <-  table1_data %>% filter(window_start==TRUE)  %>% filter(window_start==TRUE)  %>%  summarize(across( one_of( "Age") , function(x){ paste0(round(mean(x, na.rm=T), 1) , " (",round(sd(x, na.rm=T) ,1),")" ) }   )  ) %>% t  %>% as_tibble(rownames="variable") %>% set_colnames(c("variable", "all") )

cont_t1 <- cont_t1[-1,,drop=F] %>% as_tibble(rownames="variable") %>% set_colnames(c("variable", "Control", "Interv"))

cont_t1_eff<-   lapply( c("Age") , function(x){ tibble(variable=x, eff=cohens_d(x,"isTreatment", data=table1_data %>% filter(window_start==TRUE) %>% mutate(isTreatment=factor(isTreatment))) %>% phi_print) } ) %>% bind_rows

cont_t1 %<>% left_join(cont_t1_eff, by="variable")%>% left_join( cont_t1_all, by="variable")


## doing this without an explicit loop is agony
cat_t1 <-  lapply( c("Race", "Sex", "FunctionalCapacity", "ASA_PS", "Service"), 
function(x) {
  table1_data %>% filter(window_start==TRUE)  %>% dplyr::count(isTreatment, get(x) ) %>% 
  rename(v1 = `get(x)`) %>% 
  mutate(v1=as.character(v1)) %>% group_by(v1) %>% 
  mutate(n2 = sum(n)) %>% ungroup %>%
  mutate(v1 = if_else(is.na(v1), "Unknown or Other", v1 ) ) %>%
  mutate(v1 = if_else(v1 =='NA', "Unknown or Other", v1 ) ) %>%
  mutate(v1 = if_else(n2 < 100, "Unknown or Other", v1 ) ) %>%
  group_by(isTreatment, v1) %>% summarize(n=sum(n)) %>%
  ungroup %>% group_by(isTreatment) %>% 
  mutate(frac = n/sum(n)) %>% 
  mutate( state = paste0( n, "/", sum(n), " (" , round(frac,3)*100, "%)" )) %>% 
  pivot_wider( id_cols = "v1", values_from="state", names_from="isTreatment") %>% 
  set_colnames(c("level", "Control", "Interv" ))%>% 
  mutate(level= as.character(level))  %>% 
  mutate(variable=x) %>% select( variable, everything()) -> temp
  if("factor" %in% class(table1_data[[x]]) ) {
    temp <- temp[na.omit(match(levels(table1_data[[x]]) ,temp$level  ) ), ]
  }
  return(temp)
  
} ) %>% set_names(c("Race", "Sex", "FunctionalCapacity", "ASA_PS", "Service"))

cat_t1_all <-  lapply( c("Race", "Sex", "FunctionalCapacity", "ASA_PS", "Service"), 
function(x) {
  table1_data %>% filter(window_start==TRUE)  %>% dplyr::count(get(x) ) %>% 
  rename(v1 = `get(x)`) %>%
  mutate(v1=as.character(v1)) %>% 
  mutate(v1 = if_else(is.na(v1), "Unknown or Other", v1 ) ) %>%
  mutate(v1 = if_else(v1 =='NA', "Unknown or Other", v1 ) ) %>%
  mutate(v1 = if_else(n < 100, "Unknown or Other", v1 ) ) %>%
  group_by(v1) %>% summarize(n=sum(n)) %>%
  ungroup %>% 
  mutate(frac = n/sum(n)) %>% 
  mutate( state = paste0( n, "/", sum(n), " (" , round(frac,3)*100, "%)" )) %>% 
  select(level=v1, all=state ) %>% 
  mutate( level= as.character(level))  %>% 
  mutate(variable=x) %>% select( variable, everything()) -> temp
  if("factor" %in% class(table1_data[[x]]) ) {
    temp <- temp[na.omit(match(levels(table1_data[[x]]) ,temp$level  ) ), ]
  }
  return(temp)
} )  %>% set_names(c("Race", "Sex", "FunctionalCapacity", "ASA_PS", "Service"))

for( thisname in names(cat_t1)) {
cat_t1[[thisname]] <- left_join(cat_t1[[thisname]], cat_t1_all[[thisname]], by=c("variable", "level"))
}

cat_t1_eff <-   table1_data%>% filter(window_start==TRUE)  %>% summarize(across( one_of( "Race", "Sex", "FunctionalCapacity", "ASA_PS", "Service"), function(x) {bind_cols(a=x,b=isTreatment) %>% mutate(a=as.character(a)) %>% table %>% cramers_v(alternative="two.sided") %>% phi_print}    ) ) %>% t %>% as_tibble(rownames="variable") %>% set_colnames(c("variable", "eff"))

cat_t1<-lapply( cat_t1, . %>% left_join(cat_t1_eff, by="variable") %>% mutate(eff= c(eff[1], rep("", n()-1 ) ) ) )

cat_t1<-lapply( cat_t1, function(x) {bind_rows(tibble::tibble_row(variable=first(x$variable), Control="", Interv="", eff=first(x$eff) , all=""), x %>% select(variable=level, Control, Interv, all) %>% mutate( eff="") %>% mutate(variable = paste0("   ", variable) ) ) %>% select( variable, Control, Interv, eff, all ) } )



bind_rows(cont_t1 , binary_t1 ,  cat_t1 %>% bind_rows) %>% select(variable, Interv, Control, eff, all) %>% rename(Intervention=Interv, `Standardized diff`=eff) -> combined_t1

combined_t1 %<>% mutate(variable = variable %>% case_match( "CancerCurrent"~"Cancer","Dementia_MildCognitiveImpairment"~"Dementia", "CVA_Stroke"~"Stroke", "FunctionalCapacity"~"Functional Capacity", "ASA_PS"~"ASA-PS", "AFib"~"Atrial Fib" ,.default=variable  ) )

combined_t1%>% fwrite("/research/outputs/table1.csv")


## same comparing reviewed vs not

intervention_only <- last_review[table1_data[isTreatment==1]  ,on="SurgicalRecordID", nomatch=NULL]
## filter date implicitly done, pseudo records already present
## this rename facilitates code recycle
intervention_only[, isTreatment := fcoalesce(has_sent, FALSE) ]

binary_t1 <- intervention_only %>% filter(window_start==TRUE) %>% mutate(across( one_of("AFib", 'Anemia' ,'Asthma','CAD','CancerCurrent','CKD','ESRD', 'COPD', 'Dementia_MildCognitiveImpairment', 'Diabetes', 'CVA_Stroke' ,'VTE', 'OSA', 'HTN', "emergency") , function(x) {if_else(is.na(x), FALSE, x )} ) ) %>% mutate(across( one_of("AFib", 'Anemia' ,'Asthma','CAD','CancerCurrent','CKD','ESRD', 'COPD', 'Dementia_MildCognitiveImpairment', 'Diabetes', 'CVA_Stroke' ,'VTE', 'OSA', 'HTN', "emergency") , function(x) {if_else(is.na(EvalDate), NA, x )} ) )  %>% group_by(isTreatment) %>% summarize(across( one_of("AFib", 'Anemia' ,'Asthma','CAD','CancerCurrent','CKD','ESRD', 'COPD', 'Dementia_MildCognitiveImpairment', 'Diabetes', 'CVA_Stroke' ,'VTE', 'OSA', 'HTN', "emergency") , function(x) { paste0(sum(x, na.rm=T), "/" , sum(!is.na(x) ) , " (",round(mean(x, na.rm=T) ,3)*100,"%)" ) }  ) ) %>% t

binary_t1_all <- intervention_only %>% filter(window_start==TRUE) %>% mutate(across( one_of("AFib", 'Anemia' ,'Asthma','CAD','CancerCurrent','CKD','ESRD', 'COPD', 'Dementia_MildCognitiveImpairment', 'Diabetes', 'CVA_Stroke' ,'VTE', 'OSA', 'HTN', "emergency") , function(x) {if_else(is.na(x), FALSE, x )} ) ) %>% mutate(across( one_of("AFib", 'Anemia' ,'Asthma','CAD','CancerCurrent','CKD','ESRD', 'COPD', 'Dementia_MildCognitiveImpairment', 'Diabetes', 'CVA_Stroke' ,'VTE', 'OSA', 'HTN', "emergency") , function(x) {if_else(is.na(EvalDate), NA, x )} ) )  %>% summarize(across( one_of("AFib", 'Anemia' ,'Asthma','CAD','CancerCurrent','CKD','ESRD', 'COPD', 'Dementia_MildCognitiveImpairment', 'Diabetes', 'CVA_Stroke' ,'VTE', 'OSA', 'HTN', "emergency") , function(x) { paste0(sum(x, na.rm=T), "/" , sum(!is.na(x) ) , " (",round(mean(x, na.rm=T) ,3)*100,"%)" ) }  ) ) %>% t %>% as_tibble(rownames="variable") %>% set_colnames(c("variable", "all") )


phi_print <- function(x) { paste0(round(x[1,1] ,2) %>% sprintf(fmt="%0.2f"), " (", round(x[1,3] ,2)  %>% sprintf(fmt="%0.2f"), " , ", round(x[1,4] ,2) %>% pmax(.01), ")") }
binary_t1 <- binary_t1[-1,] %>% as_tibble(rownames="variable") %>% set_colnames(c("variable", "Control", "Interv"))

binary_t1_eff <- intervention_only %>% filter(window_start==TRUE) %>% mutate(across( one_of("AFib", 'Anemia' ,'Asthma','CAD','CancerCurrent','CKD','ESRD', 'COPD', 'Dementia_MildCognitiveImpairment', 'Diabetes', 'CVA_Stroke' ,'VTE', 'OSA', 'HTN', "emergency") , function(x) {if_else(is.na(x), FALSE, x )} ) ) %>% mutate(across( one_of("AFib", 'Anemia' ,'Asthma','CAD','CancerCurrent','CKD','ESRD', 'COPD', 'Dementia_MildCognitiveImpairment', 'Diabetes', 'CVA_Stroke' ,'VTE', 'OSA', 'HTN', "emergency") , function(x) {if_else(is.na(EvalDate), NA, x )} ) ) %>% summarize(across( one_of("AFib", 'Anemia' ,'Asthma','CAD','CancerCurrent','CKD','ESRD', 'COPD', 'Dementia_MildCognitiveImpairment', 'Diabetes', 'CVA_Stroke' ,'VTE', 'OSA', 'HTN', "emergency") , function(x) {bind_cols(a=x,b=isTreatment) %>% table %>% phi(alternative="two.sided") %>% phi_print}    ) ) %>% t %>% as_tibble(rownames="variable") %>% set_colnames(c("variable", "eff"))

binary_t1<- left_join(binary_t1, binary_t1_eff, by="variable") %>% left_join( binary_t1_all, by="variable")

cont_t1 <-  intervention_only %>% filter(window_start==TRUE)  %>% filter(window_start==TRUE)  %>% group_by(isTreatment) %>% summarize(across( one_of( "Age") , function(x){ paste0(round(mean(x, na.rm=T), 1) , " (",round(sd(x, na.rm=T) ,1),")" ) }   )  ) %>% t

cont_t1_all <-  intervention_only %>% filter(window_start==TRUE)  %>% filter(window_start==TRUE)  %>%  summarize(across( one_of( "Age") , function(x){ paste0(round(mean(x, na.rm=T), 1) , " (",round(sd(x, na.rm=T) ,1),")" ) }   )  ) %>% t  %>% as_tibble(rownames="variable") %>% set_colnames(c("variable", "all") )

cont_t1 <- cont_t1[-1,,drop=F] %>% as_tibble(rownames="variable") %>% set_colnames(c("variable", "Control", "Interv"))

cont_t1_eff<-   lapply( c("Age") , function(x){ tibble(variable=x, eff=cohens_d(x,"isTreatment", data=intervention_only %>% filter(window_start==TRUE) %>% mutate(isTreatment=factor(isTreatment))) %>% phi_print) } ) %>% bind_rows

cont_t1 %<>% left_join(cont_t1_eff, by="variable")%>% left_join( cont_t1_all, by="variable")


## doing this without an explicit loop is agony
cat_t1 <-  lapply( c("Race", "Sex", "FunctionalCapacity", "ASA_PS", "Service"), 
function(x) {
  intervention_only %>% filter(window_start==TRUE)  %>% dplyr::count(isTreatment, get(x) ) %>% 
  rename(v1 = `get(x)`) %>% 
  mutate(v1=as.character(v1)) %>% group_by(v1) %>% 
  mutate(n2 = sum(n)) %>% ungroup %>%
  mutate(v1 = if_else(is.na(v1), "Unknown or Other", v1 ) ) %>%
  mutate(v1 = if_else(v1 =='NA', "Unknown or Other", v1 ) ) %>%
  mutate(v1 = if_else(n2 < 100, "Unknown or Other", v1 ) ) %>%
  group_by(isTreatment, v1) %>% summarize(n=sum(n)) %>%
  ungroup %>% group_by(isTreatment) %>% 
  mutate(frac = n/sum(n)) %>% 
  mutate( state = paste0( n, "/", sum(n), " (" , round(frac,3)*100, "%)" )) %>% 
  pivot_wider( id_cols = "v1", values_from="state", names_from="isTreatment") %>% 
  set_colnames(c("level", "Control", "Interv" ))%>% 
  mutate(level= as.character(level))  %>% 
  mutate(variable=x) %>% select( variable, everything()) -> temp
  if("factor" %in% class(intervention_only[[x]]) ) {
    temp <- temp[na.omit(match(levels(intervention_only[[x]]) ,temp$level  ) ), ]
  }
  return(temp)
  
} ) %>% set_names(c("Race", "Sex", "FunctionalCapacity", "ASA_PS", "Service"))

cat_t1_all <-  lapply( c("Race", "Sex", "FunctionalCapacity", "ASA_PS", "Service"), 
function(x) {
  intervention_only %>% filter(window_start==TRUE)  %>% dplyr::count(get(x) ) %>% 
  rename(v1 = `get(x)`) %>%
  mutate(v1=as.character(v1)) %>% 
  mutate(v1 = if_else(is.na(v1), "Unknown or Other", v1 ) ) %>%
  mutate(v1 = if_else(v1 =='NA', "Unknown or Other", v1 ) ) %>%
  mutate(v1 = if_else(n < 100, "Unknown or Other", v1 ) ) %>%
  group_by(v1) %>% summarize(n=sum(n)) %>%
  ungroup %>% 
  mutate(frac = n/sum(n)) %>% 
  mutate( state = paste0( n, "/", sum(n), " (" , round(frac,3)*100, "%)" )) %>% 
  select(level=v1, all=state ) %>% 
  mutate( level= as.character(level))  %>% 
  mutate(variable=x) %>% select( variable, everything()) -> temp
  if("factor" %in% class(intervention_only[[x]]) ) {
    temp <- temp[na.omit(match(levels(intervention_only[[x]]) ,temp$level  ) ), ]
  }
  return(temp)
} )  %>% set_names(c("Race", "Sex", "FunctionalCapacity", "ASA_PS", "Service"))

for( thisname in names(cat_t1)) {
cat_t1[[thisname]] <- left_join(cat_t1[[thisname]], cat_t1_all[[thisname]], by=c("variable", "level"))
}

cat_t1_eff <-   intervention_only%>% filter(window_start==TRUE)  %>% summarize(across( one_of( "Race", "Sex", "FunctionalCapacity", "ASA_PS", "Service"), function(x) {bind_cols(a=x,b=isTreatment) %>% mutate(a=as.character(a)) %>% table %>% cramers_v(alternative="two.sided") %>% phi_print}    ) ) %>% t %>% as_tibble(rownames="variable") %>% set_colnames(c("variable", "eff"))

cat_t1<-lapply( cat_t1, . %>% left_join(cat_t1_eff, by="variable") %>% mutate(eff= c(eff[1], rep("", n()-1 ) ) ) )

cat_t1<-lapply( cat_t1, function(x) {bind_rows(tibble::tibble_row(variable=first(x$variable), Control="", Interv="", eff=first(x$eff) , all=""), x %>% select(variable=level, Control, Interv, all) %>% mutate( eff="") %>% mutate(variable = paste0("   ", variable) ) ) %>% select( variable, Control, Interv, eff, all ) } )



bind_rows(cont_t1 , binary_t1 ,  cat_t1 %>% bind_rows) %>% select(variable, Interv, Control, eff, all) %>% rename(Intervention=Interv, `Standardized diff`=eff) -> combined_t1

combined_t1 %<>% mutate(variable = variable %>% case_match( "CancerCurrent"~"Cancer","Dementia_MildCognitiveImpairment"~"Dementia", "CVA_Stroke"~"Stroke", "FunctionalCapacity"~"Functional Capacity", "ASA_PS"~"ASA-PS", "AFib"~"Atrial Fib" ,.default=variable  ) )

combined_t1%>% fwrite("/research/outputs/table1_by_review.csv")


##########
## the same including repeats for ct.gov
##########

binary_t1 <- table1_data %>%  mutate(across( one_of("AFib", 'Anemia' ,'Asthma','CAD','CancerCurrent','CKD','ESRD', 'COPD', 'Dementia_MildCognitiveImpairment', 'Diabetes', 'CVA_Stroke' ,'VTE', 'OSA', 'HTN', "emergency") , function(x) {if_else(is.na(x), FALSE, x )} ) ) %>% mutate(across( one_of("AFib", 'Anemia' ,'Asthma','CAD','CancerCurrent','CKD','ESRD', 'COPD', 'Dementia_MildCognitiveImpairment', 'Diabetes', 'CVA_Stroke' ,'VTE', 'OSA', 'HTN', "emergency") , function(x) {if_else(is.na(EvalDate), NA, x )} ) )  %>% group_by(isTreatment) %>% summarize(across( one_of("AFib", 'Anemia' ,'Asthma','CAD','CancerCurrent','CKD','ESRD', 'COPD', 'Dementia_MildCognitiveImpairment', 'Diabetes', 'CVA_Stroke' ,'VTE', 'OSA', 'HTN', "emergency") , function(x) { paste0(sum(x, na.rm=T), "/" , sum(!is.na(x) ) , " (",round(mean(x, na.rm=T) ,3)*100,"%)" ) }  ) ) %>% t

binary_t1_all <- table1_data %>%  mutate(across( one_of("AFib", 'Anemia' ,'Asthma','CAD','CancerCurrent','CKD','ESRD', 'COPD', 'Dementia_MildCognitiveImpairment', 'Diabetes', 'CVA_Stroke' ,'VTE', 'OSA', 'HTN', "emergency") , function(x) {if_else(is.na(x), FALSE, x )} ) ) %>% mutate(across( one_of("AFib", 'Anemia' ,'Asthma','CAD','CancerCurrent','CKD','ESRD', 'COPD', 'Dementia_MildCognitiveImpairment', 'Diabetes', 'CVA_Stroke' ,'VTE', 'OSA', 'HTN', "emergency") , function(x) {if_else(is.na(EvalDate), NA, x )} ) )  %>% summarize(across( one_of("AFib", 'Anemia' ,'Asthma','CAD','CancerCurrent','CKD','ESRD', 'COPD', 'Dementia_MildCognitiveImpairment', 'Diabetes', 'CVA_Stroke' ,'VTE', 'OSA', 'HTN', "emergency") , function(x) { paste0(sum(x, na.rm=T), "/" , sum(!is.na(x) ) , " (",round(mean(x, na.rm=T) ,3)*100,"%)" ) }  ) ) %>% t %>% as_tibble(rownames="variable") %>% set_colnames(c("variable", "all") )


phi_print <- function(x) { paste0(round(x[1,1] ,2) %>% sprintf(fmt="%0.2f"), " (", round(x[1,3] ,2)  %>% sprintf(fmt="%0.2f"), " , ", round(x[1,4] ,2) %>% pmax(.01), ")") }
binary_t1 <- binary_t1[-1,] %>% as_tibble(rownames="variable") %>% set_colnames(c("variable", "Control", "Interv"))

binary_t1_eff <- table1_data %>%  mutate(across( one_of("AFib", 'Anemia' ,'Asthma','CAD','CancerCurrent','CKD','ESRD', 'COPD', 'Dementia_MildCognitiveImpairment', 'Diabetes', 'CVA_Stroke' ,'VTE', 'OSA', 'HTN', "emergency") , function(x) {if_else(is.na(x), FALSE, x )} ) ) %>% mutate(across( one_of("AFib", 'Anemia' ,'Asthma','CAD','CancerCurrent','CKD','ESRD', 'COPD', 'Dementia_MildCognitiveImpairment', 'Diabetes', 'CVA_Stroke' ,'VTE', 'OSA', 'HTN', "emergency") , function(x) {if_else(is.na(EvalDate), NA, x )} ) ) %>% summarize(across( one_of("AFib", 'Anemia' ,'Asthma','CAD','CancerCurrent','CKD','ESRD', 'COPD', 'Dementia_MildCognitiveImpairment', 'Diabetes', 'CVA_Stroke' ,'VTE', 'OSA', 'HTN', "emergency") , function(x) {bind_cols(a=x,b=isTreatment) %>% table %>% phi(alternative="two.sided") %>% phi_print}    ) ) %>% t %>% as_tibble(rownames="variable") %>% set_colnames(c("variable", "eff"))

binary_t1<- left_join(binary_t1, binary_t1_eff, by="variable") %>% left_join( binary_t1_all, by="variable")


cont_t1 <-  table1_data   %>% group_by(isTreatment) %>% summarize(across( one_of( "Age") , function(x){ paste0(round(mean(x, na.rm=T), 1) , " (",round(sd(x, na.rm=T) ,1),")" ) }   )  ) %>% t

cont_t1_all <-  table1_data    %>%  summarize(across( one_of( "Age") , function(x){ paste0(round(mean(x, na.rm=T), 1) , " (",round(sd(x, na.rm=T) ,1),")" ) }   )  ) %>% t  %>% as_tibble(rownames="variable") %>% set_colnames(c("variable", "all") )

cont_t1 <- cont_t1[-1,,drop=F] %>% as_tibble(rownames="variable") %>% set_colnames(c("variable", "Control", "Interv"))

cont_t1_eff<-   lapply( c("Age") , function(x){ tibble(variable=x, eff=cohens_d(x,"isTreatment", data=table1_data  %>% mutate(isTreatment=factor(isTreatment))) %>% phi_print) } ) %>% bind_rows

cont_t1 %<>% left_join(cont_t1_eff, by="variable")%>% left_join( cont_t1_all, by="variable")


## doing this without an explicit loop is agony
cat_t1 <-  lapply( c("Race", "Sex", "FunctionalCapacity", "ASA_PS", "Service"), 
function(x) {
  table1_data   %>% dplyr::count(isTreatment, get(x) ) %>% 
  rename(v1 = `get(x)`) %>% 
  mutate(v1=as.character(v1)) %>% group_by(v1) %>% 
  mutate(n2 = sum(n)) %>% ungroup %>%
  mutate(v1 = if_else(is.na(v1), "Unknown or Other", v1 ) ) %>%
  mutate(v1 = if_else(v1 =='NA', "Unknown or Other", v1 ) ) %>%
  mutate(v1 = if_else(n2 < 100, "Unknown or Other", v1 ) ) %>%
  group_by(isTreatment, v1) %>% summarize(n=sum(n)) %>%
  ungroup %>% group_by(isTreatment) %>% 
  mutate(frac = n/sum(n)) %>% 
  mutate( state = paste0( n, "/", sum(n), " (" , round(frac,3)*100, "%)" )) %>% 
  pivot_wider( id_cols = "v1", values_from="state", names_from="isTreatment") %>% 
  set_colnames(c("level", "Control", "Interv" ))%>% 
  mutate(level= as.character(level))  %>% 
  mutate(variable=x) %>% select( variable, everything()) -> temp
  if("factor" %in% class(table1_data[[x]]) ) {
    temp <- temp[na.omit(match(levels(table1_data[[x]]) ,temp$level  ) ), ]
  }
  return(temp)
  
} ) %>% set_names(c("Race", "Sex", "FunctionalCapacity", "ASA_PS", "Service"))

cat_t1_all <-  lapply( c("Race", "Sex", "FunctionalCapacity", "ASA_PS", "Service"), 
function(x) {
  table1_data   %>% dplyr::count(get(x) ) %>% 
  rename(v1 = `get(x)`) %>%
  mutate(v1=as.character(v1)) %>% 
  mutate(v1 = if_else(is.na(v1), "Unknown or Other", v1 ) ) %>%
  mutate(v1 = if_else(v1 =='NA', "Unknown or Other", v1 ) ) %>%
  mutate(v1 = if_else(n < 100, "Unknown or Other", v1 ) ) %>%
  group_by(v1) %>% summarize(n=sum(n)) %>%
  ungroup %>% 
  mutate(frac = n/sum(n)) %>% 
  mutate( state = paste0( n, "/", sum(n), " (" , round(frac,3)*100, "%)" )) %>% 
  select(level=v1, all=state ) %>% 
  mutate( level= as.character(level))  %>% 
  mutate(variable=x) %>% select( variable, everything()) -> temp
  if("factor" %in% class(table1_data[[x]]) ) {
    temp <- temp[na.omit(match(levels(table1_data[[x]]) ,temp$level  ) ), ]
  }
  return(temp)
} )  %>% set_names(c("Race", "Sex", "FunctionalCapacity", "ASA_PS", "Service"))

for( thisname in names(cat_t1)) {
cat_t1[[thisname]] <- left_join(cat_t1[[thisname]], cat_t1_all[[thisname]], by=c("variable", "level"))
}

cat_t1_eff <-   table1_data  %>% summarize(across( one_of( "Race", "Sex", "FunctionalCapacity", "ASA_PS", "Service"), function(x) {bind_cols(a=x,b=isTreatment) %>% mutate(a=as.character(a)) %>% table %>% cramers_v(alternative="two.sided") %>% phi_print}    ) ) %>% t %>% as_tibble(rownames="variable") %>% set_colnames(c("variable", "eff"))

cat_t1<-lapply( cat_t1, . %>% left_join(cat_t1_eff, by="variable") %>% mutate(eff= c(eff[1], rep("", n()-1 ) ) ) )

cat_t1<-lapply( cat_t1, function(x) {bind_rows(tibble::tibble_row(variable=first(x$variable), Control="", Interv="", eff=first(x$eff) , all=""), x %>% select(variable=level, Control, Interv, all) %>% mutate( eff="") %>% mutate(variable = paste0("   ", variable) ) ) %>% select( variable, Control, Interv, eff, all ) } )



bind_rows(cont_t1 , binary_t1 ,  cat_t1 %>% bind_rows) %>% select(variable, Interv, Control, eff, all) %>% rename(Intervention=Interv, `Standardized diff`=eff) -> combined_t1

combined_t1 %<>% mutate(variable = variable %>% case_match( "CancerCurrent"~"Cancer","Dementia_MildCognitiveImpairment"~"Dementia", "CVA_Stroke"~"Stroke", "FunctionalCapacity"~"Functional Capacity", "ASA_PS"~"ASA-PS", "AFib"~"Atrial Fib" ,.default=variable  ) )

combined_t1%>% fwrite("/research/outputs/table1_all.csv")




##############
## merge the outcome ascertainment pictures
##############

picture_files <- c(
resp_failure = "/research/outputs/RespF_presense.csv" ,
delirium = "/research/outputs/CAM_presense.csv" ,
temperature ="/research/outputs/temperature_presense.csv" ,
hypotension = "/research/outputs/bp_presense.csv" ,
gas_flow = "/research/outputs/fgf_presense.csv" , 
pip = "/research/outputs/vent_presense.csv" , 
anesthetic = "/research/outputs/agent_presense.csv" , 
length_of_stay_and_ICU = "/research/outputs/LoS_presense.csv" , 
creatinine_and_glucose = "/research/outputs/labs_presense.csv" , 
antibiotics = "/research/outputs/antibiotic_redose_presense.csv" 
) 

ascertainment_pictures <- vector('list' , length(picture_files))
names(ascertainment_pictures) <- names(picture_files)

for( thisd in names(picture_files)) {
  temp <- fread(picture_files[thisd])

  temp[,isTreatment:=factor(isTreatment) ]
  setnames(temp, colnames(temp) %>% grep(pattern="fractionW", value=T) %>% first , "fraction")
  temp %>%  ggplot( aes(x=roughdate, y=fraction,  group = isTreatment, fill =isTreatment)) + geom_bar(stat = "identity", width=0.05, position=position_identity(), alpha=0.5) + labs(x = "Date", y = "Fraction", title = paste0(thisd, " measured by month") ) -> temp2
  ascertainment_pictures[[thisd]] <- temp2
}


## this is necessary because otherwise it error about lack of access to x11
pdf("/research/intermediates/temp.pdf")
ml <- marrangeGrob(ascertainment_pictures[c("creatinine_and_glucose","resp_failure","delirium")] , nrow=2, ncol=2)

ggsave("/research/outputs/primary_asc.pdf", ml)
ggsave("/research/outputs/primary_asc.png", ml)

ml <- marrangeGrob(ascertainment_pictures[c("temperature","hypotension","gas_flow","pip","anesthetic","length_of_stay_and_ICU","creatinine_and_glucose","antibiotics" )] , nrow=4, ncol=2)

ggsave("/research/outputs/secondary_asc.pdf", ml)
ggsave("/research/outputs/secondary_asc.png", ml)
dev.off()


