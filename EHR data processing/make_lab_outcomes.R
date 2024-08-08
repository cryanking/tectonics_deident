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
"stringr" ) , '2024-05-01')



roughen_date <- function(x) round(year(x) + (month(x)-1)/12,2)  


setwd("/research")
clarity_root <- '/early_act3/'


## Population included
join_mrns <- fread("/research/intermediates/population.csv")
cases_by_month_summary <-  copy(join_mrns )[, roughdate := Date %>% roughen_date ][, .(UniqueCase=uniqueN(SurgicalRecordID) ), by=.(roughdate, isTreatment) ][roughdate>2019.25] [order(roughdate)]

###########################
## TODO: revisit with updated data
## merge lab outcomes (first glucose, quantitative cr changes)
###########################

glucose_names <- c( #"GLUCOSE" ,
 "GLUCOSE, SERUM" ,                      
 "SCRIBED GLUCOSE" ,                     
 "GLUCOSE, WHOLE BLOOD" ,                
 "GLUCOSE (BLOOD GAS)" ,                 
 "GLUCOSE OPOC" ,                        
 "GLUCOSE, FASTING",
 "GLUCOSE BY MONITOR DEVICE POCBJH" ,
 "GLUCOSE, POC"                    , 
 "GLUCOSE POC GEM 3000"           , 
 "GLUCOSE POC I-STAT"              , 
 "GLUCOSE BLOOD, POC"             ,  
 "GLUCOSE POINT OF CARE  PHC LS" ,  
 "GLUC FASTING OPOC"            ,    
 "GLUCOSE POINT OF CARE SLCOP" ,     
 "GLUCOSE POINT OF CARE BT LS",     
                 "POC GLUCOSE" )

cr_names <- c( "SCRIBED CREATININE", "CREATININE (MG/DL) IN SER/PLAS" ,  "POC CREATININE" , "CREATININE POC" , "CREATININE, WHOLE BLOOD" , "CREATININE, POC")


## this is the "early" epic era, mostly through jan 2021 with some feb/march 2021 postop labs
if(FALSE) {
epic_labs <- fread("/research/intermediates/ACT3lab_imports.csv")
} else {
all_labs <- list.files(clarity_root , full.names=TRUE, pattern="Clarity\\sHogue\\sResult\\sSet\\sLabs\\sand\\sMicrobiology\\s\\d")
all_labs %<>% grep(pattern="csv$", value=TRUE)
all_labs %<>% c(paste0(clarity_root , 'Clarity Hogue Result Set Labs POC.csv'))
epic_labs <-  rbindlist(lapply(all_labs, function(x) fread(x)[ LAB_TEST %like% "CREATININE" | LAB_TEST %like% "GLUCOSE" ][CurrentMRN %in% join_mrns$MRN]  ))
# 1344215
epic_labs <- epic_labs[grepl(LAB_RSLT_VALUE, pattern="\\w", fixed=FALSE)]
epic_labs <- epic_labs[!grepl(LAB_PROCEDURE , pattern="CRITICAL RESULT", fixed=TRUE)]
epic_labs <- epic_labs[!grepl(LAB_PROCEDURE , pattern="COPY(IES) SENT TO:", fixed=TRUE)]
epic_labs <- epic_labs[!grepl(LAB_TEST , pattern="URINE") ]
epic_labs <- epic_labs[!grepl(LAB_TEST , pattern="UR$") ]
epic_labs <- epic_labs[!grepl(LAB_TEST , pattern="RATIO", fixed=TRUE) ]
epic_labs <- epic_labs[ LAB_TEST != "ESTIMATED AVERAGE GLUCOSE" ]
epic_labs <- epic_labs[!grepl(LAB_TEST ,pattern="COMMENT")]


epic_labs [ LAB_TEST %chin% glucose_names, LAB_TEST := "GLUCOSE"]
epic_labs <- epic_labs [ ! ((LAB_TEST == "GLUCOSE") & (substr(LAB_RSLT_VALUE,start=1,stop=1) %chin% c("+","0", "N", "T") ) )]
  
epic_labs [ LAB_TEST %chin% cr_names, LAB_TEST := "CREATININE"]
epic_labs <- epic_labs[ LAB_TEST %chin% c("GLUCOSE", "CREATININE")] 
epic_labs <- epic_labs [ !is.na(LAB_RSLT_DTTM) ]
epic_labs[ substr(LAB_RSLT_VALUE, start=1, stop=1) %chin% c("<", ">")  & is.finite(as.numeric(substr(LAB_RSLT_VALUE, start=2, stop=10) ) ) , LAB_RSLT_VALUE := substr(LAB_RSLT_VALUE, start=2, stop=10)] ## this handles some "">###" reports for glucose
epic_labs [ ,LAB_RSLT_VALUE := as.numeric(LAB_RSLT_VALUE) ]
epic_labs <- epic_labs[ is.finite(LAB_RSLT_VALUE) ]
epic_labs <- epic_labs[, .(CurrentMRN, LAB_RSLT_DTTM, LAB_TEST, LAB_RSLT_VALUE, LAB_RSLT_UOM)]
fwrite( epic_labs , "/research/ACT3lab_imports.csv") 
}


epic_labs <- join_mrns[epic_labs, on="MRN==CurrentMRN", allow.cartesian=T]


tect_labs <- list.files("/research/2023_07_TECTONICS_PaperData/2023_11_Labs_Correct/" , full.names=TRUE, pattern="csv$")

tect_labs <- lapply(tect_labs, .%>% fread(sep="^", quote="") ) %>% rbindlist
setnames(tect_labs, old=c("PAT_MRN_ID") , new="CurrentMRN" )
setnames(tect_labs, old=c("OrderDescription") , new="LAB_PROCEDURE" )
setnames(tect_labs, old=c("LabName") , new="LAB_TEST" )
setnames(tect_labs, old=c("Result_Time") , new="LAB_RSLT_DTTM" )
setnames(tect_labs, old=c("LabResult") , new="LAB_RSLT_VALUE" )
setnames(tect_labs, old=c("Reference_unit") , new="LAB_RSLT_UOM" )

tect_labs <- tect_labs[CurrentMRN %in% join_mrns$MRN] 
tect_labs <- tect_labs[ LAB_TEST %like% "CREATININE" | LAB_TEST %like% "GLUCOSE" ]
tect_labs <- tect_labs[grepl(LAB_RSLT_VALUE, pattern="\\w", fixed=FALSE)]
tect_labs <- tect_labs[!grepl(LAB_PROCEDURE , pattern="CRITICAL RESULT", fixed=TRUE)]
tect_labs <- tect_labs[!grepl(LAB_PROCEDURE , pattern="COPY(IES) SENT TO:", fixed=TRUE)]
tect_labs <- tect_labs[!grepl(LAB_TEST , pattern="URINE") ]
tect_labs <- tect_labs[!grepl(LAB_TEST , pattern="UR$") ]
tect_labs <- tect_labs[!grepl(LAB_TEST , pattern="RATIO", fixed=TRUE) ]
tect_labs <- tect_labs[ LAB_TEST != "ESTIMATED AVERAGE GLUCOSE" ]
tect_labs <- tect_labs[!grepl(LAB_TEST ,pattern="COMMENT")]

tect_labs [ LAB_TEST %chin% glucose_names, LAB_TEST := "GLUCOSE"]
tect_labs <- tect_labs [ ! ((LAB_TEST == "GLUCOSE") & (substr(LAB_RSLT_VALUE,start=1,stop=1) %chin% c("+","0", "N", "T") ) )]
  
tect_labs [ LAB_TEST %chin% cr_names, LAB_TEST := "CREATININE"]
tect_labs <- tect_labs[ LAB_TEST %chin% c("GLUCOSE", "CREATININE")] 
tect_labs <- tect_labs [ !is.na(LAB_RSLT_DTTM) ]
tect_labs[ substr(LAB_RSLT_VALUE, start=1, stop=1) %chin% c("<", ">")  & is.finite(as.numeric(substr(LAB_RSLT_VALUE, start=2, stop=10) ) ) , LAB_RSLT_VALUE := substr(LAB_RSLT_VALUE, start=2, stop=10)] ## this handles some "">###" reports for glucose
tect_labs [ ,LAB_RSLT_VALUE := as.numeric(LAB_RSLT_VALUE) ]
tect_labs <- tect_labs[ is.finite(LAB_RSLT_VALUE) ]
tect_labs <- tect_labs[, .(CurrentMRN, LAB_RSLT_DTTM, LAB_TEST, LAB_RSLT_VALUE, LAB_RSLT_UOM)]

tect_labs<- join_mrns[tect_labs, on="MRN==CurrentMRN", allow.cartesian=T]


all_labs <- rbind(tect_labs, epic_labs )


## last preop creatinine
last_preop_cr<- setorder(all_labs[ LAB_TEST == "CREATININE" ] [ AN_START_DATETIME > (LAB_RSLT_DTTM) ]  , SurgicalRecordID, -LAB_RSLT_DTTM) 
last_preop_cr<-  last_preop_cr[last_preop_cr[ , .I[1] , by="SurgicalRecordID"]$V1 , .(SurgicalRecordID, preop_cr=LAB_RSLT_VALUE)] 

## max postop cr
postop_cr_max7 <- all_labs[ LAB_TEST == "CREATININE" ] [ AN_START_DATETIME < (LAB_RSLT_DTTM) ][AN_START_DATETIME > (LAB_RSLT_DTTM) -ddays(7) ][ , .(postop_cr_max7=max(LAB_RSLT_VALUE, na.rm=TRUE)) , by ="SurgicalRecordID"]  

postop_cr_max2 <- all_labs[ LAB_TEST == "CREATININE" ] [ AN_START_DATETIME < (LAB_RSLT_DTTM) ][AN_START_DATETIME > (LAB_RSLT_DTTM) -ddays(2) ][ , .(postop_cr_max2=max(LAB_RSLT_VALUE, na.rm=TRUE)) , by ="SurgicalRecordID"]  

## first postop glucose
first_post_glucose <-  setorder(all_labs[ LAB_TEST == "GLUCOSE"] [ AN_STOP_DATETIME < (LAB_RSLT_DTTM) ] , SurgicalRecordID, LAB_RSLT_DTTM) 
first_post_glucose <- first_post_glucose[first_post_glucose[ , .I[1] , by="SurgicalRecordID"]$V1 , .(SurgicalRecordID, first_glucose=LAB_RSLT_VALUE)]  

labs_summary <- first_post_glucose %>% 
  merge( postop_cr_max2, all=TRUE, by="SurgicalRecordID") %>% 
  merge( last_preop_cr, all=TRUE, by="SurgicalRecordID") %>% 
  merge( postop_cr_max7, all=TRUE, by="SurgicalRecordID") 



temp <- labs_summary[join_mrns, on="SurgicalRecordID", nomatch=NA] [, roughdate := AN_START_DATETIME %>% roughen_date ][, .(UniqueCase=uniqueN(SurgicalRecordID), CaseWGlu=sum( is.finite(first_glucose) ), caseWCr = sum( is.finite(postop_cr_max2) ) ), by=.(roughdate, isTreatment) ][roughdate>2019.25][order(roughdate,isTreatment)][ , fractionWGlu := round( CaseWGlu/UniqueCase,2) ][, fractionWCr:= round( caseWCr/UniqueCase,2) ] 

temp  %>%  fwrite("/research/outputs/labs_presense.csv")

copy(temp)[,isTreatment:=factor(isTreatment) ] %>% ggplot( aes(x=roughdate, y=fractionWGlu,  group = isTreatment, fill =isTreatment)) + geom_bar(stat = "identity", width=0.05, position=position_identity(), alpha=0.5) + labs(x = "Date", y = "Fraction", title = "Postop Glu by month") -> temp2
temp2 %>% ggsave(device="png", filename="/research/outputs/glucose_present.png")

copy(temp)[,isTreatment:=factor(isTreatment) ] %>% ggplot( aes(x=roughdate, y=fractionWCr,  group = isTreatment, fill =isTreatment)) + geom_bar(stat = "identity", width=0.05, position=position_identity(), alpha=0.5) + labs(x = "Date", y = "Fraction", title = "Postop Cr by month") -> temp2
temp2 %>% ggsave(device="png", filename="/research/outputs/cr_present.png")




na_false <- function(x) {fifelse(is.na(x), FALSE, x ) }

table1_data <- fread("/research/intermediates/individual_t1_data.csv")

###########################
## demographic based cr imputation
###########################

table1_data %<>% merge( labs_summary, all.x=TRUE, by="SurgicalRecordID"  )
table1_data [, exclude_aki := na_false(ESRD | (preop_cr > 4)  ) ]

imp_data <- table1_data %>% select(SurgicalRecordID, CKD, Race, Age, Sex, HEIGHT_IN_INCHES, WEIGHT_IN_KG, preop_cr) %>% data.table %>% unique 

imp_data[ is.na(CKD), CKD := FALSE]  
imp_data[ , Race:= fct_lump_n(Race, n=3)]
imp_data <- imp_data[is.finite(preop_cr) ]

xgdata <- imp_data[,.(Age, WEIGHT_IN_KG, HEIGHT_IN_INCHES, CKD)] %>% as.matrix
labels1 <- imp_data$preop_cr
labels1[labels1 < 0.4] <- 0.4
labels1[labels1 > 6.] <- 6.

encode_onehot <- function(x, colname_prefix = "", colname_suffix = "") {
  if (!is.factor(x)) {
      x <- as.factor(x)
  }
  encoding_matrix <- contrasts(x, contrasts = FALSE)
  encoded_data <- encoding_matrix[as.integer(x),]
  colnames(encoded_data) <- paste0(colname_prefix, colnames(encoded_data), colname_suffix)
  encoded_data
}

xgdata <- cbind(xgdata, encode_onehot(imp_data$Sex, colname_prefix = "Sex"), encode_onehot(imp_data$Race, colname_prefix = "RACE") )

xgdata <- xgb.DMatrix(data=xgdata, info = list(label=labels1))
local_fit <- xgb.train(data=xgdata, tree_method ="approx" , nrounds=7L, params=list(eval_metric="rmse" , max_depth=6L )  ) 

## fill in patients without measurements

xgdata <- table1_data %>% select(Age, WEIGHT_IN_KG, HEIGHT_IN_INCHES, CKD) %>% mutate(CKD = !is.na(CKD) ) %>% as.matrix
xgdata <- cbind(xgdata, encode_onehot(table1_data$Sex, colname_prefix = "Sex"), encode_onehot(fct_lump_n(table1_data$Race, n=3), colname_prefix = "RACE") )
xgdata <- xgb.DMatrix(data=xgdata)
table1_data[, imputed_cr :=   predict(local_fit, newdata=xgdata ) ]
table1_data[, imputed_cr := case_when( imputed_cr < 0.4 ~ 0.4, imputed_cr > 5. ~ 5., TRUE~imputed_cr) ]

table1_data[, combined_baseline_cr := if_else(is.na(preop_cr),imputed_cr,  preop_cr) ]
table1_data[, ratio_cre := postop_cr_max7 / combined_baseline_cr ]


table1_data[, aki_grade := case_when(
    combined_baseline_cr > 4 ~ NA_integer_ ,
    exclude_aki == TRUE ~ NA_integer_ ,
    postop_cr_max7 > 4 ~ 3L ,
    ratio_cre > 3 ~3L ,
    ratio_cre > 2 ~2L ,
    ratio_cre > 1.5 ~1L ,
    coalesce(postop_cr_max2 - preop_cr, 0.) > 0.3 ~ 1L ,
    is.na(postop_cr_max7) ~ -1L ,
    TRUE ~ 0L
  ) ]


table1_data[, .(SurgicalRecordID, preop_cr,imputed_cr, combined_baseline_cr, exclude_aki,ratio_cre, aki_grade, postop_cr_max2, postop_cr_max7, first_glucose)]  %>% fwrite("/research/intermediates/lab_outcomes.csv")
  
  ## Note the encoding here: NA is not eligible, -1 is missing (presumed negative)  
