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
"stringr" , 
"effectsize" ) , '2024-05-01')



roughen_date <- function(x) round(year(x) + (month(x)-1)/12,2)  

setwd("/research")
clarity_root <- '/early_act3/'
join_mrns <- fread("/research/intermediates/population.csv")

main_pop <- read_xlsx("Template4TECTONICSData_Req21Mar2023.xlsx", sheet="Population_v2") %>% unique
main_pop  %<>% filter( (SurgicalRecordID %in% join_mrns$SurgicalRecordID)) %>% filter(weekdays(Date) %in% c('Friday'  ,  'Monday' , 'Thursday' ,  'Tuesday' , 'Wednesday')   )
main_pop %<>% filter(LastAction  %>% local_time %>% as.numeric %>% divide_by(60*60) %>% is_greater_than( 10) )
main_pop %<>% filter(FirstAction  %>% local_time %>% as.numeric %>% divide_by(60*60)  %>% is_less_than( 12)  )
main_pop %<>% mutate(active_time = difftime(LastAction,FirstAction, units="hours") %>% as.numeric ) %>% filter(active_time>2)


main_pop %<>% as.data.table
## the 2 duplicates by SurgicalRecordID are MRN merges where the lower number is prefered
setorder(main_pop, SurgicalRecordID, MRN )
main_pop %<>% unique(by="SurgicalRecordID")

###########################
## DONE
## load and merge demographics
###########################
demo <- fread("2023_07_TECTONICS_PaperData/2023_07_17_Clarity_Result_Set_Demographics.csv", sep="^", quote="") ## MRN, sex, ETHNICITY GENDER RACE, height, weight, TOBACCO_USE
new_demo <- demo[, .(CurrentMRN, WEIGHT_IN_KG, HEIGHT_IN_INCHES, TOBACCO_USE)][ as.data.table(main_pop %>% select(CurrentMRN=MRN,SurgicalRecordID) ), nomatch=NULL, allow.cartesian=FALSE, on="CurrentMRN"]

early_act_demo <- fread('/early_act3/Clarity Hogue Result Set Demographics.csv')[, .(CurrentMRN, WEIGHT_IN_KG, HEIGHT_IN_INCHES, TOBACCO_USE)][ as.data.table(main_pop %>% select(CurrentMRN=MRN,SurgicalRecordID) ), nomatch=NULL, allow.cartesian=FALSE, on="CurrentMRN"]
late_act_demo <- fread( '/mark2_act3/easy_hard_version/easydemo.csv')[, .(CurrentMRN, WEIGHT_IN_KG, HEIGHT_IN_INCHES, TOBACCO_USE)][ as.data.table(main_pop %>% select(CurrentMRN=MRN,SurgicalRecordID) ), nomatch=NULL, allow.cartesian=FALSE, on="CurrentMRN"]

gap_demo <-  fread("2023_07_TECTONICS_PaperData/2023_11_02_Clarity_Result_Set_Demographics.csv", sep="^", quote="")[, .(CurrentMRN, WEIGHT_IN_KG, HEIGHT_IN_INCHES, TOBACCO_USE)] [ as.data.table(main_pop %>% select(CurrentMRN=MRN,SurgicalRecordID) ), nomatch=NULL, allow.cartesian=FALSE, on="CurrentMRN"]

all_demo <- rbind(new_demo, early_act_demo, late_act_demo, gap_demo )[,CurrentMRN := NULL] %>% unique(by="SurgicalRecordID")

all_demo <- all_demo [ as.data.table(main_pop %>% select(CurrentMRN=MRN,SurgicalRecordID, Date) ), nomatch=NA, on="SurgicalRecordID"]
all_demo <- unique(all_demo, by="SurgicalRecordID")
# copy(all_demo )[, roughdate := Date %>% roughen_date ][, .(UniqueCase=uniqueN(SurgicalRecordID), CaseWDemo=sum( is.finite(WEIGHT_IN_KG) | is.finite(HEIGHT_IN_INCHES) ) ), by=roughdate ][roughdate>2019.25][, fractWDemo := round(CaseWDemo/UniqueCase,2)] [order(roughdate)]


###########################
## Transform PMH lists for diabetes, ESRD, CKD
###########################
source("/code/translate_icd.R")

epic_pmh<- fread(paste0(clarity_root , 'Clarity Hogue Result Set Medical History.csv') )
epic_pmh[ , ICD_10_CODES := sub(x= ICD_10_CODES, pattern='.', replacement="", fixed=T)]

epic_pmh <- join_mrns[epic_pmh, on="MRN==CurrentMRN", allow.cartesian=TRUE ]
epic_pmh <- epic_pmh[RECORDED_DATE < AN_STOP_DATETIME + ddays(2) ]

set_all_codes( epic_pmh, code_sets) 
epic_pmh <- epic_pmh[any_code == TRUE  ]
epic_pmh <- epic_pmh[ , lapply(.SD, any), by="SurgicalRecordID", .SDcols=names(code_sets) ]

mk2_pmh <- fread("/mark2_act3/easy_hard_version/easymedical_history.csv")
mk2_pmh[ , ICD_10_CODES := sub(x= ICD_10_CODES, pattern='.', replacement="", fixed=T)]

mk2_pmh <- join_mrns[mk2_pmh, on="MRN==CurrentMRN", allow.cartesian=TRUE ]
mk2_pmh <- mk2_pmh[RECORDED_DATE < AN_STOP_DATETIME + ddays(2) ]

set_all_codes( mk2_pmh, code_sets) 
mk2_pmh <- mk2_pmh[any_code == TRUE  ]
mk2_pmh <- mk2_pmh[ , lapply(.SD, any), by="SurgicalRecordID", .SDcols=names(code_sets) ]

## I think this file covers the whole period; there are only a handfull of records not included
tect_pmh <- fread("2023_07_TECTONICS_PaperData/2023_07_17_Clarity_Result_Set_Medical_History.csv", sep="^", quote="") ## MRN, first and plast data, ICD10 (comma sep) + text

tect_pmh[ , ICD_10_CODES := sub(x= ICD_10_CODES, pattern='.', replacement="", fixed=T)]

tect_pmh <- join_mrns[tect_pmh, on="MRN==CurrentMRN", allow.cartesian=TRUE ]
tect_pmh <- tect_pmh[First_RECORDED_DATE < AN_STOP_DATETIME + ddays(2) ]

set_all_codes( tect_pmh, code_sets) 
tect_pmh <- tect_pmh[any_code == TRUE  ]
tect_pmh <- tect_pmh[ , lapply(.SD, any), by="SurgicalRecordID", .SDcols=names(code_sets) ]


# setdiff(epic_pmh$SurgicalRecordID, tect_pmh$SurgicalRecordID)
# setdiff(mk2_pmh$SurgicalRecordID, tect_pmh$SurgicalRecordID)

tect_pmh <- rbind( tect_pmh, epic_pmh[!tect_pmh, on="SurgicalRecordID"] , mk2_pmh[!tect_pmh, on="SurgicalRecordID"]  )
tect_pmh <- tect_pmh %>% unique(on="SurgicalRecordID")

tect_pmh[, Diabetes := Diabetes | DM_Type1 ]
tect_pmh[ ,DM_Type1 := NULL ]

###########################
## DONE
## load outcomes directly computed by AK
###########################


## force some columns to datetime
nms <- names(read_xlsx("Template4TECTONICSData_Req21Mar2023 dates_fixed.xlsx", sheet="Comorbidities_v2", n_max = 0))
ct <- if_else(grepl("Date", nms) | grepl("Instant", nms) , "date", "guess")
main_comorb <- read_xlsx("Template4TECTONICSData_Req21Mar2023 dates_fixed.xlsx", sheet="Comorbidities_v2", guess_max=15000, col_types = ct) ## MRN, demographics, date, some pap comorbid
additional_comorbidities <- read_xlsx('MissingComorbidities.xlsx') ## MRN, date, more cormorbidities
additional_comorbidities %<>% rename(FormattedServiceDate = `Service Date Value` )


###########################
## DONE
## Table 1 - this data is also needed for imputation in Cr change outcome
## DONE: replace slice (most recent eval) with any for comorbidities
###########################

# setdiff(main_comorb %>% colnames , additional_comorbidities %>% colnames)
# setdiff(additional_comorbidities %>% colnames ,  main_comorb %>% colnames)

## TODO: re-writing to do the slice before join, so that patients without a pre-e are included
na_false <- function(x) {fifelse(is.na(x), FALSE, x ) }

bind_rows(main_comorb ,additional_comorbidities ) %>% unique  %>% 
  rename(Race =`Patient First Race`, Ethnicity=`Patient Ethnicity`, CurrentMRN=`Patient Primary MRN (Note)`, EvalDate=FormattedServiceDate) %>%  
  mutate(ESRD = ESRD | (Dialysis %in% c("Ongoing Hemodialysis" , "Ongoing Peritoneal Dialysis") ) ) %>%
  select( CurrentMRN,EvalDate,  Race,  Ethnicity, ASA_PS=ASA, AFib = AFIB, Anemia,  Asthma, CAD, CancerCurrent, CKD, ESRD, COPD, CVA_Stroke, Dementia_MildCognitiveImpairment, Diabetes=DM, VTE=DVT_PE, OSA, HTN, PlannedAnesthesia , FunctionalCapacity) %>% 
  mutate(Race = if_else(Race %in% c('Declined', 'Other', 'Unknown', 'Unable to Answer', ""), 'Unknown or Other', Race )) %>%
  mutate(Ethnicity = if_else(! (Ethnicity %in% c('Hispanic', 'Non-Hispanic') ), Ethnicity, 'Unknown or Other'))  %>%
#  mutate(SurgDate=mdy(SurgDate)) %>%
  mutate(  across( one_of("AFib", 'Anemia' ,'Asthma','CAD','CancerCurrent','CKD','ESRD', 'COPD', 'Dementia_MildCognitiveImpairment', 'Diabetes', 'CVA_Stroke' ,'VTE', 'OSA', 'HTN'), function(x) if_else( x == 1, TRUE, FALSE) ) ) %>%
  mutate( PlannedAnesthesia = grepl(PlannedAnesthesia, pattern="general", ignore.case=T)) %>%
  mutate( FunctionalCapacity = case_when(
   FunctionalCapacity %>% grepl(pattern="<4")~0L , 
   FunctionalCapacity %>% grepl(pattern="cannot")~0L , 
   FunctionalCapacity %>% grepl(pattern="ambul")~0L , 
   FunctionalCapacity %>% grepl(pattern="4-6")~1L , 
   FunctionalCapacity %>% grepl(pattern="6-10")~2L , 
   FunctionalCapacity %>% grepl(pattern=">10")~3L , 
   TRUE~NA_integer_ 
  ) ) %>%  unique  %>%
  inner_join( main_pop %>% select(CurrentMRN = MRN, SurgicalRecordID, SurgDate=Date), by=c('CurrentMRN' ) ) %>% 
  mutate( days_before = difftime(SurgDate+ddays(1), EvalDate, units="days" ) %>% as.numeric %>% (function(x) {abs(x) + (x<0)*abs(x)*40}  ) ) %>% ## want to take the last before surgery for each, where such exists, with an allowance that preops are often filled in later the same day and that one immediately after should be considered before one that is very old
  filter(days_before < 90) %>%
  group_by(SurgicalRecordID) %>%  arrange(days_before) %>%
  mutate( across(one_of("AFib", 'Anemia' ,'Asthma','CAD','CancerCurrent','CKD','ESRD', 'COPD', 'Dementia_MildCognitiveImpairment', 'Diabetes', 'CVA_Stroke' ,'VTE', 'OSA', 'HTN') , . %>% any %>% na_false) ) %>%
  mutate( across(one_of('FunctionalCapacity', 'PlannedAnesthesia', 'ASA_PS', 'Race' , 'Ethnicity') , . %>% na.omit %>% (dplyr::first ) ) ) %>%
  slice_head(n=1) %>% ungroup %>% 
  select(-one_of("CurrentMRN", "SurgDate")) %>% 
  right_join( main_pop %>% unique  %>% mutate(Age=floor(as.numeric(difftime(Date, DoB, units="days"))/365.25) ) %>% select(CurrentMRN = MRN, SurgicalRecordID, SurgDate=Date ,OR_Abbrev , PtClass, Age, Sex), by=c('SurgicalRecordID' ) )  %>%
  left_join( all_demo %>% select(SurgicalRecordID, WEIGHT_IN_KG, HEIGHT_IN_INCHES, TOBACCO_USE)  , by="SurgicalRecordID") %>% 
  mutate( BMI=WEIGHT_IN_KG / (HEIGHT_IN_INCHES*0.0254)^2 ) %>% 
  left_join(join_mrns[, .( SurgicalRecordID, Service=SERVICE_collapsed, isTreatment, emergency)], by="SurgicalRecordID" ) -> table1_data  
 
 ## not all my containser have the new verion with "by" as an argument for slice

table1_data %<>% merge( tect_pmh, by="SurgicalRecordID", suffixes = c("", ".y"), all.x=TRUE, all.y=FALSE)
table1_data %<>% mutate( Diabetes = na_false(Diabetes | Diabetes.y) , ESRD = na_false(ESRD | ESRD.y), CKD = na_false(CKD| CKD.y) ) %>% select( -ends_with(".y") )
setDT(table1_data) 
table1_data[Race == "", Race:="Unknown or Other" ]

table1_data %>% fwrite("/research/intermediates/individual_t1_data.csv")



