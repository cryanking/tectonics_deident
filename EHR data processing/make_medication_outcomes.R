.libPaths( c( "/root/R/x86_64-pc-linux-gnu-library/4.4" , .libPaths() ) )
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
# library(matrixStats)

overlaps <- function(x, y) { c(
length(setdiff(x,y)) ,
length(intersect(x,y)) ,
length(setdiff(y,x)) 
)}

roughen_date <- function(x) round(year(x) + (month(x)-1)/12,2)  

setwd("/research")
clarity_root <- '/early_act3/'

## Population included
join_mrns <- fread("/research/intermediates/population.csv")

cases_by_month_summary <-  copy(join_mrns )[, roughdate := Date %>% roughen_date ][, .(UniqueCase=uniqueN(SurgicalRecordID) ), by=.(roughdate, isTreatment) ][roughdate>2019.25] [order(roughdate)]

###########################
## DONE
## load and merge demographics
###########################
demo <- fread("2023_07_TECTONICS_PaperData/2023_07_17_Clarity_Result_Set_Demographics.csv", sep="^", quote="") ## MRN, sex, ETHNICITY GENDER RACE, height, weight, TOBACCO_USE
new_demo <- demo[, .(CurrentMRN, WEIGHT_IN_KG, HEIGHT_IN_INCHES, TOBACCO_USE)][ as.data.table(join_mrns %>% select(CurrentMRN=MRN,SurgicalRecordID) ), nomatch=NULL, allow.cartesian=TRUE, on="CurrentMRN"]

early_act_demo <- fread('/early_act3/Clarity Hogue Result Set Demographics.csv')[, .(CurrentMRN, WEIGHT_IN_KG, HEIGHT_IN_INCHES, TOBACCO_USE)][ as.data.table(join_mrns %>% select(CurrentMRN=MRN,SurgicalRecordID) ), nomatch=NULL, allow.cartesian=TRUE, on="CurrentMRN"]
late_act_demo <- fread( '/mark2_act3/easy_hard_version/easydemo.csv')[, .(CurrentMRN, WEIGHT_IN_KG, HEIGHT_IN_INCHES, TOBACCO_USE)][ as.data.table(join_mrns %>% select(CurrentMRN=MRN,SurgicalRecordID) ), nomatch=NULL, allow.cartesian=TRUE, on="CurrentMRN"]

gap_demo <-  fread("2023_07_TECTONICS_PaperData/2023_11_02_Clarity_Result_Set_Demographics.csv", sep="^", quote="")[, .(CurrentMRN, WEIGHT_IN_KG, HEIGHT_IN_INCHES, TOBACCO_USE)] [ as.data.table(join_mrns %>% select(CurrentMRN=MRN,SurgicalRecordID) ), nomatch=NULL, allow.cartesian=TRUE, on="CurrentMRN"]

all_demo <- rbind(new_demo, early_act_demo, late_act_demo ,gap_demo)[,CurrentMRN := NULL] %>% unique(by="SurgicalRecordID")

all_demo <- all_demo [ as.data.table(join_mrns %>% select(CurrentMRN=MRN,SurgicalRecordID, Date) ), nomatch=NA, on="SurgicalRecordID"]
all_demo <- unique(all_demo, by="SurgicalRecordID")
# copy(all_demo )[, roughdate := Date %>% roughen_date ][, .(UniqueCase=uniqueN(SurgicalRecordID), CaseWDemo=sum( is.finite(WEIGHT_IN_KG) | is.finite(HEIGHT_IN_INCHES) ) ), by=roughdate ][roughdate>2019.25][, fractWDemo := round(CaseWDemo/UniqueCase,2)] [order(roughdate)]


###########################
## DONE
## merge antibiotic redosing, IV sedatives for exclusion in low MAC outcome
###########################
## skip this block of data merging unless it has updated
## NOTE: the "given" filter is fine for abx, not for infusions though where "Rate Change" "Stopped" matter
if(TRUE) {
## NOTE: infusions have to be split off separately
##  2022.5 2022.58 2022.67 2022.75 2022.83 2022.92    2023 
##   16605   29765   34849   26986   35525   26282   30563 
med_admin <- fread("2023_07_TECTONICS_PaperData/2023_08_1_Clarity_Result_Set_Medication_Administration.csv", sep="^", quote="")

tect_inta_meds <- copy(join_mrns)[,MRN:=as.character(MRN) ][med_admin[MAR_Action=="Given", .(Epic_MRN, Medication_Name, TAKEN_TIME, Dose_Value, Dose_UoM)], on="MRN==Epic_MRN", allow.cartesian=TRUE, nomatch=NULL][TAKEN_TIME > AN_START_DATETIME & TAKEN_TIME < AN_STOP_DATETIME]

tect_intra_infusions <- copy(join_mrns)[,MRN:=as.character(MRN) ][med_admin[INFUSION_RATE != "NULL",  .(Epic_MRN, Medication_Name, TAKEN_TIME, Dose_Value, Dose_UoM)], on="MRN==Epic_MRN", allow.cartesian=TRUE, nomatch=NULL][TAKEN_TIME > AN_START_DATETIME & TAKEN_TIME < AN_STOP_DATETIME][is.finite(as.numeric(Dose_Value))]

## this may be a proper subset of the above
## 2022.5 2022.58 2022.67 
##  16596   29740    1797 
# > overlaps( tect_inta_meds_gap$SurgicalRecordID ,tect_inta_meds$SurgicalRecordID )
# [1]    0 2925 8946

# med_admin_gap <- fread("2023_07_TECTONICS_PaperData/2023_11_02_Clarity_Result_Set_Medication_Administration.csv", sep=",")
# med_admin_gap[,TAKEN_TIME := TAKEN_TIME %>% mdy_hm ]
# tect_inta_meds_gap <- copy(join_mrns)[,MRN:=as.character(MRN) ][med_admin_gap[MAR_Action=="Given", .(Epic_MRN, Medication_Name, TAKEN_TIME, Dose_Value, Dose_UoM)], on="MRN==Epic_MRN", allow.cartesian=TRUE, nomatch=NULL][TAKEN_TIME > AN_START_DATETIME & TAKEN_TIME < AN_STOP_DATETIME]
# 
# tect_intra_infusions_gap <- copy(join_mrns)[,MRN:=as.character(MRN) ][med_admin_gap[INFUSION_RATE != "NULL",  .(Epic_MRN, Medication_Name, TAKEN_TIME, Dose_Value, Dose_UoM)], on="MRN==Epic_MRN", allow.cartesian=TRUE, nomatch=NULL][TAKEN_TIME > AN_START_DATETIME & TAKEN_TIME < AN_STOP_DATETIME][is.finite(as.numeric(Dose_Value))]

##    2022 2022.08 2022.17 2022.25 2022.33 2022.42 
##   28524   29998   37087   31267   24478    1366 
med_admin_gap <- fread("2023_07_TECTONICS_PaperData/2023_11_15_Clarity_Result_Set_Medication_Administration.csv", sep="^", quote="")
tect_inta_meds_gap2 <- copy(join_mrns)[,MRN:=as.character(MRN) ][med_admin_gap[MAR_Action=="Given", .(Epic_MRN, Medication_Name, TAKEN_TIME, Dose_Value, Dose_UoM)], on="MRN==Epic_MRN", allow.cartesian=TRUE, nomatch=NULL][TAKEN_TIME > AN_START_DATETIME & TAKEN_TIME < AN_STOP_DATETIME]

tect_intra_infusions_gap2 <- copy(join_mrns)[,MRN:=as.character(MRN) ][med_admin_gap[INFUSION_RATE != "NULL",  .(Epic_MRN, Medication_Name, TAKEN_TIME, Dose_Value, Dose_UoM)], on="MRN==Epic_MRN", allow.cartesian=TRUE, nomatch=NULL][TAKEN_TIME > AN_START_DATETIME & TAKEN_TIME < AN_STOP_DATETIME][is.finite(as.numeric(Dose_Value))]

med_admin_gap2 <- fread("2023_07_TECTONICS_PaperData/2023_11_122_Clarity_Result_Set_Medication_Administration_202206.csv", sep="^", quote="")
tect_inta_meds_gap3 <- copy(join_mrns)[,MRN:=as.character(MRN) ][med_admin_gap2[MAR_Action=="Given", .(Epic_MRN, Medication_Name, TAKEN_TIME, Dose_Value, Dose_UoM)], on="MRN==Epic_MRN", allow.cartesian=TRUE, nomatch=NULL][TAKEN_TIME > AN_START_DATETIME & TAKEN_TIME < AN_STOP_DATETIME]

tect_intra_infusions_gap3 <- copy(join_mrns)[,MRN:=as.character(MRN) ][med_admin_gap2[INFUSION_RATE != "NULL",  .(Epic_MRN, Medication_Name, TAKEN_TIME, Dose_Value, Dose_UoM)], on="MRN==Epic_MRN", allow.cartesian=TRUE, nomatch=NULL][TAKEN_TIME > AN_START_DATETIME & TAKEN_TIME < AN_STOP_DATETIME][is.finite(as.numeric(Dose_Value))]


# copy(tect_intra_infusions)[, roughdate := TAKEN_TIME %>% roughen_date][ , .(uniqueN(SurgicalRecordID)), by="roughdate"][order(roughdate)]
# 
# copy(tect_intra_infusions_gap2)[, roughdate := TAKEN_TIME %>% roughen_date][ , .(uniqueN(SurgicalRecordID)), by="roughdate"][order(roughdate)]
# 
# copy(tect_intra_infusions_gap3)[, roughdate := TAKEN_TIME %>% roughen_date][ , .(uniqueN(SurgicalRecordID)), by="roughdate"][order(roughdate)]


tect_inta_meds <- rbind(tect_inta_meds, tect_inta_meds_gap2, tect_inta_meds_gap3)
tect_intra_infusions <- rbind(tect_intra_infusions,  tect_intra_infusions_gap2, tect_intra_infusions_gap3 )


unique_med_names <- tect_inta_meds$Medication_Name %>% unique

redose_intervals <- data.table(drug=c("ampicillin" , "aztreonam", "cefazolin", "cefepime", "cefoxitin", "ceftriaxone", "ciprofloxacin", "clindamycin", "ertapenem", "gentamycin", "linezolid", "meropenem", "metronidazole", "piperacillin", "vancomycin"), interval= c(2 , 4,  4 , 4, 2, 12, 8, 6, 24, 8, 10, 2, 8 , 2, 12))
## gent and vanc are complicated, exclude them
redose_intervals <-redose_intervals[ !grepl(drug, pattern="^gent")]
redose_intervals <-redose_intervals[ !grepl(drug, pattern="^vanc")]

abx_names <- NULL
## it turns out to be more managable to store this table
for(local_name in redose_intervals$drug) {
  abx_names <- c(abx_names , unique_med_names %>% grep(pattern=local_name, value=T, ignore.case=T) )
}

## eliminate non-IV routes as these are very likely not the anesth provider
abx_names  <- abx_names %>% grep(pattern='ORAL ', value=T, invert=T) %>% 
  grep(pattern='INTRAVITREAL', value=T, invert=T) %>% 
  grep(pattern='OPHTHALMIC', value=T, invert=T) %>% 
  grep(pattern='SUBCONJUNCTIVAL', value=T, invert=T) %>% 
  grep(pattern='ENEMA', value=T, invert=T) %>% 
  grep(pattern='IRRIGATION', value=T, invert=T) %>% 
  grep(pattern='INTRAVENTRICULAR', value=T, invert=T) %>% 
  grep(pattern='TOPICAL', value=T, invert=T) %>% 
  grep(pattern='VAGINAL', value=T, invert=T) %>% 
  grep(pattern='EYE DROPS', value=T, invert=T) %>% 
  grep(pattern='OINTMENT', value=T, invert=T) %>% 
  grep(pattern='TABLET', value=T, invert=T) %>% 
  grep(pattern='SUBJUNCTIVAL', value=T, invert=T) %>% 
  grep(pattern='CAPSULE', value=T, invert=T) 

abx_only  <- tect_inta_meds[Medication_Name  %in% abx_names]

## replace the name with a computable name using the table
abx_only [ , cname := NA_character_]
abx_only [ , redose := NA_real_]

for(local_name in redose_intervals$drug) {
  local_abx <- abx_names %>% grep(pattern=local_name, value=T, ignore.case=T) 
  abx_only [ Medication_Name  %in% local_abx , cname := local_name]
  abx_only [ Medication_Name  %in% local_abx , redose := as.numeric(redose_intervals[drug == local_name , interval])]
}

setorder(abx_only , SurgicalRecordID, cname, TAKEN_TIME, redose)

## now, keep at most 3 doses of any drug
abx_only <- abx_only[abx_only[ ,.I[seq_len(min(3, .N)) ] , by=c("SurgicalRecordID", "cname")]$V1  ]

## find the next admin time after intraop
abx_only[ , next_time := shift(TAKEN_TIME, type="lead"), by = c("SurgicalRecordID", "cname") ]
abx_only[ ,  next_time := as.numeric(difftime(next_time, TAKEN_TIME, units="hours") ) ]

tect_abx_only <- abx_only[ !is.na(next_time) , .(SurgicalRecordID, DrugName=cname, Abx_Redosed_at=next_time, Abx_Due_In=redose, Abx_success=next_time < redose+.2 , abx_loss_time = pmax(next_time - redose, 0.) )]

unique_med_names <- tect_intra_infusions$Medication_Name %>% unique

ketofol_names <- c(unique_med_names %>% grep(pattern="KETAMINE", value=T, ignore.case=T) , unique_med_names %>% grep(pattern="PROPOFOL", value=T, ignore.case=T) ) %>% unique
## NOTE: there are a tiny number of blinded placebo/ketamine administrations
tect_prop_only <- tect_intra_infusions[Medication_Name  %in% ketofol_names] %>% setorder(SurgicalRecordID, TAKEN_TIME)
## Note: this has excluded the complicated code used in ACT3 to find patient with different redosing due to prior antibiotics. Usually, these longer redosings are unlikely to occur twice.
## some drugs had renal impairment modifiers imposed later (e.g. pip tazo)

tect_prop_only <- all_demo[,.(WEIGHT_IN_KG,SurgicalRecordID)] [tect_prop_only , on="SurgicalRecordID", nomatch=NA]
tect_prop_only [!is.finite(WEIGHT_IN_KG), WEIGHT_IN_KG :=  80   ]
tect_prop_only[ Dose_UoM=="mcg/kg/min" , infusion_dose_rate :=  as.numeric(Dose_Value)]
tect_prop_only[ Dose_UoM=="mg/hr" , infusion_dose_rate :=  as.numeric(Dose_Value)  * 1000  / 60 / WEIGHT_IN_KG ]
tect_prop_only[ Dose_UoM=="mg/kg/hr" , infusion_dose_rate :=  as.numeric(Dose_Value) * 1000  / 60  ]
tect_prop_only[ Dose_UoM=="mL/hr" , infusion_dose_rate :=  as.numeric(Dose_Value) * 10000  / 60  / WEIGHT_IN_KG ]
tect_prop_only <- tect_prop_only[is.finite(infusion_dose_rate)]


## now the 2022 medication administration
med_admin <- fread('/mark2_act3/easy_hard_version/easymed_admin.csv', sep=";")


act3mk2_inta_meds <- copy(join_mrns)[,MRN:=as.character(MRN) ][med_admin[MAR_Action=="Given", .(Epic_MRN, Medication_Name, TAKEN_TIME, Dose_Value, Dose_UoM)], on="MRN==Epic_MRN", allow.cartesian=TRUE, nomatch=NULL][TAKEN_TIME > AN_START_DATETIME & TAKEN_TIME < AN_STOP_DATETIME]

act3mk2_intra_infusions <- copy(join_mrns)[,MRN:=as.character(MRN) ][med_admin[!(INFUSION_RATE %chin% c("NULL","")),  .(Epic_MRN, Medication_Name, TAKEN_TIME, Dose_Value, Dose_UoM)], on="MRN==Epic_MRN", allow.cartesian=TRUE, nomatch=NULL][TAKEN_TIME > AN_START_DATETIME & TAKEN_TIME < AN_STOP_DATETIME][is.finite(as.numeric(Dose_Value))]


unique_med_names <- act3mk2_inta_meds$Medication_Name %>% unique


abx_names <- NULL
## it turns out to be more managable to store this table
for(local_name in redose_intervals$drug) {
abx_names <- c(abx_names , unique_med_names %>% grep(pattern=local_name, value=T, ignore.case=T) )
}

## eliminate non-IV routes as these are very likely not the anesth provider
abx_names  <- abx_names %>% grep(pattern='ORAL ', value=T, invert=T) %>% 
  grep(pattern='INTRAVITREAL', value=T, invert=T) %>% 
  grep(pattern='OPHTHALMIC', value=T, invert=T) %>% 
  grep(pattern='SUBCONJUNCTIVAL', value=T, invert=T) %>% 
  grep(pattern='ENEMA', value=T, invert=T) %>% 
  grep(pattern='IRRIGATION', value=T, invert=T) %>% 
  grep(pattern='INTRAVENTRICULAR', value=T, invert=T) %>% 
  grep(pattern='TOPICAL', value=T, invert=T) %>% 
  grep(pattern='VAGINAL', value=T, invert=T) %>% 
  grep(pattern='EYE DROPS', value=T, invert=T) %>% 
  grep(pattern='OINTMENT', value=T, invert=T) %>% 
  grep(pattern='TABLET', value=T, invert=T) %>% 
  grep(pattern='SUBJUNCTIVAL', value=T, invert=T) %>% 
  grep(pattern='CAPSULE', value=T, invert=T) 

abx_only  <- act3mk2_inta_meds[Medication_Name  %in% abx_names]


## replace the name with a computable name using the table
abx_only [ , cname := NA_character_]
abx_only [ , redose := NA_real_]

for(local_name in redose_intervals$drug) {
  local_abx <- abx_names %>% grep(pattern=local_name, value=T, ignore.case=T) 
  abx_only [ Medication_Name  %in% local_abx , cname := local_name]
  abx_only [ Medication_Name  %in% local_abx , redose := as.numeric(redose_intervals[drug == local_name , interval])]
}

setorder(abx_only , SurgicalRecordID, cname, TAKEN_TIME, redose)

## now, keep at most 3 doses of any drug
abx_only <- abx_only[abx_only[ ,.I[seq_len(min(3, .N)) ] , by=c("SurgicalRecordID", "cname")]$V1  ]

## find the next admin time after intraop
abx_only[ , next_time := shift(TAKEN_TIME, type="lead"), by = c("SurgicalRecordID", "cname") ]
abx_only[ ,  next_time := as.numeric(difftime(next_time, TAKEN_TIME, units="hours") ) ]

act3mk2_abx_only <- abx_only[ !is.na(next_time) , .(SurgicalRecordID, DrugName=cname, Abx_Redosed_at=next_time, Abx_Due_In=redose, Abx_success=next_time < redose+.2 , abx_loss_time = pmax(next_time - redose, 0.) )]

unique_med_names <- act3mk2_intra_infusions$Medication_Name %>% unique

ketofol_names <- c(unique_med_names %>% grep(pattern="KETAMINE", value=T, ignore.case=T) , unique_med_names %>% grep(pattern="PROPOFOL", value=T, ignore.case=T) ) %>% unique
act3mk2_prop_only <- act3mk2_intra_infusions[Medication_Name  %in% ketofol_names] %>% setorder(SurgicalRecordID, TAKEN_TIME)
act3mk2_prop_only <- all_demo[,.(WEIGHT_IN_KG,SurgicalRecordID)] [act3mk2_prop_only , on="SurgicalRecordID", nomatch=NA]
act3mk2_prop_only [!is.finite(WEIGHT_IN_KG), WEIGHT_IN_KG :=  80   ]
act3mk2_prop_only[ Dose_UoM=="mcg/kg/min" , infusion_dose_rate :=  as.numeric(Dose_Value)]
act3mk2_prop_only[ Dose_UoM=="mg/hr" , infusion_dose_rate :=  as.numeric(Dose_Value)  * 1000  / 60 / WEIGHT_IN_KG ]
act3mk2_prop_only[ Dose_UoM=="mg/kg/hr" , infusion_dose_rate :=  as.numeric(Dose_Value) * 1000  / 60  ]
act3mk2_prop_only[ Dose_UoM=="mL/hr" , infusion_dose_rate :=  as.numeric(Dose_Value) * 10000  / 60  / WEIGHT_IN_KG ]
act3mk2_prop_only[ Dose_UoM=="NULL" , infusion_dose_rate :=  0. ]

act3mk2_prop_only <- act3mk2_prop_only[is.finite(infusion_dose_rate)]



## now the 2021 data
med_admin <- fread('/early_act3/Clarity Hogue Result Set Medication Administration.csv')
setnames(med_admin, "CurrentMRN", "Epic_MRN")
setnames(med_admin, "MED_ADMIN_AMT", "Dose_Value")
setnames(med_admin, "MED_NAME", "Medication_Name")
setnames(med_admin, "MED_ADMIN_DOSE_START_DATE", "TAKEN_TIME")
setnames(med_admin, "MED_ADMIN_DOSAGE_RATE_UNITS", "Dose_UoM")
setnames(med_admin, "MED_ADMIN_STATUS_TEXT", "MAR_Action")

act3_inta_meds <- copy(join_mrns)[med_admin[MAR_Action=="Given", .(Epic_MRN, Medication_Name, TAKEN_TIME, Dose_Value, Dose_UoM)], on="MRN==Epic_MRN", allow.cartesian=TRUE, nomatch=NULL][TAKEN_TIME > AN_START_DATETIME & TAKEN_TIME < AN_STOP_DATETIME]

## Dose_Value is cumulative, MED_ADMIN_DOSAGE_RATE_AMT is a mL/hr rate - above are both mcg/kg/min
## easy enough to join on weight to convert
# act3_intra_infusions <- copy(join_mrns)[med_admin[!(MED_ADMIN_DOSAGE_RATE_AMT %chin% c("NULL","")),  .(Epic_MRN, Medication_Name, TAKEN_TIME, Dose_Value, Dose_UoM)], on="MRN==Epic_MRN", allow.cartesian=TRUE, nomatch=NULL][TAKEN_TIME > AN_START_DATETIME & TAKEN_TIME < AN_STOP_DATETIME]
# 
# act3_intra_infusions <- copy(join_mrns)[med_admin[!(MED_ADMIN_DOSAGE_RATE_AMT %chin% c("NULL",""))], on="MRN==Epic_MRN", allow.cartesian=TRUE, nomatch=NULL][TAKEN_TIME > AN_START_DATETIME & TAKEN_TIME < AN_STOP_DATETIME][Medication_Name %like% "PROPOF"]

# [is.finite(as.numeric(Dose_Value))]
unique_med_names <- act3_inta_meds$Medication_Name %>% unique


abx_names <- NULL
## it turns out to be more managable to store this table
for(local_name in redose_intervals$drug) {
  abx_names <- c(abx_names , unique_med_names %>% grep(pattern=local_name, value=T, ignore.case=T) )
}

## eliminate non-IV routes as these are very likely not the anesth provider
abx_names  <- abx_names %>% grep(pattern='ORAL ', value=T, invert=T) %>% 
  grep(pattern='INTRAVITREAL', value=T, invert=T) %>% 
  grep(pattern='OPHTHALMIC', value=T, invert=T) %>% 
  grep(pattern='SUBCONJUNCTIVAL', value=T, invert=T) %>% 
  grep(pattern='ENEMA', value=T, invert=T) %>% 
  grep(pattern='IRRIGATION', value=T, invert=T) %>% 
  grep(pattern='INTRAVENTRICULAR', value=T, invert=T) %>% 
  grep(pattern='TOPICAL', value=T, invert=T) %>% 
  grep(pattern='VAGINAL', value=T, invert=T) %>% 
  grep(pattern='EYE DROPS', value=T, invert=T) %>% 
  grep(pattern='OINTMENT', value=T, invert=T) %>% 
  grep(pattern='TABLET', value=T, invert=T) %>% 
  grep(pattern='SUBJUNCTIVAL', value=T, invert=T) %>% 
  grep(pattern='CAPSULE', value=T, invert=T) 

abx_only  <- act3_inta_meds[Medication_Name  %in% abx_names]


## replace the name with a computable name using the table
abx_only [ , cname := NA_character_]
abx_only [ , redose := NA_real_]

for(local_name in redose_intervals$drug) {
  local_abx <- abx_names %>% grep(pattern=local_name, value=T, ignore.case=T) 
  abx_only [ Medication_Name  %in% local_abx , cname := local_name]
  abx_only [ Medication_Name  %in% local_abx , redose := as.numeric(redose_intervals[drug == local_name , interval])]
}

setorder(abx_only , SurgicalRecordID, cname, TAKEN_TIME, redose)

## now, keep at most 3 doses of any drug
abx_only <- abx_only[abx_only[ ,.I[seq_len(min(3, .N)) ] , by=c("SurgicalRecordID", "cname")]$V1  ]

## find the next admin time after intraop
abx_only[ , next_time := shift(TAKEN_TIME, type="lead"), by = c("SurgicalRecordID", "cname") ]
abx_only[ ,  next_time := as.numeric(difftime(next_time, TAKEN_TIME, units="hours") ) ]

act3_abx_only <- abx_only[ !is.na(next_time) , .(SurgicalRecordID, DrugName=cname, Abx_Redosed_at=next_time, Abx_Due_In=redose, Abx_success=next_time < redose+.2 , abx_loss_time = pmax(next_time - redose, 0.) )]

## unlike the others, uom is always mL/hr

unique_med_names <- med_admin$Medication_Name %>% unique
ketofol_names <- c(unique_med_names %>% grep(pattern="KETAMINE", value=T, ignore.case=T) , unique_med_names %>% grep(pattern="PROPOFOL", value=T, ignore.case=T) ) %>% unique

act3_prop_only <-copy(join_mrns)[ med_admin[Medication_Name %chin% ketofol_names], on="MRN==Epic_MRN", allow.cartesian=TRUE, nomatch=NULL] [TAKEN_TIME > AN_START_DATETIME & TAKEN_TIME < AN_STOP_DATETIME][MED_ADMIN_DOSAGE_RATE_AMT !=""]
act3_prop_only[, MED_ADMIN_DOSAGE_RATE_AMT := MED_ADMIN_DOSAGE_RATE_AMT %>% as.numeric]
act3_prop_only <- act3_prop_only[is.finite(MED_ADMIN_DOSAGE_RATE_AMT)]

act3_prop_only %>% setorder(SurgicalRecordID, TAKEN_TIME)
act3_prop_only <- all_demo[,.(WEIGHT_IN_KG,SurgicalRecordID)] [act3_prop_only , on="SurgicalRecordID", nomatch=NA]
act3_prop_only [!is.finite(WEIGHT_IN_KG), WEIGHT_IN_KG :=  80   ]
act3_prop_only[, infusion_dose_rate := MED_ADMIN_DOSAGE_RATE_AMT / 60. * 10000. / WEIGHT_IN_KG ]
act3_prop_only <- act3_prop_only[is.finite(infusion_dose_rate)]

rm(med_admin)




all_redoses <-  join_mrns[rbind(act3_abx_only, act3mk2_abx_only, tect_abx_only), on="SurgicalRecordID", allow.cartesian=TRUE]


temp <- merge(
   copy(all_redoses )[, roughdate := Date %>% roughen_date ][, .(UniqueCaseWithAbx=uniqueN(SurgicalRecordID)), by=.(roughdate, isTreatment )][roughdate>2019.25][order(roughdate)] ,
   cases_by_month_summary)[, fractionWRedose := round( UniqueCaseWithAbx/UniqueCase,2)][order(roughdate, isTreatment)] 
   
temp  %>%  fwrite("/research/outputs/antibiotic_redose_presense.csv")

# copy(temp)[,isTreatment:=factor(isTreatment) ] %>% ggplot( aes(x=roughdate, y=UniqueCaseWithAbx, fill=isTreatment)) + geom_bar(stat = "identity", position="stack") + labs(x = "Date", y = "Count", title = "Abx Redoses by month") -> temp2

copy(temp)[,isTreatment:=factor(isTreatment) ] %>% ggplot( aes(x=roughdate, y=fractionWRedose,  group = isTreatment, fill =isTreatment)) + geom_bar(stat = "identity", width=0.05, position=position_identity(), alpha=0.5) + labs(x = "Date", y = "Fraction", title = "Abx Redoses by month") -> temp2
temp2 %>% ggsave(device="png", filename="/research/outputs/redose_present.png")


# , position="dodge"
# all_redoses[ , .(Abx_success = mean(Abx_success) ), by="SurgicalRecordID" ]$Abx_success %>% summary

all_redoses %>% fwrite("/research/intermediates/intermediate_abx.csv")


## longify and common format all the infusions
## most consistent way to do this is to run each infusion from event to event
## this will miss low-dose propofol from ICU takebacks; fixing that is a fair bit of work (include the event most recently before AN_START_DATETIME)
tect_prop_only[ , stop_time := shift(TAKEN_TIME, type="lead"), by= .(SurgicalRecordID, Medication_Name) ]
tect_prop_only[  is.na(stop_time), stop_time := pmax(max(TAKEN_TIME) + dminutes(1), AN_STOP_DATETIME), by =.(SurgicalRecordID, Medication_Name) ]
tect_prop_only[stop_time < TAKEN_TIME , stop_time := TAKEN_TIME+1]
tect_prop_only[, duration:= difftime(stop_time,TAKEN_TIME, units="mins") %>% as.numeric %>% as.integer]
tect_prop_only [, oldrow := .I]
tect_prop_only <- tect_prop_only[ rep(1:.N, times=duration ) ]
tect_prop_only[duration<=1, effectivetime := TAKEN_TIME ]
tect_prop_only[duration>1, effectivetime := TAKEN_TIME + dminutes(seq(.N)-1L) , by="oldrow"]
tect_prop_only <- tect_prop_only[, .(orlogid=SurgicalRecordID, TAKEN_TIME=effectivetime, Medication_Name, infusion_dose_rate )]


act3_prop_only[ , stop_time := MED_ADMIN_DOSE_STOP_DATE ]
act3_prop_only[  is.na(stop_time), stop_time := pmax(max(TAKEN_TIME) + dminutes(1), AN_STOP_DATETIME), by =.(SurgicalRecordID, Medication_Name) ]
act3_prop_only[stop_time < TAKEN_TIME , stop_time := TAKEN_TIME+1]
act3_prop_only[, duration:= difftime(stop_time,TAKEN_TIME, units="mins") %>% as.numeric %>% as.integer]
act3_prop_only [, oldrow := .I]
act3_prop_only <- act3_prop_only[ rep(1:.N, times=duration ) ]
act3_prop_only[duration<=1, effectivetime := TAKEN_TIME ]
act3_prop_only[duration>1, effectivetime := TAKEN_TIME + dminutes(seq(.N)-1L) , by="oldrow"]
act3_prop_only <- act3_prop_only[, .(orlogid=SurgicalRecordID, TAKEN_TIME=effectivetime, Medication_Name, infusion_dose_rate )]


act3mk2_prop_only[ , stop_time := shift(TAKEN_TIME, type="lead"), by= .(SurgicalRecordID, Medication_Name) ]
act3mk2_prop_only[  is.na(stop_time), stop_time := pmax(max(TAKEN_TIME) + dminutes(1), AN_STOP_DATETIME), by =.(SurgicalRecordID, Medication_Name) ]
act3mk2_prop_only[stop_time < TAKEN_TIME , stop_time := TAKEN_TIME+1]
act3mk2_prop_only[, duration:= difftime(stop_time,TAKEN_TIME, units="mins") %>% as.numeric %>% as.integer]
act3mk2_prop_only [, oldrow := .I]
act3mk2_prop_only <- act3mk2_prop_only[ rep(1:.N, times=duration ) ]
act3mk2_prop_only[duration<=1, effectivetime := TAKEN_TIME ]
act3mk2_prop_only[duration>1, effectivetime := TAKEN_TIME + dminutes(seq(.N)-1L) , by="oldrow"]
act3mk2_prop_only <- act3mk2_prop_only[, .(orlogid=SurgicalRecordID, TAKEN_TIME=effectivetime, Medication_Name, infusion_dose_rate )]


all_prop <- rbind(tect_prop_only, act3_prop_only, act3mk2_prop_only) %>% unique
unique_med_names <- all_prop$Medication_Name %>% unique %>% grep(pattern="KETAM", value=TRUE)
all_prop <- all_prop[infusion_dose_rate > .001]

all_prop[, exclude_low_max := infusion_dose_rate > 40 ] ## threshold for propofol
all_prop[Medication_Name %chin% unique_med_names , exclude_low_max := infusion_dose_rate > .3/60*1000] ## lower threshold for ketamine

all_prop %>% fwrite("/research/intermediates/intermediate_propofol.csv")
} else{

all_prop <- fread("/research/intermediate_propofol.csv")
all_redoses <- fread("/research/intermediate_abx.csv")

}


