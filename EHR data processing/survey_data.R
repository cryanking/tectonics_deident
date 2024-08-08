.libPaths( c( "/root/R/x86_64-pc-linux-gnu-library/4.4/" , .libPaths() ) )
library(groundhog) 
set.groundhog.folder('/root/R_groundhog2/')
groundhog.library(c(
"data.table" ,  
"dplyr" , 
"magrittr" , 
"lubridate" , 
"ggplot2" , 
"readxl" , 
"stringr" ) , '2024-05-01')
 


setwd("/research")


nms <- names(read_xlsx('/research/2023_07_TECTONICS_PaperData/survey data/TECTONICS survey data_10.26.23_JA.xlsx' , n_max = 0))
ct <- fifelse(grepl("Timestamp", nms) | grepl("Instant", nms) , "date", "guess")

surveyresults <- read_xlsx('/research/2023_07_TECTONICS_PaperData/survey data/TECTONICS survey data_10.26.23_JA.xlsx', col_types=ct)

setDT(surveyresults )

surveyresults <- surveyresults[`In which setting do you primarily work?` != "ACT"]

## original teamstepps groups
team_function_questions <- c(
"The ACT team's skillset is useful to the OR team." 
,"The OR team can comfortably rely on the ACT team to aid them."
,"The ACT team understands their roles and responsibilities."
,"The OR team understands their roles and responsibilities."
,"The ACT has clearly articulated goals."
,"The ACT operates at a high level of efficiency."
)

situation_monitoring_questions <- c(
"The ACT team effectively identifies how to support the OR team's intra-operative patient care."
,"The ACT team shares information regarding potential complications with the OR team (e.g., patient risks to mortality, AKI, pneumonia, etc.)."
,"The ACT team develops a mitigation plan for potential patient complications and shares helpful guidance with the OR team." 
,"The ACT team provides appropriate and convincing evidence to the OR team to support their mitigation plan for potential patient complications."
,"The OR team considers ACT team input when making decisions about patient care."
,"The OR team takes time to work with the ACT team to develop a plan for patient care."
,"Disagreements between the ACT team and OR team are resolved successfully."
,"The ACT team ensures that the OR team is aware of any situations or changes that may affect patient care."
,"The OR team shares information regarding changes in intra-operative decisions made with the ACT team." 
,"The ACT team meets with the OR team to reevaluate patient care goals when aspects of the situation have changed."
,"The ACT team provides useful alerts to the OR team during high workload or urgent situations, when things might be missed." 
)

mutual_support_questions<- c(
 "The ACT team provides assistance by sending a firefighter or additional clinician physically to the OR when there is a high workload or urgent situation."
, "The OR team can request assistance from the ACT team when they feel overwhelmed."
, "The ACT team cautions the OR team about potentially dangerous clinical situations."
, "The ACT team looks for things that have been missed within the OR, such as procedural or information errors."
, "Assessments and recommendations from the ACT team to the OR team are delivered in a way to promote positive interactions and future change."
, "OR team responses to the ACT team's messages and calls are delivered in a way to promote positive interactions and future change."
, "The ACT team advocates for patients even when their opinions conflicts with that of a senior member of the team."
, "The OR team advocates for patients even when their opinions conflict with that of a senior member of the team."
, "When the ACT team has a concern about intra-operative patient care, they encourage the OR team to reconsider their care plan and make sure their concern has been heard."
)

communication_questions_original<- c(
  "The ACT team relays relevant information to the OR team in a timely manner."
, "The OR team relays relevant information to the ACT team in a timely manner."
, "The OR team acknowledges that they received the ACT team's message."
, "The ACT team follows a standardized method of sharing information to the OR team."
, "The ACT team seeks information from all available sources."
, "The OR team seeks information from all available resources."
)

team_step_score <- function(x) match(x, rev(c('Strongly Agree',	'Agree'	,'Neutral' , 'Disagree' , 'Strongly Disagree')) )

surveyresults[, teamstep_com := .SD %>% sapply( team_step_score )  %>% as.matrix %>% rowMeans( na.rm=T), .SDcols=communication_questions_original ]
surveyresults[, teamstep_support := .SD %>% sapply( team_step_score ) %>% as.matrix %>% rowMeans( na.rm=T), .SDcols=mutual_support_questions ]
surveyresults[, teamstep_sit := .SD %>% sapply( team_step_score ) %>% as.matrix %>% rowMeans( na.rm=T), .SDcols=situation_monitoring_questions ]
surveyresults[, teamstep_team := .SD %>% sapply( team_step_score ) %>% as.matrix %>% rowMeans( na.rm=T), .SDcols=team_function_questions ]

surveyresults[, overall_teamsteps := .SD %>% sapply( team_step_score ) %>% as.matrix %>% rowMeans( na.rm=T), .SDcols=c(team_function_questions,situation_monitoring_questions, mutual_support_questions,communication_questions_original) ]


# myfun <- function(x) list(mean=mean(x, na.rm=T) %>% round(1), sd=sd(x, na.rm=T)%>% round(1), N=sum(!is.na(x)) )
# surveyresults[, unlist(lapply(.SD, myfun ), recursive=T ), .SDcols = c('overall_teamsteps',"teamstep_com","teamstep_support","teamstep_sit","teamstep_team") , drop=F]


## what is the overall answer rate?
## two very unreliable surveys (1 and 3 questions)
surveyresults[ , nanswered_teamsteps := .SD %>% sapply( team_step_score ) %>% as.matrix %>% is.na %>% not %>% rowSums   , .SDcols=c(team_function_questions,situation_monitoring_questions, mutual_support_questions,communication_questions_original) ] 
# surveyresults[ , nanswered_teamsteps := rowSums( not(is.na(as.matrix(.SD %>% sapply( team_step_score ) )) ) ) , .SDcols=c(team_function_questions,situation_monitoring_questions, mutual_support_questions,communication_questions_original) ] 
surveyresults$nanswered_teamsteps %>% table
#  0  1  3 15 16 18 19 20 28 29 30 31 32 
# 24  1  1  1  3  5  8 25  1  3  2  1  9 
surveyresults$nanswered_teamsteps %>% is_greater_than(0) %>% table


aim_questions <- c(
  "I approve of the continued use of the ACT."
, "Continued use of the ACT to assist the OR team is appealing to me."
, "I like the ACT."
, "I welcome the continued use of the ACT within BJH."
)

iam_questions <- c(
  "The ACT fits the OR team's needs for remote assistance on surgical cases."
, "The ACT's components suit and work well with the current setup across BJH surgical pods."
, "The ACT's functions are applicable to patient cases across BJH surgical pods."
, "Long-term use of the ACT within BJH can improve process and clinical outcomes."
)

fim_questions <- c(
  "Large-scale implementation of the ACT across other surgical sites/hospitals/health systems seems feasible."
, "The ACT is user-friendly for both the ACT team and OR team."
)


training_questions <- c(
  "Rotations within the ACT among faculty, residents, CRNAs, and SRNAs have been educational and useful in gaining a better understanding of the perioperative patient care workflow."
, "Rotations within the ACT provide opportunities for training junior clinicians (e.g., residents, SRNAs)."
)


overall_questions <- c(
 "Use of the ACT has led to a positive change in the institution's intra-operative team structure."
,"Use of the ACT has led to a positive change in the institution's intra-operative team communication."
,"Use of the ACT has led to a positive change in intra-operative patient care within the OR."
)


surveyresults[, aim_scale := .SD %>% sapply( team_step_score )  %>% as.matrix %>% rowMeans( na.rm=T), .SDcols=aim_questions ]
surveyresults[, iam_scale := .SD %>% sapply( team_step_score ) %>% as.matrix %>% rowMeans( na.rm=T), .SDcols=iam_questions ]
surveyresults[, fim_scale:= .SD %>% sapply( team_step_score ) %>% as.matrix %>% rowMeans( na.rm=T), .SDcols=fim_questions ]
surveyresults[, training_scale := .SD %>% sapply( team_step_score ) %>% as.matrix %>% rowMeans( na.rm=T), .SDcols=training_questions ]
surveyresults[, overall_scale := .SD %>% sapply( team_step_score ) %>% as.matrix %>% rowMeans( na.rm=T), .SDcols=overall_questions ]



myf2 <- function(z) {surveyresults[, .(mean=mean(unlist(.SD), na.rm=T ) %>% round(1), sd=sd(unlist(.SD), na.rm=T ) %>% round(1), N=sum(!is.na( unlist(.SD) ) ) ) , .SDcols = z , drop=F]}

teamsteps_res <- lapply(c('overall_teamsteps',"teamstep_com","teamstep_support","teamstep_sit","teamstep_team","aim_scale", "iam_scale","fim_scale", "training_scale", "overall_scale"), myf2 ) %>% rbindlist

set(teamsteps_res , j="question_group", value=c("overall", "communication","mutual support", "situation monitoring", "team function", "AIM", "IAM", "FIM","training value","overall team") )

setcolorder(teamsteps_res, 4 )

teamsteps_res


myf2 <- function(z) {surveyresults[, .(mean=mean(unlist(.SD), na.rm=T ) %>% round(1), sd=sd(unlist(.SD), na.rm=T ) %>% round(1), N=sum(!is.na( unlist(.SD) ) ) ) , .SDcols = z , drop=F, by="What is your professional role within BJH?"]}

teamsteps_res <- lapply(c('overall_teamsteps',"teamstep_com","teamstep_support","teamstep_sit","teamstep_team","aim_scale", "iam_scale","fim_scale", "training_scale", "overall_scale"), myf2 ) %>% rbindlist

set(teamsteps_res , j="question_group", value=rep(c("overall", "communication","mutual support", "situation monitoring", "team function", "AIM", "IAM", "FIM","training value","overall team"), each=3) )

setcolorder(teamsteps_res, 5 )

teamsteps_res


# lapply(training_questions , myf)
# lapply(iam_questions , myf)
# lapply(aim_questions , myf)
# lapply(communication_questions , myf)
# lapply(support_questions , myf)
# lapply(monitoring_questions , myf)
# lapply(overall_questions , myf)
for( thisvar in unique(c(training_questions,iam_questions, aim_questions,communication_questions,support_questions,monitoring_questions,overall_questions))) {
if( surveyresults[[thisvar]] %>% equals("Not enough experience") %>% any(na.rm=T)) {
  set(surveyresults , i = surveyresults[[thisvar]] %>% equals("Not enough experience") %>% which , j=thisvar, x=NA_character_)
}
}

myf <- function(x) { list(
overall=surveyresults[, .SD,  .SDcols=x] %>% unlist %>% na.omit %>% match( c("Agree", "Strongly Agree"), nomatch=0) %>% is_greater_than(0) %>% table %>% addmargins , 
overall_prop=surveyresults[, .SD,  .SDcols=x] %>% unlist %>% na.omit %>% match( c("Agree", "Strongly Agree"), nomatch=0) %>% is_greater_than(0) %>% table %>% prop.table %>% round(2) , 
participants= surveyresults[, fcoalesce(.SD),  .SDcols=x] %>% na.omit %>% length ,
specific=surveyresults[, .SD,  .SDcols=x] %>% lapply(function(x) {x %>% na.omit %>% match( c("Agree", "Strongly Agree"), nomatch=0) %>% is_greater_than(0) %>% table%>% addmargins } ) )
}

overall_questions %>% myf
training_questions %>% myf
communication_questions %>% myf
support_questions %>% myf
monitoring_questions %>% myf
iam_questions %>% myf
aim_questions %>% myf


myf2 <- function(x) { 
temp <-  surveyresults[, .SD,  .SDcols=x] %>% unlist %>% na.omit %>% match( c("Agree", "Strongly Agree"), nomatch=0) %>% is_greater_than(0) %>% na.omit
list(
overall=paste0( temp %>% sum , "/", temp %>% length, " (" , temp %>% mean %>% round(2) %>% multiply_by(100), "%)"  )
, participants= surveyresults[, fcoalesce(.SD),  .SDcols=x] %>% na.omit %>% length )
}

survey_summary <- bind_rows(overall_questions %>% myf2
,training_questions %>% myf2
,communication_questions %>% myf2
,support_questions %>% myf2
,monitoring_questions %>% myf2
,iam_questions %>% myf2
,aim_questions %>% myf2
)

survey_summary[["question group"]] <- c("overall" , "training environment", "communication effectiveness", "mutual support", "situation monitoring", "IAM", "AIM")
survey_summary %<>% select(`question group`, everything() )
survey_summary %>% fwrite("/research/outputs/survey_summary.csv")


myf2 <- function(z, newname="") { 
  surveyresults[, .(`Question group`= newname, overall=.SD %>% unlist %>% na.omit %>% match( c("Agree", "Strongly Agree"), nomatch=0) %>% is_greater_than(0) %>% na.omit %>% (function(x) paste0( x %>% sum , "/", x %>% length, " (" , x %>% mean %>% round(2) %>% multiply_by(100), "%)"  ) ) , participants=.SD %>% fcoalesce %>% na.omit %>% length ),  .SDcols=z, by="What is your professional role within BJH?"] 
}

survey_summary <- bind_rows(overall_questions %>% myf2("overall")
,training_questions %>% myf2("training environment")
,communication_questions %>% myf2("communication effectiveness")
,support_questions %>% myf2("mutual support")
,monitoring_questions %>% myf2("situation monitoring")
,iam_questions %>% myf2("IAM")
,aim_questions %>% myf2("AIM")
)

survey_summary %<>% select(`Question group`, everything() )

survey_summary %>% fwrite("/research/outputs/survey_summary_stratified.csv")

## write out data tabulations

all_survey_vars <- (c(training_questions,iam_questions, aim_questions,communication_questions,support_questions,monitoring_questions,overall_questions))

result_holder <- list()
for( thisstatus in unique(surveyresults[["What is your professional role within BJH?"]] ) ){
local_data <- surveyresults[`What is your professional role within BJH?`==thisstatus , .SD, .SDcols=all_survey_vars]
result_holder[[thisstatus]] <- local_data %>% lapply( .%>% factor(levels=c('Strongly Agree',	'Agree'	,'Neutral' , 'Disagree' , 'Strongly Disagree')) %>% table %>% as.list ) %>% bind_rows %>% mutate(Question=colnames(local_data), `Professional Role`=thisstatus)
}

result_holder %>% bind_rows %>% fwrite("/research/outputs/survey_data_stratified.csv")



