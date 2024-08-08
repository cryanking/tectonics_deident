.libPaths( c( "/root/R/x86_64-pc-linux-gnu-library/4.4/" , .libPaths() ) )
library(groundhog) 
set.groundhog.folder('/root/R_groundhog2/')
groundhog.library(c(
"data.table" ,  
"dplyr" , 
"magrittr" , 
"lubridate" , 
"xgboost" , 
"randtoolbox" ) , '2024-05-01')



setwd("/research")
clarity_root <- '/early_act3/'
roughen_date <- function(x) round(year(x) + (month(x)-1)/12,2)  

na_false <- function(x) {fifelse(is.na(x), FALSE, x ) }

## pre-trial procedure text and mortality (epic)

ACTFAST_EP_ID = fread('/early_act3/Clarity Harper Result Set Anesthesia Identifiers.csv',sep=',' )
ACTFAST_EP_ID = ACTFAST_EP_ID[!is.na(AN_PROC_NAME) , .(AN_PROC_NAME, LOG_ID, AN_DATE)]

mortality <- fread("2023_07_TECTONICS_PaperData/2023_07_17_Clarity_Result_Set_DeathDates.csv", sep="^", quote="")[ , Death_Date:=  ymd_hms(Death_Date) ] ## MRN, date

epic_new_events <- fread(paste0(clarity_root , 'Clarity Harper Result Set Operating Room Event Log.csv'))
epic_new_events[, CPB_TIME := ymd_hm(CPB_TIME)]
an_start_stop <- epic_new_events[ , .(CurrentMRN = min(CurrentMRN), AnestStart=min(CPB_TIME[Event=="Anesthesia Start"] ), AnestStop=min(CPB_TIME[Event=="Anesthesia Stop"] ) ) , by="orlogid"]

an_start_stop <- an_start_stop[ data.table::between(AnestStart, ymd('2018-09-10'), ymd('2019-06-30') ) ] 

joined_mortality <- mortality[  an_start_stop[, .(LOG_ID=orlogid, CurrentMRN)][ ACTFAST_EP_ID, on="LOG_ID", nomatch=NULL], on="CurrentMRN", nomatch=NA]

joined_mortality[, survival_time := difftime(Death_Date, AN_DATE, units="days") %>% as.numeric] 
joined_mortality[is.na(survival_time), survival_time := 365]
joined_mortality[, death_in_30 := survival_time < 30]
joined_mortality <- joined_mortality[, .(PatientID=.I, death_in_30, survival_time, ScheduledProcedure=AN_PROC_NAME)]

## delete some tokens that don't belong
text_pipe <- . %>% tolower %>% 
 gsub(x=., pattern="dos:?\\s?[0-9\\\\\\/-]+", replacement=" " ) %>% 
 gsub(pattern="[[:punct:]]", replacement=" " ,.) %>%
 gsub(pattern="\\bdr\\s+\\w+", replacement="" ,.) %>%
 gsub(pattern="\\bunos\\s+\\w+", replacement="" ,.) %>%
 gsub(pattern="\\b\\w\\b", replacement="" ,.) %>%
 gsub(pattern="\\b\\d+\\b", replacement="" ,.) %>%
 gsub(pattern="\\s+", replacement=" " ,.) %>% 
 trimws 

joined_mortality[, ScheduledProcedure := ScheduledProcedure %>% text_pipe ]

## import the older metavision survival
## External code: avoid importing a bunch of unnecessary PHI
if(FALSE) {
surg_text <- readr::read_delim("/research2/ActFast_Big/PreOp_Static/2020_07_ACTFAST_MV_PreOp.csv" , na=c("NULL","","na","-", "*", "U", "NR", "?", "+") ,  delim="^", col_types="ccccccTccciiiddddiiiiiiiiiiiiiiiiiiiiiiiiiiiiidiiiiiiiidddddd")
surg_text %>% filter(Surg_PatientID==Data_PatientID) %>% select(MRN, Surg_PatientID, DoS, ScheduledProcedure) -> surg_text
setDT(surg_text)
surg_text[, Surg_PatientID := as.character(Surg_PatientID)]
setorder(surg_text, Surg_PatientID , -DoS)
surg_text %<>% unique(on="Surg_PatientID")
# surg_text[, ncases := .N, by="Surg_PatientID"]
intermediates_root <- "/research2/ActFast_Intermediates"
intermediate_outcomes_root <- paste0(intermediates_root, "/intermediate_outcomes")
output_id_location0 <- paste0(intermediate_outcomes_root ,"/mv_era_all_ids.csv")
id_links <- fread(output_id_location0)
id_links <- id_links[, .(Surg_PatientID=PatientID, EMPI, AnestStart)]

updated_mortality_data <- fread( "/research2/ActFast_BJ_Data/Mortality_update/metav.csv")
updated_mortality_data[, MPI:=as.character(MPI)]
updated_mortality_data <- updated_mortality_data [id_links, on="MPI==EMPI", allow.cartesian=TRUE]
updated_mortality_data[ trimws(DATE_OF_DEATH)=="", DATE_OF_DEATH:="2021-01-07-00.00.00" ]
updated_mortality_data[ , DATE_OF_DEATH := ymd_hms(DATE_OF_DEATH)]
updated_mortality_data[, Surg_PatientID:= as.character(Surg_PatientID)]
updated_mortality_data <- updated_mortality_data[ surg_text, on="Surg_PatientID", nomatch=NA] 

updated_mortality_data <- updated_mortality_data[, .(Surg_PatientID, survival_time=as.numeric(difftime(DATE_OF_DEATH,AnestStart, units="days" ) ) , ScheduledProcedure) ]
updated_mortality_data [is.na(survival_time), survival_time := 365]
updated_mortality_data[, death_in_30 := survival_time<30]
updated_mortality_data[ , Surg_PatientID := .I]
text_pipe <- . %>% tolower %>% 
 gsub(x=., pattern="dos:?\\s?[0-9\\\\\\/-]+", replacement=" " ) %>% 
 gsub(pattern="[[:punct:]]", replacement=" " ,.) %>%
 gsub(pattern="\\bdr\\s+\\w+", replacement="" ,.) %>%
 gsub(pattern="\\bunos\\s+\\w+", replacement="" ,.) %>%
 gsub(pattern="\\b\\w\\b", replacement="" ,.) %>%
 gsub(pattern="\\b\\d+\\b", replacement="" ,.) %>%
 gsub(pattern="\\s+", replacement=" " ,.) %>% 
 trimws 

updated_mortality_data[, ScheduledProcedure := ScheduledProcedure %>% text_pipe ]

## privacy preserving: remove any tokens occuring fewer than 100 times
admission_vocab <- updated_mortality_data$ScheduledProcedure %>% strsplit( split =' ') %>% unlist %>% table %>% sort( decreasing=TRUE)
very_rare <- names(admission_vocab)[admission_vocab<=100]

newwords <- updated_mortality_data$ScheduledProcedure %>% strsplit(split=" ")
newwords <- sapply(relist(fifelse(unlist(newwords) %chin% very_rare, "", unlist(newwords)), newwords), paste, collapse=" " )

updated_mortality_data[, ScheduledProcedure := newwords  %>% trimws %>% gsub(pattern="\\s+", replacement=" " ,.) ]

updated_mortality_data %>% fwrite('/research/2023_07_TECTONICS_PaperData/mv_survival.csv')
} else {
mv_survival <- fread('/research/2023_07_TECTONICS_PaperData/mv_survival.csv')
}

## join and filter for rare tokens

joined_mortality<-rbind(joined_mortality[, .(survival_time, death_in_30, ScheduledProcedure)], mv_survival[, .(survival_time, death_in_30, ScheduledProcedure)] )

joined_mortality <- joined_mortality[!is.na(ScheduledProcedure)]

newwords <- joined_mortality$ScheduledProcedure %>% strsplit(split=" ")
admission_vocab <- newwords  %>% unlist %>% table %>% sort
very_rare <- names(admission_vocab)[admission_vocab<=100]
admission_vocab <- names(admission_vocab)[admission_vocab > 100]
newwords <- sapply(relist(fifelse(unlist(newwords) %chin% very_rare, "", unlist(newwords)), newwords), paste, collapse=" " )
joined_mortality[, ScheduledProcedure := newwords  %>% trimws %>% gsub(pattern="\\s+", replacement=" " ,.) ]

## this is a rougher but simpler approach than used before; there are many dropped words that are compound (like substernal)

newwords <- joined_mortality$ScheduledProcedure %>% strsplit(split=" ")
newwords <-sapply(newwords, match, table=admission_vocab)

newcols <- Matrix::sparseMatrix( i= rep(seq_along(newwords ), newwords %>% lengths ) , j= unlist(newwords), x=1)

## write the word names
admission_vocab %>% data.table %>% fwrite("/research/intermediates/word_indicies.csv")

## xgboost
## nthread not actually passed, will use max available

xgboost_cv <- function(data
  , label=NULL
  , objective = "binary:logistic"
  , max_depth=NA_real_
  , eta=NA_real_
  , nrounds=NA_real_
  , min_child_weight=NA_real_
  , gamma=NA_real_
  , subsample=NA_real_
  , lambda=NA_real_
  , num_parallel_tree = NA_real_
  , lower_bounds = c(2L, .1,  5L, log(0.3), 0, .7  , log(.02) , 1L)
  , upper_bounds = c(6L, 1., 15L, log(200), 5, .95 , log(2000), 1L)
  , nthread=1L
  , sobol_size=15L
  , nfold=5L
  , tree_method="auto"
  , eval_metric="logloss"
  , other_params=NULL) {
  if(any( c("matrix", "dgRMatrix", "dgeMatrix", "dgTMatrix", "dgCMatrix") %in% class(data) )) {
    data <- xgb.DMatrix(data=data, info = list(label=label))
  } else if(!is.null(label)) {
    setinfo(data, "label" , label)
  }

  lower_corner <- coalesce(lower_bounds, c(2L, .1,  5L, log(0.3), 0, .7  , log(.02)  , 1L) ) 
  upper_corner <- coalesce(upper_bounds,  c(6L, 1., 15L, log(200), 5, .95 , log(2000), 1L) )
  
  evaluation_points <- sobol(n = sobol_size, dim = length(lower_corner), scrambling=FALSE)
  eval_res <- matrix(NA, nrow=sobol_size, ncol=2L)
  
  fixed_params <- c(max_depth, eta, nrounds, min_child_weight, gamma, subsample, lambda, num_parallel_tree )
  nrounds <- as.integer(ifelse(is.na(nrounds), upper_corner[3] , nrounds))
  
  for(sobol_index in seq.int(sobol_size)) {
    transformed_params <- lower_corner + (upper_corner-lower_corner)*evaluation_points[ sobol_index,, drop=TRUE]
    transformed_params <- coalesce(fixed_params,  transformed_params)
    transformed_params[c(1,3,8)] <- round(transformed_params[c(1,3,8)])
    
    local.model <- xgb.cv(data=data, nrounds=nrounds, params=c(other_params, list(
    max_depth=transformed_params[1] 
    , eta=transformed_params[2]
    ,  min_child_weight=exp(transformed_params[4])
    , gamma=transformed_params[5]
    , subsample=transformed_params[6] 
    , lambda=exp(transformed_params[7])
    , num_parallel_tree=transformed_params[8] 
    , objective = objective
    , tree_method=tree_method
    , eval_metric="logloss"
    ) ), verbose=0L, nfold= nfold, predictor="cpu_predictor")
    ## this could be a one-liner in tidy/purrr, but this spares importing dplyr namespace
    get_this <- grep(local.model %>% extract2("evaluation_log") %>% colnames, pattern="test_logloss_mean")[1]
    eval_res[sobol_index,1] <- local.model %>% extract2("evaluation_log") %>% extract2(get_this)  %>% which.min 
    eval_res[sobol_index,2] <- local.model %>% extract2("evaluation_log") %>% extract2(get_this)  %>% min
    rm(local.model)
    gc()
  }
  
  transformed_params <- lower_corner + (upper_corner-lower_corner)*evaluation_points[ which.min(eval_res[,2]), drop=TRUE]
  transformed_params <- coalesce(fixed_params,  transformed_params)
  transformed_params[c(1,3,8)] <- round(transformed_params[c(1,3,8)])
  nrounds <- eval_res[which.min(eval_res[,2]),1]


    local.model <- xgb.train(data=data, params=c(other_params, list(
    max_depth=transformed_params[1] 
    , eta=transformed_params[2]
    ,  min_child_weight=exp(transformed_params[4])
    , gamma=transformed_params[5]
    , subsample=transformed_params[6] 
    , lambda=exp(transformed_params[7])
    , num_parallel_tree=transformed_params[8] 
    , objective = objective
    , tree_method=tree_method
    , eval_metric=eval_metric
    ) ) , nrounds=nrounds, verbose=0L,  watchlist=list(train=data), predictor="cpu_predictor")  
    

  return(list(transformed_params, eval_res, local.model))

}


test_model <- xgboost_cv(label=joined_mortality[["death_in_30"]], data=newcols %>% xgb.DMatrix , sobol_size=45, eval_metric="auc" )

writeLines(test_model[[1]] %>% as.character, "/research/intermediates/xgb_params.csv")

xgb.save(test_model[[3]] , fname='/research/intermediates/name_prediction.xgb' )

