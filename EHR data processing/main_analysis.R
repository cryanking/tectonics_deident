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
"sandwich" ,
"lmtest" ,
"stringr" ,
"boot" ,
'matrixStats' , 
'scales' , 
"effectsize" ) , '2024-05-01')


boot_size <- 5000L

setwd("/research")
clarity_root <- '/early_act3/'
roughen_date <- function(x) round(year(x) + (month(x)-1)/12,2)  

na_false <- function(x) {fifelse(is.na(x), FALSE, x ) }

jama_p_format <- function(x) {case_when(
  is.na(x) ~NA_character_ ,
  x > .99~ ">0.99",
  round(x,2) >= 0.01 ~ str_pad(as.character(round(x,2)) , width=4, side="right", pad="0"),
  x < .001~"<0.001",
  x > 0.001 ~ str_pad(as.character(round(x,3)) , width=5, side="right", pad="0"),
  TRUE ~ as.character(x)
  )}
  
## these are only used in the bootstrap
generate_cl_perm <- function(local_data, boot_set ) {
  boot_set <- copy(boot_set) [, tx := sample(isTreatment) , by="SurgDate"][, isTreatment:=NULL]
  return(boot_set[ local_data, on=c("SurgDate", "OR_Abbrev") ][, isTreatment := tx] )
}
 
gee_main <- function(local_data, secondary_outcomes) {
  res_out <- rep(1,length(secondary_outcomes) )
  names(res_out) <- secondary_outcomes
  for( outcome in  secondary_outcomes ) {
    try({
    this_data <-  cbind(local_data[ , .SD[[outcome]]] , local_data[ , .(x=as.numeric(isTreatment),cluster_id )])
    colnames(this_data) <- c("y", "x", "z")
    this_data <- this_data[is.finite(y) & is.finite(x)]
    if(outcome %in%  cont_outcomes) {
      thislm <- glm( formula=y~x, data=this_data)
      V <- vcovCL(thislm, cluster = ~ z, cadjust = FALSE, type="HC1")
      thistest <- coeftest(thislm, V)
      res_out[outcome] <- (thistest['x',4])
    } else { 
      thislm <- glm( formula=y~x, family="poisson" ,data=this_data)
      V <- vcovCL(thislm, cluster = ~ z, cadjust = FALSE, type="HC1")
      thistest <- coeftest(thislm, V)

      res_out[outcome] <- (thistest['x',4])
    }
  }, silent=FALSE)
  }
  
  return(res_out)
}

gee_main_unclustered <- function(local_data, secondary_outcomes) {
  res_out <- rep(1,length(secondary_outcomes) )
  names(res_out) <- secondary_outcomes
  for( outcome in  secondary_outcomes ) {
    try({
    this_data <-  cbind(local_data[ , .SD[[outcome]]] , local_data[ , .(x=as.numeric(isTreatment),cluster_id )])
    colnames(this_data) <- c("y", "x", "z")
    this_data <- this_data[is.finite(y) & is.finite(x)]
    if(outcome %in%  cont_outcomes) {
      thislm <- glm( formula=y~x, data=this_data)
      thistest <- coeftest(thislm)
      res_out[outcome] <- (thistest['x',4])
    } else { 
      thislm <- glm( formula=y~x, family="poisson" ,data=this_data)
      thistest <- coeftest(thislm)

      res_out[outcome] <- (thistest['x',4])
    }
  }, silent=FALSE)
  }
  
  return(res_out)
}


gee_main_service <- function(local_data, secondary_outcomes) {
  res_out <- rep(1,length(secondary_outcomes) )
  names(res_out) <- secondary_outcomes
  for( outcome in  secondary_outcomes ) {
    try({
    this_data <-  cbind(local_data[ , .SD[[outcome]]] , local_data[ , .(x=as.numeric(isTreatment),cluster_id, Service )])
    colnames(this_data) <- c("y", "x", "z", "Service")
    this_data <- this_data[is.finite(y)]
    this_data <- this_data[is.finite(x)]
    this_data <- this_data[!is.na(Service)]

    if(outcome %in%  cont_outcomes) {
      thislm <- glm( formula=y~x*Service, data=this_data)
      thislm_parent<-glm( formula=y~x + Service, data=this_data)
      thistest <- waldtest(thislm_parent, thislm, vcov=function(x){vcovCL(x, cluster = ~ z, cadjust = FALSE, type="HC1")} )
      res_out[outcome] <- thistest[["Pr(>F)"]][2] 
    } else { 
      thislm <- glm( formula=y~x*Service, family="poisson" ,data=this_data)
      thislm_parent<-glm( formula=y~x + Service, family="poisson" ,data=this_data)
      thistest <- waldtest(thislm_parent, thislm, vcov=function(x){vcovCL(x, cluster = ~ z, cadjust = FALSE, type="HC1")} )

      res_out[outcome] <- thistest[["Pr(>F)"]][2] 
    }
  }, silent=FALSE)
  }
  
  return(res_out)
}

main_analysis <- function(local_data, primary_outcomes = c('CAM', 'RespF' ,'AKI' , 'EarlyDeath'), do_correct=TRUE ) {
  
  primarytab <- data.frame(outcome = c(primary_outcomes) ) 
  primarytab$control <- ""
  primarytab$interv <- ""
  primarytab$coef <- ""
  primarytab$p <- ""
  primarytab$coef2 <- ""
  primarytab$N_defined <- ""
  primarytab$coef_linear<- ""
  primarytab$coef_linear2<- ""
  ntest2 <- length(primary_outcomes)

  for( outcome in  primary_outcomes ) {
    this_data <-  cbind(local_data[ , .SD[[outcome]]] , local_data[ , .(x=as.numeric(isTreatment),cluster_id )])
    colnames(this_data) <- c("y", "x", "z")
    primarytab[primarytab$outcome ==outcome, "N_defined"] <- this_data[["y"]] %>% is.na %>% not %>% sum %>% as.character
    this_data <- this_data[is.finite(y)]
    this_data <- this_data[is.finite(x)]
    if(outcome == "ratio_cre") { exp_transform <- exp } else { exp_transform <- identity}
    if(outcome %in%  cont_outcomes) {
      primarytab[primarytab$outcome ==outcome, c("control", "interv") ] <- this_data[ , .(state=paste0(mean(y, na.rm=T) %>% exp_transform %>% round(2)  %>% formatC( format="f", digits=2 ) , " (", round(sd(y  %>% exp_transform, na.rm=TRUE),2) %>% formatC( format="f", digits=2 ), ")"  ) ) , by="x"][order(x)][["state"]]
      thislm <- glm( formula=y~x, data=this_data)
      V <- vcovCL(thislm, cluster = ~ z, cadjust = FALSE, type="HC1")
      thistest <- coeftest(thislm, V)
      primarytab[primarytab$outcome ==outcome, "coef"] <- paste0(thistest['x', 'Estimate']  %>% exp_transform  %>% round(2) %>% formatC(format='f', digits=2) , ' (' , coefci(thislm, vcov.=V, level = 1-.05/ntest2)["x",]   %>% exp_transform %>% round(2) %>% formatC(format='f', digits=2) %>%  paste0(collapse=", ") ,  ')' )
      primarytab[primarytab$outcome ==outcome, "coef2"] <- paste0(thistest['x', 'Estimate']   %>% exp_transform %>% round(2) %>% formatC(format='f', digits=2) , ' (' , coefci(thislm, vcov.=V, level = 1-.005/ntest2)["x",]  %>% exp_transform %>% round(2) %>% formatC(format='f', digits=2) %>%  paste0(collapse=", ") ,  ')' )

      primarytab[primarytab$outcome ==outcome, "p"] <- (thistest['x',4])  %>% jama_p_format
  } else {
      primarytab[primarytab$outcome ==outcome, c("control", "interv") ] <- this_data[ , .(state=paste0(sum(y, na.rm=T) , "/", sum(is.finite(y)) ," (", round(100*mean(y, na.rm=TRUE),1) %>% formatC( format="f", digits=1 ) , ")"  ) ) , by="x"][order(x)][["state"]]
      thislm <- glm( formula=y~x, family="poisson" ,data=this_data)
      V <- vcovCL(thislm, cluster = ~ z, cadjust = FALSE)
      thistest <- coeftest(thislm, V)
      primarytab[primarytab$outcome ==outcome, "coef"] <- paste0(thistest['x', 'Estimate'] %>% exp %>% round(2) %>% formatC(format='f', digits=2) , ' (' , coefci(thislm, vcov.=V, level = 1-.05/ntest2)["x",] %>% exp %>% round(2) %>% formatC(format='f', digits=2) %>%  paste0(collapse=", ") ,  ')' )
      primarytab[primarytab$outcome ==outcome, "coef2"] <- paste0(thistest['x', 'Estimate'] %>% exp %>% round(2) %>% formatC(format='f', digits=2) , ' (' , coefci(thislm, vcov.=V, level = 1-.005/ntest2)["x",] %>% exp %>% round(2) %>% formatC(format='f', digits=2) %>%  paste0(collapse=", ") ,  ')' )

      primarytab[primarytab$outcome ==outcome, "p"] <- (thistest['x',4]) %>% jama_p_format
      
      thislm <- glm( formula=y~x, data=this_data)
      V <- vcovCL(thislm, cluster = ~ z, cadjust = FALSE, type="HC1")      
      thistest <- coeftest(thislm, V)
      primarytab[primarytab$outcome ==outcome, "coef_linear"] <- paste0(thistest['x', 'Estimate']  %>% multiply_by(100)  %>% round(1) %>% formatC(format='f', digits=1) , ' (' , coefci(thislm, vcov.=V, level = 1-.05/ntest2)["x",]   %>% multiply_by(100) %>% round(1) %>% formatC(format='f', digits=1) %>%  paste0(collapse=", ") ,  ')' )
      primarytab[primarytab$outcome ==outcome, "coef_linear2"] <- paste0(thistest['x', 'Estimate']   %>% multiply_by(100) %>% round(1) %>% formatC(format='f', digits=1) , ' (' , coefci(thislm, vcov.=V, level = 1-.005/ntest2)["x",]  %>% multiply_by(100) %>% round(1) %>% formatC(format='f', digits=1) %>%  paste0(collapse=", ") ,  ')' )
      
  }

  }

  if(do_correct) {
    boot_fun <- function(x) gee_main(x, primary_outcomes)
    boot_set <- local_data[ , .(SurgDate, OR_Abbrev, isTreatment)] %>% unique %>% setkeyv(c("SurgDate", "OR_Abbrev") )  
    boot_out <- boot(parallel="multicore", ncpus=8, data=local_data, R=boot_size, sim="parametric", ran.gen=generate_cl_perm, statistic=boot_fun,  mle=boot_set )
    ## original sorted
    r_s <- (order(boot_out$t0) )
    max_jm <- sapply(seq(length(boot_out$t0)) , function(x) rowMins(boot_out$t, cols= r_s[seq(from=x, to=length(boot_out$t0))]) ) 
    c_alpha <- apply(max_jm, 2, quantile, 0.05)
    reject_output <- boot_out$t0[r_s] < (c_alpha %>% rev %>% cummin %>% rev)

    p_corrected <- sapply(seq(from=1, to=length(boot_out$t0)) , function(x) mean( boot_out$t0[r_s[x]] > max_jm[,x]  ) ) %>% cummax 
    p_corrected <- p_corrected[rank(boot_out$t0)] %>% set_names(names(boot_out$t0))

    primarytab$p_corrected <- p_corrected[primarytab$outcome] %>% jama_p_format
  }
  return(primarytab)
}

main_analysis_service <- function(local_data, primary_outcomes = c('CAM', 'RespF' ,'AKI' , 'EarlyDeath'), do_correct=TRUE ) {
  
  primarytab <- data.frame(outcome = c(primary_outcomes) ) 
  primarytab$F <- ""
  primarytab$p <- ""

  for( outcome in  primary_outcomes ) {
    this_data <-  cbind(local_data[ , .SD[[outcome]]] , local_data[ , .(x=as.numeric(isTreatment),cluster_id, Service )])
    colnames(this_data) <- c("y", "x", "z", "Service")
    this_data <- this_data[!is.na(Service)]
    this_data <- this_data[is.finite(y)]
    this_data <- this_data[is.finite(x)]
    
    if(outcome %in%  cont_outcomes) {
      thislm <- glm( formula=y~x*Service, data=this_data)
      thislm_parent<-glm( formula=y~x + Service, data=this_data)
      thistest <- waldtest(thislm_parent, thislm, vcov=function(x){vcovCL(x, cluster = ~ z, cadjust = FALSE, type="HC1")} )
      
    } else { 
      thislm <- glm( formula=y~x*Service, family="poisson" ,data=this_data)
      thislm_parent<-glm( formula=y~x + Service, family="poisson" ,data=this_data)
      thistest <- waldtest(thislm_parent, thislm, vcov=function(x){vcovCL(x, cluster = ~ z, cadjust = FALSE, type="HC1")} )
    }
      primarytab[primarytab$outcome ==outcome, "F"] <- thistest[["F"]][2] %>% round(2)
      primarytab[primarytab$outcome ==outcome, "p"] <- thistest[["Pr(>F)"]][2] %>% jama_p_format
  }

  if(do_correct) {
    boot_fun <- function(x) gee_main_service(x, primary_outcomes)
    boot_set <- local_data[ , .(SurgDate, OR_Abbrev, isTreatment)] %>% unique %>% setkey(SurgDate, OR_Abbrev)  
    boot_out <- boot(parallel="multicore", ncpus=8, data=local_data, R=boot_size, sim="parametric", ran.gen=generate_cl_perm, statistic=boot_fun,  mle=boot_set )
    ## original sorted
    r_s <- (order(boot_out$t0) )
    max_jm <- sapply(seq(length(boot_out$t0)) , function(x) rowMins(boot_out$t, cols= r_s[seq(from=x, to=length(boot_out$t0))]) ) 
    c_alpha <- apply(max_jm, 2, quantile, 0.05)
    reject_output <- boot_out$t0[r_s] < (c_alpha %>% rev %>% cummin %>% rev)

    p_corrected <- sapply(seq(from=1, to=length(boot_out$t0)) , function(x) mean( boot_out$t0[r_s[x]] > max_jm[,x]  ) ) %>% cummax 
    p_corrected <- p_corrected[rank(boot_out$t0)] %>% set_names(names(boot_out$t0))

    primarytab$p_corrected <- p_corrected[primarytab$outcome] %>% jama_p_format
  }
  return(primarytab)
}

outcome_plot_gee <- function( all_outcomes, outcome, prefix="" ) {
    this_data <-  cbind(all_outcomes[ , ..outcome] , all_outcomes[ , .(treatment,cluster_id,thisq )])
    colnames(this_data) <- c("y", "x", "cluster_id","thisq")
    this_data[ , qt_factor := fct_cross(as_factor(thisq), x )]
    this_test <- glm( formula=y~qt_factor+0, data=this_data)

    plot_data <- this_test %>% summary %>% extract2("coefficients") %>% as.data.frame %>% as_tibble(rownames="qt_fac")
    plot_data %<>% mutate(qt_fac = substring(qt_fac, 10, 100))
    plot_data %<>% mutate(tx = grepl(qt_fac, pattern="Treatment") ) %>% mutate( quart=str_replace(qt_fac, pattern=":.*", replacement="") %>% as.numeric )
    V <- vcovCL(this_test, cluster = ~ cluster_id, cadjust = FALSE)
    plot_data$se <- sqrt(diag(V))
    plot_data %<>% mutate(lower= (Estimate -2*se ) %>% if_else(!is.finite(.), NA_real_ , . ))
    plot_data %<>% mutate(upper= (Estimate +2*se ) %>% if_else(!is.finite(.), NA_real_ , . ))
    ylim <- c( min(plot_data$lower, na.rm=T), max(plot_data$upper, na.rm=T) )
    if(max(plot_data$Estimate, na.rm=T) < 1) ylim <- pmax(ylim,0) %>% pmin(1)
    if(any(!is.finite(ylim))) ylim <- c(min(plot_data$Estimate), max(plot_data$Estimate))
    pretty_names <- c(EarlyDeath="30-d Mortality", RespF="Respiratory Failure", CAM="Delirium")
    print_name <- paste0(prefix, if_else(outcome %in%  names(pretty_names), pretty_names[outcome], outcome ) )
    
    plot_data %<>% mutate(quart = floor(quart) + (((quart%%1) * 10) -1)*.25 )
    with(plot_data , plot(quart + (1-as.integer(tx))*.05, Estimate, ylim=ylim,  type="p" , col=as.integer(tx)+1 , xlab="Year", pch=19, main=print_name, ylab="Rate", axes=F, xlim=c(2019, 2023.1) ) )
    with(plot_data , arrows( x0=quart + (1-as.integer(tx))*.05, x1=quart + (1-as.integer(tx))*.05, y0=lower, y1=upper, length=0, col=as.integer(tx)+1) )
#     abline(v=2018.375, lty=3)
#     axis(1, at=c(2017.5, 2018,2018.5, 2019), labels=c("","2018","","2019"))
    axis(2)
    axis(1)
#     legend( x="topleft", legend=c("Intervention", "Control"), col=c(1,0) +1 , pch = 19)
    return(plot_data)
}


main_analysis_unclustered <- function(local_data, primary_outcomes = c('CAM', 'RespF' ,'AKI' , 'EarlyDeath'), do_correct=TRUE ) {
  
  primarytab <- data.frame(outcome = c(primary_outcomes) ) 
  primarytab$control <- ""
  primarytab$interv <- ""
  primarytab$coef <- ""
  primarytab$p <- ""
  primarytab$coef2 <- ""
  primarytab$N_defined <- ""
  primarytab$coef_linear<- ""
  primarytab$coef_linear2<- ""
  ntest2 <- length(primary_outcomes)

  for( outcome in  primary_outcomes ) {
    this_data <-  cbind(local_data[ , .SD[[outcome]]] , local_data[ , .(x=as.numeric(isTreatment),cluster_id )])
    colnames(this_data) <- c("y", "x", "z")
    primarytab[primarytab$outcome ==outcome, "N_defined"] <- this_data[["y"]] %>% is.na %>% not %>% sum %>% as.character
    this_data <- this_data[is.finite(y)]
    this_data <- this_data[is.finite(x)]
    if(outcome == "ratio_cre") { exp_transform <- exp } else { exp_transform <- identity}
    if(outcome %in%  cont_outcomes) {
      primarytab[primarytab$outcome ==outcome, c("control", "interv") ] <- this_data[ , .(state=paste0(mean(y, na.rm=T) %>% exp_transform %>% round(2)%>% formatC( format="f", digits=2 ) , " (", round(sd(y  %>% exp_transform, na.rm=TRUE),2) %>% formatC( format="f", digits=2 ), ")" , " (N=", .N, ")" ) ) , by="x"][order(x)][["state"]]
      thislm <- glm( formula=y~x, data=this_data)
#       V <- vcovCL(thislm, cluster = ~ z, cadjust = FALSE, type="HC1")
      thistest <- coeftest(thislm)
      primarytab[primarytab$outcome ==outcome, "coef"] <- paste0(thistest['x', 'Estimate']  %>% exp_transform  %>% round(2) %>% formatC(format='f', digits=2) , ' (' , coefci(thislm, level = 1-.05/ntest2)["x",]   %>% exp_transform %>% round(2) %>% formatC(format='f', digits=2) %>%  paste0(collapse=", ") ,  ')' )
      primarytab[primarytab$outcome ==outcome, "coef2"] <- paste0(thistest['x', 'Estimate']   %>% exp_transform %>% round(2) %>% formatC(format='f', digits=2) , ' (' , coefci(thislm,  level = 1-.005/ntest2)["x",]  %>% exp_transform %>% round(2) %>% formatC(format='f', digits=2) %>%  paste0(collapse=", ") ,  ')' )

      primarytab[primarytab$outcome ==outcome, "p"] <- (thistest['x',4]) %>% jama_p_format
  } else {
      primarytab[primarytab$outcome ==outcome, c("control", "interv") ] <- this_data[ , .(state=paste0(sum(y, na.rm=T) , "/", sum(is.finite(y)) ," (", round(100*mean(y, na.rm=TRUE),1) %>% formatC( format="f", digits=1 ), ")"  ) ) , by="x"][order(x)][["state"]]
      thislm <- glm( formula=y~x, family="poisson" ,data=this_data)
#       V <- vcovCL(thislm, cluster = ~ z, cadjust = FALSE)
      thistest <- coeftest(thislm)
      primarytab[primarytab$outcome ==outcome, "coef"] <- paste0(thistest['x', 'Estimate'] %>% exp %>% round(2) %>% formatC(format='f', digits=2) , ' (' , coefci(thislm,  level = 1-.05/ntest2)["x",] %>% exp %>% round(2) %>% formatC(format='f', digits=2) %>%  paste0(collapse=", ") ,  ')' )
      primarytab[primarytab$outcome ==outcome, "coef2"] <- paste0(thistest['x', 'Estimate'] %>% exp %>% round(2) %>% formatC(format='f', digits=2) , ' (' , coefci(thislm,  level = 1-.005/ntest2)["x",] %>% exp %>% round(2) %>% formatC(format='f', digits=2) %>%  paste0(collapse=", ") ,  ')' )

      primarytab[primarytab$outcome ==outcome, "p"] <- (thistest['x',4]) %>% jama_p_format
      thislm <- glm( formula=y~x, data=this_data)
      thistest <- coeftest(thislm)
      primarytab[primarytab$outcome ==outcome, "coef_linear"] <- paste0(thistest['x', 'Estimate']  %>% multiply_by(100)  %>% round(1) %>% formatC(format='f', digits=1) , ' (' , coefci(thislm,  level = 1-.05/ntest2)["x",]   %>% multiply_by(100) %>% round(1) %>% formatC(format='f', digits=1) %>%  paste0(collapse=", ") ,  ')' )
      primarytab[primarytab$outcome ==outcome, "coef_linear2"] <- paste0(thistest['x', 'Estimate']   %>% multiply_by(100) %>% round(1) %>% formatC(format='f', digits=1) , ' (' , coefci(thislm,  level = 1-.005/ntest2)["x",]  %>% multiply_by(100) %>% round(1) %>% formatC(format='f', digits=1) %>%  paste0(collapse=", ") ,  ')' )

  }

  }

  if(do_correct) {
    boot_fun <- function(x) gee_main_unclustered(x, primary_outcomes)
    boot_set <- local_data[ , .(SurgDate, OR_Abbrev, isTreatment)] %>% unique %>% setkeyv(c("SurgDate", "OR_Abbrev") )  
    boot_out <- boot(parallel="multicore", ncpus=8, data=local_data, R=boot_size, sim="parametric", ran.gen=generate_cl_perm, statistic=boot_fun,  mle=boot_set )
    ## original sorted
    r_s <- (order(boot_out$t0) )
    max_jm <- sapply(seq(length(boot_out$t0)) , function(x) rowMins(boot_out$t, cols= r_s[seq(from=x, to=length(boot_out$t0))]) ) 
    c_alpha <- apply(max_jm, 2, quantile, 0.05)
    reject_output <- boot_out$t0[r_s] < (c_alpha %>% rev %>% cummin %>% rev)

    p_corrected <- sapply(seq(from=1, to=length(boot_out$t0)) , function(x) mean( boot_out$t0[r_s[x]] > max_jm[,x]  ) ) %>% cummax 
    p_corrected <- p_corrected[rank(boot_out$t0)] %>% set_names(names(boot_out$t0))

    primarytab$p_corrected <- p_corrected[primarytab$outcome] %>% jama_p_format
  }
  return(primarytab)
}

main_analysis_uncorrected <- function(local_data, primary_outcomes = c('CAM', 'RespF' ,'AKI' , 'EarlyDeath') ) {
  
  primarytab <- data.frame(outcome = c(primary_outcomes) ) 
  primarytab$control <- ""
  primarytab$interv <- ""
  primarytab$coef <- ""
  primarytab$p <- ""
  primarytab$coef2 <- ""
  primarytab$N_defined <- ""
  primarytab$coef_linear<- ""
  primarytab$coef_linear2<- ""
  ntest2 <- 1

  for( outcome in  primary_outcomes ) {
    this_data <-  cbind(local_data[ , .SD[[outcome]]] , local_data[ , .(x=as.numeric(isTreatment),cluster_id )])
    colnames(this_data) <- c("y", "x", "z")
    primarytab[primarytab$outcome ==outcome, "N_defined"] <- this_data[["y"]] %>% is.na %>% not %>% sum %>% as.character
    this_data <- this_data[is.finite(y)]
    this_data <- this_data[is.finite(x)]
    if(outcome == "ratio_cre") { exp_transform <- exp } else { exp_transform <- identity}
    if(outcome %in%  cont_outcomes) {
      primarytab[primarytab$outcome ==outcome, c("control", "interv") ] <- this_data[ , .(state=paste0(mean(y, na.rm=T) %>% exp_transform %>% round(2)%>% formatC( format="f", digits=2 ) , " (", round(sd(y  %>% exp_transform, na.rm=TRUE),2) %>% formatC( format="f", digits=2 ), ")"  ) ) , by="x"][order(x)][["state"]]
      thislm <- glm( formula=y~x, data=this_data)
      V <- vcovCL(thislm, cluster = ~ z, cadjust = FALSE, type="HC1")
      thistest <- coeftest(thislm, V)
      primarytab[primarytab$outcome ==outcome, "coef"] <- paste0(thistest['x', 'Estimate']  %>% exp_transform  %>% round(2) %>% formatC(format='f', digits=2) , ' (' , coefci(thislm, vcov.=V, level = 1-.05/ntest2)["x",]   %>% exp_transform %>% round(2) %>% formatC(format='f', digits=2) %>%  paste0(collapse=", ") ,  ')' )
      primarytab[primarytab$outcome ==outcome, "coef2"] <- paste0(thistest['x', 'Estimate']   %>% exp_transform %>% round(2) %>% formatC(format='f', digits=2) , ' (' , coefci(thislm, vcov.=V, level = 1-.005/ntest2)["x",]  %>% exp_transform %>% round(2) %>% formatC(format='f', digits=2) %>%  paste0(collapse=", ") ,  ')' )

      primarytab[primarytab$outcome ==outcome, "p"] <- (thistest['x',4])  %>% jama_p_format
  } else {
      primarytab[primarytab$outcome ==outcome, c("control", "interv") ] <- this_data[ , .(state=paste0(sum(y, na.rm=T) , "/", sum(is.finite(y)) ," (", round(100*mean(y, na.rm=TRUE),1) %>% formatC( format="f", digits=1 ), ")"  ) ) , by="x"][order(x)][["state"]]
      thislm <- glm( formula=y~x, family="poisson" ,data=this_data)
      V <- vcovCL(thislm, cluster = ~ z, cadjust = FALSE)
      thistest <- coeftest(thislm, V)
      primarytab[primarytab$outcome ==outcome, "coef"] <- paste0(thistest['x', 'Estimate'] %>% exp %>% round(2) %>% formatC(format='f', digits=2) , ' (' , coefci(thislm, vcov.=V, level = 1-.05/ntest2)["x",] %>% exp %>% round(2) %>% formatC(format='f', digits=2) %>%  paste0(collapse=", ") ,  ')' )
      primarytab[primarytab$outcome ==outcome, "coef2"] <- paste0(thistest['x', 'Estimate'] %>% exp %>% round(2) %>% formatC(format='f', digits=2) , ' (' , coefci(thislm, vcov.=V, level = 1-.005/ntest2)["x",] %>% exp %>% round(2) %>% formatC(format='f', digits=2) %>%  paste0(collapse=", ") ,  ')' )

      primarytab[primarytab$outcome ==outcome, "p"] <- (thistest['x',4]) %>% jama_p_format
      
      thislm <- glm( formula=y~x, data=this_data)
      V <- vcovCL(thislm, cluster = ~ z, cadjust = FALSE, type="HC1")      
      thistest <- coeftest(thislm, V)
      primarytab[primarytab$outcome ==outcome, "coef_linear"] <- paste0(thistest['x', 'Estimate']  %>% multiply_by(100)  %>% round(1) %>% formatC(format='f', digits=1) , ' (' , coefci(thislm, vcov.=V, level = 1-.05/ntest2)["x",]   %>% multiply_by(100) %>% round(1) %>% formatC(format='f', digits=1) %>%  paste0(collapse=", ") ,  ')' )
      primarytab[primarytab$outcome ==outcome, "coef_linear2"] <- paste0(thistest['x', 'Estimate']   %>% multiply_by(100) %>% round(1) %>% formatC(format='f', digits=1) , ' (' , coefci(thislm, vcov.=V, level = 1-.005/ntest2)["x",]  %>% multiply_by(100) %>% round(1) %>% formatC(format='f', digits=1) %>%  paste0(collapse=", ") ,  ')' )
      
      }

  }


  return(primarytab)
}


cont_outcomes <- c('hypotension_fraction' , 'eff_flow' , "pip_mean", "low_map_auc" ,  "pip_good_frac", "ICULoS", "mean_temp","ratio_cre", "postop_los", "sumOuts")

processed_data_set <- fread("/research/outputs/deidentified_final_data.csv")
if(file.exists("/research/outputs/reidentify_data.csv") ) {
  reident <- fread("/research/outputs/reidentify_data.csv")
  processed_data_set <- reident[, .(orlogid_encoded, thisq= quarter(SurgDate, with_year=T),early_period = SurgDate < ymd("2020-05-13") ,SurgicalRecordID )][processed_data_set, on="orlogid_encoded"]

}

setnames(processed_data_set, 'SurgDate_encoded', 'SurgDate')
setnames(processed_data_set, 'OR_Abbrev_encoded', 'OR_Abbrev')
setnames(processed_data_set, 'MRN_encoded', 'MRN')
setnames(processed_data_set, 'cluster_encoded', 'cluster_id')
setnames(processed_data_set, 'SERVICE_collapsed', 'Service')

processed_data_set[, treatment := fcase( isTreatment==0, "Control", isTreatment==1, "Treatment" )  ]

processed_data_set[ , sumOuts := na_false(CAM) + na_false(as.logical(RespF) ) + na_false(as.logical(AKI) ) + na_false(EarlyDeath) ]

processed_data_set[ , CAM2 := na_false(CAM) ]
processed_data_set[ , first_glucose_restr := fifelse(Diabetes==FALSE, NA, first_glucose)  ]
processed_data_set[ , eff_flow_binary := eff_flow > .9]

fix_table_format <- . %>% 
  mutate(outcome = outcome %>% case_match("CAM"~"Delirium", "RespF"~"Respiratory Failure", "AKI"~"AKI", "EarlyDeath"~"30-day mortality", "sumOuts"~"Composite", "Abx_success"~"Antibiotic redosing", "temp_good"~"Normothermia","hypotension_fraction"~"Fraction time hypotensive" , "pip_good_frac"~"Fraction time low PIP", "first_glucose"~ "Hyperglycemia" , "no_gaps"~"Anesthetic delivery", "eff_flow_binary" ~ "Efficent gas flow > 90%" ,"eff_flow"~"Fraction time efficient gas flow", "ICU"~"ICU utilization", "postop_los"~"Length of stay", "ICULoS"~"ICU length of stay", "mean_temp"~"Mean temperature" , "low_map_auc"~"Time weighted hypotension", "ratio_cre"~"Change in creatinine" , "pip_mean"~"Mean PIP", "CAM2"~"Delirium (missing as false)" ,  .default=outcome) ) %>%
  mutate(statement = paste0("[ACT ", interv %>% sub(pattern=")", replacement="%)", fixed=T), " vs control ", control %>% sub(pattern=")", replacement="%)", fixed=T), ", RD ", coef_linear2 %>% sub(pattern="(", replacement="(95% CI ", fixed=T ) %>% sub(pattern=",", replacement=" to") ,  ", RR ", coef    %>% sub(pattern="(", replacement="(95% CI ", fixed=T ) %>% sub(pattern=",", replacement=" to"), ", p=", p_corrected  , "]") ) %>%
  select(Outcome=outcome, Intervention=interv, Control=control, `GEE coef (95% CI)`=coef, `p value`=p_corrected, N=N_defined,  `GEE coef (99.5% CI)` = coef2, `uncorrected p` = p,  everything()  ) %>% mutate_all(as.character)

fix_table_format_exp <- . %>% 
  mutate(outcome = outcome %>% case_match("CAM"~"Delirium", "RespF"~"Respiratory Failure", "AKI"~"AKI", "EarlyDeath"~"30-day mortality", "sumOuts"~"Composite", "Abx_success"~"Antibiotic redosing", "temp_good"~"Normothermia","hypotension_fraction"~"Fraction time hypotensive" , "pip_good_frac"~"Fraction time low PIP", "first_glucose"~ "Hyperglycemia" , "no_gaps"~"Anesthetic delivery", "eff_flow_binary" ~ "Efficent gas flow > 90%" , "eff_flow"~"Fraction time efficient gas flow", "ICU"~"ICU utilization", "postop_los"~"Length of stay", "ICULoS"~"ICU length of stay", "mean_temp"~"Mean temperature" , "low_map_auc"~"Time weighted hypotension", "ratio_cre"~"Change in creatinine" , "pip_mean"~"Mean PIP", "CAM2"~"Delirium (missing as false)" ,  .default=outcome) ) %>% 
  select(Outcome=outcome,  Intervention=interv, Control=control, `GEE coef`=coef, `p value`=p, N=N_defined,  `GEE coef (99.5% CI)` = coef2, `uncorrected p` = p,  everything() ) %>% mutate_all(as.character)


  
####
## main analysis: exclude repeats within 30 days
####

# secondary_variables <- c("sumOuts" , 'Abx_success', 'temp_good','hypotension_fraction' , "pip_good_frac" , 'first_glucose' , 'no_gaps' , 'eff_flow' , 'ICU', 'postop_los')
secondary_variables <- c( 'Abx_success', 'temp_good','hypotension_fraction' , "pip_good_frac" , 'first_glucose' , 'no_gaps' , 'eff_flow_binary' )
alt_spec_Variables <- c("sumOuts", 'ICULoS'  , 'mean_temp' ,'low_map_auc' , 'ratio_cre' , 'pip_mean' ,  "CAM2" , "first_glucose_restr", 'eff_flow')
# setorder(primary_data_set, MRN, SurgDate)

primary_data_set <- processed_data_set[window_start == TRUE]
setkey(primary_data_set, SurgDate, OR_Abbrev)



primarytab <- main_analysis(local_data=primary_data_set)

if( "thisq" %in% colnames(primary_data_set)) {
png("/research/outputs/main_time_plot.png" , width=6, height=6, res=300, units="in")
par(mfrow=c(2,2))
outcome_plot_gee( primary_data_set, "EarlyDeath")
outcome_plot_gee( primary_data_set, "CAM")
outcome_plot_gee( primary_data_set, "RespF")
outcome_plot_gee( primary_data_set, "AKI")
dev.off()
}

exploratory_tab <- main_analysis(local_data=primary_data_set, primary_outcomes=alt_spec_Variables, do_correct=FALSE)

secondary_tab <- main_analysis(local_data=primary_data_set, primary_outcomes=secondary_variables )

bind_rows(primarytab , secondary_tab) %>% fix_table_format %>% fwrite("/research/outputs/main_results_no_repeats.csv")

exploratory_tab%>% fix_table_format_exp %>% fwrite("/research/outputs/main_results_exploratory.csv")


primarytab_uncor <- main_analysis_uncorrected(local_data=primary_data_set, primary_outcomes=c(c('CAM', 'RespF' ,'AKI' , 'EarlyDeath') , secondary_variables ) )
primarytab_uncor[["p_corrected"]] <- 0.

primarytab_uncor  %>% fix_table_format %>% fwrite("/research/outputs/main_results_uncorrected.csv")

####
## subset: ASA >= 3
##

primarytab_asa <- main_analysis(local_data=primary_data_set[ASA_high==TRUE])

secondary_tab_asa <- main_analysis(local_data=primary_data_set[ASA_high==TRUE], primary_outcomes=secondary_variables)

bind_rows(primarytab_asa , secondary_tab_asa) %>% fix_table_format %>% fwrite("/research/outputs/main_results_ASA3.csv")

####
## subset: Age >= 65    
## 

primarytab_age <- main_analysis(local_data=primary_data_set[Age_high == TRUE])

secondary_tab_age <- main_analysis(local_data=primary_data_set[Age_high == TRUE ], primary_outcomes=secondary_variables)

bind_rows(primarytab_age , secondary_tab_age) %>% fix_table_format %>% fwrite("/research/outputs/main_results_Age65.csv")

####
## Subset: high risk
## 

primarytab_risk <- main_analysis(local_data=primary_data_set[predicted_risk_high==TRUE] )

secondary_tab_risk <- main_analysis(local_data=primary_data_set[predicted_risk_high==TRUE], primary_outcomes=secondary_variables)

bind_rows(primarytab_risk , secondary_tab_risk) %>% fix_table_format %>% fwrite("/research/outputs/main_results_highrisk.csv")


####
## Subset: 50% overlap
## 


primarytab_overlap <- main_analysis(local_data=primary_data_set[overlap_high==TRUE])

secondary_tab_overlap <- main_analysis(local_data=primary_data_set[overlap_high==TRUE], primary_outcomes=secondary_variables)

bind_rows(primarytab_overlap , secondary_tab_overlap) %>% fix_table_format %>% fwrite("/research/outputs/main_results_highoverlap.csv")


####
## Subset: emergency
## 


primarytab_e<- main_analysis(local_data=primary_data_set[emergency==TRUE])

secondary_tab_e <- main_analysis(local_data=primary_data_set[emergency==TRUE], primary_outcomes=secondary_variables)

bind_rows(primarytab_e , secondary_tab_e) %>% fix_table_format %>% fwrite("/research/outputs/main_results_e.csv")

####
## Subset: non-emergency
## 


primarytab_ne<- main_analysis(local_data=primary_data_set[emergency==FALSE])

secondary_tab_ne <- main_analysis(local_data=primary_data_set[emergency==FALSE], primary_outcomes=secondary_variables)

bind_rows(primarytab_ne , secondary_tab_ne) %>% fix_table_format %>% fwrite("/research/outputs/main_results_ne.csv")



####
## per-protocol like subset
## 
protocol_tab <- main_analysis(local_data=primary_data_set[is_matched==TRUE])
protocol_secondary_tab <- main_analysis(local_data=primary_data_set[is_matched==TRUE], primary_outcomes=secondary_variables )

protocol_tab2 <- main_analysis(local_data=primary_data_set[combined_match==TRUE])
protocol_secondary_tab2 <- main_analysis(local_data=primary_data_set[combined_match==TRUE], primary_outcomes=secondary_variables )

 
bind_rows(protocol_tab , protocol_secondary_tab)%>% fix_table_format %>% fwrite("/research/outputs/propensity_review.csv")
bind_rows(protocol_tab2 , protocol_secondary_tab2) %>% fix_table_format %>% fwrite("/research/outputs/propensity_alertcombined.csv")


####
## Secondary analysis: interaction term with Service
## 

primarytab_service <- main_analysis_service(local_data=primary_data_set)

secondary_tab_service <- main_analysis_service(local_data=primary_data_set, primary_outcomes=secondary_variables)

bind_rows(primarytab_service , secondary_tab_service) %>% 
  mutate(outcome = outcome %>% case_match("CAM"~"Delirium", "RespF"~"Respiratory Failure", "AKI"~"AKI", "EarlyDeath"~"30-day mortality", "sumOuts"~"Composite", "Abx_success"~"Antibiotic redosing", "temp_good"~"Normothermia","hypotension_fraction"~"Fraction time hypotensive" , "pip_good_frac"~"Fraction time low PIP", "first_glucose"~ "Hyperglycemia" , "no_gaps"~"Anesthetic delivery", "eff_flow"~"Fraction time efficient gas flow", "eff_flow_binary" ~ "Efficent gas flow > 90%" , "ICU"~"ICU utilization", "postop_los"~"Length of stay", "ICULoS"~"ICU length of stay", "mean_temp"~"Mean temperature" , "low_map_auc"~"Time weighted hypotension", "ratio_cre"~"Change in creatinine" , "pip_mean"~"Mean PIP", "CAM2"~"Delirium (missing as false)" ,  .default=outcome) ) %>% 
  select(Outcome=outcome, F, `p value`=p_corrected, everything() ) %>% mutate_all(as.character) %>% 
  fwrite("/research/outputs/main_results_service_Anova.csv")

## tantalizing difference (uncorrected p==0.055) in early death. Which service drives it?
## It turns out the default refernce group (cardiothoracic) has a positive lOR of 0.29! 

#     this_data <-  cbind(primary_data_set[ , .(EarlyDeath,x=as.numeric(isTreatment),cluster_id, Service )])
#     colnames(this_data) <- c("y", "x", "z", "Service")
#     this_data <- this_data[!is.na(Service)]
#     this_data <- this_data[is.finite(y)]
#     this_data <- this_data[is.finite(x)]
#     
#       thislm <- glm( formula=y~x, family="poisson" ,data=this_data[Service=="Cardiothoracic"])
#       thislm_parent<-glm( formula=y~x + Service, family="poisson" ,data=this_data)
#       thistest <- waldtest(thislm_parent, thislm, vcov=function(x){vcovCL(x, cluster = ~ z, cadjust = FALSE, type="HC1")} )


####
## Secondary analysis: include repeats
## 


primarytab_all <- main_analysis(local_data=processed_data_set)

secondary_tab_all <- main_analysis(local_data=processed_data_set, primary_outcomes=secondary_variables)

bind_rows(primarytab_all , secondary_tab_all) %>% fix_table_format %>% fwrite("/research/outputs/main_results_with_repeats.csv")


#####
## sensitivity analysis: unclustered


primarytab_uncl <- main_analysis_unclustered(local_data=processed_data_set)

secondary_tab_uncl <- main_analysis_unclustered(local_data=processed_data_set, primary_outcomes=secondary_variables)

bind_rows(primarytab_uncl , secondary_tab_uncl) %>% fix_table_format %>% fwrite("/research/outputs/main_results_unclustered.csv")


## for ct.gov, not part of analysis plan
processed_data_set[ , sumOuts2 := sumOuts > 0 ]

primarytab_all_ct <- main_analysis_unclustered(local_data=processed_data_set, primary_outcomes="sumOuts2")

primarytab_all_ct %>% fix_table_format %>% fwrite("/research/outputs/main_results_any.csv")
