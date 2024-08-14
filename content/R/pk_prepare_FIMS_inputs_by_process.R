

## define the dimensions and global variables


setup_fleets <- function(fleets,population, catchability, selectivity, data){
  flts <- fltindex <- fltacomp <- list()
  for(i in 1:length(fleets$names)){
    isfleet <- fleets$type[i] == 'landings'
    flts[[i]] <- methods::new(Fleet)
    flts[[i]]$nages <- length(population$ages)
    flts[[i]]$nyears <- population$nyears
    fltindex[[i]] <- methods::new(Index, nyears)
    fltacomp[[i]] <- methods::new(AgeComp, flts[[i]]$nyears, flts[[i]]$nages)
    fltdat <- filter(data@data, name==fleets$names[i])
    acomp <- FIMS::m_agecomp(age_frame, fleets$names[i])
    if(isfleet){
      flts[[i]]$is_survey <- FALSE
      flts[[i]]$log_Fmort <- log(fleets$init_Fmort[[i]])
      flts[[i]]$estimate_F <- fleets$estimate_F[i]
      flts[[i]]$random_F <- fleets$reFmort[i]
      fltindex[[i]]$index_data <- FIMS::m_landings(data)
      flts[[i]]$log_obs_error <- log(filter(fltdat, type=='landings')$uncertainty)
      fltacomp[[i]]$age_comp_data <- acomp * filter(fltdat, type=='age')$uncertainty
    } else {
      flts[[i]]$is_survey <- TRUE
      flts[[i]]$log_q <- catchability$init_pars[i]
      flts[[i]]$estimate_q <- is.na(catchability$fix_pars[i])
      flts[[i]]$random_q <- FALSE# catchability$re[i]!='none'
      fltindex[[i]]$index_data <- FIMS::m_index(data, fleets$names[i])
      flts[[i]]$log_obs_error <- log(filter(fltdat, type=='index')$uncertainty)
      fltacomp[[i]]$age_comp_data <- acomp * filter(fltdat, type=='age')$uncertainty
    }
    ## Set Index, AgeComp, and Selectivity using the IDs from the modules defined above
    flts[[i]]$SetObservedIndexData(fltindex[[i]]$get_id())
    flts[[i]]$SetObservedAgeCompData(fltacomp[[i]]$get_id())
    flts[[i]]$SetSelectivity(selectivity[[i]]$get_id())
  }
  message("Finished setting up fleets")
}

setup_selex <- function(selectivity){
  s <- selectivity
  sels <- list()
  for(i in 1:length(s$names)){
    if(s$form[i]=='DoubleLogistic'){
      sels[[i]] <- methods::new(DoubleLogisticSelectivity)

      sels[[i]]$inflection_point_asc$value <- s$init_pars[[i]][1]
      sels[[i]]$inflection_point_asc$is_random_effect <- FALSE ## s$re[1]!='none' & 1 %in% s$where[[i]]
      sels[[i]]$inflection_point_asc$estimated <- !(1 %in% s$fix_pars[[i]])

      sels[[i]]$slope_asc$value <- s$init_pars[[i]][2]
      sels[[i]]$slope_asc$is_random_effect <- FALSE## s$re[i]!='none' & 2 %in% s$where[[i]]
      sels[[i]]$slope_asc$estimated <- !(2 %in% s$fix_pars[[i]])

      sels[[i]]$inflection_point_desc$value <- s$init_pars[[i]][3]
      sels[[i]]$inflection_point_desc$is_random_effect <- FALSE#s$re[i]!='none' & 3 %in% s$where[[i]]
      sels[[i]]$inflection_point_desc$estimated <- !(3 %in% s$fix_pars[[i]])

      sels[[i]]$slope_desc$value <- s$init_pars[[i]][4]
      sels[[i]]$slope_desc$is_random_effect <- FALSE#s$re[i]!='none' & 4 %in% s$where[[i]]
      sels[[i]]$slope_desc$estimated <- !(4 %in% s$fix_pars[[i]])
    } else if(s$form[i]=='Logistic'){
      sels[[i]] <- methods::new(LogisticSelectivity)

      sels[[i]]$inflection_point$value <- s$init_pars[[i]][1]
      sels[[i]]$inflection_point$is_random_effect <- FALSE#s$re[i]!='none' & 1 %in% s$where[[i]]
      sels[[i]]$inflection_point$estimated <- !(1 %in% s$fix_pars[[i]])

      sels[[i]]$slope$value <- s$init_pars[[i]][2]
      sels[[i]]$slope$is_random_effect <- FALSE#s$re[i]!='none' & 2 %in% s$where[[i]]
      sels[[i]]$slope$estimated <- !(2 %in% s$fix_pars[[i]])
    }
  }
  message("Finished setting up selex")
  return(sels)
}
setup_recruitment <- function(recruitment){
  r <- recruitment
  rec <- methods::new(BevertonHoltRecruitment)
  ## methods::show(BevertonHoltRecruitment)
  rec$log_sigma_recruit$value <- log(r$init_pars[3])
  rec$log_rzero$value <- log(r$init_pars[1])
  rec$log_rzero$is_random_effect <- FALSE
  rec$log_rzero$estimated <- TRUE ## !(1 %in% r$fix_pars)
  steep <- r$init_pars[2]
  rec$logit_steep$value <- -log(1.0 - steep) + log(steep - 0.2)
  rec$logit_steep$is_random_effect <- FALSE
  rec$logit_steep$estimated <- FALSE ## !(2 %in% r$fix_pars)
  rec$estimate_log_devs <- TRUE
  rec$log_devs <-  r$init_recdevs
  message("Finished setting up recruitment")
  return(rec)
}
setup_growth <- function(growth){
  gr <- methods::new(EWAAgrowth)
  gr$ages <- ages
  gr$weights <- growth$waa
  message("Finished setting up growth")
  return(gr)
}
setup_maturity <- function(maturity){
  mat <- new(LogisticMaturity)
  mat$inflection_point$value <- maturity$init_pars[1]
  mat$inflection_point$is_random_effect <- FALSE
  mat$inflection_point$estimated <- !(1 %in% maturity$fix_pars)
  mat$slope$value <- maturity$init_pars[2]
  mat$slope$is_random_effect <- FALSE
  mat$slope$estimated <- !(2 %in% maturity$fix_pars)
  message("Finished setting up maturity")
  return(mat)
}
setup_population <- function(population, matout, growthout, recout){
  pop <- new(Population)
  pop$log_M <- log(population$M)
  pop$estimate_M <- FALSE
  pop$log_init_naa <- log(population$init_naa)
  pop$estimate_init_naa <- !population$fix_naa
  pop$nages <- population$nages
  pop$ages <- population$ages
  pop$nfleets <- population$nfleets
  pop$nseasons <- 1
  pop$nyears <- population$nyears
  ## pop$prop_female <- 1.0 # ASAP assumption
  pop$SetMaturity(matout$get_id())
  pop$SetGrowth(growthout$get_id())
  pop$SetRecruitment(recout$get_id())
  message("Finished setting up population and linking maturity, growth, recruitment")
}



