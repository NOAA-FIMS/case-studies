

## define the dimensions and global variables


setup_fleets <- function(fleets,population, catchability, selectivity, data){
  flts <- list()
  for(i in 1:length(fleets$names)){
    isfleet <- fleets$type[i] == 'landings'
    flts[[i]] <- methods::new(Fleet)
    flts[[i]]$nages <- length(population$ages)
    flts[[i]]$nyears <- population$nyears
    fltindex <- methods::new(Index, nyears)
    fltacomp <- methods::new(AgeComp, flts[[i]]$nyears, flts[[i]]$nages)
    fltdat <- filter(data@data, name==fleets$names[i])
    acomp <- FIMS::m_agecomp(age_frame, fleets$names[i])
    if(isfleet){
      flts[[i]]$is_survey <- FALSE
      flts[[i]]$log_Fmort <- log(fleets$init_Fmort[[i]])
      flts[[i]]$estimate_F <- fleets$estimate_F[i]
      flts[[i]]$random_F <- fleets$reFmort[i]
      fltindex$index_data <- FIMS::m_landings(data)
      flts[[i]]$log_obs_error <- log(filter(fltdat, type=='landings')$uncertainty)
      fltacomp$age_comp_data <- acomp * filter(fltdat, type=='age')$uncertainty
    } else {
      flts[[i]]$is_survey <- TRUE
      flts[[i]]$log_q <- catchability$init_pars[i]
      flts[[i]]$estimate_q <- is.na(catchability$fix_pars[i])
      flts[[i]]$random_q <- catchability$re[i]!='none'
      fltindex$index_data <- FIMS::m_index(data, fleets$names[i])
      flts[[i]]$log_obs_error <- log(filter(fltdat, type=='index')$uncertainty)
      fltacomp$age_comp_data <- acomp * filter(fltdat, type=='age')$uncertainty
    }
    ## Set Index, AgeComp, and Selectivity using the IDs from the modules defined above
    flts[[i]]$SetObservedIndexData(fltindex$get_id())
    flts[[i]]$SetObservedAgeCompData(fltacomp$get_id())
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
      sels[[i]]$inflection_point_asc$is_random_effect <- s$re[1]!='none' & 1 %in% s$where[[i]]
      sels[[i]]$inflection_point_asc$estimated <- !(1 %in% s$fix_pars[[i]])

      sels[[i]]$slope_asc$value <- s$init_pars[[i]][2]
      sels[[i]]$slope_asc$is_random_effect <- s$re[1]!='none' & 2 %in% s$where[[i]]
      sels[[i]]$slope_asc$estimated <- !(2 %in% s$fix_pars[[i]])

      sels[[i]]$inflection_point_desc$value <- s$init_pars[[i]][3]
      sels[[i]]$inflection_point_desc$is_random_effect <- s$re[1]!='none' & 3 %in% s$where[[i]]
      sels[[i]]$inflection_point_desc$estimated <- !(1 %in% s$fix_pars[[i]])

      sels[[i]]$slope_desc$value <- s$init_pars[[i]][4]
      sels[[i]]$slope_desc$is_random_effect <- s$re[1]!='none' & 4 %in% s$where[[i]]
      sels[[i]]$slope_desc$estimated <- !(4 %in% s$fix_pars[[i]])
    } else if(s$form[i]=='Logistic'){
      sels[[i]] <- methods::new(LogisticSelectivity)

      sels[[i]]$inflection_point$value <- s$init_pars[[i]][1]
      sels[[i]]$inflection_point$is_random_effect <- s$re[1]!='none' & 1 %in% s$where[[i]]
      sels[[i]]$inflection_point$estimated <- !(1 %in% s$fix_pars[[i]])

      sels[[i]]$slope$value <- s$init_pars[[i]][2]
      sels[[i]]$slope$is_random_effect <- s$re[1]!='none' & 2 %in% s$where[[i]]
      sels[[i]]$slope$estimated <- !(2 %in% s$fix_pars[[i]])
    }
  }
  message("Finished setting up selex")
  return(sels)
}
setup_recruitment <- function(recruitment){
  rec <- recruitment
  rec <- methods::new(BevertonHoltRecruitment)
  ## methods::show(BevertonHoltRecruitment)
  rec$log_sigma_recruit$value <- log(recruitment$init_pars[3])
  rec$log_rzero$value <- log(recruitment$init_pars[1])
  rec$log_rzero$is_random_effect <- FALSE
  rec$log_rzero$estimated <- (1 %in% recruitment$fix_pars)
  steep <- recruitment$init_pars[2]
  rec$logit_steep$value <- -log(1.0 - steep) + log(steep - 0.2)
  rec$logit_steep$is_random_effect <- FALSE
  rec$logit_steep$estimated <- (2 %in% recruitment$fix_pars)
  rec$estimate_log_devs <- estimate_recdevs
  rec$log_devs <-  recruitment$init_recdevs
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
  pop$estimate_init_naa <- population$fix_naa
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

## Order of parameters for DL is asc inf, asc slop, desc inf,
## desc slope
mle <- parfinal
data <- age_frame
myNA <- rep(NA,3)
population <- list(ages=ages, nfleets=4, nyears=nyears,
                   years=years, nages=length(ages),
                   M=as.numeric(t(matrix(rep(pkfitfinal$rep$M, each=nyears), nrow=nyears))),
                   init_naa= exp(c(log(pkfitfinal$rep$recruit[1]), log(pkfitfinal$rep$initN)) + log(1e9)),
                   fix_naa=TRUE)
fleets <- list(names=c('fleet1', 'survey2', 'survey3', 'survey6'),
               type=c('landings', rep('index',3)),
               estimate_Fmort=c(TRUE, myNA),
               init_Fmort=list(pkfitfinal$rep$F, myNA),
               reFmort=c(FALSE, myNA))
sel <- list(names=fleets$names,
              form=c('DoubleLogistic', 'DoubleLogistic', 'Logistic', 'DoubleLogistic'),
              fix_pars=list(NA, 3:4, NA, 1:2),
              re=rep('none',4),
              init_pars=list(c(mle$inf1_fsh_mean, exp(mle$log_slp1_fsh_mean), mle$inf2_fsh_mean, exp(mle$log_slp2_fsh_mean)),
                             c(mle$inf1_srv2, exp(mle$log_slp1_srv2), mle$inf2_srv2, exp(mle$log_slp2_srv2)),
                             c(mle$inf1_srv3, exp(mle$log_slp1_srv3)),
                             c(mle$inf1_srv6, exp(mle$log_slp1_srv6), mle$inf2_srv6, exp(mle$log_slp2_srv6))))
catchability <- list(names=fleets$names,
                     form=c('none', rep('linear', times=3)),
                     re=rep('none', 4),
                     init_pars=c(NA, mle$log_q2_mean, mle$log_q3_mean, mle$log_q6),
                     ## init_repars=c('rho'=.8, 'sd'=.1),
                     fix_pars=c(NA,myNA),
                     fix_repars=c(NA,myNA))
recruitment <- list(type='BevertonHolt',
                    ## R0, steepness, sigmaR
                    init_pars=c(exp(parfinal$mean_log_recruit + log(1e9)), .99999, parfinal$sigmaR),
                    fix_pars=c(2:3),
                    init_recdevs=parfinal$dev_log_recruit[-1],
                    re=c('none', 'none'))
growth <- list(type='EWAA', waa=pkinput$dat$wt_srv1[1,])
maturity <- list(type='logistic', init_pars=c(4.5, 1.5), fix_pars=1:2)

selexout <- setup_selex(sel)
setup_fleets(fleets, population, catchability, selectivity=selexout, data=age_frame)
recout <- setup_recruitment(recruitment)
growthout <- setup_growth(growth)
matout <- setup_maturity(maturity)
setup_population(population, matout, growthout, recout)



