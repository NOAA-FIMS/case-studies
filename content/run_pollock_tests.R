#  Load packages and input data -------------------------------------------
# Load required packages
packages <- c("dplyr", "tidyr", "ggplot2", "TMB", "reshape2", "here", "remotes", "lubridate")
invisible(lapply(packages, library, character.only = TRUE))

# Install the FIMS package from specific repositories
# install.packages("FIMS", repos = c("https://noaa-fims.r-universe.dev", "https://cloud.r-project.org"))
library(FIMS)

# Define the years and ages for the assessment
years <- 1970:2023
nyears <- length(years)
nseasons <- 1
nages <- 10
ages <- 1:nages

# Source the script to prepare input data
source(file.path(getwd(), "content", "R", "pk_prepare_dat.R"))

# Run FIMS without helper functions ---------------------------------------
clear()
clear_logs()

# Source the script to prepare FIMS inputs without using helper functions
source(file.path(getwd(), "content", "R", "pk_prepare_FIMS_inputs.R"))

# Create the TMB model and generate the report
success <- CreateTMBModel()
parameters <- list(p = get_fixed())
obj1 <- TMB::MakeADFun(data = list(), parameters, DLL = "FIMS", silent = TRUE)
rep1 <- obj1$report()

# Run FIMS with helper functions (by process) -------------------------------
clear()
clear_logs()

# Source FIMS helper functions
source(file.path(getwd(), "content", "R", "pk_prepare_FIMS_inputs_by_process.R"))

# Examples for using helper functions
## Order of parameters for DL is asc inf, asc slop, desc inf,
## desc slope
mle <- parfinal
data <- age_frame
myNA <- rep(NA,3)
pop <- list(ages=ages, nfleets=4, nyears=nyears,
            years=years, nages=length(ages),
            M=as.numeric(t(matrix(rep(pkfitfinal$rep$M, each=nyears), nrow=nyears))),
            init_naa= exp(c(log(pkfitfinal$rep$recruit[1]), log(pkfitfinal$rep$initN)) + log(1e9)),
            fix_naa=TRUE)
fleets <- list(names=c('fleet1', 'survey2', 'survey3', 'survey6'),
               type=c('landings', rep('index',3)),
               estimate_Fmort=c(TRUE, myNA),
               init_Fmort=list(pkfitfinal$rep$F, myNA),
               reFmort=c(FALSE, myNA))

sel_init_pars <- list(c(mle$inf1_fsh_mean, exp(mle$log_slp1_fsh_mean), mle$inf2_fsh_mean, exp(mle$log_slp2_fsh_mean)),
                      c(mle$inf1_srv2, exp(mle$log_slp1_srv2), mle$inf2_srv2, exp(mle$log_slp2_srv2)),
                      c(mle$inf1_srv3, exp(mle$log_slp1_srv3)),
                      c(mle$inf1_srv6, exp(mle$log_slp1_srv6), mle$inf2_srv6, exp(mle$log_slp2_srv6)))
sel <- list(names=fleets$names,
            form=c('DoubleLogistic', 'DoubleLogistic', 'Logistic', 'DoubleLogistic'),
            fix_pars=list(NA, 3:4, NA, 1:2),
            re=rep('none',4),
            init_pars=sel_init_pars)
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
setup_fleets(fleets, pop, catchability, selectivity=selexout, data=age_frame)
recout <- setup_recruitment(recruitment)
growthout <- setup_growth(growth)
matout <- setup_maturity(maturity)
setup_population(pop, matout, growthout, recout)

# Run FIMS
success <- CreateTMBModel()
parameters <- list(p = get_fixed())
obj2 <- MakeADFun(data = list(), parameters, DLL = "FIMS", silent = TRUE)

rep2 <- obj2$report()

# Run FIMS with helper functions (by process): fix more selex parameters --------
clear()
clear_logs()

sel_init_pars <- list(c(mle$inf1_fsh_mean, exp(mle$log_slp1_fsh_mean), mle$inf2_fsh_mean, exp(mle$log_slp2_fsh_mean)),
                      c(mle$inf1_srv2, exp(mle$log_slp1_srv2), mle$inf2_srv2, exp(mle$log_slp2_srv2)),
                      c(mle$inf1_srv3, exp(mle$log_slp1_srv3)),
                      c(mle$inf1_srv6, exp(mle$log_slp1_srv6), mle$inf2_srv6, exp(mle$log_slp2_srv6)))
                      # c(mle$inf1_srv6, exp(mle$log_slp1_srv6), mle$inf2_srv6+1, exp(mle$log_slp2_srv6))) # use this line of code during the demo
sel <- list(names=fleets$names,
            form=c('DoubleLogistic', 'DoubleLogistic', 'Logistic', 'DoubleLogistic'),
            fix_pars=list(NA, 3:4, NA, 1:2),
            # fix_pars=list(NA, 3:4, NA, 1:3), # use this line of code during the demo
            re=rep('none',4),
            init_pars=sel_init_pars)

selexout <- setup_selex(sel)
setup_fleets(fleets, pop, catchability, selectivity=selexout, data=age_frame)
recout <- setup_recruitment(recruitment)
growthout <- setup_growth(growth)
matout <- setup_maturity(maturity)
setup_population(pop, matout, growthout, recout)

success <- CreateTMBModel()
parameters_fix_sel4 <- list(p = get_fixed())
obj2_fix_sel4 <- MakeADFun(data = list(), parameters = parameters_fix_sel4, DLL = "FIMS", silent = TRUE)
rep2_fix_sel4 <- obj2_fix_sel4$report()

# all.equal(length(parameters$p), length(parameters_fix_sel4$p)+1)
all.equal(rep2, rep2_fix_sel4)

# Run FIMS with helper functions (by fleet) ---------------------------------

clear()
clear_logs()

# Source FIMS helper functions
source(file.path(getwd(), "content", "R", "pk_prepare_FIMS_inputs_by_fleet.R"))

# Examples for using helper functions
## Set up fleet data
fishing_fleet_names <- dplyr::filter(
  .data = as.data.frame(age_frame@data),
  type == "landings"
) |>
  dplyr::distinct(name) |>
  dplyr::pull(name)
survey_fleet_names <- dplyr::filter(
  .data = as.data.frame(age_frame@data),
  type == "index"
) |>
  dplyr::distinct(name) |>
  dplyr::pull(name)

fleet_names <- c(fishing_fleet_names, survey_fleet_names)
index_types <- c(
  rep("landings", length(fishing_fleet_names)),
  rep("index", length(survey_fleet_names))
)
nfleets <- length(fleet_names)

## Set up population data
population_data <- list(
  nages = age_frame@n_ages,
  ages = age_frame@ages,
  nfleets = nfleets,
  fishing_fleet_names = fishing_fleet_names,
  survey_fleet_names = survey_fleet_names,
  fleet_names = fleet_names,
  index_types = index_types,
  nseasons = 1,
  nyears = age_frame@n_years
)

## Define whether observation error should be estimated for each fleet
is_estimated_obs_error <- list(FALSE, FALSE, FALSE, FALSE)

## Define selectivity for fishing fleet and surveys using Double Logistic and Logistic forms
selectivity_fsh_mean <-
  selectivity_srv2 <- selectivity_srv6 <-
  list(
    form = list("DoubleLogisticSelectivity"),
    initial_values = list(
      inflection_point_asc = parfinal$inf1_fsh_mean,
      slope_asc = exp(parfinal$log_slp1_fsh_mean),
      inflection_point_desc = parfinal$inf2_fsh_mean,
      slope_desc = exp(parfinal$log_slp2_fsh_mean)
    ),
    is_estimated = list(
      inflection_point_asc = TRUE,
      slope_asc = TRUE,
      inflection_point_desc = TRUE,
      slope_desc = TRUE
    ),
    is_random_effect = list(
      inflection_point_asc = FALSE,
      slope_asc = FALSE,
      inflection_point_desc = FALSE,
      slope_desc = FALSE
    )
  )

## Adjust initial values for selectivity for Survey 2
selectivity_srv2$initial_values <- list(
  inflection_point_asc = parfinal$inf1_srv2,
  slope_asc = exp(parfinal$log_slp1_srv2),
  inflection_point_desc = parfinal$inf2_srv2,
  slope_desc = exp(parfinal$log_slp2_srv2)
)

## Set estimated flags for descending inflection point and slope to FALSE for Survey 2
selectivity_srv2$is_estimated$inflection_point_desc <-
  selectivity_srv2$is_estimated$slope_desc <- FALSE

## Adjust initial values for selectivity for Survey 6
selectivity_srv6$initial_values <- list(
  inflection_point_asc = parfinal$inf1_srv6,
  slope_asc = exp(parfinal$log_slp1_srv6),
  inflection_point_desc = parfinal$inf2_srv6,
  slope_desc = exp(parfinal$log_slp2_srv6)
)

## Set estimated flags for ascending inflection point and slope to FALSE for Survey 6
selectivity_srv6$is_estimated$inflection_point_asc <-
  selectivity_srv6$is_estimated$slope_asc <- FALSE

## Define selectivity for Survey 3 using Logistic form
selectivity_srv3 <- list(
  form = list("LogisticSelectivity"),
  initial_values = list(
    inflection_point = parfinal$inf1_srv3,
    slope = exp(parfinal$log_slp1_srv3)
  ),
  is_estimated = list(
    inflection_point = TRUE,
    slope = TRUE
  ),
  is_random_effect = list(
    inflection_point = FALSE,
    slope = FALSE
  )
)

## Combine all selectivity controls into a list (order matters!)
selectivity_ctl <- list(
  selectivity_fsh_mean,
  selectivity_srv2,
  selectivity_srv3,
  selectivity_srv6
)

## Define fishing mortality controls
Fmort_ctl <- list(
  is_survey = list(FALSE, TRUE, TRUE, TRUE),
  initial_values = list(log(pkfitfinal$rep$F), 0, 0, 0),
  estimate_F = list(TRUE, FALSE, FALSE, FALSE),
  random_F = list(FALSE, FALSE, FALSE, FALSE)
)

## Define catchability controls
catchability_ctl <- list(
  log_q = list(
    0, parfinal$log_q2_mean,
    parfinal$log_q3_mean, parfinal$log_q6
  ),
  estimate_q = list(FALSE, TRUE, TRUE, TRUE),
  random_q = list(FALSE, FALSE, FALSE, FALSE)
)

## Set up the fleet module using the defined controls
fleet <- set_fleet(
  fleet_data = age_frame,
  population_data = population_data,
  is_estimated_obs_error = is_estimated_obs_error,
  selectivity = selectivity_ctl,
  Fmort = Fmort_ctl,
  catchability = catchability_ctl
)

## Define recruitment controls using Beverton-Holt form
recruitment_ctl <- list(
  form = "BevertonHoltRecruitment",
  initial_values = list(
    log_rzero = parfinal$mean_log_recruit + log(1e9),
    logit_steep = -log(1.0 - .99999) + log(.99999 - 0.2),
    log_sigma_recruit = log(parfinal$sigmaR),
    log_devs = parfinal$dev_log_recruit[-1]
  ),
  is_estimated = list(
    log_rzero = TRUE,
    logit_steep = FALSE,
    log_sigma_recruit = FALSE,
    log_devs = TRUE
  ),
  is_random_effect = list(
    log_rzero = FALSE,
    logit_steep = FALSE,
    log_sigma_recruit = FALSE
  )
)

## Define growth controls using EWAAgrowth form
# growth_ctl <- list(
#   data = population_data,
#   form = "EWAAgrowth",
#   initial_values = list(weights = pkinput$dat$wt_srv1[1, ])
# )

growth_m <- set_growth(
  population_data = population_data,
  form = "EWAAgrowth",
  initial_values = list(weights = pkinput$dat$wt_srv1[1, ])
)

## Define maturity controls using logistic form
maturity_ctl <- list(
  form = "LogisticMaturity",
  initial_values = list(
    inflection_point = 4.5,
    slope = 1.5
  ),
  is_estimated = list(
    inflection_point = FALSE,
    slope = FALSE
  ),
  is_random_effect = list(
    inflection_point = FALSE,
    slope = FALSE
  )
)

## Define initial values for log_M and log__init_naa
population_initial_values <- list(
  log_M = log(as.numeric(t(matrix(
    rep(pkfitfinal$rep$M, each = population_data$nyears),
    nrow = population_data$nyears
  )))),
  log_init_naa =
    c(log(pkfitfinal$rep$recruit[1]), log(pkfitfinal$rep$initN)) + log(1e9)
)

## Define estimated flags for log_M and log__init_naa
population_is_estimated <- list(
  log_M = FALSE,
  log_init_naa = FALSE
)

## Set up the population module using the defined controls and initial values
population <- set_population(
  population_data = population_data,
  initial_values = population_initial_values,
  is_estimated = population_is_estimated,
  maturity = maturity_ctl,
  growth = growth_m,
  recruitment = recruitment_ctl
)

# Run FIMS
success <- CreateTMBModel()
parameters <- list(p = get_fixed())
obj3 <- MakeADFun(data = list(), parameters, DLL = "FIMS", silent = TRUE)

rep3 <- obj3$report()


# Compare the reports from the three different runs -----------------------
# Check if reports from the first two runs are identical (original v.s. by process)
all.equal(rep1, rep2)
# Check if reports from the first and third runs are identical (original v.s. by fleet)
all.equal(rep1, rep3)

# Print the sorted parameter values from the three runs
print(cbind(sort(obj1$par), sort(obj2$par), sort(obj3$par)))

clear()
clear_logs()

# Optimize the models and compare the optimized parameters
opt1 <- with(obj1, nlminb(par, fn, gr))
opt2 <- with(obj2, nlminb(par, fn, gr))
opt2_fix_sel4 <- with(obj2_fix_sel4, nlminb(par, fn, gr))
opt3 <- with(obj3, nlminb(par, fn, gr))

# Compare optimized reports
all.equal(obj1$report(opt1$par), obj2$report(opt2$par))
all.equal(obj1$report(opt1$par), obj3$report(opt2$par))

# Check the sum of parameters difference
sum(obj1$par) - sum(obj2$par)
sum(obj1$par) - sum(obj3$par)

# Print the sorted optimized parameter values from the three runs
print(cbind(sort(obj1$par), sort(obj2$par), sort(obj3$par)))

clear()
clear_logs()
