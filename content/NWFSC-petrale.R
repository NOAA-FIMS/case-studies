library(dplyr)
library(tidyr)
library(ggplot2)
require(FIMS)
library(TMB)
# devtools::install_github("kaskr/TMB_contrib_R/TMBhelper")
library(TMBhelper)
# remotes::install_github("r4ss/r4ss")
require(r4ss)

R_version <- version$version.string
TMB_version <- packageDescription("TMB")$Version
FIMS_commit <- substr(packageDescription("FIMS")$GithubSHA1, 1, 7)



# read SS3 input files from petrale sole assessment on github
petrale_input <- r4ss::SS_read("https://raw.githubusercontent.com/pfmc-assessments/petrale/main/models/2023.a034.001/")

# # reading output doesn't work from github, so just using hard-wired values from model output
# petrale_output <- r4ss::SS_output("https://raw.githubusercontent.com/pfmc-assessments/petrale/main/models/2023.a034.001/")

# generic names for SS3 data and control files could be useful in future generalized version of this code
ss3dat <- petrale_input$dat
ss3ctl <- petrale_input$ctl

# define the dimensions
years <- seq(ss3dat$styr, ss3dat$endyr)
nyears <- length(years)
nseasons <- 1
# ages <- 0:ss3dat$Nages # population ages in SS3, starts at age 0
ages <- 1:17 # same as data bins
nages <- length(ages)

# use function defined above to extract data for FIMS
mydat <- get_ss3_data(ss3dat, fleets = c(1, 4), ages = ages)

# remove fleets 2 and 3 and rename fleet4 as fleet2
mydat <- mydat |>
  dplyr::filter(name %in% c("fleet1", "fleet4")) |>
  dplyr::mutate(name = dplyr::case_when(
    name == "fleet1" ~ name,
    name == "fleet4" ~ "fleet2" # change fleet4 to fleet2
  ))

# # filter for just years with no missing age or index data
# # in spite of filling in -999 values earlier, just in case
# years <- 2003:2019
# nyears <- length(years)
# mydat <- mydat |> dplyr::filter(datestart %in% paste0(years, "-01-01"))


age_frame <- FIMS::FIMSFrameAge(mydat) # similar to FIMSFrame() but includes ages
# in the future FIMSFrame() and FIMSFrameAge() will likely be merged
fishery_catch <- FIMS::m_landings(age_frame) # filtering for the landings only
fishery_agecomp <- FIMS::m_agecomp(age_frame, "fleet1") # filtering for ages from fleet 1
survey_index <- FIMS::m_index(age_frame, "fleet2") # filtering for index data from fleet 2
survey_agecomp <- FIMS::m_agecomp(age_frame, "fleet2") # filtering for ages from fleet 2

fish_index <- methods::new(Index, nyears)
fish_age_comp <- methods::new(AgeComp, nyears, nages)
fish_index$index_data <- fishery_catch
# Q: I'm confused about FIMSFrame being set up with age comps in proportions
#   vs here needing age comps in numbers
# just not sorted out yet, in the future this could be made simpler
fish_age_comp$age_comp_data <- age_frame@data |>
  dplyr::filter(type == "age" & name == "fleet1") |>
  dplyr::mutate(n = value * uncertainty) |>
  dplyr::pull(n) |>
  round(1)

# switches to turn on or off estimation
estimate_fish_selex <- TRUE
estimate_survey_selex <- TRUE
estimate_q <- TRUE
estimate_F <- TRUE
estimate_recdevs <- TRUE
estimate_init_naa <- FALSE
estimate_log_rzero <- TRUE

### set up fishery
## methods::show(DoubleLogisticSelectivity)
fish_selex <- methods::new(LogisticSelectivity)

# SS3 model had length-based selectivity which leads to sex-specific
# age-based selectivity due to sexually-dimorphic growth.
# I didn't bother to calculate an age-based inflection point averaged over sexes
fish_selex$inflection_point$value <- 10
fish_selex$inflection_point$is_random_effect <- FALSE
fish_selex$inflection_point$estimated <- estimate_fish_selex
fish_selex$slope$value <- 2
fish_selex$slope$is_random_effect <- FALSE
fish_selex$slope$estimated <- estimate_fish_selex

## create fleet object for fishing fleet
fish_fleet <- methods::new(Fleet)
fish_fleet$nages <- nages
fish_fleet$nyears <- nyears
fish_fleet$log_Fmort <- log(rep(0.00001, nyears))
fish_fleet$estimate_F <- estimate_F
fish_fleet$random_F <- FALSE
fish_fleet$log_q <- 0
fish_fleet$estimate_q <- estimate_q
fish_fleet$random_q <- FALSE
fish_fleet$log_obs_error <- rep(log(sqrt(log(0.01^2 + 1))), nyears)

# Set Index, AgeComp, and Selectivity using the IDs from the modules defined above
fish_fleet$SetObservedIndexData(fish_index$get_id())
fish_fleet$SetObservedAgeCompData(fish_age_comp$get_id())
fish_fleet$SetSelectivity(fish_selex$get_id())

## Setup survey
survey_fleet_index <- methods::new(Index, nyears)
survey_age_comp <- methods::new(AgeComp, nyears, nages)
survey_fleet_index$index_data <- survey_index
survey_age_comp$age_comp_data <- mydat |>
  dplyr::filter(type == "age" & name == "fleet2") |>
  dplyr::mutate(n = value * uncertainty) |>
  dplyr::pull(n)

## survey selectivity: ascending logistic
## methods::show(DoubleLogisticSelectivity)
survey_selex <- new(LogisticSelectivity)
survey_selex$inflection_point$value <- 6
survey_selex$inflection_point$is_random_effect <- FALSE
survey_selex$inflection_point$estimated <- estimate_survey_selex
survey_selex$slope$value <- 2
survey_selex$slope$is_random_effect <- FALSE
survey_selex$slope$estimated <- estimate_survey_selex

## create fleet object for survey
survey_fleet <- methods::new(Fleet)
survey_fleet$is_survey <- TRUE
survey_fleet$nages <- nages
survey_fleet$nyears <- nyears
survey_fleet$estimate_F <- FALSE
survey_fleet$random_F <- FALSE
survey_fleet$log_q <- 1.4 # petrale sole catchability estimated ~4.0 = exp(1.4)
survey_fleet$estimate_q <- estimate_q
survey_fleet$random_q <- FALSE
# Q: why can't the index uncertainty come from FIMSFrame?
survey_fleet$log_obs_error <- age_frame@data |>
  dplyr::filter(type == "index" & name == "fleet2") |>
  dplyr::pull(uncertainty) |>
  log()

survey_fleet$SetAgeCompLikelihood(1)
survey_fleet$SetIndexLikelihood(1)
survey_fleet$SetSelectivity(survey_selex$get_id())
survey_fleet$SetObservedIndexData(survey_fleet_index$get_id())
survey_fleet$SetObservedAgeCompData(survey_age_comp$get_id())

# Population module

# recruitment
recruitment <- methods::new(BevertonHoltRecruitment)
# methods::show(BevertonHoltRecruitment)

# petrale sigmaR is 0.5
recruitment$log_sigma_recruit$value <- log(ss3ctl$SR_parms["SR_sigmaR", "INIT"])
# petrale log(R0) is around 9.6 (where R0 is in thousands)
# Q: do we need to account for SS3 R0 in thousands?
# recruitment$log_rzero$value <- log(1000) + ss3ctl$SR_parms["SR_LN(R0)", "INIT"]
recruitment$log_rzero$value <- ss3ctl$SR_parms["SR_LN(R0)", "INIT"]
recruitment$log_rzero$is_random_effect <- FALSE
recruitment$log_rzero$estimated <- estimate_log_rzero
# petrale steepness is fixed at 0.8
steep <- ss3ctl$SR_parms["SR_BH_steep", "INIT"]
recruitment$logit_steep$value <- -log(1.0 - steep) + log(steep - 0.2)
recruitment$logit_steep$is_random_effect <- FALSE
recruitment$logit_steep$estimated <- FALSE

recruitment$estimate_log_devs <- estimate_recdevs
# Q: why are parameters "log_devs" when output is "report$log_recruit_dev"?
# and are they multipliers, not deviations from zero?
# needed to change from 1 to 0 to get stable population
recruitment$log_devs <- rep(0, nyears) # set to no deviations (multiplier) to start

# growth
ewaa_growth <- methods::new(EWAAgrowth)
ewaa_growth$ages <- ages
# NOTE: getting weight-at-age vector from
# petrale_output$wtatage |>
#   dplyr::filter(Sex == 1 & Fleet == -1 & Yr == 1876) |>
#   dplyr::select(paste(0:40)) |>
#   round(4)
ewaa_growth$weights <- c(
  # 0.0010,  # age 0
  0.0148, 0.0617, 0.1449, 0.2570, 0.3876, 0.5260, 0.6640, 0.7957, 0.9175,
  1.0273, 1.1247, 1.2097, 1.2831, 1.3460, 1.3994, 1.4446, 1.4821
)

# maturity
maturity <- new(LogisticMaturity)
# approximate age-based equivalent to length-based maturity in petrale model
# based on looking at model$endgrowth |> dplyr::filter(Sex == 1) |> dplyr::select(Age_Beg, Len_Mat)
maturity$inflection_point$value <- 6.5
maturity$inflection_point$is_random_effect <- FALSE
maturity$inflection_point$estimated <- FALSE
maturity$slope$value <- 2 # arbitrary guess
maturity$slope$is_random_effect <- FALSE
maturity$slope$estimated <- FALSE

# population
population <- new(Population)
# petrale natural mortality is estimated around 0.14
M_value <- ss3ctl$MG_parms["NatM_p_1_Fem_GP_1", "INIT"]
population$log_M <- rep(log(M_value), nages * nyears)
population$estimate_M <- FALSE
# initial numbers at age based on R0 + mortality
init_naa <- exp(recruitment$log_rzero$value) * exp(-(ages - 1) * M_value)
init_naa[nages] <- init_naa[nages] / M_value # sum of infinite series
population$log_init_naa <- log(init_naa)
population$estimate_init_naa <- estimate_init_naa
population$nages <- nages
population$ages <- ages
population$nfleets <- 2 # fleets plus surveys
population$nseasons <- nseasons
population$nyears <- nyears
# population$proportion_female <- rep(0.5, nages)

population$SetMaturity(maturity$get_id())
population$SetGrowth(ewaa_growth$get_id())
population$SetRecruitment(recruitment$get_id())

if (TRUE) {
  # make FIMS model
  success <- CreateTMBModel()

  parameters <- list(p = get_fixed())
  obj <- MakeADFun(data = list(), parameters, DLL = "FIMS", silent = TRUE)

  opt <- nlminb(obj$par, obj$fn, obj$gr,
    control = list(eval.max = 10000, iter.max = 10000)
  )
  print(opt)

  # sdr <- TMB::sdreport(obj)
  # sdr_fixed <- summary(sdr, "fixed")
  # print(sdr_fixed)

  report <- obj$report()

  # copy input data to use as basis for results
  results_frame <- age_frame@data
  results_frame$expected <- NA
  # convert date string to numeric year
  results_frame <- results_frame |>
    dplyr::mutate(year = lubridate::as_date(datestart) |> lubridate::year())

  # add expected index to data frame
  results_frame$expected[results_frame$type == "index" & results_frame$name == "fleet2"] <-
    report$exp_index[[2]]

  # add estimated catch to data frame
  results_frame$expected[results_frame$type == "landings" & results_frame$name == "fleet1"] <-
    report$exp_catch[[1]]

  # add estimated age comps to data frame
  for (fleet in 1:2) {
    # copy Cole's approach to rescaling expected comps to proportions
    x1 <- matrix(report$cnaa[[fleet]], ncol = nages, byrow = TRUE)
    x1 <- x1 / rowSums(x1)
    dimnames(x1) <- list(year = years, age = ages)
    x1 <- reshape2::melt(x1, value.name = "paa") |>
      dplyr::mutate(type = "age", name = paste0("fleet", fleet))
    # add expected proportions into results_frame
    results_frame <-
      # add paa for age comps (will be NA for all other types)
      dplyr::left_join(x = results_frame, y = x1) |>
      # replace value column with paa for age data within this fleet (when not NA)
      dplyr::mutate(expected = dplyr::case_when(is.na(paa) ~ expected, TRUE ~ paa)) |>
      dplyr::select(-paa) # remove temporary paa column
  }

  # plot catch fit
  results_frame |>
    dplyr::filter(type == "landings" & value != -999) |>
    ggplot(aes(x = year, y = value)) +
    geom_point() +
    xlab("Year") +
    ylab("Catch (mt)") +
    geom_line(aes(x = year, y = expected), color = "blue") +
    theme_bw()
  ggsave("content/figures/NWFSC-petrale_fit_catch.png")

  # plot index fit
  results_frame |>
    dplyr::filter(type == "index" & value != -999) |>
    ggplot(aes(x = year, y = value)) +
    geom_point() +
    xlab("Year") +
    ylab("Index") +
    geom_line(aes(x = year, y = expected), color = "blue") +
    theme_bw()
  ggsave("content/figures/NWFSC-petrale_fit_index.png")

  # plot age comp fits
  # age comps for fleet 1
  results_frame |>
    dplyr::filter(type == "age" & name == "fleet1" & value != -999) |>
    ggplot(aes(x = age, y = value)) +
    # note: dir = "v" sets vertical direction to fill the facets which
    # makes comparison of progression of cohorts easier to see
    facet_wrap(vars(year), dir = "v") +
    geom_point() +
    xlab("Age") +
    ylab("Proportion") +
    geom_line(aes(x = age, y = expected), color = "blue") +
    theme_bw()
  ggsave("content/figures/NWFSC-petrale_fit_comps_fleet1.png")

  results_frame |>
    dplyr::filter(type == "age" & name == "fleet2" & value != -999) |>
    ggplot(aes(x = age, y = value)) +
    # note: dir = "v" sets vertical direction to fill the facets which
    # makes comparison of progression of cohorts easier to see
    facet_wrap(vars(year), dir = "v") +
    geom_point() +
    xlab("Age") +
    ylab("Proportion") +
    geom_line(aes(x = age, y = expected), color = "blue") +
    theme_bw()
  ggsave("content/figures/NWFSC-petrale_fit_comps_fleet2.png")

  timeseries <- rbind(
    data.frame(
      year = c(years, max(years) + 1),
      type = "ssb",
      value = report$ssb[[1]]
    ),
    data.frame(
      year = c(years, max(years) + 1),
      type = "biomass",
      value = report$biomass[[1]]
    ),
    data.frame(
      year = c(years),
      type = "recruitment",
      value = report$recruitment[[1]][1:nyears] # final value was 0
    ),
    data.frame(
      year = c(years),
      type = "F_mort",
      value = report$F_mort[[1]]
    )
  )

  # plot time series of 4 quantities
  timeseries |>
    ggplot(aes(x = year, y = value)) +
    facet_wrap(vars(type), scales = "free") +
    geom_line() +
    expand_limits(y = 0) +
    theme_bw()
  ggsave("content/figures/NWFSC-petrale_time_series.png")

  # create function to extract time series output from SS3 model
  get_ss3_timeseries <- function(model, platform = "ss3") {
    timeseries_ss3 <- model$timeseries |>
      dplyr::filter(Yr %in% timeseries$year) |> # filter for matching years only (no forecast)
      dplyr::select(Yr, Bio_all, SpawnBio, Recruit_0, "F:_1") |> # select quants of interest
      dplyr::rename( # change to names used with FIMS
        year = Yr,
        biomass = Bio_all, ssb = SpawnBio, recruitment = Recruit_0, F_mort = "F:_1"
      ) |>
      dplyr::mutate(ssb = 1000 * ssb) |>
      tidyr::pivot_longer( # convert quantities in separate columns into a single value column
        cols = -1,
        names_to = "type",
        values_to = "value"
      ) |>
      dplyr::arrange(type) |> # sort by type instead of year
      dplyr::mutate(platform = platform)

    return(timeseries_ss3)
  }

  # compare to SS3 output
  if (FALSE) {
    # read SS3 models from location on Ian's computer
    p1 <- r4ss::SS_output("c:/ss/Petrale/Petrale2023/petrale/models//2023.a034.001/")
    p2 <- r4ss::SS_output("c:/ss/Petrale/Petrale2023/petrale/models//2023.a050.002_FIMS_case-study_wtatage/")
    # # saving all model output creates large files (7MB for original)
    # saveRDS(p1, file = "content/data_files/NWFSC-petrale-SS3-original.rds")
    # saveRDS(p2, file = "content/data_files/NWFSC-petrale-SS3-simplified.rds")

    # combine SS3 model time series into data frame using function above
    timeseries_compare <- rbind(
      get_ss3_timeseries(model = p1, platform = "ss3_original"),
      get_ss3_timeseries(model = p2, platform = "ss3_simplified")
    )
    # save data frame of time series results
    saveRDS(timeseries_compare, file = "content/data_files/NWFSC-petrale-SS3-timeseries.rds")
  }

  # load saved time series
  timeseries_compare <- readRDS("content/data_files/NWFSC-petrale-SS3-timeseries.rds")
  # add FIMS output to time series table
  timeseries_compare <- timeseries |>
    dplyr::mutate(platform = "FIMS") |>
    rbind(timeseries_compare)

  # make plot comparing time series
  timeseries_compare |>
    ggplot(aes(year, value, color = platform)) +
    geom_line() +
    facet_wrap("type", scales = "free") +
    ylim(0, NA) +
    labs(x = NULL, y = NULL) +
    theme_bw()
  ggsave("content/figures/NWFSC-petrale_timeseries_comparison.png")

  # numbers at age as a matrix
  naa <- matrix(report$naa[[1]], ncol = nages, byrow = TRUE)
  # numbers at age as a long data frame
  naa_df <- tidyr::expand_grid(year = c(years, max(years) + 1), age = ages) |>
    data.frame(naa = report$naa[[1]])

  # bubble plot of numbers at age
  naa_df |>
    ggplot(aes(x = year, y = age, size = naa)) +
    geom_point(alpha = 0.2) +
    theme_bw()
  ggsave("content/figures/NWFSC-petrale_numbers_at_age.png")

  clear()
} # end "if (FALSE)" block to only run if model is working
