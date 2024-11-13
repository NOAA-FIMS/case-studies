# options("install.lock" = FALSE)
# remove.packages("FIMS")
# detach("package:FIMS", unload = TRUE)
# install.packages("FIMS", repos = c("https://noaa-fims.r-universe.dev", "https://cloud.r-project.org"))
# library(FIMS)
# source(file.path("content", "run_pollock_tests.R"))
# save(parameters, rep1, obj1, file = file.path("content", "data_files", "pollock_output_rep1.RData"))
# rm(list = ls())

options("install.lock" = FALSE)
detach("package:FIMS", unload = TRUE)
remove.packages("FIMS")
remotes::install_github(
  repo = "NOAA-FIMS/FIMS",
  ref = "dev-r-setup-wrapper-functions"
)

# Run FIMS with helper functions ------------------------------------------
library(FIMS)
clear()

# Define the years and ages for the assessment
years <- 1970:2023
nyears <- length(years)
nseasons <- 1
nages <- 10
ages <- 1:nages

# Source the script to prepare input data
source(file.path(getwd(), "content", "R", "pk_prepare_dat.R"))
fims_frame <- age_frame

# Define fleet specifications for fleet1 and survey2, 3, and 6
fleet1 <- survey2 <- survey6 <- list(
  selectivity = list(form = "DoubleLogisticSelectivity"),
  data_distribution = c(
    Index = "TMBDlnormDistribution",
    AgeComp = "TMBDmultinomDistribution"
  )
)

survey3 <- list(
  selectivity = list(form = "LogisticSelectivity"),
  data_distribution = c(
    Index = "TMBDlnormDistribution",
    AgeComp = "TMBDmultinomDistribution"
  )
)

fleets <- list(
  fleet1 = fleet1,
  survey2 = survey2,
  survey3 = survey3,
  survey6 = survey6
)

# Create default parameters
default_parameters <- fims_frame |>
  create_default_parameters(
    fleets = fleets,
    recruitment = list(
      form = "BevertonHoltRecruitment",
      process_distribution = c(log_devs = "TMBDnormDistribution")
    ),
    growth = list(form = "EWAAgrowth"),
    maturity = list(form = "LogisticMaturity")
  )

# Update default values for parameters
parameters_wrapper <- default_parameters |>
  update_parameters(
    modified_parameters = list(
      fleet1 = list(
        DoubleLogisticSelectivity.inflection_point_asc.value = parfinal$inf1_fsh_mean,
        DoubleLogisticSelectivity.slope_asc.value = exp(parfinal$log_slp1_fsh_mean),
        DoubleLogisticSelectivity.inflection_point_desc.value = parfinal$inf2_fsh_mean,
        DoubleLogisticSelectivity.slope_desc.value = exp(parfinal$log_slp2_fsh_mean),
        Fleet.log_Fmort.value = log(pkfitfinal$rep$F)
      )
    )
  ) |>
  update_parameters(
    modified_parameters = list(
      survey2 = list(
        DoubleLogisticSelectivity.inflection_point_asc.value = parfinal$inf1_srv2,
        DoubleLogisticSelectivity.slope_asc.value = exp(parfinal$log_slp1_srv2),
        DoubleLogisticSelectivity.inflection_point_desc.value = parfinal$inf2_srv2,
        DoubleLogisticSelectivity.inflection_point_desc.estimated = FALSE,
        DoubleLogisticSelectivity.slope_desc.value = exp(parfinal$log_slp2_srv2),
        DoubleLogisticSelectivity.slope_desc.estimated = FALSE,
        Fleet.log_q.value = parfinal$log_q2_mean
      )
    )
  ) |>
  update_parameters(
    modified_parameters = list(
      survey3 = list(
        LogisticSelectivity.inflection_point.value = parfinal$inf1_srv3,
        LogisticSelectivity.slope.value = exp(parfinal$log_slp1_srv3),
        Fleet.log_q.value = parfinal$log_q3_mean
      )
    )
  ) |>
  update_parameters(
    modified_parameters = list(
      survey6 = list(
        DoubleLogisticSelectivity.inflection_point_asc.value = parfinal$inf1_srv6,
        DoubleLogisticSelectivity.inflection_point_asc.estimated = FALSE,
        DoubleLogisticSelectivity.slope_asc.value = exp(parfinal$log_slp1_srv6),
        DoubleLogisticSelectivity.slope_asc.estimated = FALSE,
        DoubleLogisticSelectivity.inflection_point_desc.value = parfinal$inf2_srv6,
        DoubleLogisticSelectivity.slope_desc.value = exp(parfinal$log_slp2_srv6),
        Fleet.log_q.value = parfinal$log_q6
      )
    )
  ) |>
  update_parameters(
    modified_parameters = list(
      recruitment = list(
        BevertonHoltRecruitment.log_rzero.value = parfinal$mean_log_recruit + log(1e9),
        BevertonHoltRecruitment.logit_steep.value = -log(1.0 - .99999) + log(.99999 - 0.2),
        BevertonHoltRecruitment.log_devs.value = parfinal$dev_log_recruit[-1],
        TMBDnormDistribution.log_sd.value = parfinal$sigmaR
      )
    )
  ) |>
  update_parameters(
    modified_parameters = list(
      maturity = list(
        LogisticMaturity.inflection_point.value = 4.5,
        LogisticMaturity.inflection_point.estimated = FALSE,
        LogisticMaturity.slope.value = 1.5,
        LogisticMaturity.slope.estimated = FALSE
      )
    )
  ) |>
  update_parameters(
    modified_parameters = list(
      population = list(
        Population.log_M.value = log(as.numeric(t(matrix(
          rep(pkfitfinal$rep$M, each = fims_frame@n_years),
          nrow = fims_frame@n_years
        )))),
        Population.log_init_naa.value = c(log(pkfitfinal$rep$recruit[1]), log(pkfitfinal$rep$initN)) + log(1e9),
        Population.log_init_naa.estimated = FALSE
      )
    )
  )

fit <- parameters_wrapper |>
  initialize_fims(data = fims_frame) |>
  (\(parameter_list) list(parameters = parameter_list, version = "FIMS run with optimization"))() |>
  fit_fims(optimize = TRUE)

rep_wrappers <- fit@report

load(file.path(getwd(), "content", "data_files", "pollock_output_rep1.RData"))
all.equal(parameters$p, fit@input$parameters$p)
all.equal(unname(obj1$par), fit@estimates$value[fit@estimates$name == "p"])

rep1_names <- names(rep1)
filtered_names <- rep1_names[!grepl("nll", rep1_names)]
for (name in filtered_names){
  all.equal(rep1[[name]], rep_wrappers[[name]])
}

c(rep1$age_comp_nll, rep1$index_nll, rep1$rec_nll, rep1$jnll)
c(rep_wrappers$nll_components, rep_wrappers$jnll)

clear()
