##################################################################
# code below under if(FALSE) is for simplifying original
# petrale sole production assessment model to better match
# the options currently available in FIMS
##################################################################

# read SS3 input files from petrale sole assessment on GitHub
petrale_input <- r4ss::SS_read(
  "https://raw.githubusercontent.com/pfmc-assessments/petrale/main/models/2023.a034.001/",
  read_wtatage = TRUE
)

##################################################################
## SIMPLIFY SS3 DATA FILE

dat <- petrale_input$dat

# change dimensions
dat$Nsexes <- 1 # previously 2
dat$Nages <- 17 # previously 40

# filter indices (just include WCGBTS)
dat$CPUE <- dat$CPUE |>
  dplyr::filter(index == 4)

# remove discard info
dat$N_discard_fleets <- 0
dat$discard_fleet_info <- NULL
dat$discard_data <- NULL

# remove mean body weight
dat$use_meanbodywt <- 0
dat$meanbodywt <- NULL
dat$DF_for_meanbodywt <- NULL

# simplify ageing error matrices to just use first matrix
# and only for new range of ages
dat$N_ageerror_definitions <- 1
dat$ageerror <- dat$ageerror[1:2, paste0("age", 0:dat$Nages)] # first row is mean, second row is sd

# simplify age comps
dat$agecomp <-
  dat$agecomp |>
  # exclude fleet 3, and only use marginal data for fleet 4
  dplyr::filter(fleet %in% c(1:2, -4)) |>
  # exclude male compositions (columns matching pattern "m" followed
  # by numeric value for length bin, but doesn't match "month")
  dplyr::select(!dplyr::matches("^m\\d+$")) |>
  # assign observations to sex 1, ageing error 1, and include previously excluded marginal ages
  dplyr::mutate(sex = 1, ageerr = 1, fleet = abs(fleet)) |>
  # remove redundant observations
  # NOTE: this removes some age comp data because there
  # were years with multiple observations from the same fleet
  # due to multiple ageing error matrices
  dplyr::distinct(year, fleet, .keep_all = TRUE)

# simplify length comps
dat$lencomp <-
  dat$lencomp |>
  # exclude fleet 3 and any discard length comps (part == 1)
  dplyr::filter(part %in% c(0, 2) & fleet %in% c(1, 2, 4)) |>
  # exclude male compositions (columns matching pattern "m" followed
  # by numeric value for length bin, but doesn't match "month")
  dplyr::select(!dplyr::matches("^m\\d+$")) |>
  dplyr::mutate(sex = 1)

##################################################################
## SIMPLIFY SS3 CONTROL FILE

ctl <- petrale_input$ctl
ctl$Nages <- dat$Nages # previously 40
ctl$Nsexes <- dat$Nsexes # previously 2

# remove all male mortality and growth parameters
ctl$MG_parms <- ctl$MG_parms |>
  dplyr::filter(!grepl("_Mal_", rownames(ctl$MG_parms)))
ctl$max_bias_adj <- -1 # set bias adjust = 1.0 for all years

# fix growth parameters by setting phase negative for everything except M
ctl$MG_parms <- ctl$MG_parms |>
  dplyr::mutate(
    PHASE = ifelse(grepl("NatM_", rownames(ctl$MG_parms)), PHASE, -abs(PHASE))
  )

# # fix M at 0.1 in case this helps with estimation problems
# ctl$MG_parms <- ctl$MG_parms |>
#   dplyr::mutate(
#     PHASE = ifelse(
#       grepl("NatM_", rownames(ctl$MG_parms)),
#       -abs(PHASE),
#       PHASE
#     ),
#     INIT = ifelse(grepl("NatM_", rownames(ctl$MG_parms)), 0.1, INIT)
#   )

# filter inputs stuff to only fleet 4 (WCGBTS)
ctl$Q_options <- ctl$Q_options |>
  dplyr::filter(fleet == 4)
ctl$Q_parms <- ctl$Q_parms |>
  dplyr::filter(grepl("WCGBTS", rownames(ctl$Q_parms)))

# add new age-based selectivity parameters
# age-based logistic (pattern 12) for fleets 1 and 4
# have south fishery (fleet 2) mirror fleet 1
# no parameters for fleet 3 (ignored index left in place to avoid renumbering fleets)
ctl$age_selex_types <- ctl$age_selex_types |>
  dplyr::mutate(
    Pattern = c(12, 15, 0, 12),
    Special = c(0, 1, 0, 0)
  ) # fleet 2 mirror fleet 1

# create 4 parameter rows
ctl$age_selex_parms <- cbind(
  data.frame(
    LO = 1,
    HI = 10,
    INIT = c(5, 2, 5, 2),
    PRIOR = 0,
    PR_SD = 99,
    PR_type = 0,
    PHASE = 2
  ),
  matrix(0, nrow = 4, ncol = 7) # this fills in columns 8 to 14 with zeros
)
names(ctl$age_selex_parms)[8:14] <- names(ctl$size_selex_parms)[8:14]
rownames(ctl$age_selex_parms) <- paste0(
  "AgeSel_",
  c("P_1_", "P_2_"),
  c("fleet1", "fleet1", "fleet4", "fleet4")
)

# remove length-based selectivity
ctl$size_selex_types <- ctl$size_selex_types |>
  dplyr::mutate(Pattern = 0, Discard = 0, Male = 0)
ctl$size_selex_parms <- NULL
ctl$size_selex_parms_tv <- NULL

# remove blocks
ctl$N_Block_Designs <- 0
ctl$blocks_per_pattern <- NULL
ctl$Block_Design <- NULL

# turn off variance adjustments
ctl$DoVar_adjust <- 0
ctl$Variance_adjustment_list <- NULL
# use wtatage file instead of growth parameters
ctl$EmpiricalWAA <- 1

# put all recdevs into the main vector
ctl$MainRdevYrFirst <- 1861
ctl$MainRdevYrLast <- 2022
ctl$recdev_early_start <- 1845
ctl$recdev_early_phase <- -3
ctl$Fcast_recr_phase <- -4

ctl$recdev_phase <- 5

##################################################################
## SIMPLIFY wtatage file

wtatage <- petrale_input$wtatage


if (FALSE) {
  # explore estimated wtatage from SS3 model (differs among fleets due to
  # length-based selectivity and changes over time due to selectivity blocks)
  # nevertheless, it's easiest to just use the population wtatage for all fleets
  library(ggplot2)
  wtatage |>
    dplyr::filter(fleet %in% -1:4, sex == 1) |>
    select(year, fleet, "20") |>
    rename(wt = "20") |>
    ggplot(aes(x = year, y = wt, color = as.factor(fleet))) +
    geom_line()
}

# wtatage for fleets -1:0 (population), and fleets 1:4 (real fleets)
wtatage_pop <- wtatage |>
  dplyr::select(1:6, paste(0:dat$Nages)) |>
  dplyr::filter(year == 1876 & sex == 1 & fleet == -1) |>
  dplyr::mutate(year = -year)
# original model had a fecundity relationship
# so recalculate the maturity * fecundity based on the population weight-at-age
# and an age-based maturity curve (logistic with 50% maturity at age 6 and slope of 1.5)
# this is a rough approximation to the length-based maturity converted to age
# within the original model, but should be sufficient for testing the simplified model
maturity <- 1 / (1 + exp(-1.5 * (0:dat$Nages - 6)))
matfec <- maturity * as.numeric(dplyr::select(wtatage_pop, paste0(0:dat$Nages)))
wtatage_matfec <- wtatage_pop
wtatage_matfec[, paste(0:dat$Nages)] <- matfec

# # old code to use SS3 output for maturity * fecundity
# wtatage_matfec <- wtatage |>
#   dplyr::select(1:6, paste(0:dat$Nages)) |>
#   dplyr::filter(year == 1876 & sex == 1 & fleet == -2) |>
#   dplyr::mutate(year = -year)
wtatage_simple <- rbind(
  wtatage_matfec, # fleet = -2
  wtatage_pop, # fleet = -1
  wtatage_pop, # fleet = 0
  wtatage_pop, # fleet = 1
  wtatage_pop, # fleet = 2
  wtatage_pop, # fleet = 3
  wtatage_pop  # fleet = 4
) |>
  dplyr::mutate(fleet = -2:4)

# simplify forecast file
fore <- petrale_input$fore
fore$Forecast <- 0 # turn off forecast
fore$Bmark_relF_Basis <- 1 # use year range for benchmark relative F

# write modified data and control files
new_input <- petrale_input
new_input$dat <- dat
new_input$ctl <- ctl
new_input$wtatage <- wtatage_simple
new_input$fore <- fore

# reduce run display detail
new_input$start$run_display_detail <- 0

new_input$dir <- "c:/ss/Petrale/Petrale2023/petrale/models/2023.a050.003_FIMS_case-study_wtatage"
# create directory if it doesn't exist
if (!dir.exists(new_input$dir)) {
  dir.create(new_input$dir)
}

r4ss::SS_write(new_input, dir = new_input$dir, overwrite = TRUE)

# run model and compare output
if (FALSE) {
  r4ss::run(
    new_input$dir,
    show_in_console = TRUE,
    skipfinished = FALSE,
    extras = "-maxfn 3000" # was exceeding default maxfn 100 in early phases
  )
  r4ss::run(
    new_input$dir,
    show_in_console = TRUE,
    skipfinished = FALSE,
    extras = "-hess_step" # was exceeding default maxfn 100 in early phases
  )

  # read output from model that was just run
  petrale_simple_output <- r4ss::SS_output(
    new_input$dir,
    printstats = FALSE,
    verbose = FALSE,
    covar = FALSE
  )

  r4ss::SS_plots(petrale_simple_output)

  # read SS3 output files from petrale sole assessment on GitHub
  petrale_output1 <- r4ss::SS_output(
    "https://raw.githubusercontent.com/pfmc-assessments/petrale/main/models/2023.a034.001",
    printstats = FALSE,
    verbose = FALSE,
    covar = FALSE
  )

  # compare outputs from simplified and original SS3 models
  # (fairly large changes in spawning output, perhaps due to removal of the older age classes)
  SSplotComparisons(SSsummarize(list(
    petrale_simple_output,
    petrale_output1
  )))
} # end of if(FALSE) block for simplifying original SS3 model
