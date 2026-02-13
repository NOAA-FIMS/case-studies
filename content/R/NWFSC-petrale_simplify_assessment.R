##################################################################
# code below under if(FALSE) is for simplifying original 
# petrale sole production assessment model to better match
# the options currently available in FIMS
##################################################################

if (FALSE) {
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
  dat$ageerror <- dat$ageerror[1:2, paste0("age", 0:dat$Nages)]

  # simplify age comps
  dat$agecomp <-
    dat$agecomp |>
    # exclude fleet 3, and only use marginal data for fleet 4
    dplyr::filter(fleet %in% c(1:2, -4)) |>
    # exclude male compositions (columns matching pattern "m" followed
    # by numeric value for length bin, but doesn't match "month")
    dplyr::select(!dplyr::matches("^m\\d+$")) |>
    # assign observations to ageing error 1
    dplyr::mutate(sex = 1, ageerr = 1) |>
    # remove redundant observations
    # NOTE: this removes some age comp data because there
    # were years with multiple observations from the same fleet
    # due to multiple ageing error matrices
    dplyr::distinct(year, fleet, .keep_all = TRUE)

  # simplify length comps
  dat$lencomp <-
    dat$lencomp |>
    # exclude fleet 3 and any discard length comps (part == 1)
    dplyr::filter(part == 2 & fleet %in% c(1, 2, 4)) |>
    # exclude male compositions (columns matching pattern "m" followed
    # by numeric value for length bin, but doesn't match "month")
    dplyr::select(!dplyr::matches("^m\\d+$")) |>
    dplyr::mutate(sex = 1)

  ##################################################################
  ## SIMPLIFY SS3 CONTROL FILE

  ctl <- petrale_input$ctl
  ctl$Nages <- dat$Nages # previously 40
  ctl$Nsexes <- dat$Nsexes # previously 2
  # turn on empirical weight-at-age
  # ctl$EmpiricalWAA <- 1 # previously zero
  # remove all male mortality and growth parameters
  ctl$MG_parms <- ctl$MG_parms |>
    dplyr::filter(!grepl("_Mal_", rownames(ctl$MG_parms)))
  ctl$max_bias_adj <- -1 # set bias adjust = 1.0 for all years

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

  # turn off variance adjustments
  ctl$DoVar_adjust <- 0
  ctl$Variance_adjustment_list <- NULL
  # use wtatage file instead of growth parameters
  ctl$EmpiricalWAA <- 1

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
  wtatage_matfec <- wtatage |>
    dplyr::select(1:6, paste(0:dat$Nages)) |>
    dplyr::filter(year == 1876 & sex == 1 & fleet == -2) |>
    dplyr::mutate(year = -year)
  wtatage_simple <- rbind(
    wtatage_matfec,
    wtatage_pop,
    wtatage_pop,
    wtatage_pop,
    wtatage_pop,
    wtatage_pop,
    wtatage_pop
  ) |>
    mutate(fleet = -2:4)

  # write modified data and control files
  new_input <- petrale_input
  new_input$dat <- dat
  new_input$ctl <- ctl
  new_input$wtatage <- wtatage_simple

  new_input$dir <- "c:/ss/Petrale/Petrale2023/petrale/models/2023.a050.003_FIMS_case-study_wtatage"
  # create directory if it doesn't exist
  if (!dir.exists(new_input$dir)) {
    dir.create(new_input$dir)
  }

  r4ss::SS_write(new_input, dir = new_input$dir, overwrite = TRUE)

  r4ss::run(
    new_input$dir,
    show_in_console = TRUE,
    skipfinished = FALSE
  )

  petrale_simple_output <- r4ss::SS_output(new_input$dir)

  # read SS3 output files from petrale sole assessment on GitHub
  petrale_output <- r4ss::SS_output(
    "https://raw.githubusercontent.com/pfmc-assessments/petrale/main/models/2023.a034.001",
    printstats = FALSE,
    verbose = FALSE,
    covar = FALSE
  )

  # compare outputs from simplified and original SS3 models
  # (fairly large changesin spawning output, perhaps due to removal of the older age classes)
  SSplotComparisons(SSsummarize(list(petrale_simple_output, petrale_output)))

} # end of if(FALSE) block for simplifying original SS3 model
