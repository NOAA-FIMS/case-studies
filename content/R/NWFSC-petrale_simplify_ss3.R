# notes on simplifying the SS3 petrale model to better match what can be done in FIMS
# code is posted as an example only and is completely specific to petrale sole

if (FALSE) {
  # read input files if not already in workspace
  if (!exists("petrale_input")) {
    petrale_input <- r4ss::SS_read("https://raw.githubusercontent.com/pfmc-assessments/petrale/main/models/2023.a034.001/")
  }
  
  ## SIMPLIFY SS3 DATA FILE
  dat <- petrale_input$dat
  
  # change dimensions
  dat$Nsexes <- 1 # previously 2
  dat$Nages <- 17 # previously 40
  
  # mirroring selectivity and keeping both fishing fleets instead of aggregating catch
  # # aggregate landings across fleets
  # dat$catch <- dat$catch |>
  #     dplyr::group_by(year) |>
  #     dplyr::summarize(catch = sum(catch), catch_se = mean(catch_se)) |>
  #     dplyr::mutate(seas = 1, fleet = 1, .after = year) |>
  #     as.data.frame() # write_fwf4() can't write tibles
  
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
  
  # remove length comps
  dat$use_lencomp <- 0
  dat$len_info <- NULL
  dat$lencomp <- NULL
  
  # simplify ageing error matrix
  dat$N_ageerror_definitions <- 1
  dat$ageerror <- dat$ageerror[1:2, paste0("age", 0:17)]
  
  # simplify age comps
  dat$agecomp <-
    dat$agecomp |>
    # exclude fleets 2 and 3, and only use marginal data for fleet 4
    dplyr::filter(FltSvy %in% c(1, -4)) |>
    # exclude male observations
    dplyr::select(!dplyr::starts_with("m", ignore.case = FALSE)) |>
    # assign observations to ageing error 1
    dplyr::mutate(Gender = 1, Ageerr = 1) |>
    # remove redundant observations
    # NOTE: this removes some age comp data because there
    # were years with multiple observations from the same fleet
    # due to multiple ageing error matrices
    dplyr::distinct(Yr, FltSvy, .keep_all = TRUE)
  
  ## SIMPLIFY SS3 CONTROL FILE
  ctl <- petrale_input$ctl
  ctl$Nages <- 17 # previously 40
  ctl$Nsexes <- 1 # previously 2
  # turn on empirical weight-at-age
  # ctl$EmpiricalWAA <- 1 # previously zero
  # remove male mortality and growth parameters
  ctl$MG_parms <- ctl$MG_parms |>
    dplyr::filter(!grepl("_Mal_", rownames(ctl$MG_parms)))
  ctl$max_bias_adj <- -1 # set bias adjust = 1.0 for all years
  
  # filter catchability stuff to only fleet 4
  ctl$Q_options <- ctl$Q_options |>
    dplyr::filter(fleet == 4)
  ctl$Q_parms <- ctl$Q_parms |>
    dplyr::filter(grepl("WCGBTS", rownames(ctl$Q_parms)))
  
  # add new age-based selex parameters
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
      LO = 1, HI = 10, INIT = c(5, 2, 5, 2),
      PRIOR = 0, PR_SD = 99, PR_type = 0,
      PHASE = 2
    ),
    matrix(0, nrow = 4, ncol = 7)
  )
  names(ctl$age_selex_parms)[8:14] <- names(ctl$size_selex_parms)[8:14]
  rownames(ctl$age_selex_parms) <- paste0("AgeSel_", c("P_1_", "P_2_"), c("fleet1", "fleet1", "fleet4", "fleet4"))
  
  # remove length-based selectivity
  ctl$size_selex_types <- ctl$size_selex_types |>
    dplyr::mutate(Pattern = 0, Discard = 0, Male = 0)
  ctl$size_selex_parms <- NULL
  ctl$size_selex_parms_tv <- NULL
  
  # turn off variance adjustments
  ctl$DoVar_adjust <- 0
  ctl$Variance_adjustment_list <- NULL
  
  ctl$EmpiricalWAA <- 1 # use wtatage file
  
  ## CREATE wtatage.ss file
  # wtatage <- SS_readwtatage("c:/ss/Petrale/Petrale2023/petrale/models//2023.a034.001/wtatage.ss_new")
  # wtatage for fleets -1:0 (population), and fleets 1:4 (real fleets)
  wtatage <- matrix(
    rep(
      c(
        0.0010, # age 0
        0.0148, 0.0617, 0.1449, 0.2570, 0.3876, 0.5260, 0.6640, 0.7957, 0.9175,
        1.0273, 1.1247, 1.2097, 1.2831, 1.3460, 1.3994, 1.4446, 1.4821
      ),
      6
    ),
    nrow = 6, ncol = 18, byrow = TRUE
  )
  # wtatage for fleet = -2 (fecundity * maturity)
  wtatage <- rbind(
    c(
      0, 0, 0, 2.34337e-07, 6.7895e-06, 5.41665e-05, 0.000173396, 0.000338953,
      0.000514454, 0.000681685, 0.000834535, 0.000971472, 0.00109252, 0.00119837,
      0.00129006, 0.00136882, 0.00143602, 0.00149302
    ),
    wtatage
  )
  names(wtatage) <- 0:17
  wtatage <- data.frame(
    Yr = -1800, Seas = 1, Sex = 1, Bio_Pattern = 1,
    BirthSeas = 1, Fleet = -2:4, wtatage
  )
  
  
  if (TRUE) {
    # write modified data and control files
    new_input <- petrale_input
    new_input$dat <- dat
    new_input$ctl <- ctl
    new_input$wtatage <- wtatage
  
    # new_input$dir <- "c:/ss/Petrale/Petrale2023/petrale/models/2023.a050.001_FIMS_case-study"
    new_input$dir <- "c:/ss/Petrale/Petrale2023/petrale/models/2023.a050.002_FIMS_case-study_wtatage"
    # create directory if it doesn't exist
    if (!dir.exists(new_input$dir)) {
      dir.create(new_input$dir)
    }
  
    r4ss::SS_write(new_input, dir = new_input$dir, overwrite = TRUE)
  
    r4ss::run(new_input$dir, show_in_console = TRUE, skipfinished = FALSE)
    p2 <- r4ss::SS_output(new_input$dir)
  }
}