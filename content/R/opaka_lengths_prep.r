library(r4ss)

#read in data and control files of original model 
opaka_mod_dir <- file.path(getwd(), "..", "Opaka-FIMS-Case-Study", "Model", "01_original_model")
#C:/Users/Megumi.Oshima/Documents/Opaka-FIMS-Case-Study/Model/01_original_model
opaka_dat <- SS_readdat_3.30(file.path(opaka_mod_dir, "data.ss"))
opaka_ctl <- SS_readctl_3.30(file.path(opaka_mod_dir, "control.ss"), datlist = file.path(opaka_mod_dir, "data.ss"))

#create directory for new simplified model
opaka_length_dir <- file.path(opaka_mod_dir, "..", "09_case_study_lengths")
dir.create(opaka_length_dir)

#remove size freq data
opaka_dat$N_sizefreq_methods_rd <- 0
opaka_dat$N_sizefreq_methods <- NULL
opaka_dat$nbins_per_method <- NULL
opaka_dat$units_per_method <- NULL
opaka_dat$scale_per_method <- NULL
opaka_dat$mincomp_per_method <- NULL
opaka_dat$Nobs_per_method <- NULL
opaka_dat$Comp_Error_per_method <- NULL
opaka_dat$ParmSelect_per_method <- NULL
opaka_dat$sizefreq_bins_list <- NULL
opaka_dat$sizefreq_data_list <- NULL

#remove superperiods for length comp data
len_dat_original <- read.csv(file.path(opaka_mod_dir, "..", "..", "Data", "Opaka_len_data.csv"))
opaka_dat$lencomp <- len_dat_original

#remove dirichlet weighting for length comps
opaka_dat$len_info$CompError <- 0
opaka_dat$len_info$ParmSelect <- 0

#remove initial F estimation
opaka_dat$catch[1,'catch'] <- 0

#add agecomp dummy data
opaka_dat$N_agebins <- 21
opaka_dat$agebin_vector <- seq(1, 21)
opaka_dat$N_ageerror_definitions <- 1
opaka_dat$ageerror <- rbind(
    seq(0.5, 43.5), 
    rep(.01, 44)
)
opaka_dat$age_info <- data.frame(
    mintailcomp <- rep(0,4),
    addtocomp = 1e-7,
    combine_M_F = 1, 
    CompressBins = 0,
    CompError = 0,
    ParmSelect = 0, 
    minsamplesize = 1
)
opaka_dat$Lbin_method <- 1
agecomp_info <- data.frame(
    year = rep(seq(2017, 2023), 2),
    month = 1,
    fleet = rep(c(2,4), each = 7), 
    sex = 0, 
    part = 0, 
    ageerr = 1, 
    Lbin_lo = -1,
    Lbin_hi = -1, 
    Nsamp = 10
)
dummy_agecomp <- as.data.frame(matrix(data = 1, nrow = nrow(agecomp_info), ncol = length(opaka_dat$agebin_vector)))
colnames(dummy_agecomp) <-  paste0("a", opaka_dat$agebin_vector)
opaka_dat$agecomp <- cbind(agecomp_info, dummy_agecomp)

SS_writedat_3.30(datlist = opaka_dat, outfile = file.path(opaka_length_dir, "data.ss"), overwrite = T)

#remove growth platoon
opaka_ctl$N_platoon <- 1
opaka_ctl$sd_ratio <- NULL
opaka_ctl$submorphdist <- NULL 

#remove intial F estimation
opaka_ctl$init_F <- NULL

#remove extra SE parameter
opaka_ctl$Q_options[1,'extra_se'] <- 0
opaka_ctl$Q_parms <- opaka_ctl$Q_parms[-2,]
opaka_ctl$Variance_adjustment_list <- NULL
opaka_ctl$DoVar_adjust <- 0
opaka_ctl$sd_offset <- 0

#remove dirichlet weighting parameter lines
opaka_ctl$dirichlet_parms <- NULL

#fix commercial selectivity
opaka_ctl$size_selex_parms$PHASE[1:2] <- -2

#add age selectivity
opaka_ctl$age_selex_types <- data.frame(
    Pattern = rep(12, 4),
    Discard = 0,
    Male = 0,
    Special = 0
)

# opaka_ctl$age_selex_parms <-  data.frame(
#     "LO" = c(0,-5,0,0,0,-10,0,-20),
#     "HI" = c(40,50,40,40,60,60,10,50),
#     "INIT" = c(1.81975, 0.0093046, 1, 3, 1.97182, 0.00040, 1.29111, 0.00115), 
#     "PRIOR" = c(5,6,5,6,5,6,2,.5),
#     "PR_SD" = c(99,99,99,99,99,99,5,2),
#     "PR_type" = 0, 
#     "PHASE" = c(-2,-2,-2,-2,-99,-99,-2,-2),
#     "env-var" = 0, 
#     "use_dev" = 0,
#     "dev_mnyr" = 0,
#     "dev_mxyr" = 0,
#     "dev_PH" = 0,
#     "Block" = 0,
#     "Block_Fxn" = 0
# ) #control file wouldn't write when I manually add the age_selex_params

age_ctl <- SS_readctl_3.30(file.path(opaka_mod_dir, "..", "03_age_comps", "control.ss_new"), datlist = file.path(opaka_mod_dir, "..", "03_age_comps", "data.ss"))
age_selex_params <- age_ctl$age_selex_parms
opaka_ctl$age_selex_parms <- age_selex_params
opaka_ctl$age_selex_parms$PHASE <- -2

SS_writectl_3.30(opaka_ctl, outfile = file.path(opaka_length_dir, "control.ss"), overwrite = T)

ss_files <- c("forecast.ss", "starter.ss", "ss_opt_win.exe")
file.copy(file.path(opaka_mod_dir, ss_files), opaka_length_dir)

#create a bootstrap data file to get age comp data
start <- SS_readstarter(file.path(opaka_length_dir, "starter.ss"))
start$N_bootstraps <- 3
SS_writestarter(start, dir = opaka_length_dir, overwrite = T)

#run SS
run(dir = opaka_length_dir, exe = "ss_opt_win.exe", skipfinished = F)

file.copy(file.path(opaka_length_dir, "data_boot_001.ss"), file.path(opaka_length_dir, "data.ss"), overwrite = T)
start <- SS_readstarter(file.path(opaka_length_dir, "starter.ss"))
start$N_bootstraps <- 1
SS_writestarter(start, dir = opaka_length_dir, overwrite = T)

#run SS
run(dir = opaka_length_dir, exe = "ss_opt_win.exe", skipfinished = F)

#check model 
rep <- SS_output(dir = opaka_length_dir)
SS_plots(rep)

#package up data, control and rep file for using in FIMS
rm("opaka_dat")
rm("opaka_ctl")
opaka_dat <- SS_readdat_3.30(file.path(opaka_length_dir, "data.ss"))
opaka_ctl <- SS_readctl_3.30(file.path(opaka_length_dir, "control.ss"), datlist = file.path(opaka_length_dir, "data.ss"))
save(list = c("opaka_dat", "opaka_ctl", "rep"), file = file.path(opaka_length_dir, "opaka_length.RDS"))
