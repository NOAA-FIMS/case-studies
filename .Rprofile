branch <- Sys.getenv("GITHUB_REF_NAME")
if (branch == "") {
  branch <- system("git rev-parse --abbrev-ref HEAD", intern = TRUE)
}
profile <- if (grepl("main", branch)) "default" else "dev"
Sys.setenv(RENV_PROFILE = profile)
source("renv/activate.R")
