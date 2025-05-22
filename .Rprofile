# .Rprofile
source("renv/activate.R")

# Minimal automatic setup
get_git_branch <- function() {
  if (file.exists(".git/HEAD")) {
    branch_info <- readLines(".git/HEAD", n = 1)
    if (grepl("ref: refs/heads/", branch_info)) {
      return(sub("ref: refs/heads/", "", branch_info))
    }
  }
  return("main")
}

# Set profile based on branch
current_branch <- get_git_branch()
if (current_branch == "main") {
  # Use default profile
  Sys.unsetenv("RENV_PROFILE")
  message("Using default renv profile for main branch")
} else {
  # Use dev profile
  Sys.setenv(RENV_PROFILE = "dev")
  message("Using dev renv profile for branch: ", current_branch)
}
