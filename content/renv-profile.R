# Detect branch from environment (set in GHA or Git)
branch <- Sys.getenv("GITHUB_REF_NAME")

if (branch == "") {
  # fallback if not set, e.g. when running locally
  branch <- system("git rev-parse --abbrev-ref HEAD", intern = TRUE)
}

# Determine profile
profile <- if (branch != "main") "dev" else NULL

if (!is.null(profile)) {
  message("Activating renv profile: ", profile)
  renv::activate(profile = profile)
} else {
  message("Activating default renv profile")
  renv::activate()
}
