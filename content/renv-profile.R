# Detect branch from environment (set in GHA or Git)
branch <- Sys.getenv("GITHUB_REF_NAME")

if (branch == "") {
  # fallback if not set, e.g. when running locally
  branch <- system("git rev-parse --abbrev-ref HEAD", intern = TRUE)
}

# Determine profile
profile <- if (branch == "main") NULL else "dev"

message("Overriding renv activation with profile: ", if (is.null(profile)) "default" else profile)
renv::activate(profile = profile)