# Bootstrap script to set up reproducible environment
# - Installs renv (and enables pak) for dependency management
# - Installs helper packages (`import`) used in this project

message("Bootstrapping project dependencies...")

# Prefer pak for installs if available
use_pak <- function() {
  requireNamespace("pak", quietly = TRUE)
}

install_if_missing <- function(pkgs) {
  to_install <- setdiff(pkgs, rownames(installed.packages()))
  if (length(to_install)) {
    if (use_pak()) {
      pak::pkg_install(to_install)
    } else {
      install.packages(to_install, repos = getOption("repos", "https://cloud.r-project.org"))
    }
  }
}

# Ensure renv is available
install_if_missing(c("renv"))

# Enable pak for renv
options(renv.config.pak.enabled = TRUE)
Sys.setenv(RENV_CONFIG_PAK_ENABLED = "TRUE")

# Initialize renv if not yet activated
if (!file.exists("renv/activate.R")) {
  message("Initializing renv (bare)...")
  renv::init(bare = TRUE)
}

# Install helper packages commonly used
install_if_missing(c("import"))

message("Bootstrap complete. You can now run renv::snapshot() to record the lockfile.")

