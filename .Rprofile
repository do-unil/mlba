source("renv/activate.R")

# Set RETICULATE_PYTHON if needed
if (Sys.getenv("RETICULATE_PYTHON") == "") {
  if (file.exists("~/.conda/envs/MLBA/bin/python")) {
    Sys.setenv(RETICULATE_PYTHON = "~/.conda/envs/MLBA/bin/python")
  } else if (file.exists("~/miniconda3/envs/MLBA/bin/python")) {
    Sys.setenv(RETICULATE_PYTHON = "~/miniconda3/envs/MLBA/bin/python")
  }
}

# if (interactive()) {
#   library(reticulate)
#   use_virtualenv("MLBA")
# }

options(expressions = 10000)