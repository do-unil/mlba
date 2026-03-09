suppressMessages(suppressWarnings(source("renv/activate.R")))

# Set RETICULATE_PYTHON if needed
if (Sys.getenv("RETICULATE_PYTHON") == "") {
  if (file.exists("~/.conda/envs/MLBA/bin/python")) {
    Sys.setenv(RETICULATE_PYTHON = "~/.conda/envs/MLBA/bin/python")
  } else if (file.exists("~/miniconda3/envs/MLBA/bin/python")) {
    Sys.setenv(RETICULATE_PYTHON = "~/miniconda3/envs/MLBA/bin/python")
  } else if (.Platform$OS.type == "windows") {
    win_paths <- c(
      file.path(Sys.getenv("USERPROFILE"), ".conda/envs/MLBA/python.exe"),
      file.path(Sys.getenv("USERPROFILE"), "miniconda3/envs/MLBA/python.exe"),
      file.path(Sys.getenv("USERPROFILE"), "anaconda3/envs/MLBA/python.exe"),
      file.path(Sys.getenv("LOCALAPPDATA"), "miniconda3/envs/MLBA/python.exe")
    )
    for (p in win_paths) {
      if (file.exists(p)) {
        Sys.setenv(RETICULATE_PYTHON = p)
        break
      }
    }
  }
}

# if (interactive()) {
#   library(reticulate)
#   use_virtualenv("MLBA")
# }

options(expressions = 10000)