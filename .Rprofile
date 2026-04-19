source("renv/activate.R")
options(
  shiny.launch.browser = FALSE,
  repos = c(
    P3M = sprintf(
      "https://p3m.dev/cran/latest/bin/linux/manylinux_2_28-%s/%s",
      R.version["arch"],
      substr(getRversion(), 1, 3)
    ),
    CRAN = "https://cloud.r-project.org"
  )
)
