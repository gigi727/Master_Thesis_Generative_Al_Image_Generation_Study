required_packages <- c(
  "tidyverse",
  "readr",
  "writexl",
  "here",
  "gt",
  "janitor",
  "broom.mixed",
  "lme4",
  "lmerTest",
  "emmeans",
  "ordinal",
  "knitr",
  "scales"
)

installed_packages <- rownames(installed.packages())
missing_packages <- setdiff(required_packages, installed_packages)

if (length(missing_packages) > 0) {
  install.packages(missing_packages, dependencies = TRUE)
}

invisible(required_packages)
