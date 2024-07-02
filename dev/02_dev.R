# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Amend DESCRIPTION with dependencies read from package code parsing
## install.packages('attachment') # if needed.
attachment::att_amend_desc()

## Add modules ----
## Create a module infrastructure in R/

# MGT MODULES
golem::add_module(name = "DASHBOARD", with_test = TRUE) # Name of the module
golem::add_module(name = "MANAGE", with_test = TRUE) # Name of the module
golem::add_module(name = "LABELS", with_test = TRUE) # Name of the module
golem::add_module(name = "LABELS_replace", with_test = TRUE) # Name of the module
golem::add_module(name = "SEARCH", with_test = TRUE) # Name of the module

golem::add_module(name = "RP_introduction", with_test = TRUE) # Name of the module
golem::add_module(name = "RP_recut", with_test = TRUE) # Name of the module
golem::add_module(name = "RP_exit", with_test = TRUE) # Name of the module

# PENCIL STEM BLOCK MODULES
golem::add_module(name = "PencilBlock", with_test = TRUE) # Name of the module

golem::add_module(name = "PS_plant", with_test = TRUE) # Name of the module
golem::add_module(name = "PS_survival", with_test = TRUE) # Name of the module
golem::add_module(name = "PS_harvest", with_test = TRUE) # Name of the module

# FIELD SEED BLOCK MODULES
golem::add_module(name = "FS_sale", with_test = TRUE) # Name of the module
golem::add_module(name = "FS_replant", with_test = TRUE) # Name of the module
golem::add_module(name = "scanner", with_test = TRUE) # Name of the module

# SHARED BETWEEN PENCIL AND FIELD MODULES
golem::add_module(name = "planting", with_test = TRUE) # Name of the module
golem::add_module(name = "harvesting", with_test = TRUE) # Name of the module


## Add helper functions ----
## Creates fct_* and utils_*
golem::add_fct("pool", with_test = TRUE)
golem::add_fct("qrscanner", with_test = TRUE)
golem::add_utils("globalVariables", with_test = TRUE)

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file("jsQR")
golem::add_js_handler("handlers")
golem::add_css_file("styles")
golem::add_sass_file("custom")

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw(name = "my_dataset", open = FALSE)

## Tests ----
## Add one line by test you want to create
usethis::use_test("app")

# Pacakges
usethis::use_package("shiny")
usethis::use_package("odbc")
usethis::use_package("markdown")
usethis::use_package("DBI")
usethis::use_package("shinyWidgets")
usethis::use_package("bslib")
usethis::use_package("shinyjs")
usethis::use_package("DT")
usethis::use_package("shinymanager")
usethis::use_package("writexl")
usethis::use_package("config")
usethis::use_package("data.table")
usethis::use_package("shinycssloaders")
usethis::use_package("memoise")
usethis::use_package("glue")
usethis::use_package("baRcodeR")
usethis::use_package("base64enc")
usethis::use_package("dplyr")
usethis::use_package("golem")
usethis::use_package("lubridate")
usethis::use_package("shinyFeedback")
usethis::use_package("stringr")
usethis::use_package("tibble ")

usethis::use_r("fct_pool.R")

# Documentation

## Vignette ----
usethis::use_vignette("cassavaclonetracker")
devtools::build_vignettes()

## Code Coverage----
## Set the code coverage service ("codecov" or "coveralls")
usethis::use_coverage()

# Create a summary readme for the testthat subdirectory
covrpage::covrpage()

## CI ----
## Use this part of the script if you need to set up a CI
## service for your application
##
## (You'll need GitHub there)
usethis::use_github()

# GitHub Actions
usethis::use_github_action()
# Chose one of the three
# See https://usethis.r-lib.org/reference/use_github_action.html
usethis::use_github_action_check_release()
usethis::use_github_action_check_standard()
usethis::use_github_action_check_full()
# Add action for PR
usethis::use_github_action_pr_commands()

# Travis CI
usethis::use_travis()
usethis::use_travis_badge()

# AppVeyor
usethis::use_appveyor()
usethis::use_appveyor_badge()

# Circle CI
usethis::use_circleci()
usethis::use_circleci_badge()

# Jenkins
usethis::use_jenkins()

# GitLab CI
usethis::use_gitlab_ci()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")
