# Copyright 2026 European Medicines Agency
#
# This software is developed by the DARWIN EU Coordination Centre
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Setting up folders ------------------------------------------------------
tag <- "R1_v0.1.1" # Name files with study run number (first run) and GitHub package version used.
resultsFolder <- here::here(glue::glue("results/{dbName}"))
logFolder <- glue::glue("{resultsFolder}/log")
logger <- setupResultsFolder(resultsFolder, dbName) # see utils.R
msgLog("P4-C1-015 DARWIN EU - Childhood hypertension and sartans prescribing in children", type = "header")
date <- gsub("-", "", Sys.Date())

# Run study ------------------------------------------------------

#  Save session info ------------------------------------------------------
msgLog("Saving session information:")
dbms <- CDMConnector::dbms(cdm)
session_file <- glue::glue("{logFolder}/session_info_{tag}_{dbName}_{date}.txt")
capture.output(sessionInfo(), file = session_file)
write(c("DBMS: ", dbms), file = session_file, append = TRUE)

# Save CDM snapshot --------------------------------------------------
msgLog("Saving CDM snapshot information:")
cdm_snapshot <- OmopSketch::summariseOmopSnapshot(cdm)
omopgenerics::exportSummarisedResult(cdm_snapshot, fileName = glue::glue("{resultsFolder}/cdm_snapshot_{tag}_{dbName}_{date}.csv"))

# Generate cohorts --------------------------------------------------------
msgLog("Starting 0_CohortGeneration.R ======", type = "subheader")
source(here::here("R/0_CohortGeneration.R"))
msgLog("Cohort Generation is Complete",type = "success")

# Objective 1 and 2. Prevalence of CHT and prevalence of CHT prescribing --------
tryCatch(
  {
    msgLog("Starting 1_Prevalence.R ======", type = "subheader")
    source(here::here("R/1_Prevalence.R"))
    msgLog("Prevalence analysis is Complete", type = "success")
  },
  error = function(e) {
    writeLines(as.character(e), glue::glue("{logFolder}/error_prevalence_{tag}_{dbName}_{date}.txt"))
  }
)


# Export results ----------------------------------------------------------
msgLog("Exporting ZIP file with results", type = "subheader")
output_zip <- file.path(resultsFolder, glue::glue("study-results_{tag}_{dbName}_{date}.zip"))
zip::zipr(zipfile = output_zip, files = list.files(resultsFolder, full.names = TRUE))
msgLog(paste0("Results saved to: ", resultsFolder, "\n -- Thank you for running the study!"), type = "success")

