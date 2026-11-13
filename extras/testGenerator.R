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
### Restore the project library
# renv::activate()
# renv::restore()

### Load packages
library(dplyr)
library(here)
library(glue)
library(readr)
library(stringr)
library(CDMConnector)
library(CirceR)
library(CodelistGenerator)
library(CohortCharacteristics)
library(CohortConstructor)
library(DrugUtilisation)
library(IncidencePrevalence)
library(OmopSketch)
library(omopgenerics)
library(DBI)
library(odbc)
library(log4r)
library(gt)
library(TestGenerator)
source("R/utils.R")

# Connection details
dbName <- "testData"
filePath <- "extras/test/TestData_P4_C1-015_for DS.xlsx"
project <- "P4-C1-015"
date <- gsub("-", "", Sys.Date())
outputPath <- file.path(glue::glue("results/test_{project}_{date}"))
minCellCount <- 0
fs::dir_create(outputPath)

TestGenerator::readPatients(filePath = filePath, 
                            testName = "test", 
                            outputPath = outputPath, 
                            cdmVersion = "5.3")

cdm <- TestGenerator::patientsCDM(pathJson = outputPath, 
                                  testName = "test",
                                  cdmVersion = "5.3",
                                  cdmName = dbName)


### Run Analysis ----------------------------------------------
source(here::here("R/RunAnalysis.R"))

