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
# Install Renv
install.packages("renv")
install.packages("CirceR")

# Activate renv, if not already activated.
renv::activate()

# Restore the packages.
renv::restore()

# Load Libraries
library(OmopSketch)
library(omopgenerics)
library(CDMConnector)
library(PatientProfiles)
library(IncidencePrevalence)
library(odbc)
library(dplyr)
library(here)
library(log4r)
source("R/utils.R")



# [*] EDIT BELOW ==============================================================

# connection details for DatabaseConnector
options(sqlRenderTempEmulationSchema = "") # Needed for Snowflake rendering

dbName <- "" # Options: CDWBordeaux, CPRD-GOLD, CK-DHR, NAJS, IQVIA-DA
cdmSchema <- ""
writeSchema <- ""
tablePrefix <- ""
minCellCount <- 5

con <- DBI::dbConnect(...)


cdm <- CDMConnector::cdmFromCon(con=con,
                                cdmSchema = cdmSchema,
                                writeSchema = writeSchema, 
                                cdmName = dbName)

# [*] RUN STUDY ===============================================================

source(here::here("R/RunAnalysis.R"))

# [*] VIEW RESULTS ============================================================
# See 'Launch the Shiny app' in instructions.md

