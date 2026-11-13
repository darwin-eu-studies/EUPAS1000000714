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
# 0. CohortGeneration ----
# Study Period ------------------------------------------------------------
if(dbName == "NAJS"){
    study_period_start <- as.Date("2017-01-01")
}else{
    study_period_start <- as.Date("2015-01-01")
    }

if(dbName == "FinOMOP-THL"){
    study_period_end <- as.Date("2023-12-31")
}else{
    study_period_end <- as.Date("2024-12-31")
    }

# Cohort definitions ------------------------------------------------------
msgLog("Importing Cohort Definitions ====")
path_to_json <- here::here("inst/cohorts/json")

cohort_details <- CDMConnector::readCohortSet(path_to_json) %>%
  mutate(cohort_name = case_when(
    cohort_definition_id == 1880 ~ "secondary_cht", # Updated: P4-C1-015 secondary childhood hypertension no prior obs
    cohort_definition_id == 1879 ~ "primary_cht", # Updated: P4-C1-015 primary childhood hypertension no prior obs 
    cohort_definition_id == 1878 ~ "overall_cht", # Updated: P4-C1-015 childhood hypertension overall no prior obs
    cohort_definition_id == 2140 ~ "cht_non_sartan_c09", # P4-C1-015 childhood hypertension + Non-sartan agents acting on the renin-angiotensin system (C09)
    cohort_definition_id == 2035 ~ "cht_calcium_blockers_c08", # P4-C1-015 childhood hypertension +calcium channel blockers (C08)
    cohort_definition_id == 2033 ~ "cht_diuretics_c03", # P4-C1-015 childhood hypertension + diuretics (C03)
    cohort_definition_id == 2034 ~ "cht_beta_blockers_c07", # P4-C1-015 childhood hypertension + beta blocking agents (C07)
    cohort_definition_id == 2032 ~ "cht_arbs_other_c09dx", # P4-C1-015 childhood hypertension + ARBs, other combinations (C09DX)
    cohort_definition_id == 2031 ~ "cht_arbs_comb_c09da", # P4-C1-015 childhood hypertension + ARBs, combination (C09DA) 
    cohort_definition_id == 2030 ~ "cht_arbs_calcium_c09db", # P4-C1-015 childhood hypertension + ARBs and calcium channel blockers (C09DB)
    cohort_definition_id == 2029 ~ "cht_arbs_plain_c09ca", # P4-C1-015 childhood hypertension and angiotensin II receptor blockers, plain (C09CA)
    cohort_definition_id == 2319 ~ "overall_cht_sensitivity", # NEW: P4-C1-015 overall hypertension sensitivity
    TRUE ~ cohort_name # Keep the original value if no condition is met
  )) %>%
  mutate(cohort_name_snakecase = snakecase::to_snake_case(cohort_name))

cohortIds <- cohort_details %>%
  select(cohort_name, cohort_definition_id) %>%
  tibble::deframe()

cohortNames <- names(cohortIds)

# TARGET COHORTS ----------------------------------------------
msgLog("Generating Target Cohorts")
cdm <- CDMConnector::generateCohortSet(
  cdm = cdm,
  cohortSet = cohort_details,
  name = "target_cohorts",
  overwrite = TRUE
)

# DENOMINATOR COHORTS -----------------------------------------------------
for(denominator_name in c("denom_overall_paed","denom_cht_primary","denom_cht_secondary","denom_cht_overall","denom_cht_overall_sensitivity")){

  if(denominator_name == "denom_overall_paed"){
    # Obj1. Denominator paediatric population (>0 and <19). 'targetCohortTable = NULL' in generateTargetDenominatorCohortSet will include all people aged >0 and <19 in the database as defined by ageGroup
    targetCohortTable <- NULL
    targetCohortId <- NULL
  }else{
      #Obj2. Denominator cohorts for primary CHT, secondary and overall
      targetCohortTable <- "target_cohorts"
      targetCohortId <- dplyr::case_when(
        denominator_name == "denom_cht_primary" ~ cohortIds["primary_cht"],
        denominator_name == "denom_cht_secondary" ~ cohortIds["secondary_cht"],
        denominator_name == "denom_cht_overall" ~ cohortIds["overall_cht"],
        denominator_name == "denom_cht_overall_sensitivity" ~ cohortIds["overall_cht_sensitivity"]
        )
  }
  
msgLog(glue::glue("Generating Denominator Cohort - {denominator_name},{targetCohortTable},{targetCohortId}"))
  cdm <- IncidencePrevalence::generateTargetDenominatorCohortSet(
    cdm = cdm,
    name = denominator_name,
    targetCohortTable = targetCohortTable,
    targetCohortId = targetCohortId,
    cohortDateRange = as.Date(c(study_period_start, study_period_end)), # Restrict within study period
    daysPriorObservation = 0, # No prior observation
    sex = c("Both","Male","Female"),
    ageGroup = list(
      c(0, 18), # Overall: >=0 and <19 years;
      c(0,12), # Children (>0 and <13 years); 
      c(13,18) # Adolescents (>= 13 and <19 years)
      ),
    requirementsAtEntry = FALSE, # setting requirementsAtEntry to FALSE would allow an individual to graduate from one age group to another. See https://darwin-eu.github.io/IncidencePrevalence/articles/a03_Creating_target_denominator_populations.html
    requirementInteractions = TRUE
  )
  
  msgLog("Adding Age Group and Sex")
  cdm[[denominator_name]] <- cdm[[denominator_name]] %>% 
    PatientProfiles::addDemographics(
      sex = TRUE,
      ageGroup = list(c(0,12),c(13,18)) # PatientProfiles::addDemographics(): `ageGroup` must not contain overlap between groups (e.g. 0 to 18) 
    ) %>%       
    dplyr::compute(name = denominator_name, temporary = FALSE, overwrite = TRUE)
  
  msgLog("Censor at date of death")
  cdm[[denominator_name]] <- cdm[[denominator_name]] %>%
    PatientProfiles::addDeathDate() %>%
    dplyr::mutate(cohort_end_date = dplyr::if_else(!is.na(date_of_death) & (date_of_death < cohort_end_date), date_of_death, cohort_end_date)) %>% 
    dplyr::filter(cohort_start_date <= cohort_end_date) %>%
    dplyr::compute(name = denominator_name, temporary = FALSE, overwrite = TRUE) %>%
    CDMConnector::recordCohortAttrition("At least 1 observation")

  
}
