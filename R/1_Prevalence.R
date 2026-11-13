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
# 1. Prevalence (Obj1. and Obj2) =================================
# Objective 1: Estimate annual prevalence of childhood hypertension
# Stratified by age group, sex, and type of hypertension
# 
# Objective 2: Estimate annual prevalence of sartans and other antihypertension prescribing in patients with CHT
# Stratified by drug class age group, sex, and type of hypertension
# Note: drug class stratification is done via generated cohorts per each drug class

estimatePrevalence <- function(obj_name,denominator_table_name, type, target_id, outcome_table) {
  msgLog(glue::glue("{obj_name}: Estimating Annual Prevalence - {type} CHT"))
  
  prev_result <- IncidencePrevalence::estimatePeriodPrevalence(
    cdm = cdm,
    denominatorTable = denominator_table_name,
    outcomeTable = outcome_table,
    outcomeCohortId = target_id,
    interval = c("overall", "years"),
    completeDatabaseIntervals = FALSE,
    level = "person",
    strata = list("age_group","sex"),
    includeOverallStrata = TRUE
  ) %>% omopgenerics::suppress(minCellCount = minCellCount)
  
  msgLog("Check prevalence results are suppressed")
  omopgenerics::isResultSuppressed(prev_result, minCellCount = minCellCount)
  
  # Export prevalence results
  file_prev <- here::here(resultsFolder, glue::glue("prev_{obj_name}_{type}_CHT_{tag}_{dbName}_{date}.csv"))
  msgLog(glue::glue("Exporting prevalence results - {type} CHT"))
  IncidencePrevalence::exportSummarisedResult(prev_result, fileName = file_prev, minCellCount = minCellCount)
  
  # Assign result to environment
  assign(glue::glue("prev_{obj_name}_{type}"), prev_result)
  
  # Export attrition table
  msgLog(glue::glue("Evaluating cohort attrition - {obj_name} {type} CHT"))
  attrition_tbl <- IncidencePrevalence::tablePrevalenceAttrition(prev_result, type = "tibble")
  colnames(attrition_tbl) <- gsub("(.*)(\\])", "", colnames(attrition_tbl))
  file_attrition <- here::here(resultsFolder, glue::glue("attrition_{obj_name}_CHT_{type}_{tag}_{dbName}_{date}.csv"))
  readr::write_csv(attrition_tbl, file_attrition)
}


# Run estimatePrevalence for Objective 1 and 2 ----------------------------
for(denominator_name in c("denom_overall_paed","denom_cht_primary","denom_cht_secondary","denom_cht_overall","denom_cht_overall_sensitivity")){
  
  obj_name <- ifelse(denominator_name == "denom_overall_paed", "obj1","obj2")
  message("Obj:", obj_name, "Denom.:",denominator_name)
  
  target_cohorts <- switch(
    obj_name,
    "obj1" = c("overall_cht", "primary_cht", "secondary_cht","overall_cht_sensitivity"),
    "obj2" = c("cht_arbs_plain_c09ca", "cht_arbs_calcium_c09db", "cht_arbs_comb_c09da", "cht_arbs_other_c09dx","cht_diuretics_c03", "cht_beta_blockers_c07", "cht_calcium_blockers_c08", "cht_non_sartan_c09"), character(0)
  )
  
  target_ids <- cohortIds[target_cohorts]
  if(obj_name == "obj1"){
    # Estimate by type of Hypertension: Overall, Primary, Secondary ----
    # Prevalence - CHT overall ----
    estimatePrevalence(obj_name,
                       denominator_table_name = denominator_name,
                       type =  "overall",
                       target_id = switch(obj_name, "obj1" = target_ids["overall_cht"],"obj2"= target_ids),
                       outcome_table = "target_cohorts")
  # Prevalence - CHT primary ----
  estimatePrevalence(obj_name,
                     denominator_table_name = denominator_name,
                     type = "primary",
                     target_id = switch(obj_name, "obj1" = target_ids["primary_cht"],"obj2"= target_ids),
                     outcome_table = "target_cohorts")
  
  # Prevalence - CHT secondary ----
  estimatePrevalence(obj_name, 
                     denominator_table_name = denominator_name,
                     type = "secondary",
                     target_id = switch(obj_name, "obj1" = target_ids["secondary_cht"],"obj2"= target_ids),
                     outcome_table = "target_cohorts")
    
  # Sensitivity analysis (Overall CHT) ----------------------------------------------------
  estimatePrevalence(obj_name,
                       denominator_table_name = denominator_name,
                       type =  "overall_sensitivity",
                       target_id = switch(obj_name, "obj1" = target_ids["overall_cht_sensitivity"],"obj2"= target_ids),
                       outcome_table = "target_cohorts")
    
  }else{
    # Obj2. Estimate by type of Hypertension: Overall, Primary, Secondary, Overall (sensitivity) ----
    type <- case_when(denominator_name == "denom_cht_primary" ~ "primary",
                      denominator_name == "denom_cht_secondary" ~ "secondary",
                      denominator_name == "denom_cht_overall" ~ "overall",
                      denominator_name == "denom_cht_overall_sensitivity" ~ "overall_sensitivity"
                      ) 
    estimatePrevalence(obj_name,
                       denominator_table_name = denominator_name,
                       type = type,
                       target_id = switch(obj_name, "obj1" = target_ids["overall_cht"],"obj2"= target_ids),
                       outcome_table = "target_cohorts")
  }
}



