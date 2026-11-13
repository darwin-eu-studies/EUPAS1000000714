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
library(dplyr)
library(IncidencePrevalence)
library(omopgenerics)

loadDatabaseResults <- function(dataFolder,dbName){
  
  addCDMLabel <- function(df){
    db_name <- case_when(
      grepl("NAJS",dbName,ignore.case = T) ~ "NAJS", 
      grepl("DK",dbName,ignore.case = T) ~ "DK-DHR", 
      grepl("TaUH",dbName,ignore.case = T) ~ "FinOMOP-TaUH Pirha", 
      grepl("THL",dbName,ignore.case = T) ~ "FinOMOP-THL", 
      grepl("Bordeaux",dbName,ignore.case = T) ~ "CDW Bordeaux", 
      grepl("INGEF",dbName,ignore.case = T) ~ "InGef RDB", 
      grepl("SUCD",dbName,ignore.case = T) ~ "SUCD", 
      grepl("IPCI",dbName,ignore.case = T) ~ "IPCI", 
      grepl("NLHR",dbName,ignore.case = T) ~ "NLHR", 
      grepl("BIFAP",dbName,ignore.case = T) ~ "BIFAP", 
      grepl("SIDIAP",dbName,ignore.case = T) ~ "SIDIAP",
      grepl("(IQVIA-DA|IQVIA Germany)",dbName,ignore.case = T) ~ "IQVIA-DA",
      TRUE ~ dbName
    )
     
    df <- df %>% mutate(cdm_name=db_name)
    return(df)
  }
  
  labelDenominator <- function(df){
    # df <- df %>% mutate(group_level = stringr::str_replace(group_level, "(?<=_)\\d+_", ""))
    set_df <- settings(df) %>% select(result_id,denominator_age_group,denominator_sex)
    df <- df %>% left_join(set_df,by="result_id")
    df <- df %>% mutate(group_level = gsub("(.*)(&&&)","",group_level))
    df <- df %>% mutate(group_level = glue::glue("denominator_ageGroup:{denominator_age_group}_sex:{denominator_sex} &&&{group_level}")) %>% 
      select(-denominator_age_group,-denominator_sex)
    
    return(df)
  }
  
  # cohortDefinitions <- tibble(
  #   
  # )
  
  files <- list.files(dataFolder,pattern = ".csv$",full.names = TRUE)
  
  # Database Details ----------------------------------------------------------
  database_snapshot_file <- grep("/cdm_snapshot_R1",files, value = TRUE)
  snapshot <- omopgenerics::importSummarisedResult(database_snapshot_file) 
  snapshot <- snapshot %>% addCDMLabel()
  
  
  # Load Attrition ----------------------------------------------------------
  attrition_df <- NULL
  df <- NULL
  attrition_files <- grep("attrition",files, value = TRUE)
  for(i in attrition_files){
    message("Loading: ",basename(i))
    
    name <- gsub("attrition_|_v.*.csv","",basename(i))
    obj_name <- ifelse(grepl("obj1",name),"Obj. 1","Obj. 2")
    target_cohort_name <- case_when(
      grepl("overall_R",name) ~ "overall_cht",
      grepl("overall_sensitivity",name) ~ "overall_cht_sensitivity",
      grepl("primary",name) ~ "primary_cht",
      grepl("secondary",name) ~ "secondary_cht"
    )
    
    # attrition_label <- case_when(
    #   name == "overall_cht" ~ "tetanus_immunoglobulins_prior_obs",
    #   
    # )
    df <- readr::read_csv(
      i, show_col_types = FALSE, 
      col_types = readr::cols(.default = readr::col_character()))
    
    # df <- df %>% mutate(type = attrition_label)
    
    obj_name <- stringr::str_extract(string = i,pattern = "obj.")
    message("name:", name)
    message("obj: ", obj_name)
    if(!("objective_name" %in% colnames(df))){ # Bind with attrition tables including objective name column
      df <- df %>% mutate("Objective name"= obj_name) %>% relocate(`Objective name`, .after = "CDM name")
    }
    
    # colnames(df) <- colnames(df) %>% omopgenerics::toSnakeCase()
    # colnames(attrition_df) <- colnames(df)
    
    df <- df %>% mutate("Target cohort name"= target_cohort_name) %>% relocate(`Target cohort name`, .after = "Objective name")
    attrition_df <- rbind(attrition_df,df)
  }
  
  attrition_df <- attrition_df %>%
    filter(`Denominator cohort name` == "denominator_cohort_1", 
           `Denominator age group` == "0 to 18",
           `Denominator sex` == "Both"
           ) %>%
    select(-`Denominator cohort name`)# age group 0 to 18, sex both
  
  
  colnames(attrition_df) <- colnames(attrition_df) %>% omopgenerics::toSnakeCase()
  
  # Transform to attrition summarised result object ----
  # Attrition object requires a different value of 'cohort_definition_id' for each selection
  cohort_def_attrition <- attrition_df %>% select(objective_name,target_cohort_name,outcome_cohort_name) %>% distinct() %>% mutate(cohort_definition_id = row_number()) %>% relocate(cohort_definition_id) %>% as.data.frame()
  attrition_df <- left_join(attrition_df,cohort_def_attrition, by=c("objective_name","target_cohort_name","outcome_cohort_name"))
  attrition_df <- attrition_df %>% relocate(cohort_definition_id)
  
# Load Prevalence files --------------------------------------------------------
  # Prevalence Obj1. ------
  prev_pat_level_file <- grep("prev_",files, value = TRUE)
  prev_obj1_files <- grep("_obj1_",prev_pat_level_file,value=T)
  if(length(prev_obj1_files) == 0){
    message(paste0(dbName,": Prevalence file missing, skipped loading."))
    prev_pat_obj1 <- NULL 
  }else{
    message(paste0(dbName,": Loading prevalence file."))
    prev_pat_obj1 <- IncidencePrevalence::importSummarisedResult(prev_obj1_files) %>% labelDenominator() %>% addCDMLabel()
  }
  
  # Prevalence Obj2. ------
  prev_obj2_files <- grep("_obj2_",prev_pat_level_file,value=T)
   if(length(prev_obj2_files) == 0){
    message(paste0(dbName,": Prevalence file missing, skipped loading."))
     prev_pat_obj2 <- NULL 
  }else{
    message(paste0(dbName,": Loading prevalence file."))
    prev_pat_obj2 <- IncidencePrevalence::importSummarisedResult(prev_obj2_files) %>% labelDenominator() %>% addCDMLabel()
    }
  # Clean labels Obj2. ------
  message("Changing obj2. labels")
  mapping <- c(
    "cht_arbs_plain_c09ca"    = "Sartans (C09CA)",
    "cht_arbs_calcium_c09db"  = "Sartans (C09DB)",
    "cht_arbs_comb_c09da"     = "Sartans (C09DA)",
    "cht_arbs_other_c09dx"    = "Sartans (C09DX)",
    "cht_diuretics_c03"       = "Diuretics (C03)",
    "cht_beta_blockers_c07"   = "Beta blocking agents (C07)",
    "cht_calcium_blockers_c08"= "Calcium channel blockers (C08)",
    "cht_non_sartan_c09"      = "Non-sartans (C09)"
  )
  
  prev_pat_obj2$group_level <- stringr::str_replace_all(prev_pat_obj2$group_level, mapping)
  
  
  # Assign censor data labels '<5': ----
  # prev_obj1 %>% mutate(
  #   estimate_value = ifelse(
  #     grepl("(denominator_count|outcome_count)",estimate_name) & is.na(estimate_value), '<5',as.character(estimate_value)
  #   ))
                                           
  # Save results
  results_db <- list(
    "snapshot" = snapshot,
    "attrition" = attrition_df,
    "prev_obj1" = prev_pat_obj1,
    "prev_obj2" = prev_pat_obj2
    )
  return(results_db)
}




#--------------------------------------------------------------------------------------------------------------------
# LOAD RESULTS FROM ALL DPs AND GENERATE data.rds (reload it if exists)
#--------------------------------------------------------------------------------------------------------------------#
dataOutputFolder <- here::here("data")
dataFolder <- normalizePath(here::here("../../../results"))
results_db <- list()

# Functions to preprocess the data again (recreate data.rds from csv files) ------------------
# Based on PhenotypeR: https://github.com/OHDSI/PhenotypeR/blob/ade8a84dfdcdbbb4db46a187c45c9c20ee1b0cd6/inst/shiny/global.R#L37

# preprocess_again <- function(){
#   cli::cli_inform("Would you like to preprocess the data again?")
#   choice <- utils::menu(c("Yes", "No"))
#   choice == 1  # TRUE if Yes, FALSE if No
# }
# 
# is_shiny <- requireNamespace("shiny", quietly = TRUE) && shiny::isRunning()
# 
# if (!file.exists(file.path(dataOutputFolder, "data.rds"))) {
#   process_again <- TRUE
# } else if (file.exists(file.path(dataOutputFolder, "data.rds"))){ #&&  rlang::is_interactive() && !is_shiny) {
#   process_again <- preprocess_again()
# } else {
#   process_again <- FALSE
# }


# If process_again is TRUE, load database results to 'data/data.rds'
# if (isTRUE(process_again)) {

if(!file.exists(file.path(dataOutputFolder, "data.rds"))) {
  
  results_db <- list()
  
  for (dbName in list.files(dataFolder)) {
  
    cli::cli_alert_info("Loading data from {dbName} to data.rds file")
    databaseFolder <- paste0(dataFolder,"/",dbName)
    results_db_i <- loadDatabaseResults(dataFolder = databaseFolder, dbName)
    
    tab_order <- c("snapshot","attrition","prev_obj1","prev_obj2")
    
    if (length(results_db) == 0) {
      results_db <- results_db_i
    } else {
      results_db <- purrr::map2(results_db[tab_order], results_db_i[tab_order], function(x, y) {
        if (!is.null(x) && !is.null(y)) {
          if ("summarised_result" %in% class(x)) {
            omopgenerics::bind(x, y)
          } else {
            rbind(x, y)
          }
        } else {
          x %||% y # null coalescing (return x if not null, otherwise return y)
        }
      })
    }
  }
  
  # Database details ----
  snapshot <- results_db$snapshot
  # Cohort attrition ----
  attrition <- results_db$attrition 
  # Prevalence
  prev_obj1 <- results_db$prev_obj1
  prev_obj2 <- results_db$prev_obj2
  
  # Save data ----
  itemsToSave <- list(
    "snapshot" = snapshot,
    "attrition" = attrition,
    "prev_obj1" = prev_obj1,
    "prev_obj2" = prev_obj2
  )
  
  if (!dir.exists(dataOutputFolder)) {
    dir.create(dataOutputFolder, recursive = TRUE)
  }
  
  saveRDS(itemsToSave, paste0(dataOutputFolder,"/data.rds"))
  
}else{
  message("Loading results from data.rds file")
  results <- readRDS(paste0(dataOutputFolder,"/data.rds"))
  snapshot <- results$snapshot
  attrition <- results$attrition
  prev_obj1  <- results$prev_obj1
  prev_obj2  <- results$prev_obj2
  rm(results)
}

