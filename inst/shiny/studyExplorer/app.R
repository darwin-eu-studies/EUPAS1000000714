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
library(DarwinShinyModules)
library(shiny)
library(shinydashboard)
library(dplyr)
library(readr)
library(here)
library(DT)
library(reactable)
library(shinyWidgets)
library(tidyr)
library(plotly)
library(shinycssloaders)
library(IncidencePrevalence)
library(ggplot2)
library(gt)
library(PatientProfiles)
library(lubridate)
library(future)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)
library(zoo)
library(scales)
library(glue)
library(visOmopResults)
library(DrugUtilisation)
library(omopgenerics)
library(flextable)
library(openxlsx2)
library(flexlsx)
library(stringr)
library(CohortSurvival)
library(patchwork)
library(mapproj)

source("R/preprocessing.R")
sapply(grep("preprocessing.R",list.files(here::here("R"),pattern = "*.R$", full.names = TRUE),value = TRUE, invert = TRUE), source)


# databaseDetailsMod <- DatabaseDetailsModule$new(data = snapshot)
studyBackMod <- DarwinShinyModules::StudyBackground$new(
  EUPAS = "EUPAS1000000714",
  background = "Childhood hypertension (CHT), defined as elevated blood pressure in children and adolescents, is a significant health concern due to its association with organ damage during childhood, increased risk of hypertension as a young adult, and serious adverse cardiovascular outcomes in adulthood. CHT can be classified into two main categories. Primary hypertension refers to cases without an identifiable underlying cause, while hypertension that results from a specific underlying, potentially reversible cause, is classified as secondary hypertension. Secondary hypertension is frequently caused by coarctation of the aorta or renal diseases, but can also be triggered by other causes. Among the pharmacological options available for managing CHT, angiotensin receptor blockers, commonly referred to as sartans, are among the recommend first-line antihypertensive treatments. However, real-world data on prevalence of CHT and the prescribing patterns of sartans, and other antihypertensive medications in the paediatric populations remain limited. This study aims to generate real-world evidence on the epidemiology of CHT and prescribing patterns of sartans, and other antihypertensive medication among individuals with CHT across Europe to support regulatory decision-making and inform clinical practice")


prevPatienLevelModObj1 <- PrevalenceCustom$new(data = prev_obj1)
prevPatienLevelModObj2 <- PrevalenceCustom$new(data = prev_obj2)

backgroundMenu <- shinydashboard::menuItem(text = "Study background",tabName = "studyBackground")
databaseMenu <- shinydashboard::menuItem(text = "Database details",tabName = "databaseDetails")
attritionMenu <- shinydashboard::menuItem(text = "Cohort Attrition",tabName = "cohortAttrition")
prevPatienLevelMenu <- shinydashboard::menuItem(text = "Prevalence",tabName = "prevalenceTables")



ui <- shinydashboard::dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      backgroundMenu,
      databaseMenu,
      attritionMenu,
      prevPatienLevelMenu
      )
  ), dashboardBody(
    tabItems(
      tabItem(tabName = "studyBackground", studyBackMod$UI()),
      tabItem(tabName = "databaseDetails", databaseDetailsUI("database_details",snapshot)),
      tabItem(tabName = "cohortAttrition", attritionUI("cohort_attrition",attrition)),
      
      tabItem(tabName = "prevalenceTables",
              tabsetPanel(
                id = "prevalence_tabs",
                tabPanel("Prevalence (Obj. 1)", prevPatienLevelModObj1$UI()),
                tabPanel("Prevalence (Obj. 2)", prevPatienLevelModObj2$UI())
              )
      )
    
    )
  )
)


server <- function(input, output, session) {
  studyBackMod$server(input,output,session)
  databaseDetailsServer("database_details",snapshot)
  attritionServer("cohort_attrition",attrition)
  prevPatienLevelModObj1$server(input, output, session)
  prevPatienLevelModObj2$server(input, output, session)
}


shiny::shinyApp(ui = ui, server = server)

