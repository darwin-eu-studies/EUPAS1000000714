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
# Front-end structure
# Define UI --------------------

attritionUI <- function(id,data) {
  
  ns <- NS(id)
  
  
  style <- "display: inline-block;vertical-align:top; width: 200px;"
  fluidRow(
    tabsetPanel(
      type = "tabs",
      tabPanel(
        "Table",
        icon = icon("th"),
        div(
          style = style,
          pickerInput(
            inputId = ns("cdm_name_1"),
            label = "CDM name",
            choices = unique(data$cdm_name),
            selected =  unique(data$cdm_name)[1],
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 0"),
            multiple = FALSE
          )
        ),
        div(
          style = style,
          pickerInput(
            inputId = ns("objective_name_1"),
            label = "Objective name",
            choices = unique(data$objective_name),
            selected =  unique(data$objective_name)[1],
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 0"),
            multiple = FALSE
          )
        ),
        div(
          style = style,
          pickerInput(
            inputId = ns("target_cohort_name_1"),
            label = "Target Cohort Name",
            choices = unique(data$target_cohort_name),
            selected =  unique(data$target_cohort_name)[1],
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 0"),
            multiple = FALSE
          )
        ),
        div(
          style = style,
          pickerInput(
            inputId = ns("outcome_cohort_name_1"),
            label = "Outcome Cohort Name",
            choices = unique(data$outcome_cohort_name),
            selected =  unique(data$outcome_cohort_name)[1],
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 0"),
            multiple = FALSE
          )
        ),
        downloadButton(
          outputId = ns("downloadCohortAttritionTable"),
          label = "Download CSV"
        ),
        reactableOutput(ns("cohortAttritionTable"))
      ),
      
      tabPanel(
        "Diagram",
        icon = icon("chart-line"),
        div(
          style = style,
          pickerInput(
            inputId = ns("cdm_name_2"),
            label = "CDM name",
            choices = unique(data$cdm_name),
            selected =  unique(data$cdm_name)[1],
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 0"),
            multiple = FALSE
          )
        ),
        div(
          style = style,
          pickerInput(
            inputId = ns("objective_name_2"),
            label = "Objective name",
            choices = unique(data$objective_name),
            selected =  unique(data$objective_name)[1],
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 0"),
            multiple = FALSE
          )
        ),
        div(
          style = style,
          pickerInput(
            inputId = ns("target_cohort_name_2"),
            label = "Target Cohort Name",
            choices = unique(data$target_cohort_name),
            selected =  unique(data$target_cohort_name)[1],
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 0"),
            multiple = FALSE
          )
        ),
        div(
          style = style,
          pickerInput(
            inputId = ns("outcome_cohort_name_2"),
            label = "Outcome Cohort Name",
            choices = unique(data$outcome_cohort_name),
            selected =  unique(data$outcome_cohort_name)[1],
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 0"),
            multiple = FALSE
          )
        ),
        downloadButton(
          outputId = ns("downloadCohortAttritionDiagram"),
          label = "Download PNG"
        ),
        box(
          width = 12,
          grVizOutput(ns("cohortAttritionDiagram"), height = "1200px")
        )
      )
    )
  )
  
  
  
}


# Back-end functionality
# Define Server --------------------

attritionServer <- function(id,data) {
  #stopifnot(is.reactive(cdm_name))
  
  
  
  moduleServer(id, function(input,output,session) {
    
    
    # When Objective and Target cohort changes -> recompute Outcome choices ----
    outcome_choices_1 <- reactive({
      req(input$objective_name_1, input$target_cohort_name_1)  # ensure available
      obj <- input$objective_name_1
      tgt <- input$target_cohort_name_1
      
      if (obj == "obj1" && tgt == "overall_cht") {
        "overall_cht"
      } else if (obj == "obj1" && tgt == "overall_cht_sensitivity") {
        "overall_cht_sensitivity"
      } else if (obj == "obj1" && tgt == "primary_cht") {
        "primary_cht"
      } else if (obj == "obj1" && tgt == "secondary_cht") {
        "secondary_cht"
      } else if (obj == "obj2") {
        c(
          "cht_arbs_calcium_c09db",
          "cht_arbs_comb_c09da",
          "cht_arbs_other_c09dx",
          "cht_arbs_plain_c09ca",
          "cht_diuretics_c03",
          "cht_beta_blockers_c07",
          "cht_calcium_blockers_c08",
          "cht_non_sartan_c09"
        )
      } else {
        character(0)
      }
    })
    
    observeEvent(list(input$cdm_name_1, input$objective_name_1, input$target_cohort_name_1), {
      choices <- outcome_choices_1()
      
      selected_safe <- if (!is.null(input$outcome_cohort_name_1) &&
                           input$outcome_cohort_name_1 %in% choices) {
        input$outcome_cohort_name_1
      } else if (length(choices) > 0) {
        choices[[1]]
      } else {
        NULL
      }
      
      shinyWidgets::updatePickerInput(
        session, "outcome_cohort_name_1",
        choices  = choices,
        selected = selected_safe
      )
    }, ignoreInit = TRUE)
    
   
    # get attrition table  -------------------------------
    
    get_attritionTable_data <- reactive({
      
      data %>% 
        filter(cdm_name %in% input$cdm_name_1) %>%
        filter(objective_name %in% input$objective_name_1) %>%
        filter(target_cohort_name %in% input$target_cohort_name_1) %>%
        filter(outcome_cohort_name %in% input$outcome_cohort_name_1)
      
      
    })
    
    
    
    
    output$cohortAttritionTable <- renderReactable({
      summaryTable <- get_attritionTable_data()
      shiny::validate(need(nrow(summaryTable) > 0, "No results for selected inputs"))
      reactable(summaryTable,filterable = T,striped = T,resizable = T, bordered = T,pagination = F, defaultPageSize = nrow(summaryTable))
    })
    
    output$downloadCohortAttritionTable <-  downloadHandler(
      filename = function() {
        paste("cohort_attrition_", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(get_attritionTable_data(), file, row.names = FALSE)
      }
    )
    
    
    get_attritionTable_diagram_data <- reactive({
      data %>%
        filter(cdm_name %in% input$cdm_name_2) %>%
        filter(objective_name %in% input$objective_name_2) %>%
        filter(target_cohort_name %in% input$target_cohort_name_2) %>%
        filter(outcome_cohort_name %in% input$outcome_cohort_name_2)
      
    })
    
    
    get_cohortDefinitionId_diagram <- reactive({
      summaryTable <- get_attritionTable_diagram_data()
      #cohortId <- unique(summaryTable$cohort_definition_id)
      
      #plotCohortAttrition(summaryTable,cohortId = cohortId)
      CohortCharacteristics::plotCohortAttrition(summaryTable)
    })
    
    output$cohortAttritionDiagram <- renderGrViz({
      get_cohortDefinitionId_diagram()
      
    })
    
    
    output$downloadCohortAttritionDiagram <-  downloadHandler(
      filename = function() {
        #cohortId <- as.character(input$outcome_cohort_name)
        cdm_name <- as.character(input$cdm_name)
        paste("attrition_",cdm_name,"_attrition_flos_", Sys.Date(), ".png", sep="")
      },
      content = function(file) {
        # Convert grViz diagram to SVG
        svg <- get_cohortDefinitionId_diagram() %>%
          DiagrammeRsvg::export_svg() %>% 
          charToRaw()
        # Convert the SVG diagram to PNG
        rsvg_png(svg,file = file,width = 800, height = 600)
        
      }
    )
    
    
  })
}




