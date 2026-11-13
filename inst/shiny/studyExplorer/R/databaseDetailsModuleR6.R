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
# R/DatabaseDetailsModule.R

DatabaseDetailsModule <- R6::R6Class(
  classname = "DatabaseDetailsModule",
  inherit   = DarwinShinyModules::ShinyModule,  # <- inherit from the base
  public = list(
    title = NULL,
    icon  = NULL,
    data  = NULL,
    
    initialize = function(data,
                          title = "Study databases",
                          icon  = "th") {
      super$initialize()                         # call base init if present
      stopifnot(is.data.frame(data))
      if (!"cdm_name" %in% names(data)) {
        stop("`data` must contain a 'cdm_name' column.")
      }
      self$data  <- data
      self$title <- title
      self$icon  <- icon
    },
    
    # Accessors some shells use in navigation
    getTitle = function() self$title,
    getIcon  = function() self$icon
  ),
  
  private = list(
    
    # ---- UI hook called by base ui(id)
    .ui = function(ns) {
      shiny::tagList(
        shiny::h4(self$title),
        
        shiny::div(
          style = "display:inline-block; vertical-align:top; width:240px; margin-right:10px;",
          shinyWidgets::pickerInput(
            inputId  = ns("cdm_name"),
            label    = "CDM name",
            choices  = sort(unique(self$data$cdm_name)),
            selected = sort(unique(self$data$cdm_name)),
            options  = list(
              `actions-box` = TRUE,
              size = 10,
              `selected-text-format` = "count > 0",
              liveSearch = TRUE
            ),
            multiple = TRUE
          )
        ),
        
        shiny::div(
          style = "display:inline-block; vertical-align:top; margin-left:6px;",
          shiny::downloadButton(ns("downloadDatabaseDetails"), "Download")
        ),
        
        shiny::br(), shiny::br(),
        gt::gt_output(ns("databaseDetailsTable"))
      )
    },
    
    # ---- Server hook called by base server(id)
    .server = function(input, output, session) {
      
      filtered_data <- shiny::reactive({
        req(input$cdm_name)
        dplyr::filter(self$data, .data$cdm_name %in% input$cdm_name)
      })
      
      output$databaseDetailsTable <- gt::render_gt({
        df <- filtered_data()
        shiny::validate(
          shiny::need(nrow(df) > 0, "No results for selected inputs")
        )
        OmopSketch::tableOmopSnapshot(df, type = "gt")
      })
      
      output$downloadDatabaseDetails <- shiny::downloadHandler(
        filename = function() paste0("database_details_", Sys.Date(), ".docx"),
        content  = function(file) {
          df <- filtered_data()
          gt_tbl <- OmopSketch::tableOmopSnapshot(df, type = "gt")
          gt::gtsave(data = gt_tbl, filename = file)
        }
      )
    }
  )
)
