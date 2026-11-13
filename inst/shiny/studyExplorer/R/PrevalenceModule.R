# Copyright 2026 DARWIN EU®
#
# This file is part of DarwinShinyModules
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

#' @title Prevalence Module Class
#'
#' @include ShinyModule.R
#'
#' @description
#' Prevalence module that shows prevalence results from the IncidencePrevalence package.
#'
#' @export
#'
#' @examples{
#' \donttest{
#'  library(DarwinShinyModules)
#'
#'  if (
#'    require(
#'      "IncidencePrevalence",
#'      character.only = TRUE,
#'      quietly = TRUE,
#'      warn.conflicts = FALSE
#'    )
#'  ) {
#'     inc <- omopgenerics::importSummarisedResult(system.file(
#'       package = "DarwinShinyModules",
#'       "dummyData/IncidencePrevalence/1.2.0/prevalence.csv"
#'     ))
#'
#'     prevMod <- Prevalence$new(data = inc)
#'
#'     ui <- shiny::fluidPage(
#'       prevMod$UI()
#'     )
#'
#'     server <- function(input, output, session) {
#'       prevMod$server(input, output, session)
#'     }
#'
#'     if (interactive()) {
#'       shiny::shinyApp(ui = ui, server = server)
#'     }
#'   }
#' }
#' }
PrevalenceCustom <- R6::R6Class(
  classname = "Prevalence",
  inherit = DarwinShinyModules::ShinyModule,
  
  # Active ----
  active = list(
    #' @field data (`summarisedResult`) SummarisedResult object from Prevalence.
    data = function(data) {
      if (missing(data)) {
        return(private$.data)
      } else {
        # Checks on data
        checkmate::assertClass(data, "summarised_result")
        private$.data <- data
      }
    },
    
    #' @field pickers (`list`) List of pickers
    pickers = function() {
      return(private$.pickers)
    }
  ),
  
  # Public ----
  public = list(
    
    #' @description
    #' Initializer method
    #'
    #' @param data (`summarised_result`) Result object from the `IncidencePrevalence` package.
    #' @param ... Additional parameters to set fields from the `ShinyModule` parent.
    #'
    #' @returns `self`
    initialize = function(data, ...) {
      super$initialize(...)
      private$assertInstall("IncidencePrevalence", "1.2.0")
      private$assertInstall("visOmopResults", "1.0.2")
      private$assertPrevalenceData(data)
      private$.data <- data
      private$.tidyData <- private$transformData(data)
      private$initPickers()
      return(invisible(self))
    }
  ),
  
  # Private ----
  private = list(
    .data = NULL,
    .tidyData = NULL,
    .strata = NULL,
    .pickers = NULL,
    .db_palette = NULL,   # <-- AM: add this to allow setting same colors for each database
    .db_levels  = c("NAJS","DK-DHR","FinOMOP Tampere","FinOMOP THL","CDW Bordeaux","InGef RDB","SUCD","IPCI","NLHR","BIFAP","SIDIAP"), #NULL,   # <-- AM: and this
    .UI = function() {
      shiny::tagList(
        shinydashboard::tabItem(
          tabName = shiny::NS(private$.namespace, "prevalence"),
          shiny::h3("Prevalence estimates"),
          shiny::p("Prevalence estimates are shown below, please select configuration to filter them:"),
          shiny::p("Database, study outcome and strata"),
          private$.pickers[["cdm"]]$UI(),
          private$.pickers[["outcome"]]$UI(),
          # private$.pickers[["strata"]]$UI(), #AM this was split into strata_age_group and strata_sex
          private$.pickers[["ageGroup"]]$UI(), #AM 
          private$.pickers[["strataSex"]]$UI(), #AM 
          p("Denominator population settings"),
          private$.pickers[["denomTargetCohortName"]]$UI(),#AM: Include denominator target cohort name
          private$.pickers[["denomAgeGroup"]]$UI(),
          private$.pickers[["denomSex"]]$UI(),
          private$.pickers[["denomPriorObs"]]$UI(),
          private$.pickers[["denomStartDate"]]$UI(),
          private$.pickers[["denomEndDate"]]$UI(),
          private$.pickers[["denomTimeAtRisk"]]$UI(),
          p("Analysis settings"),
          private$.pickers[["analysisType"]]$UI(),
          private$.pickers[["completePeriod"]]$UI(),
          private$.pickers[["fullContribution"]]$UI(),
          private$.pickers[["minCounts"]]$UI(),
          p("Dates"),
          private$.pickers[["interval"]]$UI(),
          private$.pickers[["startDate"]]$UI(),
          shiny::tabsetPanel(
            id = shiny::NS(private$.namespace, "tabsetPanel"),
            type = "tabs",
            shiny::tabPanel(
              "Tidy Table",
              private$.pickers[["headerColumn"]]$UI(),
              private$.pickers[["groupColumn"]]$UI(),
              private$.pickers[["settingsColumn"]]$UI(),
              private$.pickers[["hideColumn"]]$UI(),
              p(),
              shiny::downloadButton(shiny::NS(private$.namespace, "downloadTidyTable"), "Download table"),
              p(),
              gt::gt_output(shiny::NS(private$.namespace, "tidyTable")) %>% shinycssloaders::withSpinner()
            ),
            shiny::tabPanel(
              "Plot",
              p("Plotting options"),
              private$.pickers[["xAxis"]]$UI(),
              private$.pickers[["facet"]]$UI(),
              private$.pickers[["color"]]$UI(),
              private$.pickers[["ribbon"]]$UI(),
              private$.pickers[["confInterval"]]$UI(),
              plotly::plotlyOutput(
                shiny::NS(private$.namespace, "plot"),
                height = "800px"
              ) %>%
                shinycssloaders::withSpinner(),
              shiny::h4("Download figure"),
              shiny::div("height:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
              shiny::div(
                style = "display: inline-block;",
                shiny::textInput(shiny::NS(private$.namespace, "download_height"), "", 10, width = "50px")
              ),
              shiny::div("cm", style = "display: inline-block; margin-right: 25px;"),
              shiny::div("width:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
              shiny::div(
                style = "display: inline-block;",
                shiny::textInput(shiny::NS(private$.namespace, "download_width"), "", 20, width = "50px")
              ),
              shiny::div("cm", style = "display: inline-block; margin-right: 25px;"),
              shiny::div("dpi:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
              shiny::div(
                style = "display: inline-block; margin-right:",
                shiny::textInput(shiny::NS(private$.namespace, "download_dpi"), "", 300, width = "50px")
              ),
              shiny::downloadButton(shiny::NS(private$.namespace, "download_plot"), "Download plot")
            ),
            shiny::tabPanel(
              "Table",
              shiny::downloadButton(shiny::NS(private$.namespace, "downloadTable"), "Download current estimates"),
              DT::DTOutput(shiny::NS(private$.namespace, "table")) %>% shinycssloaders::withSpinner()
            )
          )
        )
      )
    },
    .server = function(input, output, session) {
      for (module in private$.pickers) {
        module$server(input, output, session)
      }
      
      # AM: Assign colors to databases: ----
      # Build stable levels and palette from the full tidy data (not filtered)
      private$.db_levels <- sort(unique(private$.tidyData$database))
      
      # Pick a deterministic set of colours of the right length
      base_cols <- scales::hue_pal(l = 60, c = 100)(length(private$.db_levels))
      
      # Name the palette by database
      private$.db_palette <- stats::setNames(base_cols, private$.db_levels)
      
      # Prevalence
      getPrevalenceEstimates <- reactive({
        result <- private$.tidyData %>%
          dplyr::filter(database %in% private$.pickers[["cdm"]]$inputValues$cdm) %>%
          dplyr::filter(outcome_cohort_name %in% private$.pickers[["outcome"]]$inputValues$outcome) %>%
          # dplyr::filter(strata %in% private$.pickers[["strata"]]$inputValues$strata) %>%
          dplyr::filter(age_group %in% private$.pickers[["ageGroup"]]$inputValues$strata_age_group) %>% # AM filter by strata: age group
          dplyr::filter(sex %in% private$.pickers[["strataSex"]]$inputValues$strata_sex) %>% # AM filter by strata: sex
          dplyr::filter(denominator_target_cohort_name %in% private$.pickers[["denomTargetCohortName"]]$inputValues$denominator_target_cohort_name) %>% #AM: Include denominator target cohort name
          dplyr::filter(denominator_age_group %in% private$.pickers[["denomAgeGroup"]]$inputValues$age_group) %>%
          dplyr::filter(denominator_sex %in% private$.pickers[["denomSex"]]$inputValues$denom_sex) %>%
          dplyr::filter(denominator_days_prior_observation %in% private$.pickers[["denomPriorObs"]]$inputValues$prior_obs) %>%
          dplyr::filter(denominator_start_date %in% private$.pickers[["denomStartDate"]]$inputValues$start_date) %>%
          dplyr::filter(denominator_end_date %in% private$.pickers[["denomEndDate"]]$inputValues$end_date) %>%
          dplyr::filter(denominator_time_at_risk %in% private$.pickers[["denomTimeAtRisk"]]$inputValues$time_at_risk) %>%
          dplyr::filter(analysis_type %in% private$.pickers[["analysisType"]]$inputValues$analysis_type) %>%
          dplyr::filter(analysis_complete_database_intervals %in% private$.pickers[["completePeriod"]]$inputValues$complete_period) %>%
          dplyr::filter(analysis_full_contribution %in% private$.pickers[["fullContribution"]]$inputValues$full_contribution) %>%
          dplyr::filter(analysis_min_cell_count %in% private$.pickers[["minCounts"]]$inputValues$min_cell_count) %>%
          dplyr::filter(analysis_interval %in% private$.pickers[["interval"]]$inputValues$interval) %>%
          dplyr::filter(prevalence_start_date %in% private$.pickers[["startDate"]]$inputValues$year) %>%
          dplyr::mutate(
            n_cases = round(suppressWarnings(as.numeric(n_cases))),
            n_population = round(suppressWarnings(as.numeric(n_population))),
            prevalence = round(suppressWarnings(as.numeric(prevalence)), 18),
            prevalence_95CI_lower = round(suppressWarnings(as.numeric(prevalence_95CI_lower)), 18),
            prevalence_95CI_upper = round(suppressWarnings(as.numeric(prevalence_95CI_upper)), 18),
            prevalence_start_date = as.Date(prevalence_start_date)
          )
        
        
        # AM: Assign censor label '<5' for censored results: ---------------
        result <- result %>% mutate(
          n_cases = ifelse(is.na(n_cases),"<5",n_cases),
          n_population = ifelse(is.na(n_population),"<5",n_population)
        )
        
        return(result)
      })
      
      # Filtered data
      summarised_result_data <- reactive({
        private$.data %>%
          dplyr::filter(
            cdm_name %in% private$.pickers[["cdm"]]$inputValues$cdm) %>%
          omopgenerics::filterSettings(
            analysis_complete_database_intervals %in% private$.pickers[["completePeriod"]]$inputValues$complete_period,
            analysis_full_contribution %in% private$.pickers[["fullContribution"]]$inputValues$full_contribution,
            analysis_type %in% private$.pickers[["analysisType"]]$inputValues$analysis_type,
            min_cell_count %in% private$.pickers[["minCounts"]]$inputValues$min_cell_count,
            denominator_start_date %in% private$.pickers[["denomStartDate"]]$inputValues$start_date,
            denominator_end_date %in% private$.pickers[["denomEndDate"]]$inputValues$end_date,
            denominator_days_prior_observation %in% private$.pickers[["denomPriorObs"]]$inputValues$prior_obs,
            denominator_target_cohort_name %in% private$.pickers[["denomTargetCohortName"]]$inputValues$denominator_target_cohort_name, #AM: Include denominator target cohort name
            denominator_sex %in% private$.pickers[["denomSex"]]$inputValues$denom_sex,
            denominator_age_group %in% private$.pickers[["denomAgeGroup"]]$inputValues$age_group,
            denominator_time_at_risk %in% private$.pickers[["denomTimeAtRisk"]]$inputValues$time_at_risk) %>%
          omopgenerics::filterAdditional(analysis_interval == private$.pickers[["interval"]]$inputValues$interval,
                                         prevalence_start_date %in% private$.pickers[["startDate"]]$inputValues$year) %>%
          omopgenerics::filterGroup(outcome_cohort_name %in% private$.pickers[["outcome"]]$inputValues$outcome) %>% 
          omopgenerics::filterStrata(age_group %in% private$.pickers[["ageGroup"]]$inputValues$strata_age_group, #NEW ----
                                     sex %in% private$.pickers[["strataSex"]]$inputValues$strata_sex)
      })
      
      summarised_gt_table <- reactive({
        req(summarised_result_data())
        summarised_result <- summarised_result_data()
        options_prev <- IncidencePrevalence:::defaultTableIncidencePrevalence(.options = list(),type = "prevalence")
        options_prev$decimals <- c(integer = 0, percentage = 6, numeric = 6, proportion = 6)
        options_prev$style <- "darwin"
        
        IncidencePrevalence::tablePrevalence(result = summarised_result,
                                             header = private$.pickers[["headerColumn"]]$inputValues$headerColumn,
                                             groupColumn = private$.pickers[["groupColumn"]]$inputValues$groupColumn,
                                             settingsColumn = private$.pickers[["settingColumn"]]$inputValues$settingColumn,
                                             hide = private$.pickers[["hideColumn"]]$inputValues$hideColumn,
                                             .options = options_prev)
        # .options = list(style = "darwin"))
      })
      
      # Tidy table
      output$tidyTable <- gt::render_gt({
        req(summarised_gt_table())
        summarised_gt_table()
      })
      
      # Download tidy table
      output$downloadTidyTable <- downloadHandler(
        filename = function() {
          "Prevalence-Table.docx"
        },
        content = function(file) {
          gt::gtsave(summarised_gt_table(), file)
        }
      )
      
      ### download table ----
      output$downloadTable <- downloadHandler(
        filename = function() {
          "prevalenceEstimatesTable.csv"
        },
        content = function(file) {
          utils::write.csv(getPrevalenceEstimates(), file)
        }
      )
      
      ### table estimates ----
      output$table <- DT::renderDT({
        table <- getPrevalenceEstimates()
        shiny::validate(need(nrow(table) > 0, "No results for selected inputs"))
        
        table <- table %>%
          mutate(`prevalence (%)` = paste0(
            100 * prevalence, " (", 100 * prevalence_95CI_lower, " to ",
            100 * prevalence_95CI_upper, " )"
          )) #%>%
          # select(database, outcome_cohort_name, 
          #        strata_name, strata_level, #NEW ---
          #        denominator_age_group, denominator_sex, denominator_days_prior_observation, denominator_start_date, denominator_end_date, denominator_time_at_risk, analysis_type, analysis_complete_database_intervals, analysis_full_contribution, analysis_min_cell_count, analysis_interval, prevalence_start_date, n_cases, n_population, "prevalence (%)")
          # # select(database, outcome_cohort_name, strata, denominator_age_group, denominator_sex, denominator_days_prior_observation, denominator_start_date, denominator_end_date, denominator_time_at_risk, analysis_type, analysis_complete_database_intervals, analysis_full_contribution, analysis_min_cell_count, analysis_interval, prevalence_start_date, n_cases, n_population, "prevalence (%)")
          
        
        DT::datatable(
          table,
          rownames = FALSE,
          extensions = "Buttons",
          options = list(scrollX = TRUE, scrollCollapse = TRUE)
        )
      })
      
#_________________________________________________________________________________________________________
# AM: Custom function for improved legend color render in Plotly --------
#_________________________________________________________________________________________________________       
      #### AM: filterLegends function (handle extra fill legends when rendering ggplot in plotly)
      `%||%` <- function(x, y) if (is.null(x)) y else x # Helper: Include coalesce operator
      
      
      filterLegends <- function(p) {
        pp <- plotly::ggplotly(p, tooltip = c("x", "y"))
        
        # Small helper: get a scalar color (some traces store a vector)
        first_val <- function(v) {
          if (is.null(v)) return(NULL)
          if (is.list(v)) v <- unlist(v, use.names = FALSE)
          if (length(v) >= 1) return(v[[1]])
          NULL
        }
        
        # Build a grouping key for legend dedupe: prefer legendgroup, else name
        key_for <- function(tr) {
          lg <- tr$legendgroup %||% ""
          nm <- tr$name %||% ""
          if (nzchar(lg)) lg else nm
        }
        
        n <- length(pp$x$data)
        keys <- vapply(pp$x$data, key_for, character(1))
        groups <- split(seq_len(n), keys)
        
        # Process each legend group independently
        for (g in names(groups)) {
          idx_all <- groups[[g]]
          if (length(idx_all) == 0) next
          
          # 1) Hide everything initially
          for (i in idx_all) pp$x$data[[i]]$showlegend <- FALSE
          
          # 2) Identify ribbons (fill/fillcolor) and non-ribbons
          is_ribbon <- function(i) !is.null(pp$x$data[[i]]$fill) || !is.null(pp$x$data[[i]]$fillcolor)
          ribbons <- Filter(is_ribbon, idx_all)
          non_ribbons <- setdiff(idx_all, ribbons)
          
          # 3) Pick a representative among non-ribbons: prefer markers, then lines, else first
          is_markers <- function(i) grepl("markers", pp$x$data[[i]]$mode %||% "")
          is_lines   <- function(i) grepl("lines",   pp$x$data[[i]]$mode %||% "")
          cand_mark  <- Filter(is_markers, non_ribbons)
          cand_line  <- Filter(is_lines,   non_ribbons)
          rep <- if (length(cand_mark)) cand_mark[[1]] else if (length(cand_line)) cand_line[[1]] else if (length(non_ribbons)) non_ribbons[[1]] else idx_all[[1]]
          
          # 4) Ensure a name
          if (!nzchar(pp$x$data[[rep]]$name %||% "")) {
            if (nzchar(g)) {
              pp$x$data[[rep]]$name <- g
            }
          }
          
          # 5) Ensure a visible color on the representative
          # Source priority: marker.color -> line.color -> (fallback) any ribbon fillcolor
          col <- first_val(pp$x$data[[rep]]$marker$color) %||%
            first_val(pp$x$data[[rep]]$line$color)
          
          if (is.null(col) && length(ribbons)) {
            # harvest a color from the group's ribbon (use the first)
            col <- first_val(pp$x$data[[ribbons[[1]]]]$fillcolor)
          }
          
          # Clean up transparent values
          if (!is.null(col) && (identical(col, "transparent") || grepl("rgba\\(0\\s*,\\s*0\\s*,\\s*0\\s*,\\s*0\\)", col))) {
            col <- NULL
          }
          
          # Apply the color + enforce filled legend glyph
          mode_rep <- pp$x$data[[rep]]$mode %||% ""
          if (!is.null(col)) {
            if (grepl("markers", mode_rep)) {
              pp$x$data[[rep]]$marker$color <- col
              pp$x$data[[rep]]$marker$symbol <- "circle"
              pp$x$data[[rep]]$marker$opacity <- 1
              pp$x$data[[rep]]$marker$line$width <- 0
              if (is.null(pp$x$data[[rep]]$marker$size)) pp$x$data[[rep]]$marker$size <- 6
            } else {
              pp$x$data[[rep]]$line$color <- col
              pp$x$data[[rep]]$line$width <- pp$x$data[[rep]]$line$width %||% 2
            }
          } else {
            # last resort: pick a default dark grey so we never get a blank swatch
            default_col <- "rgba(80,80,80,1)"
            if (grepl("markers", mode_rep)) {
              pp$x$data[[rep]]$marker$color <- default_col
              pp$x$data[[rep]]$marker$symbol <- "circle"
              pp$x$data[[rep]]$marker$opacity <- 1
              pp$x$data[[rep]]$marker$line$width <- 0
            } else {
              pp$x$data[[rep]]$line$color <- default_col
              pp$x$data[[rep]]$line$width <- pp$x$data[[rep]]$line$width %||% 2
            }
          }
          
          # 6) Finally show only the representative for this group
          pp$x$data[[rep]]$showlegend <- TRUE
          # keep ribbons hidden
          for (i in ribbons) pp$x$data[[i]]$showlegend <- FALSE
        }
        
        pp
      }
      #_________________________________________________________________________________________________________     
      
      
      
      ### make plot ----
      plotPrevalenceEstimates <- reactive({
        table <- getPrevalenceEstimates()
        shiny::validate(need(nrow(table) > 0, "No results for selected inputs"))
        class(table) <- c("PrevalenceResult", "IncidencePrevalenceResult", class(table))
        
        # AM - NEW ---------------------------------------------
        # Read database names
        db_levels  <- private$.db_levels
        db_palette <- private$.db_palette
        
        # Lock factor levels for stable mapping (keep specific colors for a defined data partner order)
        table <- table %>% dplyr::mutate(database = factor(database, levels = db_levels))
        
        
        #AM NEW Combine variables to allow coloring by more than one selection
        
        # Determine selected colour variables (can be 0, 1, or many)
        selected_cols <- private$.pickers[["color"]]$inputValues$color_by
        if (length(selected_cols) == 0) selected_cols <- "database"
        
        # If multiple are selected, create an interaction label column to colour by
        if (length(selected_cols) == 1) {
          color_var <- selected_cols
        } else {
          # Create a nice combined label using the actual values, not the column names
          combo_col <- ".color_by"
          table <- table %>%
            tidyr::unite(!!combo_col, dplyr::all_of(selected_cols),
                         sep = " | ", remove = FALSE, na.rm = TRUE)
          color_var <- combo_col
          
        }
        
        # OBSERVED levels present in the current (filtered) data
        combo_levels <- sort(unique(as.character(table[[color_var]])))
        n_combo <- length(combo_levels)
        
        # Safe palette for any n (1, 2, 3+)
        safe_palette <- function(n) {
          if (n <= 0) character(0)
          else scales::hue_pal(l = 60, c = 100)(n)     # distinct for 1–2
          # else #pals::alphabet(n)                                   # large qualitative
        }
        
        combo_cols <- safe_palette(n_combo)
        names(combo_cols) <- combo_levels   # <--- IMPORTANT
        
        legend_title <- if (length(selected_cols) == 1) selected_cols else paste(selected_cols, collapse = " & ")
        
        # # Determine colour variable (single-variable) ----
        # # color_var <- private$.pickers[["color"]]$inputValues$color_by
        # # if (length(color_var) == 0) color_var <- "database"
        # 
        # # Determine colours (multiple variables) ----
        # selected_cols <- private$.pickers[["color"]]$inputValues$color_by
        # if (length(selected_cols) == 0) selected_cols <- "database"
        # 
        # if (length(selected_cols) == 1) {
        #   color_var <- selected_cols
        # } else {
        #   combo_col <- ".color_by"
        #   table <- table %>%
        #     tidyr::unite(!!combo_col, dplyr::all_of(selected_cols),
        #                  sep = " | ", remove = FALSE, na.rm = TRUE)
        #   color_var <- combo_col
        # }
        
        
        #---------------------------------------------
        
        plot <- IncidencePrevalence::plotPrevalence(
          result = table,
          x = private$.pickers[["xAxis"]]$inputValues$xAxis,
          y = "prevalence",
          line = FALSE,
          point = TRUE,
          ribbon = as.logical(private$.pickers[["ribbon"]]$inputValues$ribbon),
          ymin = "prevalence_95CI_lower",
          ymax = "prevalence_95CI_upper",
          facet = private$.pickers[["facet"]]$inputValues$facet_by,
          colour = color_var #private$.pickers[["color"]]$inputValues$color_by
        ) 
        
        #AM NEW -------------------------------------------------
        # Apply palette only when colouring by database
        if (identical(color_var, "database")) {
          plot <- plot + 
            ggplot2::scale_colour_manual(values = db_palette,limits = db_levels, drop   = FALSE) + # keep absent dbs in legend for stability 
            ggplot2::scale_fill_manual(values   = db_palette, limits = db_levels, drop = FALSE) # Do the same for ribbon fill colors
        } else {
          
          # Apply scales with explicit limits so values and labels align 
          plot <- plot +
            ggplot2::scale_colour_manual(values = combo_cols,
                                         limits = combo_levels, drop = FALSE) +
            ggplot2::scale_fill_manual(values   = combo_cols,
                                       limits = combo_levels, drop = FALSE)
          
          
          
        }

      # -------------------------------------------------------------------------

        
        
        # remove confidence interval
        if (!as.logical(private$.pickers[["confInterval"]]$inputValues$confInterval)) {
          plot$layers <- plot$layers[2]
          if (as.logical(private$.pickers[["ribbon"]]$inputValues$ribbon)) {
            plot <- plot + ggplot2::geom_line()
          }
        }
        
        #----------------------------------------------------------------------------------
        # AM: modify guide legend parameters to allow proper color rendering in plotly -----
        # When CI (and/or ribbon) is on, ggplotly() emits multiple traces per group
        # Hide fill-based legends to avoid ribbon entries
        plot <- plot + ggplot2::guides(fill = "none", alpha = "none", size = "none")
        # (Optional) make sure legend uses filled symbols
        plot <- plot + ggplot2::guides(
          colour = ggplot2::guide_legend(override.aes = list(shape = 16, alpha = 1)) #solid filled symbols in legend
          )
        
        
        #### AM: Filter extra fill legends when rendering in plotly -----
        # n_legends <- table[[private$.pickers[["color"]]$inputValues$color_by]] |> unique() |> length()
        # plot <-  filterLegends(plot, n_legends)
        # 
        plot_plotly <-  filterLegends(plot)
        # Because filterLegends already returns a plotly object, we can return a list with ggplot and plotly objects 
        # for rendering and saving, respectively
        list(gg = plot, pl = plot_plotly)
        #----------------------------------------------------------------------
        #plot
        
      })
      
      ### download plot ----
      output$download_plot <- downloadHandler(
        filename = function() {
          "prevalenceEstimatesPlot.png"
        },
        content = function(file) {
          ggplot2::ggsave(
            file,
            #plotPrevalenceEstimates(),
            plotPrevalenceEstimates()$gg,  # pass the ggplot here (included change after using custom filterLegends)
            width = as.numeric(input$download_width),
            height = as.numeric(input$download_height),
            dpi = as.numeric(input$download_dpi),
            units = "cm"
          )
        }
      )
      ### plot ----
      output$plot <- renderPlotly({
        # plotPrevalenceEstimates()
        plotPrevalenceEstimates()$pl # get plotly object (included change after using custom filterLegens function)
      })
    },
    assertPrevalenceData = function(data) {
      resSettings <- attr(data, "settings")
      if (is.null(resSettings)) {
        stop("Data does not appear to be a result object of `IncidencePrevalence`")
      }
      if (!all(resSettings$result_type %in% c("prevalence", "prevalence_attrition"))) {
        stop("Cannot assert `Prevalence` result")
      }
    },
    transformData = function(data) {
      # set strata
      strataColumn <- unique(settings(data) %>% dplyr::filter(strata != "reason") %>% dplyr::pull(strata))
      private$.strata <- unique(data %>% dplyr::filter(strata_name != "reason") %>% dplyr::pull(strata_level))
      
      # transform to readable format
      minCellCount <- attr(data, "settings") %>%
        dplyr::pull(min_cell_count) %>%
        unique()
      data <- IncidencePrevalence::asPrevalenceResult(data) %>%
        { if (!"analysis_interval" %in% names(.)) dplyr::mutate(., analysis_interval = "overall") else .} %>%
        dplyr::mutate(analysis_min_cell_count = !!minCellCount) %>%
        dplyr::rename(
          database = cdm_name,
          n_cases = outcome_count,
          n_population = denominator_count
        )
      # add strata column
      # if (strataColumn == "") {
      #   data <- data %>% dplyr::mutate(strata = "overall")
      # } else {
      #   data <- data %>% dplyr::rename(strata = strataColumn)
      # }
      return(data)
    },
    initPickers = function() {
      # cdm
      private$.pickers[["cdm"]] <- InputPanel$new(
        funs = list(cdm = shinyWidgets::pickerInput),
        args = list(cdm = list(
          inputId = "cdm", label = "Database", choices = unique(private$.tidyData$database), selected = unique(private$.tidyData$database), multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["cdm"]]$parentNamespace <- self$namespace
      
      # outcome
      private$.pickers[["outcome"]] <- InputPanel$new(
        funs = list(outcome = shinyWidgets::pickerInput),
        args = list(outcome = list(
          inputId = "outcome", label = "Outcome", choices = unique(private$.tidyData$outcome_cohort_name), selected = unique(private$.tidyData$outcome_cohort_name)[1], multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["outcome"]]$parentNamespace <- self$namespace
      
      # strata (Note: AM this was split into strata_age_group,strata_sex)
      # private$.pickers[["strata"]] <- InputPanel$new(
      #   funs = list(strata = shinyWidgets::pickerInput),
      #   args = list(strata = list(
      #     inputId = "strata", label = "Strata", choices = unique(private$.strata), selected = unique(private$.strata), multiple = TRUE,
      #     options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
      #   )),
      #   growDirection = "horizontal"
      # )
      # private$.pickers[["strata"]]$parentNamespace <- self$namespace
      # AM strata age group -----
      private$.pickers[["ageGroup"]] <- InputPanel$new(
        funs = list(strata_age_group = shinyWidgets::pickerInput),
        args = list(strata_age_group = list(
          inputId = "strata_age_group", label = "Age group", choices = unique(private$.tidyData$age_group), selected = "overall", multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["ageGroup"]]$parentNamespace <- self$namespace

      # # AM strata sex -----
      private$.pickers[["strataSex"]] <- InputPanel$new(
        funs = list(strata_sex = shinyWidgets::pickerInput),
        args = list(strata_sex = list(
          inputId = "strata_sex", label = "Sex", choices = unique(private$.tidyData$sex), selected = "overall", multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["strataSex"]]$parentNamespace <- self$namespace
      
      # AM denominator target cohort name
      private$.pickers[["denomTargetCohortName"]] <- InputPanel$new(
        funs = list(denominator_target_cohort_name = shinyWidgets::pickerInput),
        args = list(denominator_target_cohort_name = list(
          inputId = "denominator_target_cohort_name", label = "Denominator Target Cohort Name", choices = unique(private$.tidyData$denominator_target_cohort_name), selected = unique(private$.tidyData$denominator_target_cohort_name), multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["denomTargetCohortName"]]$parentNamespace <- self$namespace
      
      # denominator age group
      private$.pickers[["denomAgeGroup"]] <- InputPanel$new(
        funs = list(age_group = shinyWidgets::pickerInput),
        args = list(age_group = list(
          inputId = "age_group", label = "Denominator age group", choices = unique(private$.tidyData$denominator_age_group), selected = "0 to 18", multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["denomAgeGroup"]]$parentNamespace <- self$namespace
      
      # denominator sex
      private$.pickers[["denomSex"]] <- InputPanel$new(
        funs = list(denom_sex = shinyWidgets::pickerInput),
        args = list(denom_sex = list(
          inputId = "denom_sex", choices = unique(private$.tidyData$denominator_sex), label = "Denominator sex", selected = "Both", multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["denomSex"]]$parentNamespace <- self$namespace
      
      # prior observation
      private$.pickers[["denomPriorObs"]] <- InputPanel$new(
        funs = list(prior_obs = shinyWidgets::pickerInput),
        args = list(prior_obs = list(
          inputId = "prior_obs", choices = unique(private$.tidyData$denominator_days_prior_observation), label = "Prior observation", selected = unique(private$.tidyData$denominator_days_prior_observation), multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["denomPriorObs"]]$parentNamespace <- self$namespace
      
      # denominator start date
      private$.pickers[["denomStartDate"]] <- InputPanel$new(
        funs = list(start_date = shinyWidgets::pickerInput),
        args = list(start_date = list(
          inputId = "start_date", choices = unique(private$.tidyData$denominator_start_date), label = "Start date", selected = unique(private$.tidyData$denominator_start_date), multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["denomStartDate"]]$parentNamespace <- self$namespace
      
      # denominator end date
      private$.pickers[["denomEndDate"]] <- InputPanel$new(
        funs = list(end_date = shinyWidgets::pickerInput),
        args = list(end_date = list(
          inputId = "end_date", choices = unique(private$.tidyData$denominator_end_date), label = "End date", selected = unique(private$.tidyData$denominator_end_date), multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["denomEndDate"]]$parentNamespace <- self$namespace
      
      # denominator time at risk
      private$.pickers[["denomTimeAtRisk"]] <- InputPanel$new(
        funs = list(time_at_risk = shinyWidgets::pickerInput),
        args = list(time_at_risk = list(
          inputId = "time_at_risk", choices = unique(private$.tidyData$denominator_time_at_risk), label = "Time at risk", selected = unique(private$.tidyData$denominator_time_at_risk), multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["denomTimeAtRisk"]]$parentNamespace <- self$namespace
      
      # analysis type
      private$.pickers[["analysisType"]] <- InputPanel$new(
        funs = list(analysis_type = shinyWidgets::pickerInput),
        args = list(analysis_type = list(inputId = "analysis_type", choices = unique(private$.tidyData$analysis_type), label = "Prevalence type", selected = unique(private$.tidyData$analysis_type), multiple = FALSE)),
        growDirection = "horizontal"
      )
      private$.pickers[["analysisType"]]$parentNamespace <- self$namespace
      
      # complete period
      private$.pickers[["completePeriod"]] <- InputPanel$new(
        funs = list(complete_period = shinyWidgets::pickerInput),
        args = list(complete_period = list(
          inputId = "complete_period", choices = unique(private$.tidyData$analysis_complete_database_intervals), label = "Complete period", selected = unique(private$.tidyData$analysis_complete_database_intervals), multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["completePeriod"]]$parentNamespace <- self$namespace
      
      # full contribution
      private$.pickers[["fullContribution"]] <- InputPanel$new(
        funs = list(full_contribution = shinyWidgets::pickerInput),
        args = list(full_contribution = list(
          inputId = "full_contribution", choices = unique(private$.tidyData$analysis_full_contribution), label = "Full contribution", selected = unique(private$.tidyData$analysis_full_contribution), multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["fullContribution"]]$parentNamespace <- self$namespace
      
      # min counts
      private$.pickers[["minCounts"]] <- InputPanel$new(
        funs = list(min_cell_count = shinyWidgets::pickerInput),
        args = list(min_cell_count = list(
          inputId = "min_cell_count", choices = unique(private$.tidyData$analysis_min_cell_count), label = "Minimum counts", selected = unique(private$.tidyData$analysis_min_cell_count), multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["minCounts"]]$parentNamespace <- self$namespace
      
      # interval
      private$.pickers[["interval"]] <- InputPanel$new(
        funs = list(interval = shinyWidgets::pickerInput),
        args = list(interval = list(
          inputId = "interval", choices = unique(private$.tidyData$analysis_interval), label = "Interval", selected = "overall", multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["interval"]]$parentNamespace <- self$namespace
      
      # start date
      private$.pickers[["startDate"]] <- InputPanel$new(
        funs = list(year = shinyWidgets::pickerInput),
        args = list(year = list(
          inputId = "year", choices = unique(private$.tidyData$prevalence_start_date), label = "Prevalence start date", selected = unique(private$.tidyData$prevalence_start_date), multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["startDate"]]$parentNamespace <- self$namespace
      
      # plot pickers
      plotDataChoices <- c(
        "database", "outcome_cohort_name", "strata","denominator_target_cohort_name","denominator_cohort_name", "denominator_age_group", "denominator_sex", "denominator_days_prior_observation",
        "denominator_start_date", "denominator_end_date", "denominator_time_at_risk", "analysis_complete_database_intervals",
        "analysis_min_cell_count", "analysis_interval", "prevalence_start_date"
      )
      # x-axis
      private$.pickers[["xAxis"]] <- InputPanel$new(
        funs = list(xAxis = shinyWidgets::pickerInput),
        args = list(xAxis = list(
          inputId = "xAxis", choices = plotDataChoices, label = "Prevalence_start_date", selected = "prevalence_start_date", multiple = F,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["xAxis"]]$parentNamespace <- self$namespace
      
      # facet by
      private$.pickers[["facet"]] <- InputPanel$new(
        funs = list(facet_by = shinyWidgets::pickerInput),
        args = list(facet_by = list(
          inputId = "facet_by", choices = plotDataChoices, label = "Facet by", selected = c("outcome_cohort_name", "database"), multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["facet"]]$parentNamespace <- self$namespace
      
      # color by
      private$.pickers[["color"]] <- InputPanel$new(
        funs = list(color_by = shinyWidgets::pickerInput),
        args = list(color_by = list(
          inputId = "color_by", choices = plotDataChoices, label = "Colour by", selected = c("database"), multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["color"]]$parentNamespace <- self$namespace
      
      # ribbon
      private$.pickers[["ribbon"]] <- InputPanel$new(
        funs = list(ribbon = shinyWidgets::pickerInput),
        args = list(ribbon = list(
          inputId = "ribbon", choices = c(TRUE, FALSE), label = "Ribbon", selected = TRUE, multiple = FALSE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["ribbon"]]$parentNamespace <- self$namespace
      
      # confidence interval
      private$.pickers[["confInterval"]] <- InputPanel$new(
        funs = list(confInterval = shinyWidgets::pickerInput),
        args = list(confInterval = list(
          inputId = "confInterval", choices = c(TRUE, FALSE), label = "Confidence interval", selected = TRUE, multiple = FALSE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["confInterval"]]$parentNamespace <- self$namespace
      
      # headerColumn
      headerColumnOptions <- c("cdm_name", "estimate_name")
      private$.pickers[["headerColumn"]] <- InputPanel$new(
        funs = list(headerColumn = shinyWidgets::pickerInput),
        args = list(headerColumn = list(
          inputId = "headerColumn", choices = headerColumnOptions, label = "Header", selected = headerColumnOptions, multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["headerColumn"]]$parentNamespace <- self$namespace
      
      # groupColumn
      groupColumnOptions <- c("outcome_cohort_name","strata") #c("outcome_cohort_name", "cdm_name")
      private$.pickers[["groupColumn"]] <- InputPanel$new(
        funs = list(groupColumn = shinyWidgets::pickerInput),
        args = list(groupColumn = list(
          inputId = "groupColumn", choices = groupColumnOptions, label = "Group columns", selected = groupColumnOptions, multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["groupColumn"]]$parentNamespace <- self$namespace
      
      # settingsColumn
      settingColumnOptions <- c("denominator_time_at_risk", "denominator_age_group", "denominator_sex")
      private$.pickers[["settingsColumn"]] <- InputPanel$new(
        funs = list(settingsColumn = shinyWidgets::pickerInput),
        args = list(settingsColumn = list(
          inputId = "settingsColumn", choices = settingColumnOptions, label = "Settings columns", selected = settingColumnOptions, multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["settingsColumn"]]$parentNamespace <- self$namespace
      
      # hideColumn
      hideColumnOptions <- c("denominator_time_at_risk", "denominator_cohort_name", "denominator_age_group", "denominator_sex", "analysis_interval")
      private$.pickers[["hideColumn"]] <- InputPanel$new(
        funs = list(hideColumn = shinyWidgets::pickerInput),
        args = list(hideColumn = list(
          inputId = "hideColumn", choices = hideColumnOptions, label = "Hide columns", selected = hideColumnOptions, multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["hideColumn"]]$parentNamespace <- self$namespace
    }
  )
)
