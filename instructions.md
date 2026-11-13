# Instructions

## How to run the study package

1.  In RStudio, open the `EUPAS1000000714.Rproj` file.
2.  Open `extras/CodeToRun.R`.
3.  Restore the project library:
    -   `install.packages("renv")` if needed
    -   `install.packages("CirceR")` if needed for your site
    -   `renv::activate()`
    -   `renv::restore()`
4.  Edit connection and run settings in `CodeToRun.R`:
    -   Set `dbName` (official database name used in results paths).
    -   Set `cdmSchema` and `writeSchema`.
    -   Set `tablePrefix` to a short site-specific write prefix if required.
    -   Leave `minCellCount` as provided (`5`) unless the protocol or Coordination Centre directs otherwise.
    -   Complete `DBI::dbConnect()` for your site (see <https://dbi.r-dbi.org/reference/dbConnect.html>).
5.  Execute the study:
    -   From `CodeToRun.R`, run through to `source(here::here("R/RunAnalysis.R"))` after the connection and `cdm` object exist.

Results are written under `results/<dbName>/`.

## Review results

-   Inspect CSV and log outputs under `results/<dbName>/`.

### Launch the Shiny app

-   Results are expected under `results/<dbName>/` from the main project root (EUPAS1000000714.Rproj) and are automatically loaded in the Shiny app from this path. Refer to `inst/shiny/studyExplorer/README.md` if you would like to load the results from a different path.
-   Open `inst/shiny/studyExplorer/shinyStudyExplorer.Rproj` (or `app.R`) and launch the Shiny app (RStudio **Run App**).

## Return results to the Coordination Centre

Share the study results zip with the DARWIN EU Coordination Centre.
