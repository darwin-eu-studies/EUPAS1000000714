# Launching Study Shiny App

### **Open the R Project**

-   Navigate to the **`inst/shiny/studyExplorer`** directory.

-   Open the **`shinySstudyExplorer.Rproj`** file in **RStudio**.

### **2. Restore the Project Environment**

-   Restore the project library with **`renv::restore()`**

```         
This will install all required packages as specified in the `renv.lock` file.
```

### **3. Run app.R**

By default, the study package results are stored within the **`results/dbName`** folder from the main project root:

``` r
results/
└── dbName/ # i.e. DK-DHR, CDW Bordeaux, IPCI, FinOMOP-THL,NAJS, etc. 
```

**Note:** If your results are stored in a different location, update the path accordingly to point to the correct results folder. (See L182 in R/preprocessing.R`)` **`dataFolder <- normalizePath(here::here("../../../results"))`**

-   **Launch the Shiny App:**

    Open the **`app.R`** script in RStudio.

    Click the **“Run App”** button (top-right corner of the script editor in RStudio).
