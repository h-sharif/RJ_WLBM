## Rum Jungle Water and Load Balance Model

> Developed for the Rum Jungle mine as part of 2024/2025 WLBM updating project at Robertson GeoConsultants Inc.

### Running the App:

```r
{
  # make sure you have "scales" and "patchwork" packages
  install.packages(
      c("shiny", "tidyverse", "data.table", "bslib",
        "shinybusy", "shinyWidgets", "bsicons", "shinyalert",
        "readxl", "ggnewscale", "ggpubr", "scales", "patchwork")
  )
}
```
To run the app (you can also specify the branch):

```r
{
  shiny::runGitHub("RJ_WLBM", "h-sharif", ref = "main")
}
```

`dev` branch is based on version 3.48, and `main` branch is based on version 3.44.

#### Instructions:
1. To view the results, specify the GoldSim master spreadsheet output in the left panel. Please note that this file is not included in this repository.
2. If you encounter any issues, please report it with details.

<p align="center">
  <img width="1915" src=www/app_demo.png><br>
</p>
<br>



