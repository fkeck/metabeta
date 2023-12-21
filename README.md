
# Code and data for the manuscript

This repository provides R code and data to reproduce quantitative results and figures from the manuscript:

Keck, F. et al. The global human impact on biodiversity (2023).

## Data

Data can be found in the `/data` directory. The main dataset is provided in JSON ( `/data/data.json`). The metadata of the publications used in the meta-analysis are stored in `/data/publications_metadata.csv`.

## Analyses

Scripts can be found in the `/R` directory. They are organized in separated modules but all the analyses can be ran from the master script `run_all.R`.
Once the analyses are completed, the results can be found in the active session and the figures can be found in the `results` folder.

## Dependencies

Install R packages from CRAN:

    install.packages(c("tidyverse", "glmmTMB", "emmeans",
    "ggtext", "patchwork", "jsonlite", "tibble",
    "grImport2", "stringr", "glue", "sf", "rphylopic",
    "scales", "ggeffects", "car", "broom.mixed"))

The versions of of the packages used to generate the results of the manuscript are provided in `/session_info.txt`.

