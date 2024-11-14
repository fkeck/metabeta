
# Code and data for the manuscript

This repository provides R code and data to reproduce quantitative results and figures from the manuscript:

Keck, F. et al. The global human impact on biodiversity (2023).

## System requirements

The code can be executed on any platform (Linux, Mac, Windows) that can run R. The details about the environment on which the code has been tested is fully described in `/session_info.txt`.


## Installation

Install R packages from CRAN:

    install.packages(c("tidyverse", "glmmTMB", "emmeans",
    "ggtext", "patchwork", "jsonlite", "tibble",
    "grImport2", "stringr", "glue", "sf", "rphylopic",
    "scales", "ggeffects", "car", "broom.mixed"))

Using the package's binaries provided by CRAN, installation takes a few minutes on standard computer. The versions of the packages used to generate the results of the manuscript are provided in `/session_info.txt`.

## Instructions and Demo

### Data

Data can be found in the `/data` directory. The main dataset is provided in JSON ( `/data/data.json`). The metadata of the publications used in the meta-analysis are stored in `/data/publications_metadata.csv`.

### Analyses

Scripts can be found in the `/R` directory. They are organized in separated modules but all the analyses can be ran from the master script `run_all.R`. The execution time is around 5-10 minutes on a standard laptop.

Once the analyses are completed, the results can be found in the active session and the figures can be found in the `results` folder.
