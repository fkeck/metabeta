library(tidyverse)
library(glmmTMB)
library(emmeans)
library(ggtext)
library(patchwork)

dat <- jsonlite::read_json("data/data.json", simplifyVector = TRUE) %>% 
  as_tibble()

v <- c("Global", "Biome", "Organism", "Pressure", "Scale")

# Color palette
col_palette <- c("#5A5A5A", "#2CD17C", "#F4496D", "#FA973D", "#6f42c1")
names(col_palette) <- v

# Colors for variables
var_cols <- read_csv("data/var_cols.csv") %>% 
  select(-plot_id)


# Icons
icons <- tibble::tribble(
  ~ variable, ~ scale,
  "Marine", 0.8,
  "Freshwater", 0.8,
  "Terrestrial", 0.8,
  "Habitat change", 0.8,
  "Invasive species", 0.8,
  "Pollution", 0.8,
  "Resource exploitation", 0.8,
  "Climate change", 0.8,
  "Microbes", 1,
  "Invertebrates", 1,
  "Fish", 0.6,
  "Plants", 1,
  "Fungi", 1,
  "Tetrapods", 0.5,
  "Reptiles/Amphibians", 0.9,
  "Reptiles", 0.9,
  "Amphibians", 0.9,
  "Mammals", 0.7,
  "Birds", 0.9,
  "Other", 0.7,
  "Point/Microhabitat", 0.2, 
  "Plot", 0.4,
  "Local", 0.6,
  "Regional", 0.8,
  "Continental/Global", 1) %>% 
  mutate(img_path = str_replace_all(variable, "/", "_") %>%
           paste0("icons/", ., ".svg"),
         img = map(img_path, grImport2::readPicture))



plots <- list()
models <- list()


# Utility functions
str_fancy_title <- function(x, prefix = NULL, suffix = NULL) {
  
  x <- case_when(x == "alpha_diversity" ~ "local diversity",
                 x == "beta_similarity" ~ "homogeneity",
                 x == "beta_structure" ~ "shift")
  
  x <- stringr::str_c(prefix, x, suffix, sep = " ")
  return(x)
}

highlight_axis_label <- function(x, pattern) {
  ifelse(grepl(pattern, x), glue::glue("<b>{x}</b>"), x)
}
