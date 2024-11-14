
#### Main results ####

source("R/PBL_load_data.R")
source("R/PBL_stats.R")
source("R/PBL_plots.R")
source("R/PBL_plots_SI.R")

make_fig1(dat) %>% 
  ggsave("results/Figure_1.pdf", plot = ., width = 120, height = 130, units = "mm")

make_fig2(dat, plot_icons = TRUE) %>%
  ggsave("results/Figure_2.pdf", plot = ., width = 183, height = 100, units = "mm", device = cairo_pdf)

make_fig3(dat, plot_icons = TRUE) %>%
  ggsave("results/Figure_3.pdf", plot = ., width = 89, height = 100, units = "mm", device = cairo_pdf)

make_fig4(dat, plot_icons = TRUE) %>%
  ggsave("results/Figure_4.pdf", plot = ., width = 183, height = 183, units = "mm", device = cairo_pdf)

writeLines(capture.output(sessionInfo()), "session_info.txt")


#### Supplementary information ####
source("R/EXTRA_stats_var.R")
source("R/EXTRA_publication_bias.R")
saveRDS(dat, "computed/dat.rds")
saveRDS(test_pub_bias, "computed/test_pub_bias.rds")

# Stratified regression by study type
source("R/PBL_load_data.R")
dat <- filter(dat, `Study type` == "Field observation")
source("R/EXTRA_stats_stratified.R")
source("R/PBL_plots.R")
make_fig1(dat) %>% 
  ggsave("results/Figure_1_strat_field.pdf", plot = ., width = 120, height = 130, units = "mm")

make_fig2(dat, plot_icons = FALSE) %>%
  ggsave("results/Figure_2_strat_field.pdf", plot = ., width = 183, height = 100, units = "mm", device = cairo_pdf)

make_fig3(dat, plot_icons = FALSE) %>%
  ggsave("results/Figure_3_strat_field.pdf", plot = ., width = 89, height = 100, units = "mm", device = cairo_pdf)

source("R/PBL_load_data.R")
dat <- filter(dat, `Study type` != "Field observation")
source("R/EXTRA_stats_stratified.R")
source("R/PBL_plots.R")
make_fig1(dat) %>% 
  ggsave("results/Figure_1_strat_expe.pdf", plot = ., width = 120, height = 130, units = "mm")

make_fig2(dat, plot_icons = FALSE) %>%
  ggsave("results/Figure_2_strat_expe.pdf", plot = ., width = 183, height = 100, units = "mm", device = cairo_pdf)

make_fig3(dat, plot_icons = FALSE) %>%
  ggsave("results/Figure_3_strat_expe.pdf", plot = ., width = 89, height = 100, units = "mm", device = cairo_pdf)

source("R/PBL_load_data.R")
source("R/EXTRA_spatial_biases.R")
source("R/EXTRA_sim_betadiv.R")
source("R/EXTRA_stats_var.R")

source("R/EXTRA_stats_var_weighted.R")
make_fig2(dat, plot_icons = FALSE) %>% 
  ggsave("results/Figure_2_sqrt_weighted_var.pdf", plot = ., width = 183, height = 100, units = "mm", device = cairo_pdf)

make_fig3(dat, plot_icons = FALSE) %>% 
  ggsave("results/Figure_3_weighted_var.pdf", plot = ., width = 89, height = 100, units = "mm", device = cairo_pdf)

quarto::quarto_render("SI/supplementary_info.qmd")
