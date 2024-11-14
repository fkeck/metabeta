#### Figure 1 ####

make_fig1 <- function(dat) {
  
  plots <- list()
  
  dat_rob <- dat %>% 
    filter(!is.na(Longitude), !is.na(Latitude)) %>% 
    sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% 
    sf::st_transform(crs = sf::st_crs("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs")) %>% 
    mutate(has_alpha = ifelse(is.na(alpha_diversity), "Community composition only", "Local diversity and community composition")) %>% 
    slice_sample(prop = 1)
  
  sf::sf_use_s2(FALSE)
  
  plots$map <-
    sf::read_sf("data/world_shp/TM_WORLD_BORDERS_SIMPL-0.3.shp") %>% 
    sf::st_cast("POLYGON") %>% 
    mutate(area = sf::st_area(geometry)) %>% 
    filter(as.numeric(area) > 10^9) %>% 
    sf::st_buffer(0) %>% 
    sf::st_crop(sf::st_bbox(c(xmin = -180, xmax = 180, ymin = -90, ymax = 90))) %>% 
    sf::st_transform(crs = sf::st_crs("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs")) %>% 
    ggplot() +
    geom_sf(fill = "grey80", colour = "grey80") +
    geom_sf(aes(color = has_alpha, group = cut(sample(seq(1, nrow(dat_rob))), 100)),
            shape = 20, size = 0.3, data = dat_rob) +
    scale_color_manual(values = c("#00364eff", "#00b9ffff")) +
    scale_x_continuous(breaks = seq(-180, 180, 45)) +
    theme_minimal() +
    theme(plot.title =  element_blank(),
          plot.margin = margin(t = 0, r = 0, b = 0, l = 0),
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(size = 7),
          legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
          legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0),
          axis.text = element_blank(),
          axis.title = element_blank())
  
  plots$bar_vars <- list()
  
  plots$bar_vars$theme <- theme_minimal() +
    theme(plot.title = element_text(size = 7),
          axis.title.y = element_blank(),
          axis.text.y = element_text(hjust = 1, size = 6),
          axis.text.x = element_text(size = 6),
          axis.title = element_text(size = 7),
          strip.text.y = element_blank())
  
  
  
  d <- dat %>% 
    group_by(Biome) %>% 
    count() %>% 
    ungroup() %>% 
    left_join(var_cols, c("Biome" = "variable")) %>% 
    mutate(Biome = fct_reorder(Biome, n))
  
  plots$bar_vars$Biome <- ggplot(d) + 
    geom_col(aes(n, Biome), fill = d$var_col) +
    labs(title = "Biome") +
    xlab("Number of comparisons") +
    plots$bar_vars$theme
  
  offset <- -ggplot_build(plots$bar_vars$Biome)$layout$panel_params[[1]]$x.range %>% diff() * 0.06
  offset <- offset + 20
  
  d <- filter(icons, variable %in% (dat %>% group_by(Biome) %>% group_keys() %>% .[, 1, drop = TRUE]))
  plots$bar_vars$Biome <- plots$bar_vars$Biome +
    rphylopic::geom_phylopic(aes(x = offset, y = variable, img = img), size = d$scale, color = "#5A5A5A", data = d) +
    scale_x_continuous(labels = scales::comma, expand = expansion(add = c(abs(offset), 0.1))) +
    coord_cartesian(xlim = c(offset, NA))
  
  
  d <- dat %>% 
    group_by(Organism) %>% 
    count() %>% 
    ungroup() %>% 
    left_join(var_cols, c("Organism" = "variable")) %>% 
    mutate(Organism = fct_reorder(Organism, n))
  
  plots$bar_vars$Organism <- ggplot(d) +
    geom_col(aes(n, Organism), fill = d$var_col) +
    labs(title = "Organism") +
    xlab("Number of comparisons") + 
    plots$bar_vars$theme
  
  offset <- -ggplot_build(plots$bar_vars$Organism)$layout$panel_params[[1]]$x.range %>% diff() * 0.06
  offset <- offset - 10
  
  d <- filter(icons, variable %in% (dat %>% group_by(Organism) %>% group_keys() %>% .[, 1, drop = TRUE]))
  plots$bar_vars$Organism <- plots$bar_vars$Organism +
    rphylopic::geom_phylopic(aes(x = offset, y = variable, img = img), size = d$scale, color = "#5A5A5A", data = d) +
    scale_x_continuous(labels = scales::comma, expand = expansion(add = c(abs(offset), 0.1))) +
    coord_cartesian(xlim = c(offset, NA))
  
  
  d <- dat %>% 
    group_by(Pressure) %>% 
    count() %>% 
    ungroup() %>% 
    left_join(var_cols, c("Pressure" = "variable")) %>% 
    mutate(Pressure = fct_reorder(Pressure, n))
  
  plots$bar_vars$Pressure <- ggplot(d) +
    geom_col(aes(n, Pressure), fill = d$var_col) +
    labs(title = "Pressure") +
    xlab("Number of comparisons") + 
    plots$bar_vars$theme
  
  offset <- -ggplot_build(plots$bar_vars$Pressure)$layout$panel_params[[1]]$x.range %>% diff() * 0.06
  
  d <- filter(icons, variable %in% (dat %>% group_by(Pressure) %>% group_keys() %>% .[, 1, drop = TRUE]))
  plots$bar_vars$Pressure <- plots$bar_vars$Pressure +
    rphylopic::geom_phylopic(aes(x = offset, y = variable, img = img), size = d$scale, color = "#5A5A5A", data = d) +
    scale_x_continuous(labels = scales::comma, expand = expansion(add = c(abs(offset), 0.1))) +
    coord_cartesian(xlim = c(offset, NA))
  
  d <- dat %>% 
    group_by(Scale) %>% 
    count() %>% 
    ungroup() %>% 
    left_join(var_cols, c("Scale" = "variable")) %>% 
    mutate(Scale = fct_reorder(Scale, n))
  
  plots$bar_vars$Scale <- ggplot(d) +
    geom_col(aes(n, Scale), fill = d$var_col) +
    labs(title = "Scale") +
    xlab("Number of comparisons") + 
    plots$bar_vars$theme
  
  offset <- -ggplot_build(plots$bar_vars$Scale)$layout$panel_params[[1]]$x.range %>% diff() * 0.06
  
  d <- filter(icons, variable %in% (dat %>% group_by(Scale) %>% group_keys() %>% .[, 1, drop = TRUE]))
  plots$bar_vars$Scale <- plots$bar_vars$Scale +
    rphylopic::geom_phylopic(aes(x = offset, y = variable, img = img), size = d$scale, color = "#5A5A5A", data = d) +
    scale_x_continuous(labels = scales::comma, expand = expansion(add = c(abs(offset), 0.1))) +
    coord_cartesian(xlim = c(offset, NA))
  
  
  plots$bar_vars$All <-
    ((plots$bar_vars$Biome / plots$bar_vars$Organism) + plot_layout(heights = c(3, 10))) |
    ((plots$bar_vars$Pressure / plots$bar_vars$Scale) + plot_layout(heights = c(5, 5)))
  
  
  plots$Figure_1 <- (plots$map / plots$bar_vars$All) +
    plot_layout(heights = c(1, 1)) +
    plot_annotation(tag_levels = list(c('a', 'b', 'd', 'c', 'e'))) &
    theme(plot.tag = element_text(size = 8, face = "bold"))
  
  return(plots$Figure_1)  
}


#### Figure 2 ####

make_fig23 <- function(dat, fig, plot_icons = TRUE) {
  
  plots <- list()
    
  for (depv in names(models$one_fac)) {
    
    plots$histogram[[depv]] <-ggplot(dat) +
      geom_histogram(aes(.data[[depv]])) +
      xlab(str_fancy_title(depv, prefix = "LRR")) +
      theme_minimal() +
      theme(axis.title.y = element_blank(),
            axis.title.x = element_text(size = 7),
            axis.text.y = element_text(hjust = 1, size = 6),
            axis.text.x = element_text(size = 6))
    
    d <- models$one_fac[[depv]] %>% 
      left_join(var_cols, c("variable" = "variable")) %>% 
      mutate(label = paste0(variable, " (", scales::label_comma(accuracy = 1)(n), ")")) %>% 
      mutate(plot_id = fct_relevel(plot_id,
                                   "Global",
                                   "Biome",
                                   "Pressure",
                                   "Organism",
                                   "Scale")) %>% 
      mutate(label = fct_reorder(label, emmean, .desc = TRUE)) %>% 
      left_join(icons, by = "variable") %>% 
      mutate(img_offset =  case_when(depv == "alpha_diversity" ~ -1.3,
                                     depv == "beta_similarity" ~ -0.5,
                                     depv == "beta_structure" ~ -0.2))
    
    plots$one_fac[[depv]] <-
      ggplot(d, aes(x = emmean, y = label), color = d$var_col) +
      geom_vline(aes(xintercept = 0), color = "black", linetype = "dashed", size = 0.2) +
      geom_errorbar(aes(xmin = lower.CL95, xmax = upper.CL95), color = d$var_col, width = 0, size = 1.1) +
      geom_errorbar(aes(xmin = lower.CL99, xmax = upper.CL99), color = d$var_col, width = 0, size = 0.3) +
      geom_point(aes(size = n), fill = d$var_col, color = "white", pch = 21) +
      scale_size(range = c(1, 5)) +
      facet_grid(rows = vars(plot_id), scales = "free_y", space = "free_y", switch = "x",
                 labeller = labeller(plot_id = function(x) {c("", x[-1])})) +
      xlab(str_fancy_title(depv, prefix = "LRR")) +
      scale_colour_manual(values = col_palette) +
      scale_fill_manual(values = col_palette) +
      scale_x_continuous(expand = c(0.05, 0.01),
                         position = "top") +
      scale_y_discrete(labels = function(x) highlight_axis_label(x, "All data")) +
      theme_minimal() + 
      theme(legend.position = "none",
            axis.title.y = element_blank(),
            axis.title.x = element_text(size = 7),
            axis.text.y = element_markdown(hjust = 1, size = 6),
            axis.text.x = element_text(size = 6),
            strip.text = element_text(size = 7))
    
    if(plot_icons) {
      for(i in d$variable[!sapply(d$img, is.null)]) {
        d_i <- d %>% filter(variable == i)
        plots$one_fac[[depv]] <- plots$one_fac[[depv]] +
          rphylopic::geom_phylopic(aes(x =  img_offset, y = label, img = img), color = "#5A5A5A", size = d_i$scale, data = d_i)
      }
    }
    plots$one_fac[[depv]]
  }
  
  plots$Figure_2 <-
    plots$one_fac$beta_similarity +
    plots$one_fac$beta_structure +
    plot_annotation(tag_levels = 'a') &
    theme(plot.tag = element_text(size = 8, face = "bold"))
  
  if(fig == 2) {
    return(plots$Figure_2)
  }
  if(fig == 3) {
    return(plots$one_fac$alpha_diversity)
  }
}

make_fig2 <- function(dat, plot_icons = TRUE) {
  make_fig23(dat, fig  = 2, plot_icons = plot_icons)
}

make_fig3 <- function(dat, plot_icons = TRUE) {
  make_fig23(dat, fig  = 3, plot_icons = plot_icons)
}

#### Figure 4 ####

make_fig4 <- function(dat, plot_icons = TRUE) {
  
  plots <- list()
  
  cbn <- c("a_x_bstr", "a_x_bsim")
  plots$scatters <- list()
  plots$bp_slopes <- list()
  
  for(i in cbn) {
    
    indepv <- case_when(i == "a_x_bstr" ~ "alpha_diversity",
                        i == "a_x_bsim" ~ "alpha_diversity",
                        i == "bstr_bsim" ~ "beta_structure")
    depv <- case_when(i == "a_x_bstr" ~ "beta_structure",
                      i == "a_x_bsim" ~ "beta_similarity",
                      i == "bstr_bsim" ~ "beta_similarity")
    
    mod_i <- models$reg_subgrp %>% 
      filter(x == indepv & y == depv & plot_id == "Global") %>% 
      .$mod %>% .[[1]]
    eff_i <- ggeffects::ggemmeans(mod_i, term = paste0(indepv, " [", min(mod_i$frame[, 2]), ":", max(mod_i$frame[, 2]), " by = 0.01]"))    
    
    mod_i_marine <- models$reg_subgrp %>% 
      filter(x == indepv & y == depv & plot_id == "Biome" & variable == "Marine") %>% 
      .$mod %>% .[[1]]
    eff_i_marine <- ggeffects::ggemmeans(mod_i_marine, term = paste0(indepv, " [", min(mod_i$frame[, 2]), ":", max(mod_i$frame[, 2]), " by = 0.01]"))   
    
    mod_i_terrestrial <- models$reg_subgrp %>% 
      filter(x == indepv & y == depv & plot_id == "Biome" & variable == "Terrestrial") %>% 
      .$mod %>% .[[1]]
    eff_i_terrestrial <- ggeffects::ggemmeans(mod_i_terrestrial, term = paste0(indepv, " [", min(mod_i$frame[, 2]), ":", max(mod_i$frame[, 2]), " by = 0.01]"))   
    
    mod_i_fw <- models$reg_subgrp %>% 
      filter(x == indepv & y == depv & plot_id == "Biome" & variable == "Freshwater") %>% 
      .$mod %>% .[[1]]
    eff_i_fw <- ggeffects::ggemmeans(mod_i_fw, term = paste0(indepv, " [", min(mod_i$frame[, 2]), ":", max(mod_i$frame[, 2]), " by = 0.01]"))   
    
    
    plots$scatters[[i]] <-
      dat %>% 
      ggplot(aes(.data[[indepv]], .data[[depv]])) +
      geom_hline(yintercept = 0, color = "lightgrey", size = 0.3) +
      geom_vline(xintercept = 0, color = "lightgrey", size = 0.3) +
      geom_point(alpha = 0.1) + 
      geom_ribbon(aes(x = x, y = NULL, ymin = conf.low, ymax = conf.high), data = eff_i, alpha = 0.3) +
      geom_line(aes(x = x, y = predicted), data = eff_i) +
      geom_line(aes(x = x, y = predicted), data = eff_i_marine, color = col_palette[2]) +
      geom_line(aes(x = x, y = predicted), data = eff_i_terrestrial, color = col_palette[2], linetype = 2) +
      geom_line(aes(x = x, y = predicted), data = eff_i_fw, color = col_palette[2], linetype = 3) +
      xlab(paste(str_fancy_title(indepv, prefix = "LRR"))) +
      ylab(paste(str_fancy_title(depv, prefix = "LRR"))) +
      theme_bw() +
      theme(axis.text = element_text(size = 6),
            axis.title = element_text(size = 7),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())
    
    
    d <- models$reg_subgrp %>% 
      mutate(plot_id = as.character(plot_id), variable = as.character(variable)) %>% 
      mutate(label = variable) %>% 
      dplyr::filter(x == indepv & y == depv) %>% 
      mutate(plot_id = fct_relevel(plot_id,
                                   "Global",
                                   "Biome",
                                   "Pressure",
                                   "Organism",
                                   "Scale")) %>% 
      mutate(label = fct_reorder(label, estimate, .desc = TRUE)) %>% 
      mutate(img_offset =  case_when(i == "a_x_bstr" ~ -0.8,
                                     i == "a_x_bsim" ~ -1.5,
                                     i == "bstr_bsim" ~ -0.2)) %>% 
      left_join(var_cols, c("variable" = "variable")) %>% 
      left_join(icons, by = "variable") %>% 
      arrange(plot_id, label)
    
    plots$bp_slopes[[i]] <- d %>% 
      ggplot(aes(x = estimate, y = label, color = d$var_col)) +
      geom_col(fill = d$var_col, color = NA, width = 0.7) +
      geom_errorbar(aes(xmin = estimate - std.error, xmax = estimate + std.error), width = 0, color = "grey40") +
      facet_grid(rows = vars(plot_id), scales = "free_y", space = "free_y", switch = "x",
                 labeller = labeller(plot_id = function(x) {c("", x[-1])})) +
      xlab("Model slopes") +
      scale_x_continuous(expand = c(0.05, 0.01),
                         position = "bottom") +
      scale_y_discrete(labels = function(x) highlight_axis_label(x, "All data")) +
      theme_minimal() + 
      theme(legend.position = "none",
            axis.title.y = element_blank(),
            axis.text.y = element_markdown(hjust = 1, size = 6),
            axis.text.x = element_text(size = 6),
            axis.title = element_text(size = 7),
            strip.text.y = element_blank())
    
    
    if(plot_icons) {
      for(j in d$variable[!sapply(d$img, is.null)]) {
        d_j <- d %>% dplyr::filter(variable == j)
        plots$bp_slopes[[i]] <- plots$bp_slopes[[i]] +
          rphylopic::geom_phylopic(aes(x = img_offset, y = variable, img = img), color = "#5A5A5A", size = d_j$scale, data = d_j)
      }
    }
  }
  
  plots$Figure_4 <-
    (plots$scatters$a_x_bsim + plots$bp_slopes$a_x_bsim) /
    (plots$scatters$a_x_bstr + plots$bp_slopes$a_x_bstr)
  
  plots$Figure_4 <- plots$Figure_4 +
    plot_annotation(tag_levels = 'a') &
    theme(plot.tag = element_text(size = 8, face = "bold"))
  
  return(plots$Figure_4) 
}

