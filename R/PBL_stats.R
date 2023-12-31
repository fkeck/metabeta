
# Summary stats
dat %>% count()
dat$Article_ID %>% unique() %>% length()
dat$n_reference %>% sum()
dat$n_impacted %>% sum()
c(dat$n_reference, dat$n_impacted) %>% sum()

(!is.na(dat$alpha_diversity)) %>% sum()
dat[!is.na(dat$alpha_diversity),]$Article_ID %>% unique() %>% length()

# Models

models$multi_fac$mod_alpha_diversity <- dat %>% 
  glmmTMB(alpha_diversity ~ Biome + Pressure + Organism + Scale +
            + (1|Article_ID) + (1|`Alpha-diversity index`) + (1|`Study type`),
          data = ., family = gaussian())


models$multi_fac$mod_beta_similarity <- dat %>%
  glmmTMB(beta_similarity ~ Biome + Pressure + Organism + Scale
          + (1|Article_ID) + (1|`Type of plot extracted`) + (1|`Study type`),
          data = ., family = gaussian())

models$multi_fac$mod_beta_structure <- dat %>%
  glmmTMB(beta_structure ~ Biome + Pressure + Organism + Scale +
            + (1|Article_ID) + (1|`Type of plot extracted`) + (1|`Study type`),
          data = ., family = gaussian())


summary(models$multi_fac$mod_alpha_diversity)
car::Anova(models$multi_fac$mod_alpha_diversity)

summary(models$multi_fac$mod_beta_structure)
car::Anova(models$multi_fac$mod_beta_structure)

summary(models$multi_fac$mod_beta_similarity)
car::Anova(models$multi_fac$mod_beta_similarity)


# One factor models
for (i in c("alpha_diversity", "beta_similarity", "beta_structure")) {
  models$one_fac[[i]] <-
    map(c(1, v[-1]), function(x) {
      if(x == 1) {
        f_mod = "1"
        f_em = ~ 1
      } else {
        f_mod <- paste0("`", x , "`")
        f_em <- x
      }
      mod <- glmmTMB(reformulate(paste0(f_mod, "+ (1|Article_ID) + (1|`Study type`)"), i), data = dat, family = gaussian())
      n <- mod$frame %>%
        group_by(across(2)) %>%
        count() %>%
        set_names(c("variable", "n"))
      res <- bind_cols(
        emmeans(mod, x) %>% confint(level = 0.95) %>% as_tibble() %>%
          set_names(c("variable", "emmean", "SE", "df", "lower.CL95", "upper.CL95")),
        emmeans(mod, x) %>% confint(level = 0.99) %>% as_tibble() %>%
          select(lower.CL, upper.CL) %>% 
          set_names(c("lower.CL99", "upper.CL99"))
      )
      res <- left_join(res, n, by = "variable") %>% 
        mutate(mod = list(mod))
      if(x == 1) {
        res$variable <- "All data"
        res$n <- mod$modelInfo$nobs
      }
      return(res)
    }) %>%
    set_names(v) %>% 
    bind_rows(.id = "plot_id")
}


# Regressions
models$reg_subgrp <- list(
  map(v, function(x) { dat %>%
      group_by(.data[[x]]) %>%
      do(mod = glmmTMB(beta_structure ~ alpha_diversity + (1|Article_ID), data = ., family = gaussian())) %>% 
      rename(variable = .data[[x]]) }) %>% 
    set_names(v) %>% 
    bind_rows(.id = "plot_id") %>% 
    mutate(x = "alpha_diversity", y = "beta_structure"),
  
  map(v, function(x) { dat %>%
      group_by(.data[[x]]) %>%
      do(mod = glmmTMB(beta_similarity ~ alpha_diversity + (1|Article_ID), data = ., family = gaussian())) %>% 
      rename(variable = .data[[x]]) }) %>% 
    set_names(v) %>% 
    bind_rows(.id = "plot_id") %>% 
    mutate(x = "alpha_diversity", y = "beta_similarity"),
  
  map(v, function(x) { dat %>%
      group_by(.data[[x]]) %>%
      do(mod = glmmTMB(beta_similarity ~ beta_structure + (1|Article_ID), data = ., family = gaussian())) %>% 
      rename(variable = .data[[x]]) }) %>% 
    set_names(v) %>% 
    bind_rows(.id = "plot_id") %>% 
    mutate(x = "beta_structure", y = "beta_similarity")
) %>% bind_rows()

models$reg_subgrp <- map(models$reg_subgrp$mod, broom.mixed::tidy, effects = "fixed", conf.int = TRUE) %>% 
  set_names(models$reg_subgrp$variable) %>% 
  bind_rows() %>% 
  filter(term != "(Intercept)") %>% 
  bind_cols(models$reg_subgrp, .) %>% 
  ungroup() %>% 
  mutate(plot_id = fct_relevel(plot_id,
                               "Global",
                               "Biome",
                               "Pressure",
                               "Organism",
                               "Scale"))


