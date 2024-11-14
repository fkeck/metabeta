
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
            + (1|Article_ID),
          data = ., family = gaussian())

models$multi_fac$mod_beta_similarity <- dat %>%
  glmmTMB(beta_similarity ~ Biome + Pressure + Organism + Scale
          + (1|Article_ID),
          data = ., family = gaussian())

models$multi_fac$mod_beta_structure <- dat %>%
  glmmTMB(beta_structure ~ Biome + Pressure + Organism + Scale +
            + (1|Article_ID),
          data = ., family = gaussian())


summary(models$multi_fac$mod_alpha_diversity)
car::Anova(models$multi_fac$mod_alpha_diversity)

summary(models$multi_fac$mod_beta_structure)
car::Anova(models$multi_fac$mod_beta_structure)

summary(models$multi_fac$mod_beta_similarity)
car::Anova(models$multi_fac$mod_beta_similarity)

opti_eng <- glmmTMBControl(optimizer = optim,
                           optArgs = list(method = "BFGS"))
opti_eng <- glmmTMBControl(optimizer = nlminb,
                           optCtrl = list(iter.max = 1e5, eval.max = 1e5))

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
      if(i == "alpha_diversity") {
        mod <- glmmTMB(reformulate(paste0(f_mod, "+ (1|Article_ID)"), i),
                       data = dat, family = gaussian(), control = opti_eng)
      } else {
        mod <- glmmTMB(reformulate(paste0(f_mod, "+ (1|Article_ID)"), i),
                       data = dat, family = gaussian(), control = opti_eng)
      }
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

