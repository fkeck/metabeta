
library(tidyverse)
library(patchwork)

sample_com <- function(sp_names, sp_mu, sp_sd, env_pos, size) {
  
  stopifnot(length(sp_names) == nrow(sp_mu) & nrow(sp_mu) == nrow(sp_sd))
  stopifnot(length(env_pos) == ncol(sp_mu) & length(env_pos) == ncol(sp_sd))
  stopifnot(length(size) == 1L)
  
  prob_sp <- function(env_pos, sp_mu, sp_sd) {
    res <- matrix(NA, nrow = nrow(sp_mu), ncol = length(env_pos))
    for(i in seq_along(env_pos)) {
      res[, i] <- mapply(function(pos, mu, sd) { dnorm(pos, mu, sd) },
                         pos = env_pos[i],
                         mu = sp_mu[, i],
                         sd = sp_sd[, i])
    }
    
    res <- apply(res, 1, prod)
    return(res)
  }
  
  com <- sample(sp_names, size = size, replace = TRUE, prob = prob_sp(env_pos, sp_mu, sp_sd))
  com <- table(com)
  res <- vector("integer", length(sp_names))
  names(res) <- sp_names
  res[names(com)] <- com
  return(res)
}


compute_ES <- function(cdm, grp, plot = FALSE, method = "pcoa") {
  
  r <- seq_along(grp) %>% rev()
  
  cdm <- cdm[r, ]
  grp <- grp[r]
  
  cdm_dist <- dist(cdm)
  
  if(method == "pcoa") {
    ordi <- cmdscale(cdm_dist)
  }
  
  if(method == "nmds") {
    ordi <- MASS::isoMDS(cdm_dist)$points
  }
  
  colnames(ordi) <- c("X", "Y")
  pcoa <- as_tibble(ordi)
  
  if(plot) plot(ordi, col = grp)
  
  xi <- grp[grp < 3] %>%
    enframe(name = NULL, value = "treatment") %>%
    mutate(treatment = ifelse(treatment == "1", "Reference", "Impacted")) %>% 
    mutate(item_id = as.character(row_number()))
  
  d <- ordi[grp < 3, ] %>% 
    dist() %>% 
    broom::tidy() %>% 
    left_join(xi, by = c("item1" = "item_id")) %>% 
    left_join(xi, by = c("item2" = "item_id"))
  
  t1 <- filter(d, treatment.x == "Reference", treatment.y == "Reference")$distance
  t2 <- filter(d, treatment.x == "Impacted", treatment.y == "Impacted")$distance
  b12 <- filter(d, treatment.x != treatment.y)$distance
  w12 <- filter(d, treatment.x == treatment.y)$distance
  mean_t1 <- mean(t1)
  mean_t2 <- mean(t2)
  mean_b12 <- mean(b12)
  mean_w12 <- mean(w12)
  
  res <- list(mean_t1 = mean_t1,
              mean_t2 = mean_t2,
              mean_b12 = mean_b12,
              mean_w12 = mean_w12,
              homogeneity = log(mean_t1/mean_t2),
              shift = log(mean_b12/mean_w12))
  return(res)
}



n_rep <- 1000L
sim <- vector("list", n_rep)

for(i in seq(1L, n_rep)) {
  
  n_grp <- sample(3:5, 1)

  n_samp <- sample(20:200, n_grp)

  grp <- mapply(function(x, y) rep(x, y), 1:n_grp, n_samp, SIMPLIFY = FALSE) %>% unlist()
  
  n_env_dim <- sample(3:5, 1)

  n_sp <- sample(10:50, 1)

  sp_names <- paste0("SP", 1:n_sp)
  sp_mu <- runif(n_sp * n_env_dim, min = -1, max = 1) %>% 
    matrix(nrow = n_sp, ncol = n_env_dim, dimnames = list(sp_names, NULL))
  sp_sd <- runif(n_sp * n_env_dim) %>% 
    matrix(nrow = n_sp, ncol = n_env_dim, dimnames = list(sp_names, NULL))
  
  envs <- lapply(n_samp, function(x) rep(list(rnorm(n_env_dim, sd = 0.05)), x)) %>% 
    unlist(recursive = FALSE)
  
  cdm <- lapply(envs, function(x) sample_com(sp_names = sp_names, sp_mu = sp_mu, sp_sd = sp_sd, env_pos = x, size = 300))
  cdm <- do.call(rbind, cdm)
  
  dup <- which(duplicated(cdm))
  if(length(dup > 0)) {
    cdm <- cdm[-dup, ]
    grp <- grp[-dup]
    n_samp <- as.vector(table(grp))
  }
  
  ES_all <- compute_ES(cdm, grp, method = "nmds")
  ES_2 <- compute_ES(cdm[grp < 3, ], grp[grp < 3], method = "nmds")
  
  sim[[i]] <- tibble(n_grp, n_samp = list(n_samp), n_env_dim, n_sp,
                     cdm = list(cdm), grp = list(grp), envs = list(envs),
                     homogeneity_all = ES_all$homogeneity, shift_all = ES_all$shift,
                     homogeneity_2 = ES_2$homogeneity, shift_2 = ES_2$shift,
                     ES_all = list(ES_all), ES_2 = list(ES_2))
  
  print(i)
}

sim <- sim %>% 
  bind_rows()

sim <- sim %>%
  unnest_wider(ES_2, names_sep = "_") %>%
  unnest_wider(ES_all, names_sep = "_")

sim <- sim %>% 
  mutate(homogeneity_ratio = homogeneity_all / homogeneity_2,
         homogeneity_diff = homogeneity_2 - homogeneity_all,
         homogeneity_bias = homogeneity_diff/abs(homogeneity_2),
         shift_ratio = shift_all / shift_2,
         shift_diff = shift_2 - shift_all,
         shift_bias = shift_diff/abs(shift_2))



# PCOA
sim <- readRDS("computed/sim_beta_pcoa.rds")

# HOMOGENEITY
p_PCOA_LRRH_xy <- sim %>% 
  ggplot(aes(homogeneity_2, homogeneity_all)) +
  geom_abline(aes(slope = 1, intercept = 0), linewidth = 0.3) +
  geom_point(alpha = 0.2, size = 2) +
  xlab("LRR homogeneity (2 groups)") +
  ylab("LRR homogeneity (>2 groups)") +
  theme_bw()
  
p_PCOA_LRRH_hist <- sim %>% 
  ggplot(aes(homogeneity_diff)) +
  geom_histogram() +
  xlab("LRR homogenity differences") +
  theme_bw()

test_PCOA_LRRH_cor <- cor.test(sim$homogeneity_2, sim$homogeneity_all)
test_PCOA_LRRH_diff <- t.test(sim$homogeneity_diff, mu = 0)
t.test(sim$homogeneity_bias, mu = 0)

# SHIFT
p_PCOA_LRRS_xy <- sim %>% 
  ggplot(aes(shift_2, shift_all)) +
  geom_abline(aes(slope = 1, intercept = 0), linewidth = 0.3) +
  geom_point(alpha = 0.2, size = 2) +
  xlab("LRR shift (2 groups)") +
  ylab("LRR shift (>2 groups)") +
  theme_bw()

p_PCOA_LRRS_hist <- sim %>% 
  ggplot(aes(shift_diff)) +
  geom_histogram() +
  xlab("LRR shift differences") +
  theme_bw()

test_PCOA_LRRS_cor <- cor.test(sim$shift_2, sim$shift_all)
test_PCOA_LRRS_diff <- t.test(sim$shift_diff, mu = 0)


# NMDS
sim <- readRDS("computed/sim_beta_nmds.rds")

# HOMOGENEITY
p_NMDS_LRRH_xy <- sim %>% 
  ggplot(aes(homogeneity_2, homogeneity_all)) +
  geom_abline(aes(slope = 1, intercept = 0), linewidth = 0.3) +
  geom_point(alpha = 0.2, size = 2) +
  xlab("LRR homogeneity (2 groups)") +
  ylab("LRR homogeneity (>2 groups)") +
  theme_bw()

p_NMDS_LRRH_hist <- sim %>% 
  ggplot(aes(homogeneity_diff)) +
  geom_histogram() +
  xlab("LRR homogenity differences") +
  theme_bw()

test_NMDS_LRRH_cor <- cor.test(sim$homogeneity_2, sim$homogeneity_all)
test_NMDS_LRRH_diff <- t.test(sim$homogeneity_diff, mu = 0)

# SHIFT
p_NMDS_LRRS_xy <- sim %>% 
  ggplot(aes(shift_2, shift_all)) +
  geom_abline(aes(slope = 1, intercept = 0), linewidth = 0.3) +
  geom_point(alpha = 0.2, size = 2) +
  xlab("LRR shift (2 groups)") +
  ylab("LRR shift (>2 groups)") +
  theme_bw()

p_NMDS_LRRs_hist <- sim %>% 
  ggplot(aes(shift_diff)) +
  geom_histogram() +
  xlab("LRR shift differences") +
  theme_bw()

test_NMDS_LRRS_cor <- cor.test(sim$shift_2, sim$shift_all)
test_NMDS_LRRS_diff <- t.test(sim$shift_diff, mu = 0)

plots_sim_beta <- list()

plots_sim_beta$xy <- (p_PCOA_LRRH_xy + p_PCOA_LRRS_xy) / (p_NMDS_LRRH_xy + p_NMDS_LRRS_xy) +
  plot_annotation(tag_levels = 'a') &
  theme(plot.tag = element_text(size = 8, face = "bold"))

plots_sim_beta$hist <- (p_PCOA_LRRH_hist + p_PCOA_LRRS_hist) / (p_NMDS_LRRH_hist + p_NMDS_LRRs_hist) +
  plot_annotation(tag_levels = 'a') &
  theme(plot.tag = element_text(size = 8, face = "bold"))

