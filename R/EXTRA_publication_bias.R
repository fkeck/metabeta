
# Publication bias assessment

stouffer <- function(pp) {
  pp[pp == 0] <- 2.2e-16
  z <- sum(qnorm(pp), na.rm = TRUE) / sqrt(sum(!is.na(pp)))
  p_value <- pnorm(z)
  list(z = z, p_value = p_value)
}

test_pub_bias <- list()

# BETA SIMILARITY

test_pub_bias$beta_similarity_funnel <-
  ggplot(dat) +
  geom_vline(aes(xintercept = mean(beta_similarity)), linewidth = 0.2) +
  geom_point(aes(beta_similarity, -sqrt(dat$var_beta_similarity)), alpha = 0.2) +
  xlab("LRR homogeneity") +
  ylab("Standard Error") +
  scale_y_continuous(labels = abs) +
  theme_bw()
  

test_pub_bias$beta_similarity_fsn <- metafor::fsn(dat$beta_similarity, vi = dat$var_beta_similarity, type = "General")

pvals <- rep(NA, nrow(dat)) %>%
  as.numeric()
for(i in 1:nrow(dat)) {
  coords <- dat$beta_coords[[i]]
  d <- dist(coords[, c("X", "Y")])
  test <- vegan::betadisper(d, coords$treatment) %>%
    vegan::permutest(permutations = 999)
  pvals[i] <- test$tab$`Pr(>F)`[1]
  print(i)
}

pp_full <- pvals[pvals < 0.05 & !is.na(pvals)] * 20
pp_half <- pvals[pvals < 0.025 & !is.na(pvals)] * 40

test_pub_bias$beta_similarity_rskew_full <- stouffer(pp_full)
test_pub_bias$beta_similarity_rskew_half <- stouffer(pp_half)


# BETA STRUCTURE

test_pub_bias$beta_structure_funnel <-
  ggplot(dat) +
  geom_vline(aes(xintercept = mean(beta_structure)), linewidth = 0.2) +
  geom_point(aes(beta_structure, -sqrt(dat$var_beta_structure)), alpha = 0.2) +
  xlab("LRR shift") +
  ylab("Standard Error") +
  scale_y_continuous(labels = abs) +
  theme_bw()

test_pub_bias$beta_structure_fsn <- metafor::fsn(dat$beta_structure, vi = dat$var_beta_structure, type = "General")


pvals <- rep(NA, nrow(dat)) %>%
  as.numeric()
for(i in 1:nrow(dat)) {
  coords <- dat$beta_coords[[i]]
  d <- dist(coords[, c("X", "Y")])
  test <- vegan::adonis2(d ~ coords$treatment, permutations = 999)
  pvals[i] <- test$`Pr(>F)`[1]
  print(i)
}

pp_full <- pvals[pvals < 0.05 & !is.na(pvals)] * 20
pp_half <- pvals[pvals < 0.025 & !is.na(pvals)] * 40

test_pub_bias$beta_structure_rskew_full <- stouffer(pp_full)
test_pub_bias$beta_structure_rskew_half <- stouffer(pp_half)


# ALPHA DIVERSITY

test_pub_bias$alpha_funnel <-
  ggplot(dat[-2188, ]) +
  geom_vline(aes(xintercept = mean(dat$alpha_diversity[!is.na(dat$var_alpha)], na.rm = TRUE)), linewidth = 0.2) +
  geom_point(aes(alpha_diversity, -sqrt(var_alpha)), alpha = 0.2) +
  xlab("LRR local diversity") +
  ylab("Standard Error") +
  scale_y_continuous(labels = abs) +
  theme_bw()


test_pub_bias$alpha_fsn <- metafor::fsn(dat$alpha_diversity, vi = dat$var_alpha, type = "General")

pvals <- rep(NA, nrow(dat)) %>%
  as.numeric()
for(i in 1:nrow(dat)) {
  nx <- dat$n_reference[i]
  mx <- dat$`Position Reference`[i]
  vx <- dat$`SD Reference`[i]^2
  ny <- dat$n_impacted[i]
  my <- dat$`Position Impacted`[i]
  vy <- dat$`SD Impacted`[i]^2
  stderrx <- sqrt(vx/nx)
  stderry <- sqrt(vy/ny)
  stderr <- sqrt(stderrx^2 + stderry^2)
  df <- stderr^4/(stderrx^4/(nx - 1) + stderry^4/(ny - 1))
  tstat <- (mx - my)/stderr
  pvals[i] <- 2 * pt(-abs(tstat), df)
  print(i)
}

pp_full <- pvals[pvals < 0.05 & !is.na(pvals)] * 20
pp_half <- pvals[pvals < 0.025 & !is.na(pvals)] * 40

test_pub_bias$alpha_rskew_full <- stouffer(pp_full)
test_pub_bias$alpha_rskew_half <- stouffer(pp_half)

saveRDS(test_pub_bias, "computed/test_pub_bias.rds")
