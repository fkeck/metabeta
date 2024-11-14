# Computing variances

dat <- dat %>%
  mutate(var_alpha = ((`SD Reference`^2) / (n_reference * (`Position Reference`^2))) +
           ((`SD Impacted`^2) / (n_impacted * (`Position Impacted`^2))))

dat <- dat %>% 
  mutate(var_beta = map_df(beta_coords, function(x) {
    
    xi <- x %>%
      select(-X, -Y) %>%
      mutate(treatment = ifelse(treatment == "1", "Reference", "Impacted")) %>% 
      mutate(item_id = as.character(row_number()))
    
    d <- dist(x[, c("X", "Y")]) %>% 
      broom::tidy() %>% 
      left_join(xi, by = c("item1" = "item_id")) %>% 
      left_join(xi, by = c("item2" = "item_id"))
    
    t1 <- filter(d, treatment.x == "Reference", treatment.y == "Reference")$distance
    t2 <- filter(d, treatment.x == "Impacted", treatment.y == "Impacted")$distance
    b12 <- filter(d, treatment.x != treatment.y)$distance
    w12 <- filter(d, treatment.x == treatment.y)$distance
    
    n_t1 <- length(t1)
    n_t2 <- length(t2)
    n_b12 <- length(b12)
    n_w12 <- length(w12)
    
    mean_t1 <- mean(t1)
    mean_t2 <- mean(t2)
    mean_b12 <- mean(b12)
    mean_w12 <- mean(w12)
    
    sd_t1 <- sd(t1)
    sd_t2 <- sd(t2)
    sd_b12 <- sd(b12)
    sd_w12 <- sd(w12)
    
    var_beta_similarity <- (sd_t1^2 / (n_t1 * mean_t1^2)) + (sd_t2^2 / (n_t2 * mean_t2^2))
    var_beta_structure <- (sd_b12^2 / (n_b12 * mean_b12^2)) + (sd_w12^2 / (n_w12 * mean_w12^2))
    
    return(tibble(n_t1, n_t2, n_b12, n_w12,
                  mean_t1, mean_t2, mean_b12, mean_w12,
                  sd_t1, sd_t2, sd_b12, sd_w12,
                  var_beta_similarity, var_beta_structure))
    
  })) %>% unnest(var_beta)
