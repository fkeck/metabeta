rotate_beta <- function(x) {
  
  x$X <- scales::rescale(x$X, to = c(0, 1))
  x$Y <- scales::rescale(x$Y, to = c(0, 1))
  
  x_mean <- tapply(x$X, x$treatment, mean)
  y_mean <- tapply(x$Y, x$treatment, mean)
  x_bary <- mean(x_mean)
  y_bary <- mean(y_mean)
  
  theta <- atan((y_mean[2] - y_mean[1]) / (x_mean[2] - x_mean[1])) #* 180 / pi
  
  if(x_mean[1] > x_mean[2]) { theta <- theta + pi }
  
  x_rot <- x$X * cos(theta) + x$Y * sin(theta)
  y_rot <- x$Y * cos(theta) - x$X * sin(theta)
  
  x_rot_mean <- tapply(x_rot, x$treatment, mean)
  y_rot_mean <- tapply(y_rot, x$treatment, mean)
  x_rot_bary <- mean(x_rot_mean)
  y_rot_bary <- mean(y_rot_mean)
  
  res <- tibble(treatment = x$treatment,
                X = x_rot - x_rot_bary,
                Y = y_rot - y_rot_bary)
  
  return(res)
}


#### Stacked beta-diversity plots ####
# 13 * 14
comp <- lapply(dat$beta_coords, rotate_beta) %>% 
  bind_rows() %>% 
  slice_sample(prop = 1) %>% 
  mutate(treatment = ifelse(treatment == "2", "0", "1"))


ggplot(comp) +
  geom_point(aes(X, Y, color = treatment, group = cut(sample(seq_along(X)), 500)), size = 2, shape = ".") +
  scale_colour_manual(values = c("#BA2F2AFF", "#088158FF")) +
  coord_fixed() +
  theme_void() +
  theme(legend.position = "none")
#theme(plot.background = element_rect(fill = 'black'))
  