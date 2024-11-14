
library(tidyverse)


geo <- read_csv("data/CI_geo_distances.csv")


parse_coords <- function (x) {
  x <- str_remove_all(x, "[:blank:]")

  if(str_detect(x, "-?[0-9]+,?[0-9]*;-?[0-9]+,?[0-9]*")) {
    x <- str_replace_all(x, ",", "\\.")
    x <- str_replace_all(x, ";", ",")
  }
  
  x <- str_remove(x, "^\n+")
  x <- str_remove(x, "\n+$")
  res <- read.csv(text = x, header = FALSE)
  res <- as_tibble(res)
  if (ncol(res) == 2L) {
    colnames(res) <- c("X", "Y")
  } else if (ncol(res) == 4L) {
    colnames(res) <- c("X", "Y", "id", "group")
    res$group <- as.factor(res$group)
  } else {
    stop("parsing error", x)
  }
  return(res)
}

geo <- geo %>% 
  mutate(Coords_Ref = map(Coords_Ref, parse_coords),
         Coords_Impact = map(Coords_Impact, parse_coords),
         Coords = map2(Coords_Ref, Coords_Impact, bind_rows, .id = "treatment"))

# Compute distance matrices
geo <- geo %>% 
  mutate(stats = map(Coords, function(x) {
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
    mean_t1 <- mean(t1)
    mean_t2 <- mean(t2)
    mean_b12 <- mean(b12)
    mean_w12 <- mean(w12)
    
    res <- list(mean_t1 = mean_t1,
                mean_t2 = mean_t2,
                mean_b12 = mean_b12,
                mean_w12 = mean_w12,
                homogeneity = log(mean_t2/mean_t1) * -1,
                shift = log(mean_b12/mean_w12))
    return(res)
  }))

geo <- geo %>%
  unnest_wider(stats)

geo <- geo %>%
  left_join(dat, by = "Article_ID", multiple = "first")

ggplot(geo) +
  geom_histogram(aes(homogeneity)) +
  xlab("LRR homogeneity (spatial distances)") + 
  geom_vline(aes(xintercept = 0), linetype = 1) +
  theme_bw()

t.test(geo$homogeneity)

ggplot(geo) +
  geom_histogram(aes(shift)) +
  xlab("LRR shift (spatial distances)") + 
  geom_vline(aes(xintercept = 0), linetype = 1) +
  theme_bw()

t.test(geo$shift)


ggplot(geo) +
  geom_point(aes(homogeneity, beta_similarity)) +
  xlab("LRR homogeneity (spatial distances)") + 
  ylab("LRR homogeneity (community distances)") + 
  theme_bw()

ggplot(geo) +
  geom_point(aes(shift, beta_structure)) +
  xlab("LRR shift (spatial distances)") + 
  ylab("LRR shift (community distances)") + 
  theme_bw()

cor.test(geo$beta_similarity, geo$homogeneity)
cor.test(geo$beta_structure, geo$shift)


# geo %>% 
#   select(homogeneity, beta_similarity, shift, beta_structure) %>% 
#   write_rds("computed/geo.rds")
