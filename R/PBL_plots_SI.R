plots_cov <- list()

idx <- 1L
for (i in v[-1]) {
  for(j in v[-1]) {
    plots_cov[[idx]] <- dat %>% 
      count(.data[[i]], .data[[j]]) %>% 
      ggplot(aes(x = .data[[i]], y = n, fill = .data[[j]])) +
      geom_bar(position = "dodge", stat = "identity") +
      scale_fill_brewer(type = "qual", palette = "Set1") +
      labs(title = i) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
      theme(plot.title = element_text(size = 7),
            axis.title = element_blank(),
            axis.text.y = element_text(hjust = 1, size = 6),
            axis.text.x = element_text(size = 6),
            strip.text.y = element_blank(),
            legend.text = element_text(size = 6),
            legend.title = element_text(size = 7))
    idx <- idx + 1
  }
}

p1  <- patchwork::wrap_plots(plots_cov[seq(1, 16, by = 4)[-1]], 
                             nrow = 1, ncol = 3) +
  plot_layout(guides = 'collect')

p2  <- patchwork::wrap_plots(plots_cov[seq(2, 16, by = 4)[-2]], 
                             nrow = 1, ncol = 3) +
  plot_layout(guides = 'collect')

p3  <- patchwork::wrap_plots(plots_cov[seq(3, 16, by = 4)[-3]], 
                             nrow = 1, ncol = 3) +
  plot_layout(guides = 'collect')

p4  <- patchwork::wrap_plots(plots_cov[seq(4, 16, by = 4)[-4]], 
                             nrow = 1, ncol = 3) +
  plot_layout(guides = 'collect')


plots$SI_plot_cross <-
  p1/p2/p3/p4 +
  plot_annotation(tag_levels = "a")  &
  theme(plot.tag = element_text(size = 7, face = "bold"))

ggsave("results/SI_plot_cross.pdf", plot = plots$SI_plot_cross, width = 180, height = 240, units = "mm", device = cairo_pdf)

