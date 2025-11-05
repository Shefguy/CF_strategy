library(tidyverse)

# Daten laden
full_portfolio_paths_tibble <- readRDS("data/full_portfolio_paths_tibble.RDS")

# 1. Die Anzahl der zu plottenden Pfade
sample_size <- 30

# 2. Anyzahl aller Simulationen
N_simulations <- pull(full_portfolio_paths_tibble, sample_id) %>% n_distinct()

# 3. Zufaellige Indizes generieren
set.seed(42) 
random_indices <- sample(1:N_simulations, size = sample_size, replace = FALSE)


mean_portfolio_path <-
  full_portfolio_paths_tibble %>% 
  group_by(time_step) %>% 
  summarise(value = mean(value))

plot_sample_paths_vs_mean <-
  full_portfolio_paths_tibble %>% 
  filter(sample_id %in% random_indices) %>% 
  group_by(sample_id) %>% 
  ggplot(aes(x = time_step, y = value, color = sample_id)) +
  geom_line(lwd = 1, alpha = 0.3) +
  geom_line(data = mean_portfolio_path, color = "black", linetype = "dashed", lwd = 1) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "€", big.mark = ".")) +
  labs(x = "Zeit [Monate]", y = "Portfoliowert",
       title = "Simulierte Wertentwicklung von 30 Beispielszenarien",
       subtitle = "Simuliert mit Brownscher Bewegung"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

x <- 5
ggsave("figs/plot_sample_paths_vs_mean.png", plot_sample_paths_vs_mean,
       width = 4 * x, height = 3 * x, units = "cm")
