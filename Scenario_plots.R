library(tidyverse)

# Daten Laden
all_scenarios <- readRDS("all_scenarios.RDS")

# Den Pfad mit der schlechtesten Performance finden
worst_scenario <-
  all_scenarios %>% 
  slice_max(time_step) %>% 
  slice_min(value) %>% 
  pull(sample_id)

# Den Pfad mit der besten Performance finden
best_scenario <-
  all_scenarios %>% 
  slice_max(time_step) %>% 
  slice_max(value) %>% 
  pull(sample_id)

# Der Pfad mit dem groessten verfall und anschliessender Erholung
largest_drop_and_recovery <-
  all_scenarios %>% 
  filter(time_step != 60) %>% 
  filter(Scenario == "Später Verkauf") %>% 
  group_by(sample_id) %>% 
  mutate(
    min_value = min(value),
    drop_amount = 200000 - min_value,
    recovers = any(value > 190000 & row_number() > which.min(value))
  ) %>% 
  filter(recovers) %>% 
  arrange(-drop_amount) %>% 
  pluck("sample_id", 1)

# Der  Pfad mit dem groessten Anstieg und anschliessendem Fall zurueck zum Ausganswert
largest_rise_and_fall <-
  all_scenarios %>% 
  filter(time_step != 60) %>% 
  filter(Scenario == "Später Verkauf") %>% 
  group_by(sample_id) %>% 
  mutate(
    max_value = max(value),
    rise_amount =  max_value - 200000,
    recovers = any(value < 210000 & row_number() > which.max(value))
  ) %>% 
  filter(recovers) %>% 
  arrange(-rise_amount) %>% 
  pluck("sample_id", 1)

# Der Pfad mit der kleinsten Bewegung
smallest_diff <-
  all_scenarios %>% 
  filter(time_step != 60) %>% 
  filter(Scenario == "Später Verkauf") %>% 
  group_by(sample_id) %>% 
  mutate(
    min_value = min(value),
    max_value = max(value),
    drop_amount =   200000 - min_value,
    rise_amount = -(200000 - max_value),
    max_diff = drop_amount + rise_amount
  ) %>% 
  arrange(max_diff) %>% 
  pluck("sample_id", 1)

# Zusammenfassung der Ergebnisse
example_ids <- 
  tribble(
    ~sample_id, ~description,
    worst_scenario, "Schlechteste Performance",
    best_scenario, "Beste Performance",
    largest_drop_and_recovery, "Fall und Anstieg",
    # smallest_diff, "Flache Entwicklung",
    largest_rise_and_fall, "Anstieg und Fall"
  )

# Kombinierter Plot aller Beispielszenarien
plot_scenario_path_comparison <-
  all_scenarios %>% 
  inner_join(example_ids, by = "sample_id") %>% 
  bind_rows(
    .,
    slice_max(., time_step) %>% 
      mutate(time_step = time_step + 1)
  ) %>% 
  ggplot(aes(x = time_step, y = value, color = Scenario)) +
  geom_line(lwd = 1, alpha = 0.8) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "€", big.mark = ".")) +
  facet_wrap(~description, scales = "free") +
  labs(
    x = "Zeit [Monate]",
    y = "Portfoliowert",
    title = "Wertentwicklung fuer Unterschiedliche Wertentwicklungen"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

x <- 5
ggsave("figs/plot_scenario_path_comparison.png", plot_scenario_path_comparison,
       width = 4 * x, height = 3 * x, units = "cm")

# Mittlere Performance aller Verkaufsstrategien
plot_mean_scenario_paths <-
  all_scenarios %>% 
  group_by(time_step, Scenario) %>% 
  summarise(value = mean(value)) %>% 
  ggplot(aes(x = time_step, y = value, color = Scenario)) +
  geom_line(lwd = 1) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "€", big.mark = ".")) +
  labs(
    x = "Zeit [Monate]",
    y = "Portfoliowert",
    title = "Wertentwicklung je nach Verkaufsstrategie",
    subtitle = "Mittelwerte einer Monte Carlo Simulation mit Braunscher Bewegung \nauf Basis beobachteter Wertentwicklungen."
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")


# x <- 5
ggsave("figs/plot_mean_scenario_paths.png", plot_mean_scenario_paths,
       width = 4 * x, height = 3 * x, units = "cm")
