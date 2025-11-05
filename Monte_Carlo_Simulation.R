library(alphavantager)
library(tidyverse)

source("f_get_time_series_ticker.R")

historical_data <-
  get_time_series_ticker() %>% 
  select(date, adjusted_close)

# Monatsrenditen berechnen
monthly_returns <- 
  historical_data %>%
  mutate(returns = log(adjusted_close / lag(adjusted_close))) %>% 
  filter(!is.na(returns))

# Mu (mittlere Monatsrendite) und Sigma (Volatilität der Monatsrenditen)
mu_m_real <- mean(monthly_returns$returns)
sigma_m_real <- sd(monthly_returns$returns)

# ---------------------------------------------------------------------------
# second step

# --- 1. Parameter und Konstanten definieren ---
P0 <- 200000        # Startkapital
W <- 100000         # Liquiditätsbedarf
T_years <- 5        # Zeithorizont in Jahren
T_months <- T_years * 12 # Zeithorizont in Monaten
N_sim <- 10000      # Anzahl der Simulationen
dt <- 1/12
monthly_withdrawal <- W / T_months

# Nutze die REAL berechneten Parameter:
mu_m <- mu_m_real
sigma_m <- sigma_m_real

# --- 2. Monte Carlo Funktion (GBM) ---
simulate_gbm <- function(P_start, mu, sigma, T_months, N_sim) {
  price_paths <- matrix(P_start, nrow = N_sim, ncol = T_months + 1)
  
  for (t in 2:(T_months + 1)) {
    # Annahme: mu und sigma sind bereits monatlich (Log-Renditen)
    # Hier wird ein einfaches exponentielles Wachstum verwendet,
    # da mu_m_real bereits skaliert ist.
    drift <- (mu - 0.5 * sigma^2) 
    volatility_term <- sigma * rnorm(N_sim)
    
    # Wende die monatliche Rendite an
    price_paths[, t] <- price_paths[, t-1] * exp(drift + volatility_term)
  }
  return(price_paths)
}

# --- 3. Simulationsläufe ---
set.seed(42) 
full_portfolio_paths <- simulate_gbm(P0, mu_m, sigma_m, T_months, N_sim)


# --- 4. Strategien implementieren ---

# S1: Sofortiger Verkauf (Lump Sum Sell Now)
results_S1 <- full_portfolio_paths[, T_months + 1] / 2 
s1_paths <- full_portfolio_paths / 2
s1_paths[,1] <- full_portfolio_paths[,1]
s1_paths <- 
  s1_paths %>% 
  as_tibble(rownames = "sample_id") %>% 
  pivot_longer(
    cols = starts_with("V"),
    names_to = "time_step",
    names_prefix = "V",
    values_to = "value"
  ) %>% 
  mutate(time_step = as.numeric(time_step))

# S2: Später Verkauf (Lump Sum Sell Later)
results_S2 <- full_portfolio_paths[, T_months + 1] - W
s2_paths <- full_portfolio_paths
s2_paths[, T_months + 1] <- full_portfolio_paths[, T_months + 1] - W
s2_paths <- 
  s2_paths %>% 
  as_tibble(rownames = "sample_id") %>% 
  pivot_longer(
    cols = starts_with("V"),
    names_to = "time_step",
    names_prefix = "V",
    values_to = "value"
  ) %>% 
  mutate(time_step = as.numeric(time_step))

# S3: Gestaffelter Verkauf (Phased Selling)
results_S3 <- numeric(N_sim)
s3_paths <- matrix(P0, nrow = N_sim, ncol = T_months + 1)
for (i in 1:N_sim) {
  current_portfolio <- P0
  
  for (t in 1:T_months) {
    # Berechne den Skalierungsfaktor basierend auf der Rendite des vollen Pfades
    monthly_growth_factor <- full_portfolio_paths[i, t+1] / full_portfolio_paths[i, t]
    
    # Der verbleibende Aktienwert wächst
    current_portfolio <- current_portfolio * monthly_growth_factor
    
    # Führe den monatlichen Verkauf durch
    current_portfolio <- current_portfolio - monthly_withdrawal
    
    if (current_portfolio < 0) current_portfolio <- 0
    
    s3_paths[i,t+1] <- current_portfolio
  }
  # Der Gesamte Pfad wird iterativ gespeichert
  results_S3[i] <- current_portfolio
}
s3_paths <- 
  s3_paths %>% 
  as_tibble(rownames = "sample_id") %>% 
  pivot_longer(
    cols = starts_with("V"),
    names_to = "time_step",
    names_prefix = "V",
    values_to = "value"
  ) %>% 
  mutate(time_step = as.numeric(time_step))


# S4: Dynamischer Verkauf (Threshold Strategy)
monthly_dynamic_withdrawal <- 1.3 * monthly_withdrawal
results_S4 <- numeric(N_sim)
s4_paths <- matrix(P0, nrow = N_sim, ncol = T_months + 1)
for (i in 1:N_sim) {
  portfolio_value <- P0
  cash_needed <- W
  
  for (t in 1:T_months) {
    # Monatsrendite
    monthly_return_factor <- full_portfolio_paths[i, t+1] / full_portfolio_paths[i, t]
    portfolio_value <- portfolio_value * monthly_return_factor
    
    # Verkauf: Nur, wenn die Rendite positiv war und noch Liquidität benötigt wird
    if (monthly_return_factor > 1.00 && cash_needed > 0) {
      amount_to_sell <- min(monthly_dynamic_withdrawal, cash_needed)
      portfolio_value <- portfolio_value - amount_to_sell
      cash_needed <- cash_needed - amount_to_sell
    } 
  # Der Gesamte Pfad wird iterativ gespeichert
    s4_paths[i, t +1] <- portfolio_value
  }
  # Am Ende muss der Restbedarf gedeckt werden
  if (cash_needed > 0) {
    portfolio_value <- portfolio_value - cash_needed
    # Der letzte Wert wird angepasst, falls mehr als die definierte Rate verkauft werden muss
    s4_paths[i, T_months + 1] <- portfolio_value
  }
  
  results_S4[i] <- portfolio_value
}
s4_paths <- 
  s4_paths %>% 
  as_tibble(rownames = "sample_id") %>% 
  pivot_longer(
    cols = starts_with("V"),
    names_to = "time_step",
    names_prefix = "V",
    values_to = "value"
  ) %>% 
  mutate(time_step = as.numeric(time_step))

# --- 5. Ergebnisse zusammenfassen und visualisieren ---
results_df <- data.frame(
  Strategy = rep(c("S1: Lump Sum Now", "S2: Lump Sum Later", "S3: Phased Selling", "S4: Dynamic (Positive Return)"), each = N_sim),
  Final_Portfolio_Value = c(results_S1, results_S2, results_S3, results_S4)
)

summary_results <- results_df %>%
  group_by(Strategy) %>%
  summarise(
    Mean = mean(Final_Portfolio_Value),
    Median = median(Final_Portfolio_Value),
    Worst_Case_5th = quantile(Final_Portfolio_Value, 0.05),
    Best_Case_95th = quantile(Final_Portfolio_Value, 0.95),
    Min = min(Final_Portfolio_Value),
    Max = max(Final_Portfolio_Value)
  ) %>%
  mutate(
    Strategy = factor(Strategy, levels = c("S1: Lump Sum Now", "S2: Lump Sum Later", "S3: Phased Selling", "S4: Dynamic (Positive Return)"))
  )

print(summary_results)

# Visualisierung
plot_box <-
  ggplot(results_df, aes(x = Strategy, y = Final_Portfolio_Value, fill = Strategy)) +
  geom_boxplot(outliers = TRUE, alpha = 0.7) + 
  geom_hline(yintercept = 100000, linetype = "dashed", color = "black") + # Startwert des Rests
  scale_y_continuous(labels = scales::dollar_format(prefix = "€", big.mark = ".")) +
  labs(
    title = paste("Monte Carlo Simulation: Verkaufsstrategien (EUNL.DE-Daten)"),
    subtitle = paste("μ (Monats-Log-Rendite) =", round(mu_m_real, 4), " | σ (Monats-Volatilität) =", round(sigma_m_real, 4), " | N =", N_sim),
    x = "Verkaufsstrategie",
    y = "Verbleibendes Portfoliovermögen (nach 100K Entnahme)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Speichern der Plots
path_plot_box <- "figs/strategy_summary.png"

# Dimensionen Festlegen
base_length <- 5
width <- 4 * base_length
height <- 3 * base_length
units <- "cm"

ggsave(path_plot_box, plot_box, width = width, height = height, units = units)

# Selber Plot wie oben nur ohne Outlier
plot_box_no_outliers <-
  ggplot(results_df, aes(x = Strategy, y = Final_Portfolio_Value, fill = Strategy)) +
  # geom_boxplot(outlier.shape = NA, alpha = 0.7) + 
  geom_boxplot(outliers = FALSE, alpha = 0.7) + 
  geom_hline(yintercept = 100000, linetype = "dashed", color = "black") + # Startwert des Rests
  scale_y_continuous(labels = scales::dollar_format(prefix = "€", big.mark = ".")) +
  # coord_cartesian(ylim = quantile(results_df$Final_Portfolio_Value, c(0.01, 0.99))) + # Entferne Ausreißer für bessere Sicht
  labs(
    title = paste("Monte Carlo Simulation: Verkaufsstrategien (EUNL.DE-Daten)"),
    subtitle = paste("μ (Monats-Log-Rendite) =", round(mu_m_real, 4), " | σ (Monats-Volatilität) =", round(sigma_m_real, 4), " | N =", N_sim),
    x = "Verkaufsstrategie",
    y = "Verbleibendes Portfoliovermögen (nach 100K Entnahme)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

path_plot_box_no_outliers <- "figs/strategy_summary_no_outliers.png"

ggsave(path_plot_box_no_outliers, plot_box_no_outliers, width = width, height = height, units = units)


# alle Pfade in einen Tibble zusammenfuehren
all_scenarios <-
  bind_rows(
    s1_paths %>% 
      mutate(Scenario = "Sofortiger Verkauf"),
    s2_paths %>% 
      mutate(Scenario = "Später Verkauf"),
    s3_paths %>% 
      mutate(Scenario = "Gestaffelter Verkauf"),
    s4_paths %>% 
      mutate(Scenario = "Dynamischer Verkauf")
  ) %>% 
  mutate(
    time_step = time_step - 1,
    sample_id = factor(sample_id)
  )

# Die simulierten Pfade als tibble
full_portfolio_paths_tibble <-
  full_portfolio_paths %>% 
  as_tibble( rownames = "sample_id") %>% 
  mutate(sample_id = factor(sample_id)) %>% 
  pivot_longer(
    cols = starts_with("V"),
    names_to = "time_step",
    names_prefix = "V",
    values_to = "value"
  ) %>% 
  mutate(time_step = as.numeric(time_step) - 1)

# Die Daten speichern

saveRDS(all_scenarios, "all_scenarios.RDS")

saveRDS(full_portfolio_paths_tibble, "full_portfolio_paths_tibble.RDS")
