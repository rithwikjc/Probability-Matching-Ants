library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

file_path1 <- "RawData/FeederChoices_1.xlsx"
file_path2 <- "RawData/FeederChoices_2.xlsx"
df1 <- read_excel(file_path1)
df2 <- read_excel(file_path2)
df1$source_file <- "File 1" 
df2$source_file <- "File 2"
df <- bind_rows(df1, df2)

df$`Left feeder` <- as.numeric(df$`Left feeder`)
df$`Right feeder` <- as.numeric(df$`Right feeder`)

n_rows <- nrow(df)
n_cycles <- ceiling(n_rows/3)

# Data prep
df <- df %>%
  mutate(
    cycle_id = rep(1:n_cycles, each = 3)[1:n_rows],
    time_point = rep(c("Opening", "Closing", "InBetween"), length.out = n_rows)
  )

cycle_info <- df %>% 
  group_by(cycle_id) %>%
  summarize(Opened_feeder_val = first(`Opened feeder`[!is.na(`Opened feeder`)])) %>%
  ungroup()

df <- df %>% left_join(cycle_info, by="cycle_id")

df_long <- df %>%
  select(cycle_id, time_point, `Left feeder`, `Right feeder`, Opened_feeder_val, source_file) %>%
  pivot_longer(cols = c(`Left feeder`, `Right feeder`), 
               names_to = "Feeder", 
               values_to = "Ant_Count") %>%
  filter(!is.na(Ant_Count))

df_long$time_point <- factor(df_long$time_point, levels = c("Opening", "Closing", "InBetween"))

# Prep for extended plots
df_long <- df_long %>%
  mutate(
    Feeder_State = case_when(
      (Feeder == "Left feeder" & Opened_feeder_val == "Left") ~ "Opened",
      (Feeder == "Right feeder" & Opened_feeder_val == "Right") ~ "Opened",
      TRUE ~ "Closed"
    )
  ) %>%
  group_by(Feeder, Feeder_State, time_point) %>%
  mutate(event_id = row_number()) %>%
  ungroup()


# --- 1. Standard Smoothed Trend Plot ---
# Smoothed poisson gam fit
p_smooth <- ggplot(df_long, aes(x = cycle_id, y = Ant_Count, color = Feeder, fill = Feeder)) +
# p_smooth <- ggplot(df_long, aes(x = cycle_id, y = Ant_Count, color = time_point, fill = time_point)) +
  geom_point(alpha = 0.2, size = 1, position = position_jitter(width=0, height=0.05)) +
  geom_smooth(method = "gam", formula = y ~ s(x), method.args = list(family = "poisson"), se = TRUE, span = 0.5) +
  # facet_wrap(~ Feeder, ncol = 1) +
  facet_grid(time_point ~ source_file, scales = "free_x") +
  theme_minimal() +
  labs(title = "Smoothed Trend of Ant Counts over Cycles",
       x = "Cycle", y = "Ant Count", color = "Feeder", fill = "Feeder")
print(p_smooth)

# Save plot
png("Plots/smooth_plot.png", width = 8, height = 6, units = "in", res = 300)
print(p_smooth)
dev.off()
cat("Standard smoothed trend plot saved to smooth_plot.png\n")


# --- 2. Faceted Smoothed Trend Plot (Faceted by Feeder) ---
p_smooth_faceted <- ggplot(df_long, aes(x = cycle_id, y = Ant_Count, color = time_point, fill = time_point)) +
  geom_point(alpha = 0.2, size = 1, position = position_jitter(width=0, height=0.05)) +
  geom_smooth(method = "gam", formula = y ~ s(x), method.args = list(family = "poisson"), se = TRUE, span = 0.5) +
  facet_wrap(Feeder ~ source_file, scales = "free_x") +
  theme_minimal() +
  labs(title = "Smoothed Trend of Ant Counts (Faceted by Feeder)",
       x = "Cycle", y = "Ant Count", color = "Time Point", fill = "Time Point")
print(p_smooth_faceted)

png("Plots/smooth_plot_faceted.png", width = 12, height = 6, units = "in", res = 300)
print(p_smooth_faceted)
dev.off()
cat("Extended smoothed plot saved to p_smooth_faceted.png\n")

