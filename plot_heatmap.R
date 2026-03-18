library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

file_path1 <- "RawData/FeederChoices_1.xlsx"
file_path2 <- "RawData/FeederChoices_2.xlsx"
df1 <- read_excel(file_path1)
df2 <- read_excel(file_path2)
df <- bind_rows(df1, df2)

df$`Left feeder` <- as.numeric(df$`Left feeder`)
df$`Right feeder` <- as.numeric(df$`Right feeder`)

n_rows <- nrow(df)
n_cycles <- ceiling(n_rows/3)

# Data prep for both standard and extended plots
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
  select(cycle_id, time_point, `Left feeder`, `Right feeder`, Opened_feeder_val) %>%
  pivot_longer(cols = c(`Left feeder`, `Right feeder`), 
               names_to = "Feeder", 
               values_to = "Ant_Count") %>%
  filter(!is.na(Ant_Count))

df_long$time_point <- factor(df_long$time_point, levels = c("Opening", "Closing", "InBetween"))

# Additional data prep for extended plots
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


# --- 1. Standard Heatmap ---
p_heat <- ggplot(df_long, aes(x = cycle_id, y = time_point, fill = as.factor(Ant_Count))) +
  geom_tile(color="white") +
  facet_wrap(~ Feeder, ncol = 1) +
  scale_fill_viridis_d(name = "Ant Count", option = "magma") +
  theme_minimal() +
  labs(title = "Heatmap of Ant Counts over Cycles", x = "Cycle", y = "Time Point")
print(p_heat)

png("Plots/heatmap_plot.png", width = 10, height = 4, units = "in", res = 300)
print(p_heat)
dev.off()
cat("Standard Heatmap saved to heatmap_plot.png\n")


# --- 1.5. Standard Heatmap with Continuous Border ---
# We isolate the cycles where the feeder was open to draw the rectangles
df_opened <- df_long %>% filter(Feeder_State == "Opened") %>% distinct(cycle_id, Feeder)

# Version 1: With border for all the steps of an open cycle, with internal borders
p_heat_border <- ggplot(df_long, aes(x = cycle_id, y = time_point, fill = as.factor(Ant_Count))) +
  geom_tile(aes(color = Feeder_State), size = 0.5) +
  scale_color_manual(name = "Status", values = c("Opened" = "#A8E6CF", "Closed" = "transparent")) +
  facet_wrap(~ Feeder, ncol = 1) +
  scale_fill_viridis_d(name = "Ant Count", option = "magma") +
  theme_minimal() +
  labs(title = "Heatmap of Ant Counts over Cycles", x = "Cycle", y = "Time Point")

# Version 2: With border for all steps of an open cycle, without internal borders
p_heat_border <- ggplot(df_long, aes(x = cycle_id, y = time_point)) +
  geom_tile(aes(fill = as.factor(Ant_Count)), color = "transparent") +
  geom_rect(data = df_opened, aes(xmin = cycle_id - 0.5, xmax = cycle_id + 0.5, ymin = -Inf, ymax = Inf), 
            fill = NA, color = "#A8E6CF", size = 0.5, inherit.aes = FALSE) +
  facet_wrap(~ Feeder, ncol = 1) +
  scale_fill_viridis_d(name = "Ant Count", option = "magma") +
  theme_minimal() +
  labs(title = "Heatmap of Ant Counts over Cycles (Outlined = Opened)", x = "Cycle", y = "Time Point")

print(p_heat_border)

png("Plots/heatmap_plot_bordered.png", width = 10, height = 4, units = "in", res = 300)
print(p_heat_border)
dev.off()
cat("Standard Heatmap (Bordered) saved to heatmap_plot_bordered.png\n")



# --- 2. Faceted Heatmap (Faceted by State) ---
p_heat_faceted <- ggplot(df_long, aes(x = event_id, y = time_point, fill = as.factor(Ant_Count))) +
  geom_tile(color = "white") +
  facet_grid(Feeder ~ Feeder_State) +
  scale_fill_viridis_d(name = "Ant Count", option = "magma") +
  theme_minimal() +
  labs(title = "Heatmap of Ant Counts over Cycles (Faceted by State)", x = "Sequential Event Instance", y = "Time Point")
print(p_heat_faceted)

png("Plots/heatmap_faceted.png", width = 12, height = 6, units = "in", res = 300)
print(p_heat_faceted)
dev.off()
cat("Extended Heatmap saved to p_heat_faceted.png\n")

# Get counts of observations of each feeder+state combination
state_counts <- df_long %>%
  group_by(Feeder, Feeder_State) %>%
  summarize(count = n()) %>%
  ungroup()
print(state_counts)
