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
  select(cycle_id, time_point, `Left feeder`, `Right feeder`, Opened_feeder_val) %>%
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


# --- 1. Standard Violin Plot ---
pos <- position_jitter(seed = 42, width = 0.15, height = 0)

p_violin <- ggplot(df_long, aes(x = time_point, y = Ant_Count, fill = time_point)) +
  geom_violin(alpha = 0.5, trim = TRUE, color = NA) +
  geom_line(aes(group = cycle_id), alpha = 0.1, color = "black", position = pos) + 
  geom_point(aes(group = cycle_id), alpha = 0.3, size = 1, color = "black", position = pos) +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 3, color = "black", fill = "white", stroke = 1.5) +
  facet_wrap(~ Feeder, ncol = 2) +
  scale_fill_manual(values = c("Opening" = "#F2CB7C", "Closing" = "#9BD5F3", "InBetween" = "#A8E6CF")) +
  theme_minimal() +
  labs(title = "Violin Plots of Ant Counts across Time Points",
       x = "Time Point", y = "Ant Count", fill = "Time Point") +
  theme(legend.position = "none",
        strip.text = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 10))
print(p_violin)

png("Plots/violin_plot.png", width = 10, height = 6, units = "in", res = 300)
print(p_violin)
dev.off()
cat("Standard Box plot saved to violin_plot.png\n")


# --- 2. Faceted Violin Plot (2x2 facet) ---
p_violin_faceted <- ggplot(df_long, aes(x = time_point, y = Ant_Count, fill = time_point)) +
  geom_violin(alpha = 0.5, trim = TRUE, color = NA) +
  geom_line(aes(group = cycle_id), alpha = 0.1, color = "black", position = pos) + 
  geom_point(aes(group = cycle_id), alpha = 0.3, size = 1, color = "black", position = pos) +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 3, color = "black", fill = "white", stroke = 1.5) +
  facet_grid(Feeder ~ Feeder_State) +
  scale_fill_manual(values = c("Opening" = "#F2CB7C", "Closing" = "#9BD5F3", "InBetween" = "#A8E6CF")) +
  theme_minimal() +
  labs(title = "Violin Plots of Ant Counts across Time Points (Faceted by State)",
       x = "Time Point", y = "Ant Count", fill = "Time Point") +
  theme(legend.position = "none",
        strip.text = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 10))
print(p_violin_faceted)

png("Plots/violin_plot_faceted.png", width = 12, height = 8, units = "in", res = 300)
print(p_violin_faceted)
dev.off()
cat("Extended Box plot saved to p_violin_faceted.png\n")


# --- 3. Side-by-Side Violin Plot (Left vs Right Feeder colored) ---
# Set a shared jitter position that dodges by Feeder
pos_dodge <- position_jitterdodge(jitter.width = 0.15, jitter.height = 0, dodge.width = 0.75, seed = 42)

p_violin_sbs <- ggplot(df_long, aes(x = time_point, y = Ant_Count, fill = Feeder, color = Feeder)) +
  geom_violin(position = position_dodge(width = 0.75), alpha = 0.5, trim = TRUE, scale = "width") +
  geom_line(aes(group = interaction(cycle_id, Feeder)), alpha = 0.1, position = pos_dodge) +
  geom_point(aes(group = interaction(cycle_id, Feeder)), alpha = 0.3, size = 1, position = pos_dodge) +
  stat_summary(aes(group = Feeder), fun = mean, geom = "point", shape = 21, size = 3, color = "black", fill = "white", stroke = 1.5, position = position_dodge(width = 0.75)) +
  facet_wrap(~ Feeder_State, ncol = 2) +
  scale_fill_manual(values = c("Left feeder" = "#9BD5F3", "Right feeder" = "#F28C8C")) +
  scale_color_manual(values = c("Left feeder" = "#4A90E2", "Right feeder" = "#D0021B")) +
  theme_minimal() +
  labs(title = "Violin Plots: Left vs Right Feeder (by State)",
       x = "Time Point", y = "Ant Count", fill = "Feeder", color = "Feeder") +
  theme(strip.text = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 10))
print(p_violin_sbs)

png("Plots/violin_plot_side_by_side.png", width = 12, height = 6, units = "in", res = 300)
print(p_violin_sbs)
dev.off()
cat("Side-by-side Violin plot saved to violin_plot_side_by_side.png\n")
