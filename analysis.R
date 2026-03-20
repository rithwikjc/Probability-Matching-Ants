library(tidyverse)
library(magrittr)
library(brms)

df_long %<>%
  mutate(
    Opened_feeder = factor(Opened_feeder_val, levels = c("Left", "Right")),
    Feeder_location = factor(Feeder, levels = c("Left feeder", "Right feeder")),
    Feeder_state = factor(Feeder_State, levels = c("Opened", "Closed")),
    Time_window = factor(time_point, levels = c("Opening", "Closing", "InBetween"))
  )

contrasts(df_long$Feeder_location) <- cbind(`RightVsLeft` = c(-1, 1))
contrasts(df_long$Feeder_state) <- cbind(`OpenVsClosed` = c(1, -1))
contrasts(df_long$Time_window) <- cbind(`OpeningVsNeutral` = c(1, 0, 0), `ClosingVsNeutral` = c(0, 1, 0))
df_long$Cycle <- (df_long$cycle_id - mean(df_long$cycle_id)) / (2 * sd(df_long$cycle_id))

# Model 1: ignoring cycle
bf <- bf(Ant_Count ~ 1 + Feeder_location * Feeder_state * Time_window, 
         family = poisson(link = "log"))
m <- brm(bf, data = df_long, chains = 4, cores = 4,
         file = "m-poisson", file_refit = "on_change")
summary(m)

conditional_effects(m)
conditions <- make_conditions(df_long, c("Feeder_location"))
ce <- conditional_effects(
  m, 
  effects = "Feeder_state:Time_window", 
  conditions = conditions,
  plot = F)

p <- plot(ce, ncol = 2, theme = theme_bw()) 
p[[1]] + labs(x = "Feeder", y = "Ant count", color = "Time window", fill = "Time window")


# Model 1: with cycle
bf <- bf(Ant_Count ~ 1 + Feeder_location * Feeder_state * Time_window + 
           s(Cycle, by = interaction(Feeder_location, Feeder_state, Time_window)), 
         family = poisson(link = "log"))
m <- brm(bf, data = df_long, chains = 4, cores = 4,
         file = "m-poisson-cycle")
summary(m)

conditional_smooths(m)

conditions <- make_conditions(df_long, c("Feeder_location", "Feeder_State"))
ce <- conditional_smooths(
  m, 
  effects = "s(cycle)", 
  conditions = conditions,
  plot = F)

p <- plot(ce, ncol = 2, theme = theme_bw()) 

# For time series data, we can use cyclic smooths (bs = "cc") if we code each cycle as a time series starting at 0 and ending at 30 (or sth like that)