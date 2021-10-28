library(tidyverse)
library(lubridate)
library(mgcv)
library(patchwork)

load(here::here("data", "PRIN_5min_flagged.rda"))
load(here::here("data", "PRIN_5min_cleaned.rda"))

PRIN_5min_flagged <-  PRIN_5min_flagged %>% 
  mutate(Timestamp = ymd_hms(roundedTimestamp)) %>% 
  filter(Timestamp >= ymd("2019-10-01") &
           Timestamp < ymd("2020-01-01"))

PRIN_5min_cleaned <-  PRIN_5min_cleaned %>% 
  mutate(Timestamp = ymd_hms(roundedTimestamp)) %>% 
  filter(Timestamp >= ymd("2019-10-01") &
           Timestamp < ymd("2020-01-01")) %>% 
  rename("level" = surfacewaterElevMean,
         "temperature" = surfWaterTempMean,
         "conductance" = specificConductance,
         "dissolved_oxygen" = dissolvedOxygen)

PRIN_data <- PRIN_5min_flagged %>% 
  select(Timestamp, site, surfacewaterElevMean, surfWaterTempMean, 
         specificConductance, turbidity, dissolvedOxygen) %>% 
  rename("level" = surfacewaterElevMean,
         "temperature" = surfWaterTempMean,
         "conductance" = specificConductance,
         "dissolved_oxygen" = dissolvedOxygen)


p_turb <- PRIN_data %>% 
  select(Timestamp, turbidity, site) %>% 
  ggplot(aes(Timestamp, turbidity, color = site)) +
  geom_line() +
  scale_color_manual(values = c("#999999", "#E69F00"),
                     breaks = c("down", "up")) +
  ylab("Turbidity (FNU)")

p_cond <- PRIN_data %>% 
  select(Timestamp, conductance, site) %>% 
  ggplot(aes(Timestamp, conductance, color = site)) +
  geom_line() +
  scale_color_manual(values = c("#999999", "#E69F00"),
                     breaks = c("down", "up")) +
  ylab(expression(Conductance~(mu*S*'/'*cm)))

p_do <- PRIN_data %>% 
  select(Timestamp, dissolved_oxygen, site) %>% 
  ggplot(aes(Timestamp, dissolved_oxygen, color = site)) +
  geom_line() +
  scale_color_manual(values = c("#999999", "#E69F00"),
                     breaks = c("down", "up")) +
  ylab("DO (mg/l)")

p_level <- PRIN_data %>% 
  select(Timestamp, level, site) %>% 
  ggplot(aes(Timestamp, level, color = site)) +
  geom_line() +
  scale_color_manual(values = c("#999999", "#E69F00"),
                     breaks = c("down", "up")) +
  ylab("Level (m)")

p_temp <- PRIN_data %>% 
  select(Timestamp, temperature, site) %>% 
  ggplot(aes(Timestamp, temperature, color = site)) +
  geom_line() +
  scale_color_manual(values = c("#999999", "#E69F00"),
                     breaks = c("down", "up")) +
  ylab(expression('Temperature ('~degree*C*')')) +
  theme(legend.position = "bottom")

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(p_temp)

library(gridExtra)
grid.arrange(arrangeGrob(p_cond + theme(legend.position = "none"),
                         p_do + theme(legend.position = "none"), 
                         p_level + theme(legend.position = "none"),
                         p_temp + theme(legend.position = "none"),
                         p_turb + theme(legend.position = "none"),
                         ncol = 2), 
             mylegend, heights=c(10, 1))










###

Time_turb_down_anomaly <- PRIN_5min_flagged %>% 
  filter(site == "down",
         turbidityAnomalyFlag == 1) %>% 
  pull(Timestamp)

plot_turb_down <- PRIN_data %>%
  filter(site == "down") %>% 
  select(Timestamp, turbidity) %>% 
  ggplot(aes(Timestamp, turbidity)) +
  geom_point() +
  geom_line() +
  geom_point(data = PRIN_data %>% filter(site == "down",
                                         Timestamp %in% Time_turb_down_anomaly),
             aes(Timestamp, turbidity), color = "red", size = 1) +
  ylab("Turbidity (FNU)") +
  ggtitle("downstream")


plot_turb_up <- PRIN_5min_cleaned %>%
  filter(site == "up") %>% 
  select(Timestamp, turbidity) %>% 
  ggplot(aes(Timestamp, turbidity)) +
  geom_point() +
  geom_line() +
  ylab("Turbidity (FNU)")+
  ggtitle("upstream")


plot_level <- PRIN_5min_cleaned %>%
  filter(site == "up") %>% 
  select(Timestamp, level) %>% 
  ggplot(aes(Timestamp, level)) +
  geom_point() +
  geom_line() +
  ylab("Level (m)") +
  ggtitle("upstream")

  
plot_cond <- PRIN_5min_cleaned %>%
  filter(site == "up") %>% 
  select(Timestamp, conductance) %>% 
  ggplot(aes(Timestamp, conductance)) +
  geom_point() +
  geom_line() +
  ylab(expression(Conductance~(mu*S*'/'*cm))) +
  ggtitle("upstream")

plot_temp <- PRIN_5min_cleaned %>%
  filter(site == "up") %>% 
  select(Timestamp, temperature) %>% 
  ggplot(aes(Timestamp, temperature)) +
  geom_point() +
  geom_line() +
  ylab(expression('Temperature ('~degree*C*')'))+
  ggtitle("upstream")


grid.arrange(plot_turb_down + theme(axis.title.x = element_blank()), 
             plot_turb_up + theme(axis.title.x = element_blank()), 
             plot_cond + theme(axis.title.x = element_blank()),
             plot_level + theme(axis.title.x = element_blank()), 
             plot_temp, 
             ncol = 1)







###

load("~/PostDoc/Postdoc_meetings/Postdoc cross-node meeting/ARCLP-DES-Catchup/data/final_data_modeling.rda")
data_mod_turb <- final_data_modeling

# data_mod_turb <- data_mod_turb %>% 
#   select(-turbidity_downstream_log, -turbidity_upstream_dt_log)

Time_turb_down_anomaly <- data_mod_turb %>% 
  filter(turbidityAnomalyFlag == 1) %>% 
  pull(Timestamp)

data_mod_turb %>% 
  ggplot(aes(Timestamp, turbidity_downstream)) +
  geom_point(size = 0.05) +
  geom_point(data = data_mod_turb %>% filter(Timestamp %in% Time_turb_down_anomaly),
             aes(Timestamp, turbidity_downstream), color = "red", size = 1)

data_mod_turb %>% 
  select(-turbidityAnomalyFlag, -max_lag, -dissolved_oxygen_upstream_dt) %>% 
  rename("turbidity_upstream" = turbidity_upstream_dt,
         "conductance_upstream" = conductance_upstream_dt,
         "level_upstream" = level_upstream_dt,
         "temperature_upstream" = temperature_upstream_dt) %>% 
  gather(-Timestamp, key = "Variable", value = "value") %>% 
  mutate(Pattern = case_when(Timestamp %in% Time_turb_down_anomaly 
                             & Variable == "turbidity_downstream" 
                             ~ "Turbidity anomaly",
                             Timestamp >= ymd("2019-10-24") 
                             & Timestamp < ymd_hms("2019-10-25 12:00:00") |
                               Timestamp >= ymd("2019-11-07")
                             & Timestamp < ymd_hms("2019-11-09 00:00:00") 
                             ~ "water_qaulity_pattern",
                             TRUE ~ "Typical data"),
         Pattern = factor(Pattern, levels = c("Typical data",
                                              "water_qaulity_pattern",
                                              "Turbidity anomaly")),
         Variable = factor(Variable, levels = c("turbidity_downstream",
                                                "turbidity_upstream",
                                                "conductance_upstream",
                                                "level_upstream",
                                                "temperature_upstream"))) %>% 
  ggplot(aes(Timestamp, value, color = Pattern, size = Pattern)) +
  geom_point() +
  facet_wrap(~Variable, scales = "free_y", ncol = 1) +
  scale_color_manual(values = c("#999999", "#009E73", "#FF3333")) + 
  scale_size_manual(values = c(0.05, 1, 1)) +
  theme(legend.position = "none")


###-- Modeling --###

data_mod_turb <- data_mod_turb %>% 
  mutate(turbidity_downstream_log =
           log(turbidity_downstream),
         turbidity_upstream_dt_log =
           log(turbidity_upstream_dt))

# Computing lagged responses

turb_lag <- data_mod_turb %>%
  pull(turbidity_downstream_log)
turb_lag_df <- purrr::map_dfc(1:9, lag, x = turb_lag) 
names(turb_lag_df) <- paste("turbidity_downstream_log_lag", 1:9, sep = "")

data_mod_turb <- data_mod_turb %>% 
  bind_cols(turb_lag_df)

turb_gamar_fit <- gam(turbidity_downstream_log ~
                        s(turbidity_upstream_dt_log) +
                        s(turbidity_downstream_log_lag1) +
                        s(turbidity_downstream_log_lag2) +
                        s(turbidity_downstream_log_lag3) + 
                        s(turbidity_downstream_log_lag4) +
                        s(turbidity_downstream_log_lag5) + 
                        s(level_upstream_dt) +
                        s(conductance_upstream_dt) +
                        s(temperature_upstream_dt),
                      data = data_mod_turb)

forecast::ggtsdisplay(turb_gamar_fit$residuals, 
            plot.type = "histogram")


###- POT for outlier threshold

data_mod_turb_complete <- data_mod_turb %>%
  select(Timestamp, turbidity_downstream_log,
         turbidity_downstream_log_lag1,
         turbidity_downstream_log_lag2,
         turbidity_downstream_log_lag3,
         turbidity_downstream_log_lag4,
         turbidity_downstream_log_lag5,
         turbidity_upstream_dt_log,
         level_upstream_dt,
         conductance_upstream_dt,
         temperature_upstream_dt,
         turbidityAnomalyFlag) %>%
  drop_na()

data_mod_turb_complete <- data_mod_turb_complete %>%
  mutate(resid_gam = resid(turb_gamar_fit))

Time_df <- data_mod_turb %>%
  select(Timestamp)

resid_df <- Time_df %>%
  left_join(data_mod_turb_complete, by = "Timestamp")

resid_gam <- resid_df$resid_gam


find_threshold_POT <- function(X, threshold = NULL, level = 0.001){
  
  X <- na.omit(X) %>% abs() %>% as.numeric()
  
  # calculate threshold
  t1 <- threshold
  if(is.null(t1)){
    t1 <- quantile(X, probs = min(0.90, (1-level)))
  }
  
  n <- length(X)
  Nt <- sum(X>t1)
  
  # estimate scale and shape parameters of GPD using POT
  fit_gpd <- evd::fpot(X, threshold = t1)
  
  # compute outlier threshold
  scale_gpd <- fit_gpd$estimate[1]
  shape_gpd <- fit_gpd$estimate[2]
  thresh <- t1 + (scale_gpd/shape_gpd)*((level*n/Nt)^(-shape_gpd) - 1)
  
  k <- sum(X>thresh)
  
  return(list(k = k, outlier_threshold = thresh, 
              initial_threshold = t1))
  
}

Find_threshold_POT <- find_threshold_POT(X = abs(resid_gam),
                                         threshold = NULL,
                                         level = 0.001)

threshold_POT <- Find_threshold_POT$outlier_threshold

count_outliers_POT <- Find_threshold_POT$k


# confussion matrix

resid_df <- resid_df %>% 
  mutate(resid_POT_anomaly = if_else(resid_gam >
                                       threshold_POT 
                                     | resid_gam <
                                       -threshold_POT,
                                     1, 0),
         resid_POT_anomaly =
           factor(resid_POT_anomaly, 
                  levels = c(0,1)),
         conf_matrix_POT = case_when(turbidityAnomalyFlag
                                     == 0 
                                     & resid_POT_anomaly ==
                                       1 ~ "FP",
                                     turbidityAnomalyFlag
                                     == 1 
                                     & resid_POT_anomaly ==
                                       0 ~ "FN",
                                     turbidityAnomalyFlag
                                     == 1 
                                     & resid_POT_anomaly ==
                                       1 ~ "TP",
                                     turbidityAnomalyFlag
                                     == 0 
                                     & resid_POT_anomaly ==
                                       0 ~ "TN",
                                     TRUE ~
                                       as.character(NA)),
         confusion_matrix_POT = factor(conf_matrix_POT, 
                                  levels = c("TP", "TN", "FP", "FN",
                                             "NA"))
  )



plot_data <- resid_df %>% 
  drop_na(turbidity_downstream_log) %>% 
  mutate(turbidity_downstream = exp(turbidity_downstream_log)) %>% 
  dplyr::select(Timestamp, confusion_matrix_POT, 
                turbidity_downstream, 
                resid_gam) %>% 
  gather(-Timestamp, -confusion_matrix_POT, 
         key = variable, value = value) %>% 
  mutate(variable = factor(variable, 
                           levels =
                             c("resid_gam",
                               "turbidity_downstream"))) 


plot_data %>% 
  ggplot() +
  geom_point(aes(Timestamp, value, color = confusion_matrix_POT,
                 shape = confusion_matrix_POT, 
                 size = confusion_matrix_POT)) +
  facet_wrap(~variable, scales = "free_y", ncol = 1) +
  # theme_minimal() +
  scale_color_manual(values = c("#009E73", "#999999", "#E69F00",
                                "#FF3333")) +
  scale_shape_manual(values = c(17,16,18,15)) +
  scale_size_manual(values = c(2, 0.3, 2, 2)) +
  # theme(strip.text = element_text(size = 16)) +
  ggtitle("Outlier detection using Peak over threshold")


# confusion matrix

confussion_matrix <- resid_df %>% 
  drop_na(turbidity_downstream_log) %>% 
  dplyr::select(Timestamp, confusion_matrix_POT) %>% 
  gather(-Timestamp, key = method, value = value) %>% 
  group_by(method) %>% 
  count(value) %>% 
  spread(key = value, value = n) %>% 
  mutate(method = recode(method, 
                         confusion_matrix_POT = "POT-method"))

confussion_matrix <- confussion_matrix %>% 
  mutate(FN = 0)

confusion_matrix <- confussion_matrix %>% 
  replace_na(list(FN = 0, FP = 0, TN = 0, TP = 0)) %>% 
  mutate(Accuracy = (TP + TN)/(TP + TN + FP + FN),
         Error_rate = (FP + FN)/(TP + TN + FP + FN))

save(confusion_matrix, file = "~/PostDoc/Postdoc_meetings/Postdoc cross-node meeting/ARCLP-DES-Catchup/plots/confusion_matrix.rda")



