library(dplyr)
library(ggplot2)
library(patchwork)
library(tidyr)
library(trend)


#### %LAND ####
# Function to process data 
process_heatwave_data <- function(df, total_coords, dataset_name, indicator) {
  df %>%
    group_by(time, year = lubridate::year(time)) %>%
    summarise(
      affected_coords = sum(.data[[indicator]] == 1, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(
      percentage = (affected_coords / total_coords) * 100
    ) %>%
    group_by(year) %>%
    summarise(
      mean_daily_percent = mean(percentage, na.rm = TRUE),
      dataset = dataset_name,
      indicator = indicator,
      .groups = 'drop'
    )
}

# Process each dataset and hwdefinition
# CPC
heatwave_cpc_ftx90 <- process_heatwave_data(merged_df_cpc, 54, "CPC", "heatwave_TX90")
heatwave_cpc_tn90 <- process_heatwave_data(merged_df_cpc, 54, "CPC", "heatwave_TN90")
heatwave_cpc_ehisig <- process_heatwave_data(merged_df_cpc, 54, "CPC", "heatwave_ehisig")

# ERA5
heatwave_era5_ftx90 <- process_heatwave_data(merged_df_era5, 1008, "ERA5", "heatwave_TX90")
heatwave_era5_tn90 <- process_heatwave_data(merged_df_era5, 1008, "ERA5", "heatwave_TN90")
heatwave_era5_ehisig <- process_heatwave_data(merged_df_era5, 1008, "ERA5", "heatwave_ehisig")

# Combine all 
heatwave_combined <- bind_rows(
  heatwave_cpc_ftx90,
  heatwave_cpc_tn90,
  heatwave_cpc_ehisig,
  heatwave_era5_ftx90,
  heatwave_era5_tn90,
  heatwave_era5_ehisig
)%>%
  mutate(
    indicator = recode(indicator, "heatwave_TX90" = "heatwave_FTX90"),
    dataset_indicator = paste(dataset, indicator, sep = "_")
  )

# Combined label 
heatwave_combined <- heatwave_combined %>%
  mutate(dataset_indicator = paste(dataset, indicator, sep = "_"))

# Plot
ggplot(heatwave_combined, aes(x = factor(year), y = mean_daily_percent, fill = dataset_indicator)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Average percentage of land area with a heatwave by Year",
    x = "Year",
    y = "Percentage (%)",
    fill = "Dataset_Indicator"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_manual(
    values = c(
      "CPC_heatwave_FTX90" = "#74add1",
      "CPC_heatwave_TN90" = "lightblue",
      "CPC_heatwave_ehisig" = "#313695",
      "ERA5_heatwave_FTX90" = "#74add1",
      "ERA5_heatwave_TN90" = "lightblue",
      "ERA5_heatwave_ehisig" = "#313695"
    )
  ) +
  facet_wrap(~ dataset, ncol = 1) 


#### Trendanalysis % land #####
compute_trend <- function(df) {
  mk <- mk.test(df$mean_daily_percent)
  ss <- sens.slope(df$mean_daily_percent)
  
  data.frame(
    p_value = mk$p.value,
    tau = mk$estimates[1],  # Kendall's tau
    sens_slope = ss$estimates,
    sens_conf_low = ss$conf.int[1],
    sens_conf_high = ss$conf.int[2]
  )
}

# Apply to each dataset-indicator combination
trend_results <- heatwave_combined %>%
  group_by(dataset_indicator) %>%
  arrange(year) %>%  # ensure years are ordered
  group_modify(~ compute_trend(.x)) %>%
  ungroup()
#### SEVERITY ####
#### calculate ehf_per85 
# CPC
merged_df_cpc_2 = merged_df_cpc
merged_df_cpc_2$ehf[is.na(merged_df_cpc_2$ehiacc)] <- NA
merged_df_cpc_2 <- merged_df_cpc_2 %>%
  group_by(coordinate_id) %>%
  mutate(ehf_per85 = quantile(ehf, probs = 0.85, na.rm = TRUE)) %>%
  ungroup()
merged_df_cpc_2$HWS <- merged_df_cpc_2$ehf / merged_df_cpc_2$ehf_per85
merged_df_cpc_2$
# ERA5
merged_df_era5_2 = merged_df_era5
merged_df_era5_2$ehf[is.na(merged_df_era5_2$ehiacc)] <- NA
merged_df_era5_2 <- merged_df_era5_2 %>%
  group_by(coordinate_id.x) %>%
  mutate(ehf_per85 = quantile(ehf, probs = 0.85, na.rm = TRUE)) %>%
  ungroup()
merged_df_era5_2$HWS <- merged_df_era5_2$ehf / merged_df_era5_2$ehf_per85
merged_df_era5_2$year = merged_df_era5_2$year.x
summary(merged_df_era5_2$HWS)


##### MAX HWS #####

# Voor CPC
max_hws_cpc <- merged_df_cpc_2 %>%
  group_by(year) %>%
  summarise(
    max_HWS = max(HWS, na.rm = TRUE),
    dataset = "CPC",
    .groups = "drop"
  )

# Voor ERA5
max_hws_era5 <- merged_df_era5_2 %>%
  group_by(year) %>%
  summarise(
    max_HWS = max(HWS, na.rm = TRUE),
    dataset = "ERA5",
    .groups = "drop"
  )

# Combineer beide
max_hws_combined <- bind_rows(max_hws_cpc, max_hws_era5)

# Plot voor CPC
plot_cpc <- ggplot(max_hws_cpc, aes(x = factor(year), y = max_HWS)) +
  geom_bar(stat = "identity", fill = "#74add1") +
  geom_hline(yintercept = 1, color = "#f46d43", linetype = "dashed", size = 1.2) +
  geom_hline(yintercept = 3, color = "#a50026", linetype = "dashed", size = 1.2) +
  labs(
    title = "CPC",
    x = "Year",
    y = "Maximum HWS"
  ) +
  theme_minimal() +
  ylim(0, 12) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Plot voor ERA5
plot_era5 <- ggplot(max_hws_era5, aes(x = factor(year), y = max_HWS)) +
  geom_bar(stat = "identity", fill = "#313695") +
  geom_hline(yintercept = 1, color = "#f46d43", linetype = "dashed", size = 1.2) +
  geom_hline(yintercept = 3, color = "#a50026", linetype = "dashed", size = 1.2) +
  labs(
    title = "ERA5",
    x = "Year",
    y = "Maximum HWS"
  ) +
  theme_minimal() +
  ylim(0, 12) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Combineer onder elkaar
combined_plot <- plot_cpc / plot_era5
combined_plot



##### MEAN #####
mean_hws_cpc <- merged_df_cpc_2 %>%
  group_by(year) %>%
  summarise(
    mean_HWS = mean(HWS, na.rm = TRUE),
    dataset = "CPC",
    .groups = "drop"
  )
merged_df_cpc_2$HWS

# --- Process ERA5 ---
mean_hws_era5 <- merged_df_era5_2 %>%
  group_by(year) %>%
  summarise(
    mean_HWS = mean(HWS, na.rm = TRUE),
    dataset = "ERA5",
    .groups = "drop"
  )

# Plot for Mean HWS - CPC
plot_mean_hws_cpc <- ggplot(mean_hws_cpc, aes(x = factor(year), y = mean_HWS, fill = dataset)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = 1, color = "#f46d43", linetype = "dashed", size = 1.2) +
  labs(
    title = "CPC",
    x = "Year",
    y = "Mean HWS"
  ) +
  theme_minimal() +
  ylim(0, 1.5) +  # Set y-axis limit for Mean HWS
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_fill_manual(values = c("CPC" = "#74add1"))

# Plot for Mean HWS - ERA5
plot_mean_hws_era5 <- ggplot(mean_hws_era5, aes(x = factor(year), y = mean_HWS, fill = dataset)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = 1, color = "#f46d43", linetype = "dashed", size = 1.2) +
  labs(
    title = "ERA5",
    x = "Year",
    y = "Mean HWS"
  ) +
  theme_minimal() +
  ylim(0, 1.5) +  # Set y-axis limit for Mean HWS
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_fill_manual(values = c("ERA5" = "#313695"))

##### COMBINE BOTH PLOTS INTO TWO PANELS #####

# Combine both plots into separate panels for CPC and ERA5
combined_plot <- plot_mean_hws_cpc / plot_mean_hws_era5

# Display combined plot
combined_plot

# HWS %land CPC & ERA5
cpc_HWS_summary <- merged_df_cpc_2 %>%
  filter(!is.na(HWS)) %>%
  group_by(year, coordinate_id) %>%
  summarise(max_HWS = max(HWS), .groups = "drop") %>%
  summarise(
    perc_coords_HWS_gt1 = sum(max_HWS > 1) / 54 * 100,
    perc_coords_HWS_gt3 = sum(max_HWS > 3) / 54 * 100,
    .by = year
  ) %>%
  mutate(source = "CPC")

era5_HWS_summary <- merged_df_era5_2 %>%
  filter(!is.na(HWS)) %>%
  group_by(year, coordinate_id.x) %>%
  summarise(max_HWS = max(HWS), .groups = "drop") %>%
  summarise(
    perc_coords_HWS_gt1 = sum(max_HWS > 1) / 1008 * 100,
    perc_coords_HWS_gt3 = sum(max_HWS > 3) / 1008 * 100,
    .by = year
  ) %>%
  mutate(source = "ERA5")

# Combine data
combined_HWS_summary <- bind_rows(cpc_HWS_summary, era5_HWS_summary)

plot_data <- combined_HWS_summary %>%
  pivot_longer(cols = starts_with("perc_coords_HWS"),
               names_to = "threshold",
               values_to = "percentage") %>%
  mutate(threshold = recode(threshold,
                            "perc_coords_HWS_gt1" = "HWS > 1",
                            "perc_coords_HWS_gt3" = "HWS > 3"))

# Plot
ggplot(plot_data, aes(x = year, y = percentage, color = source, linetype = threshold)) +
  geom_line(size = 1) +
  labs(title = "Percentage of Coordinates with HWS > 1 and > 3",
       y = "Percentage",
       x = "Year",
       color = "Dataset",
       linetype = "Threshold") +
  theme_minimal()

# Filter the data to keep only HWS > 3 threshold
plot_data_HWS_gt3 <- plot_data %>%
  filter(threshold == "HWS > 3")

# Plot
ggplot(plot_data_HWS_gt3, aes(x = year, y = percentage, color = source, linetype = threshold)) +
  geom_line(size = 1) +
  labs(
    title = "Percentage of land with an extreme heatwave",
    y = "Percentage Land (%)",
    x = "Year",
    color = "Dataset"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1980, 2024, by = 2)) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_color_manual(values = c("CPC" = "#74add1", "ERA5" = "#313695"))