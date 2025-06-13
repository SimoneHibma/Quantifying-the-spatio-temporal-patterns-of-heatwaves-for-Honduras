library(Kendall)
library(trend)    
library(dplyr)
library(ggplot2)
library(writexl)
######## TREND ANALYSIS ######## 
merged_df_cpc$year.x = merged_df_cpc$year
###### HW days #####
#### Plots over time ####
# Filter to include coordinates 1 to 57
filtered_df <- subset(cpc_heatwave_days_trend_df, coordinate_id.x %in% 1:54)

# Create faceted EHF plot
ggplot(filtered_df, aes(x = year.x, y = heatwave_ehisig_days)) +
  geom_line(color = "steelblue") +
  facet_wrap(~ coordinate_id.x, scales = "free_y", ncol = 6) +
  labs(title = "Heatwave EHF Days Over Time (CPC Dataset)",
       x = "Year",
       y = "TX90 Days") +
  theme_minimal(base_size = 10) +
  theme(
    strip.text = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Create faceted TX90 plot
ggplot(filtered_df, aes(x = year.x, y = heatwave_TX90_days)) +
  geom_line(color = "steelblue") +
  facet_wrap(~ coordinate_id.x, scales = "free_y", ncol = 6) +
  labs(title = "Heatwave TX90 Days Over Time (CPC Dataset)",
       x = "Year",
       y = "TX90 Days") +
  theme_minimal(base_size = 10) +
  theme(
    strip.text = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# Create faceted TN90 plot
ggplot(filtered_df, aes(x = year.x, y = heatwave_TN90_days)) +
  geom_line(color = "steelblue") +
  facet_wrap(~ coordinate_id.x, scales = "free_y", ncol = 6) +
  labs(title = "Heatwave TN90 Days Over Time (CPC Dataset)",
       x = "Year",
       y = "TX90 Days") +
  theme_minimal(base_size = 10) +
  theme(
    strip.text = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#### ERA5 ####
# Calculate heatwave days for each heatwave definition
era5_heatwave_days_trend_df <- merged_df_era5 %>%
  group_by(coordinate_id.x, lon, lat, year.x) %>%
  summarise(
    heatwave_ehisig_days = sum(heatwave_ehisig, na.rm = TRUE),
    heatwave_TX90_days = sum(heatwave_TX90, na.rm = TRUE),
    heatwave_TN90_days = sum(heatwave_TN90, na.rm = TRUE),
    .groups = "drop"
  )

# Initialize an empty data frame to store results
era5_trend_hw_days <- data.frame(
  coordinate_id = integer(),
  tau_ehisig = numeric(), p_ehisig = numeric(), sens_slope_ehisig = numeric(),
  tau_TX90 = numeric(), p_TX90 = numeric(), sens_slope_TX90 = numeric(),
  tau_TN90 = numeric(), p_TN90 = numeric(), sens_slope_TN90 = numeric()
)

# Loop through each coordinate_id from 1 to 57
for (coord_id in 1:1008) {
  
  # Extract test series for each heatwave metric
  test_series_ehisig <- era5_heatwave_days_trend_df %>%
    filter(coordinate_id.x == as.character(coord_id)) %>%
    pull(heatwave_ehisig_days)
  
  test_series_TX90 <- era5_heatwave_days_trend_df %>%
    filter(coordinate_id.x == as.character(coord_id)) %>%
    pull(heatwave_TX90_days)
  
  test_series_TN90 <- era5_heatwave_days_trend_df %>%
    filter(coordinate_id.x == as.character(coord_id)) %>%
    pull(heatwave_TN90_days)
  
  # Skip if all test series are empty
  if (length(test_series_ehisig) == 0 & length(test_series_TX90) == 0 & length(test_series_TN90) == 0) next
  
  # Perform Mann-Kendall test & Sen's slope estimation if data exists
  mk_result_ehisig <- if(length(test_series_ehisig) > 0) MannKendall(test_series_ehisig) else list(tau=NA, sl=NA)
  sens_result_ehisig <- if(length(test_series_ehisig) > 0) sens.slope(test_series_ehisig) else list(estimates=NA)
  
  mk_result_TX90 <- if(length(test_series_TX90) > 0) MannKendall(test_series_TX90) else list(tau=NA, sl=NA)
  sens_result_TX90 <- if(length(test_series_TX90) > 0) sens.slope(test_series_TX90) else list(estimates=NA)
  
  mk_result_TN90 <- if(length(test_series_TN90) > 0) MannKendall(test_series_TN90) else list(tau=NA, sl=NA)
  sens_result_TN90 <- if(length(test_series_TN90) > 0) sens.slope(test_series_TN90) else list(estimates=NA)
  
  # Store results in the dataframe
  era5_trend_hw_days <- rbind(era5_trend_hw_days, data.frame(
    coordinate_id = coord_id,
    tau_ehisig = mk_result_ehisig$tau, p_ehisig = mk_result_ehisig$sl[1], sens_slope_ehisig = sens_result_ehisig$estimates,
    tau_TX90 = mk_result_TX90$tau, p_TX90 = mk_result_TX90$sl[1], sens_slope_TX90 = sens_result_TX90$estimates,
    tau_TN90 = mk_result_TN90$tau, p_TN90 = mk_result_TN90$sl[1], sens_slope_TN90 = sens_result_TN90$estimates
  ))
}

# Create a lookup table for lon and lat
coord_lookup <- era5_heatwave_days_trend_df %>%
  mutate(coordinate_id = as.integer(coordinate_id.x)) %>%
  select(coordinate_id, lon, lat) %>%
  distinct()

# Merge lon and lat into trend results
era5_trend_hw_days <- era5_trend_hw_days %>%
  left_join(coord_lookup, by = "coordinate_id")

# Display the summary of the results
summary(era5_trend_hw_days)

#### CPC ####
cpc_heatwave_days_trend_df <- merged_df_cpc %>%
  group_by(coordinate_id.x, lon, lat, year.x) %>%
  summarise(
    heatwave_ehisig_days = sum(heatwave_ehisig, na.rm = TRUE),
    heatwave_TX90_days = sum(heatwave_TX90, na.rm = TRUE),
    heatwave_TN90_days = sum(heatwave_TN90, na.rm = TRUE),
    .groups = "drop"
  )
cpc_heatwave_days_trend_df$lon <- as.numeric(cpc_heatwave_days_trend_df$lon)
cpc_heatwave_days_trend_df$lat <- as.numeric(cpc_heatwave_days_trend_df$lat)

# Initialize an empty data frame to store results
cpc_trend_hw_days <- data.frame(
  coordinate_id = integer(),
  tau_ehisig = numeric(), p_ehisig = numeric(), sens_slope_ehisig = numeric(),
  tau_TX90 = numeric(), p_TX90 = numeric(), sens_slope_TX90 = numeric(),
  tau_TN90 = numeric(), p_TN90 = numeric(), sens_slope_TN90 = numeric()
)

# Loop through each coordinate_id from 1 to 57
for (coord_id in 1:57) {
  
  # Extract test series for each heatwave metric
  test_series_ehisig <- cpc_heatwave_days_trend_df %>%
    filter(coordinate_id.x == as.character(coord_id)) %>%
    pull(heatwave_ehisig_days)
  
  test_series_TX90 <- cpc_heatwave_days_trend_df %>%
    filter(coordinate_id.x == as.character(coord_id)) %>%
    pull(heatwave_TX90_days)
  
  test_series_TN90 <- cpc_heatwave_days_trend_df %>%
    filter(coordinate_id.x == as.character(coord_id)) %>%
    pull(heatwave_TN90_days)
  
  # Skip if all test series are empty
  if (length(test_series_ehisig) == 0 & length(test_series_TX90) == 0 & length(test_series_TN90) == 0) next
  
  # Perform Mann-Kendall test & Sen's slope estimation if data exists
  mk_result_ehisig <- if(length(test_series_ehisig) > 0) MannKendall(test_series_ehisig) else list(tau=NA, sl=NA)
  sens_result_ehisig <- if(length(test_series_ehisig) > 0) sens.slope(test_series_ehisig) else list(estimates=NA)
  
  mk_result_TX90 <- if(length(test_series_TX90) > 0) MannKendall(test_series_TX90) else list(tau=NA, sl=NA)
  sens_result_TX90 <- if(length(test_series_TX90) > 0) sens.slope(test_series_TX90) else list(estimates=NA)
  
  mk_result_TN90 <- if(length(test_series_TN90) > 0) MannKendall(test_series_TN90) else list(tau=NA, sl=NA)
  sens_result_TN90 <- if(length(test_series_TN90) > 0) sens.slope(test_series_TN90) else list(estimates=NA)
  
  # Store results in the dataframe
  cpc_trend_hw_days <- rbind(cpc_trend_hw_days, data.frame(
    coordinate_id = coord_id,
    tau_ehisig = mk_result_ehisig$tau, p_ehisig = mk_result_ehisig$sl[1], sens_slope_ehisig = sens_result_ehisig$estimates,
    tau_TX90 = mk_result_TX90$tau, p_TX90 = mk_result_TX90$sl[1], sens_slope_TX90 = sens_result_TX90$estimates,
    tau_TN90 = mk_result_TN90$tau, p_TN90 = mk_result_TN90$sl[1], sens_slope_TN90 = sens_result_TN90$estimates
  ))
}

# Create a lookup table for lon and lat
coord_lookup <- cpc_heatwave_days_trend_df %>%
  mutate(coordinate_id = as.integer(coordinate_id.x)) %>%
  select(coordinate_id, lon, lat) %>%
  distinct()

# Merge lon and lat into trend results
cpc_trend_hw_days <- cpc_trend_hw_days %>%
  left_join(coord_lookup, by = "coordinate_id")

# Display summary of the results
summary(cpc_trend_hw_days)

#### Export ####
write_xlsx(era5_trend_hw_days, "HONDURAS HEAT/ERA5-Land/ERA5_trend_hw_days.xlsx")
write_xlsx(cpc_trend_hw_days, "HONDURAS HEAT/CPC_data/CPC_trend_hw_days.xlsx")
###### HW events ##### 
#### CPC data ####
merged_df_cpc <- merged_df_cpc %>%
  mutate(time = as.Date(time)) %>%
  mutate(heatwave_TX90 = as.numeric(heatwave_TX90)) %>%
  mutate(heatwave_TN90 = as.numeric(heatwave_TN90)) %>%
  arrange(coordinate_id.x, time)

# Function to compute heatwave metrics per coordinate for each heatwave definition
calculate_hw_metrics <- function(df) {
  df <- df %>% arrange(time) 
  
  # Function to count heatwave events for a given heatwave column
  count_events <- function(heatwave_column) {
    if (all(is.na(heatwave_column))) return(0)  # Handle cases with all NAs
    heatwave_column[is.na(heatwave_column)] <- 0  # Replace NA with 0
    rle_values <- rle(heatwave_column == 1)  # Identify consecutive heatwave days
    sum(rle_values$values & rle_values$lengths > 0)  # Count separate events
  }
  
  # Compute heatwave event count for each definition
  hw_events_ehisig <- count_events(df$heatwave_ehisig)
  hw_events_TX90 <- count_events(df$heatwave_TX90)
  hw_events_TN90 <- count_events(df$heatwave_TN90)
  
  return(data.frame(
    coordinate_id = unique(df$coordinate_id.x),
    Events_EHF = hw_events_ehisig,
    Events_TX90 = hw_events_TX90,
    Events_TN90 = hw_events_TN90  ))
}

# Apply function per coordinate using `do()`
df_heatwave_events_trend_cpc <- merged_df_cpc %>%
  group_by(coordinate_id.x, lon, lat, year.x) %>%
  do(calculate_hw_metrics(.)) %>%
  ungroup()

#### ERA5 data ####
merged_df_era5 <- merged_df_era5 %>%
  mutate(time = as.Date(time)) %>%
  arrange(coordinate_id.x, time)

# Function to compute heatwave metrics per coordinate for each heatwave definition
calculate_hw_metrics <- function(df) {
  df <- df %>% arrange(time) 
  
  # Function to count heatwave events for a given heatwave column
  count_events <- function(heatwave_column) {
    if (all(is.na(heatwave_column))) return(0)  # Handle cases with all NAs
    rle_values <- rle(heatwave_column == 1)  # Identify consecutive heatwave days
    sum(rle_values$values & rle_values$lengths > 0)  # Count separate events
  }
  
  # Compute heatwave event count for each definition
  hw_events_ehisig <- count_events(df$heatwave_ehisig)
  hw_events_TX90 <- count_events(df$heatwave_TX90)
  hw_events_TN90 <- count_events(df$heatwave_TN90)
  
  return(data.frame(
    coordinate_id = unique(df$coordinate_id.x), 
    Events_EHF = hw_events_ehisig,
    Events_TX90 = hw_events_TX90,
    Events_TN90 = hw_events_TN90
  ))
}

# Apply function per coordinate using `do()`
df_heatwave_events_trend_era5 <- merged_df_era5 %>%
  group_by(coordinate_id.x, lon, lat, year.x) %>%
  do(calculate_hw_metrics(.)) %>%
  ungroup()

# View
summary(df_heatwave_events_trend_era5)


#### CPC trend ####
# Trend analysis on event counts
cpc_trend_hw_events <- data.frame(
  coordinate_id = integer(),
  tau_EHF = numeric(), p_EHF = numeric(), sens_slope_EHF = numeric(),
  tau_TX90 = numeric(), p_TX90 = numeric(), sens_slope_TX90 = numeric(),
  tau_TN90 = numeric(), p_TN90 = numeric(), sens_slope_TN90 = numeric()
)

# Loop through coordinate IDs
for (coord_id in unique(df_heatwave_events_trend_cpc$coordinate_id)) {
  
  # Filter and extract time series for each metric
  test_series_ehisig <- df_heatwave_events_trend_cpc %>%
    filter(coordinate_id == coord_id) %>%
    pull(Events_EHF)
  
  test_series_TX90 <- df_heatwave_events_trend_cpc %>%
    filter(coordinate_id == coord_id) %>%
    pull(Events_TX90)
  
  test_series_TN90 <- df_heatwave_events_trend_cpc %>%
    filter(coordinate_id == coord_id) %>%
    pull(Events_TN90)
  
  # Mann-Kendall and Sen's slope
  mk_result_ehisig <- if(length(test_series_ehisig) > 0) MannKendall(test_series_ehisig) else list(tau=NA, sl=NA)
  sens_result_ehisig <- if(length(test_series_ehisig) > 0) sens.slope(test_series_ehisig) else list(estimates=NA)
  
  mk_result_TX90 <- if(length(test_series_TX90) > 0) MannKendall(test_series_TX90) else list(tau=NA, sl=NA)
  sens_result_TX90 <- if(length(test_series_TX90) > 0) sens.slope(test_series_TX90) else list(estimates=NA)
  
  mk_result_TN90 <- if(length(test_series_TN90) > 0) MannKendall(test_series_TN90) else list(tau=NA, sl=NA)
  sens_result_TN90 <- if(length(test_series_TN90) > 0) sens.slope(test_series_TN90) else list(estimates=NA)
  
  # Append to results
  cpc_trend_hw_events <- rbind(cpc_trend_hw_events, data.frame(
    coordinate_id = coord_id,
    tau_EHF = mk_result_ehisig$tau, p_EHF = mk_result_ehisig$sl[1], sens_slope_EHF = sens_result_ehisig$estimates,
    tau_TX90 = mk_result_TX90$tau, p_TX90 = mk_result_TX90$sl[1], sens_slope_TX90 = sens_result_TX90$estimates,
    tau_TN90 = mk_result_TN90$tau, p_TN90 = mk_result_TN90$sl[1], sens_slope_TN90 = sens_result_TN90$estimates
  ))
}

# Create a lookup table for lon and lat
coord_lookup <- cpc_heatwave_days_trend_df %>%
  mutate(coordinate_id = as.integer(coordinate_id.x)) %>%
  select(coordinate_id, lon, lat) %>%
  distinct()

cpc_trend_hw_events <- cpc_trend_hw_events %>%
  mutate (coordinate_id = as.numeric(coordinate_id))
# Merge lon and lat into trend results
cpc_trend_hw_events <- cpc_trend_hw_events %>%
  left_join(coord_lookup, by = "coordinate_id")

# View summary of the trend analysis
summary(cpc_trend_hw_events)

#### ERA5 trend #####
# Function to calculate trend using Mann-Kendall and Sen's slope
era5_trend_hw_events <- data.frame(
  coordinate_id = integer(),
  tau_EHF = numeric(), p_EHF = numeric(), sens_slope_EHF = numeric(),
  tau_TX90 = numeric(), p_TX90 = numeric(), sens_slope_TX90 = numeric(),
  tau_TN90 = numeric(), p_TN90 = numeric(), sens_slope_TN90 = numeric()
)


# Loop through coordinate IDs and perform Mann-Kendall and Sen's slope
for (coord_id in unique(df_heatwave_events_trend_era5$coordinate_id)) {
  
  # Extract the time series for each heatwave event definition
  test_series_ehisig <- df_heatwave_events_trend_era5 %>%
    filter(coordinate_id == coord_id) %>%
    pull(Events_EHF)
  
  test_series_TX90 <- df_heatwave_events_trend_era5 %>%
    filter(coordinate_id == coord_id) %>%
    pull(Events_TX90)
  
  test_series_TN90 <- df_heatwave_events_trend_era5 %>%
    filter(coordinate_id == coord_id) %>%
    pull(Events_TN90)
  
  # Perform Mann-Kendall and Sen's slope only if data exists
  mk_result_ehisig <- if(length(test_series_ehisig) > 0) MannKendall(test_series_ehisig) else list(tau=NA, sl=NA)
  sens_result_ehisig <- if(length(test_series_ehisig) > 0) sens.slope(test_series_ehisig) else list(estimates=NA)
  
  mk_result_TX90 <- if(length(test_series_TX90) > 0) MannKendall(test_series_TX90) else list(tau=NA, sl=NA)
  sens_result_TX90 <- if(length(test_series_TX90) > 0) sens.slope(test_series_TX90) else list(estimates=NA)
  
  mk_result_TN90 <- if(length(test_series_TN90) > 0) MannKendall(test_series_TN90) else list(tau=NA, sl=NA)
  sens_result_TN90 <- if(length(test_series_TN90) > 0) sens.slope(test_series_TN90) else list(estimates=NA)
  
  # Store results for each coordinate
  era5_trend_hw_events <- rbind(era5_trend_hw_events, data.frame(
    coordinate_id = coord_id,
    tau_EHF = mk_result_ehisig$tau, p_EHF = mk_result_ehisig$sl[1], sens_slope_EHF = sens_result_ehisig$estimates,
    tau_TX90 = mk_result_TX90$tau, p_TX90 = mk_result_TX90$sl[1], sens_slope_TX90 = sens_result_TX90$estimates,
    tau_TN90 = mk_result_TN90$tau, p_TN90 = mk_result_TN90$sl[1], sens_slope_TN90 = sens_result_TN90$estimates
  ))
}

# Create a lookup table for lon and lat
coord_lookup <- era5_heatwave_days_trend_df %>%
  mutate(coordinate_id = as.integer(coordinate_id.x)) %>%
  select(coordinate_id, lon, lat) %>%
  distinct()

# Merge lon and lat into trend results
era5_trend_hw_events <-  era5_trend_hw_events %>%
  mutate (coordinate_id = as.numeric(coordinate_id))%>%
  left_join(coord_lookup, by = "coordinate_id")

# View the summary of the trend analysis
summary(era5_trend_hw_events)

#### Export ####
write_xlsx(era5_trend_hw_events, "HONDURAS HEAT/ERA5-Land/ERA5_trend_hw_events.xlsx")
write_xlsx(cpc_trend_hw_events, "HONDURAS HEAT/CPC_data/CPC_trend_hw_events.xlsx")

###### HW average duration #####
#### ERA5 data ####
# Function to compute average heatwave duration per coordinate for each heatwave definition
calculate_hw_duration <- function(df) {
  df <- df %>% arrange(time) 
  
  # Function to compute average duration of heatwave events
  avg_duration <- function(heatwave_column) {
    if (all(is.na(heatwave_column))) return(NA)  # Handle cases with all NAs
    rle_values <- rle(heatwave_column == 1)  # Identify consecutive heatwave days
    event_durations <- rle_values$lengths[rle_values$values]  # Extract durations of events
    return(mean(event_durations, na.rm = TRUE))  # Compute average duration
  }
  
  # Compute average duration for each definition
  avg_dur_ehisig <- avg_duration(df$heatwave_ehisig)
  avg_dur_TX90 <- avg_duration(df$heatwave_TX90)
  avg_dur_TN90 <- avg_duration(df$heatwave_TN90)
  
  return(data.frame(
    coordinate_id = unique(df$coordinate_id.x),
    Avg_Duration_EHF = avg_dur_ehisig,
    Avg_Duration_TX90 = avg_dur_TX90,
    Avg_Duration_TN90 = avg_dur_TN90
  ))
}

# Apply function per coordinate using `do()`
df_heatwave_avg_duration_era5 <- merged_df_era5 %>%
  group_by(coordinate_id.x, lon, lat, year.x) %>%
  do(calculate_hw_duration(.)) %>%
  ungroup()

#### ERA5 trend ####
# Trend analysis on event counts and duration
era5_trend_hw_avg_duration <- data.frame(
  coordinate_id = integer(),
  tau_Dur_EHF = numeric(), p_Dur_EHF = numeric(), sens_slope_Dur_EHF = numeric(),
  tau_Dur_TX90 = numeric(), p_Dur_TX90 = numeric(), sens_slope_Dur_TX90 = numeric(),
  tau_Dur_TN90 = numeric(), p_Dur_TN90 = numeric(), sens_slope_Dur_TN90 = numeric()
)

# Loop through coordinate IDs and perform Mann-Kendall and Sen's slope
for (coord_id in unique(df_heatwave_avg_duration_era5$coordinate_id.x)) {
  
  # Extract the time series for average duration (handling NA values)
  test_series_ehisig_duration <- df_heatwave_avg_duration_era5 %>%
    filter(coordinate_id == coord_id) %>%
    pull(Avg_Duration_EHF)
  
  test_series_TX90_duration <- df_heatwave_avg_duration_era5 %>%
    filter(coordinate_id == coord_id) %>%
    pull(Avg_Duration_TX90)
  
  test_series_TN90_duration <- df_heatwave_avg_duration_era5 %>%
    filter(coordinate_id == coord_id) %>%
    pull(Avg_Duration_TN90)
  
  # Remove NA values for Mann-Kendall and Sen's Slope calculations
  test_series_ehisig_duration <- test_series_ehisig_duration[!is.na(test_series_ehisig_duration)]
  test_series_TX90_duration <- test_series_TX90_duration[!is.na(test_series_TX90_duration)]
  test_series_TN90_duration <- test_series_TN90_duration[!is.na(test_series_TN90_duration)]
  
  # Perform Mann-Kendall and Sen's slope only if data exists
  mk_result_ehisig_duration <- if(length(test_series_ehisig_duration) > 1) MannKendall(test_series_ehisig_duration) else list(tau=NA, sl=NA)
  sens_result_ehisig_duration <- if(length(test_series_ehisig_duration) > 1) sens.slope(test_series_ehisig_duration) else list(estimates=NA)
  
  mk_result_TX90_duration <- if(length(test_series_TX90_duration) > 1) MannKendall(test_series_TX90_duration) else list(tau=NA, sl=NA)
  sens_result_TX90_duration <- if(length(test_series_TX90_duration) > 1) sens.slope(test_series_TX90_duration) else list(estimates=NA)
  
  mk_result_TN90_duration <- if(length(test_series_TN90_duration) > 1) MannKendall(test_series_TN90_duration) else list(tau=NA, sl=NA)
  sens_result_TN90_duration <- if(length(test_series_TN90_duration) > 1) sens.slope(test_series_TN90_duration) else list(estimates=NA)
  
  # Store results for each coordinate (both events and duration)
  era5_trend_hw_avg_duration <- rbind(era5_trend_hw_avg_duration , data.frame(
    coordinate_id = coord_id,
    tau_Dur_EHF = mk_result_ehisig_duration$tau, p_Dur_EHF = mk_result_ehisig_duration$sl[1], sens_slope_Dur_EHF = sens_result_ehisig_duration$estimates,
    tau_Dur_TX90 = mk_result_TX90_duration$tau, p_Dur_TX90 = mk_result_TX90_duration$sl[1], sens_slope_Dur_TX90 = sens_result_TX90_duration$estimates,
    tau_Dur_TN90 = mk_result_TN90_duration$tau, p_Dur_TN90 = mk_result_TN90_duration$sl[1], sens_slope_Dur_TN90 = sens_result_TN90_duration$estimates
  ))
}

# Create a lookup table for lon and lat
coord_lookup <- era5_heatwave_days_trend_df %>%
  mutate(coordinate_id = as.integer(coordinate_id.x)) %>%
  select(coordinate_id, lon, lat) %>%
  distinct()


# Merge lon and lat into trend results
era5_trend_hw_avg_duration <-  era5_trend_hw_avg_duration %>%
  mutate (coordinate_id = as.numeric(coordinate_id))%>%
  left_join(coord_lookup, by = "coordinate_id")

# View the summary of the trend analysis
summary(era5_trend_hw_avg_duration)

#### CPC data #### 
# Function to compute average heatwave duration per coordinate for each heatwave definition
calculate_hw_duration <- function(df) {
  df <- df %>% arrange(time) 
  
  # Function to compute average duration of heatwave events
  avg_duration <- function(heatwave_column) {
    if (all(is.na(heatwave_column))) return(NA)  # Handle cases with all NAs
    rle_values <- rle(heatwave_column == 1)  # Identify consecutive heatwave days
    event_durations <- rle_values$lengths[rle_values$values]  # Extract durations of events
    return(mean(event_durations, na.rm = TRUE))  # Compute average duration
  }
  
  # Compute average duration for each definition
  avg_dur_ehisig <- avg_duration(df$heatwave_ehisig)
  avg_dur_TX90 <- avg_duration(df$heatwave_TX90)
  avg_dur_TN90 <- avg_duration(df$heatwave_TN90)
  
  return(data.frame(
    coordinate_id = unique(df$coordinate_id.x),
    Avg_Duration_EHF = avg_dur_ehisig,
    Avg_Duration_TX90 = avg_dur_TX90,
    Avg_Duration_TN90 = avg_dur_TN90
  ))
}

# Apply function per coordinate using `do()`
df_heatwave_avg_duration_cpc <- merged_df_cpc %>%
  group_by(coordinate_id.x, lon, lat, year.x) %>%
  do(calculate_hw_duration(.)) %>%
  ungroup()

#### CPC trend ####
cpc_trend_hw_avg_duration <- data.frame(
  coordinate_id = integer(),
  tau_Dur_EHF = numeric(), p_Dur_EHF = numeric(), sens_slope_Dur_EHF = numeric(),
  tau_Dur_TX90 = numeric(), p_Dur_TX90 = numeric(), sens_slope_Dur_TX90 = numeric(),
  tau_Dur_TN90 = numeric(), p_Dur_TN90 = numeric(), sens_slope_Dur_TN90 = numeric()
)

# Loop through coordinate IDs and perform Mann-Kendall and Sen's slope
for (coord_id in unique(df_heatwave_avg_duration_cpc$coordinate_id.x)) {
  
  # Extract the time series for average duration (handling NA values)
  test_series_ehisig_duration <- df_heatwave_avg_duration_cpc %>%
    filter(coordinate_id == coord_id) %>%
    pull(Avg_Duration_EHF)
  
  test_series_TX90_duration <- df_heatwave_avg_duration_cpc %>%
    filter(coordinate_id == coord_id) %>%
    pull(Avg_Duration_TX90)
  
  test_series_TN90_duration <- df_heatwave_avg_duration_cpc %>%
    filter(coordinate_id == coord_id) %>%
    pull(Avg_Duration_TN90)
  
  # Remove NA values for Mann-Kendall and Sen's Slope calculations
  test_series_ehisig_duration <- test_series_ehisig_duration[!is.na(test_series_ehisig_duration)]
  test_series_TX90_duration <- test_series_TX90_duration[!is.na(test_series_TX90_duration)]
  test_series_TN90_duration <- test_series_TN90_duration[!is.na(test_series_TN90_duration)]
  
  # Perform Mann-Kendall and Sen's slope only if data exists
  mk_result_ehisig_duration <- if(length(test_series_ehisig_duration) > 1) MannKendall(test_series_ehisig_duration) else list(tau=NA, sl=NA)
  sens_result_ehisig_duration <- if(length(test_series_ehisig_duration) > 1) sens.slope(test_series_ehisig_duration) else list(estimates=NA)
  
  mk_result_TX90_duration <- if(length(test_series_TX90_duration) > 1) MannKendall(test_series_TX90_duration) else list(tau=NA, sl=NA)
  sens_result_TX90_duration <- if(length(test_series_TX90_duration) > 1) sens.slope(test_series_TX90_duration) else list(estimates=NA)
  
  mk_result_TN90_duration <- if(length(test_series_TN90_duration) > 1) MannKendall(test_series_TN90_duration) else list(tau=NA, sl=NA)
  sens_result_TN90_duration <- if(length(test_series_TN90_duration) > 1) sens.slope(test_series_TN90_duration) else list(estimates=NA)
  
  # Store results for each coordinate (both events and duration)
  cpc_trend_hw_avg_duration <- rbind(cpc_trend_hw_avg_duration , data.frame(
    coordinate_id = coord_id,
    tau_Dur_EHF = mk_result_ehisig_duration$tau, p_Dur_EHF = mk_result_ehisig_duration$sl[1], sens_slope_Dur_EHF = sens_result_ehisig_duration$estimates,
    tau_Dur_TX90 = mk_result_TX90_duration$tau, p_Dur_TX90 = mk_result_TX90_duration$sl[1], sens_slope_Dur_TX90 = sens_result_TX90_duration$estimates,
    tau_Dur_TN90 = mk_result_TN90_duration$tau, p_Dur_TN90 = mk_result_TN90_duration$sl[1], sens_slope_Dur_TN90 = sens_result_TN90_duration$estimates
  ))
}


# Create a lookup table for lon and lat
coord_lookup <-  cpc_heatwave_days_trend_df  %>%
  mutate(coordinate_id = as.integer(coordinate_id.x)) %>%
  select(coordinate_id, lon, lat) %>%
  distinct()

# Merge lon and lat into trend results
cpc_trend_hw_avg_duration  <- cpc_trend_hw_avg_duration  %>%
  mutate (coordinate_id = as.numeric(coordinate_id))  %>%
  left_join(coord_lookup, by = "coordinate_id")

# View the summary of the trend analysis
summary(cpc_trend_hw_avg_duration)

#### Export ####
write_xlsx(era5_trend_hw_avg_duration, "HONDURAS HEAT/ERA5-Land/ERA5_trend_avg_duration.xlsx")
write_xlsx(cpc_trend_hw_avg_duration, "HONDURAS HEAT/CPC_data/CPC_trend_avg_duration.xlsx")

###### HW longest duration #####
#### CPC & ERA5 data ####
# Function to compute the longest heatwave length for each year
calculate_longest_hw <- function(df) {
  df <- df %>% arrange(time)  # Ensure data is sorted by time
  
  # Inner function to calculate longest streak of 1s
  longest_hw_per_year <- function(heatwave_column) {
    if (all(is.na(heatwave_column))) return(NA)
    heatwave_column[is.na(heatwave_column)] <- 0
    rle_values <- rle(heatwave_column == 1)
    event_durations <- rle_values$lengths[rle_values$values]
    if (length(event_durations) == 0) return(NA)
    return(max(event_durations, na.rm = TRUE))
  }
  
  # Extract year from date column
  df$year <- format(df$time, "%Y")
  
  # Summarize longest heatwave durations for each year
  longest_hw_by_year <- df %>%
    group_by(year,coordinate_id.x) %>%
    summarise(
      longest_hw_ehisig = longest_hw_per_year(heatwave_ehisig),
      longest_hw_TX90 = longest_hw_per_year(heatwave_TX90),
      longest_hw_TN90 = longest_hw_per_year(heatwave_TN90),
      .groups = "drop"
    )
  return(longest_hw_by_year)
}

# Apply the function to your dataframe
cpc_longest_heatwaves <- calculate_longest_hw(merged_df_cpc)
era5_longest_heatwaves <- calculate_longest_hw(merged_df_era5)

#### CPC trend ####
# Create an empty dataframe to store results of the trend analysis
cpc_trend_hw_longest <- data.frame(
  coordinate_id = integer(),
  tau_LHW_EHF = numeric(), p_LHW_EHF = numeric(), sens_slope_LHW_EHF = numeric(),
  tau_LHW_TX90 = numeric(), p_LHW_TX90 = numeric(), sens_slope_LHW_TX90 = numeric(),
  tau_LHW_TN90 = numeric(), p_LHW_TN90 = numeric(), sens_slope_LHW_TN90 = numeric()
)

# Loop through each coordinate
for (coord_id in unique(cpc_longest_heatwaves$coordinate_id.x)) {
  
  sub_data <- cpc_longest_heatwaves %>%
    filter(coordinate_id.x == coord_id) %>%
    arrange(year)
  
  # Get year vectors
  years <- sub_data$year
  
  # EHISIG
  data_ehisig <- data.frame(year = years, value = sub_data$longest_hw_ehisig) %>%
    filter(!is.na(value))
  
  mk_ehisig <- if (nrow(data_ehisig) > 1) MannKendall(data_ehisig$value) else list(tau = NA, sl = NA)
  slope_ehisig <- if (nrow(data_ehisig) > 1) sens.slope(data_ehisig$value) else list(estimates = NA)
  
  # TX90
  data_TX90 <- data.frame(year = years, value = sub_data$longest_hw_TX90) %>%
    filter(!is.na(value))
  
  mk_TX90 <- if (nrow(data_TX90) > 1) MannKendall(data_TX90$value) else list(tau = NA, sl = NA)
  slope_TX90 <- if (nrow(data_TX90) > 1) sens.slope(data_TX90$value) else list(estimates = NA)
  
  # TN90
  data_TN90 <- data.frame(year = years, value = sub_data$longest_hw_TN90) %>%
    filter(!is.na(value))
  
  mk_TN90 <- if (nrow(data_TN90) > 1) MannKendall(data_TN90$value) else list(tau = NA, sl = NA)
  slope_TN90 <- if (nrow(data_TN90) > 1) sens.slope(data_TN90$value) else list(estimates = NA)
  
  # Append to final trend results
  cpc_trend_hw_longest <- rbind(cpc_trend_hw_longest, data.frame(
    coordinate_id = coord_id,
    tau_LHW_EHF = mk_ehisig$tau, p_LHW_EHF = mk_ehisig$sl[1], sens_slope_LHW_EHF = slope_ehisig$estimates,
    tau_LHW_TX90 = mk_TX90$tau, p_LHW_TX90 = mk_TX90$sl[1], sens_slope_LHW_TX90 = slope_TX90$estimates,
    tau_LHW_TN90 = mk_TN90$tau, p_LHW_TN90 = mk_TN90$sl[1], sens_slope_LHW_TN90 = slope_TN90$estimates
  ))
}

# Add lon/lat
coord_lookup <- cpc_heatwave_days_trend_df %>%
  mutate(coordinate_id = as.integer(coordinate_id.x)) %>%
  select(coordinate_id, lon, lat) %>%
  distinct()

cpc_trend_hw_longest <- cpc_trend_hw_longest  %>%
  mutate (coordinate_id = as.numeric(coordinate_id)) %>%
  left_join(coord_lookup, by = "coordinate_id")

# View the results
summary(cpc_trend_hw_longest)

##### ERA5 trend #####
# Create an empty dataframe to store results of the trend analysis
era5_trend_hw_longest <- data.frame(
  coordinate_id = integer(),
  tau_LHW_EHF = numeric(), p_LHW_EHF = numeric(), sens_slope_LHW_EHF = numeric(),
  tau_LHW_TX90 = numeric(), p_LHW_TX90 = numeric(), sens_slope_LHW_TX90 = numeric(),
  tau_LHW_TN90 = numeric(), p_LHW_TN90 = numeric(), sens_slope_LHW_TN90 = numeric()
)

# Loop through each coordinate
for (coord_id in unique(era5_longest_heatwaves$coordinate_id.x)) {
  
  sub_data <- era5_longest_heatwaves %>%
    filter(coordinate_id.x == coord_id) %>%
    arrange(year)
  
  years <- sub_data$year
  
  # EHISIG
  data_ehisig <- data.frame(year = years, value = sub_data$longest_hw_ehisig) %>%
    filter(!is.na(value))
  
  mk_ehisig <- if (nrow(data_ehisig) > 1) MannKendall(data_ehisig$value) else list(tau = NA, sl = NA)
  slope_ehisig <- if (nrow(data_ehisig) > 1) sens.slope(data_ehisig$value) else list(estimates = NA)
  
  # TX90
  data_TX90 <- data.frame(year = years, value = sub_data$longest_hw_TX90) %>%
    filter(!is.na(value))
  
  mk_TX90 <- if (nrow(data_TX90) > 1) MannKendall(data_TX90$value) else list(tau = NA, sl = NA)
  slope_TX90 <- if (nrow(data_TX90) > 1) sens.slope(data_TX90$value) else list(estimates = NA)
  
  # TN90
  data_TN90 <- data.frame(year = years, value = sub_data$longest_hw_TN90) %>%
    filter(!is.na(value))
  
  mk_TN90 <- if (nrow(data_TN90) > 1) MannKendall(data_TN90$value) else list(tau = NA, sl = NA)
  slope_TN90 <- if (nrow(data_TN90) > 1) sens.slope(data_TN90$value) else list(estimates = NA)
  
  # Append to results
  era5_trend_hw_longest <- rbind(era5_trend_hw_longest, data.frame(
    coordinate_id = coord_id,
    tau_LHW_EHF = mk_ehisig$tau, p_LHW_EHF = mk_ehisig$sl[1], sens_slope_LHW_EHF = slope_ehisig$estimates,
    tau_LHW_TX90 = mk_TX90$tau, p_LHW_TX90 = mk_TX90$sl[1], sens_slope_LHW_TX90 = slope_TX90$estimates,
    tau_LHW_TN90 = mk_TN90$tau, p_LHW_TN90 = mk_TN90$sl[1], sens_slope_LHW_TN90 = slope_TN90$estimates
  ))
}

# Create a lookup table for lon and lat
coord_lookup <- era5_heatwave_days_trend_df %>%
  mutate(coordinate_id = as.integer(coordinate_id.x)) %>%
  select(coordinate_id, lon, lat) %>%
  distinct()

# Merge lon and lat into trend results
era5_trend_hw_longest <-  era5_trend_hw_longest %>%
  mutate (coordinate_id = as.numeric(coordinate_id))%>%
  left_join(coord_lookup, by = "coordinate_id")

# View the results
summary(era5_trend_hw_longest)

#### Export ####
write_xlsx(era5_trend_hw_longest, "HONDURAS HEAT/ERA5-Land/ERA5_trend_avg_longest.xlsx")
write_xlsx(cpc_trend_hw_longest, "HONDURAS HEAT/CPC_data/CPC_trend_avg_longest.xlsx")

###### HW Magnitude ######
#### CPC data ####
# Make numeric and sort 
merged_df_cpc <- merged_df_cpc %>%
  mutate(time = as.Date(time)) %>%
  mutate(tmax.x = as.numeric(tmax.x)) %>%
  mutate(tmin.x = as.numeric(tmin.x)) %>%
  mutate(mean.x = as.numeric(mean.x)) %>%
  arrange(coordinate_id.x, time)

# Compute the average TX, TN, and Tm during each heatwave definition
df_magnitude_trend_cpc <- merged_df_cpc %>%
  group_by(coordinate_id.x, lon, lat, year.x) %>%
  summarise(
    avg_Tm_EHF  = mean(mean.x[heatwave_ehisig == 1], na.rm = TRUE), # EHF-based heatwave
    avg_TX_TX90 = mean(tmax.x[heatwave_TX90 == 1], na.rm = TRUE),  # TX90-based heatwave
    avg_TN_TN90 = mean(tmin.x[heatwave_TN90 == 1], na.rm = TRUE),  # TN90-based heatwave
   .groups = "drop"
) %>%
  mutate(across(starts_with("avg_"), ~ifelse(is.nan(.), NA, .)))  # convert NaNs to NA

#### CPC trend ####

# Initialize empty trend results df
cpc_trend_magnitude <- data.frame(
  coordinate_id = integer(),
  tau_ehisig = numeric(), p_ehisig = numeric(), sens_slope_ehisig = numeric(),
  tau_TX90 = numeric(), p_TX90 = numeric(), sens_slope_TX90 = numeric(),
  tau_TN90 = numeric(), p_TN90 = numeric(), sens_slope_TN90 = numeric()
)

# Loop through each unique coordinate_id
for (coord_id in unique(df_magnitude_trend_cpc$coordinate_id.x)) {
  
  # Filter by coordinate
  coord_data <- df_magnitude_trend_cpc %>% filter(coordinate_id.x == coord_id)
  
  # Check if there are enough data points after filtering
  if (nrow(coord_data) >= 3) {
    
    # Extract time series
    series_ehisig <- coord_data$avg_Tm_EHF
    series_TX90 <- coord_data$avg_TX_TX90
    series_TN90 <- coord_data$avg_TN_TN90
    
    # Remove NAs from the series (if any)
    series_ehisig <- na.omit(series_ehisig)
    series_TX90 <- na.omit(series_TX90)
    series_TN90 <- na.omit(series_TN90)
    
    # Check if there are enough non-NA values after removing NAs
    if (length(series_ehisig) >= 3) {
      mk_ehisig <- MannKendall(series_ehisig)
      ss_ehisig <- sens.slope(series_ehisig)
    } else {
      mk_ehisig <- list(tau = NA, sl = NA)
      ss_ehisig <- list(estimates = NA)
    }
    
    if (length(series_TX90) >= 3) {
      mk_TX90 <- MannKendall(series_TX90)
      ss_TX90 <- sens.slope(series_TX90)
    } else {
      mk_TX90 <- list(tau = NA, sl = NA)
      ss_TX90 <- list(estimates = NA)
    }
    
    if (length(series_TN90) >= 3) {
      mk_TN90 <- MannKendall(series_TN90)
      ss_TN90 <- sens.slope(series_TN90)
    } else {
      mk_TN90 <- list(tau = NA, sl = NA)
      ss_TN90 <- list(estimates = NA)
    }
    
    # Store in results df
    cpc_trend_magnitude <- rbind(cpc_trend_magnitude, data.frame(
      coordinate_id = coord_id,
      tau_ehisig = mk_ehisig$tau, p_ehisig = mk_ehisig$sl[1], sens_slope_ehisig = ss_ehisig$estimates,
      tau_TX90 = mk_TX90$tau, p_TX90 = mk_TX90$sl[1], sens_slope_TX90 = ss_TX90$estimates,
      tau_TN90 = mk_TN90$tau, p_TN90 = mk_TN90$sl[1], sens_slope_TN90 = ss_TN90$estimates
    ))
  }
}

# Add lon/lat
coord_lookup <- cpc_heatwave_days_trend_df %>%
  mutate(coordinate_id = as.integer(coordinate_id.x)) %>%
  select(coordinate_id, lon, lat) %>%
  distinct()

cpc_trend_magnitude <- cpc_trend_magnitude  %>%
  mutate (coordinate_id = as.numeric(coordinate_id)) %>%
  left_join(coord_lookup, by = "coordinate_id")


summary(cpc_trend_magnitude)

#### ERA5 data ####
# Make numeric and sort
merged_df_era5 <- merged_df_era5 %>%
  mutate(time = as.Date(time)) %>%
  mutate(tmax.x = as.numeric(tmax.x)) %>%
  mutate(tmin.x = as.numeric(tmin.x)) %>%
  mutate(mean.x = as.numeric(mean.x)) %>%
  arrange(coordinate_id.x, time)

# Compute the average TX, TN, and Tm during each heatwave definition
df_magnitude_trend_era5 <- merged_df_era5 %>%
  group_by(coordinate_id.x, lon, lat, year.x) %>%
  summarise(
    avg_Tm_EHF  = mean(mean.x[heatwave_ehisig == 1], na.rm = TRUE),  # EHF-based heatwave
    avg_TX_TX90 = mean(tmax.x[heatwave_TX90 == 1], na.rm = TRUE),    # TX90-based heatwave
    avg_TN_TN90 = mean(tmin.x[heatwave_TN90 == 1], na.rm = TRUE),    # TN90-based heatwave
    .groups = "drop"
  ) %>%
  mutate(across(starts_with("avg_"), ~ifelse(is.nan(.), NA, .)))  # Convert NaNs to NA

#### ERA5 trend ####

# Initialize empty trend results df
era5_trend_magnitude <- data.frame(
  coordinate_id = integer(),
  tau_ehisig = numeric(), p_ehisig = numeric(), sens_slope_ehisig = numeric(),
  tau_TX90 = numeric(), p_TX90 = numeric(), sens_slope_TX90 = numeric(),
  tau_TN90 = numeric(), p_TN90 = numeric(), sens_slope_TN90 = numeric()
)

# Loop through each unique coordinate_id
for (coord_id in unique(df_magnitude_trend_era5$coordinate_id.x)) {
  
  # Filter by coordinate
  coord_data <- df_magnitude_trend_era5 %>% filter(coordinate_id.x == coord_id)
  
  if (nrow(coord_data) >= 3) {
    
    # Extract time series
    series_ehisig <- na.omit(coord_data$avg_Tm_EHF)
    series_TX90   <- na.omit(coord_data$avg_TX_TX90)
    series_TN90   <- na.omit(coord_data$avg_TN_TN90)
    
    # Mann-Kendall and Sen's Slope
    if (length(series_ehisig) >= 3) {
      mk_ehisig <- MannKendall(series_ehisig)
      ss_ehisig <- sens.slope(series_ehisig)
    } else {
      mk_ehisig <- list(tau = NA, sl = NA)
      ss_ehisig <- list(estimates = NA)
    }
    
    if (length(series_TX90) >= 3) {
      mk_TX90 <- MannKendall(series_TX90)
      ss_TX90 <- sens.slope(series_TX90)
    } else {
      mk_TX90 <- list(tau = NA, sl = NA)
      ss_TX90 <- list(estimates = NA)
    }
    
    if (length(series_TN90) >= 3) {
      mk_TN90 <- MannKendall(series_TN90)
      ss_TN90 <- sens.slope(series_TN90)
    } else {
      mk_TN90 <- list(tau = NA, sl = NA)
      ss_TN90 <- list(estimates = NA)
    }
    
    # Store in results df
    era5_trend_magnitude <- rbind(era5_trend_magnitude, data.frame(
      coordinate_id = coord_id,
      tau_ehisig = mk_ehisig$tau, p_ehisig = mk_ehisig$sl[1], sens_slope_ehisig = ss_ehisig$estimates,
      tau_TX90 = mk_TX90$tau, p_TX90 = mk_TX90$sl[1], sens_slope_TX90 = ss_TX90$estimates,
      tau_TN90 = mk_TN90$tau, p_TN90 = mk_TN90$sl[1], sens_slope_TN90 = ss_TN90$estimates
    ))
  }
}

# Merge lon/lat info 
coord_lookup_era5 <- merged_df_era5 %>%
  mutate(coordinate_id = as.integer(coordinate_id.x)) %>%
  select(coordinate_id, lon, lat) %>%
  distinct()

era5_trend_magnitude <- era5_trend_magnitude %>%
  mutate(coordinate_id = as.numeric(coordinate_id)) %>%
  left_join(coord_lookup_era5, by = "coordinate_id")

# View results
summary(era5_trend_magnitude)


#### Export ####
write_xlsx(cpc_trend_magnitude, "HONDURAS HEAT/CPC_data/CPC_trend_magnitude.xlsx")
write_xlsx(era5_trend_magnitude, "HONDURAS HEAT/ERA5-Land/ERA5_trend_magnitude.xlsx")
##### Intensity #####
#### ERA5 data ####
# The mean event intensity calculated as the exceedance above the 90th percentile; 
# (For EHF, the average excess heat felt during all heatwaves)
merged_df_era5_intensity <- merged_df_era5 %>%
  mutate(
    intensity_TX90 = tmax.x-TX90,   # TX90 exceedance
    intensity_TN90 =  tmin.x-TN90   # TN90 exceedance
  )

# Compute the average intensity for each heatwave definition
df_heatwave_avg_intensity_era5_trend <- merged_df_era5_intensity %>%
  group_by(coordinate_id.x, lon, lat, year.x) %>%
  summarise(
    avg_event_intensity_TX90 = mean(intensity_TX90[heatwave_TX90 == 1], na.rm = TRUE),
    avg_event_intensity_TN90 = mean(intensity_TN90[heatwave_TN90 == 1], na.rm = TRUE),
    avg_excess_heat_EHF = mean(ehf[heatwave_ehisig == 1], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(across(starts_with("avg_"), ~ifelse(is.nan(.), NA, .)))  # NaN vervangen door NA

#### ERA5 trend ####
era5_trend_intensity <- data.frame(
  coordinate_id = integer(),
  tau_ehisig = numeric(), p_ehisig = numeric(), sens_slope_ehisig = numeric(),
  tau_TX90 = numeric(), p_TX90 = numeric(), sens_slope_TX90 = numeric(),
  tau_TN90 = numeric(), p_TN90 = numeric(), sens_slope_TN90 = numeric()
)

for (coord_id in unique(df_heatwave_avg_intensity_era5_trend$coordinate_id.x)) {
  
  coord_data <- df_heatwave_avg_intensity_era5_trend %>%
    filter(coordinate_id.x == coord_id)
  
  series_ehisig <- coord_data$avg_excess_heat_EHF
  series_TX90   <- coord_data$avg_event_intensity_TX90
  series_TN90   <- coord_data$avg_event_intensity_TN90
  
  mk_ehisig <- if (sum(!is.na(series_ehisig)) >= 3) MannKendall(series_ehisig) else list(tau = NA, sl = NA)
  ss_ehisig <- if (sum(!is.na(series_ehisig)) >= 3) sens.slope(na.omit(series_ehisig)) else list(estimates = NA)
  
  mk_TX90 <- if (sum(!is.na(series_TX90)) >= 3) MannKendall(series_TX90) else list(tau = NA, sl = NA)
  ss_TX90 <- if (sum(!is.na(series_TX90)) >= 3) sens.slope(na.omit(series_TX90)) else list(estimates = NA)
  
  mk_TN90 <- if (sum(!is.na(series_TN90)) >= 3) MannKendall(series_TN90) else list(tau = NA, sl = NA)
  ss_TN90 <- if (sum(!is.na(series_TN90)) >= 3) sens.slope(na.omit(series_TN90)) else list(estimates = NA)
  
  era5_trend_intensity <- rbind(era5_trend_intensity, data.frame(
    coordinate_id = coord_id,
    tau_ehisig = mk_ehisig$tau, p_ehisig = mk_ehisig$sl[1], sens_slope_ehisig = ss_ehisig$estimates,
    tau_TX90 = mk_TX90$tau, p_TX90 = mk_TX90$sl[1], sens_slope_TX90 = ss_TX90$estimates,
    tau_TN90 = mk_TN90$tau, p_TN90 = mk_TN90$sl[1], sens_slope_TN90 = ss_TN90$estimates
  ))
}

# Merge coordinates into final output 
coord_lookup_intensity_era5 <- df_heatwave_avg_intensity_era5_trend %>%
  mutate(coordinate_id = coordinate_id.x) %>%
  select(coordinate_id, lon, lat) %>%
  distinct()

era5_trend_intensity <- era5_trend_intensity %>%
  left_join(coord_lookup_intensity_era5, by = "coordinate_id") %>%
  mutate(lon = as.numeric(lon), lat = as.numeric(lat))

## View summary
summary(era5_trend_intensity)

#### CPC data ####
#the mean event intensity calculated as the exceedance above the 90th percentile; 
#(For EHF, the average excess heat felt during all heatwaves)

# Compute the intensities
merged_df_cpc_intensity <- merged_df_cpc %>%
  mutate(
    intensity_TX90 = tmax.x-TX90,   # TX90 exceedance
    intensity_TN90 =  tmin.x-TN90   # TN90 exceedance
  )

# Compute the average intensity for each heatwave definition
df_heatwave_avg_intensity_cpc_trend <- merged_df_cpc_intensity %>%
  group_by(coordinate_id.x, lon, lat, year.x) %>%
  summarise(
    avg_event_intensity_TX90 = mean(intensity_TX90[heatwave_TX90 == 1], na.rm = TRUE),
    avg_event_intensity_TN90 = mean(intensity_TN90[heatwave_TN90 == 1], na.rm = TRUE),
    avg_excess_heat_EHF = mean(ehf[heatwave_ehisig == 1], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(across(starts_with("avg_"), ~ifelse(is.nan(.), NA, .)))  # convert NaNs to NA

#### CPC trend ####
# Initialize output dataframe
cpc_trend_intensity <- data.frame(
  coordinate_id = integer(),
  tau_ehisig = numeric(), p_ehisig = numeric(), sens_slope_ehisig = numeric(),
  tau_TX90 = numeric(), p_TX90 = numeric(), sens_slope_TX90 = numeric(),
  tau_TN90 = numeric(), p_TN90 = numeric(), sens_slope_TN90 = numeric()
)

# Loop through each unique coordinate_id
for (coord_id in unique(df_heatwave_avg_intensity_cpc_trend$coordinate_id.x)) {
  
  coord_data <- df_heatwave_avg_intensity_cpc_trend %>%
    filter(coordinate_id.x == coord_id)
  
  series_ehisig <- coord_data$avg_excess_heat_EHF
  series_TX90   <- coord_data$avg_event_intensity_TX90
  series_TN90   <- coord_data$avg_event_intensity_TN90
  
  mk_ehisig <- if (sum(!is.na(series_ehisig)) >= 3) MannKendall(series_ehisig) else list(tau = NA, sl = NA)
  ss_ehisig <- if (sum(!is.na(series_ehisig)) >= 3) sens.slope(na.omit(series_ehisig)) else list(estimates = NA)
  
  mk_TX90 <- if (sum(!is.na(series_TX90)) >= 3) MannKendall(series_TX90) else list(tau = NA, sl = NA)
  ss_TX90 <- if (sum(!is.na(series_TX90)) >= 3) sens.slope(na.omit(series_TX90)) else list(estimates = NA)
  
  mk_TN90 <- if (sum(!is.na(series_TN90)) >= 3) MannKendall(series_TN90) else list(tau = NA, sl = NA)
  ss_TN90 <- if (sum(!is.na(series_TN90)) >= 3) sens.slope(na.omit(series_TN90)) else list(estimates = NA)
  
  cpc_trend_intensity <- rbind(cpc_trend_intensity, data.frame(
    coordinate_id = coord_id,
    tau_ehisig = mk_ehisig$tau, p_ehisig = mk_ehisig$sl[1], sens_slope_ehisig = ss_ehisig$estimates,
    tau_TX90 = mk_TX90$tau, p_TX90 = mk_TX90$sl[1], sens_slope_TX90 = ss_TX90$estimates,
    tau_TN90 = mk_TN90$tau, p_TN90 = mk_TN90$sl[1], sens_slope_TN90 = ss_TN90$estimates
  ))
}


# Add lon/lat back from the intensity dataframe
coord_lookup_intensity <- df_heatwave_avg_intensity_cpc_trend %>%
  mutate(coordinate_id = coordinate_id.x) %>%
  select(coordinate_id, lon, lat) %>%
  distinct()

# Merge coordinates into final output 
cpc_trend_intensity <- cpc_trend_intensity %>%
  left_join(coord_lookup_intensity, by = "coordinate_id")
cpc_trend_intensity  <- cpc_trend_intensity  %>%
  mutate(lon = as.numeric(lon)) %>%
  mutate(lat = as.numeric(lat)) 

# View summary
summary(cpc_trend_intensity)

#### Export ####
write_xlsx(cpc_trend_intensity, "HONDURAS HEAT/CPC_data/CPC_trend_intensity.xlsx")
write_xlsx(era5_trend_intensity, "HONDURAS HEAT/ERA5-Land/ERA5_trend_intensity.xlsx")

##### Anomaly #####
#### CPC data ####
merged_df_cpc_intensity <- merged_df_cpc %>%
  mutate(
    intensity_TX90 = tmax.x - TX90,   # TX90 exceedance
    intensity_TN90 = tmin.x - TN90    # TN90 exceedance
  )

# Compute the maximum anomaly for each year for each heatwave definition
df_heatwave_avg_intensity_cpc <- merged_df_cpc_intensity %>%
  group_by(coordinate_id.x, lon, lat, year.x) %>%
  summarise(
    max_event_intensity_TX90 = ifelse(
      all(is.na(intensity_TX90[heatwave_TX90 == 1])), 
      NA_real_, 
      max(intensity_TX90[heatwave_TX90 == 1], na.rm = TRUE)
    ),
    max_event_intensity_TN90 = ifelse(
      all(is.na(intensity_TN90[heatwave_TN90 == 1])), 
      NA_real_, 
      max(intensity_TN90[heatwave_TN90 == 1], na.rm = TRUE)
    ),
    max_excess_heat_EHF = ifelse(
      all(is.na(ehf[heatwave_ehisig == 1])), 
      NA_real_, 
      max(ehf[heatwave_ehisig == 1], na.rm = TRUE)
    ),
    .groups = "drop"
  )

# Identify which intensity (TX90 or TN90) was highest per year
df_trend_yearly_max_intensity_or_EHF_cpc <- df_heatwave_avg_intensity_cpc %>%
  group_by(coordinate_id.x, lon, lat, year.x) %>%
  summarise(
    max_anomaly_TX90 = mean(max_event_intensity_TX90, na.rm = TRUE),
    max_anomaly_TN90 = mean(max_event_intensity_TN90, na.rm = TRUE),
    max_anomaly_EHF = mean(max_excess_heat_EHF, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(across(starts_with("max_anomaly"), ~ifelse(is.nan(.), NA, .)))  # convert NaNs to NA

#### ERA5 data ####
merged_df_era5_intensity <- merged_df_era5 %>%
  mutate(
    intensity_TX90 = tmax.x-TX90,   # TX90 exceedance
    intensity_TN90 =  tmin.x-TN90   # TN90 exceedance
  )

# Compute the maximum anomaly for each year for each heatwave definition
df_heatwave_avg_intensity_era5 <- merged_df_era5_intensity %>%
  group_by(coordinate_id.x, lon, lat, year.x) %>%
  summarise(
    max_event_intensity_TX90 = ifelse(
      all(is.na(intensity_TX90[heatwave_TX90 == 1])), 
      NA_real_, 
      max(intensity_TX90[heatwave_TX90 == 1], na.rm = TRUE)
    ),
    max_event_intensity_TN90 = ifelse(
      all(is.na(intensity_TN90[heatwave_TN90 == 1])), 
      NA_real_, 
      max(intensity_TN90[heatwave_TN90 == 1], na.rm = TRUE)
    ),
    max_excess_heat_EHF = ifelse(
      all(is.na(ehf[heatwave_ehisig == 1])), 
      NA_real_, 
      max(ehf[heatwave_ehisig == 1], na.rm = TRUE)
    )
  ) %>%
  ungroup()


# Identify which intensity (TX90 or TN90) was highest per year
df_average_yearly_max_intensity_or_EHF_era5 <- df_heatwave_avg_intensity_era5 %>%
  group_by(coordinate_id.x, lon, lat, year.x) %>%
  summarise(
    max_anomoly_TX90 = mean(max_event_intensity_TX90, na.rm = TRUE),
    max_anomaly_TN90 = mean(max_event_intensity_TN90, na.rm = TRUE),
    max_anomoly_EHF = mean(max_excess_heat_EHF, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(across(starts_with("max_anomaly"), ~ifelse(is.nan(.), NA, .)))  # convert NaNs to NA

#### ERA5 trend ####
# Initialize output dataframe for ERA5
era5_trend_anomaly <- data.frame(
  coordinate_id = integer(),
  tau_ehisig = numeric(), p_ehisig = numeric(), sens_slope_ehisig = numeric(),
  tau_TX90 = numeric(), p_TX90 = numeric(), sens_slope_TX90 = numeric(),
  tau_TN90 = numeric(), p_TN90 = numeric(), sens_slope_TN90 = numeric()
)

# Loop through each unique coordinate_id
for (coord_id in unique(df_average_yearly_max_intensity_or_EHF_era5$coordinate_id.x)) {
  
  coord_data <- df_average_yearly_max_intensity_or_EHF_era5 %>%
    filter(coordinate_id.x == coord_id)
  
  series_ehisig <- coord_data$max_anomoly_EHF
  series_TX90   <- coord_data$max_anomoly_TX90
  series_TN90   <- coord_data$max_anomaly_TN90
  
  mk_ehisig <- if (sum(!is.na(series_ehisig)) >= 3) MannKendall(series_ehisig) else list(tau = NA, sl = NA)
  ss_ehisig <- if (sum(!is.na(series_ehisig)) >= 3) sens.slope(na.omit(series_ehisig)) else list(estimates = NA)
  
  mk_TX90 <- if (sum(!is.na(series_TX90)) >= 3) MannKendall(series_TX90) else list(tau = NA, sl = NA)
  ss_TX90 <- if (sum(!is.na(series_TX90)) >= 3) sens.slope(na.omit(series_TX90)) else list(estimates = NA)
  
  mk_TN90 <- if (sum(!is.na(series_TN90)) >= 3) MannKendall(series_TN90) else list(tau = NA, sl = NA)
  ss_TN90 <- if (sum(!is.na(series_TN90)) >= 3) sens.slope(na.omit(series_TN90)) else list(estimates = NA)
  
  era5_trend_anomaly <- rbind(era5_trend_anomaly, data.frame(
    coordinate_id = coord_id,
    tau_ehisig = mk_ehisig$tau, p_ehisig = mk_ehisig$sl[1], sens_slope_ehisig = ss_ehisig$estimates,
    tau_TX90 = mk_TX90$tau, p_TX90 = mk_TX90$sl[1], sens_slope_TX90 = ss_TX90$estimates,
    tau_TN90 = mk_TN90$tau, p_TN90 = mk_TN90$sl[1], sens_slope_TN90 = ss_TN90$estimates
  ))
}

# Merge coordinates into final output
coord_lookup_anomaly_era5 <- df_average_yearly_max_intensity_or_EHF_era5 %>%
  mutate(coordinate_id = coordinate_id.x) %>%
  select(coordinate_id, lon, lat) %>%
  distinct()

era5_trend_anomaly <- era5_trend_anomaly %>%
  left_join(coord_lookup_anomaly_era5, by = "coordinate_id") %>%
  mutate(lon = as.numeric(lon), lat = as.numeric(lat))

summary(era5_trend_anomaly)

#### CPC trend #####
# Initialize output dataframe for CPC
cpc_trend_anomaly <- data.frame(
  coordinate_id = integer(),
  tau_ehisig = numeric(), p_ehisig = numeric(), sens_slope_ehisig = numeric(),
  tau_TX90 = numeric(), p_TX90 = numeric(), sens_slope_TX90 = numeric(),
  tau_TN90 = numeric(), p_TN90 = numeric(), sens_slope_TN90 = numeric()
)

# Loop through each unique coordinate_id
for (coord_id in unique(df_trend_yearly_max_intensity_or_EHF_cpc$coordinate_id.x)) {
  
  coord_data <- df_trend_yearly_max_intensity_or_EHF_cpc %>%
    filter(coordinate_id.x == coord_id)
  
  series_ehisig <- coord_data$max_anomaly_EHF
  series_TX90   <- coord_data$max_anomaly_TX90
  series_TN90   <- coord_data$max_anomaly_TN90
  
  mk_ehisig <- if (sum(!is.na(series_ehisig)) >= 3) MannKendall(series_ehisig) else list(tau = NA, sl = NA)
  ss_ehisig <- if (sum(!is.na(series_ehisig)) >= 3) sens.slope(na.omit(series_ehisig)) else list(estimates = NA)
  
  mk_TX90 <- if (sum(!is.na(series_TX90)) >= 3) MannKendall(series_TX90) else list(tau = NA, sl = NA)
  ss_TX90 <- if (sum(!is.na(series_TX90)) >= 3) sens.slope(na.omit(series_TX90)) else list(estimates = NA)
  
  mk_TN90 <- if (sum(!is.na(series_TN90)) >= 3) MannKendall(series_TN90) else list(tau = NA, sl = NA)
  ss_TN90 <- if (sum(!is.na(series_TN90)) >= 3) sens.slope(na.omit(series_TN90)) else list(estimates = NA)
  
  cpc_trend_anomaly <- rbind(cpc_trend_anomaly, data.frame(
    coordinate_id = coord_id,
    tau_ehisig = mk_ehisig$tau, p_ehisig = mk_ehisig$sl[1], sens_slope_ehisig = ss_ehisig$estimates,
    tau_TX90 = mk_TX90$tau, p_TX90 = mk_TX90$sl[1], sens_slope_TX90 = ss_TX90$estimates,
    tau_TN90 = mk_TN90$tau, p_TN90 = mk_TN90$sl[1], sens_slope_TN90 = ss_TN90$estimates
  ))
}

# Merge coordinates into final output
coord_lookup_anomaly_cpc <- df_trend_yearly_max_intensity_or_EHF_cpc %>%
  mutate(coordinate_id = coordinate_id.x) %>%
  select(coordinate_id, lon, lat) %>%
  distinct()

cpc_trend_anomaly <- cpc_trend_anomaly %>%
  left_join(coord_lookup_anomaly_cpc, by = "coordinate_id") %>%
  mutate(lon = as.numeric(lon), lat = as.numeric(lat))

summary(cpc_trend_anomaly)

#### Export #####
write_xlsx(cpc_trend_anomaly, "HONDURAS HEAT/CPC_data/CPC_trend_anomaly.xlsx")
write_xlsx(era5_trend_anomaly, "HONDURAS HEAT/ERA5-Land/ERA5_trend_anomaly.xlsx")
