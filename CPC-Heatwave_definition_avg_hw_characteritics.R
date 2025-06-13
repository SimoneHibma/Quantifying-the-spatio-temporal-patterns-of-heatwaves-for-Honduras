library(readxl)
library(lubridate)
library(dplyr)
library(zoo)
library(writexl)
library(readr)

#### COMBINED TN90, TX90 & EHF ####
heatwave_ehf_cpc <- read_excel("HONDURAS HEAT/CPC_data/data_cpc_ehf.xlsx")
heatwave_p90 <- read_excel("HONDURAS HEAT/CPC_data/data_cpc_90TX_90TN.xlsx")
cols_to_remove <- c("rolling_avg3", "rolling_avg_past_30", "heatwave_ehf", 
                  "tmax.y", "tmin.y", "mean.y", 
                    "day_of_year.y", "year.y", "coordinate_id.y")
merged_df_cpc <- merged_df_cpc[, !names(merged_df_cpc) %in% cols_to_remove]
merged_df_cpc$coordinate_id.x = merged_df_cpc$coordinate_id
merged_df_cpc$tmax.x = merged_df_cpc$tmax
merged_df_cpc$tmin.x = merged_df_cpc$tmin
merged_df_cpc$mean.x = merged_df_cpc$mean

# remove coordinates that only have data until 2005
merged_df_cpc_all_coordinates <- merged_df_cpc
merged_df_cpc <- merged_df_cpc %>%
  filter(!coordinate_id.x %in% c(24, 1, 2))
merged_df_cpc<- merged_df_cpc %>%
  mutate(lon = as.numeric(lon), lat = as.numeric(lat))
write.csv(merged_df_cpc, "HONDURAS HEAT/CPC_data/EHF_TN90_TX90_CPC.csv", row.names = FALSE)

###### 1 #####

# Filter data for coordinate_id = 1
filtered_df <- merged_df[merged_df$coordinate_id.x == 1, ]
filtered_df$time <- as.Date(filtered_df$time)

# Create a matrix of temperature values (tmin, tmax, mean)
temperature_matrix <- as.matrix(filtered_df[, c("tmin.x", "tmax.x", "mean.x")])

# Create an empty plot for the temperature data (time on x-axis)
plot(filtered_df$time, filtered_df$tmin.x, type = "l", col = "gray", lty = 1,
     xlab = "Time", ylab = "Temperature", main = "Temperature Variables and Heatwave Events Over Time (Coordinate ID = 1)", 
     ylim = c(15, 40))  # Set the y-axis limit from 15 to 40

lines(filtered_df$time, filtered_df$tmax.x, col = "gray", lty = 1)  # Tmax line
lines(filtered_df$time, filtered_df$mean.x, col = "black", lty = 1)  # Mean line

# Add vertical lines for heatwave events
abline(v = as.numeric(filtered_df$time[filtered_df$heatwave_ehisig == 1]), col = "red", lwd = 1)  # Heatwave EHisig
abline(v = as.numeric(filtered_df$time[filtered_df$heatwave_TX90 == 1]), col = "orange", lwd = 1)  # Heatwave TX90
abline(v = as.numeric(filtered_df$time[filtered_df$heatwave_TN90 == 1]), col = "purple", lwd = 1)  # Heatwave TN90

legend("topright", legend = c("EHF", "TX90", "TN90"), col = c("red", "orange", "purple"), lty = 1, cex = 0.8)


###### 57 #####
# Filter data for all coordinate_ids
unique_ids <- unique(merged_df_cpc$coordinate_id.x)

# Subset to the first 57 coordinates (Coordinate IDs 1 to 57)
fifty_seven_ids <- unique_ids[c(1:57)[-which(3:57 == 24)]]

# Open a new, larger plot window if you're using RStudio
dev.new(width = 15, height = 15)  # Adjust the size of the plot window

# Set up the layout for 57 plots (9 rows, 7 columns)
par(mfrow = c(9, 7), mar = c(4, 4, 1, 1), oma = c(1, 1, 1, 1))  # Adjust margins for better space

# Loop through each coordinate_id and plot
for (coord_id in fifty_seven_ids) {
  # Filter data for the current coordinate_id
  filtered_df <- merged_df_cpc[merged_df_cpc$coordinate_id.x == coord_id, ]
  
  # Ensure the time column is correctly formatted and remove any NA values
  filtered_df$time <- as.Date(filtered_df$time, format = "%Y-%m-%d")
  filtered_df <- na.omit(filtered_df)  # Remove rows with NA values
  
  # Create an empty plot for the temperature data (time on x-axis)
  plot(filtered_df$time, filtered_df$tmin.x, type = "l", col = "gray", lty = 1,
       xlab = "Time", ylab = "Temperature", main = paste("Coordinate ID =", coord_id), 
       ylim = c(15, 40))  
  
  # Plot Tmax and Mean
  lines(filtered_df$time, filtered_df$tmax.x, col = "gray", lty = 1)  # Tmax line
  lines(filtered_df$time, filtered_df$mean.x, col = "black", lty = 1)  # Mean line
  
  # Add vertical lines for heatwave events
  #abline(v = as.numeric(filtered_df$time[filtered_df$heatwave_ehisig == 1]), col = "red", lwd = 1)  # Heatwave EHisig
  #abline(v = as.numeric(filtered_df$time[filtered_df$heatwave_TX90 == 1]), col = "orange", lwd = 1)  # Heatwave TX90
  abline(v = as.numeric(filtered_df$time[filtered_df$heatwave_TN90 == 1]), col = "purple", lwd = 1)  # Heatwave TN90
}


###### CONFUSION MATRIX ######

# Create a new column with descriptive names for heatwave combinations
filtered_cm <- merged_df_cpc %>%
  mutate(heatwave_category = case_when(
    heatwave_ehisig == 1 & heatwave_TX90 == 0 & heatwave_TN90 == 0 ~ "EHF Only",
    heatwave_ehisig == 0 & heatwave_TX90 == 1 & heatwave_TN90 == 0 ~ "TX90 Only",
    heatwave_ehisig == 0 & heatwave_TX90 == 0 & heatwave_TN90 == 1 ~ "TN90 Only",
    heatwave_ehisig == 1 & heatwave_TX90 == 1 & heatwave_TN90 == 0 ~ "EHF + TX90",
    heatwave_ehisig == 1 & heatwave_TX90 == 0 & heatwave_TN90 == 1 ~ "EHF + TN90",
    heatwave_ehisig == 0 & heatwave_TX90 == 1 & heatwave_TN90 == 1 ~ "TX90 + TN90",
    heatwave_ehisig == 1 & heatwave_TX90 == 1 & heatwave_TN90 == 1 ~ "All Three",
    TRUE ~ "No Heatwave"
  ))

# Create a labeled 3×3 confusion matrix
conf_matrix <- table(filtered_cm$heatwave_category)

# Print the labeled confusion matrix
print(conf_matrix)

########  MAPS Heatwave Characteristics & Tmax ######
## THRESHOLD
threshold_cpc_df <- merged_df_cpc_2 %>%
  select(lat, lon, per95, TX90, TN90, ehf_per85) %>%
  distinct()
write_xlsx(threshold_cpc_df, "HONDURAS HEAT/CPC_data/CPC_thresholds.xlsx")


#TMAX
# Calculate mean Tmax for each coordinate_id
tmax_summary <- merged_df_cpc %>%
  group_by(coordinate_id.x, lon, lat) %>%
  summarise(mean_tmax = mean(tmax.x, na.rm = TRUE), .groups = "drop")

##### Heatwave days #####
# Calculate heatwave days for each heatwave definition
heatwave_days_summary <- merged_df_cpc %>%
  group_by(coordinate_id.x, lon, lat) %>%
  summarise(
    heatwave_ehisig_days = sum(heatwave_ehisig, na.rm = TRUE),
    heatwave_TX90_days = sum(heatwave_TX90, na.rm = TRUE),
    heatwave_TN90_days = sum(heatwave_TN90, na.rm = TRUE),
    heatwave_ehisig_days_p_yr = sum(heatwave_ehisig, na.rm = TRUE)/46,
    heatwave_TX90_days_p_yr = sum(heatwave_TX90, na.rm = TRUE)/46,
    heatwave_TN90_days_p_yr = sum(heatwave_TN90, na.rm = TRUE)/46,
    .groups = "drop"
  )

# Merge both summaries into one dataframe
df_heatwave_days_tmax_summary_cpc <- left_join(tmax_summary, heatwave_days_summary, by = c("coordinate_id.x", "lon", "lat"))

# Print the first few rows of the final dataframe
head(df_heatwave_days_tmax_summary_cpc)
summary(df_heatwave_days_tmax_summary_cpc)


# Export dataframe as an Excel file
write_xlsx(df_heatwave_days_tmax_summary_cpc, "HONDURAS HEAT/CPC_data/CPC_tmax_days.xlsx")

##### Heatwave events #####
# Ensure time is in Date format and data is sorted
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
    Events_TN90 = hw_events_TN90,
    Events_p_yr_EHF = sum(hw_events_ehisig) / 46,
    Events_p_yr_TX90 = sum(hw_events_TX90) / 46,
    Events_p_yr_TN90 = sum(hw_events_TN90) / 46
  ))
}

# Apply function per coordinate using `do()`
df_heatwave_events_summary_cpc <- merged_df_cpc %>%
  group_by(coordinate_id.x, lon, lat) %>%
  do(calculate_hw_metrics(.)) %>%
  ungroup()

# View
head(df_heatwave_events_summary_cpc)
summary(df_heatwave_events_summary_cpc)

# Export dataframe as an Excel file
write_xlsx(df_heatwave_events_summary_cpc, "HONDURAS HEAT/CPC_data/CPC_events.xlsx")

##### Heatwave average duration #####
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
  group_by(coordinate_id.x, lon, lat) %>%
  do(calculate_hw_duration(.)) %>%
  ungroup()

# View results
head(df_heatwave_avg_duration_cpc)
summary(df_heatwave_avg_duration_cpc)

# Export dataframe as an Excel file
write_xlsx(df_heatwave_avg_duration_cpc, "HONDURAS HEAT/CPC_data/CPC_avg_duration.xlsx")

##### Heatwave: The Average Length of the Longest Yearly Heatwave #####
# Function to compute the longest heatwave length for each year
calculate_longest_hw <- function(df) {
  df <- df %>% arrange(time)  # Ensure data is sorted by time
  
  # Function to find the longest heatwave in each year
  longest_hw_per_year <- function(heatwave_column) {
    if (all(is.na(heatwave_column))) return(NA)  # Handle cases with all NAs
    heatwave_column[is.na(heatwave_column)] <- 0  # Replace NA with 0
    rle_values <- rle(heatwave_column == 1)  # Identify consecutive heatwave days
    event_durations <- rle_values$lengths[rle_values$values]  # Extract durations of events
    
    # If there are no heatwave events, return NA
    if (length(event_durations) == 0) return(NA)
    
    # Find the longest heatwave (maximum duration)
    return(max(event_durations, na.rm = TRUE))
  }
  
  # Identify the longest heatwave for each year
  df$year <- format(df$time, "%Y")  # Extract year from the time column
  longest_hw_by_year <- df %>%
    group_by(year) %>%
    summarise(
      longest_hw_ehisig = longest_hw_per_year(heatwave_ehisig),
      longest_hw_TX90 = longest_hw_per_year(heatwave_TX90),
      longest_hw_TN90 = longest_hw_per_year(heatwave_TN90),
      .groups = "drop"
    )
  
  # Function to safely compute the mean without Inf
  safe_mean <- function(x) {
    if (all(is.na(x))) return(NA)  # Return NA if all values are NA
    return(mean(x, na.rm = TRUE))  # Compute mean otherwise
  }
  
  # Calculate the average length of the longest heatwave across years
  avg_longest_hw_ehisig <- safe_mean(longest_hw_by_year$longest_hw_ehisig)
  avg_longest_hw_TX90 <- safe_mean(longest_hw_by_year$longest_hw_TX90)
  avg_longest_hw_TN90 <- safe_mean(longest_hw_by_year$longest_hw_TN90)
  
  return(data.frame(
    coordinate_id = unique(df$coordinate_id.x),
    Avg_Longest_HW_Length_Ehisig = avg_longest_hw_ehisig,
    Avg_Longest_HW_Length_TX90 = avg_longest_hw_TX90,
    Avg_Longest_HW_Length_TN90 = avg_longest_hw_TN90
  ))
}

# Apply function per coordinate using `do()`
df_heatwave_avg_longest_duration_cpc <- merged_df_cpc %>%
  group_by(coordinate_id.x, lon, lat) %>%
  do(calculate_longest_hw(.)) %>%
  ungroup()

# View results
head(df_heatwave_avg_longest_duration_cpc)
summary(df_heatwave_avg_longest_duration_cpc)

# Export dataframe as an Excel file
write_xlsx(df_heatwave_avg_longest_duration_cpc, "HONDURAS HEAT/CPC_data/CPC_yr_avg_longest_duration.xlsx")

##### Magnitude #####
# Make numeric and sort 
merged_df_cpc <- merged_df_cpc %>%
  mutate(time = as.Date(time)) %>%
  mutate(tmax.x = as.numeric(tmax.x)) %>%
  mutate(tmin.x = as.numeric(tmin.x)) %>%
  mutate(mean.x = as.numeric(mean.x)) %>%
  arrange(coordinate_id.x, time)

# Compute the average TX, TN, and Tm during each heatwave definition
df_heatwave_magnitude_avg_corresponing_temp_cpc  <- merged_df_cpc %>%
  group_by(coordinate_id.x, lon, lat) %>%
  summarise(
    avg_TX_TX90 = mean(tmax.x[heatwave_TX90 == 1], na.rm = TRUE),  # TX90-based heatwave
    avg_TN_TN90 = mean(tmin.x[heatwave_TN90 == 1], na.rm = TRUE),  # TN90-based heatwave
    avg_Tm_EHF  = mean(mean.x[heatwave_ehisig == 1], na.rm = TRUE) # EHF-based heatwave
  ) %>%
  ungroup()

# View results
head(df_heatwave_magnitude_avg_corresponing_temp_cpc)
summary(df_heatwave_magnitude_avg_corresponing_temp_cpc)

# Export to Excel
write_xlsx(df_heatwave_magnitude_avg_corresponing_temp_cpc, "HONDURAS HEAT/CPC_data/CPC_HW_Magnitude_Temperature_Averages.xlsx")

##### Intensity #####
#the mean event intensity calculated as the exceedance above the 90th percentile; 
#(For EHF, the average excess heat felt during all heatwaves)
# Compute the intensities
merged_df_era5_intensity <- merged_df_cpc_2 %>%
  mutate(
    intensity_TX90 = tmax.x-TX90,   # TX90 exceedance
    intensity_TN90 =  tmin.x-TN90   # TN90 exceedance
  )

# Compute the average intensity for each heatwave definition
df_heatwave_avg_intensity_cpc <- merged_df_era5_intensity  %>%
  group_by(coordinate_id.x, lon, lat) %>%
  summarise(
    avg_event_intensity_TX90 = mean(intensity_TX90[heatwave_TX90 == 1], na.rm = TRUE),
    avg_event_intensity_TN90 = mean(intensity_TN90[heatwave_TN90 == 1], na.rm = TRUE),
    avg_excess_heat_EHF = mean(ehf[heatwave_ehisig == 1], na.rm = TRUE)
  ) %>%
  ungroup()

# View results
head(df_heatwave_avg_intensity_cpc)
summary(df_heatwave_avg_intensity_cpc)

# Export to Excel
write_xlsx(df_heatwave_avg_intensity_cpc, "HONDURAS HEAT/CPC_data/CPC_Avg_Intensity.xlsx")

##### Anomaly #####
merged_df_cpc_intensity <- merged_df_cpc %>%
  mutate(
    intensity_TX90 = tmax.x - TX90,   # TX90 exceedance
    intensity_TN90 = tmin.x - TN90    # TN90 exceedance
  )

# Compute the maximum anomaly for each year for each heatwave definition
df_heatwave_avg_intensity_cpc <- merged_df_cpc_intensity %>%
  group_by(coordinate_id.x, lon, lat, year) %>%
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
df_average_yearly_max_intensity_or_EHF_cpc <- df_heatwave_avg_intensity_cpc %>%
  group_by(coordinate_id.x, lon, lat) %>%
  summarise(
    max_anomaly_TX90 = mean(max_event_intensity_TX90, na.rm = TRUE),
    max_anomaly_TN90 = mean(max_event_intensity_TN90, na.rm = TRUE),
    max_anomaly_EHF = mean(max_excess_heat_EHF, na.rm = TRUE),
    .groups = "drop"
  )

# View results
head(df_average_yearly_max_intensity_or_EHF_cpc)
summary(df_average_yearly_max_intensity_or_EHF_cpc)

# Export to Excel
write_xlsx(df_average_yearly_max_intensity_or_EHF_cpc, "HONDURAS HEAT/CPC_data/CPC_anomaly_yr_avg_max_intensity.xlsx")


