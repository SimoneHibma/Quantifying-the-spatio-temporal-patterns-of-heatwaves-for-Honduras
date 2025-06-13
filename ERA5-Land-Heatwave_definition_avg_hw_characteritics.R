library(readxl)
library(lubridate)
library(dplyr)
library(zoo)
library(writexl)
library(readr)

#import data and add year & day of the year
data_era5 <- read_excel("D:\\HONDURAS HEAT\\ERA5-Land\\Temp_ERA5_1979-2024.csv")
data_era5$day_of_year <- yday(data$time)
data_era5$year <- year(data$time)
data_era5$lon <- as.numeric(data_era5$lon)
data_era5$lat <- as.numeric(data_era5$lat)

#lon_lat
data_era5_filtered <- data_era5[!is.na(data_era5$tmax), ]
lon_lat_era5 <- unique(data_era5_filtered[, c("lon", "lat")])
data_era5 <- data_era5_filtered
data_era5$coordinate_id <- rep(1:1008, length.out = nrow(data_era5))

######## EHF ########
# Loop through each row in the lon_lat_era5 dataframe
for (i in 1:nrow(lon_lat_era5)) {
  if (i %% 10 == 0) {
    cat("Processing row", i, "of", nrow(lon_lat_era5), "\n")
  }
  
  lon_val <- lon_lat_era5$lon[i]
  lat_val <- lon_lat_era5$lat[i]
  subset_data <- subset(data_era5, lon == lon_val & lat == lat_val)
  
  if (nrow(subset_data) > 0) {
    subset_data$coordinate_id <- i
    
    # Reference period: 1980–2009
    subset_rp <- subset(subset_data, year >= 1980 & year <= 2009)
    
    # 95th percentile of mean
    subset_95 <- quantile(subset_rp$mean, probs = 0.95, na.rm = TRUE)
    subset_data$per95 <- subset_95
    
    # Order by time
    subset_data <- subset_data[order(subset_data$time), ]
    
    # Rolling means
    subset_data$rolling_avg3 <- rollapply(subset_data$mean, width = 3, FUN = mean, align = "right", fill = NA)
    subset_data$rolling_avg_past_30 <- rollapply(subset_data$mean, width = 30, FUN = mean, align = "right", fill = NA)
    
    # EHI significant
    subset_data$ehisig <- subset_data$rolling_avg3 - subset_data$per95
    
    # Detect heatwaves (ehisig > 0 for 3+ consecutive days)
    rle_ehisig <- rle(subset_data$ehisig > 0)
    heatwave_flags <- rep(0, length(subset_data$ehisig))
    pos <- cumsum(c(1, head(rle_ehisig$lengths, -1)))
    for (j in seq_along(rle_ehisig$lengths)) {
      if (rle_ehisig$values[j] && rle_ehisig$lengths[j] >= 3) {
        idx_start <- pos[j]
        idx_end <- idx_start + rle_ehisig$lengths[j] - 1
        heatwave_flags[idx_start:idx_end] <- 1
      }
    }
    subset_data$heatwave_ehisig <- heatwave_flags
    
    # EHI accumulated
    subset_data$ehiacc <- ifelse(subset_data$heatwave_ehisig == 1,
                                 subset_data$rolling_avg3 - subset_data$rolling_avg_past_30, NA)
    
    # EHF calculation
    subset_data$ehf <- ifelse(!is.na(subset_data$ehiacc) & subset_data$ehiacc > 1,
                              subset_data$ehisig * subset_data$ehiacc,
                              subset_data$ehisig * 1)
    
    # Save to global environment (if needed)
    assign(paste0("c_", i), subset_data)
  }
}

# Combine all into one dataframe
list_of_dfs_era5 <- lapply(1:nrow(lon_lat_era5), function(i) {
  get(paste0("c_", i))
})
df_all_coordinates_era5 <- do.call(rbind, list_of_dfs_era5)
write.csv(df_all_coordinates_era5, "HONDURAS HEAT/ERA5-Land/data_era5_ehf.csv", row.names = FALSE)

######## TX90 & TN90 ########
# change lon lat to numeric, coordinate to get lon_lat

# Filter reference period (1980-2009)
ref_data <- data_era5 %>% filter(year >= 1980 & year <= 2009)%>% 
  semi_join(lon_lat_era5, by = c("lon", "lat"))

# Function to calculate the 90th percentile 
calculate_threshold <- function(df) {
  df %>%
    group_by(coordinate_id) %>%
    summarise(
      TX90 = quantile(tmax, 0.9, na.rm = TRUE),
      TN90 = quantile(tmin, 0.9, na.rm = TRUE),
      .groups = 'drop'
    )
}

# Compute threshold
temp_thresholds <- calculate_threshold(ref_data)

# Merge threshold data with 2024 data
data_era5 <- data_era5 %>%
  left_join(temp_thresholds, by = "coordinate_id" )

# Identify exceedance of TX90 (max temperature threshold)
data_era5 <- data_era5 %>%
  mutate(exceedance_TX90 = ifelse(tmax > TX90, 1, 0))

# Identify exceedance of TN90 (min temperature threshold)
data_era5 <- data_era5  %>%
  mutate(exceedance_TN90 = ifelse(tmin > TN90, 1, 0))

# Detect heatwaves: 3+ consecutive days exceeding TX90
data_era5 <- data_era5 %>%
  group_by(lon, lat) %>%
  arrange(time) %>%
  mutate(
    heatwave_TX90 = {
      r <- rle(exceedance_TX90)
      r$values[r$values == 1 & r$lengths >= 3] <- 2
      r$values[r$values == 1] <- 0
      r$values[r$values == 2] <- 1
      rep(r$values, r$lengths)
    }
  ) %>%
  ungroup()

# Detect heatwaves: 3+ consecutive days exceeding TN90
data_era5 <- data_era5 %>%
  group_by(lon, lat) %>%
  arrange(time) %>%
  mutate(
    heatwave_TN90 = {
      r <- rle(exceedance_TN90)
      r$values[r$values == 1 & r$lengths >= 3] <- 2
      r$values[r$values == 1] <- 0
      r$values[r$values == 2] <- 1
      rep(r$values, r$lengths)
    }
  ) %>%
  ungroup()

# Clean-up temporary columns if needed
# Optionally remove temporary columns that are no longer needed
data_era5 <- data_era5  %>%
  select(-exceedance_TX90, -exceedance_TN90)  # Remove temp columns

head(data_era5)
write.csv(data_era5, "HONDURAS HEAT/ERA5-Land/data_era5_90TX_90TN.csv", row.names = FALSE)


######## Combine EHF, TX90 & TN90  ########
heatwave_p90 <- data_era5
heatwave_ehf_era5 <- df_all_coordinates_era5
#heatwave_ehf_era5 <- read_excel("HONDURAS HEAT/ERA5-Land/data_era5_ehf.xlsx")
#heatwave_p90 <- read_excel("HONDURAS HEAT/ERA5-Land/data_era5_90TX_90TN.xlsx")

merged_df_era5 <- merge(heatwave_ehf_era5, heatwave_p90, by = c("lat", "lon", "time"))
colnames(merged_df_era5)
cols_to_remove <- c("rolling_avg3", "rolling_avg_past_30", "heatwave_ehf", 
                    "tmax.y", "tmin.y", "mean.y", 
                    "day_of_year.y", "year.y", "coordinate_id.y")
merged_df_era5 <- merged_df_era5[, !names(merged_df_era5) %in% cols_to_remove]
write.csv(merged_df_era5, "HONDURAS HEAT/ERA5-Land/EHF_TN90_TX90_ERA5.csv", row.names = FALSE)

# clean
rm(list = paste0("c_", 1:1008))
remove(heatwave_p90_era5_2)
######## Graps coordinates each definition ######
# Filter unique coordinate_ids
set.seed(123)  # Set seed for reproducibility
unique_ids <- unique(merged_df_era5$coordinate_id.x)

# Select 50 random coordinate IDs
random_ids <- sort(sample(unique_ids, 50))

# Open a new, larger plot window if you're using RStudio
dev.new(width = 15, height = 15)  

# Set up the layout for 50 plots (10 rows, 5 columns)
par(mfrow = c(10, 5), mar = c(4, 4, 1, 1), oma = c(1, 1, 1, 1))  

# Loop through each randomly selected coordinate_id and plot
for (coord_id in random_ids) {
  # Filter data for the current coordinate_id
  filtered_df <- merged_df_era5[merged_df_era5$coordinate_id.x == coord_id, ]
  
  # Ensure the time column is correctly formatted and remove any NA values
  filtered_df$time <- as.Date(filtered_df$time, format = "%Y-%m-%d")
  filtered_df <- na.omit(filtered_df)  
  
  # Create an empty plot for the temperature data (time on x-axis)
  plot(filtered_df$time, filtered_df$tmin.x, type = "l", col = "gray", lty = 1,
       xlab = "Time", ylab = "Temperature", main = paste("Coordinate ID =", coord_id), 
       ylim = c(15, 40))  
  
  # Plot Tmax and Mean
  lines(filtered_df$time, filtered_df$tmax.x, col = "gray", lty = 1)  
  lines(filtered_df$time, filtered_df$mean.x, col = "black", lty = 1)  
  
  # Add vertical lines for heatwave events
  abline(v = as.numeric(filtered_df$time[filtered_df$heatwave_ehisig == 1]), col = "red", lwd = 1)  
  #abline(v = as.numeric(filtered_df$time[filtered_df$heatwave_TX90 == 1]), col = "orange", lwd = 1)  
  #abline(v = as.numeric(filtered_df$time[filtered_df$heatwave_TN90 == 1]), col = "purple", lwd = 1)  
}


######## CONFUSION MATRIX Overlap definitions ######

# Create a new column with descriptive names for heatwave combinations
merged_df_era5 <- merged_df_era5 %>%
  mutate(heatwave_category = case_when(
  heatwave_ehisig == 1 & heatwave_TX90 == 0 & heatwave_TN90 == 0 ~ "EHF Only",
    heatwave_ehisig == 1 & heatwave_TX90 == 1 & heatwave_TN90 == 0 ~ "EHF + TX90",
    heatwave_ehisig == 1 & heatwave_TX90 == 0 & heatwave_TN90 == 1 ~ "EHF + TN90",
    heatwave_ehisig == 0 & heatwave_TX90 == 1 & heatwave_TN90 == 0 ~ "TX90 Only",
    heatwave_ehisig == 0 & heatwave_TX90 == 1 & heatwave_TN90 == 1 ~ "TX90 + TN90",
    heatwave_ehisig == 0 & heatwave_TX90 == 0 & heatwave_TN90 == 1 ~ "TN90 Only",
    heatwave_ehisig == 1 & heatwave_TX90 == 1 & heatwave_TN90 == 1 ~ "All Three",
    TRUE ~ "No Heatwave"
  ))

merged_df_era5 <- merged_df_era5 %>%
  mutate(heatwave_type = case_when(
heatwave_ehisig == 1 ~ "EHF", 
heatwave_TX90 == 1 ~ "TX90",
heatwave_TN90 == 1 ~ "TN90",
TRUE ~ "No Heatwave"
))


# Create a labeled 3×3 confusion matrix
conf_matrix <- table(merged_df_era5$heatwave_category)
count_hw <- table(merged_df_era5$heatwave_type)

# Print the labeled confusion matrix
print(conf_matrix)
print(count_hw)


########  MAPS Heatwave Characteristics & Tmax ######
#THRESHOLD
threshold_era5_df <- merged_df_era5_2 %>%
  select(lat, lon, per95,TX90, TN90, ehf_per85) %>%
  distinct()
write_xlsx(threshold_era5_df, "HONDURAS HEAT/ERA5-Land/ERA5_thresholds.xlsx")

#TMAX
# Calculate mean Tmax for each coordinate_id
tmax_summary <- merged_df_era5 %>%
  group_by(coordinate_id.x, lon, lat) %>%
  summarise(mean_tmax = mean(tmax.x, na.rm = TRUE), .groups = "drop")

##### Heatwave days #####
# Calculate heatwave days for each heatwave definition
heatwave_days_summary_2 <- merged_df_era5 %>%
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
df_heatwave_days_tmax_summary_era5_2 <- left_join(tmax_summary, heatwave_days_summary_2, by = c("coordinate_id.x", "lon", "lat"))

# Print the first few rows of the final dataframe
head(df_heatwave_days_tmax_summary_era5)
summary(df_heatwave_days_tmax_summary_era5_2)


# Export dataframe as an Excel file
write_xlsx(df_heatwave_days_tmax_summary_era5, "HONDURAS HEAT/ERA5-Land/ERA5_tmax_days.xlsx")


##### Heatwave events #####
# Ensure time is in Date format and data is sorted
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
    coordinate_id = unique(df$coordinate_id.x),  # Fixed typo
    Events_EHF = hw_events_ehisig,
    Events_TX90 = hw_events_TX90,
    Events_TN90 = hw_events_TN90,
    Events_EHF_p_yr = hw_events_ehisig/46,
    Events_TX90_p_yr = hw_events_TX90/46,
    Events_TN90_p_yr = hw_events_TN90/46
  ))
}

# Apply function per coordinate using `do()`
df_heatwave_events_summary_era5 <- merged_df_era5 %>%
  group_by(coordinate_id.x, lon, lat) %>%
  do(calculate_hw_metrics(.)) %>%
  ungroup()

# View
head(df_heatwave_events_summary_era5)
summary(df_heatwave_events_summary_era5)

# Export dataframe as an Excel file
write_xlsx(df_heatwave_events_summary_era5, "HONDURAS HEAT/ERA5-Land/ERA5_events.xlsx")


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
df_heatwave_avg_duration_era5 <- merged_df_era5 %>%
  group_by(coordinate_id.x, lon, lat) %>%
  do(calculate_hw_duration(.)) %>%
  ungroup()

# View results
head(df_heatwave_avg_duration_era5)
summary(df_heatwave_avg_duration_era5)

# Export dataframe as an Excel file
write_xlsx(df_heatwave_avg_duration_era5, "HONDURAS HEAT/ERA5-Land/ERA5_avg_duration.xlsx")


##### Heatwave the average length of the longest yearly heatwave #####

# Function to compute the longest heatwave length for each year
calculate_longest_hw <- function(df) {
  df <- df %>% arrange(time)  # Ensure data is sorted by time
  
  # Function to find the longest heatwave in each year
  longest_hw_per_year <- function(heatwave_column) {
    if (all(is.na(heatwave_column))) return(NA)  # Handle cases with all NAs
    rle_values <- rle(heatwave_column == 1)  # Identify consecutive heatwave days
    event_durations <- rle_values$lengths[rle_values$values]  # Extract durations of events
    
    # If there are no heatwave events, return NA
    if (length(event_durations) == 0) return(NA)
    
    # Find the longest heatwave (maximum duration)
    longest_event <- max(event_durations, na.rm = TRUE)
    return(longest_event)
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
  
  # Calculate the average length of the longest heatwave across years
  avg_longest_hw_ehisig <- mean(longest_hw_by_year$longest_hw_ehisig, na.rm = TRUE)
  avg_longest_hw_TX90 <- mean(longest_hw_by_year$longest_hw_TX90, na.rm = TRUE)
  avg_longest_hw_TN90 <- mean(longest_hw_by_year$longest_hw_TN90, na.rm = TRUE)
  
  return(data.frame(
    coordinate_id = unique(df$coordinate_id.x),
    Avg_Longest_HW_Length_Ehisig = avg_longest_hw_ehisig,
    Avg_Longest_HW_Length_TX90 = avg_longest_hw_TX90,
    Avg_Longest_HW_Length_TN90 = avg_longest_hw_TN90
  ))
}

# Apply function per coordinate using `do()`
df_heatwave_avg_longest_duration_era5 <- merged_df_era5 %>%
  group_by(coordinate_id.x, lon, lat) %>%
  do(calculate_longest_hw(.)) %>%
  ungroup()

# View results
head(df_heatwave_avg_longest_duration_era5)
summary(df_heatwave_avg_longest_duration_era5)

# Export dataframe as an Excel file
write_xlsx(df_heatwave_avg_longest_duration_era5, "HONDURAS HEAT/ERA5-Land/ERA5_yr_avg_longest_duration.xlsx")

##### Magnitude #####

# Compute the average TX, TN, and Tm during each heatwave definition
df_heatwave_magnitude_avg_corresponing_temp_cpc  <- merged_df_cpc %>%
  group_by(coordinate_id.x, lon, lat) %>%
  summarise(
    avg_TX_TX90 = ifelse(sum(heatwave_TX90, na.rm = TRUE) > 0, mean(tmax.x[heatwave_TX90 == 1], na.rm = TRUE), NA),
    avg_TN_TN90 = ifelse(sum(heatwave_TN90, na.rm = TRUE) > 0, mean(tmin.x[heatwave_TN90 == 1], na.rm = TRUE), NA),
    avg_Tm_EHF  = ifelse(sum(heatwave_ehisig, na.rm = TRUE) > 0, mean(mean.x[heatwave_ehisig == 1], na.rm = TRUE), NA),
    .groups = "drop"
  )

# View results
head(df_heatwave_magnitude_avg_corresponing_temp_cpc)
summary(df_heatwave_magnitude_avg_corresponing_temp_era5)

# Export to Excel
write_xlsx(df_heatwave_magnitude_avg_corresponing_temp_era5, "HONDURAS HEAT/ERA5-Land/ERA5_HW_Magnitude_Temperature_Averages.xlsx")

##### Intensity #####
#the mean event intensity calculated as the exceedance above the 90th percentile; 
#(For EHF, the average excess heat felt during all heatwaves)

# Compute the intensities
merged_df_era5_intensity <- merged_df_era5 %>%
  mutate(
    intensity_TX90 = tmax.x-TX90,   # TX90 exceedance
    intensity_TN90 =  tmin.x-TN90   # TN90 exceedance
  )

# Compute the average intensity for each heatwave definition
df_heatwave_avg_intensity_era5 <- merged_df_era5_intensity  %>%
  group_by(coordinate_id.x, lon, lat) %>%
  summarise(
    avg_event_intensity_TX90 = mean(intensity_TX90[heatwave_TX90 == 1], na.rm = TRUE),
    avg_event_intensity_TN90 = mean(intensity_TN90[heatwave_TN90 == 1], na.rm = TRUE),
    avg_excess_heat_EHF = mean(ehf[heatwave_ehisig == 1], na.rm = TRUE)
  ) %>%
  ungroup()

# View results
head(df_heatwave_avg_intensity_era5)
summary(df_heatwave_avg_intensity_era5)

# Export to Excel
write_xlsx(df_heatwave_avg_intensity_era5, "HONDURAS HEAT/ERA5-Land/ERA5_Avg_Intensity.xlsx")


##### Anomaly #####

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
    group_by(coordinate_id.x, lon, lat) %>%
    summarise(
      max_anomoly_TX90 = mean(max_event_intensity_TX90, na.rm = TRUE),
      max_anomaly_TN90 = mean(max_event_intensity_TN90, na.rm = TRUE),
      max_anomoly_EHF = mean(max_excess_heat_EHF, na.rm = TRUE),
      .groups = "drop"
    )
  

# View results
head(df_average_yearly_max_intensity_or_EHF_era5)
summary(df_average_yearly_max_intensity_or_EHF_era5)

write_xlsx(df_average_yearly_max_intensity_or_EHF_era5, "HONDURAS HEAT/ERA5-Land/ERA5_anomaly_yr_avg_max_intensity.xlsx")

