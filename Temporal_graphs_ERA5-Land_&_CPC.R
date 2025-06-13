library(ggplot2)
library(dplyr)
library(viridis)
library(patchwork)
library(terra)
library(sf)
library(lubridate)

# Create an sf object of the bounding box
Honduras_shp <- vect("C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\Honduras.shp")
shape_sf <- st_as_sf(Honduras_shp)
cropext_era5 <- ext(-89.4,-83.2,13,16)
cropbox_era5 <- st_as_sf(as.polygons(cropext_era5))
st_crs(cropbox_era5) <- 4326  # set CRS to match shape_sf if needed
Honduras_shp_era5 <- st_intersection(shape_sf, cropbox_era5)

######## INTERANNUAL ########
merged_df_era5$month <- month(merged_df_era5$time)
merged_df_cpc$month <- month(merged_df_cpc$time)
###### HEATWAVE DAYS #####
# ERA5
era5_heatwave_days_month <- merged_df_era5 %>%
  group_by(coordinate_id.x, lon, lat, year.x, month) %>%
  summarise(
    heatwave_ehisig_days = sum(heatwave_ehisig, na.rm = TRUE),
    heatwave_TX90_days = sum(heatwave_TX90, na.rm = TRUE),
    heatwave_TN90_days = sum(heatwave_TN90, na.rm = TRUE),
    .groups = "drop"
  )

# CPC
cpc_heatwave_days_month <- merged_df_cpc %>%
  group_by(coordinate_id.x, lon, lat, year.x, month) %>%
  summarise(
    heatwave_ehisig_days = sum(heatwave_ehisig, na.rm = TRUE),
    heatwave_TX90_days = sum(heatwave_TX90, na.rm = TRUE),
    heatwave_TN90_days = sum(heatwave_TN90, na.rm = TRUE),
    .groups = "drop"
  )

# Options: "heatwave_ehisig_days", "heatwave_TX90_days", or "heatwave_TN90_days"

####  Choose Dataset and Metric ####

# Options: "era5" or "cpc"
dataset_to_use <- "cpc" # Change this to "cpc" for CPC data

# Options: "heatwave_ehisig_days", "heatwave_TX90_days", or "heatwave_TN90_days"
metric_to_plot <- "heatwave_ehisig_days"  # Change to TX90 or TN90 as needed

# Assign the dataset
heatwave_data_month <- if (dataset_to_use == "era5") {
  era5_heatwave_days_month
} else {
  cpc_heatwave_days_month
}

####  Filter and Format ####

heatwave_data_month_filtered <- heatwave_data_month %>%
  filter(year.x >= 2000, year.x <= 2024) %>%
  mutate(
    year = factor(year.x, levels = 2000:2024),
    month = factor(month, levels = 1:12, labels = month.abb)
  )

####  Plot Facet Map (Year Rows × Month Columns) ####
pdf("C:\\Users\\User\\Documents\\HONDURAS HEAT\\Maps, graphs & tables\\Month\\new_map_give_a_name.pdf", 
    width = 8.27, height = 11.69)

# Your ggplot code
ggplot() +
  geom_tile(data = heatwave_data_month_filtered, 
            aes(x = lon, y = lat, fill = .data[[metric_to_plot]]), alpha = 0.9) +
  geom_sf(data = Honduras_shp_era5, fill = NA, color = "black", size = 0.3) +
  scale_fill_gradientn(
    colours = c("white", "#313695", "#74add1", "#ffffbf", "#f46d43", "#a50026"),
    values = scales::rescale(c(0, 1, 10, 20, 25, 31)),
    limits = c(0, 31),
    name = "Heatwave Days",
    na.value = "white"
  ) +
  coord_sf(xlim = c(-89.4, -83.2), ylim = c(13, 16), expand = FALSE) +
  theme_minimal() +
  facet_grid(rows = vars(year), cols = vars(month)) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.spacing = unit(0.3, "lines"),
    strip.text = element_text(size = 6),
    legend.position = "bottom"
  ) +
  labs(
    title = paste("Monthly Heatwave Days (", metric_to_plot, ") from ", toupper(dataset_to_use), sep = ""),
    x = NULL,
    y = NULL
  )

dev.off()

##### Percentage total ####
# Total coordinates considered
coor_era5 <- 1008
coor_cpc <- 54

# For ERA5 - April 2020
apr20_era5 <- subset(era5_heatwave_days_month, month == 4)
apr20_era5 <- subset(apr20_era5, year.x == 2020)
apr20_era5_ehf <- subset(apr20_era5, heatwave_ehisig_days >= 25)
apr20_era5_tx90 <- subset(apr20_era5, heatwave_TX90_days >= 25)
apr20_era5_tn90 <- subset(apr20_era5, heatwave_TN90_days >= 25)

# Count the number of days for each condition in ERA5 (April 2020)
ehf_days_apr_2020_era5 <- nrow(apr20_era5_ehf)
tx90_days_apr_2020_era5 <- nrow(apr20_era5_tx90)
tn90_days_apr_2020_era5 <- nrow(apr20_era5_tn90)

# Calculate percentages for ERA5 (April 2020)
ehf_pct_apr_2020_era5 <- (ehf_days_apr_2020_era5 / coor_era5) * 100
tx90_pct_apr_2020_era5 <- (tx90_days_apr_2020_era5 / coor_era5) * 100
tn90_pct_apr_2020_era5 <- (tn90_days_apr_2020_era5 / coor_era5) * 100
ehf_pct_apr_2020_era5 
tx90_pct_apr_2020_era5 
tn90_pct_apr_2020_era5 


# For ERA5 - May 2020
may20_era5 <- subset(era5_heatwave_days_month, month == 5)
may20_era5 <- subset(may20_era5, year.x == 2020)
may20_era5_ehf <- subset(may20_era5, heatwave_ehisig_days >= 25)
may20_era5_tx90 <- subset(may20_era5, heatwave_TX90_days >= 25)
may20_era5_tn90 <- subset(may20_era5, heatwave_TN90_days >= 25)

# Count the number of days for each condition in ERA5 (May 2020)
ehf_days_may_2020_era5 <- nrow(may20_era5_ehf)
tx90_days_may_2020_era5 <- nrow(may20_era5_tx90)
tn90_days_may_2020_era5 <- nrow(may20_era5_tn90)

# Calculate percentages for ERA5 (May 2020)
ehf_pct_may_2020_era5 <- (ehf_days_may_2020_era5 / coor_era5) * 100
tx90_pct_may_2020_era5 <- (tx90_days_may_2020_era5 / coor_era5) * 100
tn90_pct_may_2020_era5 <- (tn90_days_may_2020_era5 / coor_era5) * 100

ehf_pct_may_2020_era5
tx90_pct_may_2020_era5
tn90_pct_may_2020_era5

# For ERA5 - May 2024
may24_era5 <- subset(era5_heatwave_days_month, month == 5)
may24_era5 <- subset(may24_era5, year.x == 2024)
may24_era5_ehf <- subset(may24_era5, heatwave_ehisig_days >= 25)  # Adjusted to 31
may24_era5_tx90 <- subset(may24_era5, heatwave_TX90_days >= 25)    # Adjusted to 31
may24_era5_tn90 <- subset(may24_era5, heatwave_TN90_days >= 25)    # Adjusted to 31

# Count the number of days for each condition in ERA5 (May 2024)
ehf_days_may_2024_era5 <- nrow(may24_era5_ehf)
tx90_days_may_2024_era5 <- nrow(may24_era5_tx90)
tn90_days_may_2024_era5 <- nrow(may24_era5_tn90)
ehf_days_may_2024_era5

# Calculate percentages for ERA5 (May 2024)
ehf_pct_may_2024_era5 <- (ehf_days_may_2024_era5 / coor_era5) * 100
tx90_pct_may_2024_era5 <- (tx90_days_may_2024_era5 / coor_era5) * 100
tn90_pct_may_2024_era5 <- (tn90_days_may_2024_era5 / coor_era5) * 100


# For CPC - April 2020
apr20_cpc <- subset(cpc_heatwave_days_month, month == 4)
apr20_cpc <- subset(apr20_cpc, year.x == 2020)
apr20_cpc_ehf <- subset(apr20_cpc, heatwave_ehisig_days >= 25)
apr20_cpc_tx90 <- subset(apr20_cpc, heatwave_TX90_days >= 25)
apr20_cpc_tn90 <- subset(apr20_cpc, heatwave_TN90_days >= 25)

# Count the number of days for each condition in CPC (April 2020)
ehf_days_apr_2020_cpc <- nrow(apr20_cpc_ehf)
tx90_days_apr_2020_cpc <- nrow(apr20_cpc_tx90)
tn90_days_apr_2020_cpc <- nrow(apr20_cpc_tn90)

# Calculate percentages for CPC (April 2020)
ehf_pct_apr_2020_cpc <- (ehf_days_apr_2020_cpc / coor_cpc) * 100
tx90_pct_apr_2020_cpc <- (tx90_days_apr_2020_cpc / coor_cpc) * 100
tn90_pct_apr_2020_cpc <- (tn90_days_apr_2020_cpc / coor_cpc) * 100
ehf_pct_apr_2020_cpc
tx90_pct_apr_2020_cpc
tn90_pct_apr_2020_cpc

# For CPC - May 2024
may24_cpc <- subset(cpc_heatwave_days_month, month == 5)
may24_cpc <- subset(may24_cpc, year.x == 2024)
may24_cpc_ehf <- subset(may24_cpc, heatwave_ehisig_days >= 25)  # Adjusted to 31
may24_cpc_tx90 <- subset(may24_cpc, heatwave_TX90_days >= 25)    # Adjusted to 31
may24_cpc_tn90 <- subset(may24_cpc, heatwave_TN90_days >= 25)    # Adjusted to 31

# Count the number of days for each condition in CPC (May 2024)
ehf_days_may_2024_cpc <- nrow(may24_cpc_ehf)
tx90_days_may_2024_cpc <- nrow(may24_cpc_tx90)
tn90_days_may_2024_cpc <- nrow(may24_cpc_tn90)

# Calculate percentages for CPC (May 2024)
ehf_pct_may_2024_cpc <- (ehf_days_may_2024_cpc / coor_cpc) * 100
tx90_pct_may_2024_cpc <- (tx90_days_may_2024_cpc / coor_cpc) * 100
tn90_pct_may_2024_cpc <- (tn90_days_may_2024_cpc / coor_cpc) * 100
ehf_pct_may_2024_cpc
tx90_pct_may_2024_cpc 
tn90_pct_may_2024_cpc                           

###### HEATWAVE INTENSITY #####
merged_df_era5_2$month <- month(merged_df_era5_2$time)
merged_df_cpc_2$month <- month(merged_df_cpc_2$time)
### ERA5 ####
# The mean event intensity calculated as the exceedance above the 90th percentile; 
# (For EHF, the average excess heat felt during all heatwaves)

merged_df_era5_intensity <- merged_df_era5_2 %>%
  mutate(
    intensity_TX90 = tmax.x-TX90,   # TX90 exceedance
    intensity_TN90 =  tmin.x-TN90   # TN90 exceedance
  )

# Compute the average intensity for each heatwave definition
df_heatwave_avg_intensity_era5_month <- merged_df_era5_intensity %>%
  group_by(coordinate_id.x, lon, lat, year.x, month) %>%
  summarise(
    avg_event_intensity_TX90 = mean(intensity_TX90[heatwave_TX90 == 1], na.rm = TRUE),
    avg_event_intensity_TN90 = mean(intensity_TN90[heatwave_TN90 == 1], na.rm = TRUE),
    avg_excess_heat_EHF = mean(ehf[heatwave_ehisig == 1], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(across(starts_with("avg_"), ~ifelse(is.nan(.), NA, .)))  # NaN vervangen door NA

### CPC data ####
#the mean event intensity calculated as the exceedance above the 90th percentile; 
#(For EHF, the average excess heat felt during all heatwaves)

# Compute the intensities
merged_df_cpc_intensity <- merged_df_cpc_2 %>%
  mutate(
    intensity_TX90 = tmax.x-TX90,   # TX90 exceedance
    intensity_TN90 =  tmin.x-TN90   # TN90 exceedance
  )

# Compute the average intensity for each heatwave definition
df_heatwave_avg_intensity_cpc_month <- merged_df_cpc_intensity %>%
  group_by(coordinate_id.x, lon, lat, year.x, month) %>%
  summarise(
    avg_event_intensity_TX90 = mean(intensity_TX90[heatwave_TX90 == 1], na.rm = TRUE),
    avg_event_intensity_TN90 = mean(intensity_TN90[heatwave_TN90 == 1], na.rm = TRUE),
    avg_excess_heat_EHF = mean(ehf[heatwave_ehisig == 1], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(across(starts_with("avg_"), ~ifelse(is.nan(.), NA, .)))  # convert NaNs to NA

####  Choose Dataset and Metric ####

# Options: "era5" or "cpc"
dataset_to_use <- "era5"  # Change to "era5" as needed

# Options: "avg_excess_heat_EHF", "avg_event_intensity_TX90", "avg_event_intensity_TN90"
metric_to_plot <- "avg_event_intensity_TN90"  # Change as needed

# Assign the dataset
intensity_data_month <- if (dataset_to_use == "era5") {
  df_heatwave_avg_intensity_era5_month
} else {
  df_heatwave_avg_intensity_cpc_month
}

####  Filter and Format ####

intensity_data_month_filtered <- intensity_data_month %>%
  filter(year.x >= 2000, year.x <= 2024) %>%
  mutate(
    year = factor(year.x, levels = 2000:2024),
    month = factor(month, levels = 1:12, labels = month.abb)
  )

####  Plot Facet Map (Year Rows × Month Columns) ####

pdf("C:\\Users\\User\\Documents\\HONDURAS HEAT\\Maps, graphs & tables\\Month\\Tx90cpc.pdf", 
    width = 8.27, height = 11.69)

ggplot() +
  geom_tile(data = intensity_data_month_filtered, 
            aes(x = lon, y = lat, fill = .data[[metric_to_plot]]), alpha = 0.9) +
  geom_sf(data = Honduras_shp_era5, fill = NA, color = "black", size = 0.3) +
  scale_fill_gradientn(
    colours = c("#313695", "#74add1", "#ffffbf", "#f46d43", "#a50026"),
    values = scales::rescale(c(0, 0.5, 1.5, 2.5, 4, 6.25), from = c(0, 6.25)),
    limits = c(0, 6.25),
    name = "Heatwave Intensity (in °C²)",
    na.value = "white"
  ) +
  coord_sf(xlim = c(-89.4, -83.2), ylim = c(13, 16), expand = FALSE) +
  theme_minimal() +
  facet_grid(rows = vars(year), cols = vars(month)) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.spacing = unit(0.3, "lines"),
    strip.text = element_text(size = 6),
    legend.position = "bottom"
  ) +
  labs(
    title = paste("Monthly Heatwave Intensity (", metric_to_plot, ") from ", toupper(dataset_to_use), sep = ""),
    x = NULL,
    y = NULL
  )

dev.off()


######## HONDURAS & CITIES #########
# Also for TX90 & TN90
###### HEATWAVE DAYS #####
#### ERA5 ####
df_hw_days_era5 <- merged_df_era5 %>%
  group_by(coordinate_id.x, lon, lat,year.x) %>%
  summarise(
    heatwave_ehisig_days = sum(heatwave_ehisig, na.rm = TRUE),
    heatwave_TX90_days = sum(heatwave_TX90, na.rm = TRUE),
    heatwave_TN90_days = sum(heatwave_TN90, na.rm = TRUE),
    .groups = "drop"
  )

# Summarize the data for Honduras by year
Honduras <- merged_df_era5 %>%
  group_by(year.x) %>%
  summarise(
    heatwave_ehisig_days = sum(heatwave_ehisig, na.rm = TRUE)/1008,
    heatwave_TX90_days = sum(heatwave_TX90, na.rm = TRUE)/1008,
    heatwave_TN90_days = sum(heatwave_TN90, na.rm = TRUE)/1008,
    .groups = "drop"
  )

ERA5_Tegucigalpa <- subset(df_hw_days_era5 , coordinate_id.x == 840 & lon == -87.2 & lat == 14.1)
ERA5_SanPedroSula <- subset(df_hw_days_era5 , coordinate_id.x == 137 & lon == -88 & lat == 15.5)
ERA5_LaCeiba <- subset(df_hw_days_era5 , coordinate_id.x == 20 & lon == -86.8 & lat == 15.8)
ERA5_Choluteca <- subset(df_hw_days_era5 , coordinate_id.x == 986)

# Add location labels to each data frame
ERA5_Tegucigalpa <- ERA5_Tegucigalpa %>%
  mutate(location = "Tegucigalpa")
ERA5_SanPedroSula <- ERA5_SanPedroSula %>%
  mutate(location = "San Pedro Sula")
ERA5_LaCeiba <- ERA5_LaCeiba %>%
  mutate(location = "La Ceiba")
ERA5_Choluteca <- ERA5_Choluteca %>%
  mutate(location = "Choluteca")
Honduras <- Honduras %>%
  mutate(location = "Honduras") %>%
  mutate(
    coordinate_id.x = NA,
    lon = NA,
    lat = NA
  ) # add missing columns to match others

# Combine all into one data frame
era5_heatwave_days_temporal_df <- bind_rows(
  Honduras,
  ERA5_Tegucigalpa,
  ERA5_SanPedroSula,
  ERA5_LaCeiba,
  ERA5_Choluteca
)

location_colors <- c(
  "Honduras" = "#36449c",   # Example color for Honduras
  "Tegucigalpa" = "#a4d0e4", # Example color for Tegucigalpa
  "San Pedro Sula" = "#fce499", # Example color for San Pedro Sula
  "La Ceiba" = "#fcb76e",    # Example color for La Ceiba
  "Choluteca" = "#ca282b"    # Example color for Choluteca
)

# Ensure that 'location' is a factor with the desired order
era5_heatwave_days_temporal_df$location <- factor(era5_heatwave_days_temporal_df$location, 
                                                  levels = c("Honduras", "Tegucigalpa", "San Pedro Sula", "La Ceiba", "Choluteca"))

# Function to plot each heatwave definition for ERA5
output_dir <- "C:/Users/User/Documents/HONDURAS HEAT/Maps, graphs & tables/Cities"

# Function to plot and save each ERA5 heatwave variable
plot_and_save_era5_heatwave <- function(variable_name) {
  p <- ggplot(era5_heatwave_days_temporal_df, aes_string(x = "factor(year.x)", y = variable_name, color = "location", group = "location")) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs(
      title = paste("ERA5 Total", variable_name, "by Location and Year"),
      x = "Year",
      y = "Total Heatwave Days",
      color = "Location"
    ) +
    scale_color_manual(values = location_colors) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )
  
  # Save the plot as PDF
  ggsave(
    filename = paste0(output_dir, "/", variable_name, "_ERA5.pdf"),
    plot = p,
    width = 10,
    height = 6
  )
}

# Save each plot
plot_and_save_era5_heatwave("heatwave_ehisig_days")
plot_and_save_era5_heatwave("heatwave_TX90_days")
plot_and_save_era5_heatwave("heatwave_TN90_days")

#### CPC ####
df_hw_days_cpc <- merged_df_cpc %>%
  group_by(coordinate_id.x, lon, lat,year.x) %>%
  summarise(
    heatwave_ehisig_days = sum(heatwave_ehisig, na.rm = TRUE),
    heatwave_TX90_days = sum(heatwave_TX90, na.rm = TRUE),
    heatwave_TN90_days = sum(heatwave_TN90, na.rm = TRUE),
    .groups = "drop"
  )

# Summarize the data for Honduras by year
Honduras <- merged_df_cpc %>%
  group_by(year.x) %>%
  summarise(
    heatwave_ehisig_days = sum(heatwave_ehisig, na.rm = TRUE)/53,
    heatwave_TX90_days = sum(heatwave_TX90, na.rm = TRUE)/53,
    heatwave_TN90_days = sum(heatwave_TN90, na.rm = TRUE)/53,
    .groups = "drop"
  )

CPC_Tegucigalpa <- subset(df_hw_days_cpc, coordinate_id.x == 42 & lon == -87.25 & lat == 14.25)
CPC_LaCeiba <- subset(df_hw_days_cpc, coordinate_id.x == 6 & lon == -86.75 & lat == 15.75)
CPC_Choluteca <- subset(df_hw_days_cpc, coordinate_id.x == 55 & lon == -87.25 & lat == 13.25)
CPC_high <- subset(df_hw_days_cpc, coordinate_id.x == 38)

plot(CPC_high$year.x,CPC_high$heatwave_ehisig_days)
plot(CPC_high$year.x,CPC_high$heatwave_TN90_days)

plot_TN90_cpc <- ggplot(cpc_heatwave_days_temporal_df, aes(x = factor(year.x), y = heatwave_TN90_days, color = location, group = location)) +
  geom_line(size = 0.7) + geom_point(size = 1.2) +
  labs(title = "TN90 using CPC", x = "Year", y = "Days") +
  scale_color_manual(values = location_colors) + base_theme

# coordinates for SanPedroSula  because on the border
sps_coords <- c(3, 14)

# Filter and average the heatwave days for San Pedro Sula
CPC_SanPedroSula <- df_hw_days_cpc %>%
  filter(coordinate_id.x %in% sps_coords) %>%
  group_by(year.x) %>%
  summarise(
    heatwave_ehisig_days = mean(heatwave_ehisig_days, na.rm = TRUE),
    heatwave_TX90_days = mean(heatwave_TX90_days, na.rm = TRUE),
    heatwave_TN90_days = mean(heatwave_TN90_days, na.rm = TRUE),
    .groups = "drop"
  )

# Add location labels to each data frame
CPC_Tegucigalpa <- CPC_Tegucigalpa %>%
  mutate(location = "Tegucigalpa")
CPC_SanPedroSula <- CPC_SanPedroSula %>%
  mutate(location = "San Pedro Sula")
CPC_LaCeiba <- CPC_LaCeiba %>%
  mutate(location = "La Ceiba")
CPC_Choluteca <- CPC_Choluteca %>%
  mutate(location = "Choluteca")
Honduras <- Honduras %>%
  mutate(location = "Honduras") %>%
  mutate(
    coordinate_id.x = NA,
    lon = NA,
    lat = NA
  )

# Combine all into one data frame
cpc_heatwave_days_temporal_df <- bind_rows(
  Honduras,
  CPC_Tegucigalpa,
  CPC_SanPedroSula,
  CPC_LaCeiba,
  CPC_Choluteca
)

# Apply same color scheme as ERA5
location_colors <- c(
  "Honduras" = "#36449c",
  "Tegucigalpa" = "#a4d0e4",
  "San Pedro Sula" = "#fce499",
  "La Ceiba" = "#fcb76e",
  "Choluteca" = "#ca282b"
)

# Ensure 'location' is a factor with a consistent order
cpc_heatwave_days_temporal_df$location <- factor(
  cpc_heatwave_days_temporal_df$location,
  levels = c("Honduras", "Tegucigalpa", "San Pedro Sula", "La Ceiba", "Choluteca")
)

# Function to plot each heatwave definition for CPC
output_dir <- "C:/Users/User/Documents/HONDURAS HEAT/Maps, graphs & tables/Cities"

# Function to plot and save each variable
plot_and_save_cpc_heatwave <- function(variable_name) {
  p <- ggplot(cpc_heatwave_days_temporal_df, aes_string(x = "factor(year.x)", y = variable_name, color = "location", group = "location")) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs(
      title = paste("CPC Total", variable_name, "by Location and Year"),
      x = "Year",
      y = "Total Heatwave Days",
      color = "Location"
    ) +
    scale_color_manual(values = location_colors) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )
  
  # Save as PDF
  ggsave(
    filename = paste0(output_dir, "/", variable_name, "_CPC.pdf"),
    plot = p,
    width = 10,
    height = 6
  )
}

# Save each plot
plot_and_save_cpc_heatwave("heatwave_ehisig_days")
plot_and_save_cpc_heatwave("heatwave_TX90_days")
plot_and_save_cpc_heatwave("heatwave_TN90_days")

#### All 6 combined ####
# Define consistent theme for all plots
base_theme <- theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

# Create ERA5 plots
plot_ehisig_era5 <- ggplot(era5_heatwave_days_temporal_df, aes(x = factor(year.x), y = heatwave_ehisig_days, color = location, group = location)) +
  geom_line(size = 0.7) + geom_point(size = 1.2) +
  labs(title = "EHF using ERA5", x = "Year", y = "Days") +
  scale_color_manual(values = location_colors) + base_theme

plot_TX90_era5 <- ggplot(era5_heatwave_days_temporal_df, aes(x = factor(year.x), y = heatwave_TX90_days, color = location, group = location)) +
  geom_line(size = 0.7) + geom_point(size = 1.2) +
  labs(title = "TX90 using ERA5", x = "Year", y = "Days") +
  scale_color_manual(values = location_colors) + base_theme

plot_TN90_era5 <- ggplot(era5_heatwave_days_temporal_df, aes(x = factor(year.x), y = heatwave_TN90_days, color = location, group = location)) +
  geom_line(size = 0.7) + geom_point(size = 1.2) +
  labs(title = "TN90 using ERA5", x = "Year", y = "Days") +
  scale_color_manual(values = location_colors) + base_theme
plot_TN90_era5 
# Create CPC plots with consistent titles
plot_ehisig_cpc <- ggplot(cpc_heatwave_days_temporal_df, aes(x = factor(year.x), y = heatwave_ehisig_days, color = location, group = location)) +
  geom_line(size = 0.7) + geom_point(size = 1.2) +
  labs(title = "EHF using CPC", x = "Year", y = "Days") +
  scale_color_manual(values = location_colors) + base_theme

plot_TX90_cpc <- ggplot(cpc_heatwave_days_temporal_df, aes(x = factor(year.x), y = heatwave_TX90_days, color = location, group = location)) +
  geom_line(size = 0.7) + geom_point(size = 1.2) +
  labs(title = "TX90 using CPC", x = "Year", y = "Days") +
  scale_color_manual(values = location_colors) + base_theme

plot_TN90_cpc <- ggplot(cpc_heatwave_days_temporal_df, aes(x = factor(year.x), y = heatwave_TN90_days, color = location, group = location)) +
  geom_line(size = 0.7) + geom_point(size = 1.2) +
  labs(title = "TN90 using CPC", x = "Year", y = "Days") +
  scale_color_manual(values = location_colors) + base_theme

plot_TN90_cpc 

# Define the desired breaks
year_breaks <- as.character(seq(1980, max(as.numeric(levels(factor(era5_heatwave_days_temporal_df$year.x))), na.rm = TRUE), by = 5))

# Update each plot with discrete x-axis breaks
plot_ehisig_era5 <- plot_ehisig_era5 + scale_x_discrete(breaks = year_breaks) + ylim(0, 160)
plot_TX90_era5   <- plot_TX90_era5   + scale_x_discrete(breaks = year_breaks) + ylim(0, 160)
plot_TN90_era5   <- plot_TN90_era5   + scale_x_discrete(breaks = year_breaks) + ylim(0, 160)
plot_ehisig_cpc  <- plot_ehisig_cpc  + scale_x_discrete(breaks = year_breaks) + ylim(0, 160)
plot_TX90_cpc    <- plot_TX90_cpc    + scale_x_discrete(breaks = year_breaks) + ylim(0, 160)
plot_TN90_cpc    <- plot_TN90_cpc    + scale_x_discrete(breaks = year_breaks) + ylim(0, 160)

# Combine all 6 into one 3x2 layout
combined_plot <- ( plot_ehisig_cpc | plot_TX90_cpc | plot_TN90_cpc)/
  ( plot_ehisig_era5 | plot_TX90_era5 | plot_TN90_era5)+
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

combined_plot 



