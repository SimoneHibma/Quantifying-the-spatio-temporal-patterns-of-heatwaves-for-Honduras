library(lubridate)
library(ncdf4)
library(dplyr)
library(terra)
library(data.table)
library(openxlsx)

###### TMAX ######
# Define directories
shapefile_path <- "C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\Honduras.shp"
data_dir <- "C:\\Users\\User\\Documents\\HONDURAS HEAT\\ERA5-Land\\tmax\\"
output_nc <- "C:\\Users\\User\\Documents\\HONDURAS HEAT\\ERA5-Land\\tmax\\merged_ERA5_tmax.nc"

# Load shapefile
shape <- vect(shapefile_path)

# Temporary storage for yearly files
yearly_files <- c()

for (year in 1979:2024) {
  yearly_rasters <- c()  # Initialize empty raster list
  
  for (month in 1:12) {
    file_path <- file.path(data_dir, paste0("era5_", month, "-", year, "_tmax.nc"))
    
    if (file.exists(file_path)) {
      r <- try(rast(file_path), silent=TRUE)  # Load safely
      
      if (inherits(r, "SpatRaster")) {
        r <- mask(r, shape)  # Apply mask
        yearly_rasters <- c(yearly_rasters, r)  # Add to list
      } else {
        message("Skipping invalid file: ", file_path)
      }
    } else {
      message("Missing file: ", file_path)
    }
  }
  
  # Merge monthly rasters & save yearly output
  if (length(yearly_rasters) > 0) {
    yearly_stack <- do.call(c, yearly_rasters)
    year_output <- file.path(data_dir, paste0("yearly_tmax_", year, ".nc"))
    
    writeCDF(yearly_stack, year_output, overwrite=TRUE)  # Save immediately
    yearly_files <- c(yearly_files, year_output)  # Store for final merge
    
    message("Processed & saved year: ", year)
  }
}

# Final merge of yearly files
if (length(yearly_files) > 0) {
  final_rasters <- lapply(yearly_files, rast)  # Load all yearly outputs
  final_stack <- do.call(c, final_rasters)  # Merge yearly outputs
  writeCDF(final_stack, output_nc, overwrite=TRUE)  # Save final NetCDF
  
  message("Final merged NetCDF saved at: ", output_nc)
} else {
  message("No valid data to merge.")
}

###### TMIN ######
# Define directories
shapefile_path <- "C:\\Users\\User\\Documents\\HONDURAS HEAT\\CPC_data\\Honduras.shp"
data_dir <- "C:\\Users\\User\\Documents\\HONDURAS HEAT\\ERA5-Land\\tmin\\"
output_nc <- "C:\\Users\\User\\Documents\\HONDURAS HEAT\\ERA5-Land\\tmin\\merged_ERA5_tmin.nc"

# Load shapefile
shape <- vect(shapefile_path)

# Temporary storage for yearly files
yearly_files <- c()

for (year in 1979:2024) {
  yearly_rasters <- c()  # Initialize empty raster list
  
  for (month in 1:12) {
    file_path <- file.path(data_dir, paste0("era5_", month, "-", year, "_tmin.nc"))
    
    if (file.exists(file_path)) {
      r <- try(rast(file_path), silent=TRUE)  # Load safely
      
      if (inherits(r, "SpatRaster")) {
        r <- mask(r, shape)  # Apply mask
        yearly_rasters <- c(yearly_rasters, r)  # Add to list
      } else {
        message("Skipping invalid file: ", file_path)
      }
    } else {
      message("Missing file: ", file_path)
    }
  }
  
  # Merge monthly rasters & save yearly output
  if (length(yearly_rasters) > 0) {
    yearly_stack <- do.call(c, yearly_rasters)
    year_output <- file.path(data_dir, paste0("yearly_tmin_", year, ".nc"))
    
    writeCDF(yearly_stack, year_output, overwrite=TRUE)  # Save immediately
    yearly_files <- c(yearly_files, year_output)  # Store for final merge
    
    message("Processed & saved year: ", year)
  }
}

# Final merge of yearly files
if (length(yearly_files) > 0) {
  final_rasters <- lapply(yearly_files, rast)  # Load all yearly outputs
  final_stack <- do.call(c, final_rasters)  # Merge yearly outputs
  writeCDF(final_stack, output_nc, overwrite=TRUE)  # Save final NetCDF
  
  message("Final merged NetCDF saved at: ", output_nc)
} else {
  message("No valid data to merge.")
}

###### Combine to Excel file ######
# Define file paths
nc_tmax_file <- "C:\\Users\\User\\Documents\\HONDURAS HEAT\\ERA5-Land\\tmax\\merged_ERA5_tmax.nc"
nc_tmin_file <- "C:\\Users\\User\\Documents\\HONDURAS HEAT\\ERA5-Land\\tmin\\merged_ERA5_tmin.nc"
output_file <- "D:\\HONDURAS HEAT\\ERA5-Land\\Temp_ERA5_1979-2024.csv"

# Open NetCDF files
nc_tmax <- nc_open(nc_tmax_file)
nc_tmin <- nc_open(nc_tmin_file)

# Get time, longitude, and latitude
start_date <- as.Date("1979-01-01")
end_date <- as.Date("2024-12-31")
date_list <- seq.Date(from = start_date, to = end_date, by = "day")

lon <- ncvar_get(nc_tmax, "longitude")
lat <- ncvar_get(nc_tmax, "latitude")

# Extract Tmax and Tmin variables
tmax <- ncvar_get(nc_tmax, "merged_ERA5_tmax")  # Change variable name if needed
tmin <- ncvar_get(nc_tmin, "merged_ERA5_tmin")  # Change variable name if needed

# Close NetCDF files
nc_close(nc_tmax)
nc_close(nc_tmin)

# Create a structured data frame
data <- expand.grid(lon = lon, lat = lat, time = date_list) %>%
  mutate(tmax = as.vector(tmax),
         tmin = as.vector(tmin))

# Remove leap day (Feb 29)
data <- data %>% filter(format(time, "%m-%d") != "02-29")

# Compute mean temperature
data$mean <- rowMeans(data[, c("tmax", "tmin")], na.rm = TRUE)

# Kelvin to Celsius
data$tmax <- data$tmax-273.15
data$tmin <- data$tmin-273.15
data$mean <- data$mean-273.15

# Save the data to an Excel file
fwrite(data, output_file)
message("Data saved to: ", output_file)

