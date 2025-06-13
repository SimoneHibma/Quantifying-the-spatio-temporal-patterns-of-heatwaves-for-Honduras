# Quantifying the spatio temporal patterns of heatwaves for Honduras
The scripts for spatio-temporal analysis of heatwaves for Honduras.
# Project Description
For my MSc thesis at ITC Faculty, University of Twente, I analyzed heatwaves in Honduras, looking at spatial difference, at trends, and differences over time.
For this I used two datasets and three heatwave definitions.  
  
The three heatwave definitions include:  
•	Excess Heat Factor (EHF)  
•	the 90th percentile of the maximum temperature (TX90)   
•	the 90th percentile of the minimum temperature (TN90)  
  
The two datasets are global gridded data: the CPC Global Unified Temperature and the ERA5-Land data, which were chosen as I found they were closest to weather station data. 
  
For each of these 6 combinations heatwave characteristics are calculated:     
•	Days  
•	Events  
•	Duration  
•	Yearly longest duration  
•	Magnitude  
•	Intesity  
•	Yearly most intense   
•	% of land affected  
  
The results are trend analysis, spatial and temporal patterns.   
Spatial: Maps of average for 1979-2024  
Trend: Maps with results of Mann-Kendall test & Sen’s Slope  
Temporal: Interannual maps for heatwave days and intensity and % of land affected per year   

# Data
The data is pubblically available and can be found on the following websites.  
CPC Global Unified Temperature : https://psl.noaa.gov/data/gridded/data.cpc.globaltemp.html  
ERA5-Land : https://doi.org/10.24381/cds.e9c9c792  

# Scripts
The data is netcdf files per year which are first clipped and combined for CPC and ERA5-Land seperately.
The heatwave defintions are calculated and the average for 1979-2024 of each heatwave definition is calculated for CPC and ERA5-Land,  
this script also contains the script for graphs over-time per coordinate & the overlap between heatwave defintions (both in the Appendix of the thesis). 
The scripts for the trend-analysis, interannual maps, % of land affected are not seperate based on data (CPC and ERA5-Land).   
  
The order of using the scripts is as follows:  
-  clip & combine (CPC and ERA5-Land seperately)  
-  Heatwave_definition_avg_hw_characteritics (CPC and ERA5-Land seperately)   
-  Trend-analysis  
-  %land & severity  
-  Temporal_graphs
  
The input and output paths need to be mannually changed, and the first 2 scripts need to be ran until the merged_df_cpc and merged_df_era5.   
In the script what is done is also descriped and headings are made for reasability. 
