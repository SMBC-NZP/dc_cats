# Estimating cat abundance in metropolitan Washington DC using camera traps and transect sampling
Code and data for Bennett et al. Domestic Cat Abundance and Activity Across a Residential Land Use Gradient (in review)

## data

- cam_detections.csv: The location and date-time of camera trap detections
- camera_observation_covs.csv: Weather and site data associated with camera sampling events (<i>Note: All weather data were obtained from Weather Underground using the nearest weather station to a given site</i>)
- imp_DC.grd & imp_DC.gri: raster data of impervious surface for Greater Washington D.C. (<i>Note: Data were derived from https://www.mrlc.gov/</i>)
- sampling.csv: All sampling activity for the project
- site_covs.csv: Land cover and US Census data associated with study sites
- transect_detections.csv: Count data from transect sampling
- transect_weather.csv: Weather data associated with transect sampling (<i>Note: All weather data were obtained from Weather Underground using the nearest weather station to a given site</i>)
  
## scripts
- setup.R: Functions used across analyses
- ~/preparation
  - prepare_data.R: Generate unmarked frames for abundance/density analyses
  - prepare_site_covs.R: Gather site-level Census (from API) and impervious surface (imp_DC) data
  - sampling_time.R: Generate sampling schedule
- ~/analysis
  - activity_analysis.R: Analyze the daily activity of cats
  - camera_analysis.R: Estimate abundance from camera trap data
  - transect_analysis.R: Estimate abundance and density from transect sampling data
  - predictions.R: Predict abundance and density for camera trap and transect models

