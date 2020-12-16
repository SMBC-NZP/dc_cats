# Estimating cat abundance in metropolitan Washington DC using camera traps and transect sampling
Code and data for Domestic Cat Abundance and Activity Across a Residential Land Use Gradient (in review)

Authors: Kevin F.P. Bennett, Brian S. Evans, J. Alan Clark, and Peter P. Marra

## Abstract
Free-ranging domestic cats are a detriment to wildlife and humans by preying on native species and transmitting disease. As a result, removing free-ranging cats from the landscape has become a conservation and public health priority. Estimating cat population size with an unbiased sampling design, however, especially in human-dominated areas, is logistically challenging and rarely done. The lack of robust cat population sampling limits our understanding of where cats pose risks, which is important for evaluating management strategies, such as trap-remove or trap-neuter-return. We hypothesized that cat abundance and activity both depend on human land use and demographics. Using a network of sites participating in a community science program, we conducted transect and camera trap surveys to test predictions of cat population abundance and activity across a gradient of residential land use intensity. Both sampling methods determined that cat abundance was greatest in areas with intermediate human population density and lower educational attainment. Transect data also provided evidence that cat abundance was greatest at intermediate levels of impervious surface cover, while data from camera traps also showed that cat abundance was positively associated with household income. Using counts of cats observed on cameras, we found that the timing of cat activity varied depending the degree of urban intensity. Cats were more strictly nocturnal in medium and high intensity residential land-use areas, possibly because a greater proportion of these cats are unowned. These results suggest that transect surveys conducted during the day may undercount cats in urban environments where unowned free-roaming cats predominate. Taken together, our results highlight the importance of incorporating human demographics, land use patterns, and urban context in estimating the abundance of free-ranging cats to better inform management decisions and improve conservation outcomes.

## Files

### ~/data
- cam_detections.csv: The location and date-time of camera trap detections
- camera_observation_covs.csv: Weather and site data associated with camera sampling events (<i>Note: All weather data were obtained from Weather Underground using the nearest weather station to a given site</i>)
- imp_DC.grd & imp_DC.gri: raster data of impervious surface for Greater Washington D.C. (<i>Note: Data were derived from https://www.mrlc.gov/</i>)
- sampling.csv: All sampling activity for the project
- site_covs.csv: Land cover and US Census data associated with study sites
- transect_detections.csv: Count data from transect sampling
- transect_weather.csv: Weather data associated with transect sampling (<i>Note: All weather data were obtained from Weather Underground using the nearest weather station to a given site</i>)

### ~/output
- models: abundance models
- figures: Figures 1 - 5 from the manuscript
  
### ~/scripts
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

