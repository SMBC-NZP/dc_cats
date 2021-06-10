# setup -------------------------------------------------------------------

smartLibrary(
  c(
    'tigris',
    'acs',
    'stringr',
    'leaflet',
    'rgdal',
    'sp',
    'raster',
    'tidyverse'
  ))

# get spatial tigerline data ----------------------------------------------

# Add api key for census data (must add your own api key!):

api.key.install(key = 'your key here')

# Get census tracts for each state:

tractsAll <-
  purrr::map(
    c('DC', 'MD', 'VA'),
    function(x){
      tracts(x)
    }
  ) %>%
  rbind_tigris()


# add census data to tracts -----------------------------------------------

# For more variables see: http://www2.census.gov/geo/tiger/TIGER_DP/2014ACS/Metadata/BG_METADATA_2014.txt

# Define census tables to read:

census_tables <-
  tibble(
    metric = c('income', 'population', 'edu'),
    census_code = c('B19013', 'B01003', 'B01002', 'B15003', 'B12001'))

# Read in the census data:

census_raw <-
  purrr::map(
    1:nrow(census_tables),
    function(i){
      purrr::map_dfr(
        c('DC', 'VA', 'MD'),
        function(x){
          census_acs <-
            acs.fetch(
              endyear = 2012, 
              span = 5,
              geography = geo.make(
                state = x,
                county = '*',
                tract = '*'),
              table.number = census_tables[i,]$census_code,
              col.names = "pretty")
          
          tibble(
            GEOID = paste0(
              str_pad(census_acs@geography$state, 2, "left", pad="0"),
              str_pad(census_acs@geography$county, 3, "left", pad="0"),
              str_pad(census_acs@geography$tract, 6, "left", pad="0"))) %>%
            bind_cols(
              as_tibble(census_acs@estimate)
            )
        })
    }) %>%
  set_names(census_tables$metric)

# Wrangle census data, keeping variables of interest:

census <-
  bind_rows(
    census_raw$income %>%
      transmute(
        GEOID = GEOID,
        metric = 'income',
        value = unlist(.[,2])),
    census_raw$population %>%
      left_join(
        tractsAll@data %>%
          dplyr::select(GEOID, ALAND),
        by = 'GEOID'
      ) %>%
      # Calculate human population density:
      transmute(
        GEOID = GEOID,
        metric = 'hDensity',
        # Convert area to hectares
        value = unlist(.[,2])/(as.numeric(ALAND)*1E-6)), 
    census_raw$edu %>%
      transmute(
        GEOID = GEOID,
        metric = 'edu',
        # Subset to education columns of interest (20:26 for Associate or 
        # higher) then calculate the proportion (2 = total population)
        value = rowSums(.[,20:26])/unlist(.[,2]) 
      )) %>%
  filter(value >= 0) %>%
  spread(metric, value)

# Merge census data with geographic dataset:

census_spatial <-
  geo_join(
    tractsAll, 
    census, 
    by = 'GEOID')

# extract census data to cat sampling points ------------------------------

# Get survey locations:

# Please note that this code reads a three column data frame where column
# one is the site name and columns two and three are the longitude and
# latitude (decimal degrees), respectively. Location data have been
# masked to avoid sharing PII.

survey_sites <-
  read_csv('site_locations.csv') %>%
  SpatialPointsDataFrame(
    .[, 2:3],
    data = .,
    proj4string = CRS('+init=epsg:4326')
  ) %>%
  spTransform(projection(census_spatial))

# Get data at survey locations:

covs_census <-
  bind_cols(
    survey_sites@data,
    over(survey_sites, census_spatial) %>%
      dplyr::select(edu:income))

# subset census data to area around cat sampling points -------------------

# Get tracts associated with survey locations:

geoIds <-
  over(survey_sites, census_spatial) %>%
  pull(GEOID) %>%
  unique()

# Crop to the extent of sampled census tracts:

census_subset <-
  crop(
    census_spatial, 
    base::subset(
      census_spatial,
      GEOID %in% geoIds) %>%
      extent())


# add impervious surface data and write to file  --------------------------

covs_census %>%
  mutate(
    # Extract impervious surface data to sites (100 m buffer):
    imp = 
      extract(
        raster('data/raw_data/imp_DC'),
        read_csv('data/raw_data/sites.csv') %>%
          SpatialPointsDataFrame(
            .[, 2:3],
            data = .,
            proj4string = CRS('+init=epsg:4326')
          ),
        buffer = 100,
        fun = mean,
        na.rm = TRUE
      )) %>%
  dplyr::select(site, imp, edu:income) %>% 
  # Write to file:
  write_csv('data/site_covs.csv')

