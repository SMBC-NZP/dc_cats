# setup -------------------------------------------------------------------

source('scripts/setup.R')

smartLibrary(c('unmarked', 'tidyverse'))

options(stringsAsFactors = FALSE)

# prepare camera data -----------------------------------------------------

# Observation-level covariates

camera_observation_covs <- 
  read_csv('data/camera_observation_covs.csv') %>%
  arrange(site, date) %>%
  mutate(doy = date %>%
           strftime(format = '%j') %>%
           as.numeric) %>%
  mutate_at(
    c('tempHigh', 'dewHigh', 'doy'),
    ~ scaleVar(.)) %>%
  select(site, date, day, tempHigh, dewHigh, doy)

# Site-level covariates:

camera_site_covs <- 
  read_csv('data/site_covs.csv') %>%
  arrange(site) %>%
  filter(site %in% camera_observation_covs$site) %>% 
  mutate_at(vars(imp:income), scaleVar)

# Camera cat observations:

camera_detections <-
  camera_observation_covs %>%
  select(site, date, day) %>%
  distinct() %>%
  left_join(
    read_csv('data/cam_detections.csv') %>%
      mutate(date = as.Date(date_time)) %>%
      group_by(site, date) %>%
      summarize(count = length(unique(cat))),
    by = c('site', 'date')) %>% 
  mutate(count = replace_na(count, 0)) %>% 
  select(-date) %>%
  pivot_wider(names_from = day, values_from = count) %>%
  select(-site)

# Create an unmarkedFramePCount object for pcount

umf_cam <- 
  unmarked::unmarkedFramePCount(
    camera_detections,
    siteCovs = select(camera_site_covs, -site),
    obsCovs = select(camera_observation_covs, -c(site, date)))

# prepare transect data ---------------------------------------------------

# Site-level covariates:

covs_transect <-
  read_csv('data/site_covs.csv') %>%
  arrange(site) %>%
  mutate_at(vars(imp:income), ~ scaleVar(.))

# Transect sampling data:

observations_transect <- 
  read_csv('data/transect_detections.csv') %>%
  filter(
    !is.na(count),
    species == 'cat') %>%
  left_join(
    read_csv('data/sampling.csv') %>%
      filter(activity == 'transect'),
    by = c('site', 'visit')) %>%
  left_join(
    read_csv('data/transect_weather.csv'),
    by = c('site', 'date', 'time')) %>%
  mutate(
    visit = as.factor(visit),
    doy = as.numeric(strftime(date, format = '%j'))) %>%
  arrange(site)

# Create an unmarked frame of distance data for the transect counts:

umf_transect <- 
  formatDistData(
    data.frame(observations_transect), 
    distCol="distance",
    transectNameCol="site", 
    dist.breaks=seq(0, 50, by=5),
    occasionCol='visit')

# Create a dataframe of potential cat detection covariates by site:

transect_observation_covs <- 
  observations_transect %>%
  dplyr::select(-c(species:date)) %>%
  mutate(time = as.numeric(time)/60) %>%
  distinct %>%
  mutate_at(yCov_names, scaleVar)

# Create a list of wide-form detection covariates:

ySiteCovs <-
  purrr::map(
    c('time', 'temp','dew', 'doy'),
    function(covariate) {
      transect_observation_covs %>%
        select(site, visit, cov = covariate) %>%
        mutate(cov = scaleVar(cov)) %>%
        spread(visit, cov) %>%
        select(-site) %>%
        as.matrix()
    }) %>%
  set_names(c('time', 'temp','dew', 'doy'))

# Create unmarkedFrameGDS object for gdistsamp:

umf_transect <-
  unmarkedFrameGDS(
    y = as.matrix(catTransectUmf),
    siteCovs =  covs_transect %>%
      select(-site) %>%
      data.frame(),
    numPrimary = 6,
    yearlySiteCovs = ySiteCovs,
    survey = 'line',
    dist.breaks =  seq(0, 50, by = 5),
    tlength = rep(200, nrow(catTransectUmf)),
    unitsIn = 'm')
