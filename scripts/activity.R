# setup -------------------------------------------------------------------

source('scripts/setup.R')

# Read libraries:

lapply(
  c('activity', 'suncalc', 'lubridate', 'tidyverse'),
  require,
  character.only = TRUE
)

# Function to convert date_time to decimal hours:

get_time_hours <-
  function(dt) {
    require(lubridate)
    hour(dt) + minute(dt)/60
  }

# data --------------------------------------------------------------------

# Urban intensity, classified into low, medium, and high-intensity:

urban_classes <-
  read_csv('data/site_covs.csv') %>%
  transmute(
    site,
    urban_intensity =
      case_when(
        imp < 10 ~ 'low',
        imp >= 40 ~ 'high',
        TRUE ~ 'medium') %>% 
      factor(levels = c('low', 'medium', 'high')))

# Camera trap detections:

detections <-
  read_csv('data/cam_detections_dt.csv') %>% 
  transmute(
    site,
    cat,
    date_time = force_tz(date_time, 'EST'),
    date = lubridate::as_date(date_time),
    hr = get_time_hours(date_time))
  
# Get sunrise and sunset times for Washington DC:

sunrise_sunset <-
  suncalc::getSunlightTimes(
    date = unique(pull(detections, date)),
    lat = 38.9,
    lon = -77,
    keep = c('sunrise', 'sunset'),
    tz = 'EST') %>% 
  select(-c(lat, lon)) %>% 
  as_tibble()

# A list for comparisons between classes:

urban_comparisons <-
  list(
    c('low', 'medium'),
    c('low', 'high'),
    c('medium', 'high')) %>% 
  set_names(c('low-medium', 'low-high', 'medium-high'))


# Combine data and prepare for analysis:

cam_time_transformed <-
  detections %>%
  # Join sunrise and sunset times:
  left_join(sunrise_sunset, by = 'date') %>%
  # Join site-level urban intensity:
  left_join(urban_classes, by = 'site') %>% 
  # Subset to observations greater than 30 minutes apart:
  filter(
    !(difftime(date_time, lag(date_time), 'minutes') < 30 &
             cat == lag(cat))) %>% 
  # Convert time to decimal hours:
  mutate_at(
    vars(sunrise, sunset, date_time),
    ~ hour(.) + minute(.)/60) %>% 
  # Subset and day vs. night:
  transmute(
    urban_intensity,
    time_hr = date_time,
    time_clock = time_hr,
    sunrise,
    sunset,
    # Determine if an observation was in the day (0) or night (1):
    day_night = 
      ifelse(
        time_hr > sunrise & time_hr < sunset,
        0,
        1)) %>% 
  # Convert time to radians:
  mutate_at(
    vars(time_clock, sunrise, sunset),
    ~ ./24*2*pi) %>% 
  # Transform to solar time:
  mutate(
    time_solar =
      activity::transtime(
        time_clock, 
        anchor = sunrise, 
        type = 'single'))


# activity distributions --------------------------------------------------

# Fit circular density distributions:

fit_list <-
  purrr::map(
    c('low', 'medium', 'high'),
    ~ {
        filter(cam_time_transformed, urban_intensity == .) %>% 
        pull(time_solar) %>% 
        activity::fitact(sample = 'none')
    }) %>% 
  set_names(c('low', 'medium', 'high'))

# Compare distributions:

distribution_stats_list <-
  purrr::map(
    urban_comparisons,
    ~ activity::compareCkern(
      fit_list[[pluck(., 1)]], 
      fit_list[[pluck(., 2)]], 
      reps = 10000))

# activity distributions, plotting ----------------------------------------

# Set color palette for urban intensity levels:

color_blind_palette <-
  c('Low' = '#0072B2',
    'Medium' = '#E69F00',
    'High' = 'red')

# Plot distributions (Figure 4):

purrr::map_dfr(
  names(fit_list),
  function(urban_intensity) {
    as.data.frame(fit_list[[urban_intensity]]@pdf) %>% 
    mutate(urban_intensity = urban_intensity) %>% 
      as_tibble()
  }) %>% 
  mutate(
    time_of_day = x*24/2/pi,
    `Urban Intensity` = 
      factor(urban_intensity, 
             levels = c('low', 'medium', 'high'),
             labels = c('Low', 'Medium', 'High'))) %>% 
  ggplot(
    aes(x = time_of_day,
        y = y, 
        color = `Urban Intensity`,
        fill = `Urban Intensity`)) + 
  geom_line(size = 0.75) +
  geom_area(
    position = 'identity',
    alpha = 0.3,
    color = NA) +
  scale_fill_manual(values = color_blind_palette) + 
  scale_color_manual(values = color_blind_palette) +
  scale_x_continuous(
    limits = c(0, 24),
    expand = c(0,0),
    breaks = c(0, 6, 12, 18, 24)) +
  scale_y_continuous(
    limits = c(0, 0.5),
    expand = c(0,0),
    breaks = seq(0, 0.5, by = 0.1)) +
  labs(
    x = 'Time of day (hour)',
    y = 'Density') +
  theme_bw() +
  plot_theme()

# Save plot:

ggsave(
  'output/figure_4.png', 
  width = 7.5, 
  height = 3.8, 
  units = 'in')

# compare day vs night ----------------------------------------------------

# Summary of observed data:

day_night_summary <-
  cam_time_transformed %>% 
  # Calculate proportion of day detections for each class:
  group_by(urban_intensity) %>% 
  summarize(prop = sum(day_night)/n())

# Randomly assign day and night labels:

day_night_summary_resampled <-
  purrr::map_dfr(
    # For each class comparison:
    1:length(urban_comparisons),
    function(i) {
      purrr::map_dfr(
        # For each iteration:
        1:10000,
        function(j) {
          cam_time_transformed %>% 
            # Subset to two classes:
            filter(urban_intensity %in% urban_comparisons[[i]]) %>% 
            mutate(
              # Add labels for iteration and type of comparison:
              comparison = paste(urban_comparisons[[i]], collapse = '-'), 
              iteration = j,
              # Sample urban class labels:
              urban_intensity = sample(urban_intensity)) %>% 
            # Calculate proportion of day detections for each class:
            group_by(comparison, iteration, urban_intensity) %>% 
            summarize(prop = sum(day_night)/n())
        })
    })

# Observed difference between classes:

observed_difference <-
  map_dfr(
    1:length(urban_comparisons),
    function(i) {
      values <-
        purrr::map(
          1:2,
          function(j) {
            day_night_summary %>% 
              filter(urban_intensity == urban_comparisons[[i]][j]) %>% 
              pull(prop)
          })
      tibble(
        comparison = names(urban_comparisons[i]),
        observed_diff = values[[1]] - values[[2]])
    })

# Calculate sampled differences:

sampled_diff <-
  day_night_summary_resampled %>% 
  ungroup() %>% 
  pivot_wider(
    names_from = urban_intensity, 
    values_from = prop) %>% 
  mutate(
    diff = 
      case_when(
        comparison == 'low-medium' ~ low - medium,
        comparison == 'low-high' ~ low - high,
        comparison == 'medium-high' ~ medium - high
      )) %>% 
  left_join(observed_difference, by = 'comparison')

# P values:

sampled_diff %>% 
  group_by(comparison) %>% 
  mutate(pass = diff <= observed_diff) %>%
  group_by(comparison) %>% 
  summarize(sum(pass)/n())

