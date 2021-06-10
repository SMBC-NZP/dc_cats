# This script was used to generate sampling times for each site.

library(tidyverse)

# Get sites:

catSites <-
  read_csv('data/site_covs.csv') %>%
  arrange(site) %>%
  select(site)

# Generate a list based on the number of months:

samplingList <-
  purrr::map(
    # Number of months:
    1:3,
    function(x){
    # Cat sites with month number:
    catSites %>%
      mutate(month = x) %>%
      # Random site generation:
      sample_frac() %>%
      mutate(
        sampleOrder = row_number())
  })

# Resample months 2 and 3 such that sites are not visited within 14
# sampling periods:

for(i in 2:length(samplingList)){
  samplingList[[i]] <- 
    samplingList[[i-1]] %>%
    group_by(sampleOrder >= max(sampleOrder) - 14) %>%
    sample_frac() %>%
    ungroup %>%
    mutate(sampleOrder =  row_number()) %>%
    select(site:sampleOrder)}

bind_rows(samplingList)


