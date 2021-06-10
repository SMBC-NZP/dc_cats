sites <-
  read_csv('data/site_covs.csv') %>% 
  transmute(
    site_name = site, 
    site = stringr::str_c('site_', row_number()))

purrr::map(
  list.files('data', pattern = '.csv', full.names = TRUE),
  function(file_path) {
    read_csv(file_path) %>% 
      rename(site_name = site) %>% 
      left_join(sites, by = 'site_name') %>% 
      select(site, !site_name) %>%
      write_csv(file_path)
  })