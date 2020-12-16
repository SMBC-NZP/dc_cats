# setup -------------------------------------------------------------------

source('scripts/setup.R')

smartLibrary(
  c('unmarked', 'AICcmodavg','MuMIn', 'tidyverse'))

# Prepare camera data:

source('scripts/preparation/prepare_data.R')

# camera null model fitting ---------------------------------------------

# Detection formulas:

p_formulas <-
  c('~doy',
    '~tempHigh',
    '~dewHigh',
    '~doy + tempHigh',
    '~doy + dewHigh',
    '~tempHigh + dewHigh',
    '~tempHigh * dewHigh',
    '~doy + tempHigh * dewHigh',
    '~1')

# Fit the models:

p_mods_cam <-
  purrr::map(
    p_formulas,
    function(x){
      pcount(
        formula = as.formula(paste(x, '~1')),
        data = umf_cam,
        mixture = 'NB',
        K = 50)
    }) %>%
  set_names(p_formulas)

# Model selection table:

aictab(p_mods_cam)

# impervious surface -------------------------------------------------

# Define formulas:

imp_formulas <-
  c('~imp', '~imp + I(imp^2)', '~1')

# Fit the models:

imp_mods_cam <-
  purrr::map(
    imp_formulas,
    function(x){
      pcount(
        formula = as.formula(paste('~dewHigh', x)),
        data = umf_cam,
        mixture = 'NB',
        K = 50)
    }) %>%
  set_names(imp_formulas)

# Model selection table:

aictab(imp_mods_cam)

# human demography, exploring quadratic terms -----------------------------

# Define formulas:

hDem_formulas <-
  c(
    '~1',
    # One variable:
    '~income',
    '~hDensity',
    '~edu',
    # Two variables:
    '~income + hDensity',
    '~income + edu',
    '~hDensity + edu',
    # Three variables:
    '~income + hDensity + edu',
    # With quadratic hDensity:
    '~hDensity + I(hDensity^2)',
    '~income + hDensity + I(hDensity^2)',
    '~hDensity + I(hDensity^2) + edu',
    '~income + hDensity + I(hDensity^2) + edu')

# Fit the models:

hDem_mods_cam <-
  purrr::map(
    hDem_formulas,
    function(x){
      pcount(
        formula = as.formula(paste('~dewHigh', x)),
        data = umf_cam,
        mixture = 'NB',
        K = 50)
    }) %>%
  set_names(hDem_formulas)

# Model selection table:

aictab(hDem_mods_cam)

# Compare models with quadratic housing density term vs. linear only:

aictab(hDem_mods_cam) %>%
  as_tibble() %>% 
  filter(str_detect(Modnames, 'hDensity')) %>%
  mutate(
    var = 'hDensity',
    mods = case_when(
      str_detect(Modnames, 'hDensity\\^2') ~ 'quad',
      TRUE ~ 'linear'
    )) %>%
  group_by(var, mods) %>%
  summarize(
    cumWt = sum(AICcWt)) %>%
  spread(mods, cumWt)

# human demographics, reduced ---------------------------------------------

# Define formulas:

hDem_formulas_reduced_cam <-
  hDem_formulas[
    str_detect(hDem_formulas, 'hDensity\\^2')|
      !str_detect(hDem_formulas, 'hDensity')]

# Fit the models:

hDem_mods_reduced_cam <-
  purrr::map(
    hDem_formulas_reduced_cam,
    function(x){
      pcount(
        formula = as.formula(paste('~dewHigh', x)),
        data = umf_cam,
        mixture = 'NB',
        K = 50)
    }) %>%
  set_names(hDem_formulas_reduced_cam)

aictab(hDem_mods_reduced_cam)


# write model outputs for plotting predictions ----------------------------

write_rds(
  imp_mods_cam,
  'output/models/imp_mods_cam.rds')

write_rds(
  hDem_mods_reduced_cam,
  'output/models/hDem_mods_reduced_cam.rds')


