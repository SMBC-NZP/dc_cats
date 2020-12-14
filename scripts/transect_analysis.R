# setup -------------------------------------------------------------------

source('scripts/setup.R')

smartLibrary(c('unmarked', 'AICcmodavg', 'MuMIn', 'tidyverse'))

source('scripts/prepare_data.R')

# probability density function --------------------------------------------

# Following vignette: https://rstudio-pubs-static.s3.amazonaws.com/221408_23c61679859e48e6bae0b9c5c2e48a92.html

# Determine which distance function best describes how counts vary by distance:

detection_functions <-
  c('halfnorm', 'hazard')

phi_formulas <- '~1'

p_formulas <- '~1'

formula_frame <-
  crossing(detection_functions, phi_formulas, p_formulas)

detection_mods <-
  purrr::map(
    1:nrow(formula_frame),
    function(x){
      gdistsamp(
        lambdaformula = '~ 1',
        phiformula = formula_frame[x,]$phi_formulas,
        pformula = formula_frame[x,]$p_formulas,
        data = umf_transect,
        keyfun = formula_frame[x,]$detection_functions,
        mixture = 'NB',
        K = 50,
        output = 'abund')
    }) %>%
  set_names(detection_functions)

# Model selection table:

aictab(detection_mods)

# Hazard distance function was best supported.

# availability and detection functions ------------------------------------

# Define formulas:

detection_functions <- 'hazard'

phi_formulas_trans <-
  c('~1', '~time')

p_formulas_trans <-
  c(
    '~1',
    '~time',
    '~temp',
    '~dew',
    '~time + temp',
    '~time + dew',
    '~temp + dew',
    '~temp * dew',
    '~time + temp + dew',
    '~time + temp * dew')

formula_frame <-
  crossing(detection_functions, phi_formulas_trans, p_formulas_trans)

# Fit models:

phi_p_mods_trans <-
  purrr::map(
    1:nrow(formula_frame),
    function(x){
      gdistsamp(
        lambdaformula = '~ 1',
        phiformula = formula_frame[x,]$phi_formulas_trans,
        pformula = formula_frame[x,]$p_formulas_trans,
        data = umf_transect,
        keyfun = formula_frame[x,]$detection_functions,
        mixture = 'NB',
        K = 50,
        output = 'abund')
    }) %>%
  set_names(
    str_c(
      formula_frame$detection_functions,
      formula_frame$phi_formulas_trans,
      formula_frame$p_formulas_trans,
      sep = ' '
    ))

# Model selection table:

aictab(phi_p_mods_trans)

# Density model (with best mod):

trans_phi_p_mods_density <-
  gdistsamp(
    lambdaformula = '~1',
    phiformula = '~time',
    pformula = ~1,
    data = umf_transect,
    keyfun = 'hazard',
    mixture = 'NB',
    K = 50,
    output = 'density',
    unitsOut = 'ha')

# impervious surface ------------------------------------------------------

# Define formulas:

imp_formulas <-
  c('~imp', '~imp + I(imp^2)', '~1')

# Fit models:

imp_mods_trans <-
  purrr::map(
    imp_formulas,
    function(x){
      gdistsamp(
        lambdaformula = x,
        phiformula = '~time',
        pformula = '~1',
        data = umf_transect,
        keyfun = 'hazard',
        mixture = 'NB',
        K = 50,
        output = 'abund')}) %>% 
  set_names(imp_formulas)

# Model selection table:

aictab(imp_mods_trans)

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

# Fit models:

hDem_mods_trans <-
  purrr::map(
    hDem_formulas,
    function(x){
      gdistsamp(
        lambdaformula = x,
        phiformula = '~time',
        pformula = '~1',
        data = umf_transect,
        keyfun = 'hazard',
        mixture = 'NB',
        K = 50,
        output = 'abund')}) %>% 
  set_names(hDem_formulas)

# Model selection table:

aictab(hDem_mods_trans)

# Compare models with quadratic housing density term vs. linear only:

linear_quadratic_comparison_trans <-
  aictab(hDem_mods_trans) %>%
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

hDem_formulas_reduced_trans <-
  hDem_formulas[
    str_detect(hDem_formulas, 'hDensity\\^2')|
      !str_detect(hDem_formulas, 'hDensity')]

# Fit models:

hDem_mods_reduced_trans <-
  purrr::map(
    hDem_formulas_reduced_trans,
    function(x){
      gdistsamp(
        lambdaformula = x,
        phiformula = '~time',
        pformula = '~1',
        data = umf_transect,
        keyfun = 'hazard',
        mixture = 'NB',
        K = 50,
        output = 'abund')}) %>% 
  set_names(hDem_formulas_reduced_trans)

# Model selection table:

hDem_mods_reduced_trans_density <-
  purrr::map(
    hDem_formulas_reduced_trans,
    function(x){
      gdistsamp(
        lambdaformula = x,
        phiformula = '~time',
        pformula = '~1',
        data = umf_transect,
        keyfun = 'hazard',
        mixture = 'NB',
        K = 50,
        output = 'density',
        unitsOut = 'ha')}) %>% 
  set_names(hDem_formulas_reduced_trans)

# Model selection table:

aictab(hDem_mods_reduced_trans)


# write model outputs for plotting predictions ----------------------------

write_rds(
  imp_mods_trans,
  'output/models/imp_mods_trans.rds')

write_rds(
  hDem_mods_reduced_trans_density,
  'output/models/imp_mods_reduced_trans.rds')
