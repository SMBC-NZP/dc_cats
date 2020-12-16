# setup -------------------------------------------------------------------

options(stringsAsFactors = FALSE)

source('scripts/setup.R')

source('scripts/preparation/prepare_data.R')


# get model output and site covs ------------------------------------------

# Model output:

imp_mods_cam <-
  read_rds('output/models/imp_mods_cam.rds')

imp_mods_trans <-
  read_rds('output/models/imp_mods_trans.rds')

hDem_mods_reduced_cam <-
  read_rds('output/models/hDem_mods_reduced_cam.rds')

hDem_mods_reduced_trans <-
  read_rds('output/models/hDem_mods_reduced_trans.rds')

hDem_mods_reduced_trans_density <-
  read_rds('output/trans_hDem_mods_reduced_density.rds')
 
# predictions, no covs ----------------------------------------------------

# No covariates, cam:

predict(
  hDem_mods_reduced_cam$`~1`, 
  type = 'state') %>% 
  distinct()

# No covariates, transect:

predict(
  hDem_mods_reduced_trans$`~1`,
  type = 'lambda') %>% 
  distinct()

# No covariates, transect density:

predict(
  hDem_mods_reduced_trans_density$`~1`,
  type = 'lambda') %>% 
  distinct()

# new data ----------------------------------------------------------------

# Site covs:

site_covs <-
  list(
    transect = read_csv('data/site_covs.csv'),
    camera = 
      read_csv('data/site_covs.csv') %>% 
      filter(
        site %in% read_csv('data/camera_observation_covs.csv')$site)) %>%
  purrr::map(~select(., -site))


# Scale covariate values for site covs:

new_data <-
  purrr::map(
  site_covs,
  function(x) {
    purrr::map_dfc(
      names(x),
      function(y) {
        scale_newVar(
          seq(min(x[,y]), max(x[,y]), length = 100), 
          var = unlist(x[,y]))
      }) %>% 
      set_names(names(site_covs[[1]]))
  })

# Prepare data frames for predictions:

new_data_by_covariate <-
  purrr::map(
    names(new_data),
    function(x) {
      purrr::map_dfr(
        names(new_data$transect),
        function(covariate) {
          mutate_at(
            new_data[[x]],
            vars(-one_of(covariate)),
            ~ 0) %>% 
            mutate(target = covariate) %>% 
            select(target, everything()) %>% 
            mutate(
              var_unscaled = 
                unscaleVar_new(
                  .[[covariate]], 
                  site_covs[[x]][[covariate]]))
        })
    }) %>% 
  set_names(names(new_data))

# plot impervious predictions ---------------------------------------------
# Code for Figure 1 in the manuscript

# Transect predictions:

imp_transect <-
  bind_cols(
    new_data_by_covariate %>% 
      pluck('transect') %>% 
      filter(target == 'imp'),
    gdistsamp(
      lambdaformula = as.formula('~imp + I(imp^2)'),
      phiformula = ~time,
      pformula = ~1,
      data = umf_transect,
      keyfun = 'hazard',
      mixture = 'NB',
      K = 50,
      output = 'abund') %>%
      predict(
        type = 'lambda',
        newdata = 
          new_data_by_covariate %>% 
          pluck('transect') %>% 
          filter(target == 'imp') %>% 
          as.data.frame())) %>% 
  mutate(method = 'Transect')

# Camera predictions:

imp_cam <-
  new_data_by_covariate$camera %>% 
  filter(target == 'imp') %>% 
  bind_cols(
    predict(
      imp_mods_cam$`~imp + I(imp^2)`,
      type = 'state',
      newdata = as.data.frame(.))) %>% 
  mutate(method = 'Camera trap')

# Combine predictions:

imp_predictions <-
  bind_rows(imp_transect,imp_cam)

# Set different axis limits for the panels:

blank_data <-
  tibble(
    method = c(
      rep('Camera trap', 2),
      rep('Transect', 2)),
    x = 0,
    y = c(0, 6, 0, 40))

# Plot the data:

impPlot <- 
  imp_predictions %>% 
  mutate(
    x = var_unscaled, 
    y = Predicted) %>%
  ggplot(
    aes(x = x, y)) +
  geom_line() +
  geom_ribbon(
    aes(ymin = lower, ymax = upper), 
    alpha = 0.1) +
  scale_x_continuous(
    limits = c(0, 80),
    breaks = seq(0, 80, by = 10),
    expand = c(0,0)) +
  theme_bw() +
  labs(
    x = 'Proportion of impervious surface within 100 m (%)',
    y = 'Predicted abundance'
  ) +
  geom_blank(
    data = blank_data,
    aes(x = x, y = y)) +
  facet_wrap(~ method, scales = 'free') +
  expand_limits(y = 0) + scale_y_continuous(expand = c(0, 0)) +
  plot_theme()

# Save the plot:

ggsave(
  'figure_1.png',
  plot = impPlot,
  path = 'output/figures/',
  width = 7, 
  height = 3.5,
  units = 'in')


# plot model betas --------------------------------------------------------
# Code for Figure 2 in the manuscript

# Rewrite model formulas, quadratics I(x^2) to x2:

formulas <-
  hDem_formulas[
    str_detect(hDem_formulas, 'hDensity\\^2')|
      !str_detect(hDem_formulas, 'hDensity')] %>%
  str_replace_all('I\\(hDensity\\^2\\)', 'hq')

# Create a new unmarkedFramePCount object for pcount:

camUmfWithCovs_new <- 
  unmarkedFramePCount(
    data.frame(umfCam[, -1]),
    siteCovs = read_csv('data/site_covs.csv') %>%
      arrange(site) %>%
      filter(site %in% camera_observation_covs$site) %>%
      mutate(
        iq = imp^2, 
        hq = hDensity^2) %>%
      mutate_at(vars(imp:hq), scaleVar) %>%
      select(-site),
    obsCovs = data.frame(camera_observation_covs) %>%
      select(-c(site, date))
  )

# Create a new unmarkedFrameGDS object for gdistsamp:

gUmfWithCovs_new <-
  unmarkedFrameGDS(
    y = as.matrix(catTransectUmf),
    siteCovs =  read_csv('data/site_covs.csv') %>%
      arrange(site) %>%
      mutate(
        iq = imp^2, 
        hq = hDensity^2) %>%
      mutate_at(vars(imp:hq), scaleVar) %>%
      select(-site) %>%
      data.frame(),
    numPrimary = 6,
    yearlySiteCovs = ySiteCovs,
    survey = 'line',
    dist.breaks =  seq(0, 50, by = 5),
    tlength = rep(200, nrow(catTransectUmf)),
    unitsIn = 'm'
  )

# Fit models, impervious:

imp_cam_new <-
  purrr::map(
    c('~imp', '~imp + iq', '~1'),
    function(x){
      pcount(
        formula = as.formula(paste('~dewHigh', x)),
        data = camUmfWithCovs_new,
        mixture = 'NB',
        K = 50)
    }) %>%
  set_names(c('~imp', '~imp + iq', '~1'))


imp_trans_new <-
  purrr::map(
    c('~imp', '~imp + iq', '~1'),
    function(x){
      gdistsamp(
        lambdaformula = x,
        phiformula = '~time',
        pformula = '~1',
        data = gUmfWithCovs_new,
        keyfun = 'hazard',
        mixture = 'NB',
        K = 50,
        output = 'abund')}) %>% 
  set_names(c('~imp', '~imp + iq', '~1'))

# Fit models, human density:

hDem_cam_new <-
  purrr::map(
    formulas,
    function(x){
      pcount(
        formula = as.formula(paste('~dewHigh', x)),
        data = camUmfWithCovs_new,
        mixture = 'NB',
        K = 50)
    }) %>%
  set_names(formulas)


hDem_trans_new <-
  purrr::map(
    formulas,
    function(x){
      gdistsamp(
        lambdaformula = x,
        phiformula = '~time',
        pformula = '~1',
        data = gUmfWithCovs_new,
        keyfun = 'hazard',
        mixture = 'NB',
        K = 50,
        output = 'abund')}) %>% 
  set_names(formulas)

# Generate a nested list of models

mods_list <-
  list(
    imp = list(
      cam = imp_cam_new,
      transect = imp_trans_new),
    hDem = list(
      cam = hDem_cam_new,
      transect = hDem_trans_new))

var_method <- 
  crossing(
    mod = 'hDem',
    method = c('cam', 'transect'),
    var = c('hDensity', 'hq', 'income', 'edu')
  ) %>%
  bind_rows(
    crossing(
      mod = 'imp',
      method = c('cam', 'transect'),
      var = c('imp', 'iq')))

# Get model-averaged betas:

beta_table <-
  purrr::map_dfr(
    1:nrow(var_method),
    function(i){
      mod <- var_method[i,]$mod
      method <- var_method[i,]$method
      var <- var_method[i,]$var
      mod_list <- 
        mods_list[[mod]][[method]]
      modavg_out <-
        modavg(
          mod_list,
          parm = var,
          parm.type = 'lambda')
      tibble(
        mod,
        method,
        var = var,
        beta = modavg_out$Mod.avg.beta,
        se = modavg_out$Uncond.SE,
        lower = modavg_out$Lower.CL,
        upper = modavg_out$Upper.CL)
    })

beta_table_for_plot <-
  beta_table %>%
  mutate(
    method = factor(
      method,
      levels = c('cam', 'transect'),
      labels = c('Camera trap', 'Transect')
    ),
    var = factor(
      var,
      levels = c(
        'income',
        'edu',
        'hq',
        'hDensity',
        'iq',
        'imp'),
      labels = c(
        'Income',
        'Education',
        'Human density^2',
        'Human density',
        'Impervious^2',
        'Impervious')))


beta_plot <-
  beta_table_for_plot %>%
  ggplot(aes(x = beta, y = var)) +
  geom_vline(aes(xintercept = 0), linetype = 'dashed', color = '#aeaeae') +
  geom_segment(
    aes(x = beta-se,
        xend = beta+se,
        y = var,
        yend = var), 
    size = 1.25,
    lineend = 'round') +
  geom_segment(
    aes(x = lower,
        xend = upper,
        y = var,
        yend = var),
    lineend = 'round') +
  geom_point(size = 2.75) +
  facet_wrap(~ method, scales = 'free_x') +
  labs(
    x = expression(paste(beta, ' coefficient')),
    y  = 'Covariate') +
  scale_y_discrete(
    labels = c(
      'Impervious^2' = expression(Impervious^2),
      'Human density^2' = expression(paste('Human ',density^2)))
  ) +
  theme_bw() +
  plot_theme() +
  theme(
    strip.background = element_blank(),
    axis.text = element_text(size = rel(1.05)),
    axis.title = element_text(size = rel(1.2)),
    strip.text = element_text(size = rel(1.2)),
    panel.border = element_rect(color =  '#000000', fill = NA))

# Save the plot:

ggsave(
  'figure_2.png',
  plot = beta_plot,
  path = 'output/figures/',
  width = 6, 
  height = 4.5,
  units = 'in')


# predictions for human demography models, transect -----------------------
# Code for Figure 3

# Predict transect models:

hDensity_transect <-
  new_data_by_covariate$transect %>% 
  filter(target == 'hDensity') %>% 
  bind_cols(
    predict(
      gdistsamp(
        lambdaformula = as.formula('~hDensity + I(hDensity^2)'),
        phiformula = ~time,
        pformula = ~1,
        data = umf_transect,
        keyfun = 'hazard',
        mixture = 'NB',
        K = 50,
        output = 'abund'),
      type = 'lambda',
      newdata = 
        as.data.frame(
          new_data_by_covariate$transect %>% 
            filter(target == 'hDensity'))
    ))

edu_transect <-
  new_data_by_covariate$transect %>% 
  filter(target == 'edu') %>% 
  bind_cols(
    predict(
      gdistsamp(
        lambdaformula = as.formula('~edu'),
        phiformula = ~time,
        pformula = ~1,
        data = umf_transect,
        keyfun = 'hazard',
        mixture = 'NB',
        K = 50,
        output = 'abund'),
      type = 'lambda',
      newdata = 
        as.data.frame(
          new_data_by_covariate$transect %>% 
            filter(target == 'edu'))
    ))

# Combine frames:

hDem_transect <-
  bind_rows(
    hDensity_transect,
    edu_transect
  )


# Blank data to define panel limits:

# Set different axis limits for the panels:


blank_data <-
  tibble(
    target_label = 
      c(rep("Education~('%'~college)",2),
        rep("People~per~km^{2}", 2)) %>% 
      as.factor(),
    x = c(50, 100, 0, 13500),
    y = rep(c(0, 20), 2))


# Transect plot:

human_demography_transect_plot <-
  bind_rows(
    hDensity_transect,
    edu_transect) %>%
  select(target, var_unscaled:upper) %>% 
  mutate(
    x = case_when(
      target == 'edu' ~ var_unscaled*100,
      target == 'income' ~ var_unscaled/1000,
      TRUE ~ var_unscaled),
    y = Predicted,
    upper = ifelse(upper > 15, 15, upper),
    target_label = factor(
      target, 
      levels = c('edu', 'hDensity'),
      labels = c(
        "Education~('%'~college)",
        "People~per~km^{2}"
      ))) %>%
  ggplot(aes(x, y)) +
  geom_blank(
    data = blank_data,
    aes(x = x, y = y)) +
  geom_line(size = .3) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 15)) +
  theme_bw() +
  labs(
    x = 'Covariate value',
    y = 'Predicted abundance'
  ) +
  facet_wrap(
    ~target_label, 
    scales = 'free', 
    labeller = label_parsed) +
  plot_theme() +
  theme(
    strip.background = element_rect(fill = NA, color = NA),
    panel.ontop = TRUE,
    panel.border = element_blank(),
    axis.line = element_line(),
    # panel.border = element_rect(fill = NA, color = '#000000'),
    panel.background = element_rect(fill = NA),
    strip.text.y = element_text(margin = margin(l = 5)),
    strip.text.x = element_text(margin = margin(b = 5)))


ggsave(
  'figure_3.png',
  plot = human_demography_transect_plot,
  path = 'output/figures/',
  width = 6, 
  height = 3,
  units = 'in')


# predictions for human demography models, camera -------------------------
# Code for Figure 4

# Predictions:

predictions_cam <-
  bind_rows(
    new_data_by_covariate$camera %>% 
      filter(target == 'income') %>% 
      bind_cols(
        predict(
          hDem_mods_reduced_cam$`~income`,
          type = 'state',
          newdata = as.data.frame(.)
        )),
    new_data_by_covariate$camera %>% 
      filter(target == 'hDensity') %>% 
      bind_cols(
        predict(
          hDem_mods_reduced_cam$`~hDensity + I(hDensity^2)`,
          type = 'state',
          newdata = as.data.frame(.)
        )),
    new_data_by_covariate$camera %>% 
      filter(target == 'edu') %>% 
      bind_cols(
        predict(
          hDem_mods_reduced_cam$`~edu`,
          type = 'state',
          newdata = as.data.frame(.)
        )))



# Blank data to define panel limits:

# Set different axis limits for the panels:

blank_data <-
  tibble(
    target_label = 
      c(rep("Education~('%'~college)",2),
        rep("People~per~km^{2}", 2),
        rep("Income~(thousands~of~'$')", 2)) %>% 
      as.factor(),
    x = c(50, 100, 0, 14000, 50, 250),
    y = rep(c(0, 5), 3))


# Camera plot:

human_demography_cam_plot <-
  predictions_cam %>% 
  mutate(
    x = case_when(
      target == 'edu' ~ var_unscaled*100,
      target == 'income' ~ var_unscaled/1000,
      TRUE ~ var_unscaled),
    y = Predicted,
    upper = ifelse(upper > 5, 5, upper),
    target_label = factor(
      target, 
      levels = c('edu', 'hDensity', 'income'),
      labels = c(
        "Education~('%'~college)",
        "People~per~km^{2}",
        "Income~(thousands~of~'$')"
      ))) %>%
  ggplot(aes(x, y)) +
  geom_blank(
      data = blank_data,
      aes(x = x, y = y)) +
  geom_line(size = .3) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 5)) +
  theme_bw() +
  labs(
    x = 'Covariate value',
    y = 'Predicted abundance'
  ) +
  facet_wrap(
    ~target_label, 
    scales = 'free', 
    labeller = label_parsed) +
  plot_theme() +
  theme(
    strip.background = element_rect(fill = NA, color = NA),
    panel.ontop = TRUE,
    panel.border = element_blank(),
    axis.line = element_line(),
    panel.background = element_rect(fill = NA),
    strip.text.y = element_text(margin = margin(l = 5)),
    strip.text.x = element_text(margin = margin(b = 5)))

ggsave(
  'figure_4.png',
  plot = human_demography_cam_plot,
  path = 'output/figures/',
  width = 7.5, 
  height = 2.75,
  units = 'in')