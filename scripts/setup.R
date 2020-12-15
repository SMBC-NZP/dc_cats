# Function searches packages in installed package list, installs them if they are not present, and loads the library:

smartLibrary <-
  function(packageVector) {
    for (i in 1:length(packageVector)) {
      package <- packageVector[i]
      if (!package %in% rownames(installed.packages())) {
        install.packages(packageVector[i],
                         repos = "http://cran.rstudio.com/",
                         dependencies = TRUE)
      }
    }
    lapply(packageVector, library, character.only = TRUE)
  }


# Function that scales variables:

scaleVar <-
  function(var) {
    (var - mean(var, na.rm = TRUE)) / sd(var, na.rm = TRUE)
  }

# Function to scale a simulated variable based on scaling a measured var:

scale_newVar <-
  function(newVar, var) {
    (newVar - mean(var, na.rm = TRUE))/sd(var, na.rm = TRUE)
  }

# Function to unscale var to new variable:

unscaleVar_new <-
  function(newVar_scaled, var){
    sd(var, na.rm = TRUE)*newVar_scaled + mean(var, na.rm = TRUE)
  }

# Plot theme to be used across output:

plot_theme <-
  function() {
    theme(
      title = element_text(size = rel(1.1)),
      axis.title.x = element_text(size = rel(1), margin = margin(15, 0, 0, 0)),
      axis.title.y = element_text(size = rel(1), margin = margin(0, 15, 0, 0)),
      axis.text = element_text(size = rel(0.7)),
      panel.grid.minor = element_blank(),
      plot.margin = margin(10, 10, 10, 10),
      panel.background = element_rect(fill = 'white'),
      panel.grid.major = element_blank(),#line(color = 'gray80', size = .2),
      strip.background = element_rect(fill = 'white', color = 'white'),
      strip.text = element_text(size = rel(1.1), margin = margin(0, 0, 20, 0)),
      panel.border = element_blank(),
      axis.line.y = element_line(color = 'black'),
      axis.line.x = element_line(color = 'black')
    )
  }