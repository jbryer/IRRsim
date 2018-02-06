library(devtools)

document()
build(vignettes = FALSE)
build_vignettes()
install()
check(cran = TRUE)

library(IRRsim)

vignette(package = 'IRRsim') # Make sure the vignette is listed
vignette('IRRsim') # Open the vignette

IRRsim_demo() # Run the shiny app
