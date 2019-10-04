# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

source("R/button-indicator-helpers.R")
source("R/get-study-names.R")

pkgload::load_all()
options( "golem.app.prod" = TRUE)
dccvalidator::run_app() # add parameters here (if any)
