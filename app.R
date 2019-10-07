# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

source("R/modules/button-indicator.R")
source("R/modules/get-study-names.R")
source("R/modules/upload-documentation.R")

pkgload::load_all()
options( "golem.app.prod" = TRUE)
dccvalidator::run_app() # add parameters here (if any)
