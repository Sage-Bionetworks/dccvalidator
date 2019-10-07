# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

source("R/Modules/button-indicator.R")
source("R/Modules/get-study-names.R")
source("R/Modules/upload-documentation.R")

pkgload::load_all()
options( "golem.app.prod" = TRUE)
dccvalidator::run_app() # add parameters here (if any)
