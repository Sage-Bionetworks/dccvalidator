# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

Sys.setenv(R_CONFIG_ACTIVE = "default") # Replace "default" with your config
pkgload::load_all()
options("golem.app.prod" = TRUE)
dccvalidator::run_app() # add parameters here (if any)
