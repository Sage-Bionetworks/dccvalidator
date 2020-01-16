Sys.setenv(R_CONFIG_ACTIVE = "shinytest")
pkgload::load_all()
dccvalidator::run_app()