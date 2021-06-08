#' App server
#'
#' Create the server-side component of the dccvalidator Shiny app.
#'
#' @import shiny
#' @import shinydashboard
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#' @return none
#' @export
#' @examples
#' \dontrun{
#' shinyApp(ui = app_ui, server = app_server)
#' }
app_server <- function(input, output, session) {
  syn <- synapse$Synapse()

  if (!config::get("production")) {
    set_staging_endpoints(syn)
  }
  session$sendCustomMessage(type = "readCookie", message = list())

  ## Show message if user is not logged in to synapse
  unauthorized <- observeEvent(input$authorized, {
    showModal(
      modalDialog(
        title = "Not logged in",
        HTML("You must log in to <a target=\"_blank\" href=\"https://www.synapse.org/\">Synapse</a> to use this application. Please log in, and then refresh this page.") # nolint
      )
    )
  })

  observeEvent(input$cookie, {
    is_logged_in <- FALSE
    ## Use authToken and handle error here if still not logged in
    tryCatch({
      syn$login()
      is_logged_in <- TRUE
    },
    error = function(err) {
      showModal(
        modalDialog(
          title = "Login error",
          HTML("There was an error with the login process. Please refresh your Synapse session by logging out of and back in to <a target=\"_blank\" href=\"https://www.synapse.org/\">Synapse</a>. Then refresh this page to use the application."), # nolint
          footer = NULL
        )
      )
    }
    )
    ## Check that user did not log in as anonymous
    if (syn$username == "anonymous") {
      showModal(
        modalDialog(
          title = "Login error",
          HTML("There was an error with the login process. You have been logged in as anonymous."), # nolint
          footer = NULL
        )
      )
      is_logged_in <- FALSE
    }
    req(is_logged_in)

    ## Check if user is in AMP-AD Consortium team (needed in order to create
    ## folder at the next step), and if they are a certified user.
    user <- syn$getUserProfile()
    membership <- check_team_membership(
      teams = config::get("teams"),
      user = user,
      syn = syn
    )
    certified <- check_certified_user(user$ownerId, syn = syn)
    report_unsatisfied_requirements(membership, certified, syn = syn)

    ## If user is a member of the team(s), create folder to save files and
    ## enable inputs
    if (inherits(membership, "check_pass") &
      inherits(certified, "check_pass")) {
      created_folder <- try(
        create_folder(
          parent = config::get("parent"),
          name = user$userName,
          synapseclient = synapse,
          syn = syn
        )
      )

      all_studies <- get_study_names(reactive(config::get("study_table")), syn)

      if (config::get("docs_tab")$include_tab &
        config::get("docs_tab")$include_upload_widget) {
        # Documentation server needs created_folder to run correctly
        callModule(
          upload_documents_server,
          "documentation",
          parent_folder = reactive(created_folder),
          study_names = all_studies,
          synapseclient = synapse,
          syn = syn
        )
      }
    }

      # Validation module
      callModule(
        validator_server,
        "validator",
        study_names = all_studies,
        species_list = config::get("species_list"),
        assay_templates = config::get("templates")$assay_templates,
        annotations_table = config::get("annotations_table"),
        annots_link = config::get("annotations_link"),
        templates_link = config::get("templates_link"),
        contact_email = config::get("contact_email"),
        parent = created_folder,
        synapseclient = synapse,
        syn = syn
      )
  })
}

#' Modal with next step
#'
#' If none of the checks inherit `check_fail`, then pop up a modal to tell
#' users what to do next.
#'
#' @param results List of conditions
#' @param email Contact email as a string
#' @noRd
next_step_modal <- function(results, email) {
  is_failure <- purrr::map_lgl(results, function(x) {
    inherits(x, "check_fail")
  })
  if (!any(is_failure)) {
    showModal(
      modalDialog(
        title = "Great work!",
        HTML(
          glue::glue(
            "Your validated file(s) had no failures. Please contact <a target=\"_blank\" href=\"{email}\">{email}</a> to proceed with the next step if you have validated all finalized metadata and manifest files at once. For multiple assays, please validate each assay with your other metadata files (individual and/or biospecimen) and manifest." # nolint
          )
        ),
        easyClose = TRUE
      )
    )
  }
}
