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

  ## Initial titles for report boxes; try to populate ASAP
  callModule(results_boxes_server, "Validation Results", results = NULL)

  ## Synapse client for a specific user
  syn <- synapse$Synapse()
  ## Set client endpoints to staging, if needed
  if (!get_golem_config("production")) {
    set_staging_endpoints(syn)
  }

  ## Log into Synapse
  ## Assume local .synapseConfig if running locally with run_app()
  if(interactive()) {
    attempt_login(syn)
    if(!logged_in(syn)) {
      show_modal(
        title = "Not logged in",
        message = "You are not logged in. If you are running locally, check that you have a .synapseConfig with your credentials." # nolint
      )
    }
  } else {
    ## If not locally running, kick off the OAuth process and try to log in
    params <- parseQueryString(isolate(session$clientData$url_search))
    access_token <- oauth_process(params)
    attempt_login(syn, authToken = access_token)
    if(!logged_in(syn)) {
      show_modal(
        title = "Not logged in",
        message = "Something went wrong with the log in process."
      )
    }
  }

  observe({

    ## Check if user is in AMP-AD Consortium team (needed in order to create
    ## folder at the next step), and if they are a certified user.
    user <- syn$getUserProfile()
    membership <- check_team_membership(
      teams = get_golem_config("teams"),
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
          parent = get_golem_config("parent"),
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
