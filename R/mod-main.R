#' @title Main dccvalidator UI
#'
#' @description Create the main UI and server components of the dccvalidator
#' Shiny app.
#'
#' @export
#' @import shiny
#' @import shinydashboard
#' @param id The module id.
mod_main_ui <- function(id) {
  ns <- NS(id)
  dashboardPage(
    dashboardHeader(title = "Metadata Validation"),
    dashboardSidebar(
      sidebarMenu(
        if (!is.na(get_golem_config("path_to_markdown"))) {
          menuItem("Using the App", tabName = ns("vignette"))
        },
        if (get_golem_config("docs_tab")$include_tab) {
          menuItem(
            get_golem_config("docs_tab")$tab_name,
            tabName = ns("documentation")
          )
        },
        menuItem("Validator", tabName = ns("validator"))
      ),
      create_footer(get_golem_config("contact_email"))
    ),
    dashboardBody(

      # Add resources in www
      golem_add_external_resources(),

      # Use shinyjs
      shinyjs::useShinyjs(),

      # Make a list of the tabItems; this is a workaround
      # for a problem with tabItems and shinyDashboard
      tags$div(
        list(
          # Embedd How To Use App vignette
          if (!is.na(get_golem_config("path_to_markdown"))) {
            tabItem(
              tabName = ns("vignette"),
              get_markdown(get_golem_config("path_to_markdown"))
            )
          },
          # Validator UI
          validator_ui(
            id = ns("validator"),
            species_list = get_golem_config("species_list"),
            assay_templates = get_golem_config("templates")$assay_templates,
            include_biospecimen_type = get_golem_config(
              "include_biospecimen_type"
            )
          ),
          # Documentation tab UI
          if (get_golem_config("docs_tab")$include_tab) {
            docs_options <- get_golem_config("docs_tab")
            upload_documents_ui(
              id = ns("documentation"),
              markdown_path = docs_options$path_to_docs_markdown,
              include_widget = docs_options$include_upload_widget
            )
          }
        ),
        class = "tab-content"
      )
    )
  )
}

#' @title Main dccvalidator server
#'
#' @rdname mod_main_ui
#' @export
#' @import shiny
#' @import shinydashboard
#' @param id The module ID.
#' @param syn Synapse client object.
mod_main_server <- function(id, syn) {
  moduleServer(
    id,
    function(input, output, session) {
      shiny::req(inherits(syn, "synapseclient.client.Synapse") & logged_in(syn))
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

        all_studies <- get_study_names(get_golem_config("study_table"), syn)

        if (get_golem_config("docs_tab")$include_tab &
          get_golem_config("docs_tab")$include_upload_widget) {
          # Documentation server needs created_folder to run correctly
          callModule(
            upload_documents_server,
            "documentation",
            parent_folder = created_folder,
            study_names = all_studies,
            synapseclient = synapse,
            syn = syn
          )
        }
        # Validation module
        callModule(
          validator_server,
          "validator",
          study_names = all_studies,
          species_list = get_golem_config("species_list"),
          assay_templates = get_golem_config("templates")$assay_templates,
          annotations_table = get_golem_config("annotations_table"),
          annots_link = get_golem_config("annotations_link"),
          templates_link = get_golem_config("templates_link"),
          contact_email = get_golem_config("contact_email"),
          include_biospecimen_type = get_golem_config(
            "include_biospecimen_type"
          ),
          parent = created_folder,
          synapseclient = synapse,
          syn = syn
        )
      }
    }
  )
}
