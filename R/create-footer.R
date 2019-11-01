#' Create app footer
#'
#' The app footer shows a link to the issue tracker and a contact link.
#'
#' @keywords internal
#' @param email Email address to contact
#' @return A footer tag
create_footer <- function(email) {
  tags$footer(
    p(
      a(href = "https://github.com/Sage-Bionetworks/dccvalidator/issues", "Report an issue"), # nolint
      " | ",
      a(href = glue::glue("mailto:{email}"), "Contact us")
    ),
    align = "left"
  )
}
