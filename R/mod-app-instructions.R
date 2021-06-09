#' @title Get markdown as HTML
#'
#' @description Knit a Rmarkdown file and return it as HTML.
#'
#' @noRd
#' @param markdown_path Path to the markdown file
get_markdown <- function(markdown_path) {
  HTML(markdown::markdownToHTML(
    knitr::knit(
      input = glue::glue(
        tools::file_path_sans_ext(markdown_path), ".Rmd"
      ),
      output = glue::glue(
        tools::file_path_sans_ext(markdown_path), ".md"
      ),
      quiet = TRUE
    ),
    stylesheet = app_sys("www/custom.css")
  ))
}
