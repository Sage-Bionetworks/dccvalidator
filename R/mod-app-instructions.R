#' @title Get markdown as HTML
#'
#' @description Knit a Rmarkdown file and return it as HTML.
#'
#' @noRd
#' @param markdown_path Path to the markdown file
get_markdown <- function(markdown_path) {
  HTML(markdown::markdownToHTML(
    rmarkdown::render(
      input = glue::glue(
        tools::file_path_sans_ext(markdown_path), ".Rmd"
      ),
      quiet = TRUE,
      output_format = "md_document"
    ),
    stylesheet = app_sys("www/custom.css")
  ))
}
