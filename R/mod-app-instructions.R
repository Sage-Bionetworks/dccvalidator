get_markdown <- function() {
  HTML(markdown::markdownToHTML(
    knitr::knit(input = glue::glue(
      tools::file_path_sans_ext(config::get("path_to_markdown")), ".Rmd"
    ),
    output = glue::glue(
      tools::file_path_sans_ext(config::get("path_to_markdown")), ".md"
    ),
    quiet = TRUE),
    stylesheet = system.file("app/www/custom.css", package = "dccvalidator")
    )
    )
}
