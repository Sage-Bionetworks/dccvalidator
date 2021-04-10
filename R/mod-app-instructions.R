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
    stylesheet = system.file("app/www/custom.css", package = "dccvalidator")
  ))
}
