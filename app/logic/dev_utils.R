# app/logic/dev_utils.R

box::use(
  shiny[HTML, tags],
)

#' @export
add_dev_headers <- function() {
  tags$head(
    tags$meta(http_equiv = "Cache-Control",
              content = "no-cache, no-store, must-revalidate"),
    tags$meta(http_equiv = "Pragma",
              content = "no-cache"),
    tags$meta(http_equiv = "Expires",
              content = "0")
  )
}
