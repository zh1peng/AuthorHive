#' Generate all sections (title page, acknowledgement, conflict, contribution)
#'
#' @description A convenience wrapper that calls the various section generators
#' and combines them into one text block. Ideal for quick copy-paste into Word.
#'
#' @param data A data frame with the columns needed by each function:
#'   \code{generate_title_page}, \code{generate_acknowledgement},
#'   \code{generate_conflict}, \code{generate_contribution}.
#' @param title Optional paper title to pass to \code{generate_title_page()}.
#' @param style Title page style (\"default\", \"APA\", \"Nature\").
#' @return A character string containing all sections.
#' @export
#' @examples
#' \dontrun{
#' authors <- read.csv("authordown_template.csv")
#' cat(authordown(authors, title = "My Great Paper", style = "default"))
#' }
authordown <- function(data, title = NULL, style = "default") {
  
  # 1) Title page
  tp <- generate_title_page(
    data = data,
    style = style,
    title = title,
    co_first_footnote = TRUE
  )
  
  # 2) Acknowledgements
  ack <- generate_acknowledgement(data, style = "paragraph")
  
  # 3) Conflict of interest
  coi <- generate_conflict(data)
  
  # 4) Contributions
  contrib <- generate_contribution(data, list_style = TRUE)
  
  # Combine all
  final_text <- paste(
    tp,
    "\n\n", ack,
    "\n\n", coi,
    "\n\n", contrib,
    sep = ""
  )
  
  return(final_text)
}
