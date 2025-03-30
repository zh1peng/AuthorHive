#' Generate a formatted title page
#'
#' @description Creates a title page string (ready to copy/paste into Word)
#' using the provided author data. It handles multiple affiliations,
#' co-first authors, and corresponding authors.
#' 
#' @param data A data frame with at least these columns:
#'   \code{FirstName}, \code{LastName}, \code{Rank}, \code{Correspondence},
#'   and one or more \code{AffiliationX} columns.
#' @param style A character string indicating the style (e.g., \"default\",
#'   \"APA\", \"Nature\"). Different styles can alter the formatting, order,
#'   or punctuation.
#' @param title Optional paper title to include at the top. If NULL, omitted.
#' @param co_first_footnote Logical. If TRUE, automatically detects authors
#'   with the same top \code{Rank} and marks them as co-first authors.
#' @return A character string representing the entire title page.
#' @export
#' @examples
#' authors <- data.frame(
#'   FirstName = c("Alice", "Bob"),
#'   MiddleName = c("M.", ""),
#'   LastName = c("Smith", "Johnson"),
#'   Degree = c("PhD", "MD"),
#'   Email = c("alice@example.com", "bob@example.com"),
#'   Rank = c(1, 1),  # co-first authors
#'   Correspondence = c(TRUE, FALSE),
#'   Affiliation1 = c("University of X, Dept. of Y", "University of X, Dept. of Y"),
#'   Affiliation2 = c(NA, "Company Z, Research Division")
#' )
#' generate_title_page(authors, style = "default", title = "Example Paper")
generate_title_page <- function(data,
                                style = c("default", "APA", "Nature"),
                                title = NULL,
                                co_first_footnote = TRUE) {
  style <- match.arg(style)
  
  # 1) Order authors by Rank, then by row
  data <- data[order(data$Rank, decreasing = FALSE), ]
  
  # 2) Identify unique affiliations
  aff_cols <- grep("^Affiliation", names(data), value = TRUE)
  # gather all non-NA affiliations
  all_affils <- unique(stats::na.omit(unlist(data[aff_cols])))
  
  # 3) Create an index for each unique affiliation
  affil_index <- seq_along(all_affils)
  
  # 4) Build the author name string with superscript references
  build_author_label <- function(row) {
    # Basic name
    name_parts <- c(row[["FirstName"]], row[["MiddleName"]], row[["LastName"]], row[["Degree"]])
    name_parts <- name_parts[!is.na(name_parts) & name_parts != ""]
    full_name <- paste(name_parts, collapse = " ")
    
    # Collect affiliations for this author
    row_affils <- unlist(row[aff_cols])
    row_affils <- row_affils[!is.na(row_affils)]
    
    # Map each affiliation to the index
    aff_nums <- sapply(row_affils, function(a) which(all_affils == a))
    aff_nums_str <- paste0(aff_nums, collapse = ",")
    
    # Mark corresponding author with a symbol (e.g. '*')
    corr_symbol <- if(isTRUE(row[["Correspondence"]])) "*" else ""
    
    # e.g. "Alice M. Smith PhD* ^1,2^"
    if (length(aff_nums) > 0) {
      paste0(full_name, corr_symbol, " ^", aff_nums_str, "^")
    } else {
      # No affiliations, just return name
      paste0(full_name, corr_symbol)
    }
  }
  
  author_lines <- apply(data, 1, build_author_label)
  
  # 5) If co_first_footnote = TRUE, detect co-first authors
  #    i.e. authors with the same minimal rank
  min_rank <- min(data$Rank, na.rm = TRUE)
  is_co_first <- sum(data$Rank == min_rank) > 1
  
  # 6) Build final string
  # Title
  txt_title <- if(!is.null(title)) paste0("# ", title, "\n\n") else ""
  
  # Authors
  txt_authors <- paste(author_lines, collapse = ", ")
  
  # Affiliations (footnotes)
  affil_lines <- mapply(function(i, aff) {
    paste0("^", i, "^ ", aff)
  }, affil_index, all_affils)
  txt_affils <- paste(affil_lines, collapse = "\n")
  
  # Mark co-first authors footnote
  co_first_text <- ""
  if (co_first_footnote && is_co_first) {
    co_first_text <- "\n\n[co-first] These authors contributed equally as first authors."
  }
  
  # Mark corresponding footnote
  corr_rows <- data[data$Correspondence == TRUE, ]
  corr_text <- ""
  if (nrow(corr_rows) > 0) {
    corr_text <- paste0("\n\n*Corresponding author(s): ",
                        paste0(corr_rows$Email, collapse = "; "))
  }
  
  # Style handling (placeholder for different formatting rules)
  # Could adjust punctuation, spacing, etc.
  if (style == "APA") {
    # e.g., we might remove the numeric footnotes or do different spacing
    # (For brevity, we won't implement big changes here)
    # Just a note: real APA styling can be more nuanced
  } else if (style == "Nature") {
    # e.g., different line breaks, disclaimers
  }
  
  # Combine all
  final_txt <- paste0(
    txt_title,
    txt_authors, "\n\n",
    txt_affils,
    co_first_text,
    corr_text,
    "\n"
  )
  
  return(final_txt)
}
