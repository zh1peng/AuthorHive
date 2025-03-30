install.packages(c("usethis", "devtools", "roxygen2"))

usethis::create_package("authordown")

usethis::use_git()


usethis::use_roxygen_md()


devtools::document()
#devtools::check()
# devtools::check(args="--no-tests --no-vignettes")
# devtools::check(args="--no-tests --no-vignettes --as-cran")
# styler::style_pkg()
devtools::build()
devtools::install()

usethis::use_github()
usethis::use_agpl3_license()

usethis::use_package("shiny")



generate_template() 

 authors <- data.frame(
   FirstName = c("Alice", "Bob"),
   LastName = c("Smith", "Johnson"),
   Conflict = c("No conflict", "Consultant at Company Z"))
 generate_conflict(authors)


authors <- data.frame(
FirstName = c("Alice", "Bob"),
  LastName = c("Smith", "Johnson"),
   Contribution = c("Conceptualization; Data curation", "Supervision; Writing - review")
)
generate_contribution(authors)


authors <- data.frame(
FirstName = c("Alice", "Bob"),
Acknowledgement = c("Thanks to funder A", "")
 )
generate_acknowledgement(authors)


html_path <- render_section_html("Conflict of Interest", generate_conflict, authors)
browseURL(html_path)