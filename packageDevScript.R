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

testthat::
