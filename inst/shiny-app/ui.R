library(shiny)

fluidPage(
  titlePanel("AuthorHive Title Page Generator"),
  fileInput("author_file", "Upload Author Info CSV"),
  selectInput("style", "Journal Style", choices = c("default", "APA", "Nature")),
  actionButton("generate", "Generate"),
  verbatimTextOutput("titlepage")
)
