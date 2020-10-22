## Example shiny app to create a plot from sortable inputs

library(shiny)
library(htmlwidgets)
library(sortable)
library(magrittr)
library(ggplot2)



File <- read.csv("C:/Users/aliri/Downloads/File1.csv", sep=";")

colnames_to_tags <- function(df){
  lapply(
    colnames(df),
    function(co) {
      tag(
        "p",
        list(
          class = class(df[, co]),
          tags$span(class = "glyphicon glyphicon-move"),
          tags$strong(co)
        )
      )
    }
  )
}


ui <- fluidPage(
  fluidRow(
    class = "panel panel-heading",
    div(
      class = "panel-heading",
      h3("Dragging variables to define a plot")
    ),
    fluidRow(
      class = "panel-body",
      column(
        width = 3,
        tags$div(
          class = "panel panel-default",
          tags$div(class = "panel-heading", "Variables"),
          tags$div(
            class = "panel-body",
            id = "sort1",
            colnames_to_tags(File)
          )
        )
      ),
      column(
        width = 3,
        #analyseasx
        tags$div(
          class = "panel panel-default",
          tags$div(
            class = "panel-heading",
            tags$span(class = "glyphicon glyphicon-stats"),
            "Analyze as x (drag here)"
          ),
          tags$div(
            class = "panel-body",
            id = "sort2"
          )
        ),
        
        #analyseasy
        tags$div(
          class = "panel panel-default",
          tags$div(
            class = "panel-heading",
            tags$span(class = "glyphicon glyphicon-stats"),
            "Analyze as y (drag here)"
          ),
          tags$div(
            class = "panel-body",
            id = "sort3"
          )
        )
        
      ),
      column(
        width = 6,
        verbatimTextOutput("plot")
        
      )
      
    )
  ),
  sortable_js(
    "sort1",
    options = sortable_options(
      group = list(
        name = "sortGroup1",
        put = TRUE
      ),
      sort = FALSE,
      onSort = sortable_js_capture_input("sort_vars")
    )
  ),
  sortable_js(
    "sort2",
    options = sortable_options(
      group = list(
        group = "sortGroup1",
        put = htmlwidgets::JS("function (to) { return to.el.children.length < 13; }"),
        pull = TRUE
      ),
      onSort = sortable_js_capture_input("sort_x")
    )
  ),
  sortable_js(
    "sort3",
    options = sortable_options(
      group = list(
        group = "sortGroup1",
        put = htmlwidgets::JS("function (to) { return to.el.children.length < 1; }"),
        pull = TRUE
      ),
      onSort = sortable_js_capture_input("sort_y")
    )
  )
)

server <- function(input, output) {
  output$variables <- renderPrint(input[["sort_vars"]])
  output$analyse_x <- renderPrint(input[["sort_x"]])
  output$analyse_y <- renderPrint(input[["sort_y"]])
  
  
  x <- reactive({
    x <- input$sort_x
    if (is.character(x)) x %>% trimws()
  })

  
  y <- reactive({
    input$sort_y %>% trimws()
  })
  
  output$plot <-
    renderPrint({
      validate(
        need(x(), "Drag a variable to x"),
        need(y(), "Drag a variable to y")
      )
      
      y <- File[c(paste(y()))]
      y1 <- colnames(y)
      
      x <- File[c(paste(x()))]
      x1 <- colnames(x)
      x2 <- c(paste(x1))

      
      summary(lm(paste(y1, " ~ ",paste(x2, collapse="+"),sep = ""), data=File))$adj.r.squared
    })
  

  
      
}
shinyApp(ui, server)

