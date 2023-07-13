library(shiny)
library(bslib)
library(tidyverse)
library(httr)
library(reactable)

url_r <- Sys.getenv("url_r")
temp_file2 <- tempfile(fileext = ".xlsx")

ui <- page_navbar(
  title = "Aggregation and Boosting game",
  theme = bs_theme(bootswatch = "minty"), # set a Bootstrap theme
  layout_sidebar(
    sidebar = sidebar(
      title = "Inputs",
      numericInput("True", "True value", value = 0, min = 0),
      numericInput("ymin", "Max Y", value = 0, min = 0),
      numericInput("ymax", "Min Y", value = 0, min = 0),),
      actionButton("submit", "Get Data"),
      card(card_header(""),
               card_body(reactableOutput("dsummary")),
      card(card_header(""),
               card_body(plotOutput("graph"))

    )
)))

server <- function(input, output) {

the_data <- reactiveVal()
  
  observeEvent(input$submit, {
    req <- GET(
      url_r,
      authenticate(Sys.getenv("u2"), Sys.getenv("pw2")),
      write_disk(path = temp_file2)
    )
     dataset  <- readxl::read_excel(temp_file2, sheet = 1) |> 
     mutate(Your_group = case_when(Your_group == "group_1" ~ "1 Person",
                                   Your_group == "group_2" ~ "2 People",
                                   Your_group == "group_3" ~ "Many People"))

     the_data(dataset)
  })

  data_summary <- reactive({
    req(input$submit)
    the_data() |> 
    summarize(mean=mean(Your_guess), 
    .by=Your_group) 
  })

  output$dsummary <- renderReactable({
     reactable(data_summary())
  })

complete_data <- reactive({
    req(input$True)
    true  <- input$True
    true_val  <- tibble(Your_group = "True",
                    Your_guess = true)
    complete_data  <- bind_rows(data_summary(), true_val)
   return(complete_data)
  })


output$graph  <- renderPlot({
req(input$True)

complete_data() |> 
  ggplot(aes(Your_group, Your_guess, fill=Your_group)) +
  geom_point() 
  #ylim(input$ymin, input$ymax)
})
  
}

shinyApp(ui, server)

