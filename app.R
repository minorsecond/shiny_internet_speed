# My subscription is for 100Mbps down, 10  Mbps up
#
rm(list = ls())
library(lubridate)
library(Cairo)
library(shiny)
library(shinydashboard)
library(RPostgreSQL)
library(ggplot2)
library(gridExtra)
#library(keyringr)

#source("global.R")
plots <- readRDS("plots.Rds")
mydashboardHeader <- function(..., title = NULL, disable = FALSE,title.navbar=NULL, .list = NULL) {
  items <- c(list(...), .list)
  #lapply(items, tagAssert, type = "li", class = "dropdown")
  tags$header(class = "main-header",
              style = if (disable) "display: none;",
              span(class = "logo", title),
              tags$nav(class = "navbar navbar-static-top", role = "navigation",
                       # Embed hidden icon so that we get the font-awesome dependency
                       span(shiny::icon("bars"), style = "display:none;"),
                       # Sidebar toggle button
                       #                        a(href="#", class="sidebar-toggle", `data-toggle`="offcanvas",
                       #                          role="button",
                       #                          span(class="sr-only", "Toggle navigation")
                       #                        ),
                       
                       title.navbar,
                       div(class = "navbar-custom-menu",
                           tags$ul(class = "nav navbar-nav",
                                   items
                           )
                       )
              )
  )
}

# Define UI for application that draws a histogram
ui <- dashboardPage(
  title="Spectrum Speed",
  skin = "black",
  mydashboardHeader(title = "Spectrum Speed"),
  dashboardSidebar(width = 0),
  dashboardBody(
    tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico")),
    tags$head(tags$style(
      type="text/css",
      "#date_download img, #date_upload img, #binned_down img, #binned_up img, #day_week_down img, #day_week_up img{
    display: block;
    margin-left: auto;
    margin-right: auto;
    }")),
    includeCSS("www/custom.css"),
    tabsetPanel(
      #tags$head(tags$style("#all_down_up_speeds{height:100vh !important;}")),
      tabPanel(title = "Speeds Over Time",
               tabsetPanel(
                 tabPanel(title = "Download Speed",
                          plotOutput("date_download", width = "100%", height = "100%")
                          ),
                 tabPanel(title = "Upload Speed",
                          plotOutput("date_upload", width = "100%", height = "100%")
                          )
                 )),
      tabPanel(title = "Speeds Binned by Hour of Day",
               tabsetPanel(
                 tabPanel(title = "Download Speed",
                          plotOutput("binned_down", width = "100%", height = "100%")),
                 
                 tabPanel(title = "Upload Speed",
                          plotOutput("binned_up", width = "100%", height = "100%")))),
      
      tabPanel(title = "Mean Speeds by Day of Week",
               tabsetPanel(
                 tabPanel(title = "Download Speed",
                          plotOutput("day_week_down", width = "100%", height = "100%")),
                 
                 tabPanel(title = "Upload Speed",
                          plotOutput("day_week_up", width = "100%", height = "100%")))),
      
      tabPanel(title = "Overall Download / Upload Speeds",
               plotOutput("all_down_up_speeds")),
      tabPanel(title = "About",
               br(),
               p("After calling Charter/Spectrum numberous times about slow or 
                 nonexistant internet service, I wanted to emperically show
                 what was going on. We subscribe to Spectrum's 100 Mb per second 
                 down / 10 Mb per second up service. Using R and Shiny, I created this webapp
                 to display my internet speed over time."),
               br(),
               br(),
               p("Using Matt Martz' speedtest-cli (found at the link at bottom of page),
                 I test my upload and download speeds every quarter hour, commit the
                 results to a PostgreSQL database, and plot them using Shiny."),
               br(),
               br(),
               br(),
               br(),
               p("https://github.com/sivel/speedtest-cli"))
      )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  session$allowReconnect(TRUE)  # Allow new-session reconnections
  #source("global.R")
  rm(plots)
  output$date_download <- renderPlot(
    plots$download_speed,
    height = 600,
    width = 900
  )
  
  output$date_upload <- renderPlot(
    plots$upload_speed,
    height = 600,
    width = 900
  )
  
  output$binned_down <- renderPlot(
    plots$time.of.day.down.speed,
    height = 600,
    width = 586
  )
  
  output$binned_up <- renderPlot(
    plots$time.of.day.up.speed,
    height = 600,
    width = 586
  )
  
  output$day_week_down <- renderPlot(
    plots$day.of.week.down,
    height = 600,
    width = 586
  )
  
  output$day_week_up <- renderPlot(
    plots$day.of.week.up,
    height = 600,
    width = 586
  )

  output$all_down_up_speeds <- renderPlot(
    grid.arrange(plots$overall_down_speeds, plots$overall_up_speeds, ncol = 2)
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
