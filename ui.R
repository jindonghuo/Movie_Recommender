#####################Usagae############################
#####1. Download the zipped file and unzip at <foldername>
#####2. Open ui.R file, setwd(<foldername>) 
#####3. Click Run App
#####################Usagae############################
#setwd("C:/WSL/CS598_statics_learning/project4/Project4/MovieRecommender/")

## ui.R
mypackages = c("devtools", "shiny","shinydashboard", "ShinyRatingInput","shinyjs",
               "data.table", "reshape2", "recommenderlab","Matrix", "tidyverse", "hash", "slam")   # required packages
tmp = setdiff(mypackages, rownames(installed.packages()))  # packages need to be installed
if (length(tmp) > 0) install.packages(tmp)
lapply(mypackages, require, character.only = TRUE)

#devtools::install_github("stefanwilhelm/ShinyRatingInput", force = TRUE) 

source('functions/helpers.R')

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Recommend by Genre", tabName = "System1", icon = icon("star")),
    menuItem("Recommend by Rating", tabName = "System2", icon = icon("star"))
  )
)


body <- dashboardBody(includeCSS("css/movies.css"),
        tabItems(
                      
                tabItem(tabName = "System1",
                        includeCSS("css/movies.css"),
                        fluidRow(
                          box(width = 12, title = "Step 1: Select your Favorite Genre", status = "info",
                              solidHeader = TRUE, collapsible = TRUE,
                              div(class = "rategenres",
                                  selectInput("selected_genre", "select a genre from the drop down menu:",
                                              list ("Action", "Adventure", "Animation",
                                                   "Children's", "Comedy", "Crime",
                                                   "Documentary", "Drama", "Fantasy",
                                                   "Film-Noir", "Horror", "Musical",
                                                   "Mystery", "Romance", "Sci-Fi",
                                                   "Thriller", "War", "Western")
                                  )
                              )
                            )
                        ),
                        fluidRow(
                          useShinyjs(),
                          box(
                            width = 12, status = "info", solidHeader = TRUE,
                            title = "Step 2: Discover movies you might like",
                            br(),
                            withBusyIndicatorUI(
                              actionButton("btn1", "Click here to get your recommendations", class = "btn-warning")
                            ),
                            br(),
                            tableOutput("results1")
                          )
                        )
                ),

              tabItem(tabName = "System2",
                        includeCSS("css/movies.css"),
                        fluidRow(
                          box(width = 12, title = "Step 1: Rate as many movies as possible", status = "info", 
                              solidHeader = TRUE, collapsible = TRUE,
                              div(class = "rateitems",
                                  uiOutput('ratings')
                              )
                          )
                        ),
                        fluidRow(
                          useShinyjs(),
                          box(
                            width = 12, status = "info", solidHeader = TRUE,
                            title = "Step 2: Discover movies you might like",
                            br(),
                            withBusyIndicatorUI(
                              actionButton("btn", "Click here to get your recommendations", class = "btn-warning")
                            ),
                            br(),
                            tableOutput("results")
                          )
                        )
              )
        )
)  

shinyUI(
  dashboardPage(
    skin = "blue",
    dashboardHeader(title = "Movie Recommender"),
    #dashboardSidebar(disable = TRUE),

    sidebar,
    body
    
  )
) 
