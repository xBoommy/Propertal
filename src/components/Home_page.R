library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinyBS)



tabHome <- tabPanel(title = "Homepage",
                    div(style = "height:100vh; width:97.5vw; position: absolute; margin: 0 auto; z-index: -1;",
                        img(src = 'home_background.jpg', style = "width: 100%; object-fit: cover;")
                        ),
                    fluidRow(
                      div(style = "height:30vh; width: 100%; margin: 0 auto; text-align: center; background-color: transparent; padding-top: 5vh;",
                          h3(strong("Welcome to")),
                          img(src = 'logo.png', style = "width: 35%;"),
                          h4("Your one-stop avenue for Housing Location Analysis and Resale Information")
                          )
                    ),
                    br(),
                    fluidRow(column(width = 2),
                             column(width = 3, align = "center",
                                    br(),
                                    br(),
                                    div(style = "height:50vh; width:100%; background-color:rgba(255,255,255, 0.8); border-radius:15px 50px; padding: 0px 50px;",
                                        # Price
                                        br(),
                                        h2(strong("Resale Analysis")),
                                        br(),
                                        p(style = "text-align:justify; word-wrap: break-word;",
                                          "Compare pricing trends and information across recent years based on different filter categories of flats that you are interested to look at"
                                          ),
                                        br(),
                                        actionButton(inputId = "navigatePrice", label = h4(strong("Navigate")))
                                        )
                             ),
                             column(width = 3, align = "center",
                                    div(style = "height:50vh; width:100%; background-color: rgba(255,255,255, 0.8); border-radius:15px 50px; padding: 0px 50px;",
                                        br(),
                                        # Amenities
                                        h2(strong("Location Analysis")),
                                        br(),
                                        p(style = "text-align:justify; word-wrap: break-word;",
                                           "Analyse your housing locations and find out about facilities around it"
                                           ),
                                        p(style = "text-align:justify; word-wrap: break-word;",
                                          "Obtain score ratings of your locations based on proximity and accessibility to facilities and compare them against multiple locations that you are interested to look at"
                                        ),
                                        p(style = "text-align:justify; word-wrap: break-word;",
                                          "You can also include your preferences to directly compare among the locations"
                                        ),
                                        br(),
                                        actionButton(inputId = "navigateAmenities", label = h4(strong("Navigate")))
                                        )
                             ),
                             column(width = 3, align = "center",
                                    br(),
                                    br(),
                                    div(style = "height:50vh; width:100%; background-color: rgba(255,255,255, 0.8); border-radius:15px 50px; padding: 0px 50px;",
                                        br(),
                                        # Recent
                                        h2(strong("Recent Summary")),
                                        br(),
                                        p(style = "text-align:justify; word-wrap: break-word;",
                                           "Find out about recent flat resale transactions"
                                           ),
                                        p(style = "text-align:justify; word-wrap: break-word;",
                                          "Information on transactions have been compiled form the past 3 months and summarised into different visualisations that you can explore"
                                        ),
                                        br(),
                                        actionButton(inputId = "navigateRecent", label = h4(strong("Navigate")))
                                        )
                             ),
                             column(width = 2))
                    )

# ui <- navbarPage(title = "Home Page",
#                  tabHome
# )
# 
# server <- function(input, output){
#   
#   #===HOME PAGE===#
#   observeEvent(input$navigatePrice, {
#     updateTabsetPanel("main_nav",
#                       selected = "tab_resale")
#   })
#   
#   observeEvent(input$navigateAmenities, {
#     updateTabsetPanel("main_nav",
#                       selected = "tab_amenities")
#   })
#   
#   observeEvent(input$navigateRecent, {
#     updateTabsetPanel("main_nav",
#                       selected = "tab_recent")
#   })
#   #=============================#
# }
# 
# shinyApp(ui = ui, server = server)
