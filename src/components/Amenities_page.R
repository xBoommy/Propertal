library(tidyverse)
library(ggplot2)
library(leaflet)
library(leaflet.extras)
library(jsonlite)
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinycssloaders)
library(plotly)
library(geosphere)
library(ggmap)

#####################
#                   #
# LOCATION ANALYSIS #
#                   #
#####################


##### Register GOOGLE API key
google_api_key <- "AIzaSyDwQKrqvv9aby2GROX1XWYjuy05m3pEt8w"
register_google(google_api_key)

##### Read In Data
### Amenities
#D:/University/DBA3702/Team Project/Project_v3/

# Categories:
TRANSPORT <- read_csv("./www/TRANSPORT.csv")
DINING <- read_csv("./www/DINING.csv")
GROCERIES <- read_csv("./www/GROCERIES.csv")
HEALTHCARE <- read_csv("./www/HEALTHCARE.csv")
PARKS <- read_csv("./www/PARKS.csv")

# EDUCATION
schools_ELEMENTARY <- read_csv("./www/schools_ELEMENTARY.csv")
schools_PRIMARY <- read_csv("./www/schools_PRIMARY.csv")
schools_SECONDARY <- read_csv("./www/schools_SECONDARY.csv")
schools_POSTSEC <- read_csv("./www/schools_POSTSEC.csv")

amenities <- rbind(TRANSPORT,
                   HEALTHCARE,
                   DINING,
                   GROCERIES,
                   PARKS,
                   schools_ELEMENTARY,
                   schools_PRIMARY,
                   schools_SECONDARY,
                   schools_POSTSEC)

# Quantiles Reference
quantiles <- read_csv("./www/quantiles.csv")

# Icons 
icon_list1 <- iconList(
  DINING = makeIcon("cutlery1.png", "cutlery1.png", 21, 21),
  GROCERIES = makeIcon("groceries1.png", "groceries1.png", 21, 21),
  HEALTHCARE = makeIcon("hospital1.png", "hospital1.png", 21, 21),
  PARKS = makeIcon("park1.png", "park1.png", 25, 25),
  TRANSPORTATION = makeIcon("mrt1.png", "mrt1.png", 21, 21),
  `EDUCATION (ELEMENTARY)` = makeIcon("school1.png", "school1.png", 21, 21),
  `EDUCATION (PRIMARY)` = makeIcon("school1.png", "school1.png", 21, 21),
  `EDUCATION (SECONDARY)` = makeIcon("school1.png", "school1.png", 21, 21),
  `EDUCATION (POST SECONDARY)` = makeIcon("school1.png", "school1.png", 21, 21)
)

icon_list2 <- iconList(
  DINING = makeIcon("cutlery2.png", "cutlery2.png", 21, 21),
  GROCERIES = makeIcon("groceries2.png", "groceries2.png", 21, 21),
  HEALTHCARE = makeIcon("hospital2.png", "hospital2.png", 21, 21),
  PARKS = makeIcon("park2.png", "park2.png", 25, 25),
  TRANSPORTATION = makeIcon("mrt2.png", "mrt2.png", 21, 21),
  `EDUCATION (ELEMENTARY)` = makeIcon("school2.png", "school2.png", 21, 21),
  `EDUCATION (PRIMARY)` = makeIcon("school2.png", "school2.png", 21, 21),
  `EDUCATION (SECONDARY)` = makeIcon("school2.png", "school2.png", 21, 21),
  `EDUCATION (POST SECONDARY)` = makeIcon("school2.png", "school2.png", 21, 21)
)

##### Functions

# API request for address coordinates 
get_coord <- function(address){
  address <- paste0(address, ", Singapore")
  ## retrieve Coordinates 
  tryCatch({
    input.coord = geocode(address)
    return(input.coord)
  },
  error = function(cond) {
    return(NA)
  },
  warning = function(cond) {
    return(NA)
  })
}

# Calculate score based on distance and category
get_score <- function(dist, category){
  if (category == "TRANSPORTATION") {
    inv_score = as.numeric(cut(dist, c(0, quantiles$TRANSPORT), labels = c(1:10)))
    score = as.numeric(abs((inv_score)-11))
    return(score)
  }
  if (category == "HEALTHCARE") {
    inv_score = as.numeric(cut(dist, c(0, quantiles$HEALTHCARE), labels = c(1:10)))
    score = as.numeric(abs((inv_score)-11))
    return(score)
  }
  if (category == "DINING") {
    inv_score = as.numeric(cut(dist, c(0, quantiles$DINING), labels = c(1:10)))
    score = as.numeric(abs((inv_score)-11))
    return(score)
  }
  if (category == "GROCERIES") {
    inv_score = as.numeric(cut(dist, c(0, quantiles$GROCERIES), labels = c(1:10)))
    score = as.numeric(abs((inv_score)-11))
    return(score)
  }
  if (category == "PARKS") {
    inv_score = as.numeric(cut(dist, c(0, quantiles$PARKS), labels = c(1:10)))
    score = as.numeric(abs((inv_score)-11))
    return(score)
  }
  if (category == "EDUCATION (ELEMENTARY)") {
    inv_score = as.numeric(cut(dist, c(0, quantiles$schools_ELEMENTARY), labels = c(1:10)))
    score = as.numeric(abs((inv_score)-11))
    return(score)
  }
  if (category == "EDUCATION (PRIMARY)") {
    inv_score = as.numeric(cut(dist, c(0, quantiles$schools_PRIMARY), labels = c(1:10)))
    score = as.numeric(abs((inv_score)-11))
    return(score)
  }
  if (category == "EDUCATION (SECONDARY)") {
    inv_score = as.numeric(cut(dist, c(0, quantiles$schools_SECONDARY), labels = c(1:10)))
    score = as.numeric(abs((inv_score)-11))
    return(score)
  }
  if (category == "EDUCATION (POST SECONDARY)") {
    inv_score = as.numeric(cut(dist, c(0, quantiles$schools_POSTSEC), labels = c(1:10)))
    score = as.numeric(abs((inv_score)-11))
    return(score)
  }
}


# Find nearest amenities + distance + score
nearest_amenities <- function(coord){
  result = amenities %>%
    rowwise() %>%
    mutate(dist = distm(c(coord$lon, coord$lat), c(longitude, latitude), fun=distHaversine)) %>%
    ungroup() %>%
    group_by(category)%>%
    slice_min(dist) %>%
    ungroup() %>%
    rowwise() %>%
    mutate(score = get_score(dist, category))
  
  return (result)
}

#==============================================================================#

tabAmenities <- tabPanel(title = "Location Analysis",
                         value = "tab_amenities",
                         div(style = "height:100vh; width:97.5vw; position: absolute; margin: 0 auto; z-index: -1;",
                             img(src = 'amenities_background.jpg', style = "width: 100%; object-fit: cover;")
                         ),
                         div(style = "height: 165.5vh; 97.5vw; background-color: rgba(255,255,255, 0.5); margin: 0 auto; padding-top:25px; padding-left: 3vh;",
                             sidebarLayout(
                               
                               #===SIDEBAR===#
                               sidebarPanel(width = 3, # Start of Sidebar
                                            h1(strong("LOCATION ANALYSIS")),
                                            p("Get comparative score rating analysis based on the amenities, with respect to proximity or accessibility from the house locations selected in the sections below.",
                                              style = "text-align: justify;"),
                                            tabsetPanel(
                                              tabPanel(title = strong("Compare"),
                                                       br(),
                                                       p("Enter the housing location addresses that you would like to take a closer look at and analyse.",
                                                         style = "text-align: justify;"),
                                                       p("Compare up to 2 locations at once.",
                                                         style = "text-align: justify;"),
                                                       textInput(inputId = "address1in",
                                                                 label = h5(strong("Address 1")),
                                                                 value = "",
                                                                 placeholder = "Input an address"),
                                                       textInput(inputId = "address2in",
                                                                 label = h5(strong("Address 2")),
                                                                 value = "",
                                                                 placeholder = "Input an address"),
                                                       actionButton(inputId = "compare", label = "Compare")
                                                       ),
                                              tabPanel(title = strong("Preference"),
                                                       br(),
                                                       p("Set your desired preferences with respect to accessibility to different category of facilities from your selected location, with 10 being of high importance for accessibility and 0 being the low importance.",
                                                         style = "text-align: justify;"),
                                                       # p("Indicate a high number to express a strong preference with respect to the category and a low number to express a weak preference.",
                                                       #   style = "text-align: justify;"),
                                                       sliderInput(inputId = "transport_pref",
                                                                   label = h5(strong("Transport")),
                                                                   min = 0, 
                                                                   max = 10,
                                                                   value = 10),
                                                       sliderInput(inputId = "healthcare_pref",
                                                                   label = h5(strong("Healthcare")),
                                                                   min = 0, 
                                                                   max = 10,
                                                                   value = 10),
                                                       sliderInput(inputId = "dining_pref",
                                                                   label = h5(strong("Dining")),
                                                                   min = 0, 
                                                                   max = 10,
                                                                   value = 10),
                                                       sliderInput(inputId = "groceries_pref",
                                                                   label = h5(strong("Goceries")),
                                                                   min = 0, 
                                                                   max = 10,
                                                                   value = 10),
                                                       sliderInput(inputId = "parks_pref",
                                                                   label = h5(strong("Parks")),
                                                                   min = 0, 
                                                                   max = 10,
                                                                   value = 10),
                                                       sliderInput(inputId = "schools_elem_pref",
                                                                   label = h5(strong("Education (Elementary)")),
                                                                   min = 0, 
                                                                   max = 10,
                                                                   value = 10),
                                                       sliderInput(inputId = "schools_pri_pref",
                                                                   label = h5(strong("Education (Primary)")),
                                                                   min = 0, 
                                                                   max = 10,
                                                                   value = 10),
                                                       sliderInput(inputId = "schools_sec_pref",
                                                                   label = h5(strong("Education (Secondary)")),
                                                                   min = 0, 
                                                                   max = 10,
                                                                   value = 10),
                                                       sliderInput(inputId = "schools_post_pref",
                                                                   label = h5(strong("Education (Post Secondary)")),
                                                                   min = 0, 
                                                                   max = 10,
                                                                   value = 10),
                                                       actionButton(inputId = "preference", label = strong("Add Preferences"))
                                                       )
                                            )
                               ),# End of Sidebar
                               
                               #===MAIN PANEL===#
                               mainPanel( # start of main panel
                                 
                                 tabsetPanel( # start of tabset panel
                                   
                                   tabPanel(title = strong("Radar Chart"),
                                            width = 12,
                                            fluidRow(
                                              box(width = 12,
                                                  withSpinner(plotlyOutput("radarPlot", height="50vh"))
                                              )
                                            ),
                                            fluidRow(
                                              column(width = 6, align = "center",
                                                     div(style = "height:50vh; width:90%; background-color:rgba(255,255,255,0.7); border-style:solid; border-color:#FFFFFF; border-radius:15px 50px; padding-top: 5px; padding-left: 40px; padding-right: 40px;",
                                                               h3(strong("What is this?")),
                                                               p("From the address you enter, we can help you find the nearest facilities used in everyday life, ranging from different categories such as MRT stations, supermarkets, healthcare facilities and many more. ",
                                                                 style = "text-align: justify;"),
                                                               p("We have gone one step further and benchmarked your location of interest in comparison with the entirety of Singapore, so that you will know for sure if your new apartment is above average in terms of its location.",
                                                                 style = "text-align: justify;"),
                                                               p("We have also created a profile based on your preferences and you can compare it with the locations you are interested in. If the shapes in the Radar Chart match, it’s a fit! You will also be able to see in what categories your location of interest scores in alignment with your preferences and in which there is room for improvement.",
                                                                 style = "text-align: justify;")
                                                         )
                                                  
                                              ),
                                              column(width = 6, align = "center",
                                                     div(style = "height:50vh; width:90%; background-color:rgba(255,255,255,0.7); border-style:solid; border-color:#FFFFFF; border-radius:15px 50px; padding-top: 5px; padding-left: 40px; padding-right: 40px;",
                                                         h3(strong("Facilities Categories")),
                                                         div(style = "text-align: left;",
                                                             p(strong("Transportation")),
                                                             tags$ul(tags$li(p("MRT Stations"))),
                                                             p(strong("Healthcare")),
                                                             tags$ul(tags$li(p("Clinics"))),
                                                             p(strong("Dining")),
                                                             tags$ul(tags$li(p("Hawker Centres"))),
                                                             p(strong("Groceries")),
                                                             tags$ul(tags$li(p("Supermarkets"))),
                                                             p(strong("Parks")),
                                                             tags$ul(tags$li(p("Nature reserves, parks and playgrounds"))),
                                                             p(strong("Education")),
                                                             tags$ul(tags$li(p("Elementary / Primary / Secondary / Post-Secondary Schools")))
                                                         )
                                                      )
                                              )
                                            )
                                   ),
                                   
                                   tabPanel(title = strong("Map"),
                                            width = 12,
                                            fluidRow(
                                              column(width = 12,
                                                     div(style = "height:60vh; width:100%; background-color: transparent; margin: 0 auto;",
                                                         withSpinner(leafletOutput("amenitiesMap", height="60vh"))
                                                     ),
                                                     fluidRow(column(width = 6, align = "center", style = "height:60vh;",
                                                                     br(),
                                                                     div(
                                                                       bsCollapse(open = 1,
                                                                                  bsCollapsePanel(
                                                                                    title = h4(strong("Amenities (Address 1)"), style = "color: #000000"),
                                                                                    style ="info",
                                                                                    value = 1,
                                                                                    dataTableOutput(outputId = "address1Amenities")
                                                                                  )
                                                                       )
                                                                     )
                                                                     ),
                                                              column(width = 6, align = "center", style = "height:60vh;",
                                                                     br(),
                                                                     div(
                                                                       bsCollapse(open = 1,
                                                                                  bsCollapsePanel(
                                                                                    title = h4(strong("Amenities (Address 2)"), style = "color: #000000"),
                                                                                    style = "info",
                                                                                    value = 1,
                                                                                    dataTableOutput(outputId = "address2Amenities")
                                                                                  )
                                                                       )
                                                                     )
                                                                     )
                                                              )
                                                     )
                                              )  
                                   ),
                                   tabPanel(title = strong("Summary"),
                                            width = 12,
                                            
                                            fluidRow(
                                              column(width = 5, align = "center",
                                                     
                                                     div(style = "width: 90%; background-color:rgba(255,255,255,0.3); border-radius:15px 50px; margin-top: 20px; padding-bottom: 20px;",
                                                       div(align = "center",
                                                           plotlyOutput(outputId = "summaryAdd1overall"),
                                                           h3(strong("Address 1 Overall Score"))
                                                       ),
                                                       br(),
                                                       div(
                                                         bsCollapse(
                                                           bsCollapsePanel(title = h4(strong("Score Breakdown")),
                                                                           tableOutput(outputId = "summaryAdd1breakdown")
                                                           )
                                                         )
                                                       ),
                                                       br(),
                                                       div(
                                                         h3(strong("Preference Fit Score")),
                                                         h1(strong(textOutput(outputId = "summaryAdd1fit")))
                                                       )
                                                     )
                                              ),
                                              column(width = 2, align = "center",
                                                     # BLANK SPACE TO DIVIDE
                                              ),
                                              column(width = 5, align = "center",
                                                     div(style = "width: 90%; background-color:rgba(255,255,255,0.3); border-radius:15px 50px; margin-top: 20px; padding-bottom: 20px;",
                                                         div(align = "center",
                                                             plotlyOutput(outputId = "summaryAdd2overall"),
                                                             h3(strong("Address 2 Overall Score"))
                                                         ),
                                                         br(),
                                                         div(
                                                           bsCollapse(
                                                             bsCollapsePanel(title = h4(strong("Score Breakdown")),
                                                                             tableOutput(outputId = "summaryAdd2breakdown")
                                                             )
                                                           )
                                                         ),
                                                         br(),
                                                         div(
                                                           h3(strong("Preference Fit Score")),
                                                           h1(strong(textOutput(outputId = "summaryAdd2fit")))
                                                         )
                                                         )
                                              )
                                            )
                                            
                                   ) # end of tabset panel
                                   
                                 )
                               ) # end of main panel
                             ) # end of sidebarLayout
                             )
                 ) # end of tabPanel






# ui <- navbarPage(title = "Amenities Analysis Page",
#                  tabAmenities
# )
# 
# server <- function(input, output){
# 
#   #===AMENITIES PAGE===#
#   # Initialize RADAR plot
#   figRadar <- plot_ly(
#     type = 'scatterpolar',
#     fill = 'toself',
#     mode = 'lines'
#   ) %>%
#     layout(
#       polar = list(
#         radialaxis = list(
#           showgrid = T,
#           showline = F,
#           showticklabels = F,
#           showtickprefix = F,
#           ticks = '',
#           range = c(0,10)
#         ),
#         angularaxis = list (
#           visible =T,
#           showticklabels = T,
#           showtickprefix = T,
#           ticks = '',
#           tickwidth = 1,
#           linewidth =2,
#           layer = 'below traces'
#         )
#       ),
#       showlegend = TRUE,
#       plot_bgcolor = '#00000000',
#       paper_bgcolor = 'rgba(0,0,0,0)'
#     )
#   
#   # Reactive to store updates
#   updateAddress1 <- reactiveValues(update = NULL)
#   updateAddress2 <- reactiveValues(update = NULL)
#   updatePreference <- reactiveValues(update = NULL)
#   
#   # Reactive to store amenities
#   amenitiesAdd1 <- reactiveValues(data = NULL)
#   amenitiesAdd2 <- reactiveValues(data = NULL)
#   preferencesAdd <- reactiveValues(data = NULL)
#   
#   # Initialize Leaflet MAP
#   amenities_Map <- leaflet() %>%
#     addProviderTiles(providers$CartoDB.Positron)
#   
#   # Compare Button Event
#   observeEvent(input$compare, {
#     # Read in address input
#     address1 <- isolate(input$address1)
#     address2 <- isolate(input$address2)
#     
#     if (address1 != ""){
#       address1_coord <- get_coord(address1)
#       address1_df <- nearest_amenities(address1_coord) %>% arrange(category)
#       
#       amenitiesAdd1$data = address1_df
#       
#       # Update radar with address1 info
#       updateAddress1$update <- function(base){
#         result = base %>% add_trace(
#           r = c(address1_df$score, address1_df$score[1]),
#           theta = c(address1_df$category, address1_df$category[1]),
#           name = input$address1,
#           connectgaps = TRUE,
#           fillcolor = rgb(200, 0, 0, 50, maxColorValue = 255),
#           line = list(color = "black")
#         )
#         
#         return(result)
#       }
#       
#       # Update map with address1 info
#       amenities_Map <- amenities_Map %>%
#         addCircleMarkers(
#           lng = address1_coord$lon,
#           lat = address1_coord$lat,
#           label = address1,
#           group = address1,
#           radius = 5,
#           color = rgb(200, 0, 0, 255, maxColorValue = 255)
#         )
#       
#       # Update map with address1 amenities
#       amenities_Map <- amenities_Map %>%
#         addMarkers(
#           lng = address1_df$longitude,
#           lat = address1_df$latitude,
#           label = address1_df$name,
#           group = address1,
#           icon = icon_list1[address1_df$category]
#         )
#     }
#     if (address2 != ""){
#       address2_coord <- get_coord(address2)
#       address2_df <- nearest_amenities(address2_coord) %>% arrange(category)
#       
#       amenitiesAdd2$data = address2_df
#       
#       # Function to update radar with address2 info
#       updateAddress2$update <- function(base){
#         result = base %>% add_trace(
#           r = c(address2_df$score, address2_df$score[1]),
#           theta = c(address2_df$category, address2_df$category[1]),
#           name = input$address2,
#           connectgaps = TRUE,
#           fillcolor = rgb(0, 0, 200, 50, maxColorValue = 255),
#           line = list(color = "black")
#         )
#         
#         return(result)
#       }
#       
#       # Update map with address2 info
#       amenities_Map <- amenities_Map %>%
#         addCircleMarkers(
#           lng = address2_coord$lon,
#           lat = address2_coord$lat,
#           label = address2,
#           group = input$address2,
#           radius = 5,
#           color = rgb(0, 0, 200, 255, maxColorValue = 255)
#         )
#       
#       # Update map with address2 amenities
#       amenities_Map <- amenities_Map %>%
#         addMarkers(
#           lng = address2_df$longitude,
#           lat = address2_df$latitude,
#           label = address2_df$name,
#           group = address2,
#           icon = icon_list2[address2_df$category]
#         )
#     }
#     
#     ### Output Amenities map
#     output$amenitiesMap <- renderLeaflet({
#       amenities_Map %>%
#         addLayersControl(
#           overlayGroups =c(address1, address2),
#           options = layersControlOptions(collapsed=FALSE)
#         )
#     })
#     
#     ### Output Address1 Amenities Data
#     output$address1Amenities <- renderDataTable(
#       
#       if (!is.null(amenitiesAdd1$data)) {
#         amenitiesAdd1$data %>%
#           select(-label,-label2, -country, -latitude, -longitude, -score) %>%
#           mutate(postal_code = as.character(postal_code), dist = round(dist,1),
#                  address = ifelse(category == "TRANSPORTATION", "", address),
#                  postal_code = ifelse(category == "TRANSPORTATION", "", postal_code)) %>%
#           rename(Category = category) %>%
#           rename(Name = name) %>%
#           rename(Address = address) %>%
#           rename(`Postal Code` = postal_code) %>%
#           rename(`Distance (m)` = dist)
#         
#       }
#     )
#     
#     ### Output Address2 Amenities Data
#     output$address2Amenities <- renderDataTable(
#       
#       if (!is.null(amenitiesAdd2$data)) {
#         amenitiesAdd2$data %>%
#           select(-label,-label2, -country, -latitude, -longitude, -score) %>%
#           mutate(postal_code = as.character(postal_code), dist = round(dist,1),
#                  address = ifelse(category == "TRANSPORTATION", "", address),
#                  postal_code = ifelse(category == "TRANSPORTATION", "", postal_code)) %>%
#           rename(Category = category) %>%
#           rename(Name = name) %>%
#           rename(Address = address) %>%
#           rename(`Postal Code` = postal_code) %>%
#           rename(`Distance (m)` = dist)
#       }
#     )
#     
#   }) # End of Compare button event
#   
#   
#   # Add preferences Button Event
#   observeEvent(input$preference, {
#     transport_pref <- isolate(input$transport_pref)
#     healthcare_pref <- isolate(input$healthcare_pref)
#     dining_pref <- isolate(input$dining_pref)
#     groceries_pref <- isolate(input$groceries_pref)
#     parks_pref <- isolate(input$parks_pref)
#     schools_elem_pref <- isolate(input$schools_elem_pref)
#     schools_pri_pref <- isolate(input$schools_pri_pref)
#     schools_sec_pref <- isolate(input$schools_sec_pref)
#     schools_post_pref <- isolate(input$schools_post_pref)
#     
#     preference_score <- c(dining_pref,
#                           schools_elem_pref,
#                           schools_post_pref,
#                           schools_pri_pref,
#                           schools_sec_pref,
#                           groceries_pref,
#                           healthcare_pref,
#                           parks_pref,
#                           transport_pref)
#     
#     category <- c("HEALTHCARE",
#                   "GROCERIES",
#                   "DINING",
#                   "TRANSPORTATION",
#                   "EDUCATION (SECONDARY)",
#                   "EDUCATION (PRIMARY)",
#                   "EDUCATION (POST SECONDARY)",
#                   "EDUCATION (ELEMENTARY)", 
#                   "PARKS") %>% sort()
# 
#     # Store preferences
#     preferencesAdd$data <- data.frame(pref_score = preference_score, category)
#     
#     # Function to update radar with preference info
#     updatePreference$update <- function(base){
#       result = base %>% add_trace(
#         r = c(preference_score, preference_score[1]),
#         theta = c(category, category[1]),
#         name = "Your preferences",
#         connectgaps = TRUE,
#         fillcolor = rgb(0, 200, 0, 50, maxColorValue = 255),
#         line = list(color = "black")
#       )
#       
#       return(result)
#     }
#     
#   },ignoreInit = FALSE, ignoreNULL = FALSE) # End of Add preferences button event
#   
#   # Compare and Add Preference Button Event
#   observeEvent(input$compare, {
#     
#     if(!is.null(amenitiesAdd1$data)){
#       data_fit1 <- merge((amenitiesAdd1$data %>% select(category, score)), preferencesAdd$data, by = "category") %>% 
#         mutate(fit = ifelse(pref_score - score > 0, pref_score - score, 0))
#       fit1 <- sum(data_fit1[, "fit"])
#       pie1 <- data.frame(score = c((100 - fit1), fit1), label = c("Percentage Fit", ""))
#     }
#     
#     if(!is.null(amenitiesAdd2$data)){
#       data_fit2 <- merge((amenitiesAdd2$data %>% select(category, score)), preferencesAdd$data, by = "category") %>% 
#         mutate(fit = ifelse(pref_score - score > 0, pref_score - score, 0))
#       fit2 <- sum(data_fit2[, "fit"])
#       pie2 <- data.frame(score = c((100 - fit2), fit2), label = c("Percentage Fit", ""))
#     }
#     
#     # Output Address 1 Fit 
#     output$address1 <- renderPlotly({
#         if(!is.null(pie1)) {
#           # Plot in Pie Chart
#           plot_ly(data = pie1, values = ~score, textposition = 'inside') %>%
#             add_pie(hole = 0.6) %>%
#             layout(title = FALSE,  showlegend = TRUE,
#                    xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
#                    yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
#                    plot_bgcolor  = "rgba(0, 0, 0, 0)",
#                    paper_bgcolor = "rgba(0, 0, 0, 0)")
#           }
#     })
#     
#     # Output Address 2 table 
#     output$table1 <- renderDataTable({
#       if(!is.null(data_fit1)){
#         data_fit1 %>% mutate(Fit = paste0(10-fit,"/10")) %>% select(category, Fit) %>% distinct()
#       }
#     })
# 
#     # Output Address 2 Fit 
#     output$address2 <- renderPlotly({
#         if(!is.null(pie2)) {
#         # Plot in Pie Chart
#           plot_ly(data = pie2, values = ~score, textposition = 'inside') %>%
#             add_pie(hole = 0.6) %>%
#             layout(title = FALSE,  showlegend = TRUE,
#                    xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
#                    yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
#                    plot_bgcolor  = "rgba(0, 0, 0, 0)",
#                    paper_bgcolor = "rgba(0, 0, 0, 0)")
#         }
#     })
#     
#     # Output Address 2 Table 
#     output$table2 <- renderDataTable({
#       if(!is.null(data_fit2)){
#         data_fit2 %>% mutate(Fit = paste0(10-fit,"/10")) %>% select(category, Fit) %>% distinct()
#       }
#     })
#   })
#   
#   ### Output Radar plot
#   output$radarPlot <- renderPlotly({
#     if (!is.null(updatePreference$update)) {
#       figRadar <- updatePreference$update(figRadar)
#     }
#     
#     if (!is.null(updateAddress1$update)) {
#       figRadar <- updateAddress1$update(figRadar)
#     }
#     
#     if (!is.null(updateAddress2$update)) {
#       figRadar <- updateAddress2$update(figRadar)
#     }
#     
#     figRadar
#   })
#   
#   #=======================#
# 
# } # End of server
# 
# ###
# # RUN APP
# ###
# shinyApp(ui = ui, server = server)