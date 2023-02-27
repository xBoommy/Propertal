library(tidyverse)
library(sf)
library(leaflet)
library(leaflet.extras)
library(leaflet.minicharts)
library(jsonlite)
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyBS)
library(geosphere)
library(ggmap)
library(lubridate)

# Read in SG boundary polygons
sg_boundary <- readRDS("./www/sg_planning_areas_new.rds")
#"D:/University/DBA3702/Team Project/Project Drafts/Project_v3"

# formatted_data from Amenities_page.R

# From scrapped data, find recent 3 month date range
recent_filter <- max(formatted_data$date) %m-% months(3)

# Filter data to recent dates
recent_data <- formatted_data %>%
  filter(date > recent_filter) 


##### TOWN SUMMARY MAP

town_summary <- recent_data %>%
  group_by(town) %>%
  summarise(count = n(), avg_price = mean(resale_price),
            max_price = max(resale_price),
            min_price = min(resale_price)) %>%
  mutate(avg_price = paste0("$", prettyNum(avg_price, big.mark = ",", scientific = FALSE)),
         max_price = paste0("$", prettyNum(max_price, big.mark = ",", scientific = FALSE)),
         min_price = paste0("$", prettyNum(min_price, big.mark = ",", scientific = FALSE))) %>%
  right_join(sg_boundary, by = c("town" = "PLN_AREA_N")) %>%
  st_as_sf()

pal <- colorNumeric(
  palette = colorRampPalette(c('white', 'blue'))(length(town_summary$count)), 
  domain = town_summary$count)

factop <- function(x) {
  ifelse(is.na(x), 0, 0.5)
}

# Town summary
townSummary_map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lat = 1.320783, lng = 103.858196, zoom = 11) %>%
  addPolygons(data = town_summary,
              weight = 2, 
              stroke = TRUE,
              smoothFactor = 0.1,
              fillOpacity = ~factop(count),
              color = ~pal(count),
              popup = paste0(
                "<div>",
                "<h3>", town_summary$town, "</h3>",
                "<strong>",  "Flats Sold: ", "</strong>",
                town_summary$count,
                "<br>",
                "<br>",
                "<strong>", "Average Resale Price: ", "</strong>",
                town_summary$avg_price,
                "<br>",
                "<strong>", "Max Resale Price: ", "</strong>",
                town_summary$max_price,
                "<br>",
                "<strong>", "Min Resale Price: ", "</strong>",
                town_summary$min_price,
                "</div>"
              ))


##### TYPE SUMMARY PIE
type_summary <- recent_data %>%
  group_by(flat_type) %>%
  summarise(count = n())

typeSummary_plot <- plot_ly(data = type_summary,
                            labels = ~flat_type,
                            values = ~count,
                            textposition='inside') %>%
  add_pie(hole = 0.6) %>%
  layout(title = FALSE,  showlegend = TRUE,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
         plot_bgcolor  = "rgba(0, 0, 0, 0)",
         paper_bgcolor = "rgba(0, 0, 0, 0)")

##### MODEL SUMMARY PIE
model_summary <- recent_data %>%
  group_by(flat_model) %>%
  summarise(count = n())

modelSummary_plot <- plot_ly(data = model_summary,
                     labels = ~flat_model,
                     values = ~count,
                     textposition='inside') %>%
  add_pie(hole = 0.6) %>%
  layout(title = FALSE,  showlegend = TRUE,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         plot_bgcolor  = "rgba(0, 0, 0, 0)",
         paper_bgcolor = "rgba(0, 0, 0, 0)")

##### OVERALL COUNT
total_resale_count <- nrow(recent_data)


#=====================================================#
tabRecent <- tabPanel(title = "Recent Summary",
                      value = "tab_recent",
                      div(style = "height:100vh; width:97.5vw; position: absolute; margin: 0 auto; z-index: -1;",
                          img(src = 'home_background.jpg', style = "width: 100%; object-fit: cover;")
                      ),
                      div(style = "height:100vh; 97.5vw; background-color: rgba(255,255,255, 0.5); margin: 0 auto; padding-top:25px;",
                          fluidRow(
                            column(width = 2,
                                   div(style = "padding-left: 3vh;",
                                       h1(strong("RECENT SUMMARY")),
                                       p("We have compiled the information of resale flat transactions in Singapore for the past 3 months and summarised them into bite-size pieces",
                                         style = "text-align: justify;"),
                                       p("Explore the page and learn more about the recent events",
                                         style = "text-align: justify;")
                                   )
                            ),
                            column(width = 6, align = "center",
                                   withSpinner(leafletOutput(outputId = "townSummary",
                                                 width = "100%",
                                                 height = "60vh")),
                                   p("Interact with the map to find out more information"),
                                   br(),
                                   div(style = "height:20vh; width:50%; background-color: rgba(255,255,255,0.7); border-style:solid; border-color:#FFFFFF; border-radius:50px 15px; text-align:center;",
                                       h3("Total Flats Sold"),
                                       h1(strong(total_resale_count)),
                                       h4(paste("Since", format(recent_filter %m+% months(1), "%B %Y")))
                                   )
                            ),
                            column(width = 4, align = "center",
                                   div(style = "height:42vh; width:90%; background-color:rgba(255,255,255,0.7); border-style:solid; border-color:#FFFFFF; border-radius:15px 50px;",
                                       withSpinner(plotlyOutput(outputId = "typeSummary", height = "37vh")),
                                       h4(strong("Flats Sold by Type"))
                                   ),
                                   br(),
                                   div(style = "height:42vh; width:90%; background-color:rgba(255,255,255,0.7); border-style:solid; border-color:#FFFFFF; border-radius:15px 50px;",
                                       withSpinner(plotlyOutput(outputId = "modelSummary", height = "37vh")),
                                       h4(strong("Flats Sold by Model"))
                                   )
                            )
                          )
                          )
                      )

# ui <- navbarPage(title = "Recent Event Page",
#                  tabRecent
# )
# 
# 
# server <- function(input, output) {
# 
#   #===RECENT EVENTS PAGE===#
# 
#   output$townSummary <- renderLeaflet(
#     townSummary_map
#   )
# 
#   output$typeSummary <- renderPlotly(
#     typeSummary_plot
#   )
# 
#   output$modelSummary <- renderPlotly(
#     modelSummary_plot
#   )
# 
#   #=======================#
# }
# 
# shinyApp(ui = ui, server = server)
# 
