library(tidyverse)
library(ggplot2)
library(leaflet)
library(leaflet.extras)
library(jsonlite)
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyBS)
library(plotly)
library(geosphere)
library(ggmap)
library(lubridate)

##########################
#                        #
# RESALE PRICES ANALYSIS #
#                        #
##########################

#####################
# FLAT RESALE PRICE #

# Non-loop implementation to obtain data.gov "Resale Flat Prices" data
get_resale_data <- function() {
  # Query single entry to obtain dataset information
  root_url = "https://data.gov.sg/api/action/datastore_search?resource_id=f1765b54-a209-4718-8d38-a39237f502b3"
  query = "&limit="
  query_url = paste0(root_url, query, 1)
  query_json = fromJSON(query_url)
  
  # Obtain dataset total size
  total = query_json$result$total
  
  # Query total data
  query_url = paste0(root_url, query, total)
  query_json = fromJSON(query_url)
  
  data = query_json$result$records
  
  return(data)
}

# Function to format data
format_data <-function(data) {
  data %>%
    mutate(town = ifelse((town == "KALLANG/WHAMPOA"),"CENTRAL AREA" ,town)) %>%
    mutate(town = as.factor(town),
           flat_type = as.factor(flat_type),
           flat_model = as.factor(flat_model),
           floor_area_sqm = as.numeric(floor_area_sqm),
           resale_price = as.numeric(resale_price),
           lease_commence_date = as.numeric(lease_commence_date)) %>%
    separate(storey_range, into = c("Lower_storey", "Upper_storey"), sep = " TO ") %>%
    mutate(Lower_storey = as.numeric(Lower_storey),
           Upper_storey = as.numeric(Upper_storey)) %>%
    separate(remaining_lease, into = c("remaining_lease_year", "remaining_lease_month"), sep = " years") %>%
    mutate(remaining_lease_year = as.numeric(remaining_lease_year),
           remaining_lease_month = as.numeric(str_trim(gsub("[month|months]","", remaining_lease_month))),
           remaining_lease_month = replace_na(remaining_lease_month, 0)) %>%
    mutate(date = as.Date(paste(month, "01", sep = "-"), format = "%Y-%m-%d"))
}

### Read & Format Data
data <-  get_resale_data()
formatted_data <- format_data(data)
rm(data)

### Filter Options
# town // town_select
town_option <- as.character(unique(formatted_data$town))
town_option <- sort(str_to_title(town_option))
town_option <- c("All", town_option)

town_filter <- function(df, select) {
  if (select == "All") {
    new_df = filter(df, town %in% str_to_upper(town_option))
  } else {
    new_df = filter(df, town == str_to_upper(select))
  }
  
  return(new_df)
}


# flat_type // type_select
type_option <- as.character(unique(formatted_data$flat_type))
type_option <- sort(str_to_title(type_option))
type_option <- c("All", type_option)

type_filter <- function(df, select) {
  if (select == "All") {
    new_df = filter(df, flat_type %in% str_to_upper(type_option))
  } else {
    new_df = filter(df, flat_type == str_to_upper(select))
  }
  
  return(new_df)
}


# flat_model // model_select
model_option <- as.character(unique(formatted_data$flat_model))
model_option <- sort(str_to_title(model_option))
model_option <- c("All", model_option)

model_filter <- function(df, select) {
  if (select == "All") {
    new_df = filter(df, flat_model %in% model_option)
  } else {
    new_df = filter(df, flat_model == select)
  }
  
  return(new_df)
}


# floor_area_sqm // area_select
floor_area_sqm_max <- max(formatted_data$floor_area_sqm)
floor_area_sqm_min <- min(formatted_data$floor_area_sqm)
#(floor_area_sqm >= area_select[1]) & (floor_area_sqm <= area_select[2])

area_filter <- function(df, min, max) {
  new_df = filter(df, (floor_area_sqm >= min) & (floor_area_sqm <= max))
  return(new_df)
}


# floor_level // level_select
floor_level_max <- max(formatted_data$Upper_storey)
floor_level_min <- min(formatted_data$Lower_storey)
#(Lower_storey >= level_select[1]) & (Upper_storey <= level_select[2])

level_filter <- function(df, min, max) {
  new_df = filter(df, (Lower_storey >= min) & (Upper_storey <= max))
  return(new_df)
}


# remaining_lease // lease_select
remaining_lease_max <- max(formatted_data$remaining_lease_year)
remaining_lease_min <- min(formatted_data$remaining_lease_year)
#(remaining_lease_year >= lease_select[1]) & (remaining_lease_year <= lease_select[2])

lease_filter <- function(df, min, max) {
  new_df = filter(df, (remaining_lease_year >= min) & (remaining_lease_year <= max))
  return(new_df)
}

##########################
# HDB Resale Price Index #


# Function to retrieve data.gov "monthly CPI" data (default base year = 2019)
get_cpi_data <- function() {
  # Query single entry to obtain dataset information
  root_url = "https://data.gov.sg/api/action/datastore_search?resource_id=bf8bfcb3-efbd-4d76-bc69-7336f4443f93"
  query = "&limit="
  query_url = paste0(root_url, query, 1)
  query_json = fromJSON(query_url)
  
  # Obtain dataset total size
  total = query_json$result$total
  
  # Query total data
  query_url = paste0(root_url, query, total)
  query_json = fromJSON(query_url)
  
  data = query_json$result$records
  
  return(data)
}

# Function to adjust dataset
adjust_cpi <- function(cpi){
  # Retrieve CPI index based on "all items"
  cpi <- cpi %>%
    filter(level_1 == "All Items") %>%
    mutate(value = as.numeric(value)) %>%
    select(month, value) %>%
    distinct() %>%
    rename(cpi_2019 = value)
  
  # Find CPI of 2017-01 in terms of 2019-01 base
  base_2017 <- cpi %>%
    filter(month == "2017-01") %>%
    select(cpi_2019) %>%
    as.numeric()
  
  # Calculate CPI with base 2017
  cpi %>% mutate(cpi_2017 = cpi_2019/base_2017*100)
}

### Read & Format Data
cpi <- adjust_cpi(get_cpi_data())

# Calculate inflation adjusted prices in "formatted_data" dataset
formatted_data <- formatted_data %>% 
  left_join(cpi, by = "month") %>% 
  mutate(resale_price_adj_2017 =(resale_price/cpi_2017)*100,
         resale_price_adj_2019 =(resale_price/cpi_2019)*100)

#==============================================================================#



tabResale <- tabPanel(title = "Resale Analysis",
                      value = "tab_resale",
                      div(style = "height:100vh; width:97.5vw; position: absolute; margin: 0 auto; z-index: -1;",
                          img(src = 'resale_background.jpg', style = "width: 100%; object-fit: cover;")
                      ),
                      div(style = "height:120vh; 97.5vw; background-color: rgba(255,255,255, 0.5); margin: 0 auto; padding-top:25px; padding-left: 3vh;",
                        sidebarLayout(
                          #===SIDEBAR===#
                          sidebarPanel(width = 3, # Start of Sidebar
                                       h1(strong("RESALE ANALYSIS")),
                                       p("We have compiled and process the information of resale flat transactions in Singapore since 2017 to provide you with visual trends of the change in resale prices over time",
                                         style = "text-align: justify;"),
                                       p("Here, you can filter the information to your specific requirements and observe their price trends",
                                         style = "text-align: justify;"),
                                       bsCollapse(multiple = TRUE, open = 1,
                                                  bsCollapsePanel(title = h5(strong("Filter")),
                                                                  value = 1,
                                                                  # town -select box
                                                                  selectInput(inputId = "town_select",
                                                                              label = h5(strong("Town")),
                                                                              choices = list(town_option = town_option), 
                                                                              selected = 1),
                                                                  # flat_type -select box
                                                                  selectInput(inputId = "type_select",
                                                                              label = h5(strong("Flat Type")),
                                                                              choices = list(type_option = type_option), 
                                                                              selected = 1),
                                                                  # flat_model -select box
                                                                  selectInput(inputId = "model_select",
                                                                              label = h5(strong("Flat Model")),
                                                                              choices = list(model_option = model_option), 
                                                                              selected = 1),
                                                                  # floor_area_sqm (range) -slider range
                                                                  sliderInput(inputId = "area_select",
                                                                              label = h5(strong("Floor Area (sqm)")),
                                                                              min = floor_area_sqm_min, 
                                                                              max = floor_area_sqm_max,
                                                                              value = c(floor_area_sqm_min, floor_area_sqm_max)),
                                                                  # floor_level (range)
                                                                  sliderInput(inputId = "level_select",
                                                                              label = h5(strong("Storey Range")),
                                                                              min = floor_level_min, 
                                                                              max = floor_level_max,
                                                                              value = c(floor_level_min, floor_level_max)),
                                                                  # remaining lease (range)
                                                                  sliderInput(inputId = "lease_select",
                                                                              label = h5(strong("Remaining lease (years)")),
                                                                              min = remaining_lease_min, 
                                                                              max = remaining_lease_max,
                                                                              value = c(remaining_lease_min, remaining_lease_max)),
                                                                  # Confirmation button
                                                                  actionButton(inputId = "search", label = h5(strong("Confirm Filter")))
                                                  )
                                       )
                          ), # End of Sidebar
                          
                          #===MAIN PANEL===#
                          mainPanel( # start of main panel
                            fluidRow(column(width = 12, align = "center",
                                            tabsetPanel(
                                              tabPanel(title = strong("Price Trend"), width = 12,
                                                       withSpinner(plotlyOutput("plot", height="50vh"))
                                              )
                                            ),
                                            br(),
                                            fluidRow(
                                              column(width = 6, align = "center",
                                                     uiOutput(outputId = "inflationInfo")
                                              ),
                                              column(width = 6, align = "center",
                                                     uiOutput(outputId = "valuationInfo")
                                              )
                                            )
                            ))
                            
                          ) # End of main panel
                          
                        ) # end of sidebarLayout
                      )
                         
                      ) # end of tabPanel






# ui <- navbarPage(title = "Price Analysis Page",
#                  tabResale
# )
# 
# 
# server <- function(input, output) {
# 
#   #===RESALE PAGE===#
#   
#   observeEvent(input$search,{
#     ## Function to filter data based on input
#     filter_data <- function(df) {
#       filtered_df = town_filter(df, input$town_select)
#       filtered_df = type_filter(filtered_df, input$type_select)
#       filtered_df = model_filter(filtered_df, input$model_select)
#       filtered_df = area_filter(filtered_df, input$area_select[1], input$area_select[2])
#       filtered_df = level_filter(filtered_df, input$level_select[1], input$level_select[2])
#       filtered_df = lease_filter(filtered_df, input$lease_select[1], input$lease_select[2])
#       
#       return(filtered_df)
#     }
#     
#     # filtering data based on input filter options
#     filtered_data <-  filter_data(formatted_data)
#     
#     # plot data
#     plot_data <-  filtered_data %>%
#       group_by(month) %>%
#       summarize(price = mean(resale_price),
#                 price_adj_2017 = mean(resale_price_adj_2017),
#                 price_adj_2019 = mean(resale_price_adj_2019))
#     
#     output$plot <- renderPlotly({
#       fig <- plot_ly(
#         plot_data,
#         x = ~ month,
#         y = ~ price,
#         type = 'scatter',
#         mode = 'lines',
#         name = "Nominal",
#         line = list(
#           color = '#7F7F7F'
#         )
#       ) %>%
#         add_trace(
#           x = ~ month,
#           y = ~ price_adj_2017,
#           type = 'scatter',
#           mode = 'lines',
#           name = "Adjusted (2017)",
#           line = list(
#             color = '#17BECF'
#           )
#         ) %>%
#         # add_trace(
#         #   x = ~ month,
#         #   y = ~ price_adj_2019,
#         #   type = 'scatter',
#         #   mode = 'lines',
#         #   name = "Adjusted (2019)",
#         #   line = list(
#         #     color = '#17cf1d'
#         #   )
#         # ) %>%
#         layout(
#           title = list(text = "Average Resale Prices", x = 0.15, y = 0.9),
#           xaxis = list(
#             type = 'date',
#             tickformat = "%Y-%m"
#           )
#         )
#       
#       fig
#     }) #renderPlot end
#     
#     ### For analysis on inflation and valuation
#     info_data <- plot_data %>%
#       filter(!is.na(price_adj_2017)|!is.na(price_adj_2019)) %>%
#       mutate(month = as.Date(paste(month, "01", sep = "-"), format = "%Y-%m-%d")) %>%
#       slice_max(order_by = month, n = 4) %>%
#       mutate(inflation_rate_2017 = (price - price_adj_2017)/price_adj_2017 * 100,
#              inflation_rate_2019 = (price - price_adj_2019)/price_adj_2019 * 100,
#              valuation_2017 = ((price_adj_2017 - lead(price_adj_2017))/price_adj_2017)*100,
#              valuation_2019 = ((price_adj_2019 - lead(price_adj_2019))/price_adj_2019)*100)
#     
#     inflation_info <- mean(info_data$inflation_rate_2017, na.rm = TRUE) %>%
#       cut(breaks = c(-Inf, -5, -2, 0, 2, 5, Inf),
#           labels = c("Highly Deflatted", "Moderately Deflatef", "Mildly Deflated", "Mildly Inflated", "Moderately Inflated", "Highly Inflated"))
#     
#     # latest gradient only
#     valuation_latest <- info_data %>%
#       filter(!is.na(valuation_2017)) %>%
#       slice_max(month)
#     
#     # average gradient of past 4 month
#     valuation_info <- mean(info_data$valuation_2017, na.rm = TRUE) %>%
#       cut(breaks = c(-Inf, -5, -2, 0, 2, 5, Inf),
#           labels = c("Sharp Decrease", "Moderate Decrease", "Mild Decrease", "Mild Increase", "Moderate Increase", "High Increase"))
#     
#     # Info boxes on Inflation
#     output$inflationInfo <- renderUI(
#       
#       if (inflation_info == "Highly Inflated") {
#         div(
#           div(style = "height:10vh; width:75%; background-color: #F1F3F2; border-style:solid; border-color:#F30B0B; border-width:10px; border-radius:10px; text-align:center;",
#               h4(strong("Housing Price")),
#               h5(inflation_info)
#           ),
#           p("Information provided is based on data from the latest 4 months")
#         )
#         
#       } else if (inflation_info == "Moderately Inflated") {
#         div(
#           div(style = "height:10vh; width:75%; background-color: #F1F3F2; border-style:solid; border-color:#EDFF04; border-width:10px; border-radius:10px; text-align:center;",
#               h4(strong("Housing Price")),
#               h5(inflation_info)
#           ),
#           p("Information provided is based on data from the latest 4 months")
#         )
#       } else {
#         div(
#           div(style = "height:10vh; width:75%; background-color: #F1F3F2; border-style:solid; border-color:#36FF04; border-width:10px; border-radius:10px; text-align:center;",
#               h4(strong("Housing Price")),
#               h5(inflation_info)
#           ),
#           p("Information provided is based on data from the latest 4 months")
#         )
#       }
#     )
#     
#     # Info boxes on Valuation
#     output$valuationInfo <- renderUI(
#       
#       if (valuation_info == "Sharp Decrease") {
#         div(
#           div(style = "height:10vh; width:75%; background-color: #F1F3F2; border-style:solid; border-color:#F30B0B; border-width:10px; border-radius:10px; text-align:center;",
#               h4(strong("Change in Property Value")),
#               h5(valuation_info)
#           ),
#           p("Information provided is based on data from the latest 4 months")
#         )
#       } else if (valuation_info == "Moderate Decrease") {
#         div(
#           div(style = "height:10vh; width:75%; background-color: #F1F3F2; border-style:solid; border-color:#EDFF04; border-width:10px; border-radius:10px; text-align:center;",
#               h4(strong("Change in Property Value")),
#               h5(valuation_info)
#           ),
#           p("Information provided is based on data from the latest 4 months")
#         )
#         
#       } else {
#         div(
#           div(style = "height:10vh; width:75%; background-color: #F1F3F2; border-style:solid; border-color:#36FF04; border-width:10px; border-radius:10px; text-align:center;",
#               h4(strong("Change in Property Value")),
#               h5(valuation_info)
#           ),
#           p("Information provided is based on data from the latest 4 months")
#         )
#         
#       }
#     )
#     
#   },ignoreInit = FALSE, ignoreNULL = FALSE)#observeEvent end
#   
#   #=======================#
# 
# }
# 
# shinyApp(ui = ui, server = server)
