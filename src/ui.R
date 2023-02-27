library(shiny)

source("./components/Amenities_page.R")
source("./components/Resale_page.R")
source("./components/Recent_page.R")
source("./components/Home_page.R")


ui <- navbarPage(
                 title = strong("propertal"),
                 id = "main_nav",
                 tabHome,
                 tabAmenities,
                 tabResale,
                 tabRecent
                 )