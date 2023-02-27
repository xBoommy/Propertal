library(shiny)

server <- function(input, output, session) {
  #===HOME PAGE===#
  {#===HOME PAGE===#
    observeEvent(input$navigatePrice, {
      updateNavbarPage(session, "main_nav",
                        selected = "tab_resale")
    })
    
    observeEvent(input$navigateAmenities, {
      updateNavbarPage(session, "main_nav",
                        selected = "tab_amenities")
    })
    
    observeEvent(input$navigateRecent, {
      updateNavbarPage(session, "main_nav",
                        selected = "tab_recent")
    })
    #=============================#
  }
  
  #===AMENITIES PAGE===#
  { #===AMENITIES PAGE===#
    # Initialize RADAR plot
    figRadar <- plot_ly(
      type = 'scatterpolar',
      fill = 'toself',
      mode = 'lines'
    ) %>%
      layout(
        polar = list(
          radialaxis = list(
            showgrid = T,
            showline = F,
            showticklabels = F,
            showtickprefix = F,
            ticks = '',
            range = c(0,10)
          ),
          angularaxis = list (
            visible =T,
            showticklabels = T,
            showtickprefix = T,
            ticks = '',
            tickwidth = 1,
            linewidth =2,
            layer = 'below traces'
          )
        ),
        showlegend = TRUE,
        plot_bgcolor = '#00000000',
        paper_bgcolor = 'rgba(0,0,0,0)'
      )
    
    # Reactive to store updates
    updateAddress1 <- reactiveValues(update = NULL)
    updateAddress2 <- reactiveValues(update = NULL)
    updatePreference <- reactiveValues(update = NULL)
    
    # Reactive to store amenities
    amenitiesAdd1 <- reactiveValues(data = NULL)
    amenitiesAdd2 <- reactiveValues(data = NULL)
    preferencesAdd <- reactiveValues(data = NULL)
    
    # categories
    category <- c("HEALTHCARE",
                  "GROCERIES",
                  "DINING",
                  "TRANSPORTATION",
                  "EDUCATION (SECONDARY)",
                  "EDUCATION (PRIMARY)",
                  "EDUCATION (POST SECONDARY)",
                  "EDUCATION (ELEMENTARY)", 
                  "PARKS") %>% sort()
    
    # Initialize Leaflet MAP
    amenities_Map <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron)
    
    # Compare Button Event
    observeEvent(input$compare, {
      # Read in address input
      address1 <- isolate(input$address1in)
      address2 <- isolate(input$address2in)
      
      if (address1 != ""){
        address1_coord <- get_coord(address1)
        address1_df <- nearest_amenities(address1_coord) %>% arrange(category)
        
        amenitiesAdd1$data = address1_df
        
        # Update radar with address1 info
        updateAddress1$update <- function(base){
          result = base %>% add_trace(
            r = c(address1_df$score, address1_df$score[1]),
            theta = c(address1_df$category, address1_df$category[1]),
            name = address1,
            connectgaps = TRUE,
            fillcolor = rgb(200, 0, 0, 50, maxColorValue = 255),
            line = list(color = "black")
          )
          
          return(result)
        }
        
        # Update map with address1 info
        amenities_Map <- amenities_Map %>%
          addCircleMarkers(
            lng = address1_coord$lon,
            lat = address1_coord$lat,
            label = address1,
            group = address1,
            radius = 5,
            color = rgb(200, 0, 0, 255, maxColorValue = 255)
          )
        
        # Update map with address1 amenities
        amenities_Map <- amenities_Map %>%
          addMarkers(
            lng = address1_df$longitude,
            lat = address1_df$latitude,
            label = address1_df$name,
            group = address1,
            icon = icon_list1[address1_df$category]
          )
      } else {
        amenitiesAdd1$data = NULL
      }
      
      
      if (address2 != ""){
        address2_coord <- get_coord(address2)
        address2_df <- nearest_amenities(address2_coord) %>% arrange(category)
        
        amenitiesAdd2$data = address2_df
        
        # Function to update radar with address2 info
        updateAddress2$update <- function(base){
          result = base %>% add_trace(
            r = c(address2_df$score, address2_df$score[1]),
            theta = c(address2_df$category, address2_df$category[1]),
            name = address2,
            connectgaps = TRUE,
            fillcolor = rgb(0, 0, 200, 50, maxColorValue = 255),
            line = list(color = "black")
          )
          
          return(result)
        }
        
        # Update map with address2 info
        amenities_Map <- amenities_Map %>%
          addCircleMarkers(
            lng = address2_coord$lon,
            lat = address2_coord$lat,
            label = address2,
            group = address2,
            radius = 5,
            color = rgb(0, 0, 200, 255, maxColorValue = 255)
          )
        
        # Update map with address2 amenities
        amenities_Map <- amenities_Map %>%
          addMarkers(
            lng = address2_df$longitude,
            lat = address2_df$latitude,
            label = address2_df$name,
            group = address2,
            icon = icon_list2[address2_df$category]
          )
      } else {
        amenitiesAdd2$data = NULL
      }
      
      ### Output Amenities map
      output$amenitiesMap <- renderLeaflet({
        amenities_Map %>%
          addLayersControl(
            overlayGroups =c(address1, address2),
            options = layersControlOptions(collapsed=FALSE)
          )
      })
      
      ### Output Address1 Amenities Data
      output$address1Amenities <- renderDataTable(
        
        if (!is.null(amenitiesAdd1$data)) {
          amenitiesAdd1$data %>%
            select(-label,-label2, -country, -latitude, -longitude, -score) %>%
            mutate(postal_code = as.character(postal_code), dist = round(dist,1),
                   address = ifelse(category == "TRANSPORTATION", "", address),
                   postal_code = ifelse(category == "TRANSPORTATION", "", postal_code)) %>%
            rename(Category = category) %>%
            rename(Name = name) %>%
            rename(Address = address) %>%
            rename(`Postal Code` = postal_code) %>%
            rename(`Distance (m)` = dist)
          
        },
        options = list(scrollX = TRUE, dom = 't', scrollY = "60vh")
      )
      
      ### Output Address2 Amenities Data
      output$address2Amenities <- renderDataTable(
        
        if (!is.null(amenitiesAdd2$data)) {
          amenitiesAdd2$data %>%
            select(-label,-label2, -country, -latitude, -longitude, -score) %>%
            mutate(postal_code = as.character(postal_code), dist = round(dist,1),
                   address = ifelse(category == "TRANSPORTATION", "", address),
                   postal_code = ifelse(category == "TRANSPORTATION", "", postal_code)) %>%
            rename(Category = category) %>%
            rename(Name = name) %>%
            rename(Address = address) %>%
            rename(`Postal Code` = postal_code) %>%
            rename(`Distance (m)` = dist)
        },
        options = list(scrollX = TRUE, dom = 't', scrollY = "60vh")
      )
      
    },ignoreInit = FALSE, ignoreNULL = FALSE) # End of Compare button event
    
    # Compare Button Event (SUMMARY)
    observeEvent((input$compare | input$preference), {
      max_score <- length(category)*10
      
      # Address 1 SUMMARY
      {
        if (!is.null(amenitiesAdd1$data)) {
          add1Scoring_data <- amenitiesAdd1$data %>%
            group_by(category) %>%
            slice_max(score, with_ties = FALSE) %>%
            ungroup() %>%
            select(category, score) %>%
            arrange(category) %>%
            left_join(preferencesAdd$data, by = "category", suffix = c("", "pref"))
          
          add1_totalscore <- data.frame(label = c("score", ""),
                                        points = c(sum(add1Scoring_data$score), max_score-sum(add1Scoring_data$score)))
        } else {
          add1Scoring_data <- data.frame(category, score = NA, pref_score = NA) %>% arrange(category)
          
          add1_totalscore <- data.frame(label = c("score", ""),
                                        points = c(0, max_score))
        }
        
        output$summaryAdd1overall <- renderPlotly({
          plot_ly(data = add1_totalscore, values = ~points, labels = ~label,
                  textinfo = 'none', marker = list(colors = c('#8078FE', '#FFFFFFaa')), sort = FALSE) %>%
            add_pie(hole = 0.6) %>%
            layout(title = FALSE,  showlegend = FALSE,
                   margin = list(l = 20, r = 20),
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   plot_bgcolor  = "rgba(0, 0, 0, 0)",
                   paper_bgcolor = "rgba(0, 0, 0, 0)") %>%
            style(hoverinfo = 'none') %>%
            layout(p, annotations=list(text=paste(add1_totalscore$points[1], "/", max_score),
                                       font = list(size = 24),
                                       "showarrow" = FALSE))
        })
        
        output$summaryAdd1breakdown <- renderTable({
          add1Scoring_data %>%
            mutate(pref_score = paste(score, "/", pref_score),
                   score = paste(score, "/", 10)) %>%
            rename(Category = category, Score = score,  `Score (Pref)` = pref_score)
        })
        
        output$summaryAdd1fit <- renderText({
          
          if (!is.na(add1Scoring_data$score)){
            compare_fit1 <- add1Scoring_data %>%
              mutate(compare_score = ifelse((score - pref_score) >= 0, pref_score, score))
            
            output1fit <- paste0(round(sum(compare_fit1$compare_score)/sum(compare_fit1$pref_score) * 100, 2), "%")
            output1fit
          } else {
            NA
          }
          
        })
      }
      
      # Address 2 SUMMARY
      {
        if (!is.null(amenitiesAdd2$data)) {
          add2Scoring_data <- amenitiesAdd2$data %>%
            group_by(category) %>%
            slice_max(score, with_ties = FALSE) %>%
            ungroup() %>%
            select(category, score) %>%
            arrange(category) %>%
            left_join(preferencesAdd$data, by = "category", suffix = c("", "pref"))
          
          add2_totalscore <- data.frame(label = c("score", ""),
                                        points = c(sum(add2Scoring_data$score), max_score-sum(add2Scoring_data$score)))
        } else {
          add2Scoring_data <- data.frame(category, score = NA, pref_score = NA) %>% arrange(category)
          
          add2_totalscore <- data.frame(label = c("score", ""),
                                        points = c(0, max_score))
        }
        
        output$summaryAdd2overall <- renderPlotly({
          plot_ly(data = add2_totalscore, values = ~points, labels = ~label,
                  textinfo = 'none', marker = list(colors = c('#8078FE', '#FFFFFFaa')), sort = FALSE) %>%
            add_pie(hole = 0.6) %>%
            layout(title = FALSE,  showlegend = FALSE,
                   margin = list(l = 20, r = 20),
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   plot_bgcolor  = "rgba(0, 0, 0, 0)",
                   paper_bgcolor = "rgba(0, 0, 0, 0)") %>%
            style(hoverinfo = 'none') %>%
            layout(p, annotations=list(text=paste(add2_totalscore$points[1], "/", max_score),
                                       font = list(size = 24),
                                       "showarrow" = FALSE))
        })
        
        output$summaryAdd2breakdown <- renderTable({
          add2Scoring_data %>%
            mutate(pref_score = paste(score, "/", pref_score),
                   score = paste(score, "/", 10)) %>%
            rename(Category = category, Score = score,  `Score (Pref)` = pref_score)
        })
        
        output$summaryAdd2fit <- renderText({
          
          if (!is.na(add2Scoring_data$score)){
            compare_fit2 <- add2Scoring_data %>%
              mutate(compare_score = ifelse((score - pref_score) >= 0, pref_score, score))
            
            output2fit <- paste0(round(sum(compare_fit2$compare_score)/sum(compare_fit2$pref_score) * 100, 2), "%")
            output2fit
          } else {
            NA
          }
          
        })
      }
      
    },ignoreInit = FALSE, ignoreNULL = FALSE)
    
    # Add preferences Button Event
    observeEvent(input$preference, {
      transport_pref <- isolate(input$transport_pref)
      healthcare_pref <- isolate(input$healthcare_pref)
      dining_pref <- isolate(input$dining_pref)
      groceries_pref <- isolate(input$groceries_pref)
      parks_pref <- isolate(input$parks_pref)
      schools_elem_pref <- isolate(input$schools_elem_pref)
      schools_pri_pref <- isolate(input$schools_pri_pref)
      schools_sec_pref <- isolate(input$schools_sec_pref)
      schools_post_pref <- isolate(input$schools_post_pref)
      
      preference_score <- c(dining_pref,
                            schools_elem_pref,
                            schools_post_pref,
                            schools_pri_pref,
                            schools_sec_pref,
                            groceries_pref,
                            healthcare_pref,
                            parks_pref,
                            transport_pref)
      
      # Store preferences
      preferencesAdd$data <- data.frame(pref_score = preference_score, category)
      
      # Function to update radar with preference info
      updatePreference$update <- function(base){
        result = base %>% add_trace(
          r = c(preference_score, preference_score[1]),
          theta = c(category, category[1]),
          name = "Your preferences",
          connectgaps = TRUE,
          fillcolor = rgb(0, 200, 0, 50, maxColorValue = 255),
          line = list(color = "black")
        )
        
        return(result)
      }
      
    },ignoreInit = FALSE, ignoreNULL = FALSE) # End of Add preferences button event
    
    
    ### Output Radar plot
    output$radarPlot <- renderPlotly({
      if (!is.null(updatePreference$update)) {
        figRadar <- updatePreference$update(figRadar)
      }
      
      if (!is.null(updateAddress1$update)) {
        figRadar <- updateAddress1$update(figRadar)
      }
      
      if (!is.null(updateAddress2$update)) {
        figRadar <- updateAddress2$update(figRadar)
      }
      
      figRadar
    })
    
    #=======================#
  }
  
  #===RESALE PAGE===#
  {#===RESALE PAGE===#
    
    observeEvent(input$search,{
      ## Function to filter data based on input
      filter_data <- function(df) {
        filtered_df = town_filter(df, input$town_select)
        filtered_df = type_filter(filtered_df, input$type_select)
        filtered_df = model_filter(filtered_df, input$model_select)
        filtered_df = area_filter(filtered_df, input$area_select[1], input$area_select[2])
        filtered_df = level_filter(filtered_df, input$level_select[1], input$level_select[2])
        filtered_df = lease_filter(filtered_df, input$lease_select[1], input$lease_select[2])
        
        return(filtered_df)
      }
      
      # filtering data based on input filter options
      filtered_data <-  filter_data(formatted_data)
      
      # plot data
      plot_data <-  filtered_data %>%
        group_by(month) %>%
        summarize(price = mean(resale_price),
                  price_adj_2017 = mean(resale_price_adj_2017),
                  price_adj_2019 = mean(resale_price_adj_2019))
      
      output$plot <- renderPlotly({
        fig <- plot_ly(
          plot_data,
          x = ~ month,
          y = ~ price,
          type = 'scatter',
          mode = 'lines',
          name = "Nominal",
          line = list(
            color = '#7F7F7F'
          )
        ) %>%
          add_trace(
            x = ~ month,
            y = ~ price_adj_2017,
            type = 'scatter',
            mode = 'lines',
            name = "Adjusted (2017)",
            line = list(
              color = '#17BECF'
            )
          ) %>%
          # add_trace(
          #   x = ~ month,
          #   y = ~ price_adj_2019,
          #   type = 'scatter',
          #   mode = 'lines',
          #   name = "Adjusted (2019)",
          #   line = list(
          #     color = '#17cf1d'
          #   )
          # ) %>%
          layout(
            title = list(text = "Average Resale Prices", x = 0.15, y = 0.9),
            xaxis = list(
              type = 'date',
              tickformat = "%Y-%m"
            )
          )
        
        fig
      }) #renderPlot end
      
      ### For analysis on inflation and valuation
      info_data <- plot_data %>%
        filter(!is.na(price_adj_2017)|!is.na(price_adj_2019)) %>%
        mutate(month = as.Date(paste(month, "01", sep = "-"), format = "%Y-%m-%d")) %>%
        slice_max(order_by = month, n = 4) %>%
        mutate(inflation_rate_2017 = (price - price_adj_2017)/price_adj_2017 * 100,
               inflation_rate_2019 = (price - price_adj_2019)/price_adj_2019 * 100,
               valuation_2017 = ((price_adj_2017 - lead(price_adj_2017))/price_adj_2017)*100,
               valuation_2019 = ((price_adj_2019 - lead(price_adj_2019))/price_adj_2019)*100)
      
      inflation_info <- mean(info_data$inflation_rate_2017, na.rm = TRUE) %>%
        cut(breaks = c(-Inf, -5, -2, 0, 2, 5, Inf),
            labels = c("Highly Deflated", "Moderately Deflated", "Mildly Deflated", "Mildly Inflated", "Moderately Inflated", "Highly Inflated"))
      
      # latest gradient only
      valuation_latest <- info_data %>%
        filter(!is.na(valuation_2017)) %>%
        slice_max(month)
      
      # average gradient of past 4 month
      valuation_info <- mean(info_data$valuation_2017, na.rm = TRUE) %>%
        cut(breaks = c(-Inf, -5, -2, 0, 2, 5, Inf),
            labels = c("Sharp Decrease", "Moderate Decrease", "Mild Decrease", "Mild Increase", "Moderate Increase", "Sharp Increase"))
      
      # Info boxes on Inflation
      output$inflationInfo <- renderUI({
        
        if (nrow(info_data) < 4) {
          div(
            div(style = "height:10vh; width:80%; background-color: rgba(255,255,255,0.7); border-radius:10px 30px; display: flex; flex-direction: row; overflow: hidden;",
                div(style = "height: 100%; width: 25%; background-color:#C8C8CB; "),
                div(style = "height: 100%; width: 75%; background-color:#FFFFFF; display: flex; flex-direction: column; text-align:left; justify-content: center; padding-left: 5%",
                    div( #Grey
                      h4(strong("Housing Price")),
                      h5("Insufficient data available")
                    )
                )
                
            ),
            div(style = "width:60%; margin: 0 auto;",
                p("*Information provided is aggregated on data from the latest 4 months")
            )
          )
        } else if (inflation_info == "Highly Inflated") {
          div(
            div(style = "height:10vh; width:80%; background-color: rgba(255,255,255,0.7); border-radius:10px 30px; display: flex; flex-direction: row; overflow: hidden;",
                div(style = "height: 100%; width: 25%; background-color:#FB6962; "),
                div(style = "height: 100%; width: 75%; background-color:#FFFFFF; display: flex; flex-direction: column; text-align:left; justify-content: center; padding-left: 5%",
                    div( #RED
                      h4(strong("Housing Price")),
                      h5(inflation_info)
                    )
                )
                
            ),
            div(style = "width:60%; margin: 0 auto;",
                p("*Information provided is aggregated on data from the latest 4 months")
            )
          )
        } else if (inflation_info == "Moderately Inflated") {
          div(
            div(style = "height:10vh; width:80%; background-color: rgba(255,255,255,0.7); border-radius:10px 30px; display: flex; flex-direction: row; overflow: hidden;",
                div(style = "height: 100%; width: 25%; background-color:#FCFC99; "),
                div(style = "height: 100%; width: 75%; background-color:#FFFFFF; display: flex; flex-direction: column; text-align:left; justify-content: center; padding-left: 5%",
                    div( #YELLOW
                      h4(strong("Housing Price")),
                      h5(inflation_info)
                    )
                )
                
            ),
            div(style = "width:60%; margin: 0 auto;",
                p("*Information provided is aggregated on data from the latest 4 months")
            )
          )
        } else {
          div(
            div(style = "height:10vh; width:80%; background-color: rgba(255,255,255,0.7); border-radius:10px 30px; display: flex; flex-direction: row; overflow: hidden;",
                div(style = "height: 100%; width: 25%; background-color:#79DE79; "),
                div(style = "height: 100%; width: 75%; background-color:#FFFFFF; display: flex; flex-direction: column; text-align:left; justify-content: center; padding-left: 5%",
                    div( #GREEN
                      h4(strong("Housing Price")),
                      h5(inflation_info)
                    )
                )
                
            ),
            div(style = "width:60%; margin: 0 auto;",
                p("*Information provided is aggregated on data from the latest 4 months")
            )
          )
        }
        
      })
      
      # Info boxes on Valuation
      output$valuationInfo <- renderUI({
        
        if (nrow(info_data) < 4) {
          div(
            div(style = "height:10vh; width:80%; background-color: rgba(255,255,255,0.7); border-radius:10px 30px; display: flex; flex-direction: row; overflow: hidden;",
                div(style = "height: 100%; width: 25%; background-color:#C8C8CB; "),
                div(style = "height: 100%; width: 75%; background-color:#FFFFFF; display: flex; flex-direction: column; text-align:left; justify-content: center; padding-left: 5%",
                    div( #Grey
                      h4(strong("Change in Property Value")),
                      h5("Insufficient data available")
                    )
                )
                
            ),
            div(style = "width:60%; margin: 0 auto;",
                p("*Information provided is aggregated on data from the latest 4 months")
            )
          )
        } else if (valuation_info == "Sharp Decrease") {
          div(
            div(style = "height:10vh; width:80%; background-color: rgba(255,255,255,0.7); border-radius:10px 30px; display: flex; flex-direction: row; overflow: hidden;",
                div(style = "height: 100%; width: 25%; background-color:#FB6962; "),
                div(style = "height: 100%; width: 75%; background-color:#FFFFFF; display: flex; flex-direction: column; text-align:left; justify-content: center; padding-left: 5%",
                    div( #RED
                      h4(strong("Change in Property Value")),
                      h5(valuation_info)
                    )
                )
                
            ),
            div(style = "width:60%; margin: 0 auto;",
                p("*Information provided is aggregated on data from the latest 4 months")
            )
          )
        } else if (valuation_info == "Moderate Decrease") {
          div(
            div(style = "height:10vh; width:80%; background-color: rgba(255,255,255,0.7); border-radius:10px 30px; display: flex; flex-direction: row; overflow: hidden;",
                div(style = "height: 100%; width: 25%; background-color:#FCFC99; "),
                div(style = "height: 100%; width: 75%; background-color:#FFFFFF; display: flex; flex-direction: column; text-align:left; justify-content: center; padding-left: 5%",
                    div( #YELLOW
                      h4(strong("Change in Property Value")),
                      h5(valuation_info)
                    )
                )
                
            ),
            div(style = "width:60%; margin: 0 auto;",
                p("*Information provided is aggregated on data from the latest 4 months")
            )
          )
        } else {
          div(
            div(style = "height:10vh; width:80%; background-color: rgba(255,255,255,0.7); border-radius:10px 30px; display: flex; flex-direction: row; overflow: hidden;",
                div(style = "height: 100%; width: 25%; background-color:#79DE79; "),
                div(style = "height: 100%; width: 75%; background-color:#FFFFFF; display: flex; flex-direction: column; text-align:left; justify-content: center; padding-left: 5%",
                    div( #GREEN
                      h4(strong("Change in Property Value")),
                      h5(valuation_info)
                    )
                )
                
            ),
            div(style = "width:60%; margin: 0 auto;",
                p("*Information provided is aggregated on data from the latest 4 months")
            )
          )
          
        }
        
      })
      
    },ignoreInit = FALSE, ignoreNULL = FALSE)#observeEvent end
    
    #=======================#
  }
  
  #===RECENT EVENTS PAGE===#
  {#===RECENT EVENTS PAGE===#
    
    output$townSummary <- renderLeaflet(
      townSummary_map
    )
    
    output$typeSummary <- renderPlotly(
      typeSummary_plot
    )
    
    output$modelSummary <- renderPlotly(
      modelSummary_plot
    )
    
    #=======================#
  }
  
  } # End of Server