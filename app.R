library(sf)
library(shiny)
library(leaflet)
library(dplyr)
library(tidyr)
library(rlang)
library(stringr)
library(ggplot2)
library(rsconnect)


ui <- fluidPage(
  
  # loading fonts (doesn't work in css file for some reason); big shoulders, roboto, lora
  tags$head(
    tags$style(HTML("
                    @import url('https://fonts.googleapis.com/css2?family=Big+Shoulders:opsz,wght@10..72,100..900&display=swap');
                    @import url('https://fonts.googleapis.com/css2?family=Roboto:ital,wght@0,100..900;1,100..900&display=swap');
                    @import url('https://fonts.googleapis.com/css2?family=Lora:ital,wght@0,400..700;1,400..700&display=swap')
                    "))
  ),
  
  ## import style sheet
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style_app.css")
  ),
  
  ## main body
  withTags({
    
    div(class = "main-container",
        
        div(class = "title-container",
            p("CHICAGO",
              span(class = "red-title",
                   "NEIGHBORHOOD"),
              span(class = "white-title",
                   "EXPLORER"))
        ), ## / title-container
        
        div(class = "content-container",
            
            div(class = "map-container",
                
                leafletOutput('chicago.map', width = "40vw", height = "95vh")
                
            ), ## / map-container
            
            div(class = "box-container",
                
                div(class = "info-box symbology-selection-box",
                    selectInput("user.symbology",
                                label = "Change map symbology:",
                                choices = c("None",
                                            "Population (2015)",
                                            "Population (2023)",
                                            "Median Age (2015)",
                                            "Median Age (2023)",
                                            "Median Household Income (2015)",
                                            "Median Household Income (2023)",
                                            "Minorities Percent (2015)",
                                            "Minorities Percent (2023)",
                                            "Renters Percent (2015)",
                                            "Renters Percent (2023)",
                                            "Family Households Percent (2015)",
                                            "Family Households Percent (2023)",
                                            "Median Rent (2015)",
                                            "Median Rent (2023)",
                                            "Median Home Value (2015)",
                                            "Median Home Value (2023)"),
                                multiple = F,
                                width = "100%")),
                
                div(class = "info-box selection-box",
                    p(textOutput('text.selection') %>%
                        tagAppendAttributes(class = "box-element-header"))),
                
                div(class = "top-boxes",
                    div(class = "info-box top-left",
                        p(class = "box-element-header",
                          "Social"),
                        p(tableOutput("table.social") %>%
                            tagAppendAttributes(class = "box-element-content"))),
                    div(class = "info-box top-right",
                        p(class = "box-element-header",
                          "Transit"),
                        p(tableOutput("table.transit") %>%
                            tagAppendAttributes(class = "box-element-content")))
                ), ## / top-boxes
                
                div(class = "info-box bottom-large",
                    p(class = "box-element-header",
                      "Demographics"),
                    p(tableOutput("table.demographics") %>%
                        tagAppendAttributes(class = "box-element-content")))
                
            ) ## / box-container
            
        ) ## / content-container
        
    ) ## / main-container
    
  })
  
)

server <- function(input, output, session) {
  
  ## Read in geospatial layers
  chicago.boundary <- st_read("www/1_dependencies/processed/PackagedLayers.gpkg",
                              layer = "chicago")
  community.areas <- st_read("www/1_dependencies/processed/PackagedLayers.gpkg",
                             layer = "community_areas") %>%
    mutate(id = row_number(.)) %>%
    arrange(id)
  cta.l.stations <- st_read("www/1_dependencies/processed/PackagedLayers.gpkg",
                            layer = "cta_l_stations")
  cta.l.lines <- st_read("www/1_dependencies/processed/PackagedLayers.gpkg",
                         layer = "cta_l_lines") %>%
    mutate(line_color = c("Blue", "Brown", "Green", "Multiple", "Orange", "Pink", "Purple", "Red", "Yellow"))
  cta.bus.stations <- st_read("www/1_dependencies/processed/PackagedLayers.gpkg",
                              layer = "cta_bus_stations")
  divvy.stations <- st_read("www/1_dependencies/processed/PackagedLayers.gpkg",
                            layer = "divvy_stations")
  bike.routes <- st_read("www/1_dependencies/processed/PackagedLayers.gpkg",
                         layer = "bike_routes")
  parks <- st_read("www/1_dependencies/processed/PackagedLayers.gpkg",
                   layer = "parks")
  
  ## Read in non-geospatial data
  building.permits <- read.csv("www/1_dependencies/processed/BuildingPermits.csv")
  business.licenses <- read.csv("www/1_dependencies/processed/BusinessLicenses.csv")
  crime <- read.csv("www/1_dependencies/processed/Crime.csv")
  transit.stats <- read.csv("www/1_dependencies/processed/TransitStats.csv")
  citywide.census.data <- read.csv("www/1_dependencies/processed/CitywideCensusData.csv")
  
  ## Create palette for cta lines
  cta.lines.pal <- colorFactor(
    palette = c("blue", "brown", "darkgreen", "black", "orange", "pink", "purple", "red", "yellow"),
    domain = cta.l.lines$abbr
  )
  
  ## Create initial map
  output$chicago.map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = TRUE,
                                     zoomSnap = 0.25,
                                     zoomDelta = 0.25)) %>%
      setView(lng = -87.725,
              lat = 41.840,
              zoom = 10.5) %>%
      addProviderTiles(provider = providers$Esri.WorldTopoMap) %>%
      ## Chicago Boundary
      addPolygons(data = chicago.boundary,
                  color = "black",
                  weight = 2,
                  fillOpacity = 0,
                  options = pathOptions(clickable = FALSE)) %>%
      ## Community Areas
      addPolygons(data = community.areas,
                  color = "black",
                  weight = 1,
                  fillOpacity = 0,
                  fillColor = "white",
                  layerId = ~id,
                  group = "Community Areas") %>%
      ## Parks
      addPolygons(data = parks,
                  weight = 1,
                  color = "black",
                  fillColor = "darkgreen",
                  opacity = 0.75,
                  fillOpacity = 0.75,
                  popup = paste0(parks$park),
                  group = "Parks") %>%
      ## CTA L Stations
      addMarkers(data = cta.l.stations,
                 icon = makeIcon(
                   iconUrl = "www/2_resources/railway-station.png",
                   iconWidth = 10, iconHeight = 10,
                   iconAnchorX = 0, iconAnchorY = 0),
                 popup = paste0('<strong>', cta.l.stations$station_name, '</strong>', ", ",
                                cta.l.stations$station_line),
                 group = "CTA L (Stations)") %>%
      ## CTA L Lines
      addPolylines(data = cta.l.lines,
                   opacity = 0.75,
                   color = ~cta.lines.pal(abbr),
                   popup = paste0(cta.l.lines$line_color, " Line"),
                   group = "CTA L (Lines)") %>%
      ## CTA Bus Stations
      addMarkers(data = cta.bus.stations,
                 icon = makeIcon(
                   iconUrl = "www/2_resources/bus-station.png",
                   iconWidth = 30, iconHeight = 30,
                   iconAnchorX = 0, iconAnchorY = 0),
                 popup = paste0('<strong>', cta.bus.stations$station_name, '</strong>', ", ",
                                cta.bus.stations$route),
                 clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE),
                 group = "CTA Bus Stations") %>%
      ## Divvy Stations
      addMarkers(data = divvy.stations,
                 icon = makeIcon(
                   iconUrl = "www/2_resources/bike.png",
                   iconWidth = 30, iconHeight = 30,
                   iconAnchorX = 0, iconAnchorY = 0),
                 popup = paste0(divvy.stations$station_name),
                 clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE),
                 group = "Divvy Stations") %>%
      ## Layer Controls
      addLayersControl(
        overlayGroups = c("Parks", "CTA L (Stations)", "CTA L (Lines)", "CTA Bus Stations", "Divvy Stations"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      ## Hide Context Layers on startup
      hideGroup("Parks") %>%
      hideGroup("CTA L (Stations)") %>%
      hideGroup("CTA L (Lines)") %>%
      hideGroup("CTA Bus Stations") %>%
      hideGroup("Divvy Stations")
  })
  
  
  ## Reactive Outputs
  rv <- reactiveVal()
  
  ### On map click, set rv and highlight clicked neighborhood
  observeEvent(input$chicago.map_shape_click, {
    if (input$chicago.map_shape_click$group == "Community Areas") {
      rv(input$chicago.map_shape_click$id)
      if(!is.null(rv())) {
        click <- input$chicago.map_shape_click$id
        selection <- community.areas %>% filter(id == click)
        print(input$chicago.map_shape_click$group)
        leafletProxy("chicago.map") %>%
          removeShape("selected") %>%
          addPolygons(
            data = selection,
            color = "cyan",
            fillColor = "lightblue",
            weight = 5,
            fillOpacity = 0.5,
            layerId = "selected",
            group = "Community Areas"
          )
      }
    }
  })
  
  ### On map zoom, enlarge CTA Rail icons
  observeEvent(input$chicago.map_zoom, {
    print(input$chicago.map_zoom)
    
    new.size <- case_when((input$chicago.map_zoom <= 10.5) ~10,
                          (input$chicago.map_zoom > 10.5 & input$chicago.map_zoom <= 11.5) ~15,
                          (input$chicago.map_zoom > 11.5 & input$chicago.map_zoom <= 12.5) ~20,
                          (input$chicago.map_zoom > 12.5 & input$chicago.map_zoom <= 13.5) ~25,
                          (input$chicago.map_zoom > 13.5) ~30)
    
    leafletProxy("chicago.map") %>%
      clearGroup("CTA L (Stations)") %>%
      ## CTA L Stations
      addMarkers(data = cta.l.stations,
                 icon = makeIcon(
                   iconUrl = "www/2_resources/railway-station.png",
                   iconWidth = new.size, iconHeight = new.size,
                   iconAnchorX = 0, iconAnchorY = 0),
                 popup = paste0('<strong>', cta.l.stations$station_name, '</strong>', ", ",
                                cta.l.stations$station_line),
                 group = "CTA L (Stations)")
  })
  
  ### On selectInput, change map symbology
  observeEvent(input$user.symbology, {
    
    if (input$user.symbology != "None") {
      
      new.symbology <- input$user.symbology %>%
        gsub(pattern = "\\(", replacement = "") %>%
        gsub(pattern = "\\)", replacement = "") %>%
        gsub(pattern = " ", replacement = "_") %>%
        str_to_lower()
      
      bin.sizes <- as.numeric(c(0,
                                quantile(community.areas[[new.symbology]], 0.25),
                                quantile(community.areas[[new.symbology]], 0.50),
                                quantile(community.areas[[new.symbology]], 0.75),
                                quantile(community.areas[[new.symbology]], 1)))
      
      newPal <- colorBin("Purples", domain = community.areas[[new.symbology]], bins = bin.sizes)
      
      ## For Population (123,456)
      if (grepl("population", new.symbology)) {
        leafletProxy("chicago.map") %>%
          clearGroup("Community Areas") %>%
          clearControls() %>%
          addPolygons(
            data = community.areas,
            color = "black",
            weight = 1,
            opacity = 1,
            fillColor = newPal(community.areas[[new.symbology]]),
            fillOpacity = 0.8,
            layerId = ~id,
            group = "Community Areas"
          ) %>%
          addLegend(pal = newPal, 
                    values = community.areas[[new.symbology]], 
                    group = "Community Areas",
                    labFormat = labelFormat(digits = 0,
                                            big.mark = ",")) %>%
          ## Hide Context Layers on startup
          hideGroup("Parks") %>%
          hideGroup("CTA L (Stations)") %>%
          hideGroup("CTA L (Lines)") %>%
          hideGroup("CTA Bus Stations") %>%
          hideGroup("Divvy Stations")
      }
      
      ## For Median Age (12.3)
      else if (grepl("median_age", new.symbology)) {
        leafletProxy("chicago.map") %>%
          clearGroup("Community Areas") %>%
          clearControls() %>%
          addPolygons(
            data = community.areas,
            color = "black",
            weight = 1,
            opacity = 1,
            fillColor = newPal(community.areas[[new.symbology]]),
            fillOpacity = 0.8,
            layerId = ~id,
            group = "Community Areas"
          ) %>%
          addLegend(pal = newPal, 
                    values = community.areas[[new.symbology]], 
                    group = "Community Areas",
                    labFormat = labelFormat(digits = 1)) %>%
          ## Hide Context Layers on startup
          hideGroup("Parks") %>%
          hideGroup("CTA L (Stations)") %>%
          hideGroup("CTA L (Lines)") %>%
          hideGroup("CTA Bus Stations") %>%
          hideGroup("Divvy Stations")
      }
      
      ## For dollars ($12,345)
      else if (grepl("income", new.symbology) |
               grepl("rent", new.symbology) |
               grepl("value", new.symbology)) {
        leafletProxy("chicago.map") %>%
          clearGroup("Community Areas") %>%
          clearControls() %>%
          addPolygons(
            data = community.areas,
            color = "black",
            weight = 1,
            opacity = 1,
            fillColor = newPal(community.areas[[new.symbology]]),
            fillOpacity = 0.8,
            layerId = ~id,
            group = "Community Areas"
          ) %>%
          addLegend(pal = newPal, 
                    values = community.areas[[new.symbology]], 
                    group = "Community Areas",
                    labFormat = labelFormat(prefix = "$",
                                            digits = 0,
                                            big.mark = ",")) %>%
          ## Hide Context Layers on startup
          hideGroup("Parks") %>%
          hideGroup("CTA L (Stations)") %>%
          hideGroup("CTA L (Lines)") %>%
          hideGroup("CTA Bus Stations") %>%
          hideGroup("Divvy Stations")
      }
      
      ## For percents (12%)
      else if (grepl("percent", new.symbology)) {
        leafletProxy("chicago.map") %>%
          clearGroup("Community Areas") %>%
          clearControls() %>%
          addPolygons(
            data = community.areas,
            color = "black",
            weight = 1,
            opacity = 1,
            fillColor = newPal(community.areas[[new.symbology]]),
            fillOpacity = 0.8,
            layerId = ~id,
            group = "Community Areas"
          ) %>%
          addLegend(pal = newPal, 
                    values = community.areas[[new.symbology]], 
                    group = "Community Areas",
                    labFormat = labelFormat(suffix = "%",
                                            digits = 0,
                                            transform = function(x) x * 100)) %>%
          ## Hide Context Layers on startup
          hideGroup("Parks") %>%
          hideGroup("CTA L (Stations)") %>%
          hideGroup("CTA L (Lines)") %>%
          hideGroup("CTA Bus Stations") %>%
          hideGroup("Divvy Stations")
      }
    }
    else {
      leafletProxy("chicago.map") %>%
        clearGroup("Community Areas") %>%
        clearControls() %>%
        addPolygons(data = community.areas,
                    color = "black",
                    weight = 1,
                    fillOpacity = 0,
                    fillColor = "white",
                    layerId = ~id,
                    group = "Community Areas") %>%
        ## Hide Context Layers on startup
        hideGroup("Parks") %>%
        hideGroup("CTA L (Stations)") %>%
        hideGroup("CTA L (Lines)") %>%
        hideGroup("CTA Bus Stations") %>%
        hideGroup("Divvy Stations")
    }
  })
  
  ### Update selection box
  output$text.selection <- renderText({
    tryCatch(
      expr = {
        if (is.null(rv()) | rv() == "selected") {
          paste0("Click on a neighborhood to view its stats.")
        }
        else {
          paste0(community.areas$community[rv()])
        }
      },
      error = function(e) {
        paste0("Click on a neighborhood to view its stats.")
      }
    )
  })
  
  ### Update social table
  output$table.social <- renderTable({
    tryCatch(
      expr = {
        selected.community <- community.areas$community[which(community.areas$id == rv())]
        
        if(is_empty(selected.community)) {
          table.out <- data.frame(
            Indicators = c("Grocery Stores", "Restaurants & Bars", 
                           "No. of Crimes in 2024", "No. of Crimes in 2024 per 1,000 people", 
                           "New Construction Permits since 2010"),
            Citywide = c(
              sum(business.licenses$Grocery.Store, na.rm = T) %>%
                round() %>%
                format(nsmall = 0, big.mark = ","),
              sum(business.licenses$Restaurant...Bar, na.rm = T) %>%
                round() %>%
                format(nsmall = 0, big.mark = ","),
              sum(crime$crime_count[which(crime$year == 2024)], na.rm = T) %>%
                round() %>%
                format(nsmall = 0, big.mark = ","),
              (sum(crime$crime_count[which(crime$year == 2024)], na.rm = T) / 
                 sum(community.areas$population_2023, na.rm = T)  * 1000) %>%
                round(2),
              sum(building.permits$new_constructions[which(building.permits$year >= 2010)]) %>%
                round() %>%
                format(nsmall = 0, big.mark = ",")
            )
          )
        }
        else {
          table.out <- data.frame(
            Indicators = c("Grocery Stores", "Restaurants & Bars", 
                           "No. of Crimes in 2024", "No. of Crimes in 2024 per 1,000 people", 
                           "New Construction Permits since 2010"),
            Neighborhood = c(
              business.licenses$Grocery.Store[which(business.licenses$community == selected.community)] %>%
                round() %>%
                format(nsmall = 0, big.mark = ","),
              business.licenses$Restaurant...Bar[which(business.licenses$community == selected.community)] %>%
                round() %>%
                format(nsmall = 0, big.mark = ","),
              crime$crime_count[which(crime$community == selected.community & crime$year == 2024)] %>%
                round() %>%
                format(nsmall = 0, big.mark = ","),
              (crime$crime_count[which(crime$community == selected.community & crime$year == 2024)] / 
                 community.areas$population_2023[which(community.areas$community == selected.community)] * 1000) %>%
                round(2),
              sum(building.permits$new_constructions[which(building.permits$community == selected.community & building.permits$year >= 2010)], na.rm = T) %>%
                round() %>%
                format(nsmall = 0, big.mark = ",")
            ),
            Citywide = c(
              sum(business.licenses$Grocery.Store, na.rm = T) %>%
                round() %>%
                format(nsmall = 0, big.mark = ","),
              sum(business.licenses$Restaurant...Bar, na.rm = T) %>%
                round() %>%
                format(nsmall = 0, big.mark = ","),
              sum(crime$crime_count[which(crime$year == 2024)], na.rm = T) %>%
                round() %>%
                format(nsmall = 0, big.mark = ","),
              (sum(crime$crime_count[which(crime$year == 2024)], na.rm = T) / 
                 sum(community.areas$population_2023, na.rm = T)  * 1000) %>%
                round(2),
              sum(building.permits$new_constructions[which(building.permits$year >= 2010)]) %>%
                round() %>%
                format(nsmall = 0, big.mark = ",")
            )
          )
        }
        table.out
      },
      error = function(e) {
        table.out <- data.frame(. = ("Something went wrong. Please reload the page and try again! :("))
        table.out
      }
    )
  },
  width = "100%",
  spacing = "xs",
  align = "c",
  )
  
  ### Update transit table
  output$table.transit <- renderTable({
    tryCatch(
      expr = {
        selected.community <- community.areas$community[which(community.areas$id == rv())]
        table.out <- data.frame(
          Indicators = c("Number of L Stations", "L Lines",
                         "Number of Bus Stations", "Bus Routes (top 20 by stations)",
                         "% Commuters using Transit",
                         "Number of Divvy Stations",
                         "Length of Bike Routes"),
          Neighborhood = c(
            transit.stats$l_station_count[which(transit.stats$community == selected.community)],
            transit.stats$l_line[which(transit.stats$community == selected.community)],
            transit.stats$bus_station_count[which(transit.stats$community == selected.community)],
            transit.stats$bus_route[which(transit.stats$community == selected.community)],
            paste0(round(community.areas$transit_users_percent_2023[which(community.areas$community == selected.community)] * 100), "%"),
            transit.stats$divvy_station_count[which(transit.stats$community == selected.community)],
            ifelse(nrow(bike.routes %>% 
                          st_intersection(community.areas %>% filter(community == selected.community))) > 0,
                   paste0(round((bike.routes %>% 
                                   st_intersection(community.areas %>% filter(community == selected.community)) %>%
                                   filter(community == selected.community) %>%
                                   group_by(community) %>%
                                   summarize(length = sum(as.numeric(st_length(geom)))))$length[1] * 0.000621371, 2), " miles"),
                   paste0("0 miles"))
          )
        )
        table.out
      },
      error = function(e) {
        table.out <- data.frame(. = ("Select a neighborhood to view transit stats."))
        table.out
      }
    )
  },
  width = "100%",
  spacing = "xs",
  align = "c",
  )
  
  ## Update demographics table
  output$table.demographics <- renderTable({
    tryCatch(
      expr = {
        selected.community <- community.areas$community[which(community.areas$id == rv())]
        table.out <- data.frame(
          Indicators = c("Population", "Median Age", "Median Income (Infl. Adj.)",
                         "% Minority", "% Renters", "% Family Households",
                         "Median Rent", "Median Home Value"),
          Neighborhood.2023 = c(
            community.areas$population_2023[which(community.areas$community == selected.community)] %>%
              round() %>%
              format(nsmall = 0, big.mark = ","),
            community.areas$median_age_2023[which(community.areas$community == selected.community)] %>%
              round(1),
            paste0("$", community.areas$median_household_income_2023[which(community.areas$community == selected.community)] %>%
                     round() %>%
                     format(nsmall = 0, big.mark = ",")),
            paste0(round(community.areas$minorities_percent_2023[which(community.areas$community == selected.community)] * 100), "%"),
            paste0(round(community.areas$renters_percent_2023[which(community.areas$community == selected.community)] * 100), "%"),
            paste0(round(community.areas$family_households_percent_2023[which(community.areas$community == selected.community)] * 100), "%"),
            paste0("$", community.areas$median_rent_2023[which(community.areas$community == selected.community)] %>%
                     round() %>%
                     format(nsmall = 0, big.mark = ",")),
            paste0("$", community.areas$median_home_value_2023[which(community.areas$community == selected.community)] %>%
                     round() %>%
                     format(nsmall = 0, big.mark = ","))
          ),
          Citywide.2023 = c(
            citywide.census.data$population_2023[1] %>%
              round() %>%
              format(nsmall = 0, big.mark = ","),
            citywide.census.data$median_age_2023[1] %>%
              round(1),
            paste0("$", citywide.census.data$median_household_income_2023[1] %>%
                     round() %>%
                     format(nsmall = 0, big.mark = ",")),
            paste0(round(citywide.census.data$minorities_percent_2023[1] * 100), "%"),
            paste0(round(citywide.census.data$renters_percent_2023[1] * 100), "%"),
            paste0(round(citywide.census.data$family_households_percent_2023[1] * 100), "%"),
            paste0("$", citywide.census.data$median_rent_2023[1] %>%
                     round() %>%
                     format(nsmall = 0, big.mark = ",")),
            paste0("$", citywide.census.data$median_home_value_2023[1] %>%
                     round() %>%
                     format(nsmall = 0, big.mark = ","))
          ),
          `Neighborhood Change c 2015` = c(
            paste0((((community.areas$population_2023[which(community.areas$community == selected.community)] -
                        community.areas$population_2015[which(community.areas$community == selected.community)]) / 
                       community.areas$population_2015[which(community.areas$community == selected.community)]) * 100) %>%
                     round(1), "%"),
            paste0((((community.areas$median_age_2023[which(community.areas$community == selected.community)] -
                        community.areas$median_age_2015[which(community.areas$community == selected.community)]) / 
                       community.areas$median_age_2015[which(community.areas$community == selected.community)]) * 100) %>%
                     round(1), "%"),
            paste0((((community.areas$median_household_income_2023[which(community.areas$community == selected.community)] -
                        community.areas$median_household_income_2015[which(community.areas$community == selected.community)]) / 
                       community.areas$median_household_income_2015[which(community.areas$community == selected.community)]) * 100) %>%
                     round(1), "%"),
            paste0((((community.areas$minorities_percent_2023[which(community.areas$community == selected.community)] -
                        community.areas$minorities_percent_2015[which(community.areas$community == selected.community)]) / 
                       community.areas$minorities_percent_2015[which(community.areas$community == selected.community)]) * 100) %>%
                     round(1), " %-points"),
            paste0((((community.areas$renters_percent_2023[which(community.areas$community == selected.community)] -
                        community.areas$renters_percent_2015[which(community.areas$community == selected.community)]) / 
                       community.areas$renters_percent_2015[which(community.areas$community == selected.community)]) * 100) %>%
                     round(1), " %-points"),
            paste0((((community.areas$family_households_percent_2023[which(community.areas$community == selected.community)] -
                        community.areas$family_households_percent_2015[which(community.areas$community == selected.community)]) / 
                       community.areas$family_households_percent_2015[which(community.areas$community == selected.community)]) * 100) %>%
                     round(1), " %-points"),
            paste0((((community.areas$median_rent_2023[which(community.areas$community == selected.community)] -
                        community.areas$median_rent_2015[which(community.areas$community == selected.community)]) / 
                       community.areas$median_rent_2015[which(community.areas$community == selected.community)]) * 100) %>%
                     round(1), "%"),
            paste0((((community.areas$median_home_value_2023[which(community.areas$community == selected.community)] -
                        community.areas$median_home_value_2015[which(community.areas$community == selected.community)]) / 
                       community.areas$median_home_value_2015[which(community.areas$community == selected.community)]) * 100) %>%
                     round(1), "%")
          ),
          `Citywide Change c 2015` = c(
            paste0((((citywide.census.data$population_2023[1] - citywide.census.data$population_2015[1]) / 
                       citywide.census.data$population_2015[1]) * 100) %>%
                     round(1), "%"),
            paste0((((citywide.census.data$median_age_2023[1] - citywide.census.data$median_age_2015[1]) / 
                       citywide.census.data$median_age_2015[1]) * 100) %>%
                     round(1), "%"),
            paste0((((citywide.census.data$median_household_income_2023[1] - citywide.census.data$median_household_income_2015[1]) / 
                       citywide.census.data$median_household_income_2015[1]) * 100) %>%
                     round(1), "%"),
            paste0(((citywide.census.data$minorities_percent_2023 - citywide.census.data$minorities_percent_2015) * 100) %>% 
                     round(1), " %-points"),
            paste0(((citywide.census.data$renters_percent_2023 - citywide.census.data$renters_percent_2015) * 100) %>% 
                     round(1), " %-points"),
            paste0(((citywide.census.data$family_households_percent_2023 - citywide.census.data$family_households_percent_2015) * 100) %>% 
                     round(1), " %-points"),
            paste0((((citywide.census.data$median_rent_2023[1] - citywide.census.data$median_rent_2015[1]) / 
                       citywide.census.data$median_rent_2015[1]) * 100) %>%
                     round(1), "%"),
            paste0((((citywide.census.data$median_home_value_2023[1] - citywide.census.data$median_home_value_2015[1]) / 
                       citywide.census.data$median_home_value_2015[1]) * 100) %>%
                     round(1), "%")
          )
        )
        table.out <- table.out %>%
          mutate(
            Neighborhood.Change.c.2015 = ifelse(
              substr(Neighborhood.Change.c.2015, 1, 1) == "-",
              Neighborhood.Change.c.2015,
              paste0("+", Neighborhood.Change.c.2015)
            ),
            Citywide.Change.c.2015 = ifelse(
              substr(Citywide.Change.c.2015, 1, 1) == "-",
              Citywide.Change.c.2015,
              paste0("+", Citywide.Change.c.2015)
            )
          )
        table.out
      },
      error = function(e) {
        table.out <- data.frame(
          Indicators = c("Population", "Median Age", "Median Income (Infl. Adj.)",
                         "% Minority", "% Renters", "% Family Households",
                         "Median Rent", "Median Home Value"),
          Citywide.2023 = c(
            citywide.census.data$population_2023[1] %>%
              round() %>%
              format(nsmall = 0, big.mark = ","),
            citywide.census.data$median_age_2023[1] %>%
              round(1),
            paste0("$", citywide.census.data$median_household_income_2023[1] %>%
                     round() %>%
                     format(nsmall = 0, big.mark = ",")),
            paste0(round(citywide.census.data$minorities_percent_2023[1] * 100), "%"),
            paste0(round(citywide.census.data$renters_percent_2023[1] * 100), "%"),
            paste0(round(citywide.census.data$family_households_percent_2023[1] * 100), "%"),
            paste0("$", citywide.census.data$median_rent_2023[1] %>%
                     round() %>%
                     format(nsmall = 0, big.mark = ",")),
            paste0("$", citywide.census.data$median_home_value_2023[1] %>%
                     round() %>%
                     format(nsmall = 0, big.mark = ","))
          ),
          `Citywide Change c 2015` = c(
            paste0((((citywide.census.data$population_2023[1] - citywide.census.data$population_2015[1]) / 
                       citywide.census.data$population_2015[1]) * 100) %>%
                     round(1), "%"),
            paste0((((citywide.census.data$median_age_2023[1] - citywide.census.data$median_age_2015[1]) / 
                       citywide.census.data$median_age_2015[1]) * 100) %>%
                     round(1), "%"),
            paste0((((citywide.census.data$median_household_income_2023[1] - citywide.census.data$median_household_income_2015[1]) / 
                       citywide.census.data$median_household_income_2015[1]) * 100) %>%
                     round(1), "%"),
            paste0(((citywide.census.data$minorities_percent_2023 - citywide.census.data$minorities_percent_2015) * 100) %>% 
                     round(1), " %-points"),
            paste0(((citywide.census.data$renters_percent_2023 - citywide.census.data$renters_percent_2015) * 100) %>% 
                     round(1), " %-points"),
            paste0(((citywide.census.data$family_households_percent_2023 - citywide.census.data$family_households_percent_2015) * 100) %>% 
                     round(1), " %-points"),
            paste0((((citywide.census.data$median_rent_2023[1] - citywide.census.data$median_rent_2015[1]) / 
                       citywide.census.data$median_rent_2015[1]) * 100) %>%
                     round(1), "%"),
            paste0((((citywide.census.data$median_home_value_2023[1] - citywide.census.data$median_home_value_2015[1]) / 
                       citywide.census.data$median_home_value_2015[1]) * 100) %>%
                     round(1), "%")
          )
        )
      }
    )
  },
  width = "100%",
  spacing = "xs",
  align = "c",
  )
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
}

shinyApp(ui, server)