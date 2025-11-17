# function to install packages easily
usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
# install/load packages
usePackage("sf")
usePackage("tigris")
usePackage("tidycensus")
usePackage("dplyr")
usePackage("tidyr")
usePackage("jsonlite")
usePackage("stringr")
options(scipen = 999)
census_api_key(readLines("www/1_dependencies/reference/censuskey.txt"))

# Reading/downloading/formatting administrative boundaries 
## City Boundary (https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-City-Map/ewy2-6yfk)
chicago <- st_read("www/1_dependencies/raw/City_Boundary.geojson") %>%
  st_transform(crs = 4326) %>%
  select(name, geometry) %>%
  `colnames<-`(c("city_name", "geometry"))

## Community Areas (https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Community-Areas/igwz-8jzy/about_data)
community.areas <- st_read("www/1_dependencies/raw/Community_Areas.geojson") %>%
  st_transform(crs = 4326) %>%
  select(community, geometry) %>%
  mutate(community = str_to_title(community))

## Census Tracts (2010 and 2020) (TIGER/Line)
tracts.2010 <- tigris::tracts(
  state = "IL",
  county = "Cook County",
  year = 2011
) %>%
  st_transform(crs = 4326) %>%
  st_intersection(chicago) %>%
  select(GEOID, geometry) %>%
  `colnames<-`(c("geoid_10", "geometry"))

tracts.2020 <- tigris::tracts(
  state = "IL",
  county = "Cook County",
  year = 2021
) %>%
  st_transform(crs = 4326) %>%
  st_intersection(chicago) %>%
  select(GEOID, geometry) %>%
  `colnames<-`(c("geoid_20", "geometry"))

# Reading/downloading formatting map data (geospatial)
## CTA L Stations (https://data.cityofchicago.org/dataset/CTA-L-Rail-Stations-Shapefile/vmyy-m9qj/about_data)
cta.l.stations <- st_read("www/1_dependencies/raw/CTA_L_Stations/CTA_RailStations.shp") %>%
  st_transform(crs = 4326) %>%
  select(LONGNAME, LINES, ADA, geometry) %>%
  `colnames<-`(c("station_name", "station_line", "ada", "geometry"))

## CTA L Lines (https://data.cityofchicago.org/Transportation/CTA-L-Rail-Lines/xbyr-jnvx/about_data)
cta.l.lines <- st_read("www/1_dependencies/raw/CTA_L_Lines.geojson") %>%
  st_transform(crs = 4326) %>%
  select(legend, geometry) %>%
  `colnames<-`(c("abbr", "geometry")) %>%
  group_by(abbr) %>%
  summarize(geometry = st_union(geometry))

## CTA Bus Stations (https://data.cityofchicago.org/Transportation/CTA_BusStops/qs84-j7wh/about_data)
### NOTE: The bus stations have been pre-processed through a spatial join that adds the community area name
cta.bus.stations <- st_read("www/1_dependencies/raw/CTA_Bus_Stations.geojson") %>%
  st_transform(crs = 4326) %>%
  mutate(community = str_to_title(community)) %>%
  select(routesstpg, owlroutes, public_nam, community, geometry) %>%
  `colnames<-`(c("route", "owl_route", "station_name", "community", "geometry"))

## Divvy Stations (https://data.cityofchicago.org/Transportation/Divvy-Bicycle-Stations/bbyy-e7gq/about_data)
### NOTE: The divvy stations have been pre-processed through a spatial join that adds the community area name
divvy.stations <- st_read("www/1_dependencies/raw/Divvy_Stations.geojson") %>%
  st_transform(crs = 4326) %>%
  mutate(community = str_to_title(community)) %>%
  select(station_name, community, geometry)

## Parks (https://data.cityofchicago.org/Parks-Recreation/Parks-Chicago-Park-District-Park-Boundaries-curren/ej32-qgdr)
sf_use_s2(FALSE)
parks <- st_read("www/1_dependencies/raw/Parks.geojson") %>%
  mutate(park = str_to_title(park),
         park_area = as.numeric(st_area(geometry))) %>%
  select(park, park_area, geometry)

## Bike Routes (https://data.cityofchicago.org/Transportation/Bike-Routes/hvv9-38ut)
bike.routes <- st_read("www/1_dependencies/raw/BikeRoutes.geojson") %>%
  mutate(street = str_to_title(street)) %>%
  select(street, geometry)

# Reading/downloading formatting map data (NON-geospatial)
## Crime (2015-present) (https://data.cityofchicago.org/Public-Safety/Crimes-2001-to-Present/ijzp-q8t2/about_data)
### NOTE: The original crime dataset has been pre=processed to add community area data, and to limit to 2015 onwards.
crime <- read.csv("www/1_dependencies/raw/Crimes.csv") %>%
  st_drop_geometry() %>%
  select(Primary.Type, Year, community) %>%
  mutate(community = str_to_title(community)) %>%
  `colnames<-`(c("crime_type", "year", "community")) %>%
  filter(complete.cases(.)) %>%
  mutate(count = 1) %>%
  group_by(year, community) %>%
  summarize(crime_count = sum(count))

## Building Permits (new construction) (https://data.cityofchicago.org/Buildings/Building-Permits/ydr8-5enu/about_data)
### NOTE: The original building permits data has been pre-processed to add community area data and filter to "Active" or "Complete" permits.
building.permits <- read.csv("www/1_dependencies/raw/BuildingPermits.csv") %>%
  filter(!is.na(LATITUDE) &
           !is.na(LONGITUDE) &
           PERMIT_TYPE == "PERMIT - NEW CONSTRUCTION") %>%
  mutate(year = substr(ISSUE_DATE, 7, 10),
         count = 1,
         community.x = str_to_title(community.x)) %>%
  group_by(year, community.x) %>%
  summarize(new_constructions = sum(count)) %>%
  `colnames<-`(c("year", "community", "new_constructions"))

## Business Licenses
business.licenses <- read.csv("www/1_dependencies/raw/BusinessLicenses.csv") %>%
  select(LICENSE.DESCRIPTION, ADDRESS, DOING.BUSINESS.AS.NAME, BUSINESS.ACTIVITY, community.x) %>%
  mutate(community = str_to_title(community.x)) %>%
  select(-community.x) %>%
  filter(complete.cases(.)) %>%
  distinct() %>%
  mutate(
    Category = ifelse(LICENSE.DESCRIPTION %in% c("Consumption on Premises - Incidental Activity",
                                                 "Food - Shared Kitchen",
                                                 "Food - Shared Kitchen - Supplemental",
                                                 "Mobile Food Dispenser",
                                                 "Mobile Food License",
                                                 "Outdoor Patio",
                                                 "Retail Food - Seasonal Lakefront Food Establishment",
                                                 "Retail Food Establishment"),
                      "Restaurant / Bar",
                      ""),
    Category = ifelse(LICENSE.DESCRIPTION %in% c("Package Goods", "Wholesale Food Establishment"),
                      "Grocery Store",
                      Category)
  ) %>%
  filter(Category != "") %>%
  select(DOING.BUSINESS.AS.NAME, ADDRESS, community, Category) %>%
  distinct() %>%
  mutate(count = 1) %>%
  pivot_wider(names_from = Category,
              values_from = count) %>%
  mutate(`Grocery Store` = ifelse(is.na(`Grocery Store`), 0, `Grocery Store`)) %>%
  mutate(category = ifelse(`Grocery Store` == 1,
                           "Grocery Store",
                           "Restaurant / Bar")) %>%
  mutate(count = 1) %>%
  group_by(category, community) %>%
  summarize(count = sum(count)) %>%
  pivot_wider(names_from = category,
              values_from = count)
  

## CTA L Stations Count
cta.l.stations.nongeo <- cta.l.stations %>%
  st_intersection(community.areas) %>%
  st_drop_geometry() %>%
  mutate(count = 1) %>%
  group_by(community) %>%
  summarize(station_line = paste(station_line, collapse = ", "),
            station_counts = sum(count)) %>%
  ungroup() %>%
  mutate(
    red = ifelse(grepl("Red", station_line), 1, 0),
    blue = ifelse(grepl("Blue", station_line), 1, 0),
    green = ifelse(grepl("Green", station_line), 1, 0),
    yellow = ifelse(grepl("Yellow", station_line), 1, 0),
    purple = ifelse(grepl("Purple", station_line), 1, 0),
    orange = ifelse(grepl("Orange", station_line), 1, 0),
    pink = ifelse(grepl("Pink", station_line), 1, 0),
    brown = ifelse(grepl("Brown", station_line), 1, 0),
    stations = ""
  )

for (i in 1:nrow(cta.l.stations.nongeo)) {
  temp <- ""
  if(cta.l.stations.nongeo$red[i] == 1) {
    temp <- paste0(temp, ", Red")
  }
  if(cta.l.stations.nongeo$blue[i] == 1) {
    temp <- paste0(temp, ", Blue")
  }
  if(cta.l.stations.nongeo$green[i] == 1) {
    temp <- paste0(temp, ", Green")
  }
  if(cta.l.stations.nongeo$yellow[i] == 1) {
    temp <- paste0(temp, ", Yellow")
  }
  if(cta.l.stations.nongeo$purple[i] == 1) {
    temp <- paste0(temp, ", Purple")
  }
  if(cta.l.stations.nongeo$orange[i] == 1) {
    temp <- paste0(temp, ", Orange")
  }
  if(cta.l.stations.nongeo$pink[i] == 1) {
    temp <- paste0(temp, ", Pink")
  }
  if(cta.l.stations.nongeo$brown[i] == 1) {
    temp <- paste0(temp, ", Brown")
  }
  cta.l.stations.nongeo$stations[i] <- substr(temp, 2, nchar(temp))
}

cta.l.stations.nongeo <- cta.l.stations.nongeo %>%
  select(community, station_counts, stations)

## CTA Bus Stations Count
cta.bus.stations.nongeo <- cta.bus.stations %>%
  st_drop_geometry() %>%
  mutate(routes.split = strsplit(route, ",")) %>%
  unnest(routes.split) %>%
  mutate(count = 1) %>%
  group_by(community, routes.split) %>%
  summarize(count = sum(count, na.rm = T)) %>%
  filter(!is.na(routes.split) &
           trimws(routes.split) != "") %>%
  ungroup() %>%
  group_by(community) %>%
  top_n(19, count) %>%
  dplyr::summarise(routes.clean =  paste(routes.split, collapse = ", "),
            count = sum(count)) %>%
  filter(!is.na(community))
  
## Divvy Station Count
divvy.stations.nongeo <- divvy.stations %>%
  st_drop_geometry() %>%
  mutate(count = 1) %>%
  group_by(community) %>%
  summarize(count = sum(count))

## Combine CTA bus, l, and divvy stats
transit.stats <- community.areas %>%
  st_drop_geometry() %>%
  left_join(cta.bus.stations.nongeo %>%
              `colnames<-`(c("community", "bus_route", "bus_station_count")),
            by = "community") %>%
  left_join(cta.l.stations.nongeo %>%
              `colnames<-`(c("community", "l_station_count", "l_line")) %>%
              select(community, l_line, l_station_count),
            by = "community") %>%
  left_join(divvy.stations.nongeo %>%
              `colnames<-`(c("community", "divvy_station_count")),
            by = "community") %>%
  mutate(
    bus_route = ifelse(is.na(bus_route), "None", bus_route),
    bus_station_count = ifelse(is.na(bus_station_count), 0, bus_station_count),
    l_line = ifelse(is.na(l_line), "None", l_line),
    l_station_count = ifelse(is.na(l_station_count), 0, l_station_count),
    divvy_station_count = ifelse(is.na(divvy_station_count), 0 , divvy_station_count)
  )

# Create community-area-to-tract crosswalk(s)
tract.crosswalk.2010 <- tracts.2010 %>%
  mutate(orig_area = as.numeric(st_area(geometry))) %>%
  st_intersection(community.areas) %>%
  mutate(new_area = as.numeric(st_area(geometry))) %>%
  st_drop_geometry() %>%
  mutate(aw_percent = new_area / orig_area) %>%
  select(geoid_10, community, aw_percent) %>%
  filter(aw_percent > 0.05)

tract.crosswalk.2020 <- tracts.2020 %>%
  mutate(orig_area = as.numeric(st_area(geometry))) %>%
  st_intersection(community.areas) %>%
  mutate(new_area = as.numeric(st_area(geometry))) %>%
  st_drop_geometry() %>%
  mutate(aw_percent = new_area / orig_area) %>%
  select(geoid_20, community, aw_percent) %>%
  filter(aw_percent > 0.05)

# Pull census data and aggregate to community areas
census.data.2015.aw <- get_acs(
  geography = "tract",
  state = "IL",
  county = "Cook County",
  year = 2015,
  survey = "acs5",
  variables = c("B01001_001E", # Total Population,
                "B03002_001E", # RE_Total
                "B03002_003E", # RE_White Non Hispanic
                "B08006_001E", # MTW_Total
                "B08006_002E", # MTW_Car/Truck/Van
                "B11001_001E", # Total Households
                "B11001_002E", # Family Households
                "B25003_001E", # Tenure_Total
                "B25003_003E" # Tenure_Renters
                )) %>%
  select(GEOID, variable, estimate) %>%
  pivot_wider(names_from = variable,
              values_from = estimate) %>%
  left_join(tract.crosswalk.2010,
            by = c("GEOID" = "geoid_10")) %>%
  mutate_each(funs(.*aw_percent), starts_with("B")) %>%
  select(-GEOID, -aw_percent) %>%
  group_by(community) %>%
  summarize_all(sum)

census.data.2015.nonaw <- get_acs(
  geography = "tract",
  state = "IL",
  county = "Cook County",
  year = 2015,
  survey = "acs5",
  variables = c("B01001_001E", # Population
                "B01002_001E", # Median Age
                "B19013_001E", # Median Household Income
                "B25064_001E", # Median Gross Rent
                "B25077_001E" # Median Home Value
  )) %>%
  select(GEOID, variable, estimate) %>%
  pivot_wider(names_from = variable,
              values_from = estimate) %>%
  left_join(tract.crosswalk.2010,
            by = c("GEOID" = "geoid_10")) %>%
  mutate(wt = B01001_001 * aw_percent) %>%
  select(-GEOID, -B01001_001, -aw_percent) %>%
  group_by(community) %>%
  summarize(B01002_001 = weighted.mean(B01002_001, wt, na.rm = T),
            B19013_001 = weighted.mean(B19013_001, wt, na.rm = T),
            B25064_001 = weighted.mean(B25064_001, wt, na.rm = T),
            B25077_001 = weighted.mean(B25077_001, wt, na.rm = T))

census.data.2015 <- census.data.2015.aw %>%
  left_join(census.data.2015.nonaw, by = "community") %>%
  mutate(
    population = B01001_001,
    median_age = B01002_001,
    minorities_percent = 1 - (B03002_003 / B03002_001),
    transit_users_percent = B08006_002 / B08006_001,
    family_households_percent = B11001_002 / B11001_001,
    renters_percent = B25003_003 / B25003_001,
    median_household_income = B19013_001,
    median_home_value = B25077_001,
    median_rent = B25064_001
  ) %>%
  select(community,
         population,
         median_age,
         minorities_percent,
         transit_users_percent,
         family_households_percent,
         renters_percent,
         median_household_income,
         median_home_value,
         median_rent) %>%
  `colnames<-`(c("community", paste0(colnames(.), "_2015")[2:length(colnames(.))])) %>%
  filter(!is.na(community))

census.data.2023.aw <- get_acs(
  geography = "tract",
  state = "IL",
  county = "Cook County",
  year = 2023,
  survey = "acs5",
  variables = c("B01001_001E", # Total Population,
                "B03002_001E", # RE_Total
                "B03002_003E", # RE_White Non Hispanic
                "B08006_001E", # MTW_Total
                "B08006_002E", # MTW_Car/Truck/Van
                "B11001_001E", # Total Households
                "B11001_002E", # Family Households
                "B25003_001E", # Tenure_Total
                "B25003_003E" # Tenure_Renters
  )) %>%
  select(GEOID, variable, estimate) %>%
  pivot_wider(names_from = variable,
              values_from = estimate) %>%
  left_join(tract.crosswalk.2020,
            by = c("GEOID" = "geoid_20")) %>%
  mutate_each(funs(.*aw_percent), starts_with("B")) %>%
  select(-GEOID, -aw_percent) %>%
  group_by(community) %>%
  summarize_all(sum)

census.data.2023.nonaw <- get_acs(
  geography = "tract",
  state = "IL",
  county = "Cook County",
  year = 2023,
  survey = "acs5",
  variables = c("B01001_001E", # Population
                "B01002_001E", # Median Age
                "B19013_001E", # Median Household Income
                "B25064_001E", # Median Gross Rent
                "B25077_001E" # Median Home Value
  )) %>%
  select(GEOID, variable, estimate) %>%
  pivot_wider(names_from = variable,
              values_from = estimate) %>%
  left_join(tract.crosswalk.2020,
            by = c("GEOID" = "geoid_20")) %>%
  mutate(wt = B01001_001 * aw_percent) %>%
  select(-GEOID, -B01001_001, -aw_percent) %>%
  group_by(community) %>%
  summarize(B01002_001 = weighted.mean(B01002_001, wt, na.rm = T),
            B19013_001 = weighted.mean(B19013_001, wt, na.rm = T),
            B25064_001 = weighted.mean(B25064_001, wt, na.rm = T),
            B25077_001 = weighted.mean(B25077_001, wt, na.rm = T))

census.data.2023 <- census.data.2023.aw %>%
  left_join(census.data.2023.nonaw, by = "community") %>%
  mutate(
    population = B01001_001,
    median_age = B01002_001,
    minorities_percent = 1 - (B03002_003 / B03002_001),
    transit_users_percent = B08006_002 / B08006_001,
    family_households_percent = B11001_002 / B11001_001,
    renters_percent = B25003_003 / B25003_001,
    median_household_income = B19013_001,
    median_home_value = B25077_001,
    median_rent = B25064_001
  ) %>%
  select(community,
         population,
         median_age,
         minorities_percent,
         transit_users_percent,
         family_households_percent,
         renters_percent,
         median_household_income,
         median_home_value,
         median_rent) %>%
  `colnames<-`(c("community", paste0(colnames(.), "_2023")[2:length(colnames(.))])) %>%
  filter(!is.na(community))

rm(census.data.2015.aw)
rm(census.data.2015.nonaw)
rm(census.data.2023.aw)
rm(census.data.2023.nonaw)

# Pull same census data for citywide
chicago.census.data.2015 <- get_acs(
  geography = "place",
  state = "IL",
  year = 2015,
  survey = "acs5",
  variables = c("B01001_001E", # Total Population,
                "B03002_001E", # RE_Total
                "B03002_003E", # RE_White Non Hispanic
                "B08006_001E", # MTW_Total
                "B08006_002E", # MTW_Car/Truck/Van
                "B11001_001E", # Total Households
                "B11001_002E", # Family Households
                "B25003_001E", # Tenure_Total
                "B25003_003E", # Tenure_Renters
                "B01002_001E", # Median Age
                "B19013_001E", # Median Household Income
                "B25064_001E", # Median Gross Rent
                "B25077_001E" # Median Home Value
  )
) %>%
  filter(NAME == "Chicago city, Illinois") %>%
  select(NAME, variable, estimate) %>%
  pivot_wider(names_from = variable,
              values_from = estimate) %>%
  mutate(
    population = B01001_001,
    median_age = B01002_001,
    minorities_percent = 1 - (B03002_003 / B03002_001),
    transit_users_percent = B08006_002 / B08006_001,
    family_households_percent = B11001_002 / B11001_001,
    renters_percent = B25003_003 / B25003_001,
    median_household_income = B19013_001,
    median_home_value = B25077_001,
    median_rent = B25064_001
  ) %>%
  select(NAME,
         population,
         median_age,
         minorities_percent,
         transit_users_percent,
         family_households_percent,
         renters_percent,
         median_household_income,
         median_home_value,
         median_rent) %>%
  `colnames<-`(c("name", paste0(colnames(.), "_2015")[2:length(colnames(.))]))

chicago.census.data.2023 <- get_acs(
  geography = "place",
  state = "IL",
  year = 2023,
  survey = "acs5",
  variables = c("B01001_001E", # Total Population,
                "B03002_001E", # RE_Total
                "B03002_003E", # RE_White Non Hispanic
                "B08006_001E", # MTW_Total
                "B08006_002E", # MTW_Car/Truck/Van
                "B11001_001E", # Total Households
                "B11001_002E", # Family Households
                "B25003_001E", # Tenure_Total
                "B25003_003E", # Tenure_Renters
                "B01002_001E", # Median Age
                "B19013_001E", # Median Household Income
                "B25064_001E", # Median Gross Rent
                "B25077_001E" # Median Home Value
  )
) %>%
  filter(NAME == "Chicago city, Illinois") %>%
  select(NAME, variable, estimate) %>%
  pivot_wider(names_from = variable,
              values_from = estimate) %>%
  mutate(
    population = B01001_001,
    median_age = B01002_001,
    minorities_percent = 1 - (B03002_003 / B03002_001),
    transit_users_percent = B08006_002 / B08006_001,
    family_households_percent = B11001_002 / B11001_001,
    renters_percent = B25003_003 / B25003_001,
    median_household_income = B19013_001,
    median_home_value = B25077_001,
    median_rent = B25064_001
  ) %>%
  select(NAME,
         population,
         median_age,
         minorities_percent,
         transit_users_percent,
         family_households_percent,
         renters_percent,
         median_household_income,
         median_home_value,
         median_rent) %>%
  `colnames<-`(c("name", paste0(colnames(.), "_2023")[2:length(colnames(.))]))

chicago.census.data <- chicago.census.data.2015 %>%
  left_join(chicago.census.data.2023,
            by = "name")

## adjust all 2015 census data dollar variables for inflation 2015-2023 (1.30 multiplier)
census.data.2015 <- census.data.2015 %>%
  mutate(
    median_household_income_2015 = median_household_income_2015 * 1.30,
    median_home_value_2015 = median_home_value_2015 * 1.30,
    median_rent_2015 = median_rent_2015 * 1.30,
  )

chicago.census.data <- chicago.census.data %>%
  mutate(
    median_household_income_2015 = median_household_income_2015 * 1.30,
    median_home_value_2015 = median_home_value_2015 * 1.30,
    median_rent_2015 = median_rent_2015 * 1.30,
  )

# Create community areas layer with census data joined to it
community.areas.maplayer <- community.areas %>%
  left_join(census.data.2015, by = "community") %>%
  left_join(census.data.2023, by = "community")

# Package mapping layers 
## delete everything in processed folder
unlink("www/1_dependencies/processed/*")
## chicago boundary
st_write(chicago,
         "www/1_dependencies/processed/PackagedLayers.gpkg",
         "chicago")
## community areas with census data
st_write(community.areas.maplayer,
         "www/1_dependencies/processed/PackagedLayers.gpkg",
         "community_areas",
         append = TRUE)
## cta l stations and lines
st_write(cta.l.stations,
         "www/1_dependencies/processed/PackagedLayers.gpkg",
         "cta_l_stations",
         append = TRUE)
st_write(cta.l.lines,
         "www/1_dependencies/processed/PackagedLayers.gpkg",
         "cta_l_lines",
         append = TRUE)
## cta bus stations
st_write(cta.bus.stations,
         "www/1_dependencies/processed/PackagedLayers.gpkg",
         "cta_bus_stations",
         append = TRUE)
## divvy stations
st_write(divvy.stations,
         "www/1_dependencies/processed/PackagedLayers.gpkg",
         "divvy_stations",
         append = TRUE)
## bike routes
st_write(bike.routes,
         "www/1_dependencies/processed/PackagedLayers.gpkg",
         "bike_routes",
         append = TRUE)
## parks
st_write(parks,
         "www/1_dependencies/processed/PackagedLayers.gpkg",
         "parks",
         append = TRUE)

# Write non-geospatial data
write.csv(building.permits,
          "www/1_dependencies/processed/BuildingPermits.csv",
          row.names = F)
write.csv(crime,
          "www/1_dependencies/processed/Crime.csv",
          row.names = F)
write.csv(business.licenses,
          "www/1_dependencies/processed/BusinessLicenses.csv",
          row.names = F)
write.csv(transit.stats,
          "www/1_dependencies/processed/TransitStats.csv",
          row.names = F)
write.csv(chicago.census.data,
          "www/1_dependencies/processed/CitywideCensusData.csv",
          row.names = F)

  