# This code will install necessary packages on your system if you don't already have them. Don't run this script unless you're willing to do that!

#1 Load necessary packages (installing if needed)
if(!require(pacman)){
  install.packages("pacman")
}

pacman::p_load(sf, leaflet, leaflet.extras, leaflet.extras2, dplyr, glue, htmlwidgets)

#2 I'll  want to include current and new City Councilors in my map popup, so create two "lookup vectors" with that info.

# Lookup vectors for city councilors:
districts <- as.character(1:9)
current_councilors <- c("Christine Long",
                        "Cesar Stewart-Morales",
                        "Adam Steiner",
                        "Michael Cannon",
                        "Robert Case",
                        "Philip Ottaviani Jr.",
                        "Margareth Basilio Shepard",
                        "John Stefanini",
                        "Tracy Bryant"
)
names(current_councilors) <- districts

adopted_councilors <- c("Christine Long",
                        "Cesar Stewart-Morales",
                        "Adam Steiner or Mary Kate Feeney",
                        "Michael Cannon",
                        "Noval Alexander",
                        "Philip Ottaviani Jr.",
                        "Leora Mallach",
                        "John Stefanini",
                        "Tracy Bryant"
)
names(adopted_councilors) <- districts

#3 Import precinct shapefiles into R as sf objects
current_shapefile <- "data/Framingham_Districts_and_Precincts_2018/geo_export_e698cdf7-8644-4283-b652-bdd34e2c4db6.shp"
adopted_shapefile <- "data/precincts_12072021/precincts_12072021.shp"

Current <- sf::read_sf(current_shapefile) %>%
  mutate(
    Precinct = stringr::str_replace(precinct, "P", ""),
    District = as.character(district),
    id = glue::glue("District {District} Precinct {Precinct}"),
  ) %>%
  arrange(District, Precinct) %>%
  sf::st_transform("WGS84")
Current$Councilor <- current_councilors[Current$District]

Adopted <- sf::read_sf(adopted_shapefile) %>%
  mutate(
    Precinct = as.character(PRECINCT),
    District = case_when(
      Precinct %in% c("22", "23", "24") ~ "1",
      Precinct %in% c("25", "26", "27") ~ "2",
      Precinct %in% c("19", "20", "21") ~ "3",
      Precinct %in% c("16", "17", "18") ~ "4",
      Precinct %in% c("10", "15", "11") ~ "5",
      Precinct %in% c("7", "8", "9") ~ "6",
      Precinct %in% c("12", "13", "14") ~ "7",
      Precinct %in% c("4", "5", "6") ~ "8",
      Precinct %in% c("1", "2", "3") ~ "9",
      TRUE ~ "Other"
    ),
    id = glue::glue("District {District} Precinct {Precinct}")
  ) %>%
  arrange(District, Precinct) %>%
  sf::st_transform("WGS84")
Adopted$Councilor <- adopted_councilors[Adopted$District]

#3 Create map palettes and popups

my_colors <- c("#332288FF", "#88CCEEFF", "#44AA99FF", "#117733FF", "#999933FF", "#DDCC77FF", "#661100FF", "#CC6677FF", "#6699CCFF")

current_palette <- colorFactor(my_colors, sort(unique(Current$District)))
adopted_palette <- colorFactor(my_colors, sort(unique(Adopted$District)))

current_popup <- glue::glue("<strong>Current District: {Current$District}</strong><br />Current Precinct: {Current$Precinct}<br />Current District Councilor: {Current$Councilor}") %>% 
  lapply(htmltools::HTML)
adopted_popup <- glue::glue("<strong>New District: {Adopted$District}</strong><br />New Precinct: {Adopted$Precinct}<br />New District Councilor: {Adopted$Councilor}") %>% 
  lapply(htmltools::HTML)

#4 Generate leaflet maps with side-by-side slider and address search and save to my_maps variable

my_maps <- leaflet() %>%
  addMapPane("left", zIndex = 0) %>%
  addMapPane("right", zIndex = 0) %>%
  addProviderTiles(providers$CartoDB.Positron, group = "current", layerId = "Current",
                   options = pathOptions(pane = "left")
  ) %>%
  addProviderTiles(providers$CartoDB.Positron, group = "adopted", layerId = "Adopted",
                   options = pathOptions(pane = "right")
  ) %>%
  addPolygons(data = Current, stroke = TRUE, smoothFactor = 0.2, fillOpacity = 0.0, 
              fillColor = ~current_palette(Current$District), color = "black", weight = 2,
              group = "Current",
              options = pathOptions(pane = "left")
  )  %>%
  addPolygons(data = Current, stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.7, 
              fillColor = ~current_palette(Current$District), label = current_popup, 
              group = "Current",
              options = pathOptions(pane = "left")
  ) %>%
  addPolygons(data = Adopted, stroke = TRUE, smoothFactor = 0.2, fillOpacity = 0.0, 
              fillColor = ~adopted_palette(Adopted$District), color = "black", weight = 2,
              group = "Adopted",
              options = pathOptions(pane = "right"))  %>%
  addPolygons(data = Adopted, stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.7, 
              fillColor = ~adopted_palette(Adopted$District), label = adopted_popup, 
              group = "Adopted",
              options = pathOptions(pane = "right")) %>%
  addLayersControl(position = "topright", options = layersControlOptions(collapsed = FALSE, overlayGroups = c("Current", "Adoptedt"))) %>%
  addSidebyside(layerId = "sidecontrols",
                rightId = "Adopted",
                leftId = "Current"
  ) %>%
  addLegend(colors = my_colors[1:length(unique(Adopted$District))], values = sort(unique(Adopted$District)), labels = sort(unique(Adopted$District)), position = "topright", title = "District", opacity = 0.6) %>%
  addResetMapButton() %>%
  addSearchOSM(options = searchOptions(autoCollapse = TRUE, minLength = 6))

#5 Save my_maps object with maps as a self-contained HTML file
saveWidget(my_maps, "maps.html")

