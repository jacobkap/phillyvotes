

wards_map <- function() {
  wards <- readOGR("shiny_data/philly_political_wards/Political_Wards.shp")
  wards <- leaflet(wards) %>%
    addTiles(options = providerTileOptions(minZoom = 10)) %>%
    addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                opacity = 1.0, fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "white",
                                                    weight = 2,
                                                    bringToFront = TRUE),
                layerId=wards$WARD_NUM) %>%
    setView(lng = -75.1, lat = 40, zoom = 10) %>%
    setMaxBounds(-75.224159881,39.9964773281, -74.9506328757,39.9964773281)
wards
}


