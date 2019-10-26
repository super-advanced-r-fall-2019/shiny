library(shiny)
library(dplyr)
library(leaflet)
library(lubridate)
load("trees.Rdata")

ui <- pageWithSidebar(
  headerPanel("Mapping with leaflet"),
  sidebarPanel(
      radioButtons("decade", label = h3("Decade planted:"),
                   choices = list("1950s" = 1950,
                                  "1960s" = 1960,
                                  "1970s" = 1970,
                                  "1980s" = 1980,
                                  "1990s" = 1990,
                                  "2000s" = 2000,
                                  "2010s" = 2010),
                   selected = 1950)
  ),
  mainPanel(
    leafletOutput("mymap")
  )
)

server <- function(input, output) {
  # render leaflet map of trees planted in seattle in selected decate
  # boring looking leaf indicates tree on private property
  # fun swooshy leaf indicates tree on public property
  # zoom in to see individual trees rather than clusters
  # hover over icon to see common name of tree
  output$mymap <- renderLeaflet({
    # make custom icon
    privLeafIcon <- makeIcon(
      iconUrl = "_ionicons_svg_md-leaf.svg",
      iconWidth = 38, iconHeight = 95,
      iconAnchorX = 22, iconAnchorY = 94
    )
    # make custom icon
    publicLeafIcon <- makeIcon(
      iconUrl = "_ionicons_svg_ios-leaf.svg",
      iconWidth = 38, iconHeight = 95,
      iconAnchorX = 22, iconAnchorY = 94
    )
    # make list of custom icon
    LeafIcons <- iconList(private = privLeafIcon, public = publicLeafIcon)
    # create clustered map of trees with sorted icons and common name visible on hover
    leaflet(data = Trees[Trees$decade == input$decade, ]) %>%
      addTiles() %>%
      addMarkers(~longitude, ~latitude, icon = ~LeafIcons[private_vs_public],
                 label = ~COMMON_NAME,
                 clusterOptions = markerClusterOptions())
  })
}

# run app
shinyApp(ui, server)

