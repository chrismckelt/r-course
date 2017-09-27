# Peer-graded Assignment: R Markdown and Leaflet
# https://www.coursera.org/learn/data-products/peer/NLn0H/r-markdown-and-leaflet
#Create a web page using R Markdown that features a map created with Leaflet.

#Host your webpage on either GitHub Pages, RPubs, or NeoCities.

#Your webpage must contain the date that you created the document, and it must contain a map created with Leaflet. We would love to see you show off your creativity!

suppressMessages(rm(list = ls()))
suppressMessages(setwd("C:/dev/r-course/9-data-products"))
source('c:/dev/r-course/include.r')

using("shiny")
using("shinyjs")
using("leaflet")

# ==== fonction allowing geolocalisation
jsCode <- '
shinyjs.geoloc = function() {
    navigator.geolocation.getCurrentPosition(onSuccess, onError);
    function onError (err) {
        Shiny.onInputChange("geolocation", false);
    }
    function onSuccess (position) {
        setTimeout(function () {
            var coords = position.coords;
            console.log(coords.latitude + ", " + coords.longitude);
            Shiny.onInputChange("geolocation", true);
            Shiny.onInputChange("lat", coords.latitude);
            Shiny.onInputChange("long", coords.longitude);
        }, 5)
    }
};
'


# ==== server
server <- function(input, output) {

    # Basic map 
    output$map <- renderLeaflet({
        leaflet() %>%
          setView(lng = 0, lat = 0, zoom = 2) %>%
        addProviderTiles("Esri.WorldImagery")
    })

    # Find geolocalisation coordinates when user clicks
    observeEvent(input$geoloc, {
        js$geoloc()
    })


    # zoom on the corresponding area
    observe({
        if (!is.null(input$lat)) {
            map <- leafletProxy("map")
            dist <- 0.2
            lat <- input$lat
            lng <- input$long
            map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
        }
    })
}

# ==== UI
ui <- fluidPage(
    
# Tell shiny we will use some Javascript
    useShinyjs(),
    extendShinyjs(text = jsCode),
    h1("Coursera - Developing Data Products - Assignment 1"),
    h2("Author: Chris McKelt"),
    h2("27th September 2017"),
# One button and one map
    br(),
    actionButton("geoloc", "Localize me", class = "btn btn-primary", onClick = "shinyjs.geoloc()"),
    leafletOutput("map", height = "600px")
)

shinyApp(ui = ui, server = server)
 