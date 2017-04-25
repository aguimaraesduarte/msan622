library(shiny)
library(leaflet)

setwd("~/Desktop/")

ui <- fluidPage(
  leafletOutput("mymap", width = "100%", height = 800)
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  d <- read.csv("schools_math_eng_scores.csv")
  
  max_score <- max(d$score)
  
  pal <- colorNumeric("RdYlGn", domain = sort(unique(d$score)), n = length(unique(d$score)))
  
  output$mymap <- renderLeaflet({
    
    m <- leaflet() %>%
      addTiles() %>%
      addCircleMarkers(data = d, lng = ~longitude, lat = ~latitude,
                       color = ~pal(score), fillOpacity = 0.7, 
                       radius = ~1.5*score,
                       popup = ~paste("<style>",
                                      "div.leaflet-popup-content-wrapper {opacity: 0.9; background:#cccccc;}",
                                      "</style>",
                                      "<h3>", school_name, "</h3>",
                                      "<h4>", grades, "</h4>",
                                      "<h5>Address: ", address, " (", neighb, ")", "</h5>",
                                      "<h4>Score: ", score, " out of ", max_score, "</h4>",
                                      "<h5>", math_percent, "% pass CAASPP Math</h5>",
                                      "<h5>", eng_percent, "% pass CAASPP English</h5>",
                                      sep = "")) %>%
      addLegend(pal = pal, values = d$score, title = "Score") %>%
      addProviderTiles(providers$CartoDB.Positron)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
