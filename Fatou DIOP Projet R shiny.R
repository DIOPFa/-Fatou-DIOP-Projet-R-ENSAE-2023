library(shiny)
library(leaflet) #permet de créer des cartes interactives
library(sf)
library(rnaturalearth)
library(dbscan) #utilisé pour regrouper des données spatiales en fonction de leur densité.
library(cluster) #de diviser un ensemble de données en sous-groupes
library(ggplot2)



# Chargement des données d'intension csv
west_africa<- read.csv("ACLED-Western_Africa.csv")

# divisons  les  données en clusters
clusters <- dbscan(west_africa[, c("latitude", "longitude")], eps = 0.2, minPts = 4)
west_africa$cluster <- as.factor(clusters$cluster)

# Vérification de l'existence de la variable 'annee'
if (!"annee" %in% colnames(west_africa)) {
  stop("La variable 'annee' n'est pas présente dans les données.")
}

# UI
library(shiny)
library(shinydashboard)
library(leaflet)

# Interface utilisateur
ui <- dashboardPage( #tructure globale du tableau de bord
  dashboardHeader(title = "Événement Map"),#l'en-tête du tableau de bord
  dashboardSidebar( # créer la barre latérale d'un tableau 
    sidebarMenu(
      menuItem("Carte du monde par évènements", icon = icon("globe-americas"), tabName = "map_tab"),
      menuItem("Filtrer les événements", icon = icon("filter"), tabName = "filter_tab")
    )  #pour créer des éléments de menu cliquables
  ),
  dashboardBody(
    # Thème du tableau de bord
    skin = "pink",
    
    tabItems(
      # Onglet - Carte du monde par évènements
      tabItem(
        tabName = "map_tab",
        fluidRow(
          box(
            title = "Choisir un pays",
            checkboxGroupInput("event_filter", "Choisir un pays :", choices = unique(west_africa$pays), selected = unique(west_africa$pays)),
            br(),
            actionButton("filter_button", "Filtrer", icon = icon("search"))
          ),
          box(
            title = "Carte du monde",
            leafletOutput("map")
          )
        )
      ),
      
      # Onglet - Filtre des événements
      tabItem(
        tabName = "filter_tab",
        fluidRow(
          box(
            title = "Filtrer les événements",
            selectInput("country_filter", "Choisir un pays :", choices = unique(west_africa$pays)),
            br(),
            selectInput("event_type_filter", "Choisir un type d'événement :", choices = unique(west_africa$type)),
            br(),
            sliderInput("year_filter", "Choisir une année :", min = min(west_africa$annee), max = max(west_africa$annee), value = c(min(west_africa$annee), max(west_africa$annee)))
          ),
          box(
            title = "Carte filtrée",
            leafletOutput("filtered_map")
          )
        )
      )
    )
  )
)



# Server
server <- function(input, output, session) {
  
  # Carte de l'Afrique de l'Ouest
  # Carte par pays
  output$map <- renderLeaflet({
    filtered <- subset(west_africa, pays %in% input$event_filter)
    
    leaflet(filtered) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = 5,
        color = ~cluster,
        opacity = 0.8,
        fillOpacity = 0.8,
        label = ~paste("Pays :", pays, "<br>Type :", type, "<br>Année :", annee),
        clusterOptions = markerClusterOptions()
      )
  })
  
  # Filtrage des événements
  filteredData <- reactive({
    subset(west_africa, pays == input$country_filter & type == input$event_type_filter & annee >= input$year_filter[1] & annee <= input$year_filter[2])
  })
  
  # Carte filtrée
  output$filtered_map <- renderLeaflet({
    filtered <- filteredData()
    
    leaflet(filtered) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = 5,
        color = ~cluster,
        opacity = 0.8,
        fillOpacity = 0.8,
        label = ~paste("Pays :", pays, "<br>Type :", type, "<br>Année :", annee),
        clusterOptions = markerClusterOptions()
      )
  })
   
     
  
}

# Lancement de l'application Shiny
shinyApp(ui = ui, server = server)



















