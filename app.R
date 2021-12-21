library(shiny)
library(shinyWidgets)
library(leaflet)
library(dplyr)
library(dygraphs)
library(DT)
library(readr)
library(shinyjs)
library(shinyBS)
library(htmltools)
library(bsplus)
library(leaflet.extras)
library(dplyr)
library(tidyr)
library(readxl)

# References ####
## Leaftlet.extra 
# search https://github.com/bhaskarvk/leaflet.extras/blob/master/inst/examples/search.R
# https://github.com/bhaskarvk/leaflet.extras/blob/master/R/search.R

# http://www.goring.org/resources/Shiny-leaflet-tutorial.html leaflet global tutorial

# http://www.memoiredeshommes.sga.defense.gouv.fr/fr/article.php?larub=31&titre=presentation-detaillee-des-atlas



# reading data #################################################################





places_geocode <- readRDS("places_geocode12.rdata")
places_geocode <- places_geocode %>%
  mutate(id2 =paste0(category,id))

voyages<- readRDS("voyages2.rdata")

library(readxl)
geocode <- read_excel("voyages_geocode_carfix.xlsx")






# Data wrangling ###############################################################



## LArge to long format. 

exped2 <- voyages %>%
  gather(category, place, ptdepimp:mjslptimp)



exped <- inner_join(exped2, geocode, by =c("place" = "place"))
exped <- exped %>%
  mutate(id2 =paste0(category,id)) %>%
  mutate(color =case_when(category=="ptdepimp" ~ "#31a354", 
                          category=="mjbyptimp" ~ "#3182bd",
                          category=="mjslptimp" ~ "#fd8d3c")
         )

# UI ###########################################################################



# layout :
## FillPage : plein écran
## Absolutepanel : mise en page basée sur plusieurs "absolute panels" pour éviter les "décalages" du texte en fonction de l'affichage de la table reactive des noms de navires
## Absolute panel : définition d'un altitude supérieure à la carte
# Slider chronologique appelé range et etiquetté "chronologie"
# Définition des valeurs min et max et de l'intervalle en fonction de la variable yearam.
# Datable : utilisation de css


ui <- fillPage(
  useShinyjs(),
  
# Style html ####  
## Sidebar ####
  tags$head(tags$style(HTML('
                             #startupModal .modal-body {
                                        margin-top: 0px;
                                        border-radius: 5px;
                                        padding-top: 20px;
                                        padding-right: 20px;
                                        padding-bottom:50px;
                                        padding-left: 20px;
                                       }
                            '))
        ),

  tags$head(tags$style(HTML("
                      @import url('https://fonts.googleapis.com/css?family=Open+Sans:700'); 
                       h1 {
                           font-family: 'Open Sans', sans-serif;
                           color: #428BCA;
                           }
                    
                    "))
          ),

tags$head(tags$style(HTML('
                             #credits {
                          border-radius: 0px;
                          padding-top: 0px;
                          padding-right: 0px;
                          padding-bottom:0px;
                          padding-left: 0px;
                          color: #23527c;
                          background-color: #000000;
font-size: 8px; font-weight: normal;

                          }
                          '))
),


# Map UI ####



  leafletOutput("map", width = "100%", height = "100%"),
modalDialog(id = 'startupModal',
        size = 'l',
        easyClose  =  TRUE ,
        footer  =  modalButton ( "Fermer" ),
        tags$style(type="text/css", "a { font-size: 12px; }"),
        tags$style(type="text/css", "p { font-size: 12px; font-weight: bold; margin-bottom: 0px; }"),
        tags$style(type="text/css", "hr { height: 1px;background: #8c8c8c; opacity: 1;}"),
        tags$h1("Principaux lieux de la traite atlantique"),
        tags$h4("Port de départ des expéditions, lieux d'achat et de débarquement des esclaves"),                
        tags$h5("Carte thématique interactive construite à partir des données de la Trans Atlantic Slave Trade Database (version 2010). L’importance des différents lieux est figuré par des cerces proportionnels au nombre d’expéditions de traite.") ,
        tags$hr(),
        tags$p("Source des données  :  "),tags$a(class ="lien", href="http://www.slavevoyages.org/", "The Trans-Atlantic Slave Trade Database (2010), Emory University, Données historiques (Domaine publique) - Données substituées (cc by-nc)"),
        tags$br(),
        tags$p("Couches de cartes anciennes  : "),
        tags$a("David Rumsey Map Collection", href="https://www.davidrumsey.com/"),
        tags$br(),
        tags$p("Conception : "),
        tags$a("Wilfrid Cariou (2018)", href="http://www.histoire.univ-nantes.fr/", "Nantes Université, Département d'Histoire"),
        tags$br(),
        img(src='index.jpg', width ="60", align = "left")),
tags$br(),
tags$br(),
tags$br(),
tags$head(tags$style("#startupModal .modal-footer{ display:none}")),
tags$head(tags$style("#startupModal .modal-header{ display:none}")),


# config UI ####
  absolutePanel(
    bottom = 10, left = 10,
    id ="chrono",
    style="z-index:500;",
    tags$style(type='text/css',
               ".selectize-dropdown-content{
                                max-height: 100px;
  overflow-y: auto;;
               }"), # altitude de la légende ! https://github.com/rstudio/leaflet/issues/534

# Input chronology ####                  
        
dropdownButton(
    tags$h4("Chronologie"),
    sliderInput("periode", NULL,
                 min(places_geocode$yearam),
                 max(places_geocode$yearam),
                 value = range(places_geocode$yearam),
                step = 1,
                sep = ""),

    
# Input Places choices ####              
      tags$h4("Lieux"),       
     checkboxGroupInput(
       "choix", NULL,
        choices = list("Ports de départ" = "ptdepimp", 
       "Principaux lieux d'achat" = "mjbyptimp", 
       "Principaux lieux de vente" = "mjslptimp"),
       selected = c("ptdepimp", "mjbyptimp", "mjslptimp")
       ),
       tags$h4("Tableau de données"),
       materialSwitch("show_table", label = "Affichage"),

  selectInput(
         "select",
         label =  NULL, 
         names(exped),
         selected = names(exped)[c(1,  44, 2, 3, 9, 12, 53, 63, 68)],
         multiple = TRUE), 
         numericInput("obs", NULL,
            min = 1, max = 15, value = 5, width = "300px"),


 # button
  circle = FALSE, status = "danger", icon = icon("gear"), width = "300px", right = FALSE, up = TRUE,
  tooltip = tooltipOptions(title = "Configuration")
   )
  ),

# DT UI
  absolutePanel(top = 10, right = 10,
                id ="table",
                tags$style(HTML(".dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing,.dataTables_wrapper .dataTables_paginate .paginate_button, .dataTables_wrapper .dataTables_paginate .paginate_button.disabled {
                                color: #939393 !important;
                                }")),
                style="z-index:500;", # Z legend ! https://github.com/rstudio/leaflet/issues/534
                div(textOutput("etiquette_lieu_table"),  style = "padding-left: 10px;font-size: 150%; background-color: rgba(128,128,128, 0.8)"), 
                
                div(DT::dataTableOutput(outputId =  "table_exped"), style = "font-size:80%; background-color: rgba(128,128,128, 0.8)"),
                style = "#737373"
  ), 

absolutePanel(bottom= 20, right = 10,
              div(dygraphOutput("dygraph", height='350px',width='500px'), style = "float: right; color :'white'")
              ),
              

absolutePanel(bottom = 00, right = 300,
              id ="credits",
              tags$p("Principaux lieux de la traite atlantique.Wilfrid Cariou (2018)", href="http://www.histoire.univ-nantes.fr/", "Université de Nantes, Département d'Histoire")
              )            

)

















# Server #######################################################################

server <- function(input, output, session) {
  
  # Modal open default
  toggleModal(session, "startupModal", toggle = "open")
  
    observeEvent(input$show_table, {
      shinyjs::toggle(id = "table", anim = TRUE, animType = "slide")
    })
 

  
## Expression reactive chronologie, catégorie #####
  
  # Pour chaque modification des paramètres d'entrée du slideInput "periode"
  # filtrer la variable yearam selon la période déterminée
  # filtrer la variable yearam selon les choix : ports, lieu achat, vente...
  # calculer le total des expéditions pour chaque port
  # classer des ports part ordre décroissant d'expédition (afin de gérer le recouvrement des cercles )
  reactive_data_chrono <- reactive({ req(input$choix)
    places_geocode %>%
      filter(yearam >= input$periode[1] & yearam <= input$periode[2]) %>%
      filter(category %in% input$choix) %>%
      count(place,longitude, latitude, category, id2) %>%
      arrange(desc(n)) %>%
      mutate(label= paste(paste(place, as.character(n), sep = " : "), "exped."))
        })
  # reactive data for table filtering 
  date_min <- reactive({input$periode[1]})
  date_max <- reactive({input$periode[2]})  
  choix_category <- reactive({input$choix})   
  
 

  # Palette de couleurs basée sur les catégories (données place géocode)
# palettes https://github.com/d3/d3-3.x-api-reference/blob/master/Ordinal-Scales.md#categorical-colors
    pal <- colorFactor(
    palette = c('#6baed6', '#fd8d3c', '#74c476'),
    domain = places_geocode$category
  )
  

  # Affichage non reactif du fond de carte    
    output$map <- renderLeaflet({
      leaflet(places_geocode) %>%
        addProviderTiles(providers$CartoDB.DarkMatterNoLabels, group = "DarkMatter") %>%
        fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude)) %>%
        addTiles("https://maps.georeferencer.com/georeferences/673422615974/2017-12-29T21:36:14.564535Z/map/{z}/{x}/{y}.png?key=D7AwmpRP1H6pUic6DIK3",
                  group ="Wassende-Grade-Kaart van de Aetiopische Ocean, van Keulen, Joannes, 1680") %>%
        addTiles("https://maps.georeferencer.com/georeferences/190155356360/2017-12-30T15:04:20.007647Z/map/{z}/{x}/{y}.png?key=D7AwmpRP1H6pUic6DIK3",
                 group ="La Guinee et Pays circomvoisins, Sanson, Nicolas, 1656") %>%
        addLayersControl(
          baseGroups = c("DarkMatter", "Wassende-Grade-Kaart van de Aetiopische Ocean, van Keulen, Joannes, 1680", "La Guinee et Pays circomvoisins, Sanson, Nicolas, 1656"), position ="topleft")
    })  

  
  # Affichage reactif des données   
  observe({
    leafletProxy("map", data = reactive_data_chrono()) %>%
      clearShapes() %>%
      addCircles(lng=~longitude, lat=~latitude, weight = 2, radius = ~(n*100), color = ~pal(category), group = "splaces", layerId = ~id2, label =~label, highlightOptions = highlightOptions(color = "white", weight = 2))  %>%
      addSearchFeatures(
        targetGroups = "splaces",
        options = searchFeaturesOptions(
          zoom = 6, openPopup = TRUE, firstTipSubmit = TRUE, collapsed = TRUE,  # https://bhaskarvk.github.io/leaflet.extras/reference/search-options.html
          autoCollapse = TRUE, hideMarkerOnCollapse = TRUE, position ="topleft", textPlaceholder = "Recherche..."))
        })
   
  
  
  
  
  # Observe circles from leafletProxy "map" (clic event)
  
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()

        # reactive filtering ####
    x <- exped
    x1 <- x[x$id2 == event$id, ]                   # id lieu
    x2 <- x1[x1$yearam >= date_min(), ]
    x3 <- x2[x2$yearam <= date_max(), ]
    x4 <- x3 # [x3$category == choix_category(),]    # id category (ptdepimp...)
    x5 <- x4 %>%
      arrange(yearam)
    
    # Labelling DT  
    x2b <- x1 %>%
      distinct(place)
    x2c <- as.character(x2b)

    # counting for dygraph y values
        x6 <- x5 %>%
      count(yearam)
    
    # color for dygraph 
    colorr <- unique(x5$color)

    # dygraph  server side
    output$dygraph <- renderDygraph({ req(x6$n) # display graph require x6$n to avoid error message
      dygraph(x6, main = unique(x5$place)) %>%
        dyAxis("x", axisLabelColor = "#FFFFFF") %>%
        dyAxis("y", axisLabelColor =  "#FFFFFF") %>%
        dyAnnotation("1670", text = "1", tooltip = "Première grande époque de l'expansion sucrière aux Antilles française") %>%
        dyAnnotation("1776", text = "2", tooltip = "Début de la guerre d'indépendance américaine") %>%
        dyAnnotation("1790", text = "3", tooltip = "Début de la guerre d'indépendance de Saint-Domingue") %>%
        dyAnnotation("1807", text = "4", tooltip = "Abolition de la traite au Royaume-Uni") %>%
        dyOptions(colors =  colorr, axisLineWidth = 1, stepPlot = TRUE, fillGraph = TRUE, drawGrid = FALSE, fillAlpha = 0.1, includeZero = TRUE, 
        axisLineColor =  "rgba(0,0,0,0)")  })
    
    output$etiquette_lieu_table<- renderText(x2c)
    
    # DT server side 
    output$table_exped <- DT::renderDataTable({req(input$select)
          dat <- datatable(x4[, input$select, drop=FALSE],
              options = list(
                             autoWidth = FALSE,
                             language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json'),
                             dom = 'fp',
                             paging =TRUE,
                             pageLength =input$obs,
                             processing = TRUE))
    })
  })
}



shinyApp(ui, server)

