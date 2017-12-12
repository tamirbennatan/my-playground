library(leaflet)
library(scales)
library(lattice)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(chorddiag)



library(shiny)

# Load Data.
df.taxi <-  read.csv("data/production.csv")
# Convert date/time columns to date/time objects
df.taxi$pickup_datetime <- as.POSIXct(df.taxi$pickup_datetime, format="%Y-%m-%d %H:%M:%S")
df.taxi$dropoff_datetime <-   as.POSIXct(df.taxi$dropoff_datetime, format="%Y-%m-%d %H:%M:%S")

#Change the pickup and dropoff neighborhood columns to strings, not factors
df.taxi$pickup_neighborhood <- as.character(df.taxi$pickup_neighborhood)
df.taxi$dropoff_neighborhood <- as.character(df.taxi$dropoff_neighborhood)

#Load neighborhoods
all_neighborhoods <- read.csv("data/neighborhoods.csv")
all_neighborhoods <- as.vector(all_neighborhoods$x)

# Convert datetime columns to date times (currently factor)

# Map input: select where to place bubbles
location_options = c(
      "Pickup Location" = "pickup_location",
      "Dropoff Location" = "dropoff_location",
      "Trip Halfway Point " = "halfway_point"
)

# Map input: what the size/color of the bubbles should be
bubble_appearence_options = c(
      "Trip Duration" = "trip_duration",
      "Number Of Passengers" = "passenger_count"
)




ui <- fluidPage(

      navbarPage("NYC Taxi Explorer", id="nav",
                 tabPanel("Interactive map",
                       div(class="outer",

                           tags$head(
                                 # Include our custom CSS
                                 includeCSS("styles.css")
                           ),


                           leafletOutput("map", width="100%", height="100%"),

                           tags$div(id="cite",
                                    'Modified from ', tags$a(href = "https://shiny.rstudio.com/gallery/superzip-example.html",
                                                             "ZipCode Explorer"), ' by RStudio.',




                           absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                         draggable = TRUE, top = 60, left = "10", right = 20, bottom = "auto",
                                         width = 430, height = "auto",

                                         h2("Taxi Ride Volume"),

                                         # Input date
                                         dateInput("selected.date", "Select Date:", value = "2016-05-20",format = "yyyy-mm-dd", min = "2016-04-01", max = "2016-06-31"),

                                         #input time.
                                         sliderInput("selected.hours", "Select Hour Interval:", 0, 24, value = c(18, 21)),

                                         selectInput("bubble_location", "Bubble Location:", location_options),


                                         selectInput("bubble_size", "Bubble Size:", bubble_appearence_options, selected = "passenger_count"),
                                         selectInput("bubble_color", "Bubble Color:", bubble_appearence_options, selected = "trip_duration"),


                                         plotOutput("durationDensity", height = 200)



                                         )

                           )


                        )
                 ),

            tabPanel("Neighborhood Flow Map",
                     # Row for inputs

                     fluidRow(
                           h3("Flow Between NYC's Neighborhoods"),
                           div( class="chord-input",
                              column(5,
                                     # input date/time. Note: these date/time input variables are the same as those from the
                                     # interactive map, so it will adust the map _and_ the chord diagram whenever these are changed.
                                     # This will add some overhead. If it becomes too much, let this be a different set of variables.
                                     dateInput("selected.date.chord", "Select Date:", value = "2016-05-20",format = "yyyy-mm-dd", min = "2016-04-01", max = "2016-06-31"),
                                     sliderInput("selected.hours.chord", "Select Hour Interval:", 0, 24, value = c(18, 21))
                              ),
                              column(5, offset = 1,
                                    # Let user select which neighborhoods to include in chord map
                                    selectInput("selected.neighborhoods", "Neighborhoods To Include In Chord Diagram:",
                                                all_neighborhoods, multiple = TRUE ,selected = all_neighborhoods[1:6])
                              )
                           )
                     ),
                     # Here Is the chord diagram
                     fluidRow(top = 50,
                          chorddiagOutput('chorddiag', height = '600px')
                              )


            )
      )

)

server <- function(input, output) {

      # A reactive block which filters dataframe to the date in question.
      limitDate <- reactive({
            df.taxi %>% filter(as.Date(pickup_datetime) < input$selected.date + 1
                               & as.Date(pickup_datetime) >= input$selected.date &
                               between(hour(pickup_datetime),  input$selected.hours[1], input$selected.hours[2])
                               )
      })


      # A reactive block which extracts just the lat/longitude, numbrer of passengers, and duration in the window.
      # This will be used to populate the map.
      mapData <- reactive({

            #based on user_input, determine what to group by
            if(input$bubble_location == "pickup_location"){
                  map_data <- limitDate() %>%
                        mutate(
                              latitude = pickup_latitude,
                              longitude = pickup_longitude) %>%
                        select(-pickup_latitude, -pickup_longitude)

            }
            else if (input$bubble_location == "dropoff_location"){
                  map_data <- limitDate() %>%
                        mutate(
                              latitude = dropoff_latitude,
                              longitude = dropoff_longitude) %>%
                        select(-dropoff_latitude, -dropoff_longitude)

            }
            else if (input$bubble_location == "halfway_point"){
                  map_data <- limitDate() %>%
                        mutate(
                              latitude = halfway_latitude,
                              longitude = halfway_longitude) %>%
                        select(-halfway_latitude, -halfway_longitude)
            }

            map_data
      })


      ## Interactive Map ###########################################

      # Create the map
      output$map <- renderLeaflet({
            leaflet() %>%
                  addTiles(
                        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
                        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
                  ) %>%
                  setView(lat = 40.7607, lng = -74., zoom = 13) #initialize map at NYC coordinates
      })

      # This observer is responsible for maintaining the circles and legend,
      # according to the variables the user has chosen to map to color and size.
      observe({
            #user defined mapping parameters
            bubble_color <- input$bubble_color
            bubble_size <- input$bubble_size

            # data to plot on map
            map.data <- mapData()

            # Define color mapping function
            pal <- colorNumeric("viridis", map.data[[bubble_color]], na.color = "red")
            # Define radius of bubbles
            radius <- ((map.data[[bubble_size]] - min(map.data[[bubble_size]]) + 1) / max(map.data[[bubble_size]])) *130


            # Add bubbles!
            leafletProxy("map", data = map.data) %>%
                  clearShapes() %>%
                  addCircles(~longitude, ~latitude, radius=radius,
                             stroke=FALSE, fillOpacity=0.4, fillColor=pal(map.data[[bubble_color]])) %>%
                  addLegend("bottomright", pal=pal, values=map.data[[bubble_color]], title=bubble_color,
                            layerId="colorLegend")




      })

      output$durationDensity <- renderPlot({
            limitDate() %>%
            ggplot(aes(x = trip_duration))  + geom_histogram(bins = 50, fill = "cadetblue1") + theme_economist_white() +
            ggtitle("Trip Duration Distribution") +
            labs(subtitle =sprintf("On %s between %d:00 and %d:00", input$selected.date,
                            input$selected.hours[1], input$selected.hours[2])) +
            theme(plot.subtitle=element_text(size=12, face="italic", color="black"))
            }, height = 200)


      ################ Chord Diagram #########################

      # A reactive block which fitlers dataframe to the date/time in question (for the chord diagram)
      limitDate.chord <- reactive({
            df.taxi %>% filter(as.Date(pickup_datetime) < input$selected.date.chord + 1
                               & as.Date(pickup_datetime) >= input$selected.date.chord &
                                     between(hour(pickup_datetime),  input$selected.hours.chord[1], input$selected.hours.chord[2])
            )
      })

      # a reactive block which lmits the neighborhoods to show in the chord plot
      limitNeighborhoods.chord <- reactive({
            limitDate.chord() %>%
                  filter(pickup_neighborhood %in% input$selected.neighborhoods &
                               dropoff_neighborhood  %in% input$selected.neighborhoods) %>%
                  select (pickup_neighborhood, dropoff_neighborhood)
      })

      # A responsive block which creates a data structure ingestable by the chorddiag plot
      chordData <- reactive({
            # get data as a dataframe
            df <- limitNeighborhoods.chord()

            #conv
            df$pickup_neighborhood <- as.factor(df$pickup_neighborhood)
            df$dropoff_neighborhood <- as.factor(df$dropoff_neighborhood)

            levels(df$pickup_neighborhood) <- input$selected.neighborhoods
            levels(df$dropoff_neighborhood) <- input$selected.neighborhoods

            #get a frequency table
            frequencey.table <- data.frame(table(df$pickup_neighborhood, df$dropoff_neighborhood))

            #Convert pairwise frequency matrix (still dataframe)
            frequency.matrix <-  spread(data = frequencey.table, key = "Var1", value = "Freq", fill = 0)

            # convert to native numeric matrix
            m <- as.matrix(frequency.matrix[, -1])
            row.names(m) <- colnames(m)

            m

            })


      all_colors <- c("#BEBD7F", "#C2B078", "#C6A664", "#E5BE01", "#CDA434", "#A98307",
                      "#E4A010", "#DC9D00", "#8A6642", "#C7B446", "#EAE6CA", "#E1CC4F",
                      "#E6D690", "#EDFF21", "#F5D033", "#F8F32B", "#9E9764", "#999950",
                      "#F3DA0B", "#FAD201", "#AEA04B", "#FFFF00", "#9D9101", "#F4A900",
                      "#D6AE01", "#F3A505", "#EFA94A", "#6A5D4D", "#705335", "#F39F18",
                      "#ED760E", "#C93C20", "#CB2821", "#FF7514", "#F44611", "#FF2301",
                      "#FFA420", "#F75E25", "#F54021", "#D84B20", "#EC7C26", "#E55137",
                      "#C35831", "#AF2B1E", "#A52019", "#A2231D", "#9B111E", "#75151E",
                      "#5E2129", "#412227", "#642424", "#781F19", "#C1876B", "#A12312",
                      "#D36E70", "#EA899A", "#B32821", "#E63244", "#D53032", "#CC0605",
                      "#D95030", "#F80000", "#FE0000", "#C51D34", "#CB3234", "#B32428",
                      "#721422", "#B44C43", "#6D3F5B", "#922B3E", "#DE4C8A", "#641C34",
                      "#6C4675", "#A03472", "#4A192C", "#924E7D", "#A18594", "#CF3476",
                      "#8673A1", "#6C6874", "#354D73", "#1F3438", "#20214F", "#1D1E33",
                      "#18171C", "#1E2460", "#3E5F8A", "#26252D", "#025669", "#0E294B", "#231A24",
                      "#3B83BD", "#1E213D", "#606E8C", "#2271B3", "#063971", "#3F888F", "#1B5583",
                      "#1D334A", "#256D7B", "#252850", "#49678D", "#5D9B9B", "#2A6478", "#102C54",
                      "#316650", "#287233", "#2D572C", "#424632", "#1F3A3D", "#2F4538", "#3E3B32",
                      "#343B29", "#39352A", "#31372B", "#35682D", "#587246", "#343E40", "#6C7156",
                      "#47402E", "#3B3C36", "#1E5945", "#4C9141", "#57A639", "#BDECB6", "#2E3A23",
                      "#89AC76", "#25221B", "#308446", "#3D642D", "#015D52", "#84C3BE", "#2C5545",
                      "#20603D", "#317F43", "#497E76", "#7FB5B5", "#1C542D", "#193737", "#008F39",
                      "#00BB2D", "#78858B", "#8A9597", "#7E7B52", "#6C7059", "#969992", "#646B63",
                      "#6D6552", "#6A5F31", "#4D5645", "#4C514A", "#434B4D", "#4E5754", "#464531",
                      "#434750", "#293133", "#23282B", "#332F2C", "#686C5E", "#474A51", "#2F353B",
                      "#8B8C7A", "#474B4E", "#B8B799", "#7D8471", "#8F8B66", "#D7D7D7", "#7F7679",
                      "#7D7F7D", "#B5B8B1", "#6C6960", "#9DA1AA", "#8D948D", "#4E5452", "#CAC4B0",
                      "#909090", "#82898F", "#D0D0D0", "#898176", "#826C34", "#955F20", "#6C3B2A",
                      "#734222", "#8E402A", "#59351F", "#6F4F28", "#5B3A29", "#592321", "#382C1E", "#633A34", "#4C2F27", "#45322E", "#403A3A", "#212121", "#A65E2E", "#79553D", "#755C48", "#4E3B31", "#763C28", "#FDF4E3", "#E7EBDA", "#F4F4F4", "#282828", "#0A0A0A", "#A5A5A5", "#8F8F8F", "#FFFFFF", "#1C1C1C", "#F6F6F6", "#1E1E1E", "#D7D7D7", "#9C9C9C")

      output$chorddiag <- renderChorddiag(
            chorddiag(chordData(),
                      groupColors = sample(all_colors, length(input$selected.neighborhoods)),

                      showTicks = FALSE,
                      margin = input$margin,
                      clickAction = "Shiny.onInputChange('sourceIndex', d.source.index+1);
                                 Shiny.onInputChange('targetIndex', d.target.index+1);",
                      clickGroupAction = "Shiny.onInputChange('groupIndex', d.index+1);")
      )

      output$table0 <- renderTable( limitNeighborhoods.chord())
      output$table1 <- renderTable( chordData() )
      #



}

shinyApp(ui = ui, server = server)
