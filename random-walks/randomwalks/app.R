
library(shiny)
library(shinyjs)

library(dplyr)
library(plotly)
library(ggplot2)
library(ggthemes)

# A single step of the simple random walk
p_to_vec <- function(p,size = 1, x_neg = .1666, x_pos = .1666, y_neg = .1666, y_pos = .1666, z_neg = .1666, z_pos = .1666){
      # create intervals for cdf
      intervals = cumsum(c(x_neg, x_pos, y_neg, y_pos, z_neg, z_pos))


      if(p < intervals[1]){
            return(c(-size,0,0))
      }
      else if(between(p, intervals[1], intervals[2])){
            return(c(size,0,0))
      }
      else if(between(p, intervals[2], intervals[3])){
            return(c(0,-size,0))
      }
      else if(between(p, intervals[3], intervals[4])){
            return(c(0,size,0))
      }
      else if(between(p, intervals[4],intervals[5])){
            return(c(0,0,-size))
      }
      else {
            return(c(0,0,size))
      }
}
# vectorize probability to vector to create a matrix
pvec_to_matrix <- Vectorize(p_to_vec)


ui <- fluidPage(
      shinyjs::useShinyjs(),

      navbarPage("Walking Randomly.", id="nav",
                 tabPanel("3D Simulation",
                          sidebarPanel(
                                selectInput("rw_type", "Random Walk Variation:",
                                            c("Simple Random Walk (Drunken Walk)" = "simple",
                                              "Self Correcting Random Walk (Sobering Up Walk)" = "self_correcting_rw",
                                              "Multivariate ARMA Time Series" = "armav")),
                                 sliderInput("n", "Number Of Steps To Simulate",
                                            500, 50000, 10000, step = 500),

                                # Parameters specific to the simple random walk
                                conditionalPanel(
                                      condition = "input.rw_type == 'simple'",
                                      selectInput(
                                            "state_space", "State Space (Discrete/Continuous)",
                                            c("Discrete", "Continuous")
                                      )
                                ),
                                conditionalPanel(
                                      condition = "input.state_space == 'Continuous'",
                                      radioButtons("step_dist", "Step Size Distribution:",
                                                         choiceNames =
                                                               list("Uniform(0,1)" ),
                                                         choiceValues =
                                                               list("uniform")
                                      )
                                ),
                                fluidRow( id = "probs-sidepanel",

                                      fluidRow(
                                            column(6,
                                                   numericInput("x_neg", "Prob of\nstep in\n-X direction:", 0.1666666, min = 0, max = 1, step = .01)

                                            ),
                                            column(6,
                                                   numericInput("x_pos", "Prob of\nstep in\n+X direction:", 0.1666666, min = 0, max = 1, step = .01)

                                            )
                                      ),
                                      fluidRow(
                                            column(6,
                                                   numericInput("y_neg", "Prob of\nstep in\n-Y direction:", 0.1666667, min = 0, max = 1, step = .01)

                                            ),
                                            column(6,
                                                   numericInput("y_pos", "Prob of\nstep in\n+Y direction:", 0.1666667, min = 0, max = 1, step = .01)

                                            )
                                      ),
                                      fluidRow(
                                            column(6,
                                                   numericInput("z_neg", "Prob of\nstep in\n-Z direction:", 0.1666667, min = 0, max = 1, step = .01)

                                            ),
                                            column(6,
                                                   numericInput("z_pos", "Prob of\nstep in\n+Z direction:", 0.1666667, min = 0, max = 1, step = .01)

                                            )
                                      ),

                                      fluidRow(
                                            column(4,
                                                   p("Cumulative\nprobability:")
                                            ),
                                            column(4,
                                                   verbatimTextOutput("cdf")
                                            ),
                                            column(4,
                                                   fluidRow(
                                                         column(4,
                                                                actionButton(inputId = "render_viz", label = "Render!")
                                                         )
                                                   ),
                                                   fluidRow(
                                                         column(5,
                                                                actionButton(inputId = "reset_probs", label = "Reset.")
                                                         )
                                                   )


                                            )
                                      )

                                    )

                          ),
                          mainPanel(
                                fluidRow(
                                      #3D visualization
                                      plotlyOutput("plot3d")
                                ),
                                fluidRow(
                                    div(style = "height:100px")
                                ),
                                fluidRow(
                                      column(6,
                                          plotOutput('plot_norm')
                                       ),
                                      column(6,
                                             h3('chart 2')
                                      )

                                )

                          )
                 ),
                 tabPanel("Monte Carlo Simulation"

                 )
      )
)




# Define server logic required to draw a histogram
server <- function(input, output) {

      # Listen to see if user resets the probabilities.
      observeEvent(input$reset_probs, {
            shinyjs::reset("probs-sidepanel")
      })

      cdf <- reactive({
            try(input$x_neg + input$x_pos + input$y_neg + input$y_pos + input$z_neg + input$z_pos)
            })
      output$cdf = renderText(cdf())
      Xt = NULL

      observe({
            if(input$x_neg + input$x_pos + input$y_neg + input$y_pos + input$z_neg + input$z_pos != 1 ||
               is.na(input$x_neg + input$x_pos + input$y_neg + input$y_pos + input$z_neg + input$z_pos)){
                  shinyjs::disable("render_viz")
            }
            else{
                  shinyjs::enable("render_viz")
            }
      })

      # Wait for user to press the render button before running visualization
      observeEvent(input$render_viz, {
            # If user selected the simple random walk, initialize data as such.
            if(input$rw_type == "simple"){
                  Zt <- pvec_to_matrix(runif(n = input$n),
                                       size = ifelse(input$state_space == "Discrete", 1, runif(n = input$n)),
                                       x_neg = input$x_neg,
                                       x_pos = input$x_pos,
                                       y_neg = input$y_neg,
                                       y_pos = input$y_pos,
                                       z_neg = input$z_neg,
                                       z_pos = input$z_pos)
                  Xt <- apply(Zt, 1, cumsum)
                  colnames(Xt) <- c("X","Y", "Z")
                  Xt <- as.data.frame(Xt) %>% mutate(color = as.factor(row_number()), t = row_number(), norm = sqrt(X^2 + Y^2 + Z^2))
            }

            output$plot3d <- renderPlotly({
                  plot_ly(Xt, x = ~X, y = ~Y, z = ~Z, type = 'scatter3d', mode = 'lines',
                          line = list(color =~ color, width = 2), width = 800, height = 500) %>%
                        layout(showlegend = FALSE)
            })


            output$plot_norm <- renderPlot({
                  Xt %>%
                        ggplot(aes(x = t, y = norm)) +
                        geom_line() +
                        theme_few()
            }, height = 300, width = 400)



      })


}

# Run the application
shinyApp(ui = ui, server = server)

