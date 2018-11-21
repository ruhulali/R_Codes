server <- function(input, output, session) {
  
  ## Your reactive logic goes here.
  
  # Listen for the 'done' event. This event will be fired when a user
  # is finished interacting with your application, and clicks the 'done'
  # button.
  observeEvent(input$done, {
    
    # Here is where your Shiny application might now go an affect the
    # contents of a document open in RStudio, using the `rstudioapi` package.
    #
    # At the end, your application should call 'stopApp()' here, to ensure that
    # the gadget is closed after 'done' is clicked.
    stopApp()
  })
}


library(shiny)
library(miniUI)

# We'll wrap our Shiny Gadget in an addin.
# Let's call it 'clockAddin()'.

clockAddin <- function() {
  
  # Our ui will be a simple gadget page, which
  # simply displays the time in a 'UI' output.
  ui <- miniPage(
    gadgetTitleBar("Clock"),
    miniContentPanel(
      uiOutput("time")
    )
  )
  
  server <- function(input, output, session) {
    
    # Set some CSS styles for our clock.
    clockStyles <- paste(
      "border: 1px solid #DADADA",
      "background-color: #EFEFEF",
      "border-radius: 5px",
      "font-size: 6em",
      "margin-top: 60px",
      "text-align: center",
      sep = "; "
    )
    
    # We'll use a 'reactiveTimer()' to force Shiny
    # to update and show the clock every second.
    invalidatePeriodically <- reactiveTimer(intervalMs = 1000)
    observe({
      
      # Call our reactive timer in an 'observe' function
      # to ensure it's repeatedly fired.
      invalidatePeriodically()
      
      # Get the time, and render it as a large paragraph element.
      time <- Sys.time()
      output$time <- renderUI({
        p(style = clockStyles, time)
      })
    })
    
    # Listen for 'done' events. When we're finished, we'll
    # insert the current time, and then stop the gadget.
    observeEvent(input$done, {
      timeText <- paste0("\"", as.character(Sys.time()), "\"")
      rstudioapi::insertText(timeText)
      stopApp()
    })
    
  }
  
  # We'll use a pane viwer, and set the minimum height at
  # 300px to ensure we get enough screen space to display the clock.
  viewer <- paneViewer(150)
  runGadget(ui, server, viewer = viewer)
  
}

# Try running the clock!
clockAddin()



# Now all that's left is sharing this addin -- put this function
# in an R package, provide the registration metadata at
# 'inst/rstudio/addins.dcf', and you're ready to go!


#############################################
install.packages("DT", dependencies = T)

library(shiny)
library(miniUI)
library(leaflet)

ui <- miniPage(
  gadgetTitleBar("Shiny gadget example"),
  miniContentPanel(padding = 0,
                   leafletOutput("map", height = "100%")
  ),
  miniButtonBlock(
    actionButton("reset", "Reset bounds")
  )
)

server <- function(input, output, session) {
  output$cars <- renderPlot({
    require(ggplot2)
    ggplot(cars, aes(speed, dist)) + geom_point()
  })
  
  output$map <- renderLeaflet({
    leaflet(quakes, height = "100%") %>% addTiles() %>% addMarkers()
  })
  
  output$table <- DT::renderDataTable({
    diamonds
  })
  
  observeEvent(input$done, {
    stopApp(TRUE)
  })
  
  observeEvent(input$reset, {
    leafletProxy("map", data = quakes) %>% fitBounds(
      ~min(long), ~min(lat),
      ~max(long), ~max(lat)
    )
  })
}

runGadget(ui, server, viewer = paneViewer())
#' @examples
#' options(shiny.autoreload=TRUE)
#' shiny::runApp("test2.R", launch.browser = getOption("viewer", TRUE))

########################################

library(shiny)
library(miniUI)
library(leaflet)
library(ggplot2)

ui <- miniPage(
  gadgetTitleBar("Shiny gadget example"),
  miniTabstripPanel(
    miniTabPanel("Parameters", icon = icon("sliders"),
                 miniContentPanel(
                   sliderInput("year", "Year", 1978, 2010, c(2000, 2010), sep = "")
                 )
    ),
    miniTabPanel("Visualize", icon = icon("area-chart"),
                 miniContentPanel(
                   plotOutput("cars", height = "100%")
                 )
    ),
    miniTabPanel("Map", icon = icon("map-o"),
                 miniContentPanel(padding = 0,
                                  leafletOutput("map", height = "100%")
                 ),
                 miniButtonBlock(
                   actionButton("resetMap", "Reset")
                 )
    ),
    miniTabPanel("Data", icon = icon("table"),
                 miniContentPanel(
                   DT::dataTableOutput("table")
                 )
    )
  )
)

server <- function(input, output, session) {
  output$cars <- renderPlot({
    require(ggplot2)
    ggplot(cars, aes(speed, dist)) + geom_point()
  })
  
  output$map <- renderLeaflet({
    force(input$resetMap)
    
    leaflet(quakes, height = "100%") %>% addTiles() %>%
      addMarkers(lng = ~long, lat = ~lat)
  })
  
  output$table <- DT::renderDataTable({
    diamonds
  })
  
  observeEvent(input$done, {
    stopApp(TRUE)
  })
}

runGadget(shinyApp(ui, server), viewer = paneViewer())
