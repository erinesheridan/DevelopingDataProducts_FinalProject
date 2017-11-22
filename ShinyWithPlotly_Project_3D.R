library(shiny)
library(plotly)

ui <- fluidPage(
  titlePanel("Predict Miles Per Gallon from Car Characteristics"),
  sidebarLayout(
    sidebarPanel(
      h1("Move the Sliders"),
      sliderInput("sliderHP","Select car horsepower:", 50, 350, 200),
      sliderInput("sliderWT", "Select car weight:", 1.5, 5.5, 3.5),
      submitButton("Submit")
    ),
  mainPanel(
    h3("Predicted Miles Per Gallon from Horsepower and Weight:"),
    textOutput("pred1"),
    plotlyOutput("plot1"),
    h5("The chart above is made with data from 32 vehicles. It plots each
    vehicle's horsepower (hp), against its weight and miles per gallon. From those
    32 vehicles' data, a linear regression is performed to predict the miles per 
    gallon based on a vehicle's horsepower and weight."), 
    h5("Use the sliders on the left to select the horsepower and weight to be used to
    predict the miles per gallon of a car, then click 'Submit'. The value under
    'Predicted Miles Per Gallon from Horsepower and Weight' is the predicted value
    of the miles per gallon. The red point is the location of the
    prediction on the diagram.")
  )
))

server <- function(input, output) {
  model1 <- lm(mpg ~ hp + wt, data = mtcars)
  
  model1_pred <- reactive({
    hpInput <- input$sliderHP
    wtInput <- input$sliderWT
    predict(model1, newdata = data.frame(hp = hpInput, wt = wtInput))
  })
  
  output$plot1 <- renderPlotly({
    mtcars2 <- mtcars
    mtcars2$cyl[mtcars2$cyl==4] <- "4 Cylinder"
    mtcars2$cyl[mtcars2$cyl=="6"] <- "6 Cylinder"
    mtcars2$cyl[mtcars2$cyl=="8"] <- "8 Cylinder"

    hpInput <- input$sliderHP
    wtInput <- input$sliderWT

    plot3D <- plot_ly(data = mtcars2, x = ~hp, y=~wt, z = ~mpg,
            type = "scatter3d", color = ~factor(cyl))
    add_markers(p=plot3D, x=hpInput, y=wtInput, z=model1_pred(), color=I("red"),
                name = "Prediction")
  })
  
  output$pred1 <- renderText({
    model1_pred()
  })
  
}

shinyApp(ui, server)