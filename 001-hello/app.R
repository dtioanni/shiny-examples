library(shiny)

# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel("My first Shiny App!"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)

    ),
    selectInput(inputId = "dataset",
                label = "dataset",
                choices =c("faithful","iris"),
    )

  ),
  # Main panel for displaying outputs ----
  mainPanel(

    # Output: Histogram ----
    plotOutput(outputId = "distPlot"),

    verbatimTextOutput("range")

  )
)


# Define server logic required to draw a histogram ----
server <- function(input, output) {

  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot

  dataInput <- reactive({
    data <- switch(input$dataset,
                   "faithful" = faithful$waiting,
                   "iris" = iris$Sepal.Length)
    bins <- input$bins
    xlab<- ifelse(input$dataset=="faithful","Waiting time to next eruption (in mins)","Sepal Length")
    main<-ifelse(input$dataset=="faithful","Histogram of waiting times","Histogram of Sepal Length")

    return(list(data=data,bins=bins,xlab=xlab,main=main))


  })

  output$distPlot <- renderPlot({
    x<-dataInput()$data
    bins<-dataInput()$bins
    xlab<-dataInput()$xlab
    main<-dataInput()$main
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = xlab,
         main = main)
  })
output$range<-renderPrint({range(dataInput()$data)})
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)

