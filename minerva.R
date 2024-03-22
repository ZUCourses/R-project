library(shiny)
library(ggplot2)
library(readxl)
library(here)

# Define UI for the application
ui <- fluidPage(
  # Application title
  titlePanel("Interactive Histogram of Average Outcome for Courses"),
  
  # Sidebar with a select input for choosing a course
  sidebarLayout(
    sidebarPanel(
      selectInput("selectedCourse",
                  "Select a Course:",
                  choices = NULL) # Choices will be updated when the server starts
    ),
    
    # Show a plot of the selected course's average outcome
    mainPanel(
      plotOutput("coursePlot")
    )
  )
)

# Define server logic to read data and generate plots
server <- function(input, output, session) {
  
  # Reactive expression to read the data from the Excel file
  coursesData <- reactive({
    read_xlsx(here("cht.xlsx")) 
  })
  
  # Observe the data and update the choices for the select input
  observe({
    updateSelectInput(session, "selectedCourse",
                      choices = unique(coursesData()$`Course Title`))
  })
  
  # Generate the plot based on the selected course
  output$coursePlot <- renderPlot({
    # Filter data for the selected course
    data <- coursesData()
    selectedData <- data[data$`Course Title` == input$selectedCourse,]
    
    # Calculate mean and standard deviation
    mean_value <- mean(selectedData$`Avg. Outcome`, na.rm = TRUE)
    sd_value <- sd(selectedData$`Avg. Outcome`, na.rm = TRUE)
    
    # Create the histogram
    p <- ggplot(selectedData, aes(x = `Avg. Outcome`)) +
      geom_histogram(binwidth = 0.4, fill = 'blue', color = 'black') +
      geom_vline(aes(xintercept = mean_value), color = "red", linetype = "dashed", size = 1) +
      geom_vline(aes(xintercept = mean_value + sd_value), color = "green", linetype = "dashed", size = 1) +
      geom_vline(aes(xintercept = mean_value - sd_value), color = "green", linetype = "dashed", size = 1) +
      labs(title = paste("Average Outcome Distribution for", input$selectedCourse),
           x = "Avg. Outcome",
           y = "Frequency",
           subtitle = paste("Mean =", round(mean_value, 2), "| SD =", round(sd_value, 2))) +
      theme_minimal()
    
    # Set the limits of the x-axis to 0 and 4
    # Note that this will remove data outside these limits
    p + xlim(0, 4) 
  })
}

# Run the application 
shinyApp(ui = ui, server = server)