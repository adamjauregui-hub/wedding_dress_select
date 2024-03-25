library(shiny)

# Define the list of dress filenames
all_dresses <- paste0("Dress", 1:8, ".jpg")

# Server logic
server <- function(input, output, session) {
  # Reactive value to control the visibility of the introduction screen
  showIntro <- reactiveVal(TRUE)
  
  # Serve the condition for showing/hiding the intro
  output$showIntro <- reactive({ showIntro() })
  outputOptions(output, 'showIntro', suspendWhenHidden = FALSE)  # Ensure reactivity even when hidden
  
  # Initialize reactive values to store the current dresses for comparison
  dress1 <- reactiveVal(all_dresses[1])
  dress2 <- reactiveVal(all_dresses[2])
  # Reactive value to store the index of the next dress to show
  next_dress_index <- reactiveVal(3)
  
  # Observe the "Start" button to hide the introduction and show the dress selection
  observeEvent(input$startBtn, {
    showIntro(FALSE)  # Hide the introductory screen
  })
  
  # Observe event for "Choose Left" button
  observeEvent(input$choose_left, {
    # Update dress1 with the winner and dress2 with the next dress in line
    dress2(dress1())
    dress1(all_dresses[next_dress_index()])
    next_dress_index(next_dress_index() + 1)
  })
  
  # Observe event for "Choose Right" button
  observeEvent(input$choose_right, {
    if (next_dress_index() <= length(all_dresses)) {
      dress1(dress2()) # Keep the winner (right-side dress)
      dress2(all_dresses[next_dress_index()]) # Set the next dress for comparison
      next_dress_index(next_dress_index() + 1) # Increment to the next index
    } else {
      # If there are no more dresses to compare, keep the last winner visible
      dress1(dress2()) # The final winner is the last one picked
      dress2(NULL) # Indicate there are no more dresses to compare
    }
  })
  
  
  # Render the images in the UI
  output$dress_images <- renderUI({
    if (!is.null(dress2())) {
      tagList(
        img(src = dress1(), class = "dress-image"),
        img(src = dress2(), class = "dress-image")
      )
    } else {
      tagList(
        h3("The final winning dress is:"),
        img(src = dress1(), class = "dress-image")
      )
    }
  })
  
  
}

# UI code
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .image-container {
        display: flex; /* Use Flexbox to lay out child elements */
        justify-content: center; /* Center children horizontally in the container */
        align-items: center; /* Center children vertically */
      }
      .dress-image {
        width: auto; /* Maintain the aspect ratio */
        max-width: 45%; /* Maximum width as a percentage of the container */
        max-height: 600px; /* Maximum height in pixels */
        margin: 0 10px; /* Add some space between the images */
      }
      .intro-image {
        width: 100%; /* Adjust based on your needs */
        max-width: 600px; /* Maximum width */
        height: auto; /* Maintain aspect ratio */
        display: block; /* Center the image */
        margin-left: auto;
        margin-right: auto;
      }
      .btn-action {
        margin: 20px;
      }
    "))
  ),
  titlePanel("Wedding Dress Selector"),
  # Introductory screen
  conditionalPanel(
    condition = "output.showIntro",
    div(
      id = "intro",
      img(src = "dress_COLLAGE.jpg", class = "intro-image"),
      h2("Welcome to the Wedding Dress Selector!"),
      p("Click 'Start' to begin selecting your favorite wedding dresses."),
      actionButton("startBtn", "Start", class = "btn-action")
    )
  ),
  
  # Dress selection screen, initially hidden
  conditionalPanel(
    condition = "!output.showIntro",
    div(class = "image-container", uiOutput("dress_images")),
    actionButton("choose_left", "Choose Left", class = "btn-action"),
    actionButton("choose_right", "Choose Right", class = "btn-action")
  )
)

# Run the app
shinyApp(ui, server)
