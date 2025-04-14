# First, we create the user interface (UI)
ui <- fluidPage({
  # Theme and title of the app
  theme = shinytheme("flatly"),
  titlePanel("Omics Data Visualization"),
  
  # Here we customize the screen. We want a sidebar panel for file upload and 
  # editing plot parameters. In the main panel, we want several tabs:
  # the first for data preview, the second for the heatmap,
  # and the third for a correlation plot.
  
  # Design the sidebar panel
  sidebarLayout(
    sidebarPanel(
      # File upload
      fileInput("file", 
                "Upload .CSV file", 
                accept = ".csv"),
      # Analyze button
      actionButton("analyze",
                   "Analyze Data"),
      hr(),
      # Checkbox to customize Heat Map
      checkboxInput("cust_hm",
                    "Customize HeatMap",
                    value = FALSE),
      # Checkbox to customize correlation plot
      checkboxInput("cust_corr",
                    "Customize correlation plot",
                    value = FALSE),
      # Conditional panel: appears IF the HeatMap customization checkbox is selected
      conditionalPanel(
        condition = "input.cust_hm == true",
        # Sliders for cell height and width
        sliderInput("cellHeight",
                    "Cell Height",
                    min = 50, max = 150,
                    value = 50,
                    step = 5),
        sliderInput("cellWidth",
                    "Cell Width",
                    min = 50, max = 150,
                    value = 50,
                    step = 5),
        # Slider for label size
        sliderInput("label_size",
                    "Label Size",
                    min = 5, max = 50,
                    value = 8,
                    step = 1),
        # Checkbox to customize color scale
        checkboxInput("cust_color",
                      "Customize color scale", 
                      value = FALSE),
        # Conditional panel: appears IF the color scale customization checkbox is selected
        conditionalPanel(
          # Condition
          condition = "input.cust_color == true",
          # Sliders for min and max color scale values
          sliderInput("min_col",
                      "Scale Minimum",
                      min = -10, max = 0,
                      value = -3,
                      step = 1),
          sliderInput("max_col",
                      "Scale Maximum",
                      min = 1, max = 10,
                      value = 3,
                      step = 1),
          # Color selection for lower, middle, and upper scale points
          colorPickr("lower_col",
                     "Minimum Color",
                     selected = "green"),
          colorPickr("mid_col",
                     "Middle Color",
                     selected = "white"),
          colorPickr("upper_col",
                     "Maximum Color",
                     selected = "red")
        )
      ),
      # Conditional panel: appears IF the correlation plot customization checkbox is selected
      conditionalPanel(
        # Condition
        condition = "input.cust_corr == true",
        # Checkbox to show/hide legend
        checkboxInput("show_legend", 
                      "Show legend", 
                      value = TRUE),
        # Slider for axis text size
        sliderInput("text_size", 
                    "Axis text size", 
                    min = 0.5, max = 2, 
                    value = 1, 
                    step = 0.1),
        # Dropdown to select color scheme
        selectInput("color_scheme", 
                    "Color Scheme",
                    choices = c("Default" = "default",
                                "Blue-White-Red" = "blue_white_red",
                                "Green-Yellow-Red" = "green_yellow_red")),
        # Dropdown to select the symbol in the plot
        selectInput("corr_method",
                    "Method",
                    choices = c("Circles" = "circle",
                                "Squares" = "square",
                                "Ellipses" = "ellipse",
                                "Numbers" = "number"))
      )
    ),
    
    # Now we design the main panel
    mainPanel(
      # Here we define the tabs our UI will have, their names, and their content
      tabsetPanel(
        # Tab for data preview
        tabPanel(
          "Data",
          tableOutput("data_table"),
          verbatimTextOutput("text_info")
        ),
        # Tab for the HeatMap
        tabPanel(
          "HeatMap",
          plotOutput("plot_heatmap",
                     height = "600px")
        ),
        # Tab for the correlation plot
        tabPanel(
          "Correlation",
          plotOutput("plot_corr",
                     height = "600px")
        )
      )
    )
  )
})

# Now we define the server: what the app does
server <- function(input, output){
  
  # Load the CSV file
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath, header = TRUE, row.names = 1)
  })
  
  # Display data on the screen
  
  # In the "Data" tab, show a short info message
  output$text_info <- renderPrint({
    "Web application created by Francisco González. 2024."
  })
  
  # When the "Analyze" button is pressed, everything inside observeEvent becomes active
  observeEvent(input$analyze, {
    # For the ID data_table, render a table showing the data
    output$data_table <- renderTable({
      data()
    })
    
    # For the ID plot_heatmap, render the heatmap using the UI inputs for customization
    output$plot_heatmap<- renderPlot({
      
      # Set color palette using selected colors IF customization checkbox is active
      col_fun <- if(input$cust_hm & input$cust_color){
        colorRampPalette(c(input$lower_col, input$mid_col, input$upper_col))(100)
      } else {
        colorRampPalette(c("green", "white","red"))(100)
      }
      
      # Create color scale defined by user IF customization is active, otherwise use default
      breaks_fun <- if(input$cust_hm & input$cust_color){
        seq(from = input$min_col, to = input$max_col, length.out = 101)
      } else {
        seq(from = min(data()), to = max(data()), length.out = 101)
      }
      
      # Show the HeatMap
      pheatmap(
        as.matrix(data()), # data matrix
        breaks = breaks_fun, # scale
        color = col_fun, # color palette
        fontsize = input$label_size, # label size
        cellwidth = ifelse(input$cust_hm, input$cellWidth,  50), # cell width and height
        cellheight = ifelse(input$cust_hm, input$cellHeight, 50),
        name = "HeatMap"
      )
    })
    # For the ID plot_corr, render the correlation plot with user-selected customization
    output$plot_corr <- renderPlot({
      
      # Choose color palette from predefined options
      color_scheme <- switch(input$color_scheme,
                             "default" = colorRampPalette(c("#6D9EC1", "white", "#E46726"))(200),
                             "blue_white_red" = colorRampPalette(c("blue", "white", "red"))(200),
                             "green_yellow_red" = colorRampPalette(c("green", "yellow", "red"))(200))
      
      # Create the correlation plot
      corrplot(cor(data()), 
               method = ifelse(input$cust_corr,input$corr_method,"circle"), # method
               tl.cex = ifelse(input$cust_corr,input$text_size,1), # text size
               number.cex = ifelse(input$cust_corr,input$text_size,1),
               col = color_scheme, # color palette
               addCoef.col = "black",
               tl.col = "black",
               tl.srt = 45,
               diag = FALSE, # remove diagonal
               type = "upper", # show upper triangle only
               addgrid.col = "gray",
               cl.pos = if(input$show_legend) "r" else "n") # legend
    })
  })
}

# Start the application
shinyApp(ui = ui, server = server)
