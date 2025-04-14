# Primero creamos la interfaz de usuario (UI)
ui <- fluidPage({
  # Tema y titulo de la aplicación
  
  theme = shinytheme("flatly"),
  titlePanel("Visualización de datos ómicos"),
  
  #Aquí personalizamos la pantalla. Queremos que tenga un panel lateral donde cargar los archivos y
  #podamos editar los parámetros de los gráficos. Queremos que en la pantalla principal, tenga varias pestañas:
  #en la primera, una previsualización de los datos; en la segunda, el heatmap; en la tercera, 
  #un gráfico de correlación de datos
  
  #Diseñamos el panel lateral
  
  sidebarLayout(
    sidebarPanel(
      #Carga de archivo
      fileInput("archivo", 
                "Cargar archivo .CSV", 
                accept = ".csv"),
      #Botón de analizar
      actionButton("analizar",
                   "Analizar datos"),
      hr(),
      #Casilla para personalizar el Heat Map
      checkboxInput("pers_hm",
                    "Personalizar HeatMap",
                    value = FALSE),
      # Casilla para personalizar el gráfico de correlación
      checkboxInput("pers_corr",
                    "Personalizar gráfico de correlación",
                    value = FALSE),
      #Panel condicional: aparece SI la casilla de personalización de heatmap está seleccionada (condición)
      conditionalPanel(
        condition = "input.pers_hm == true",
        # Slider para altura y ancho de las celdas
        sliderInput("alturaCeldas",
                    "Altura de las celdas",
                    min = 50, max = 150,
                    value = 50,
                    step = 5),
        sliderInput("anchoCeldas",
                    "Ancho de las celdas",
                    min = 50, max = 150,
                    value = 50,
                    step = 5),
        #slider para el tamaño de las etiquetas
        sliderInput("tam_texto",
                    "Tamaño de las etiquetas",
                    min = 5, max = 50,
                    value = 8,
                    step = 1),
        #casilla para la personalización de la escala de colores
        checkboxInput("pers_color",
                      "Personalizar escala de colores", 
                      value = FALSE),
        # panel condicional: aparece SI la casilla de personalización de los colores está seleccionada
        conditionalPanel(
          #condición
          condition = "input.pers_color == true",
          #sliders con el mínimo y el máximo de la escala
          sliderInput("min_col",
                      "Mínimo escala",
                      min = -10, max = 0,
                      value = -3,
                      step = 1),
          sliderInput("max_col",
                      "Máximo Escala",
                      min = 1, max = 10,
                      value = 3,
                      step = 1),
          #selección de color para los puntos mínimo, medio y máximo de la escala
          colorPickr("lower_col",
                     "Color mínimo",
                     selected = "green"),
          colorPickr("mid_col",
                     "Color medio",
                     selected = "white"),
          colorPickr("upper_col",
                     "Color máximo",
                     selected = "red")
        )
      ),
      #panel condicional: aparece SI la casilla de personalización del gráfico de correlación está seleccionada
      conditionalPanel(
        #condición
        condition = "input.pers_corr == true",
        #casilla para mostrar/esconder leyenda
        checkboxInput("show_legend", 
                      "Mostrar leyenda", 
                      value = TRUE),
        #slider para el tamaño del texto
        sliderInput("text_size", 
                    "Tamaño de letra de los ejes", 
                    min = 0.5, max = 2, 
                    value = 1, 
                    step = 0.1),
        #menú desplegable para elegir una paleta de colores entre las disponibles
        selectInput("color_scheme", 
                    "Esquema de colores",
                    choices = c("Default" = "default",
                                "Azul-Blanco-Rojo" = "blue_white_red",
                                "Verde-Amarillo-Rojo" = "green_yellow_red")),
        #menú desplegable para elegir el símbolo que aparece en el gráfico
        selectInput("corr_method",
                    "Método",
                    choices = c("Círculos" = "circle",
                                "Cuadrados" = "square",
                                "Elipses" = "ellipse",
                                "Números" = "number"))
      )
      
      
    ),
    
    
    # Ahoradiseñamos el panel principal
    mainPanel(
      # Aquí señalamos qué pestañas (tabs) tiene nuestra UI, cómo se llaman y qué tienen
      tabsetPanel(
        #Pestaña para la previsualización de los datos
        tabPanel(
          "Datos",
          tableOutput("data_table"),
          verbatimTextOutput("text_info")
        ),
        #Pestaña para el HeatMap
        tabPanel(
          "HeatMap",
          plotOutput("plot_heatmap",
                     height = "600px")
        ),
        # Pestaña para el gráfico de correlación
        tabPanel(
          "Correlación",
          plotOutput("plot_corr",
                     height = "600px")
        )
      )
    )
  )
})

#Ahora definimos el servidor: qué hace la aplicación
server <- function(input, output){
  
  #cargamos el archivo CSV
  data <- reactive({
    req(input$archivo)
    read.csv(input$archivo$datapath, header = TRUE, row.names = 1)
  })
  
  # mostramos los datos por Pantalla
  
  #en la página principal "Datos" Moestramos un pequeño texto de información
  output$text_info <- renderPrint({
    "Aplicación Web creada por Francisco González para 'Software para el análisis de datos'. 2024."
  })
  
  # Cuando ocurre un evento (se pulsa el botón de analizar), queda activo el interior de observeEvent
  observeEvent(input$analizar, {
    #para el ID data_table, mostramos una tabla que contiene la información cargada en data
    output$data_table <- renderTable({
      data()
    })
    
    # para el id plot_heatmap, aparece la gráfica del heatmap, con todos los inputs descritos en UI, 
    # que nos permiten editar el gráfico
    output$plot_heatmap<- renderPlot({
      
      # editamos paleta de colores mediante los colores elegidos por el usuario SI la casilla de 
      # personalización está activada. En otro caso, se muestra una paleta preestablecida
      col_fun <- if(input$pers_hm & input$pers_color){
        colorRampPalette(c(input$lower_col, input$mid_col, input$upper_col))(100)
      } else {
        colorRampPalette(c("green", "white","red"))(100)
      }
      
      # Se crea la escala que el usuario ha definido SI el botón de personalización está
      # seleccionado, en caso contrario, se usa una escala preestablecida
      breaks_fun <- if(input$pers_hm & input$pers_color){
        seq(from = input$min_col, to = input$max_col, length.out = 101)
      } else {
        seq(from = min(data()), to = max(data()), length.out = 101)
      }
      
      #Moestramos el HeatMap
      pheatmap(
        as.matrix(data()), #matriz de datos
        breaks = breaks_fun, #escala
        color = col_fun, # paleta de colores
        fontsize = input$tam_texto, # tamaño del texto
        cellwidth = ifelse(input$pers_hm, input$anchoCeldas,  50), #ancho y alto de celdas
        cellheight = ifelse(input$pers_hm, input$alturaCeldas, 50),
        name = "HeatMap"
        
      )
    })
    
    
    # para el id plot_corr, aparece el gráfico de correlación, con todos los inputs descritos en UI, 
    # que nos permiten editar el gráfico
    output$plot_corr <- renderPlot({
      
      # en esta ocasión, damos a seleccionar unas paletas de colores preseleccionadas
      color_scheme <- switch(input$color_scheme,
                             "default" = colorRampPalette(c("#6D9EC1", "white", "#E46726"))(200),
                             "blue_white_red" = colorRampPalette(c("blue", "white", "red"))(200),
                             "green_yellow_red" = colorRampPalette(c("green", "yellow", "red"))(200))
      
      
      #creamos el gráfico de correlación
      corrplot(cor(data()), 
               method = ifelse(input$pers_corr,input$corr_method,"circle"), #método
               tl.cex = ifelse(input$pers_corr,input$text_size,1), #tamaño textos
               number.cex = ifelse(input$pers_corr,input$text_size,1),
               col = color_scheme, #paleta de colores
               addCoef.col = "black",
               tl.col = "black",
               tl.srt = 45,
               diag = FALSE, #quitamos la diagonal
               type = "upper", #elegimos el tipo upper
               addgrid.col = "gray",
               cl.pos = if(input$show_legend) "r" else "n") #leyenda
    })
  })
  
  
}

#comienza nuestra aplicación
shinyApp(ui = ui, server = server)