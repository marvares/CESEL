# Instalar shiny si no lo has hecho antes
# install.packages("shiny")

library(shiny)

# --- Interfaz de Usuario (UI) ---
# Define cómo se verá la aplicación.
ui <- fluidPage(
  
  # Título de la aplicación
  titlePanel("Formulario de Cliente"),
  
  # Barra lateral con los campos de entrada
  sidebarLayout(
    sidebarPanel(
      # Campo para el ID del cliente
      textInput("id_cliente", "Cx ID:", ""),
      
      # Campo para el teléfono del cliente
      textInput("phone_cliente", "Cx Phone:", ""),
      
      # Botón para mostrar la información
      actionButton("mostrar_info", "Mostrar Información"),
      
      hr(), # Una línea horizontal para separar secciones
      
      # Opciones de saludo
      h4("Elige una opción:"),
      radioButtons("opcion_saludo", "",
                   choices = list("Saludar" = 1, "Salir" = 2),
                   selected = character(0))
    ),
    
    # Panel principal donde se mostrarán los resultados
    mainPanel(
      h4("Información del Cliente:"),
      textOutput("info_display"),
      br(), # Un salto de línea
      h4("Mensaje:"),
      textOutput("saludo_display")
    )
  )
)

# --- Lógica del Servidor ---
# Define cómo funciona la aplicación.
server <- function(input, output) {
  
  # Crea un objeto reactivo para almacenar la información del cliente
  info_data <- eventReactive(input$mostrar_info, {
    paste("Cx ID:", input$id_cliente, "Cx Phone:", input$phone_cliente)
  })
  
  # Muestra la información del cliente cuando se presiona el botón
  output$info_display <- renderText({
    info_data()
  })
  
  # Muestra el mensaje de saludo o despedida basado en la selección
  output$saludo_display <- renderText({
    if (is.null(input$opcion_saludo)) {
      return("Selecciona una opción.")
    }
    
    if (input$opcion_saludo == "1") {
      "¡Hola!"
    } else if (input$opcion_saludo == "2") {
      "¡Adiós!"
    }
  })
  
}

# --- Ejecutar la Aplicación ---
shinyApp(ui = ui, server = server)