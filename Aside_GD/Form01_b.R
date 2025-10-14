# Cargar las librerías necesarias
library(shiny)
library(bslib)
library(DT)
library(shinyjs)

# --- Interfaz de Usuario (UI) ---
ui <- fluidPage(
  theme = bs_theme(bootswatch = "darkly"),
  useShinyjs(),
  titlePanel("Gestor de Clientes"),
  
  # Sección del formulario
  h4(textOutput("form_title")),
  textInput("nombre", "Nombre Completo:", ""),
  textInput("telefono", "Teléfono:", ""),
  textInput("email", "Correo Electrónico:", ""),
  dateInput("fecha_registro", "Fecha de Registro:", value = Sys.Date(), format = "yyyy-mm-dd", language = "es"),
  textInput("DOLO", "Date of Latest Order:", ""),
  textInput("ID", "Cx:", ""),
  
  # CAMPOS NUMÉRICOS AÑADIDOS AQUÍ
  hr(),
  h4("Dominios"),
  fluidRow(
    column(4, numericInput("dominios_com", "Dominios .com", value = 0, min = 0)),
    column(4, numericInput("dominios_ca", "Dominios .ca", value = 0, min = 0)),
    column(4, numericInput("dominios_otros", "Otros dominios", value = 0, min = 0))
  ),
  
  # Botones de acción
  hr(),
  actionButton("submit", "Guardar Cliente"),
  shinyjs::hidden(actionButton("update", "Actualizar Cliente")),
  shinyjs::hidden(actionButton("cancel", "Cancelar Modificación")),
  hr(),
  
  # Sección de la tabla de clientes
  h4("Clientes Registrados"),
  DT::dataTableOutput("tabla_clientes")
)

# --- Lógica del Servidor (Server) ---
server <- function(input, output, session) {
  
  archivo_csv <- "clientes.csv"
  
  # --- Gestión de Datos Reactivos ---
  valores <- reactiveValues()
  
  # Nombres de las columnas para asegurar consistencia (CON NUEVOS CAMPOS)
  columnas_nombres <- c("Nombre", "Telefono", "Email", "Fecha_Registro", "DOLO", "ID", 
                        "Dominios_com", "Dominios_ca", "Dominios_otros")
  
  if (file.exists(archivo_csv)) {
    valores$df <- read.csv(archivo_csv, stringsAsFactors = FALSE)
  } else {
    # Creamos un dataframe vacío con la estructura correcta
    df_vacio <- data.frame(matrix(ncol = length(columnas_nombres), nrow = 0))
    colnames(df_vacio) <- columnas_nombres
    valores$df <- df_vacio
  }
  
  # Título dinámico para el formulario
  output$form_title <- renderText("Agregar Nuevo Cliente")
  
  # --- Lógica para AGREGAR datos ---
  observeEvent(input$submit, {
    nuevo_registro <- data.frame(
      Nombre = input$nombre,
      Telefono = input$telefono,
      Email = input$email,
      Fecha_Registro = as.character(input$fecha_registro),
      DOLO = input$DOLO,
      ID = input$ID,
      Dominios_com = input$dominios_com, # Guardar nuevo campo
      Dominios_ca = input$dominios_ca,   # Guardar nuevo campo
      Dominios_otros = input$dominios_otros, # Guardar nuevo campo
      stringsAsFactors = FALSE
    )
    valores$df <- rbind(valores$df, nuevo_registro)
    write.csv(valores$df, archivo_csv, row.names = FALSE)
    
    # Limpiar todos los campos
    updateTextInput(session, "nombre", value = "")
    updateTextInput(session, "telefono", value = "")
    updateTextInput(session, "email", value = "")
    updateDateInput(session, "fecha_registro", value = Sys.Date())
    updateTextInput(session, "DOLO", value = "")
    updateTextInput(session, "ID", value = "")
    updateNumericInput(session, "dominios_com", value = 0) # Resetear nuevo campo
    updateNumericInput(session, "dominios_ca", value = 0)   # Resetear nuevo campo
    updateNumericInput(session, "dominios_otros", value = 0) # Resetear nuevo campo
  })
  
  # --- Lógica para MODIFICAR datos ---
  
  # 1. Cargar datos en el formulario
  observeEvent(input$modify_button, {
    row_to_modify <- as.numeric(sub("modify_", "", input$modify_button))
    valores$row_to_modify <- row_to_modify
    
    fila_datos <- valores$df[row_to_modify, ]
    
    updateTextInput(session, "nombre", value = fila_datos$Nombre)
    updateTextInput(session, "telefono", value = fila_datos$Telefono)
    updateTextInput(session, "email", value = fila_datos$Email)
    updateDateInput(session, "fecha_registro", value = as.Date(fila_datos$Fecha_Registro))
    updateTextInput(session, "DOLO", value = fila_datos$DOLO)
    updateTextInput(session, "ID", value = fila_datos$ID)
    updateNumericInput(session, "dominios_com", value = fila_datos$Dominios_com) # Cargar nuevo campo
    updateNumericInput(session, "dominios_ca", value = fila_datos$Dominios_ca)   # Cargar nuevo campo
    updateNumericInput(session, "dominios_otros", value = fila_datos$Dominios_otros) # Cargar nuevo campo
    
    output$form_title <- renderText("Modificando Cliente")
    shinyjs::hide("submit")
    shinyjs::show("update")
    shinyjs::show("cancel")
  })
  
  # 2. Guardar los cambios
  observeEvent(input$update, {
    fila <- valores$row_to_modify
    valores$df[fila, "Nombre"] <- input$nombre
    valores$df[fila, "Telefono"] <- input$telefono
    valores$df[fila, "Email"] <- input$email
    valores$df[fila, "Fecha_Registro"] <- as.character(input$fecha_registro)
    valores$df[fila, "DOLO"] <- input$DOLO
    valores$df[fila, "ID"] <- input$ID
    valores$df[fila, "Dominios_com"] <- input$dominios_com # Actualizar nuevo campo
    valores$df[fila, "Dominios_ca"] <- input$dominios_ca   # Actualizar nuevo campo
    valores$df[fila, "Dominios_otros"] <- input$dominios_otros # Actualizar nuevo campo
    
    write.csv(valores$df, archivo_csv, row.names = FALSE)
    
    # Limpiar y restaurar
    updateTextInput(session, "nombre", value = "")
    updateTextInput(session, "telefono", value = "")
    updateTextInput(session, "email", value = "")
    updateDateInput(session, "fecha_registro", value = Sys.Date())
    updateTextInput(session, "DOLO", value = "")
    updateTextInput(session, "ID", value = "")
    updateNumericInput(session, "dominios_com", value = 0)
    updateNumericInput(session, "dominios_ca", value = 0)
    updateNumericInput(session, "dominios_otros", value = 0)
    
    output$form_title <- renderText("Agregar Nuevo Cliente")
    shinyjs::show("submit")
    shinyjs::hide("update")
    shinyjs::hide("cancel")
  })
  
  # 3. Cancelar la modificación
  observeEvent(input$cancel, {
    updateTextInput(session, "nombre", value = "")
    updateTextInput(session, "telefono", value = "")
    updateTextInput(session, "email", value = "")
    updateDateInput(session, "fecha_registro", value = Sys.Date())
    updateTextInput(session, "DOLO", value = "")
    updateTextInput(session, "ID", value = "")
    updateNumericInput(session, "dominios_com", value = 0)
    updateNumericInput(session, "dominios_ca", value = 0)
    updateNumericInput(session, "dominios_otros", value = 0)
    
    output$form_title <- renderText("Agregar Nuevo Cliente")
    shinyjs::show("submit")
    shinyjs::hide("update")
    shinyjs::hide("cancel")
  })
  
  # --- Lógica para ELIMINAR datos ---
  observeEvent(input$delete_button, {
    row_to_delete <- as.numeric(sub("delete_", "", input$delete_button))
    valores$df <- valores$df[-row_to_delete, ]
    write.csv(valores$df, archivo_csv, row.names = FALSE)
  })
  
  # --- Mostrar la TABLA de datos ---
  output$tabla_clientes <- DT::renderDataTable({
    df <- valores$df
    df$Acciones <- paste(
      sapply(1:nrow(df), function(id) {
        as.character(actionButton(paste0("modify_", id), "Modificar", onclick = paste0('Shiny.setInputValue(\"modify_button\", \"', paste0("modify_", id), '\", {priority: "event"})')))
      }),
      sapply(1:nrow(df), function(id) {
        as.character(actionButton(paste0("delete_", id), "Eliminar", onclick = paste0('Shiny.setInputValue(\"delete_button\", \"', paste0("delete_", id), '\", {priority: "event"})')))
      })
    )
    DT::datatable(df, escape = FALSE, options = list(scrollX = TRUE, columnDefs = list(list(orderable = FALSE, targets = ncol(df) - 1))))
  })
  
}

# --- Ejecutar la Aplicación ---
shinyApp(ui = ui, server = server)