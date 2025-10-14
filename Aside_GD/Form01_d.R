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
  
  hr(),
  h4("Dominios"),
  fluidRow(
    column(4, numericInput("dominios_com", "Dominios .com", value = 0, min = 0)),
    column(4, numericInput("dominios_ca", "Dominios .ca", value = 0, min = 0)),
    column(4, numericInput("dominios_otros", "Otros dominios", value = 0, min = 0))
  ),
  
  hr(),
  h4("MS365 sans sécurité"),
  fluidRow(
    column(4, numericInput("ms365_ee", "EE", value = 0, min = 0)),
    column(4, numericInput("ms365_ep", "EP", value = 0, min = 0)),
    column(4, numericInput("ms365_bol", "BOL", value = 0, min = 0))
  ),
  fluidRow(
    column(4, numericInput("ms365_bp", "BP", value = 0, min = 0)),
    column(4, numericInput("ms365_be", "BE", value = 0, min = 0))
  ),
  
  hr(),
  h4("MS365 avec sécurité"),
  fluidRow(
    column(4, numericInput("ms365_ees", "EES", value = 0, min = 0)),
    column(4, numericInput("ms365_eps", "EPS", value = 0, min = 0)),
    column(4, numericInput("ms365_bols", "BOLS", value = 0, min = 0))
  ),
  fluidRow(
    column(4, numericInput("ms365_bps", "BPS", value = 0, min = 0)),
    column(4, numericInput("ms365_bes", "BES", value = 0, min = 0))
  ),
  
  hr(),
  h4("WAM"),
  fluidRow(
    column(3, numericInput("wam_basic", "Basic", value = 0, min = 0)),
    column(3, numericInput("wam_standard", "Standard", value = 0, min = 0)),
    column(3, numericInput("wam_premium", "Premium", value = 0, min = 0)),
    column(3, numericInput("wam_ec", "EC", value = 0, min = 0))
  ),
  
  # --- CAMPOS DE WP AÑADIDOS AQUÍ ---
  hr(),
  h4("WP"),
  fluidRow(
    column(4, numericInput("wp_basic", "Basic", value = 0, min = 0)),
    column(4, numericInput("wp_deluxe", "Deluxe", value = 0, min = 0)),
    column(4, numericInput("wp_ultimate", "Ultimate", value = 0, min = 0))
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
  
  # Nombres de las columnas actualizados
  columnas_nombres <- c("Nombre", "Telefono", "Email", "Fecha_Registro", "DOLO", "ID", 
                        "Dominios_com", "Dominios_ca", "Dominios_otros",
                        "MS365_EE", "MS365_EP", "MS365_BOL", "MS365_BP", "MS365_BE",
                        "MS365_EES", "MS365_EPS", "MS365_BOLS", "MS365_BPS", "MS365_BES",
                        "WAM_Basic", "WAM_Standard", "WAM_Premium", "WAM_EC",
                        "WP_Basic", "WP_Deluxe", "WP_Ultimate") # Nuevas columnas WP
  
  if (file.exists(archivo_csv)) {
    valores$df <- read.csv(archivo_csv, stringsAsFactors = FALSE)
  } else {
    df_vacio <- data.frame(matrix(ncol = length(columnas_nombres), nrow = 0))
    colnames(df_vacio) <- columnas_nombres
    valores$df <- df_vacio
  }
  
  output$form_title <- renderText("Agregar Nuevo Cliente")
  
  # --- Lógica para AGREGAR datos ---
  observeEvent(input$submit, {
    nuevo_registro <- data.frame(
      Nombre = input$nombre, Telefono = input$telefono, Email = input$email,
      Fecha_Registro = as.character(input$fecha_registro), DOLO = input$DOLO, ID = input$ID,
      Dominios_com = input$dominios_com, Dominios_ca = input$dominios_ca, Dominios_otros = input$dominios_otros,
      MS365_EE = input$ms365_ee, MS365_EP = input$ms365_ep, MS365_BOL = input$ms365_bol,
      MS365_BP = input$ms365_bp, MS365_BE = input$ms365_be,
      MS365_EES = input$ms365_ees, MS365_EPS = input$ms365_eps, MS365_BOLS = input$ms365_bols,
      MS365_BPS = input$ms365_bps, MS365_BES = input$ms365_bes,
      WAM_Basic = input$wam_basic, WAM_Standard = input$wam_standard,
      WAM_Premium = input$wam_premium, WAM_EC = input$wam_ec,
      WP_Basic = input$wp_basic, WP_Deluxe = input$wp_deluxe, WP_Ultimate = input$wp_ultimate,
      stringsAsFactors = FALSE
    )
    valores$df <- rbind(valores$df, nuevo_registro)
    write.csv(valores$df, archivo_csv, row.names = FALSE)
    
    # Limpiar todos los campos
    updateTextInput(session, "nombre", value = ""); updateTextInput(session, "telefono", value = "")
    updateTextInput(session, "email", value = ""); updateDateInput(session, "fecha_registro", value = Sys.Date())
    updateTextInput(session, "DOLO", value = ""); updateTextInput(session, "ID", value = "")
    updateNumericInput(session, "dominios_com", value = 0); updateNumericInput(session, "dominios_ca", value = 0)
    updateNumericInput(session, "dominios_otros", value = 0)
    updateNumericInput(session, "ms365_ee", value = 0); updateNumericInput(session, "ms365_ep", value = 0)
    updateNumericInput(session, "ms365_bol", value = 0); updateNumericInput(session, "ms365_bp", value = 0)
    updateNumericInput(session, "ms365_be", value = 0); updateNumericInput(session, "ms365_ees", value = 0)
    updateNumericInput(session, "ms365_eps", value = 0); updateNumericInput(session, "ms365_bols", value = 0)
    updateNumericInput(session, "ms365_bps", value = 0); updateNumericInput(session, "ms365_bes", value = 0)
    updateNumericInput(session, "wam_basic", value = 0); updateNumericInput(session, "wam_standard", value = 0)
    updateNumericInput(session, "wam_premium", value = 0); updateNumericInput(session, "wam_ec", value = 0)
    updateNumericInput(session, "wp_basic", value = 0); updateNumericInput(session, "wp_deluxe", value = 0)
    updateNumericInput(session, "wp_ultimate", value = 0)
  })
  
  # --- Lógica para MODIFICAR datos ---
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
    updateNumericInput(session, "dominios_com", value = fila_datos$Dominios_com)
    updateNumericInput(session, "dominios_ca", value = fila_datos$Dominios_ca)
    updateNumericInput(session, "dominios_otros", value = fila_datos$Dominios_otros)
    updateNumericInput(session, "ms365_ee", value = fila_datos$MS365_EE)
    updateNumericInput(session, "ms365_ep", value = fila_datos$MS365_EP)
    updateNumericInput(session, "ms365_bol", value = fila_datos$MS365_BOL)
    updateNumericInput(session, "ms365_bp", value = fila_datos$MS365_BP)
    updateNumericInput(session, "ms365_be", value = fila_datos$MS365_BE)
    updateNumericInput(session, "ms365_ees", value = fila_datos$MS365_EES)
    updateNumericInput(session, "ms365_eps", value = fila_datos$MS365_EPS)
    updateNumericInput(session, "ms365_bols", value = fila_datos$MS365_BOLS)
    updateNumericInput(session, "ms365_bps", value = fila_datos$MS365_BPS)
    updateNumericInput(session, "ms365_bes", value = fila_datos$MS365_BES)
    updateNumericInput(session, "wam_basic", value = fila_datos$WAM_Basic)
    updateNumericInput(session, "wam_standard", value = fila_datos$WAM_Standard)
    updateNumericInput(session, "wam_premium", value = fila_datos$WAM_Premium)
    updateNumericInput(session, "wam_ec", value = fila_datos$WAM_EC)
    updateNumericInput(session, "wp_basic", value = fila_datos$WP_Basic)
    updateNumericInput(session, "wp_deluxe", value = fila_datos$WP_Deluxe)
    updateNumericInput(session, "wp_ultimate", value = fila_datos$WP_Ultimate)
    
    output$form_title <- renderText("Modificando Cliente")
    shinyjs::hide("submit"); shinyjs::show("update"); shinyjs::show("cancel")
  })
  
  observeEvent(input$update, {
    fila <- valores$row_to_modify
    valores$df[fila, "Nombre"] <- input$nombre; valores$df[fila, "Telefono"] <- input$telefono
    valores$df[fila, "Email"] <- input$email; valores$df[fila, "Fecha_Registro"] <- as.character(input$fecha_registro)
    valores$df[fila, "DOLO"] <- input$DOLO; valores$df[fila, "ID"] <- input$ID
    valores$df[fila, "Dominios_com"] <- input$dominios_com; valores$df[fila, "Dominios_ca"] <- input$dominios_ca
    valores$df[fila, "Dominios_otros"] <- input$dominios_otros
    valores$df[fila, "MS365_EE"] <- input$ms365_ee; valores$df[fila, "MS365_EP"] <- input$ms365_ep
    valores$df[fila, "MS365_BOL"] <- input$ms365_bol; valores$df[fila, "MS365_BP"] <- input$ms365_bp
    valores$df[fila, "MS365_BE"] <- input$ms365_be; valores$df[fila, "MS365_EES"] <- input$ms365_ees
    valores$df[fila, "MS365_EPS"] <- input$ms365_eps; valores$df[fila, "MS365_BOLS"] <- input$ms365_bols
    valores$df[fila, "MS365_BPS"] <- input$ms365_bps; valores$df[fila, "MS365_BES"] <- input$ms365_bes
    valores$df[fila, "WAM_Basic"] <- input$wam_basic; valores$df[fila, "WAM_Standard"] <- input$wam_standard
    valores$df[fila, "WAM_Premium"] <- input$wam_premium; valores$df[fila, "WAM_EC"] <- input$wam_ec
    valores$df[fila, "WP_Basic"] <- input$wp_basic; valores$df[fila, "WP_Deluxe"] <- input$wp_deluxe
    valores$df[fila, "WP_Ultimate"] <- input$wp_ultimate
    
    write.csv(valores$df, archivo_csv, row.names = FALSE)
    
    # Limpiar campos después de guardar
    updateTextInput(session, "nombre", value = ""); updateTextInput(session, "telefono", value = "")
    updateTextInput(session, "email", value = ""); updateDateInput(session, "fecha_registro", value = Sys.Date())
    updateTextInput(session, "DOLO", value = ""); updateTextInput(session, "ID", value = "")
    updateNumericInput(session, "dominios_com", value = 0); updateNumericInput(session, "dominios_ca", value = 0)
    updateNumericInput(session, "dominios_otros", value = 0)
    updateNumericInput(session, "ms365_ee", value = 0); updateNumericInput(session, "ms365_ep", value = 0)
    updateNumericInput(session, "ms365_bol", value = 0); updateNumericInput(session, "ms365_bp", value = 0)
    updateNumericInput(session, "ms365_be", value = 0); updateNumericInput(session, "ms365_ees", value = 0)
    updateNumericInput(session, "ms365_eps", value = 0); updateNumericInput(session, "ms365_bols", value = 0)
    updateNumericInput(session, "ms365_bps", value = 0); updateNumericInput(session, "ms365_bes", value = 0)
    updateNumericInput(session, "wam_basic", value = 0); updateNumericInput(session, "wam_standard", value = 0)
    updateNumericInput(session, "wam_premium", value = 0); updateNumericInput(session, "wam_ec", value = 0)
    updateNumericInput(session, "wp_basic", value = 0); updateNumericInput(session, "wp_deluxe", value = 0)
    updateNumericInput(session, "wp_ultimate", value = 0)
    
    output$form_title <- renderText("Agregar Nuevo Cliente")
    shinyjs::show("submit"); shinyjs::hide("update"); shinyjs::hide("cancel")
  })
  
  observeEvent(input$cancel, {
    # Limpiar todos los campos
    updateTextInput(session, "nombre", value = ""); updateTextInput(session, "telefono", value = "")
    updateTextInput(session, "email", value = ""); updateDateInput(session, "fecha_registro", value = Sys.Date())
    updateTextInput(session, "DOLO", value = ""); updateTextInput(session, "ID", value = "")
    updateNumericInput(session, "dominios_com", value = 0); updateNumericInput(session, "dominios_ca", value = 0)
    updateNumericInput(session, "dominios_otros", value = 0)
    updateNumericInput(session, "ms365_ee", value = 0); updateNumericInput(session, "ms365_ep", value = 0)
    updateNumericInput(session, "ms365_bol", value = 0); updateNumericInput(session, "ms365_bp", value = 0)
    updateNumericInput(session, "ms365_be", value = 0); updateNumericInput(session, "ms365_ees", value = 0)
    updateNumericInput(session, "ms365_eps", value = 0); updateNumericInput(session, "ms365_bols", value = 0)
    updateNumericInput(session, "ms365_bps", value = 0); updateNumericInput(session, "ms365_bes", value = 0)
    updateNumericInput(session, "wam_basic", value = 0); updateNumericInput(session, "wam_standard", value = 0)
    updateNumericInput(session, "wam_premium", value = 0); updateNumericInput(session, "wam_ec", value = 0)
    updateNumericInput(session, "wp_basic", value = 0); updateNumericInput(session, "wp_deluxe", value = 0)
    updateNumericInput(session, "wp_ultimate", value = 0)
    
    output$form_title <- renderText("Agregar Nuevo Cliente")
    shinyjs::show("submit"); shinyjs::hide("update"); shinyjs::hide("cancel")
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