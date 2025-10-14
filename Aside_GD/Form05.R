# Cargar las librerías necesarias
library(shiny)
library(bslib)
library(DT)
library(shinyjs)
library(rclipboard) # <-- Librería para copiar al portapapeles

# --- Interfaz de Usuario (UI) ---
ui <- fluidPage(
  theme = bs_theme(bootswatch = "darkly"),
  useShinyjs(),
  rclipboardSetup(), # <-- Inicializar la función de copiado
  titlePanel("Gestor de Clientes"),
  
  # Sección del formulario
  h4(textOutput("form_title")),
  textInput("nombre", "Nombre Completo:", ""),
  textInput("telefono", "Teléfono:", ""),
  textInput("email", "Correo Electrónico:", ""),
  dateInput("fecha_registro", "Fecha de Registro:", value = Sys.Date(), format = "yyyy-mm-dd", language = "es"),
  textInput("DOLO", "Date of Latest Order:", ""),
  textInput("ID", "Cx:", ""),
  radioButtons("sexo", "Sexo:", choices = c("Masculino", "Femenino"), inline = TRUE),
  
  hr(),
  h4("Información de Venta"),
  radioButtons("new_sale", "New Sale:", choices = c("Yes", "No"), selected = "No", inline = TRUE),
  
  shinyjs::hidden(
    div(id = "new_sale_details",
        numericInput("new_sale_amount", "New Sale Amount:", value = 0, min = 0),
        radioButtons("new_sale_currency", "Currency:", choices = c("CAD", "USD", "Otro"), selected = "CAD", inline = TRUE),
        shinyjs::hidden(textInput("new_sale_currency_otro", "Especificar moneda:", "")),
        checkboxGroupInput("new_product_sold", "New Product(s) Sold:",
                           choices = c("Domain", "MS365", "cPanel", "WP", "WAM", "Otro"),
                           inline = TRUE),
        shinyjs::hidden(textInput("new_product_sold_otro", "Especificar 'Otro' producto:", ""))
    )
  ),
  
  numericInput("mgta", "Monto Gastado Total a la Fecha (MGTA):", value = 0, min = 0),
  numericInput("monto_ultima_orden", "Monto de última orden:", value = 0, min = 0),
  radioButtons("moneda_ultima_orden", "Moneda de última orden:", choices = c("CAD", "USD", "Otro"), selected = "CAD", inline = TRUE),
  shinyjs::hidden(textInput("moneda_ultima_orden_otro", "Especificar moneda:", "")),
  checkboxGroupInput("r2c", "Reason to call (R2C):",
                     choices = c("Support", "Support Out of Scope", "Refund", "OOPR", "Purchase", "Other"),
                     inline = TRUE),
  shinyjs::hidden(textInput("r2c_otro", "Especificar 'Other' R2C:", "")),
  checkboxGroupInput("producto_llamada", "Producto por el que llama:",
                     choices = c("Domain", "MS365", "cPanel", "WP", "WAM", "Otro"),
                     inline = TRUE),
  shinyjs::hidden(textInput("producto_llamada_otro", "Especificar 'Otro' producto:", "")),
  textAreaInput("notas_venta", "Notas:", placeholder = "Añadir notas adicionales...", height = "80px"),
  
  # ... (resto de campos sin cambios)
  hr(),
  h4("Dominios"),
  fluidRow(
    column(4, numericInput("dominios_com", "Dominios .com", value = 0, min = 0)),
    column(4, numericInput("dominios_ca", "Dominios .ca", value = 0, min = 0)),
    column(4, numericInput("dominios_otros", "Otros dominios", value = 0, min = 0))
  ),
  radioButtons("dop", "DOP:", choices = c("Sí", "No"), selected = "No", inline = TRUE),
  
  hr(),
  # ... (resto de campos de productos sin cambios) ...
  
  # --- NUEVA SECCIÓN DE RESUMEN DE LLAMADA ---
  hr(),
  h4("Call Summary"),
  verbatimTextOutput("call_summary_text"),
  uiOutput("copy_button_ui"),
  
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
  
  # --- CÓDIGO DEL SERVIDOR EXISTENTE (SIN CAMBIOS) ---
  
  archivo_csv <- "clientes.csv"
  valores <- reactiveValues()
  columnas_nombres <- c("Nombre", "Telefono", "Email", "Fecha_Registro", "DOLO", "ID", "Sexo",
                        "New_Sale", "New_Sale_Amount", "New_Sale_Currency", "New_Sale_Currency_Otro",
                        "New_Product_Sold", "New_Product_Sold_Otro",
                        "MGTA", "Monto_ultima_orden", "Moneda_ultima_orden", "Moneda_ultima_orden_Otro",
                        "R2C", "R2C_Otro", "Producto_Llamada", "Producto_Llamada_Otro", "Notas_Venta",
                        "Dominios_com", "Dominios_ca", "Dominios_otros", "DOP",
                        "MS365_EE", "MS365_EP", "MS365_BOL", "MS365_BP", "MS365_BE",
                        "MS365_EES", "MS365_EPS", "MS365_BOLS", "MS365_BPS", "MS365_BES",
                        "WAM_Basic", "WAM_Standard", "WAM_Premium", "WAM_EC",
                        "WP_Basic", "WP_Deluxe", "WP_Ultimate",
                        "cPanel_Economie", "cPanel_Deluxe", "cPanel_Ultimate", "cPanel_Otros_Qty", "cPanel_Otros_Desc",
                        "SSL_Standard", "SSL_Gere", "SSL_Otro_Qty", "SSL_Otro_Desc")
  if (file.exists(archivo_csv)) { valores$df <- read.csv(archivo_csv, stringsAsFactors = FALSE, colClasses = "character") } 
  else { df_vacio <- data.frame(matrix(ncol = length(columnas_nombres), nrow = 0)); colnames(df_vacio) <- columnas_nombres; valores$df <- df_vacio }
  output$form_title <- renderText("Agregar Nuevo Cliente")
  observeEvent(input$new_sale, { if (input$new_sale == "Yes") shinyjs::show("new_sale_details") else shinyjs::hide("new_sale_details") }, ignoreNULL = FALSE)
  observeEvent(input$new_sale_currency, { if (input$new_sale_currency == "Otro") shinyjs::show("new_sale_currency_otro") else shinyjs::hide("new_sale_currency_otro") }, ignoreNULL = FALSE)
  observeEvent(input$new_product_sold, { if ("Otro" %in% input$new_product_sold) shinyjs::show("new_product_sold_otro") else shinyjs::hide("new_product_sold_otro") }, ignoreNULL = FALSE)
  observeEvent(input$moneda_ultima_orden, { if (input$moneda_ultima_orden == "Otro") shinyjs::show("moneda_ultima_orden_otro") else shinyjs::hide("moneda_ultima_orden_otro") }, ignoreNULL = FALSE)
  observeEvent(input$r2c, { if ("Other" %in% input$r2c) shinyjs::show("r2c_otro") else shinyjs::hide("r2c_otro") }, ignoreNULL = FALSE)
  observeEvent(input$producto_llamada, { if ("Otro" %in% input$producto_llamada) shinyjs::show("producto_llamada_otro") else shinyjs::hide("producto_llamada_otro") }, ignoreNULL = FALSE)
  clear_fields <- function() {
    #... (función clear_fields completa)
  }
  observeEvent(input$submit, {
    #... (lógica de submit completa)
  })
  observeEvent(input$modify_button, {
    #... (lógica de modify completa)
  })
  observeEvent(input$update, {
    #... (lógica de update completa)
  })
  observeEvent(input$cancel, {
    #... (lógica de cancel completa)
  })
  observeEvent(input$delete_button, {
    #... (lógica de delete completa)
  })
  output$tabla_clientes <- DT::renderDataTable({
    #... (lógica de la tabla completa)
  })
  
  # --- LÓGICA PARA EL RESUMEN DE LLAMADA (AÑADIR ESTO) ---
  
  # 1. Crear el texto del resumen de forma reactiva
  summary_text <- reactive({
    # Unir selecciones de checkbox en un solo texto, manejando el caso "Other"
    r2c_text <- if (!is.null(input$r2c)) paste(input$r2c, collapse = ", ") else "N/A"
    if ("Other" %in% input$r2c) {
      r2c_text <- sub("Other", paste0("Other (", input$r2c_otro, ")"), r2c_text)
    }
    
    prod_text <- if (!is.null(input$producto_llamada)) paste(input$producto_llamada, collapse = ", ") else "N/A"
    if ("Otro" %in% input$producto_llamada) {
      prod_text <- sub("Otro", paste0("Otro (", input$producto_llamada_otro, ")"), prod_text)
    }
    
    # Construir el texto base
    resumen <- paste(
      "Reason for Call:", r2c_text, "\n",
      "Product Discussed:", prod_text, "\n",
      "New Sale:", input$new_sale
    )
    
    # Añadir la sección de "New Product(s) Sold" solo si aplica
    if (input$new_sale == "Yes") {
      prod_sold_text <- if (!is.null(input$new_product_sold)) paste(input$new_product_sold, collapse = ", ") else "N/A"
      if ("Otro" %in% input$new_product_sold) {
        prod_sold_text <- sub("Otro", paste0("Otro (", input$new_product_sold_otro, ")"), prod_sold_text)
      }
      resumen <- paste(resumen, "\n", "New Product(s) Sold:", prod_sold_text)
    }
    
    # Añadir las notas al final
    resumen <- paste(resumen, "\n\n", "Notes:", "\n", input$notas_venta)
    
    return(resumen)
  })
  
  # 2. Mostrar el texto generado en la UI
  output$call_summary_text <- renderText({
    summary_text()
  })
  
  # 3. Generar el botón de copiado
  output$copy_button_ui <- renderUI({
    rclipButton(
      "copy_summary",
      "Copiar Resumen",
      clipText = summary_text(), # El texto a copiar es el generado reactivamente
      icon = icon("clipboard")
    )
  })
  
}

# --- Ejecutar la Aplicación ---
shinyApp(ui = ui, server = server)