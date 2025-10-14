# Cargar las librerías necesarias
library(shiny)
library(bslib)
library(DT)
library(shinyjs)
library(rclipboard) # <-- Nueva librería

# --- Interfaz de Usuario (UI) ---
ui <- fluidPage(
  theme = bs_theme(bootswatch = "darkly"),
  useShinyjs(),
  rclipboardSetup(), # <-- Inicializar rclipboard
  titlePanel("Gestor de Clientes"),
  
  # ... (El resto de la UI es igual hasta los botones de acción)
  
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
        shinyjs::hidden(textInput("new_sale_currency_otro", "Especificar moneda:", ""))
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
  
  hr(),
  # ... (resto de las secciones de productos sin cambios) ...
  
  # --- NUEVA SECCIÓN DE TEXTO AUTOGENERADO ---
  hr(),
  h4("Texto Autogenerado"),
  verbatimTextOutput("texto_generado"), # Cuadro para mostrar el texto
  uiOutput("copy_button_ui"), # Espacio para el botón de copiar
  
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
  
  # ... (Todo el código del servidor hasta el final es igual)
  
  # --- LÓGICA PARA TEXTO AUTOGENERADO (AÑADIR AL FINAL DEL SERVER) ---
  
  # 1. Crear el texto de forma reactiva
  texto_reactivo <- eventReactive(list(input$nombre, input$r2c, input$producto_llamada, input$notas_venta), {
    
    # Unir las selecciones de los checkbox en un solo texto
    r2c_texto <- if (!is.null(input$r2c)) paste(input$r2c, collapse = ", ") else "N/A"
    producto_texto <- if (!is.null(input$producto_llamada)) paste(input$producto_llamada, collapse = ", ") else "N/A"
    
    # Construir la plantilla del texto
    paste(
      "Resumen de la llamada con:", input$nombre, "\n",
      "-----------------------------------\n",
      "Razón de la llamada (R2C):", r2c_texto, "\n",
      "Producto principal:", producto_texto, "\n",
      "Notas Adicionales:", "\n",
      input$notas_venta, "\n",
      "-----------------------------------"
    )
  })
  
  # 2. Mostrar el texto en el cuadro de la UI
  output$texto_generado <- renderText({
    texto_reactivo()
  })
  
  # 3. Crear el botón de copiar con el texto reactivo
  output$copy_button_ui <- renderUI({
    rclipButton(
      "copy_button",
      label = "Copiar Resumen al Portapapeles",
      clipText = texto_reactivo(), # El texto a copiar es el que generamos
      icon = icon("clipboard")
    )
  })
  
  
  # --- El resto del código del servidor (sin cambios) ---
  archivo_csv <- "clientes.csv"
  valores <- reactiveValues()
  columnas_nombres <- c("Nombre", "Telefono", "Email", "Fecha_Registro", "DOLO", "ID", "Sexo",
                        "New_Sale", "New_Sale_Amount", "New_Sale_Currency", "New_Sale_Currency_Otro",
                        "MGTA", "Monto_ultima_orden", "Moneda_ultima_orden", "Moneda_ultima_orden_Otro",
                        "R2C", "R2C_Otro",
                        "Producto_Llamada", "Producto_Llamada_Otro",
                        "Notas_Venta",
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
  observeEvent(input$moneda_ultima_orden, { if (input$moneda_ultima_orden == "Otro") shinyjs::show("moneda_ultima_orden_otro") else shinyjs::hide("moneda_ultima_orden_otro") }, ignoreNULL = FALSE)
  observeEvent(input$r2c, { if ("Other" %in% input$r2c) shinyjs::show("r2c_otro") else shinyjs::hide("r2c_otro") }, ignoreNULL = FALSE)
  observeEvent(input$producto_llamada, { if ("Otro" %in% input$producto_llamada) shinyjs::show("producto_llamada_otro") else shinyjs::hide("producto_llamada_otro") }, ignoreNULL = FALSE)
  clear_fields <- function() {
    updateTextInput(session, "nombre", value = ""); updateTextInput(session, "telefono", value = ""); updateTextInput(session, "email", value = "");
    updateDateInput(session, "fecha_registro", value = Sys.Date()); updateTextInput(session, "DOLO", value = ""); updateTextInput(session, "ID", value = "");
    updateRadioButtons(session, "sexo", selected = "Masculino"); updateRadioButtons(session, "new_sale", selected = "No");
    updateNumericInput(session, "new_sale_amount", value = 0); updateRadioButtons(session, "new_sale_currency", selected = "CAD");
    updateNumericInput(session, "mgta", value = 0); updateNumericInput(session, "monto_ultima_orden", value = 0);
    updateRadioButtons(session, "moneda_ultima_orden", selected = "CAD"); 
    updateCheckboxGroupInput(session, "r2c", selected = character(0)); updateCheckboxGroupInput(session, "producto_llamada", selected = character(0)); 
    updateTextAreaInput(session, "notas_venta", value = ""); updateRadioButtons(session, "dop", selected = "No");
  }
  observeEvent(input$submit, {
    new_data <- data.frame(
      Nombre = input$nombre, Telefono = input$telefono, Email = input$email,
      Fecha_Registro = as.character(input$fecha_registro), DOLO = input$DOLO, ID = input$ID, Sexo = input$sexo,
      New_Sale = input$new_sale, New_Sale_Amount = input$new_sale_amount, New_Sale_Currency = input$new_sale_currency, New_Sale_Currency_Otro = input$new_sale_currency_otro,
      MGTA = input$mgta, Monto_ultima_orden = input$monto_ultima_orden,
      Moneda_ultima_orden = input$moneda_ultima_orden, Moneda_ultima_orden_Otro = input$moneda_ultima_orden_otro,
      R2C = paste(input$r2c, collapse = ", "), R2C_Otro = input$r2c_otro,
      Producto_Llamada = paste(input$producto_llamada, collapse = ", "), Producto_Llamada_Otro = input$producto_llamada_otro,
      Notas_Venta = input$notas_venta,
      Dominios_com = input$dominios_com, Dominios_ca = input$dominios_ca, Dominios_otros = input$dominios_otros, DOP = input$dop,
      MS365_EE = input$ms365_ee, MS365_EP = input$ms365_ep, MS365_BOL = input$ms365_bol, MS365_BP = input$ms365_bp, MS365_BE = input$ms365_be,
      MS365_EES = input$ms365_ees, MS365_EPS = input$ms365_eps, MS365_BOLS = input$ms365_bols, MS365_BPS = input$ms365_bps, MS365_BES = input$ms365_bes,
      WAM_Basic = input$wam_basic, WAM_Standard = input$wam_standard, WAM_Premium = input$wam_premium, WAM_EC = input$wam_ec,
      WP_Basic = input$wp_basic, WP_Deluxe = input$wp_deluxe, WP_Ultimate = input$wp_ultimate,
      cPanel_Economie = input$cpanel_economie, cPanel_Deluxe = input$cpanel_deluxe, cPanel_Ultimate = input$cpanel_ultimate, cPanel_Otros_Qty = input$cpanel_otros_qty, cPanel_Otros_Desc = input$cpanel_otros_desc,
      SSL_Standard = input$ssl_standard, SSL_Gere = input$ssl_gere, SSL_Otro_Qty = input$ssl_otro_qty, SSL_Otro_Desc = input$ssl_otro_desc,
      stringsAsFactors = FALSE
    )
    new_data_ordered <- new_data[, columnas_nombres]; valores$df <- rbind(valores$df, new_data_ordered);
    write.csv(valores$df, archivo_csv, row.names = FALSE); clear_fields()
  })
  observeEvent(input$modify_button, {
    row_to_modify <- as.numeric(sub("modify_", "", input$modify_button)); valores$row_to_modify <- row_to_modify;
    fila_datos <- valores$df[row_to_modify, ];
    updateTextInput(session, "nombre", value = fila_datos$Nombre); updateTextInput(session, "telefono", value = fila_datos$Telefono); updateTextInput(session, "email", value = fila_datos$Email);
    updateDateInput(session, "fecha_registro", value = as.Date(fila_datos$Fecha_Registro)); updateTextInput(session, "DOLO", value = fila_datos$DOLO); updateTextInput(session, "ID", value = fila_datos$ID);
    updateRadioButtons(session, "sexo", selected = fila_datos$Sexo); updateRadioButtons(session, "new_sale", selected = fila_datos$New_Sale);
    updateNumericInput(session, "new_sale_amount", value = as.numeric(fila_datos$New_Sale_Amount)); updateRadioButtons(session, "new_sale_currency", selected = fila_datos$New_Sale_Currency); updateTextInput(session, "new_sale_currency_otro", value = fila_datos$New_Sale_Currency_Otro);
    updateNumericInput(session, "mgta", value = as.numeric(fila_datos$MGTA)); updateNumericInput(session, "monto_ultima_orden", value = as.numeric(fila_datos$Monto_ultima_orden));
    updateRadioButtons(session, "moneda_ultima_orden", selected = fila_datos$Moneda_ultima_orden); updateTextInput(session, "moneda_ultima_orden_otro", value = fila_datos$Moneda_ultima_orden_Otro);
    r2c_selected <- strsplit(fila_datos$R2C, ", ")[[1]]; updateCheckboxGroupInput(session, "r2c", selected = r2c_selected);
    updateTextInput(session, "r2c_otro", value = fila_datos$R2C_Otro);
    prod_selected <- strsplit(fila_datos$Producto_Llamada, ", ")[[1]]; updateCheckboxGroupInput(session, "producto_llamada", selected = prod_selected);
    updateTextInput(session, "producto_llamada_otro", value = fila_datos$Producto_Llamada_Otro);
    updateTextAreaInput(session, "notas_venta", value = fila_datos$Notas_Venta);
    updateNumericInput(session, "dominios_com", value = as.numeric(fila_datos$Dominios_com)); updateNumericInput(session, "dominios_ca", value = as.numeric(fila_datos$Dominios_ca)); updateNumericInput(session, "dominios_otros", value = as.numeric(fila_datos$Dominios_otros));
    updateRadioButtons(session, "dop", selected = fila_datos$DOP);
    output$form_title <- renderText("Modificando Cliente");
    shinyjs::hide("submit"); shinyjs::show("update"); shinyjs::show("cancel")
  })
  observeEvent(input$update, {
    fila <- valores$row_to_modify
    valores$df[fila, ] <- list(
      input$nombre, input$telefono, input$email, as.character(input$fecha_registro), input$DOLO, input$ID, input$sexo,
      input$new_sale, input$new_sale_amount, input$new_sale_currency, input$new_sale_currency_otro,
      input$mgta, input$monto_ultima_orden, input$moneda_ultima_orden, input$moneda_ultima_orden_otro,
      paste(input$r2c, collapse = ", "), input$r2c_otro,
      paste(input$producto_llamada, collapse = ", "), input$producto_llamada_otro,
      input$notas_venta,
      input$dominios_com, input$dominios_ca, input$dominios_otros, input$dop,
      input$ms365_ee, input$ms365_ep, input$ms365_bol, input$ms365_bp, input$ms365_be,
      input$ms365_ees, input$ms365_eps, input$ms365_bols, input$ms365_bps, input$ms365_bes,
      input$wam_basic, input$wam_standard, input$wam_premium, input$wam_ec,
      input$wp_basic, input$wp_deluxe, input$wp_ultimate,
      input$cpanel_economie, input$cpanel_deluxe, input$cpanel_ultimate, input$cpanel_otros_qty, input$cpanel_otros_desc,
      input$ssl_standard, input$ssl_gere, input$ssl_otro_qty, input$ssl_otro_desc
    )
    write.csv(valores$df, archivo_csv, row.names = FALSE); clear_fields();
    output$form_title <- renderText("Agregar Nuevo Cliente");
    shinyjs::show("submit"); shinyjs::hide("update"); shinyjs::hide("cancel")
  })
  observeEvent(input$cancel, { clear_fields(); output$form_title <- renderText("Agregar Nuevo Cliente"); shinyjs::show("submit"); shinyjs::hide("update"); shinyjs::hide("cancel") })
  observeEvent(input$delete_button, {
    row_to_delete <- as.numeric(sub("delete_", "", input$delete_button))
    valores$df <- valores$df[-row_to_delete, ]
    write.csv(valores$df, archivo_csv, row.names = FALSE)
  })
  output$tabla_clientes <- DT::renderDataTable({
    df <- valores$df; df$Acciones <- paste(
      sapply(1:nrow(df), function(id) as.character(actionButton(paste0("modify_", id), "Modificar", onclick = paste0('Shiny.setInputValue(\"modify_button\", \"', paste0("modify_", id), '\", {priority: "event"})')))),
      sapply(1:nrow(df), function(id) as.character(actionButton(paste0("delete_", id), "Eliminar", onclick = paste0('Shiny.setInputValue(\"delete_button\", \"', paste0("delete_", id), '\", {priority: "event"})'))))
    )
    DT::datatable(df, escape = FALSE, options = list(scrollX = TRUE, columnDefs = list(list(orderable = FALSE, targets = ncol(df) - 1))))
  })
  
}

# --- Ejecutar la Aplicación ---
shinyApp(ui = ui, server = server)