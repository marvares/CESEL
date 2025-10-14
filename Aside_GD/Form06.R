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
  
  hr(),
  h4("Dominios"),
  fluidRow(
    column(4, numericInput("dominios_com", "Dominios .com", value = 0, min = 0)),
    column(4, numericInput("dominios_ca", "Dominios .ca", value = 0, min = 0)),
    column(4, numericInput("dominios_otros", "Otros dominios", value = 0, min = 0))
  ),
  radioButtons("dop", "DOP:", choices = c("Sí", "No"), selected = "No", inline = TRUE),
  
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
  
  hr(),
  h4("WP"),
  fluidRow(
    column(4, numericInput("wp_basic", "Basic", value = 0, min = 0)),
    column(4, numericInput("wp_deluxe", "Deluxe", value = 0, min = 0)),
    column(4, numericInput("wp_ultimate", "Ultimate", value = 0, min = 0))
  ),
  
  hr(),
  h4("cPanel"),
  fluidRow(
    column(3, numericInput("cpanel_economie", "Économie", value = 0, min = 0)),
    column(3, numericInput("cpanel_deluxe", "Deluxe", value = 0, min = 0)),
    column(3, numericInput("cpanel_ultimate", "Ultimate", value = 0, min = 0)),
    column(3, numericInput("cpanel_otros_qty", "Otros (Cant.)", value = 0, min = 0))
  ),
  textInput("cpanel_otros_desc", "Otros (Especificar)", placeholder = "ej: Producto X"),
  
  hr(),
  h4("SSL"),
  fluidRow(
    column(4, numericInput("ssl_standard", "Standard", value = 0, min = 0)),
    column(4, numericInput("ssl_gere", "Géré", value = 0, min = 0)),
    column(4, numericInput("ssl_otro_qty", "Otro (Cant.)", value = 0, min = 0))
  ),
  textInput("ssl_otro_desc", "Otro (Especificar)", placeholder = "ej: SSL Wildcard"),
  
  hr(),
  h4("Call Summary"),
  verbatimTextOutput("call_summary_text"),
  uiOutput("copy_button_ui"),
  
  hr(),
  actionButton("submit", "Guardar Cliente"),
  shinyjs::hidden(actionButton("update", "Actualizar Cliente")),
  shinyjs::hidden(actionButton("cancel", "Cancelar Modificación")),
  hr(),
  
  h4("Clientes Registrados"),
  DT::dataTableOutput("tabla_clientes")
)

# --- Lógica del Servidor (Server) ---
server <- function(input, output, session) {
  
  archivo_csv <- "clientes.csv"
  
  valores <- reactiveValues()
  
  columnas_nombres <- c("Nombre", "Telefono", "Email", "Fecha_Registro", "DOLO", "ID", "Sexo",
                        "New_Sale", "New_Sale_Amount", "New_Sale_Currency", "New_Sale_Currency_Otro",
                        "New_Product_Sold", "New_Product_Sold_Otro",
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
  
  if (file.exists(archivo_csv)) {
    valores$df <- read.csv(archivo_csv, stringsAsFactors = FALSE, colClasses = "character")
  } else {
    df_vacio <- data.frame(matrix(ncol = length(columnas_nombres), nrow = 0))
    colnames(df_vacio) <- columnas_nombres
    valores$df <- df_vacio
  }
  
  output$form_title <- renderText("Agregar Nuevo Cliente")
  
  observeEvent(input$new_sale, { if (input$new_sale == "Yes") shinyjs::show("new_sale_details") else shinyjs::hide("new_sale_details") }, ignoreNULL = FALSE)
  observeEvent(input$new_sale_currency, { if (input$new_sale_currency == "Otro") shinyjs::show("new_sale_currency_otro") else shinyjs::hide("new_sale_currency_otro") }, ignoreNULL = FALSE)
  observeEvent(input$new_product_sold, { if ("Otro" %in% input$new_product_sold) shinyjs::show("new_product_sold_otro") else shinyjs::hide("new_product_sold_otro") }, ignoreNULL = FALSE)
  observeEvent(input$moneda_ultima_orden, { if (input$moneda_ultima_orden == "Otro") shinyjs::show("moneda_ultima_orden_otro") else shinyjs::hide("moneda_ultima_orden_otro") }, ignoreNULL = FALSE)
  observeEvent(input$r2c, { if ("Other" %in% input$r2c) shinyjs::show("r2c_otro") else shinyjs::hide("r2c_otro") }, ignoreNULL = FALSE)
  observeEvent(input$producto_llamada, { if ("Otro" %in% input$producto_llamada) shinyjs::show("producto_llamada_otro") else shinyjs::hide("producto_llamada_otro") }, ignoreNULL = FALSE)
  
  clear_fields <- function() {
    updateTextInput(session, "nombre", value = ""); updateTextInput(session, "telefono", value = ""); updateTextInput(session, "email", value = "");
    updateDateInput(session, "fecha_registro", value = Sys.Date()); updateTextInput(session, "DOLO", value = ""); updateTextInput(session, "ID", value = "");
    updateRadioButtons(session, "sexo", selected = "Masculino"); 
    updateRadioButtons(session, "new_sale", selected = "No");
    updateNumericInput(session, "new_sale_amount", value = 0);
    updateRadioButtons(session, "new_sale_currency", selected = "CAD");
    updateCheckboxGroupInput(session, "new_product_sold", selected = character(0));
    updateNumericInput(session, "mgta", value = 0); updateNumericInput(session, "monto_ultima_orden", value = 0);
    updateRadioButtons(session, "moneda_ultima_orden", selected = "CAD"); 
    updateCheckboxGroupInput(session, "r2c", selected = character(0)); 
    updateCheckboxGroupInput(session, "producto_llamada", selected = character(0)); 
    updateTextAreaInput(session, "notas_venta", value = "");
    updateRadioButtons(session, "dop", selected = "No");
    updateNumericInput(session, "dominios_com", value = 0); updateNumericInput(session, "dominios_ca", value = 0); updateNumericInput(session, "dominios_otros", value = 0)
    updateNumericInput(session, "ms365_ee", value = 0); updateNumericInput(session, "ms365_ep", value = 0); updateNumericInput(session, "ms365_bol", value = 0); updateNumericInput(session, "ms365_bp", value = 0); updateNumericInput(session, "ms365_be", value = 0); updateNumericInput(session, "ms365_ees", value = 0); updateNumericInput(session, "ms365_eps", value = 0); updateNumericInput(session, "ms365_bols", value = 0); updateNumericInput(session, "ms365_bps", value = 0); updateNumericInput(session, "ms365_bes", value = 0)
    updateNumericInput(session, "wam_basic", value = 0); updateNumericInput(session, "wam_standard", value = 0); updateNumericInput(session, "wam_premium", value = 0); updateNumericInput(session, "wam_ec", value = 0)
    updateNumericInput(session, "wp_basic", value = 0); updateNumericInput(session, "wp_deluxe", value = 0); updateNumericInput(session, "wp_ultimate", value = 0)
    updateNumericInput(session, "cpanel_economie", value = 0); updateNumericInput(session, "cpanel_deluxe", value = 0); updateNumericInput(session, "cpanel_ultimate", value = 0); updateNumericInput(session, "cpanel_otros_qty", value = 0); updateTextInput(session, "cpanel_otros_desc", value = "")
    updateNumericInput(session, "ssl_standard", value = 0); updateNumericInput(session, "ssl_gere", value = 0); updateNumericInput(session, "ssl_otro_qty", value = 0); updateTextInput(session, "ssl_otro_desc", value = "")
  }
  
  observeEvent(input$submit, {
    new_data <- data.frame(
      Nombre = input$nombre, Telefono = input$telefono, Email = input$email,
      Fecha_Registro = as.character(input$fecha_registro), DOLO = input$DOLO, ID = input$ID, Sexo = input$sexo,
      New_Sale = input$new_sale, 
      New_Sale_Amount = ifelse(input$new_sale == "Yes", input$new_sale_amount, 0),
      New_Sale_Currency = ifelse(input$new_sale == "Yes", input$new_sale_currency, "CAD"),
      New_Sale_Currency_Otro = ifelse(input$new_sale == "Yes", input$new_sale_currency_otro, ""),
      New_Product_Sold = ifelse(input$new_sale == "Yes", paste(input$new_product_sold, collapse = ", "), ""),
      New_Product_Sold_Otro = ifelse(input$new_sale == "Yes", input$new_product_sold_otro, ""),
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
    new_data_ordered <- new_data[, columnas_nombres]; valores$df <- rbind(valores$df, new_data_ordered)
    write.csv(valores$df, archivo_csv, row.names = FALSE); clear_fields()
  })
  
  observeEvent(input$modify_button, {
    row_to_modify <- as.numeric(sub("modify_", "", input$modify_button)); valores$row_to_modify <- row_to_modify
    fila_datos <- valores$df[row_to_modify, ]
    
    updateTextInput(session, "nombre", value = fila_datos$Nombre); updateTextInput(session, "telefono", value = fila_datos$Telefono); updateTextInput(session, "email", value = fila_datos$Email);
    updateDateInput(session, "fecha_registro", value = as.Date(fila_datos$Fecha_Registro)); updateTextInput(session, "DOLO", value = fila_datos$DOLO); updateTextInput(session, "ID", value = fila_datos$ID);
    updateRadioButtons(session, "sexo", selected = fila_datos$Sexo); 
    updateRadioButtons(session, "new_sale", selected = fila_datos$New_Sale)
    updateNumericInput(session, "new_sale_amount", value = as.numeric(fila_datos$New_Sale_Amount));
    updateRadioButtons(session, "new_sale_currency", selected = fila_datos$New_Sale_Currency);
    updateTextInput(session, "new_sale_currency_otro", value = fila_datos$New_Sale_Currency_Otro);
    prod_sold_selected <- strsplit(fila_datos$New_Product_Sold, ", ")[[1]];
    updateCheckboxGroupInput(session, "new_product_sold", selected = prod_sold_selected);
    updateTextInput(session, "new_product_sold_otro", value = fila_datos$New_Product_Sold_Otro);
    updateNumericInput(session, "mgta", value = as.numeric(fila_datos$MGTA)); updateNumericInput(session, "monto_ultima_orden", value = as.numeric(fila_datos$Monto_ultima_orden));
    updateRadioButtons(session, "moneda_ultima_orden", selected = fila_datos$Moneda_ultima_orden); updateTextInput(session, "moneda_ultima_orden_otro", value = fila_datos$Moneda_ultima_orden_Otro);
    r2c_selected <- strsplit(fila_datos$R2C, ", ")[[1]]; updateCheckboxGroupInput(session, "r2c", selected = r2c_selected);
    updateTextInput(session, "r2c_otro", value = fila_datos$R2C_Otro);
    prod_selected <- strsplit(fila_datos$Producto_Llamada, ", ")[[1]]; updateCheckboxGroupInput(session, "producto_llamada", selected = prod_selected);
    updateTextInput(session, "producto_llamada_otro", value = fila_datos$Producto_Llamada_Otro);
    updateTextAreaInput(session, "notas_venta", value = fila_datos$Notas_Venta);
    updateNumericInput(session, "dominios_com", value = as.numeric(fila_datos$Dominios_com)); updateNumericInput(session, "dominios_ca", value = as.numeric(fila_datos$Dominios_ca)); updateNumericInput(session, "dominios_otros", value = as.numeric(fila_datos$Dominios_otros));
    updateRadioButtons(session, "dop", selected = fila_datos$DOP);
    
    output$form_title <- renderText("Modificando Cliente")
    shinyjs::hide("submit"); shinyjs::show("update"); shinyjs::show("cancel")
  })
  
  observeEvent(input$update, {
    fila <- valores$row_to_modify
    valores$df[fila, ] <- list(
      input$nombre, input$telefono, input$email, as.character(input$fecha_registro), input$DOLO, input$ID, input$sexo,
      input$new_sale, 
      ifelse(input$new_sale == "Yes", input$new_sale_amount, 0),
      ifelse(input$new_sale == "Yes", input$new_sale_currency, "CAD"),
      ifelse(input$new_sale == "Yes", input$new_sale_currency_otro, ""),
      ifelse(input$new_sale == "Yes", paste(input$new_product_sold, collapse = ", "), ""),
      ifelse(input$new_sale == "Yes", input$new_product_sold_otro, ""),
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
    output$form_title <- renderText("Agregar Nuevo Cliente"); shinyjs::show("submit"); shinyjs::hide("update"); shinyjs::hide("cancel")
  })
  
  observeEvent(input$cancel, { clear_fields(); output$form_title <- renderText("Agregar Nuevo Cliente"); shinyjs::show("submit"); shinyjs::hide("update"); shinyjs::hide("cancel") })
  
  observeEvent(input$delete_button, {
    row_to_delete <- as.numeric(sub("delete_", "", input$delete_button))
    valores$df <- valores$df[-row_to_delete, ]; write.csv(valores$df, archivo_csv, row.names = FALSE)
  })
  
  output$tabla_clientes <- DT::renderDataTable({
    df <- valores$df; df$Acciones <- paste(
      sapply(1:nrow(df), function(id) as.character(actionButton(paste0("modify_", id), "Modificar", onclick = paste0('Shiny.setInputValue(\"modify_button\", \"', paste0("modify_", id), '\", {priority: "event"})')))),
      sapply(1:nrow(df), function(id) as.character(actionButton(paste0("delete_", id), "Eliminar", onclick = paste0('Shiny.setInputValue(\"delete_button\", \"', paste0("delete_", id), '\", {priority: "event"})'))))
    )
    DT::datatable(df, escape = FALSE, options = list(scrollX = TRUE, columnDefs = list(list(orderable = FALSE, targets = ncol(df) - 1))))
  })
  
  # --- LÓGICA PARA EL RESUMEN DE LLAMADA (AÑADIR ESTO) ---
  
  summary_text <- reactive({
    r2c_text <- if (!is.null(input$r2c)) paste(input$r2c, collapse = ", ") else "N/A"
    if ("Other" %in% input$r2c) { r2c_text <- sub("Other", paste0("Other (", input$r2c_otro, ")"), r2c_text) }
    
    prod_text <- if (!is.null(input$producto_llamada)) paste(input$producto_llamada, collapse = ", ") else "N/A"
    if ("Otro" %in% input$producto_llamada) { prod_text <- sub("Otro", paste0("Otro (", input$producto_llamada_otro, ")"), prod_text) }
    
    resumen <- paste("Reason for Call:", r2c_text, "\n", "Product Discussed:", prod_text, "\n", "New Sale:", input$new_sale)
    
    if (input$new_sale == "Yes") {
      prod_sold_text <- if (!is.null(input$new_product_sold)) paste(input$new_product_sold, collapse = ", ") else "N/A"
      if ("Otro" %in% input$new_product_sold) { prod_sold_text <- sub("Otro", paste0("Otro (", input$new_product_sold_otro, ")"), prod_sold_text) }
      resumen <- paste(resumen, "\n", "New Product(s) Sold:", prod_sold_text)
    }
    
    resumen <- paste(resumen, "\n\n", "Notes:", "\n", input$notas_venta)
    return(resumen)
  })
  
  output$call_summary_text <- renderText({ summary_text() })
  
  output$copy_button_ui <- renderUI({
    rclipButton("copy_summary", "Copiar Resumen", clipText = summary_text(), icon = icon("clipboard"))
  })
  
}

# --- Ejecutar la Aplicación ---
shinyApp(ui = ui, server = server)