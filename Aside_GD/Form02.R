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
  
  sidebarLayout(
    sidebarPanel(
      width = 5,
      h4(textOutput("form_title")),
      
      # --- Campos de Información del Cliente ---
      textInput("nombre", "Nombre Completo:", ""),
      textInput("telefono", "Teléfono:", ""),
      textInput("email", "Correo Electrónico:", ""),
      textInput("DOLC", "Date of Latest Contact:", ""),
      textInput("ID", "Cx:", ""),
      
      hr(),
      
      # --- Sección de Productos con Campos Numéricos ---
      h4("Productos del Cliente"),
      
      wellPanel(
        h5("Dominios"),
        fluidRow(
          column(6, numericInput("dominios_com", ".com", 0, min = 0)),
          column(6, numericInput("dominios_ca", ".ca", 0, min = 0))
        ),
        textInput("dominios_otros", "Otros Dominios", placeholder = "ej: .net, .org")
      ),
      
      wellPanel(
        h5("MS365"),
        checkboxInput("ms365_seguridad", "Con Seguridad", FALSE),
        fluidRow(
          column(6, numericInput("ms365_essential", "Essential", 0, min = 0)),
          column(6, numericInput("ms365_plus", "Plus", 0, min = 0)),
          column(6, numericInput("ms365_business_on_line", "Business On Line", 0, min = 0)),
          column(6, numericInput("ms365_business_professional", "Business Professional", 0, min = 0)),
          column(6, numericInput("ms365_business_enterprise", "Business Enterprise", 0, min = 0))
        )
      ),
      
      wellPanel(
        h5("Hosting, WAM y Wordpress"),
        fluidRow(column(6, strong("WAM")), column(6, strong("Hosting cPanel"))),
        fluidRow(column(6, numericInput("wam_basic", "Basic", 0, min = 0)), column(6, numericInput("hosting_cpanel_economy", "Economy", 0, min = 0))),
        fluidRow(column(6, numericInput("wam_standard", "Standard", 0, min = 0)), column(6, numericInput("hosting_cpanel_deluxe", "Deluxe", 0, min = 0))),
        fluidRow(column(6, numericInput("wam_premium", "Premium", 0, min = 0)), column(6, numericInput("hosting_cpanel_ultimate", "Ultimate", 0, min = 0))),
        fluidRow(column(6, numericInput("wam_e-commerce", "E-Commerce", 0, min = 0)), column(6, strong("Wordpress"))),
        fluidRow(column(6, numericInput("wordpress_basic", "Basic", 0, min = 0)), column(6, numericInput("wordpress_deluxe", "Deluxe", 0, min = 0))),
        fluidRow(column(6, numericInput("wordpress_ultimate", "Ultimate", 0, min = 0)))
      ),
      
      wellPanel(h5("Otros"), textAreaInput("otros_productos", "Otros Productos", placeholder = "Cualquier otro producto o nota")),
      
      hr(),
      
      actionButton("submit", "Guardar Cliente"),
      shinyjs::hidden(actionButton("update", "Actualizar Cliente")),
      shinyjs::hidden(actionButton("cancel", "Cancelar Modificación"))
    ),
    
    mainPanel(
      width = 7,
      h4("Clientes Registrados"),
      DT::dataTableOutput("tabla_clientes")
    )
  )
)

# --- Lógica del Servidor (Server) ---
server <- function(input, output, session) {
  
  archivo_csv <- "clientes.csv"
  
  # --- Definiciones y Helpers ---
  product_levels <- list(
    ms365 = c("essential", "plus", "business_on_line", "business_professional", "business_enterprise"),
    wam = c("basic", "standard", "premium", "e-commerce"),
    hosting_cpanel = c("economy", "deluxe", "ultimate"),
    wordpress = c("basic", "deluxe", "ultimate")
  )
  
  # (Se mantienen las funciones auxiliares de la versión anterior)
  simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "", collapse = " ")
  }
  
  build_shorthand <- function(prefix) { # ... (código sin cambios)
    levels <- product_levels[[prefix]]
    parts <- sapply(levels, function(level) {
      qty <- input[[paste0(prefix, "_", level)]]
      if (!is.na(qty) && qty > 0) {
        level_name <- simpleCap(gsub("_", " ", level))
        return(paste(level_name, "x", qty))
      }
      return(NULL)
    })
    return(paste(unlist(parts), collapse = ", "))
  }
  
  update_numerics_from_shorthand <- function(prefix, text) { # ... (código sin cambios)
    levels <- product_levels[[prefix]]
    for(level in levels) {
      updateNumericInput(session, paste0(prefix, "_", level), value = 0)
    }
    if (!is.null(text) && nchar(text) > 0) {
      parts <- strsplit(text, ", ")[[1]]
      for (part in parts) {
        match <- regexec("(.+?) x (\\d+)", part)
        if (match[[1]][1] != -1) {
          matches <- regmatches(part, match)[[1]]
          name <- tolower(gsub(" ", "_", matches[2]))
          qty <- as.numeric(matches[3])
          updateNumericInput(session, paste0(prefix, "_", name), value = qty)
        }
      }
    }
  }
  
  build_domains_shorthand <- function() { # ... (código sin cambios)
    parts <- c()
    if (!is.na(input$dominios_com) && input$dominios_com > 0) { parts <- c(parts, paste(".com x", input$dominios_com)) }
    if (!is.na(input$dominios_ca) && input$dominios_ca > 0) { parts <- c(parts, paste(".ca x", input$dominios_ca)) }
    if (!is.null(input$dominios_otros) && nchar(input$dominios_otros) > 0) { parts <- c(parts, paste("Otros:", input$dominios_otros)) }
    return(paste(parts, collapse = ", "))
  }
  
  update_domains_from_shorthand <- function(text) { # ... (código sin cambios)
    updateNumericInput(session, "dominios_com", value = 0)
    updateNumericInput(session, "dominios_ca", value = 0)
    updateTextInput(session, "dominios_otros", value = "")
    if (!is.null(text) && nchar(text) > 0) {
      parts <- strsplit(text, ", ")[[1]]
      for (part in parts) {
        if (grepl("\\.com x \\d+", part)) { updateNumericInput(session, "dominios_com", value = as.numeric(sub("\\.com x ", "", part))) }
        else if (grepl("\\.ca x \\d+", part)) { updateNumericInput(session, "dominios_ca", value = as.numeric(sub("\\.ca x ", "", part))) }
        else if (grepl("Otros:", part)) { updateTextInput(session, "dominios_otros", value = sub("Otros: ", "", part)) }
      }
    }
  }
  
  # --- Estructura de Datos (CORREGIDA) ---
  columnas_nombres <- c("Nombre", "Telefono", "Email", "DOLC", "ID",
                        "Dominios", "MS365_Seguridad", "MS365_Productos",
                        "WAM", "Hosting_cPanel", "Wordpress", # <-- COLUMNAS FALTANTES AÑADIDAS
                        "Otros_Productos")
  
  valores <- reactiveValues()
  if (file.exists(archivo_csv)) {
    valores$df <- read.csv(archivo_csv, stringsAsFactors = FALSE)
  } else {
    valores$df <- data.frame(matrix(ncol = length(columnas_nombres), nrow = 0))
    colnames(valores$df) <- columnas_nombres
  }
  
  # --- Lógica de la App ---
  output$form_title <- renderText("Agregar Nuevo Cliente")
  
  clear_fields <- function() {
    updateTextInput(session, "nombre", value = ""); updateTextInput(session, "telefono", value = "")
    updateTextInput(session, "email", value = ""); updateTextInput(session, "DOLC", value = "")
    updateTextInput(session, "ID", value = ""); updateTextAreaInput(session, "otros_productos", value = "")
    update_domains_from_shorthand("")
    updateCheckboxInput(session, "ms365_seguridad", value = FALSE)
    for(prefix in names(product_levels)) { update_numerics_from_shorthand(prefix, "") }
  }
  
  # --- Eventos de Botones (CORREGIDOS) ---
  observeEvent(input$submit, {
    # Esta estructura ahora coincide con `columnas_nombres`
    nuevo_registro <- data.frame(
      Nombre = input$nombre, Telefono = input$telefono, Email = input$email, DOLC = input$DOLC, ID = input$ID,
      Dominios = build_domains_shorthand(),
      MS365_Seguridad = input$ms365_seguridad, MS365_Productos = build_shorthand("ms365"),
      WAM = build_shorthand("wam"),
      Hosting_cPanel = build_shorthand("hosting_cpanel"),
      Wordpress = build_shorthand("wordpress"),
      Otros_Productos = input$otros_productos,
      stringsAsFactors = FALSE
    )
    valores$df <- rbind(valores$df, nuevo_registro)
    write.csv(valores$df, archivo_csv, row.names = FALSE)
    clear_fields()
  })
  
  observeEvent(input$modify_button, {
    row_to_modify <- as.numeric(sub("modify_", "", input$modify_button))
    valores$row_to_modify <- row_to_modify
    fila_datos <- valores$df[row_to_modify, ]
    
    updateTextInput(session, "nombre", value = fila_datos$Nombre); updateTextInput(session, "telefono", value = fila_datos$Telefono)
    updateTextInput(session, "email", value = fila_datos$Email); updateTextInput(session, "DOLC", value = fila_datos$DOLC)
    updateTextInput(session, "ID", value = fila_datos$ID); updateTextAreaInput(session, "otros_productos", value = fila_datos$Otros_Productos)
    update_domains_from_shorthand(fila_datos$Dominios)
    updateCheckboxInput(session, "ms365_seguridad", value = fila_datos$MS365_Seguridad)
    update_numerics_from_shorthand("ms365", fila_datos$MS365_Productos)
    update_numerics_from_shorthand("wam", fila_datos$WAM)
    update_numerics_from_shorthand("hosting_cpanel", fila_datos$Hosting_cPanel)
    update_numerics_from_shorthand("wordpress", fila_datos$Wordpress)
    
    output$form_title <- renderText("Modificando Cliente")
    shinyjs::hide("submit"); shinyjs::show("update"); shinyjs::show("cancel")
  })
  
  observeEvent(input$update, {
    fila <- valores$row_to_modify
    # Esta lista ahora coincide con `columnas_nombres`
    valores$df[fila, ] <- list(
      input$nombre, input$telefono, input$email, input$DOLC, input$ID,
      build_domains_shorthand(),
      input$ms365_seguridad, build_shorthand("ms365"),
      build_shorthand("wam"),
      build_shorthand("hosting_cpanel"),
      build_shorthand("wordpress"),
      input$otros_productos
    )
    write.csv(valores$df, archivo_csv, row.names = FALSE)
    
    clear_fields()
    output$form_title <- renderText("Agregar Nuevo Cliente")
    shinyjs::show("submit"); shinyjs::hide("update"); shinyjs::hide("cancel")
  })
  
  observeEvent(input$cancel, { clear_fields(); output$form_title <- renderText("Agregar Nuevo Cliente"); shinyjs::show("submit"); shinyjs::hide("update"); shinyjs::hide("cancel") })
  
  observeEvent(input$delete_button, {
    row_to_delete <- as.numeric(sub("delete_", "", input$delete_button))
    valores$df <- valores$df[-row_to_delete, ]
    write.csv(valores$df, archivo_csv, row.names = FALSE)
  })
  
  # --- Mostrar la TABLA de datos ---
  output$tabla_clientes <- DT::renderDataTable({
    df <- valores$df
    df$Acciones <- paste(
      sapply(1:nrow(df), function(id) as.character(actionButton(paste0("modify_", id), "Modificar", onclick = paste0('Shiny.setInputValue(\"modify_button\", \"', paste0("modify_", id), '\", {priority: "event"})')))),
      sapply(1:nrow(df), function(id) as.character(actionButton(paste0("delete_", id), "Eliminar", onclick = paste0('Shiny.setInputValue(\"delete_button\", \"', paste0("delete_", id), '\", {priority: "event"})'))))
    )
    DT::datatable(df, escape = FALSE, options = list(scrollX = TRUE, columnDefs = list(list(orderable = FALSE, targets = ncol(df) - 1))))
  })
}

# --- Ejecutar la Aplicación ---
shinyApp(ui = ui, server = server)