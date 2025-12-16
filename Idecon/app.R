


# Librerías necesarias
library(shiny)
library(readxl)
library(dplyr)
library(stringr)
library(lubridate)

# ---------------------------
# Función para limpiar texto
# ---------------------------
cleanData <- function(x){
  x <- if_else(is.na(x), "", x)
  x <- str_trim(x)
  x <- str_squish(x)
  x <- str_to_upper(x)
  x <- str_remove_all(x, pattern = '["|;]')
  x <- str_replace_all(x, "\u00C4", "A")  # Ä
  x <- str_replace_all(x, "\u00C1", "A")  # Á
  x <- str_replace_all(x, "\u00CB", "E")  # Ë
  x <- str_replace_all(x, "\u00C9", "E")  # É
  x <- str_replace_all(x, "\u00CF", "I")  # Ï
  x <- str_replace_all(x, "\u00CD", "I")  # Í
  x <- str_replace_all(x, "\u00D6", "O")  # Ö
  x <- str_replace_all(x, "\u00D3", "O")  # Ó
  x <- str_replace_all(x, "\u00DC", "U")  # Ü
  x <- str_replace_all(x, "\u00DA", "U")  # Ú
  x <- str_replace_all(x, "\u00AA", "")   # ª
  return(x)
}

# -------------------------------------------------
# Función principal: devuelve lista con
#   - data: dataframe final
#   - removed_col: TRUE si 'PEP PRINCIPALES' existía (y se eliminó), FALSE si no estaba
# -------------------------------------------------
rr_uploaded_pep <- function(df_nombramientos, df_parientes){
  
  # ITEM correlativo para parientes
  max_item <- suppressWarnings(max(as.integer(df_nombramientos$ITEM), na.rm = TRUE))
  max_item <- ifelse(is.finite(max_item), max_item, 0)
  item <- seq(from = max_item + 1, length.out = nrow(df_parientes)) %>% as.character()
  
  df_parientes <- df_parientes %>%
    rename("NOMBRE 1" = "NOMBRES") %>%
    mutate(
      ITEM = item,
      `TIPO DE ENTIDAD` = "FAMILIAR",
      ENTIDAD = "",
      CARGO = "",
      `NOMBRE 2` = "",
      `NOMBRE 3` = ""
    ) %>%
    # Aseguramos el orden esperado que tenías
    select(6, 7, 8, 9, 3, 4, 5, 10, 11, 1, 2)
  
  df_nombramientos <- df_nombramientos %>% mutate(ITEM = as.character(ITEM))
  
  # Unión
  df_pep <- bind_rows(df_nombramientos, df_parientes)
  
  # Guardamos si existe la columna PEP PRINCIPALES antes de eliminarla
  had_pep_principales <- "PEP PRINCIPALES" %in% names(df_pep)
  
  fecha <- format(Sys.Date(), "%d/%m/%Y")
  
  # Limpiamos texto en todas las columnas
  df_pep <- df_pep %>% mutate(across(everything(), cleanData))
  
  # Remover dígitos en columnas 2:9 solo si existen esas posiciones
  idx_cols <- intersect(2:9, seq_along(df_pep))
  if (length(idx_cols) > 0) {
    df_pep[, idx_cols] <- lapply(df_pep[, idx_cols, drop = FALSE], function(col) {
      if (is.character(col)) {
        str_replace_all(col, "\\d+", "")
      } else {
        col
      }
    })
  }
  
  # Ajustes de campos fijos
  df_pep <- df_pep %>%
    mutate(
      `TIPO DE DOCUMENTO` = if_else(`TIPO DE DOCUMENTO` == "CE", "CEX", `TIPO DE DOCUMENTO`),
      RESOLUCION = paste("Actualizacion", fecha),
      FECHA_INICIO = fecha,
      FECHA_PUBLICACION = fecha
    ) %>%
    # -----> ELIMINAR LA COLUMNA PEP PRINCIPALES SI EXISTE
    select(-any_of("PEP PRINCIPALES"))
  
  return(list(
    data = df_pep,
    removed_col = had_pep_principales
  ))
}

# ---------------------------
# Shiny App
# ---------------------------
ui <- fluidPage(
  titlePanel("Carga de archivos PEP IDECON"),
  sidebarLayout(
    sidebarPanel(
      fileInput("nombramientos", "Sube pep_latest.xlsx", accept = ".xlsx"),
      fileInput("parientes", "Sube parientes_latest.xlsx", accept = ".xlsx"),
      actionButton("procesar", "Ejecutar"),
      downloadButton("descargar", "Descargar CSV")
    ),
    mainPanel(
      textOutput("resultado")
    )
  )
)

server <- function(input, output) {
  
  # Reactivos para guardar resultado y estado de la columna
  datos_procesados <- reactiveVal(NULL)
  col_eliminada <- reactiveVal(FALSE)
  
  observeEvent(input$procesar, {
    req(input$nombramientos, input$parientes)
    
    df_nombramientos <- read_excel(path = input$nombramientos$datapath)
    df_parientes <- read_excel(path = input$parientes$datapath)
    
    res <- rr_uploaded_pep(df_nombramientos, df_parientes)
    
    datos_procesados(res$data)
    col_eliminada(isTRUE(res$removed_col))
    
    # Mensaje en UI según si existía la columna
    output$resultado <- renderText({
      if (col_eliminada()) {
        "Ejecución correcta. Se eliminó la columna 'PEP PRINCIPALES'. Ahora puedes descargar el archivo."
      } else {
        "Ejecución correcta. La columna 'PEP PRINCIPALES' no estaba presente. Ahora puedes descargar el archivo."
      }
    })
  })
  
  # Handler de descarga
  output$descargar <- downloadHandler(
    filename = function() {
      fecha <- format(Sys.Date(), "%d-%m-%Y")
      paste0("IDECON_", fecha, ".csv")
    },
    content = function(file) {
      req(datos_procesados())
      write.table(
        x = datos_procesados(),
        file = file,
        row.names = FALSE,
        na = "",
        quote = FALSE,
        sep = "|",
        fileEncoding = "UTF-8"
      )
    }
  )
}

shinyApp(ui = ui, server = server)


