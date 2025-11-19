

# Librerías necesarias
library(shiny)
library(readxl)
library(dplyr)
library(stringr)
library(lubridate)

# Función para limpiar texto
cleanData <- function(x){
  x <- if_else(is.na(x), "", x)
  x <- str_trim(x)
  x <- str_squish(x)
  x <- str_to_upper(x)
  x <- str_remove_all(x, pattern = '[\"|;]')
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

# Función principal (ahora devuelve el dataframe procesado)
rr_uploaded_pep <- function(df_nombramientos, df_parientes){
  max_item <- max(as.integer(df_nombramientos$ITEM), na.rm = TRUE) + 1
  item <- seq(from = max_item, to = max_item + nrow(df_parientes) - 1) %>% as.character()
  
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
    select(6, 7, 8, 9, 3, 4, 5, 10, 11, 1, 2)
  
  df_nombramientos <- df_nombramientos %>% mutate(ITEM = as.character(ITEM))
  
  df_pep <- bind_rows(df_nombramientos, df_parientes)
  
  fecha <- format(Sys.Date(), "%d/%m/%Y")
  
  df_pep <- df_pep %>%
    mutate(across(everything(), cleanData)) %>%
    mutate(across(2:9, ~ str_replace_all(., "\\\\d+", ""))) %>%
    mutate(
      `TIPO DE DOCUMENTO` = if_else(`TIPO DE DOCUMENTO` == "CE", "CEX", `TIPO DE DOCUMENTO`),
      RESOLUCION = paste("Actualizacion", fecha),
      FECHA_INICIO = fecha,
      FECHA_PUBLICACION = fecha
    )
  
  return(df_pep)
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
  # Variable reactiva para guardar el resultado
  datos_procesados <- reactiveVal(NULL)
  
  observeEvent(input$procesar, {
    req(input$nombramientos, input$parientes)
    
    df_nombramientos <- read_excel(input$nombramientos$datapath)
    df_parientes <- read_excel(input$parientes$datapath)
    
    resultado <- rr_uploaded_pep(df_nombramientos, df_parientes)
    datos_procesados(resultado)
    
    output$resultado <- renderText("Ejecución correcta. Ahora puedes descargar el archivo.")
  })
  
  # Handler de descarga
  output$descargar <- downloadHandler(
    filename = function() {
      fecha <- format(Sys.Date(), "%d-%m-%Y")
      paste0("IDECON_", fecha, ".csv")
    },
    content = function(file) {
      write.table(
        x = datos_procesados(),
        file = file,
        row.names = FALSE,
        na = "",
        quote = FALSE,
        sep = "|"
      )
    }
  )
}

shinyApp(ui, server)
