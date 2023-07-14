library(shiny)
library(DBI)
library(magrittr)
library(tibble)
library(sf)
library(dplyr)
library(RPostgres)

if (!require("ipa")) remotes::install_gitlab("davidmacer/ipa@develop")

# Conecta con la base de datos del servicio de localidades
conexion <- ipa::db_connect(dbname = "siclr_db",
                            host = "database")

### Define las variables de entrada para las listas desplegables
# Tipo de consulta
tipo_consulta <- list("Selecciona una opción de consulta" = "null",
                      "Localidades" = "id_loc",
                      "Propiedad social" = "id_ps",
                      "Propiedad privada" = "id_pp",
                      "Municipio" = "id_mun")
# Estados de Pacífico Sur
estados <- list("Selecciona un estado" = 0,
                "Chiapas" = 7,
                "Guerrero" = 12,
                "Oaxaca" = 20)
# Temas que el usuario puede escoger
lista_temas <- list("Carencia de bienes y medios de comunicación" = 2,
                    "Exclusión indígena" = 3,
                    "Incidencia de analfabetismo" = 7,
                    "Presencia de piso de tierra, carencia de agua y saneamiento" = 5,
                    "Presencia de población económicamente inactiva" = 9,
                    "Presencia de población en edad avanzada y población con discapacidad" = 8,
                    "Presencia de población infantil y en edad dependiente" = 6,
                    "Rezago educativo, carencia de auto e internet" = 4,
                    "Aspectos cualitativos de vulnerabilidad" = 10)

###
# Define la interfaz de usuario
ui <- fluidPage(

    # Application title
    titlePanel("Servicio de Información y Conocimiento de 
               Localidades Rurales y sus Territorios"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(# El usuario escoge qué tipo de consulta quiere hacer
          div(
            selectInput(inputId = "id_tipo_consulta",
                        label = "Tipo de consulta",
                        choices = tipo_consulta,
                        selected = NULL)
          ),
          # Debe escoger el estado de interés
          div(
            selectInput(inputId = "id_estado",
                        label = "Estado",
                        choices = estados)
          ),
          # El contenido de la lista de municipios se llena según el estado seleccionado
          div(
            selectInput(inputId = "id_municipio",
                        label = "Municipio",
                        choices = NULL)
          ),
          # El contenido de la lista de localidades se llena según el municipio seleccionado
          div(
            selectInput(inputId = "id_localidades",
                        label = "Localidad",
                        choices = NULL)
          ),
          # Temas de interés
          div(
            selectInput(inputId = "id_tema",
                        label = "Tema",
                        choices = lista_temas)
          ),
          # Año de interés
          uiOutput(outputId = "id_anio"),
          # Subtema
          uiOutput(outputId = "subtemaInputUI"),
          # Indicadores
          uiOutput(outputId = "indicadorInputUI")
          ),
        
        # Show a plot of the generated distribution
        mainPanel()
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  ### Evento para llenar la lista de municipios según el estado seleccionado
  observe({
    id_estado <- input$id_estado
    
    # Extrae de la BD los municipios del estado seleccionado
    query_municipios <- paste("SELECT * FROM edo_mun.municipios WHERE ",
                              "\"ID_ENT\"", " = ", id_estado, ";")
    municipios <- ipa::db_get_table(conn = conexion,
                                    statement = query_municipios)
    municipios <- municipios |>
      as_tibble() |>
      select(id_mun, nomgeo) |>
      arrange(nomgeo)
    
    municipios <- setNames(as.list(municipios$id_mun), municipios$nomgeo)
    primer_elemento <- list("Selecciona un municipio" = 0)
    municipios <- primer_elemento |>
      append(municipios)
    
    # Can also set the label and select items
    updateSelectInput(session, "id_municipio",
                      label = "Municipios",
                      choices = municipios)
  })
  ###
  
  ### Evento para llenar la lista de localidades según el municipio seleccionado
  observe({
    req(input$id_municipio)
    id_mun <- input$id_municipio
    
    # Extrae de la BD los municipios del estado seleccionado
    query_localidades <- paste("SELECT * FROM loc.localidades WHERE ",
                               "\"ID_MUN\"", " = ", id_mun, ";")
    localidades <- ipa::db_get_table(conn = conexion,
                                     statement = query_localidades)
    localidades <- localidades |>
      as_tibble() |>
      select(id_loc, nom_loc) |>
      arrange(nom_loc)
    
    localidades <- setNames(as.list(localidades$id_loc), localidades$nom_loc)
    primer_elemento <- list("Localidad" = 0)
    localidades <- primer_elemento |>
      append(localidades)
    
    # Can also set the label and select items
    updateSelectInput(session, "id_localidades",
                      label = "Localidades",
                      choices = localidades)
  })
  ###
  
  ### Muestra la lista de subtemas cuando selecciona 
  # Aspectos cualitativos de vulnerabilidad y esconde el elemento 
  # para escoger año
  output$subtemaInputUI <- renderUI({
    if (input$id_tema == 10) {
      query_subtemas <- "SELECT * FROM catalogo.des_soc_subtema;"
      lista_subtemas <- ipa::db_get_table(conn = conexion,
                                          statement = query_subtemas)
      selectInput(inputId = "id_subtema",
                  label = "Subtemas",
                  choices = lista_subtemas$subtema)
    }
  })
  
  # Muestra la casilla año si el tema es distinto a Aspectos cualitativos de vul.
  # Este tema solo tiene datos para el 2020, por lo que no tiene caso mostrar la casilla
  observeEvent(input$id_tema, ignoreNULL = FALSE, {
    if (input$id_tema == 10){
      output$id_anio <- renderUI(NULL)
    } else {
      output$id_anio <- renderUI(
        selectInput(
          inputId = 'id_anio',
          label = 'Año',
          selected = '',
          choices = list("2010" = 2010, "2020" = 2020)
        )
      )
    }
  })
  ###
  
  ### Inicio evento para selecciones los indicadores de índice y nivel
  output$indicadorInputUI <- renderUI({
    req(input$id_tema, input$id_anio)
    
    id_tema <- input$id_tema
    anio <- input$id_anio
    query_indicadores <- paste0("SELECT * FROM catalogo.indicadores
                          WHERE cve_sub = (SELECT cve_sub FROM catalogo.subtema
                                          WHERE cve_tem = ", id_tema, " and 
                                          anio = ", anio, ");")
      tbl_indicadores <- ipa::db_get_table(conn = conexion,
                                          statement = query_indicadores)
      checkboxGroupInput(inputId = "id_indicadores",
                         label = "Indicadores",
                         choices = tbl_indicadores$indicadores,
                         selected = 1)
  })
  ###
  
}

# Run the application 
shinyApp(ui = ui, server = server)
