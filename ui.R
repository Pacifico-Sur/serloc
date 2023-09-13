library(shiny)
library(DBI)
library(magrittr)
library(tibble)
library(sf)
library(dplyr)
library(RPostgres)

if (!require("ipa")) remotes::install_gitlab("davidmacer/ipa@develop")
source("./R/funciones.R")
source("./R/mapa_municipio_localidades.R")

### Define las variables de entrada para las listas desplegables
# Tipo de consulta
tipo_consulta <- list("Selecciona una opción de consulta" = "null",
                      "Localidades" = "id_loc", "Propiedad Social" = "id_ps")
###

# Define la interfaz de usuario
ui <- fluidPage(
  
  shinyjs::useShinyjs(),  # Set up shinyjs
  
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
          ### Inicia panel para mostrar los elementos para 'Localidades'
          conditionalPanel(
            condition = "input.id_tipo_consulta == 'id_loc'",
            # Lista desplegable para escoger el estado
            selectInput_estados(),
            # Lista desplegable para escoger el municipio
            selectInput_municipios(),
            # Lista desplegable para escoger las localidades
            selectInput_localidades(),
            # Lista desplegable para escoger el tema
            selectInput_tema(),
            # Lista desplegable de subtemas
            uiOutput(outputId = "subtemaInputUI"),
            # Año de interés
            uiOutput(outputId = "id_anio"),
            # Checkbox de indicadores
            uiOutput(outputId = "indicadorInputUI"),
            # Checkbox de variables
            uiOutput(outputId = "variablesInputUI"),
            # Checkbox de variables de desarrollo local
            uiOutput(outputId = "variablesDesLocInputUI")
            # Botón para visualizar datos
            # actionButton("id_visualizar", "Ver tabla de datos")
          ),
          
          ### Inicio panel para mostrar los elementos para municipio
          conditionalPanel(
            condition = "input.id_tipo_consulta == 'id_mun'",
            selectInput(
              inputId = 'id_mun_estado',
              label = 'Estado',
              selected = '',
              choices = list("Selecciona un estado" = 0,
                             "Chiapas" = 7,
                             "Guerrero" = 12,
                             "Oaxaca" = 20)
            ),
            selectInput(
              inputId = "id_mun_municipio",
              label = "Municipio",
              choices = NULL
            ),
            # Año de interés
            selectInput(
              inputId = 'id_mun_anio',
              label = 'Año',
              selected = '',
              choices = list("2010" = 2010,
                             "2020" = 2020)
            )
          ),
          ### Fin panel para mostrar los elementos para municipio
          
          ### Inicio panel para mostrar los elementos para propiedad social
          conditionalPanel(
            condition = "input.id_tipo_consulta == 'id_ps'",
            selectInput(
              inputId = 'id_ps_estado',
              label = 'Estado',
              selected = '',
              choices = list("Selecciona un estado" = 0,
                             "Chiapas" = 7,
                             "Guerrero" = 12,
                             "Oaxaca" = 20)
            ),
            selectInput(
              inputId = "id_ps_municipio",
              label = "Municipio",
              choices = NULL
            ),
            selectInput(
              inputId = "id_ejido",
              label = "Ejido o comunidad",
              choices = NULL
            ),
            # Año de interés
            selectInput(
              inputId = 'id_ps_anio',
              label = 'Año',
              selected = '',
              choices = list("2010" = 2010,
                             "2020" = 2020)
            ),
            # Botón para ver la infografía
            downloadButton(
              "id_ps_infografia", "Ver ficha infográfica del ejido o comunidad"
            )
          ),
          ### Fin panel para mostrar los elementos para propiedad social
          
          ### Inicia panel para mostrar los elementos de propiedad privada
          conditionalPanel(
            condition = "input.id_tipo_consulta == 'id_pp'",
            selectInput(
              inputId = 'id_pp_estado',
              label = 'Estado',
              selected = '',
              choices = list("Selecciona un estado" = 0,
                             "Chiapas" = 7,
                             "Guerrero" = 12,
                             "Oaxaca" = 20)
            ),
            selectInput(
              inputId = "id_pp_municipio",
              label = "Municipio",
              choices = NULL
            )
          )
          ### Fin panel para mostrar los elementos de propiedad privada
        ),
        
        # Muestra la tabla de datos o la infografía
        mainPanel(
          tableOutput(outputId = "data_table"),
          
          # Botón para descargar los datos
          downloadButton(
            outputId = "id_descargar",
            label = "Descargar datos"
          ),
          
          
          
          # Botón para descargar el descriptor de datos
          downloadButton(
            outputId = "descargarDescriptor",
            label = "Descargar descriptor"
          ),
          
          # Botón para descargar el descriptor de datos
          downloadButton(
            outputId = "downloadPlot",
            label = "Descargar mapa del municipio"
          ),
          
          htmlOutput("filetable1"),
          htmlOutput("filetable2"),
          htmlOutput("filetable3")
        )
    )
)
