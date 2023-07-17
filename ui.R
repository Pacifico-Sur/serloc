library(shiny)
library(DBI)
library(magrittr)
library(tibble)
library(sf)
library(dplyr)
library(RPostgres)

if (!require("ipa")) remotes::install_gitlab("davidmacer/ipa@develop")

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
            uiOutput(outputId = "id_estado")
          ),
          # El contenido de la lista de municipios se llena según el estado seleccionado
          div(
            uiOutput(outputId = "id_municipio")
          ),
          # El contenido de la lista de localidades se llena según el municipio seleccionado
          div(
            uiOutput(outputId = "id_localidades")
          ),
          # Temas de interés
          div(
            selectInput(inputId = "id_tema",
                        label = "Tema",
                        choices = NULL)
          ),
          # Año de interés
          uiOutput(outputId = "id_anio"),
          # Subtema
          uiOutput(outputId = "subtemaInputUI"),
          # Indicadores
          uiOutput(outputId = "indicadorInputUI"),
          # Botón para visualizar datos
          actionButton("id_visualizar", "Ver tabla de datos")
          ),
        
        # Muestra la tabla de datos o la infografía
        mainPanel(
          tableOutput(outputId = "data_table"),
          
          # Botón para descargar los datos
          downloadButton(outputId = "id_descargar",
                         label = "Descargar datos")
          )
    )
)
