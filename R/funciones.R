selectInput_estados <- function() {
  selectInput(
    inputId = 'id_estado',
    label = 'Estado',
    selected = '',
    choices = list("Selecciona un estado" = 0,
                   "Chiapas" = 7,
                   "Guerrero" = 12,
                   "Oaxaca" = 20)
  )
}

selectInput_municipios <- function() {
  selectInput(
    inputId = "id_municipio",
    label = "Municipio",
    choices = NULL
  )
}

selectInput_localidades <- function() {
  selectInput(
    inputId = "id_localidades",
    label = "Localidad",
    choices = '',
    multiple = TRUE
  )
}

selectInput_tema <- function() {
  selectInput(
    inputId = "id_tema",
    label = "Tema",
    choices = NULL
  )
}

selectInput_anio <- function() {
  selectInput(
    inputId = 'id_anio',
    label = 'Año',
    selected = '',
    choices = list("2010" = 2010,
                   "2020" = 2020)
  )
}