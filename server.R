# Conecta con la base de datos del servicio de localidades
conexion <- ipa::db_connect(dbname = "siclr_db",
                            host = "database")

# Define la lógica del servidor
server <- function(input, output, session) {
  
  observeEvent(input$id_tipo_consulta, ignoreNULL = FALSE, {
    if (input$id_tipo_consulta == "id_loc") {
      ### Inicio evento para mostrar la selección del estado
      output$id_estado <- renderUI(
        selectInput(
          inputId = 'id_estado',
          label = 'Estado',
          selected = '',
          choices = list("Selecciona un estado" = 0,
                         "Chiapas" = 7,
                         "Guerrero" = 12,
                         "Oaxaca" = 20)
        )
      )
      ### Fin evento para mostrar la selección del estado
      
      ### Inicio evento para mostrar la casilla de selección de municipio
      output$id_municipio <- renderUI(
        selectInput(
          inputId = "id_municipio",
          label = "Municipio",
          choices = NULL
        )
      )
      ### Fin evento para mostrar la casilla de selección de municipio
    }
  })
  ### Evento para llenar la lista de municipios según el estado seleccionado
  observe({
    req(input$id_estado)
    
    # Extrae de la BD los municipios del estado seleccionado
    query_municipios <- paste("SELECT * FROM edo_mun.municipios WHERE ",
                              "\"ID_ENT\"", " = ", input$id_estado, ";")
    
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
  
  ### Inicio evento para llenar la lista de localidades según el municipio seleccionado
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
    
    # Actualiza selectInput de id_localidades
    updateSelectInput(session, "id_localidades",
                      label = "Localidades",
                      choices = localidades)
  })
  ### Fin evento para llenar la lista de localidades según el municipio seleccionado
  observe({
    req(input$id_localidades)
    
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
    
    # Actualiza selectInput de id_localidades
    updateSelectInput(session, "id_tema",
                      label = "Tema",
                      choices = lista_temas)
  })
  ### Inicio evento para mostrar el selectInput de tema
  
  ### Fin evento para mostrar el selectInput de tema
  
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
  
  ### Inicio evento para seleccionar los indicadores de índice y nivel
  df_indicadores <- reactive({
    # Requiere el tema y el año para hacer la consulta
    req(input$id_tema, input$id_anio)
    
    query_indicadores <- paste0("SELECT * FROM catalogo.indicadores
                          WHERE cve_sub = (SELECT cve_sub FROM catalogo.subtema
                                          WHERE cve_tem = ", input$id_tema, " and 
                                          anio = ", input$id_anio, ");")
    
    # Extrae la tabla de indicadores para el tema y el año. El año lo ocupo para
    # poder extraer las claves de los indicadores de ese año
    tbl_indicadores <- ipa::db_get_table(conn = conexion,
                                         statement = query_indicadores)
    
  })
  
  output$indicadorInputUI <- renderUI({
    req(df_indicadores())
    
    # Extraigo la clave del subtema y el nombre del indicador para que sea 
    # llave-valor en el checkgroup
    lista_indicadores <- df_indicadores() |>
      select(id, indicadores)
    lista_indicadores <- setNames(
      as.list(lista_indicadores$id), lista_indicadores$indicadores)
    
    
    checkboxGroupInput(inputId = "id_indicadores", 
                       label = "Indicadores",
                       choices = lista_indicadores,
                       selected = 1)
    
  })
  ###
  
  indicadores_seleccionados <- reactive({
    req(input$id_indicadores)
    df_indicadores <- input$id_indicadores
  })
  
  ### Inicio botón de visualización de datos
  df_localidades_indicadores <- reactive({
    req(input$id_localidades, input$id_visualizar, input$id_tema, input$id_anio)
    
    id_tema <- input$id_tema
    anio <- input$id_anio
    
    # Extraigo la clave del indicador (cve_ind) de df_indicadores según el
    # id del indicador seleccionado
    clave_indicador <- df_indicadores() |>
      filter(id %in% indicadores_seleccionados()) |>
      pull(cve_ind) |>
      toupper()
    
    # Separa las claves de indicadores con comas y las pone entre comillas para 
    # usarlas de manera fácil en el query
    clave_indicadores <- paste(dQuote(clave_indicador, FALSE), collapse=",")
    
    # Selecciona la tabla de datos de interés según el año seleccionado
    tabla_localidades_bd <- paste0("loc_rur_", input$id_anio)
    
    # Extrae los indicadores según el estado, municipio, localidad, tema, 
    # subtema (se es el caso), año e indicadores seleccionados
    localidades_seleccionadas <- paste(input$id_localidades, collapse=",")
    query_indicadores <- paste0("select b.\"NOM_LOC\" as \"Localidad\", a.\"FCB_0201\", a.\"FCB_0201\" ",
                                "FROM ivp.loc_rur_2010 as a ",
                                "INNER JOIN loc.localidades as b ",
                                "ON a.\"CGLOC\" = b.\"CGLOC\" AND ",
                                "b.\"ID_LOC\" in (", localidades_seleccionadas, ");")
    indi <- ipa::db_get_table(conn = conexion,
                              statement = query_indicadores)
  })
  
  observe({
    req(df_localidades_indicadores())
    output$data_table <- renderTable(df_localidades_indicadores())
  })
  ### Fin botón de visualización de datos
  
  ### Inicio proceso de descarga de datos
  output$id_descargar <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(df_localidades_indicadores(),
                file,
                row.names = FALSE)
    }
  )
  ### Fin proceso de descarga de datos
}