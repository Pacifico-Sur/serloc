# Conecta con la base de datos del servicio de localidades
conexion <- ipa::db_connect(dbname = "siclr_db",
                            host = "database")

# Define la lógica del servidor
server <- function(input, output, session) {
  
  ### Inicia evento para llenar la lista de municipios según el estado seleccionado
  ### en el tipo de consulta 'Localidad'
  observeEvent(input$id_estado, {
    
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
  ### Fin
  
  ### Inicia evento para llenar la lista de municipios según el estado seleccionado
  ### en el tipo de consulta 'Propiedad social'
  observeEvent(input$id_ps_estado, {
    
    # Extrae de la BD los municipios del estado seleccionado
    query_municipios <- paste("SELECT * FROM edo_mun.municipios WHERE ",
                              "\"ID_ENT\"", " = ", input$id_ps_estado, ";")
    
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
    updateSelectInput(session, "id_ps_municipio",
                      label = "Municipios",
                      choices = municipios)
    
  })
  ###
  ### Fin
  
  ### Inicia evento para llenar la lista de municipios según el estado seleccionado
  ### en el tipo de consulta 'Municipio'
  observeEvent(input$id_mun_estado, {
    
    # Extrae de la BD los municipios del estado seleccionado
    query_municipios <- paste("SELECT * FROM edo_mun.municipios WHERE ",
                              "\"ID_ENT\"", " = ", input$id_mun_estado, ";")
    
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
    updateSelectInput(session, "id_mun_municipio",
                      label = "Municipios",
                      choices = municipios)
    
  })
  ### Fin
  
  ### Inicia evento para llenar la lista de municipios según el estado seleccionado
  ### en el tipo de consulta 'Propiedad privada'
  observeEvent(input$id_pp_estado, {
    
    # Extrae de la BD los municipios del estado seleccionado
    query_municipios <- paste("SELECT * FROM edo_mun.municipios WHERE ",
                              "\"ID_ENT\"", " = ", input$id_pp_estado, ";")
    
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
    updateSelectInput(session, "id_pp_municipio",
                      label = "Municipios",
                      choices = municipios)
    
  })
  ### Fin
  
  ### Inicio evento para llenar la lista de localidades según el municipio seleccionado
  observe({
    req(input$id_municipio)
    
    # Extrae de la BD los municipios del estado seleccionado
    query_localidades <- paste("SELECT * FROM loc.localidades WHERE ",
                               "\"ID_MUN\"", " = ", input$id_municipio, ";")
    localidades <- ipa::db_get_table(conn = conexion,
                                     statement = query_localidades)
    localidades <- localidades |>
      as_tibble() |>
      select(id_loc, nom_loc) |>
      arrange(nom_loc)
    
    localidades <- setNames(as.list(localidades$id_loc), localidades$nom_loc)
    # primer_elemento <- list("Localidad" = 0)
    # localidades <- primer_elemento |>
    #   append(localidades)
    
    # Actualiza selectInput de id_localidades
    updateSelectInput(session, "id_localidades",
                      label = "Localidades",
                      choices = localidades,
                      selected = NULL)
  })
  ### Fin evento para llenar la lista de localidades según el municipio seleccionado
  
  ### Inicia evento para llenar lista de selección de temas
  observe({
    
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
    
    # Actualiza selectInput de id_tema
    updateSelectInput(session, "id_tema",
                      label = "Tema",
                      choices = lista_temas)
  })
  ### Fin evento para llenar lista de selección de temas
  
  ### Muestra la lista de subtemas cuando selecciona 
  # Aspectos cualitativos de vulnerabilidad y esconde el elemento 
  # para escoger año
  observeEvent(input$id_tema, {
      if (input$id_tema == 10) {
        output$subtemaInputUI <- renderUI({
          query_subtemas <- "SELECT * FROM catalogo.des_soc_subtema;"
          lista_subtemas <- ipa::db_get_table(conn = conexion,
                                              statement = query_subtemas)
          lista_subtemas <- lista_subtemas |>
            as_tibble() |>
            select(id, subtema) |>
            arrange(subtema)
          
          lista_subtemas <- setNames(
            as.list(lista_subtemas$id), lista_subtemas$subtema)
          
          selectInput(inputId = "id_subtema",
                      label = "Subtemas",
                      choices = lista_subtemas)
        })
        
        output$id_anio <- renderUI(NULL)
        
      } else {
        output$subtemaInputUI <- renderUI(NULL)
        
        output$id_anio <- renderUI(
          selectInput_anio()
        )
      }
  })
  
  ### Inicia evento para extraer la tabla descriptora de indicadores
  df_indicadores <- reactive({
    # Requiere el tema y el año para hacer la consulta
    req(input$id_tema, input$id_anio)
    
    if (input$id_tema != 10) {
      query_indicadores <- paste0(
        "SELECT *
        FROM catalogo.indicadores
        WHERE cve_sub = (SELECT cve_sub
                        FROM catalogo.subtema
                        WHERE cve_tem = ", input$id_tema, " AND
                        anio = ", input$id_anio, ");")
    } else {
      query_indicadores <- paste0(
      "SELECT *
      FROM catalogo.des_soc_indicadores
      WHERE cve_sub = 
        (SELECT cve_sub
        FROM catalogo.des_soc_subtema
        WHERE id = ", input$id_subtema, ");")
    }

    # Extrae la tabla de indicadores para el tema y el año. El año lo ocupo para
    # poder extraer las claves de los indicadores de ese año
    tbl_indicadores <- ipa::db_get_table(conn = conexion,
                                         statement = query_indicadores)

  })
  
  ### Inicia evento para crear checkboxgroup de los indicadores de índice y nivel
  lista_indice_nivel <- reactive({
    es_indice_nivel <- df_indicadores() |>
      pull(cve_ind) |>
      stringr::str_ends(c("01|2"))
    
    lista_indice_nivel <- df_indicadores()[es_indice_nivel, ]
    lista_indice_nivel <- setNames(
      as.list(lista_indice_nivel$id), lista_indice_nivel$indicadores)
  })
  
  output$indicadorInputUI <- renderUI({
    
    checkboxGroupInput(inputId = "id_indicadores", 
                       label = "Indicadores",
                       choices = lista_indice_nivel())
  })
  ### Fin evento para crear checkboxgroup de los indicadores de índice y nivel
  
  observeEvent(input$id_indicadores, {
    if (input$id_indicadores |> isTruthy()) {
      shinyjs::hide(id = "id_variables")
    } else {
      shinyjs::show(id = "id_variables")
    }
  }, ignoreNULL = FALSE)
  
  observeEvent(input$id_variables, {
    if (input$id_variables |> isTruthy()) {
      shinyjs::hide(id = "id_indicadores")
    } else {
      shinyjs::show(id = "id_indicadores")
    }
  }, ignoreNULL = FALSE)
  
  ### Inicia evento para crear checkboxgroup de las variables
  lista_variables <- reactive({
    es_indice_nivel <- df_indicadores() |>
      pull(cve_ind) |>
      stringr::str_ends(c("01|2"))
    
    lista_indice_nivel <- df_indicadores()[!es_indice_nivel, ]
    lista_indice_nivel <- setNames(
      as.list(lista_indice_nivel$id), lista_indice_nivel$indicadores)
  })
  
  output$variablesInputUI <- renderUI({
    
    checkboxGroupInput(inputId = "id_variables", 
                       label = "Variables",
                       choices = lista_variables(),
                       selected = 1)
  })
  ### Inicia evento para crear checkboxgroup de las variables
  
  clave_indicador <- reactive({
    if (input$id_indicadores |> isTruthy()) {
      df_indicadores() |>
        filter(id %in% input$id_indicadores) |>
        pull(cve_ind) |>
        toupper()
    } else if (input$id_variables |> isTruthy()) {
      df_indicadores() |>
        filter(id %in% input$id_variables) |>
        pull(cve_ind) |>
        toupper()
    }
  })
  
  
  
  
  ### Inicio botón de visualización de datos
  df_localidades_indicadores <- reactive({
    # req(input$id_localidades, input$id_visualizar, input$id_tema, input$id_anio)
    req(clave_indicador(), input$id_localidades)
    # Extraigo la clave del indicador (cve_ind) de df_indicadores según el
    # id del indicador seleccionado
    
    
    # Separa las claves de indicadores con comas y las pone entre comillas para 
    # usarlas de manera fácil en el query
    clave_indicadores <- paste("a.", dQuote(clave_indicador(), FALSE), collapse=",")
    
    # Extrae los indicadores según el estado, municipio, localidad, tema, 
    # subtema (se es el caso), año e indicadores seleccionados
    localidades_seleccionadas <- paste(input$id_localidades, collapse = ",")
    
    # Si el tema es distinto de Aspectos cualitativos de vulnerabilidad accede 
    # a la tabla de vulnerabilidad de localidades rurales
    if (input$id_tema != 10) {
      # Selecciona la tabla de datos de interés según el año seleccionado
      tabla_localidades_bd <- paste0("ivp.loc_rur_", input$id_anio)
      
      query_indicadores <- paste0(
        "select b.\"NOM_LOC\" as \"Localidad\",", clave_indicadores, 
        " FROM ", tabla_localidades_bd, " as a ",
        "INNER JOIN loc.localidades as b ",
          "ON a.\"CGLOC\" = b.\"CGLOC\" AND ",
          "b.\"ID_LOC\" in (", localidades_seleccionadas, ");")
    } else {
      tabla_localidades_bd <- "ivp.des_local_2020"
      
      query_indicadores <- paste0(
        "select b.\"NOM_LOC\" as \"Localidad\",", clave_indicadores, 
        " FROM ", tabla_localidades_bd, " as a ",
        "INNER JOIN loc.localidades as b ",
        "ON a.\"CGLOC\" = b.\"CGLOC\" AND ",
        "b.\"ID_LOC\" in (", localidades_seleccionadas, ");")
    }
    
    indi <- ipa::db_get_table(conn = conexion,
                              statement = query_indicadores)
  })
  
  
  
  ### Inicia evento para mostrar el checkboxgroup del índice y nivel de los temás
  observeEvent(df_localidades_indicadores(), {
    
    # Extrae los nombres de los indicadores usando la clave del indicador
    indicadores_nombre_largo <- df_indicadores() |>
      filter(cve_ind %in% clave_indicador()) |> pull(indicadores)
    
    # Crea un vector para cambiar los nombres de los atributos de la tabla 
    # que se muestra al usuario en la aplicación
    tabla_para_mostrar <- df_localidades_indicadores()
    indicadores_nombre_largo <- c("Localidad", indicadores_nombre_largo)
    colnames(tabla_para_mostrar) <- indicadores_nombre_largo
    
    # Renderiza la tabla para mostrar al usuario
    output$data_table <- renderTable(tabla_para_mostrar)
  })
  ### Fin botón de visualización de datos
  
  
  es_mostrar_tabla_botones <- reactive({
    (input$id_indicadores |> isTruthy() | 
                                 input$id_variables |> isTruthy()) & 
    input$id_localidades |> isTruthy()
  })
  
  observeEvent(es_mostrar_tabla_botones(), {
    shinyjs::hide("id_descargar")
    shinyjs::hide("downloadPlot")
    
    if(es_mostrar_tabla_botones()) {
      shinyjs::show("id_descargar")
      shinyjs::show("downloadPlot")
    }
  })
  
  observe({
    shinyjs::hide("data_table")

    if(es_mostrar_tabla_botones()) {
      shinyjs::show("data_table")
    }
  })
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
  
  ### Inicia proceso de descarga de descriptor de datos
  output$descargarDescriptor <- downloadHandler(
    # Nombre del archivo que se va a guardar
    filename = "descriptor_datos.pdf",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("./docs/report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      # params <- list(n = input$slider)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        # params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  ### Fin proceso de descarga de descriptor de datos
  
  ### Inicia proceso de creación y descarga de mapa de municipio y localidades
  my_plot <- reactive({
    mapa_municipio_localidades(id_mun = input$id_municipio)
  })
  
  output$downloadPlot <- downloadHandler(
    filename = function() { "output.pdf" },
    content = function(file) {
      pdf(file, paper = "default")
      plot(my_plot())
      dev.off()
    }
  )
  ### Fin proceso de creación y descarga de mapa de municipio y localidades
  
}
