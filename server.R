if (!require("ipa")) install.packages("remotes::install_gitlab('davidmacer/ipa@develop')")
source("./R/mapa_municipio_localidades.R")

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
  
  ### Inicia módulo para Propiedad Social
  ### Inicia evento para llenar la lista de municipios según el estado seleccionado
  nombre_estado <- reactive({
    req(input$id_ps_estado)
    
    estados <- ipa::db_get_estado(conn = conexion)
    nombre_estado <- estados |>
      filter(id_ent == input$id_ps_estado) |>
      select(nomgeo)
  })
  
  municipios_df <- reactive({
    req(input$id_ps_estado)
    
    # Extrae de la BD los municipios del estado seleccionado
    query_municipios <- paste("SELECT * FROM edo_mun.municipios WHERE ",
                              "\"ID_ENT\"", " = ", input$id_ps_estado, ";")
    
    municipios <- ipa::db_get_table(conn = conexion,
                                    statement = query_municipios)
    
    municipios <- municipios |>
      as_tibble() |>
      select(id_mun, nomgeo) |>
      arrange(nomgeo)
  })
  
  observeEvent(municipios_df(), {
    
    municipios_list <- setNames(as.list(municipios_df()$id_mun),
                                municipios_df()$nomgeo)
    primer_elemento <- list("Selecciona un municipio" = 0)
    
    municipios_list <- primer_elemento |>
      append(municipios_list)
    
    # Can also set the label and select items
    updateSelectInput(session, "id_ps_municipio",
                      label = "Municipios",
                      choices = municipios_list)
    
  })
  ###
  ### Finaliza evento para llenar la lista de municipios según el estado seleccionado
  
  ### Inicio evento para llenar la lista de núcleos agrarios según el municipio seleccionado
  nucleos_agrarios <- reactive({
    req(input$id_ps_municipio)
    
    # Extrae de la BD los municipios del estado seleccionado
    query_na <- paste("SELECT * FROM public.na WHERE ",
                      "\"ID_MUN\"", " = ", input$id_ps_municipio, ";")
    nucleo_agrario <- ipa::db_get_table(conn = conexion,
                                        statement = query_na)
    nucleo_agrario <- nucleo_agrario |>
      as_tibble() |>
      select(id_na, nom_nucleo, tipo) |>
      arrange(nom_nucleo)
    })
  
  observe({
    req(nucleos_agrarios())
    
    nucleos_agrarios_list <- setNames(as.list(as.numeric(nucleos_agrarios()$id_na)),
                               paste(nucleos_agrarios()$nom_nucleo, nucleos_agrarios()$tipo))
    updateSelectInput(session, "id_ejido",
                      label = "Ejido o comunidad",
                      choices = nucleos_agrarios_list,
                      selected = NULL)
  })
  ### Finaliza evento para llenar la lista de núcleos agrarios según el municipio seleccionado
  
  ### Inicia evento para la consulta a la BD de los indicadores de propiedad social
  # Crea el query para realizar el llamado a la BD
  query_propiedad_social <- reactive({
    # Requiere el tema y el año para hacer la consulta
    req(input$id_ejido)
    
    query <- paste0(
    "SELECT *
      FROM ivp.na_", input$id_ps_anio,
      " WHERE \"ID_NA\" = ", input$id_ejido, ";")
  })
  
  # Haz el llamado a la BD para obtener los indicadores de propiedad social
  propiedad_social_indicadores <- reactive({
    df <- ipa::db_get_table(conn = conexion,
                            statement = query_propiedad_social())
  })
  
  query_prop_social_catalogo <- reactive({
    # Extrae el catálogo de indicadores para relacionar las claves con la descripción
    query <- paste0(
      "SELECT *
      FROM catalogo.indicadores;")
  })
  
  # Haz el llamado a la BD para obtener la descripción de los indicadores
  propiedad_social_catalogo <- reactive({
    df <- ipa::db_get_table(conn = conexion,
                            statement = query_prop_social_catalogo())
  })
  ### Finaliza evento para la consulta a la BD de los indicadores de propiedad social
  
  observeEvent(input$renderInfoPs, {
    output$renderedInfoPs <- renderUI({
      nucleo_agrario <- nucleos_agrarios() |>
        filter(id_na == input$id_ejido) # Extrae el nombre del núcleo agrario seleccionado por el usuario
      nombre_municipio <- municipios_df() |>
        filter(id_mun == input$id_ps_municipio) |>
        select(nomgeo)
      
      # Set up parameters to pass to Rmd document
      parametros <- list(nucleo_agrario = nucleo_agrario,
                         nombre_municipio = nombre_municipio,
                         nombre_estado = nombre_estado(),
                         df_propiedad_social = propiedad_social_indicadores(),
                         df_ps_catalogo = propiedad_social_catalogo())
      
      includeMarkdown(rmarkdown::render(
        "./docs/infografia_ps.Rmd",
        params = parametros)
        )
    })
  })
  
  # Inicia botones para descarga de productos en tema de Propiedad Social
  es_mostrar_botones_ps <- reactive({
    (input$renderInfoPs |> isTruthy() |
       input$id_mun |> isTruthy())
  })
  
  observeEvent(es_mostrar_botones_ps(), {
    shinyjs::hide("descargar_infografia_ps")
    shinyjs::hide("descargar_mapa_na_contorno")
    
    if(es_mostrar_botones_ps()) {
      shinyjs::show("descargar_infografia_ps")
      shinyjs::show("descargar_mapa_na_contorno")
    }
  })
  # Fin botones para descarga de productos en tema de Localidades
  ### Finaliza módulo para Propiedad Social
  
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
  lista_subtemas <- reactive({
      if (input$id_tema == 10) {
        query_subtemas <- "SELECT * FROM catalogo.des_soc_subtema;"
        lista_subtemas <- ipa::db_get_table(conn = conexion,
                                            statement = query_subtemas)
        
        lista_subtemas <- lista_subtemas |>
          as_tibble() |>
          select(id, subtema) |>
          arrange(subtema)
        
        lista_subtemas <- setNames(
          as.list(lista_subtemas$id), lista_subtemas$subtema)
        
      }
  })
  
  observeEvent(lista_subtemas(), {
    output$subtemaInputUI <- renderUI({
      if (lista_subtemas() |> isTruthy()) {
        selectInput(inputId = "id_subtema",
                    label = "Subtemas",
                    choices = lista_subtemas())
      } else {
        output$subtemaInputUI <- renderUI(NULL)
      }
    })
  })
  
  observeEvent(input$id_tema, {
    if (input$id_tema == 10) {
      output$id_anio <- renderUI(NULL)
    } else {
      output$id_anio <- renderUI(
        selectInput_anio()
      )
    }
  })
  
  ### Inicia evento para extraer la tabla descriptora de indicadores
  query_indicadores <- reactive({
    # Requiere el tema y el año para hacer la consulta
    req(input$id_tema, input$id_anio)
    
    if (input$id_tema != 10) {
      query <- paste0(
        "SELECT *
        FROM catalogo.indicadores
        WHERE cve_sub = (SELECT cve_sub
                        FROM catalogo.subtema
                        WHERE cve_tem = ", input$id_tema, " AND
                        anio = ", input$id_anio, ");")
    }
  })
  
  query_des_loc_variables <- reactive({
    req(input$id_subtema)
    query <- paste0(
      "SELECT *
      FROM catalogo.des_soc_indicadores
      WHERE cve_sub =
        (SELECT cve_sub
        FROM catalogo.des_soc_subtema
        WHERE id = ", input$id_subtema, ");")
  })
  
  df_indicadores_variables <- reactive({
    req(input$id_tema)
    # Extrae la tabla de indicadores para el tema y el año. El año lo ocupo para
    # poder extraer las claves de los indicadores de ese año
    if (query_indicadores() |> isTruthy()) {
      tbl_indicadores <- ipa::db_get_table(conn = conexion,
                                           statement = query_indicadores())
    } else if (query_des_loc_variables() |> isTruthy()) {
      tbl_indicadores <- ipa::db_get_table(conn = conexion,
                                           statement = query_des_loc_variables())
    }
    
  })
  
  ### Inicia evento para crear checkboxgroup de los indicadores de índice y nivel
  lista_indice_nivel <- reactive({
    es_indice_nivel <- df_indicadores_variables() |>
      pull(cve_ind) |>
      stringr::str_ends(c("01|2"))
    
    lista_indicadores <- df_indicadores_variables()[es_indice_nivel, ]
    lista_indicadores <- setNames(
      as.list(lista_indicadores$id), lista_indicadores$indicadores)
  })
  
  ### Inicia evento para crear checkboxgroup de las variables
  lista_variables <- reactive({
    es_indice_nivel <- df_indicadores_variables() |>
      pull(cve_ind) |>
      stringr::str_ends(c("01|2"))
    
    lista_indicadores <- df_indicadores_variables()[!es_indice_nivel, ]
    lista_indicadores <- setNames(
      as.list(lista_indicadores$id), lista_indicadores$indicadores)
  })
  
  ### Inicia evento para crear checkboxgroup de las variables del tema de 
  # Desarrollo Social
  lista_des_soc_variables <- reactive({
    lista_indicadores <- setNames(
      as.list(df_indicadores_variables()$id), df_indicadores_variables()$indicadores)
  })
  
  observeEvent(input$id_tema, {
    if (input$id_tema == 10) {
      output$variablesDesLocInputUI <- renderUI({
        
        checkboxGroupInput(inputId = "id_des_loc_variables", 
                           label = "Variables",
                           choices = lista_des_soc_variables())
      })
      
      output$indicadorInputUI <- renderUI(NULL)
      
      output$variablesInputUI <- renderUI(NULL)
      
    } else {
      output$indicadorInputUI <- renderUI({
        
        checkboxGroupInput(inputId = "id_indicadores", 
                           label = "Indicadores",
                           choices = lista_indice_nivel())
      })
      ### Fin evento para crear checkboxgroup de los indicadores de índice y nivel
      
      output$variablesInputUI <- renderUI({
        
        checkboxGroupInput(inputId = "id_variables", 
                           label = "Variables",
                           choices = lista_variables(),
                           selected = 1)
      })
      ### Inicia evento para crear checkboxgroup de las variables
    }
  })
  
  
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
  
  clave_indicador <- reactive({
    if (input$id_indicadores |> isTruthy()) {
      df_indicadores_variables() |>
        filter(id %in% input$id_indicadores) |>
        pull(cve_ind) |>
        toupper()
    } else if (input$id_variables |> isTruthy()) {
      df_indicadores_variables() |>
        filter(id %in% input$id_variables) |>
        pull(cve_ind) |>
        toupper()
    } else if (input$id_des_loc_variables |> isTruthy()) {
      df_indicadores_variables() |>
        filter(id %in% input$id_des_loc_variables) |>
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
        "SELECT 
          c.\"NOMGEO\" as estado,
          d.\"NOMGEO\" as municipio,
          b.\"NOM_LOC\" as \"Localidad\",
          b.\"CGLOC\", ",
          clave_indicadores, 
        "FROM ", tabla_localidades_bd, " as a ",
        "INNER JOIN loc.localidades as b ",
          "ON a.\"CGLOC\" = b.\"CGLOC\" AND ",
          "b.\"ID_LOC\" in (", localidades_seleccionadas, ")",
        "INNER JOIN edo_mun.estados as c
         ON c.\"ID_ENT\" = ", input$id_estado,
        "INNER JOIN edo_mun.municipios as d
         ON d.\"ID_MUN\" = ", input$id_municipio,
        "ORDER BY \"Localidad\";")
    } else {
      tabla_localidades_bd <- "ivp.des_local_2020"
      
      query_indicadores <- paste0(
        "SELECT
            c.\"NOMGEO\" as estado,
            d.\"NOMGEO\" as municipio,
            b.\"NOM_LOC\" as \"Localidad\",
            b.\"CGLOC\", ",
            clave_indicadores, 
        "FROM ", tabla_localidades_bd, " as a ",
        "INNER JOIN loc.localidades as b ",
        "ON a.\"CGLOC\" = b.\"CGLOC\" AND ",
            "b.\"ID_LOC\" in (", localidades_seleccionadas, ")",
        "INNER JOIN edo_mun.estados as c
         ON c.\"ID_ENT\" = ", input$id_estado,
        "INNER JOIN edo_mun.municipios as d
         ON d.\"ID_MUN\" = ", input$id_municipio,
        "ORDER BY \"Localidad\";")
    }
    
    indi <- ipa::db_get_table(conn = conexion,
                              statement = query_indicadores)
  })
  
  
  
  ### Inicia evento para mostrar el checkboxgroup del índice y nivel de los temás
  observeEvent(df_localidades_indicadores(), {
    
    # Extrae los nombres de los indicadores usando la clave del indicador
    indicadores_nombre_largo <- df_indicadores_variables() |>
      filter(cve_ind %in% clave_indicador()) |> pull(indicadores)
    
    # Crea un vector para cambiar los nombres de los atributos de la tabla 
    # que se muestra al usuario en la aplicación
    tabla_para_mostrar <- df_localidades_indicadores() |>
      select(-c(estado, municipio, cgloc))
    indicadores_nombre_largo <- c("Localidad", indicadores_nombre_largo)
    colnames(tabla_para_mostrar) <- indicadores_nombre_largo
    
    # Renderiza la tabla para mostrar al usuario
    output$data_table <- renderTable(tabla_para_mostrar)
  })
  ### Fin botón de visualización de datos
  
  ### Inicia botones para descarga de productos en tema de Localidades
  es_mostrar_tabla_botones <- reactive({
    (input$id_indicadores |> isTruthy() |
       input$id_variables |> isTruthy() |
       input$id_des_loc_variables |> isTruthy()) & 
    input$id_localidades |> isTruthy()
  })
  
  observeEvent(es_mostrar_tabla_botones(), {
    shinyjs::hide("id_descargar")
    shinyjs::hide("descargarDescriptor")
    shinyjs::hide("downloadPlot")
    
    if(es_mostrar_tabla_botones()) {
      shinyjs::show("id_descargar")
      shinyjs::show("descargarDescriptor")
      shinyjs::show("downloadPlot")
    }
  })
  ### Fin botones para descarga de productos en tema de Localidades
  
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
  ### Inicio consulta para extraer tabla de descriptor de datos
  df_descriptor <- reactive({
    req(clave_indicador())
    
    # Separa las claves de indicadores con comas y las pone entre comillas para 
    # usarlas de manera fácil en el query
    clave_indicadores <- paste(sQuote(clave_indicador(), FALSE), collapse=",")
    
    # Si el tema es distinto de Aspectos cualitativos de vulnerabilidad accede 
    # a la tabla de vulnerabilidad de localidades rurales
    if (input$id_tema != 10) {
      query_indicadores <- paste0(
        "SELECT * FROM catalogo.indicadores 
        WHERE cve_ind in (", clave_indicadores, ");")
    } else {
      query_indicadores <- paste0(
        "SELECT * FROM catalogo.des_soc_indicadores
        WHERE cve_ind in (", clave_indicadores, ");")
    }
    
    indi <- ipa::db_get_table(conn = conexion,
                              statement = query_indicadores)
    indi <- indi |>
      select(cve_ind, indicadores)
  })
  ### Fin consulta para extraer tabla de descriptor de datos
  
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
      params <- list(df_localidades = df_descriptor())
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
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
