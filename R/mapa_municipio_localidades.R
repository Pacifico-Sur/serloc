#!/usr/bin/env Rscript
# Este script crea el mapa que contiene el contorno del polígono del municipio
# elegido por el usuario y las localidades rurales dentro del municipio. Las 
# localidades están representadas (según el censo del inegi) por polígonos 
# (aquellas localidades que están amanzanadas y por puntos (aquellas localidades 
# que no están amanzanadas).

# Se conecta con la base de datos de localidades rurales
# El host, puerto, user y password están definidas en el docker-compose.yml del 
# repositorio database (https://github.com/Pacifico-Sur/database)
# library(ipa)
library(ggplot2)
library(magrittr)

mapa_municipio_localidades <- function(id_mun = 1458) {
  con <- DBI::dbConnect(RPostgres::Postgres(), dbname = "siclr_db",
                        host = "database", port = "5432",
                        user = "postgres", password = "postgres")
  
  # Toma como argumento el ID del municipio seleccionado por el usuario
  # args <- commandArgs(TRUE)
  # id_muni <- args[1]
  id_muni <- id_mun
  
  # Extrae los datos de municipio, estado, localidades (puntos) y localidades 
  # (polígonos)
  query_mun <- paste("SELECT * from edo_mun.municipios WHERE ",
                     "\"ID_MUN\"", " = ", id_muni, ";")
  municipio <- ipa::db_get_table(con, query_mun)
  
  query_edo <- paste("SELECT * from edo_mun.estados WHERE ",
                     "\"ID_ENT\"", " = ", municipio$id_ent, ";")
  estado <- ipa::db_get_table(con, query_edo)
  
  query_loc_puntos <- paste("SELECT * FROM loc.localidades ",
                            "WHERE \"ID_MUN\"", "= ", id_muni, " AND",
                            "\"POBTOT\" >= 100 AND \"POBTOT\" <= 2500;")
  loc_puntos <- ipa::db_get_table(con, query_loc_puntos)
  
  query_loc_poligonos <- "SELECT * FROM loc.loc_poli WHERE \"TIPO\" = \'Rural\';"
  loc_poli <- ipa::db_get_table(con, query_loc_poligonos)
  
  loc_poli <- loc_poli %>%
    filter(cve_ent == estado$cve_ent,
           cve_mun == municipio$cve_mun,
           cve_loc %in% loc_puntos$cve_loc)
  loc_poli <- loc_poli %>%
    st_set_crs(st_crs(loc_puntos))
  
  loc_puntos <- loc_puntos %>%
    filter(!cve_loc %in% loc_poli$cve_loc)
  
  # Define un mapa base por defecto
  basemaps::set_defaults(
    map_service = "osm", 
    map_type = "streets",
    map_res = 0.75)
  
  # Llama la función png para inicializar el gráfico
  # save_path <- "../temp-img/"
  # dir.create(save_path)
  # El directorio para guardar el archivo
  # png(file = paste0(save_path, "mi_mapa.png"),
  #     width = 30, # The width of the plot in cm
  #     height = 27, # The height of the plot in cm
  #     units = "cm",  # The units to save the plot
  #     res = 100)
  
  # Crea la gráfica con el diseño necesario
  mymap <- ggplot() +
    basemaps::basemap_gglayer(municipio) +
    geom_sf(data = municipio %>% st_transform(crs = st_crs(3857)),
            fill = "blue", alpha = 0.030, colour = "black") +
    geom_sf(data = loc_puntos) +
    geom_sf(data = loc_poli,
            color = "black",
            fill = "black",
            alpha = 0.6) +
    coord_sf(expand = FALSE) +
    scale_fill_identity() +
    labs(title = paste0("Municipio de ", municipio$nomgeo, ", ", estado$nomgeo),
         x = "Longitud",
         y = "Latitud",
         caption = "CentroGeo. Servicio de Información y Conocimiento de Localidades Rurales") +
    theme(legend.position = "none",
          axis.text = element_text(size = 10),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          plot.title = element_text(size = 18),
          plot.subtitle = element_text(size = 14),
          plot.caption = element_text(size = 6))
  
  # Cierra la gráfica que se guardó
  # dev.off()
  return(mymap)
}