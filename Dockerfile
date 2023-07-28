FROM rocker/shiny-verse:4.3.0
LABEL maintainer="USER <david.martinez@macer.tech>"

# Incluye los drivers para conectarse con la base de datos de PostgreSQL
RUN apt-get update && \
    apt-get install --yes libpq-dev

# Librerías para manejo de datos espaciales
RUN apt-get install --yes \
    libgdal-dev \
    libgeos-dev \
    libgeos++-dev \
    # La librería libssl-dev instala el paquete sf de R
    libssl-dev \
    libudunits2-dev \
    libproj-dev \
    # Librería para poder instalar en R el paquete basemaps
    libmagick++-dev

# Instalar Tinytex
RUN wget -qO- "https://yihui.org/tinytex/install-bin-unix.sh" | sh

# Instala librerías necesarias para trabajar con datos espaciales y bases de datos en PostgreSQL
RUN R -e "install.packages(c('DBI', 'ggplot2', 'RPostgres', 'remotes', 'basemaps', 'shinyjs'), dependencies = TRUE)"

# Instala paquete ipa para el manejo de la BD del servicio
RUN R -e "remotes::install_gitlab('davidmacer/ipa@develop')"
# Difine app/ como el directorio de trabajo dentro del contenedor
WORKDIR /app 
# Copia los archivos que están dentro de /serloc-app al directorio de trabajo
COPY . /app
# Otorga todos los permisos al wordir 
RUN chmod -R 777 /app
# Expón el puerto del contenedor
EXPOSE 4040
# Agrega usuario para tener persmisos de administrador     
RUN useradd shiny_user
USER shiny_user
# Corre aplicación dentro del contenedor
CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 4040)"]
