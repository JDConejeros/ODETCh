#################################################################################################################/
##################### ODETCh - Extracción ET ############################################################
#################################################################################################################/
# Dudas: jdconejeros@uc.cl 
# Objetivo: Proceso recursivo para extraer data de enfermedades transmisibles del minsal
# Web de extracción: http://epi.minsal.cl/enfermedades-de-notificacion-obligatoria/
# Nota: dada la naturaleza de la web del minsal este código requiere de mantención, para esto se realiza una 
# mantención cada 3 meses. 
# Prox actualización: mayo, 2023
#################################################################################################################/

# Settings ----

# Función para instalar/cargar los paquetes en uso
rm(list=ls())
install_load <- function(packages){
  for (i in packages) {
    if (i %in% rownames(installed.packages())) {
      library(i, character.only=TRUE)
    } else {
      install.packages(i)
      library(i, character.only = TRUE)
    }
  }
}

packages_session <- c("robotstxt", # Evalúa impedimentos para realizar webscraping
                      "httr",# Funciones que me permiten acceder a los datos
                      "rvest", # Para realizar webscraping (extraer htmls: cambia la url)
                      "dplyr", # Funciones para el procesammiento de variable
                      "tidyr", # Funciones para operar tablas
                      "purrr", # Para procesos iterativos
                      "stringr", # Para trabajar cadenas de caracteres
                      "lubridate", # Procesamiento de fechas
                      "writexl" # Generador de excel
                      )     
install_load(packages_session)

# 1. Evaluando la extracción ----

# Evaluamos si la página tiene alguna restricción para el webscrapping
minsal_ver <- get_robotstxt("http://epi.minsal.cl/enfermedades-de-notificacion-obligatoria/") # Todo está disponible y aplica para todas las páginas subyacentes
minsal_ver
r_parsed <- parse_robotstxt(minsal_ver)
r_parsed # Al parecer esta todo disponible para el uso

# También verificamos unos de los excel que queremos descargar (a modo de ejemplo)
minsal_ver_file <- get_robotstxt("http://epi.minsal.cl/wp-content/uploads/2022/02/brucelosis.xlsx") # Todo está disponible y aplica para todas las páginas subyacentes
minsal_ver_file
r_parsed <- parse_robotstxt(minsal_ver_file)
r_parsed # Al parecer esta todo disponible para el uso

# 2. Realizamos una primera extracción de prueba (Ejemplo) ----

url <- "http://epi.minsal.cl/brucelosis-bases-de-datos/" # Link de la web

html <- read_html(url) # Leemos el html file

excel_link <- html  %>% 
  html_nodes("a[href$='.xlsx']") %>% 
  html_attr("href") %>%  
  head(1) # Generamos el link de extracción

download.file(excel_link, destfile = paste0("Data/Ejemplo_", Sys.Date()," .xlsx"), mode = "wb") # Descargamos el file

# 3. Iteramos todas las extracciones ----

# Intentaremos generalizar lo anterior

## 3.1 Extracción de enlaces ----

# Especificar la URL de la página web
url <- "http://epi.minsal.cl/enfermedades-de-notificacion-obligatoria/"
html <- read_html(url)

# Extraemos los enlaces globales
enlaces <- html_nodes(html, "a") %>%
  html_attr("href") %>% 
  str_subset(pattern = "^/.*/") %>%
  paste0("http://epi.minsal.cl", .)

# Incorporación de enlaces manuales
enlaces <- c(enlaces, 
             "http://epi.minsal.cl/haemophilus-influenzae/", 
             "http://epi.minsal.cl/enfermedad-invasora-por-streptococcus-pneumoniae-6",
             "http://epi.minsal.cl/fiebres-hemorragicas/",
             "http://epi.minsal.cl/hepatitis-e/",
             "http://epi.minsal.cl/hepatitis-ninos/"
             #"http://epi.minsal.cl/sindrome-inflamatorio-multisistemico-pediatrico/"
             #"http://epi.minsal.cl/sindrome-hemolitico-uremico/"
             )

# Extraemos los enlaces específicos
data_enlaces <- data.frame()
for(url in enlaces){
  # Leer el contenido de la página
  pagina <- read_html(url)
  
  enlace <- pagina %>% html_nodes(xpath = "/html/body/div[3]/div/div[2]/div[2]/div/ul/li[9]/a") %>% html_attr("href")
  
  data_enlaces <- bind_rows(bind_cols(url, enlace), data_enlaces)
  
}

# Veamos las diferencias entre el vector de caracteres y la data 
setdiff(enlaces, data_enlaces$...1)
# Las siguientes páginas no tienen algún link que contenga la palabra base de datos
# [1] "http://epi.minsal.cl/chikungunya/" # Esta fuente si tiene datos disponibles                                   
# [2] "http://epi.minsal.cl/diarreas/"                                       
# [3] "http://epi.minsal.cl/diarreas-rotavirus/"                             
# [4] "http://epi.minsal.cl/varicela/"                                       
# [5] "http://epi.minsal.cl/viruela/"                                        
# [6] "http://epi.minsal.cl/viruela-simica/"   
# Las fuentes a continuación corresponden a enlaces que se pueden obtener de forma más directa desde otros lados
# [7] "http://epi.minsal.cl/boletin-epidemiologico-trimestral-edicion4-2019/"
# [8] "http://epi.minsal.cl/boletin-de-brotes/"                              
# [9] "http://epi.minsal.cl/ens_tableau_ens/#/"                              
# [10] "http://epi.minsal.cl/encavi-2018/#/"                                  
# [11] "http://epi.minsal.cl/encuesta-ens/"  
# También se agrega el Síndrome Inflamatorio Multisistémico Pediátrico por contar con dos fuentes de datos distinas y datos poco actualizados


# Se nos quedan dos que el path a la base de datos es distinto
# Chikungunya (full path específico) 

data_enlaces <- data_enlaces %>% add_row(
  `...1`="http://epi.minsal.cl/chikungunya/",
  `...2`=read_html("http://epi.minsal.cl/chikungunya/") %>% html_nodes(xpath = "/html/body/div[3]/div/div[2]/div[2]/div/ul/li[8]/a") %>% html_attr("href")
)  %>% add_row(
  `...1`="http://epi.minsal.cl/hepatitis-a/",
  `...2`=read_html("http://epi.minsal.cl/hepatitis-a/") %>% html_nodes(xpath = "/html/body/div[3]/div/div[2]/div[2]/div/ul/li[10]/a") %>% html_attr("href")
)

## 3.2 Extracción de datos ----

urls <- data_enlaces$...2 # Todos los enlaces a buscar

# Función para descargar todos los archivos 
# Evalúa enlace por enlace la información
download_files <- function(urls, path = paste0("./Data/Extract_", Sys.Date(), "/"), results_path = NULL) {
  
  dir.create(paste0("Data/Extract_", Sys.Date())) # Carpeta con nuestros resultados
  
  resultados <- data.frame(url = character(), resultado = logical(),
                           stringsAsFactors = FALSE) # Data con los resultados de la extracción
  # Iteración de búsqueda 
  for (url in urls) {
    tryCatch({
      # Descargar el archivo
      nodes <- html_nodes(read_html(url), "a[href$='.csv'],a[href$='.xls'],a[href$='.xlsx']")
      if (length(nodes) > 0) {
        x <- html_attr(nodes, "href")
        file_name <- basename(x)
        file_path <- file.path(paste0(path, Sys.Date(), "_", file_name))
        if (!file.exists(file_path)) {
          cat(sprintf("Descargando archivo desde %s...\n", url))
          download.file(x, file_path)
          cat(sprintf("Archivo guardado como %s\n", file_name))
        } else {
          cat(sprintf("El archivo %s ya existe en %s\n", file_name, path))
        }
        resultado <- TRUE
      } else {
        cat(sprintf("No se encontró ningún archivo para descargar en %s\n", url))
        resultado <- FALSE
      }
    }, error = function(e) {
      cat(sprintf("Error al descargar archivo desde %s: %s\n", url, e$message))
      resultado <- FALSE
    })
    resultados <- rbind(resultados, data.frame(url = url, resultado = resultado, file=file_name))
  }
  # Generamos una tabla de resultados
  if (!is.null(path)) {
    writexl::write_xlsx(resultados, file.path(path, "Resumen_extraccion_data.xlsx"))
  }
  
  return(resultados)
}


# Aplicamos la función sobre el vector de interés
resultados <- download_files(urls)

## 3.3 Revisión de resultados ----

# Los siguientes enlaces: 
# a) requiere de una solicitud por transparencia para acceder a los datos
# b) la tabla de datos está en proceso de actualización
#   http://epi.minsal.cl/fiebres-hemorragicas-basedatos/
#   http://epi.minsal.cl/informes-covid-19/
#   http://epi.minsal.cl/tuberculosis-basedatos/
#   http://epi.minsal.cl/rabia-humana-basedatos/
#   http://epi.minsal.cl/poliomielitis-basedatos/
#   http://epi.minsal.cl/peste-basedatos/
#   http://epi.minsal.cl/leishmaniasis-basedatos/
#   http://epi.minsal.cl/influenza-basedatos/
#   http://epi.minsal.cl/hepatitis-viral-a-preguntas-frecuentes/
#   http://epi.minsal.cl/fiebre-tifoidea-y-paratifoidea-basedatos/
#   http://epi.minsal.cl/difteria-basedatos/
#   http://epi.minsal.cl/dengue-base-datos/
#   http://epi.minsal.cl/colera-basedatos/
#   http://epi.minsal.cl/enfermedad-invasora-haemophilus-basedatos/  (POR REVISAR)

# Limpiamos la memoria 
rm(list=ls())
