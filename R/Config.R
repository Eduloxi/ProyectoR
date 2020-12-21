# Instalar la librería para trabajar con XML
install.packages("XML")
library(XML)

# declaramos una vble de tipo string que guarda el nombre del fichero que vamos a analizar

url <- "../MI_CONFIG/config.XML"

# Creamos un apuntador que localiza el fichero

xmldoc <- xmlParse(url)

# Ahora Vamos a recorrer ese documento al que está apuntando, por cada uno de los nodos que tiene este XML


# Para ello , primero vemos los nodos raiz
rootnode <- xmlRoot(xmldoc)

# El rootnode nos coloca en el origen del fichero.
# Por ejemplo podemos consultar el primer elemento, el segundo, el tercero...

rootnode[1]
rootnode[2]
rootnode[3]

# Aquí no obtenemos todos los datos, sino que hay que extraerlos del XML.
# Para ello lo vamos a poner en un dataframe

# Cargamos los datos en una tabla mediante xmlsApply que es una función
# que hay que pasarle el nodo raiz, y una función con smlsApply para extraerle los valores
datos <- xmlSApply(rootnode, function(x) xmlSApply(x, xmlValue))

# Hacemos una transposición, para crear un dataframe

datos <- data.frame(t(datos), row.names = NULL)

################################################################

# METODO DE ANDER:

#' Title
#'
#' @param path 
#'
#' @return
#' 
#' @import XML
#' @import logging
#'
#' @examples


path <-("C:/Users/ENVY/TheBridge/Trabajos/PROYECTO_R 20201222/ProyectoR")

leerConfig <- function(path){
  
  library(XML)
  
  
  configPath <- paste0(path, "MI_CONFIG/config.XML")
  
# El tryCatch te permite capturar y manejar errores 
   
  tryCatch(expr = {
    
    #Leer el xml y convertirlo a lista
    config <- XML::xmlToList(xmlParse(configPath))
    
    
  }, error = function(e){
    
    logerror("Config no encontrado en su ruta. Verifica que se llame config.xml",
             logger = 'log')
    stop()
  })
  
  loginfo("Config leido.", logger = 'log')
  
  validateConfigNodes(config)
  
  config$columnas$predictorasNumericas <- trimws(strsplit(config$columnas$predictorasNumericas, ",")[[1]])
  config$columnas$fechas$tiempos <- as.numeric(trimws(strsplit(config$columnas$fechas$tiempos, ",")[[1]]))
  
  config$columnas$mails$ratios <-  as.logical(config$columnas$mails$ratios)
  
  
  separadoresAceptados <- config$input$sep %in% c(",", ";")
  
  if(!separadoresAceptados){
    
    logerror("Sep solo puede valer ',' o ';' ", logger = 'log')
    stop()
    
  }
  
  return(config)
  
}




#' @title validateConfigNodes
#'
#' @param config 
#'
#' @import logging
#' 
validateConfigNodes <- function(config){
  
  nodoPrincipal <- identical(names(config), c("input", "columnas"))
  nodoInput <- identical(names(config$input), c("name", "sep"))
  nodoColumnas <- identical(names(config$columnas), c("ID", "predictorasNumericas",
                                                      "fuenteOriginal", "dominio_mail",
                                                      "fechas", "mails", "target", "llamada"))
  
  nodoFechas <- identical(names(config$columnas$fechas), c("creacion", "ultima_mod",
                                                           "apertura_ultimo", "envio_ultimo",
                                                           "apertura_primero", "envio_primero",
                                                           "visita_primero", "visita_ultimo",
                                                           "tiempos"))
  
  nodoMails <- identical(names(config$columnas$mails), c("mailsDl", "mailsCl", "mailsOp", "ratios"))
  
  nodos <- c("nodoPrincipal" = nodoPrincipal, "nodoInput" = nodoInput, 
             "nodoColumnas" = nodoColumnas, "nodoFechas" = nodoFechas,
             "nodoMails" = nodoMails)
  
  check <- all(nodos)
  
  if(!check){
    
    nodosMalos <- names(nodos)[!nodos]
    
    logerror(paste0("Los nodos: ", paste(nodosMalos, collapse = ", "),
                    " estan mal estructurados!"), logger = 'log')
    stop()
    
  }
  
}


  
