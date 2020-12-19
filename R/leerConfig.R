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
leerConfig <- function(path){
  
  library(XML)
  
  
  configPath <- paste0(path, "/config/config2.xml")
  
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
  # Una vez leido el config, tengo que validar la estructura de nodos a partir de los nombres de las listas
  
  validateConfigNodes(config)
  
  config$input$names <- trimws(strsplit(config$input$name, ",")[[1]])
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
  
  nodoPrincipal <- identical(names(config), c("input", "prediction"))
  nodoInput <- identical(names(config$input), c("name", "sep"))
  nodoPrediction <- identical(names(config$prediction), c("pais", "anyo","valor"))
  
    nodos <- c("nodoPrincipal" = nodoPrincipal, "nodoInput" = nodoInput, 
             "nodoPrediction" = nodoPrediction)
  
  check <- all(nodos)
  

  if(!check){
    
    nodosMalos <- names(nodos)[!nodos]
    
    logerror(paste0("Los nodos: ", paste(nodosMalos, collapse = ", "),
                    " estan mal estructurados!"), logger = 'log')
    stop()
    
  }
  
}

