

inputarNansApp <- function(path){
  tryCatch({
    library(logging)
    
    # Generamos el manejado de logs
    addHandler(writeToFile, logger = 'log', file = paste0(path, "logs\logfile.log"))
    loginfo('Iniciando app...', logger = 'log')
    
    # Leemos el archivo de configuración
    loginfo('Leyendo config...', logger = 'log')
    config <- leerConfig(path)
    loginfo('Config leido.', logger = 'log')
    
    # Leemos los datos y devolvemos dataframes a usar
    loginfo('Leyendo datos...', logger = 'log')
    dataframes <- leerDatos(config, path)
    loginfo('Datos leídos')
    
    # Transformamos en un único dataframe y sacamos la variable a predecir
    loginfo('Procesando datos...', logger = 'log')
    df_Xpred <- procesarDatos(config, dataframes)
    loginfo('Datos procesador', logger = 'log')
    
    # Generamos un modelo de predicción
    loginfo('Creando modelo...', logger = 'log')
    model <- generarModelo(df_Xpred[[1]])
    loginfo('Modelo creado', logger = 'log')
    
    # Predecimos con el modelo y X_pred
    loginfo('Prediciendo...', logger = 'log')
    y_pred <- generarOutput(model, df_Xpred[[2]])
    loginfo('Predicción creada')
    
    
  }, error = function(e) {
    logerror('La aplicación ha fallado.', logger = 'log')
    stop()
  }, finally = {
    loginfo('Fin de la ejecución', logger = 'log')
    removeHandler(writeToFile, logger = 'log')
  })
}