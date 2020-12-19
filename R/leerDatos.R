# Constants

path <- 'data\\'
config <- list(input = list(csv = c('wn_bothhouses_c.csv',
                                    'population_total.csv'),
                            sep = ','),
               output = list(year = 2002,
                             country = 'Afghanistan',
                             pred_csv = 'inflation_annual_percent.csv'))
# Main function

leerDatos <- function(config, path) {
  dfs <- list()
  year <- config$output$year
  country <- config$output$country
  
  for (i in seq(length(config$input$csv)+1)) {
    
    # Para Y
    if (i == 1){
      df_path <- paste0(path, config$output$pred_csv)
      df <- validateY(df_path, country, year)
      dfs[[i]] <- df
      
    } else {
    # Para las X
      df_path <- paste0(path, config$input$csv[i-1])
      
      if (!validateX(df_path, country, year)[[1]]) {
        next
      } else {
      dfs[[i]] <- validateX(df_path, country, year)[[2]]
      }
    }
  }
  num_df <- length(dfs)
  if (num_df<2) {
    logerror('El programa no tiene suficientes datos para predecir.',
             logger = 'log')
    
    stop('Fin')
  } else {
    loginfo(paste('Vas a predecir con', num_df,'de',
                  length(config$input$csv)+1, 'dataframes.'))
    return(dfs)
    }
} 


# Support functions

validateY <- function(df_path, country, year) {
  
  df <- tryCatch(
    {
    
    step <- 'Carga de archivo'
    df <- read.csv(df_path, sep = config$input$sep)
    
    step <- 'Comprobando dimensión'
    if (!check_dim(df)){
      stop()
    }
    
    step <- 'Normalizando dataframe.'
    df <- normalize_df(df)
    
    step <- 'Comprobando que el país está en el dataframe.'
    if (!check_country(df, country)){
      stop()
    }
    
    step <- 'Comprobando que el año esté en el dataframe.'
    if (!check_year_in_cols(df, year)){
      stop()
    }
    
    step <- 'Comprobando que el valor no exista.'
    if (!check_year_with_value(df, country, year)[[1]]){
      step <- paste('El valor está en el dataframe:', check_year_with_value(df, country, year)[[2]])
      stop()
    }
    
    return(df)
    
    }, error = function(e) {
      logerror(paste('Error en la carga del dataframe a predecir:', logger = 'log', step))
      stop()
  })
  return(df)
}

# Validate X

validateX <- function(df_path, country, year) {
  
  df <- tryCatch(
    {
      
      step <- 'Carga de archivo'
      df <- read.csv(df_path, sep = config$input$sep)
      
      step <- 'Comprobando dimensión'
      if (!check_dim(df)){
        return(list(F))
      }
      
      step <- 'Normalizando dataframe.'
      df <- normalize_df(df)
      
      step <- 'Comprobando que el país está en el dataframe.'
      if (!check_country(df, country)){
        return(list(F))
      }
      
      step <- 'Comprobando que el año esté en el dataframe.'
      if (!check_year_in_cols(df, year)){
        return(list(F))
      }
      
      step <- 'Comprobando que el año a predecir tenga un valor en el dataframe'
      if (check_year_with_value(df, country, year)[[1]]){
        step <- paste0('El valor para el año ', year, ' no está en el dataframe.')
        return(list(F))
      }
      
      return(list(T, df))
      
    }, error = function(e) {
      logerror('Error en la carga del dataframe a predecir.
               Este dataframe será obviado y se intentará predecir con el resto', logger = 'log')
      stop()
    })
  return(df)
}



# Normalizamos dataframe
normalize_df <- function(df) {
  rownames(df) <- df$country #Definimos country como index
  df$country  <- NULL #Borramos columna country
  colnames(df) <- substring(colnames(df), 2) #Normalizamos nombre de columnas
  return(df)
}



# Comprueba si los csv se han cargado bien
check_dim <- function(df){
  if ((dim(df)[1] > 0) && (dim(df[2]) > 0)) {
    return(TRUE)
  }
  else {
    return(FALSE)
  }
}


# Comprueba si el país a predecir está en los csv predictores
check_country <- function(df, country) {
  if (country %in% rownames(df)) {
      return(TRUE)
    } else {
      return(FALSE)
    }
}

# Comprueba si el año a predecir existe como valor en los csv predictores
check_year <- function(df, country, year) {
  if (year %in% names(df)) {
      return(TRUE)
    } else {
      return(FALSE)
    }
}
is.na(df1_1['Brazil', as.character(1700)])
config$output$year %in% colnames(df1_1)
df1_1['Brazil', config$output$year]

# Comprobamos que el año esté en las columnas
check_year_in_cols <- function(df, year) {
  if (year %in% colnames(df)) {
    return(T)
  }
  else {
    return(F)
  }
}

# Comprobamos que el año a predecir no esté ya predicho
check_year_with_value <- function(df, country, year) {
  
  value <- df[country, as.character(year)]
  if (is.na(value)) {
    return(list(TRUE, NA))
  }
  else {
    return(list(FALSE, value))
  }
}



undebug(leerDatos)
undebug(validateY)
undebug(validateX)
undebug(check_country)
debug(leerDatos)
debug(validateY)
debug(validateX)
debug(check_country)
leerDatos(config, path)

a <- leerDatos(config, path)

aa <- paste('algo', '+', 'otra cosa')
aa

validateY(df_path = paste0(path, config$output$pred_csv))
read.csv(paste0(path, config$output$pred_csv), sep = config$input$sep)
normalize(read.csv(paste0(path, config$output$pred_csv), sep = config$input$sep))








library(logging)
tryCatch( expr = {
  
  addHandler(writeToFile, logger = 'log', file = "logs/logfile.log")
  loginfo("Empezamos la app...", logger = 'log')
  abc <- acd
  
}, error = function(e) {
  logerror('Esto es una prueba' = 'log')
  
}, finally = {
  
  loginfo("Fin de la ejecucion.", logger = 'log')
  removeHandler(writeToFile, logger = 'log')  
  
})
