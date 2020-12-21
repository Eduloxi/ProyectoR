

# Main function

procesarDatos <- function(config, dataframes) {
  dataframes <- isolate_dataframes_country(dataframes) # Nos quedamos solo con country
  df <- merge_dataframes(dataframes) # Combinamos en un único dataframe
  
  # Sacamos X_pred
  X_pred <- df[, as.character(config$output$year)]
  X_pred <- X_pred[1:length(X_pred)-1]
  
  # Eliminamos nans y trasponemos
  df <- t(df[, colSums(is.na(df)) == 0]) 
  
  check_size <- check_merged_dimension(df)
  if (!check_size[[1]]) {
    logerror(paste0("El dataframe no tiene suficientes datos como para predecir,
                    prueba con otros. Tamaño: ", check_size[[2]]),
             logger = 'log')
    stop()
  }
  colnames(df) <- get_columns_names(dataframes) # Ponemos nombre a las columnas
  
  return(list(df, X_pred))
  
}

# Support functions

# Creamos nombre de columnas
get_columns_names <- function(dfs) {
  columns_names <- c()
  for (i in seq(length(dfs))) {
    if (i == 1){
      columns_names[i] <- 'Y'
    } else {
      columns_names[i] <- paste0('X', i-1)
    }
  }
  return(columns_names)
}

#Aislamos el país que queremos predecir
isolate_dataframes_country <- function(dfs) {
  for (i in seq(length(dfs))) {
    dfs[[i]] <- dfs[[i]][config$output$country, ]
  }
  return(dfs)
}

# Combinamos dataframes
merge_dataframes <- function(dfs) {
  merged_df <- Reduce(function(x, y, ...) merge(x,
                                                y,
                                                by = intersect(names(x), names(y)),
                                                all = TRUE, ...), dfs)
  return(merged_df)
}


# Comprobamos dimension
check_merged_dimension <- function(df){
  size <- dim(df)[1]
  if (size < 4) {
    return(list(FALSE, size))
  } else {
    return(list(TRUE, size))
  }
}


# Test

undebug(procesarDatos)
df <- procesarDatos(config, a)