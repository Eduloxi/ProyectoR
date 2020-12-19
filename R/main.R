path <- 'data\\'

setwd(path)

lapply(paste0('R\\', list.files(path = 'R\\', recursive = TRUE)), source)

inputarNansApp(path)