# (Este script tiene la función, que va a ejecutar la función de predicción)


# 1)	Una primera parte donde cargamos todas las librerías para ejecutar la función.
# Cuando tengamos aquí cargadas las librerías ya no tenemos que hacer un library en ninguna otra parte.

Lapply(c(“dummies”, “logging”, “xgboost”, “XML”), require, character, only=T)

# 2)	Cargo el PATH (sitio donde Eduardo tiene la estructura de las carpetas).

directorio <- "~/BOOTCAMPS/18112019/clasificarContactos/"

# 3)	Lo fijo como working directory, para que no tenga que hacer rutas absolutas sino que con relativas me bastaría.

setwd(directorio)


# 4)	Lo siguiente es ejecutarlo todo mediante el source ( Es un source de cada uno de los 5 scripts que guardaríamos en la carpeta R)

lapply(paste0("R/", list.files(path = "R/", recursive = TRUE)), source)

# 5)	El source ejecuta la función principal (que la llamo predictNA)
  
debug(clasificarContactosApp)
predictNA(directorio)
undebug(clasificarContactosApp)
