# Función para cargar paquetes e instalar los que hagan falta -------------

cargar_paquetes <- function(...){
  paquetes <- c("tidyverse", "stringr", "lubridate", "foreign", ...)
  if (length(setdiff(paquetes, rownames(installed.packages()))) > 0) {
    install.packages(setdiff(paquetes, rownames(installed.packages())))  
  }
  lapply(paquetes, require, character.only = TRUE)
  return(search())
}

cargar_paquetes()

# Función para calcular significancia de parámetros -----------------------

prob <- function(x){
  out <- min(length(x[x>0])/length(x), length(x[x<0])/length(x))
  out
}