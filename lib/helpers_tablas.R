# Función para copiar tablas a portapapeles -------------------------------

# Notar que por default se escriben con separador por columna ';'

copiar_tabla <- function(base, sep = ";" , row.names = F, ...){
  if(row.names == T){
    if(!is.data.frame(base)) base <- as.data.frame(base)
    base <- base %>% rownames_to_column()
    names(base)[1] <- "" 
    # Identificar sistema operativo
    if (.Platform$OS.type == "windows") { 
      write.table(base, "clipboard", sep = sep, row.names = F, ...)
    } else if (Sys.info()["sysname"] == "Darwin") {
      write.table(base, pipe('pbcopy'), sep = sep, row.names = F, ...)
    }
  } else{
    if (.Platform$OS.type == "windows") { 
      write.table(base, "clipboard", sep = sep, row.names = row.names, ...)
    } else if (Sys.info()["sysname"] == "Darwin") {
      write.table(base, pipe('pbcopy'), sep = sep, row.names = row.names, ...) 
    }
  }
}

# Funciones para calcular perfiles ----------------------------------------

perfilesColumna <- function(df){
  # Input: 
  #   df - Es un dataframe de porcentajes donde la primera columna contiene los 
  #        nombres de atributos
  #
  # Output:
  #  Matriz con perfiles columna
  names(df)[1] <- "nombres_atrib"
  mat <- df %>%
    as.data.frame() %>% 
    remove_rownames() %>% 
    column_to_rownames("nombres_atrib") %>% 
    as.matrix
  
  apply(mat, 2, function(x){x/mean(x, na.rm = T)})
}

perfilesRenglon <- function(df){
  # Input: 
  #   df - Es un dataframe de porcentajes donde la primera columna contiene los 
  #        nombres de atributos
  #
  # Output:
  #  Matriz con perfiles renglón
  names(df)[1] <- "nombres_atrib"
  mat <- df %>%
    as.data.frame() %>% 
    remove_rownames() %>% 
    column_to_rownames("nombres_atrib") %>% 
    as.matrix
  
  t(apply(mat, 1, function(x){x/mean(x, na.rm = T)}))
}

perfilesDobles <- function(df){
  # Input: 
  #   df - Es un dataframe de porcentajes donde la primera columna contiene los 
  #        nombres de atributos
  #
  # Output:
  #  Matriz con perfiles dobles (Índices entre 0-100)
  apply(perfilesRenglon(df), 2, function(x){x/mean(x, na.rm = T)})*100
}

# Para obtener relación entre variables categóricas -----------------------

# Para variables nominales (no existe orden en las categorías)

lambdaGK <- function(var1, var2, nivel_IC = NA, tabla_cont = F, tipo = "symmetric"){
  # Input:
  #   var1, var2 - Variables a tabular
  #   nivel_IC - Nivel del intervalo de confianza, si se desea obtener
  #   tabla_cont - Valor lógico, TRUE si se desea mostrar la tabla de 
  #                contingencia en el output
  #   tipo - "symmetric" si se desea calcular la lambda sin importar sobre qué
  #                      variable se condicione. 
  #           "row" si se quiere predecir var2 dada var1
  #           "column" si se quiere predecir var1 dada var2
  # 
  # Output:
  #   lambda - Valor del estadístico lambda de Goodman y Kruskal
  #            Ran(lambda) = [0, 1]
  #            Se interpreta como la información que aporta conocer una catgoría al 
  #            predecir la otra, respecto a la predicción sin información adicional
  #   IC - (en caso de dar nivel) Intervalo de confianza 
  #   tabla_cont - (en caso de TRUE) Tabla de contingencia con frecuencias
  #                para cada combinación de niveles de ambas variables
  # 
  cargar_paquetes("DescTools")
  lam <- Lambda(var1, var2, conf.level = nivel_IC, direction = tipo) %>% 
    as.numeric()
  tab_cont <- table(var1, var2,
                    dnn = c(deparse(substitute(var1)), deparse(substitute(var2))))
  if(is.na(nivel_IC)){
    if(tabla_cont) 
      return(list(lambda = lam, tabla_cont = tab_cont))
    else 
      return(lam)
  } 
  else{
    if(tabla_cont)
      return(list(lambda = lam[1], IC = c(lam[2], lam[3]), 
                  tabla_cont = tab_cont))
    else return(list(lambda = lam[1], IC = c(lam[2], lam[3])))
  }
  # Obs: La tabla de contingencia puede convertirse a df con 
  # as.data.frame.matrix(tabla_cont)
}

# Para variables ordinales (categorías ordenadas)

gammaGK <- function(var1, var2, nivel_IC = NA, tabla_cont = F){
  # Input:
  #   var1, var2 - Variables a tabular
  #   nivel_IC - Nivel del intervalo de confianza, si se desea obtener
  #   tabla_cont - Valor lógico, TRUE si se desea mostrar la tabla de 
  #                contingencia en el output
  # 
  # Output:
  #   gamma - Valor del estadístico gamma de Goodman y Kruskal
  #           Ran(gamma) = [-1, 1]
  #           Nos dice qué tan más probable es tener concordancia que 
  #           discordancia en ambas clasificaciones, cuando se seleccionan dos
  #           individuos de la población al azar
  #   IC - (en caso de dar nivel) Intervalo de confianza 
  #   tabla_cont - (en caso de TRUE) Tabla de contingencia con frecuencias
  #                para cada combinación de niveles de ambas variables
  # 
  cargar_paquetes("vcdExtra")
  tab_cont <- table(var1, var2, 
                    dnn = c(deparse(substitute(var1)), deparse(substitute(var2)))) 
  gam <- tab_cont %>% 
    vcdExtra::GKgamma(level = nivel_IC) 
  if(is.na(nivel_IC)){
    if(tabla_cont)
      return(list(gamma = gam$gamma, tabla_cont = tab_cont))
    else
      return(gam$gamma)
  }
  else {
    if(tabla_cont)
      return(list(gamma = gam$gamma, IC = gam$CI, tabla_cont = tab_cont))
    else
      return(list(gamma = gam$gamma, IC = gam$CI))
  }
  # Obs: La tabla de contingencia puede convertirse a df con 
  # as.data.frame.matrix(tabla_cont)
}
