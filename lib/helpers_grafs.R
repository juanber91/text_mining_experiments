# Paletas de colores ------------------------------------------------------

# Paleta cbb (52 colores) con gris

cbb.s <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
           "#8DD3C7","#FFFFB3","#BEBADA","#FB8072","#80B1D3","#FDB462","#B3DE69","#FCCDE5",
           "#D9D9D9","#BC80BD","#CCEBC5","#A6CEE3","#1F78B4","#B2DF8A","#33A02C","#FB9A99","#E31A1C",
           "#FDBF6F","#FF7F00","#CAB2D6","#6A3D9A","#FFFF99","#1B9E77","#D95F02","#7570B3","#E7298A",
           "#66A61E","#E6AB02","#A6761D","#666666","#7FC97F","#BEAED4","#FDC086","#386CB0",
           "#F0027F","#BF5B17","#66C2A5","#FC8D62","#8DA0CB","#E78AC3","#A6D854","#FFD92F","#E5C494","#B3B3B3")

# Para color de lìnea y/o puntos

col.ptos <-   scale_colour_manual(values = cbb.s)

# Para usar como color de relleno

col.fill <-  scale_fill_manual(values = cbb.s)


# Theme de ggplot ---------------------------------------------------------

theme_set(theme_bw())

# Multiplot ---------------------------------------------------------------

# Función para mostrar múltiples gráficos en un layout
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  # Input:
  #   Pueden pasarse las gráficas de ggplot2 en ..., 
  #          o hacer una plotlist (lista de objetos de ggplot2)
  #   cols - Número de columnas en el layout
  #   layout - Una matriz especificando el layout. Si no es NULL, se ignora cols
  #
  # Output: Layout de gráficas con la configuración deseada
  #
  cargar_paquetes('grid')
  
  # Hacer una lista con los argumentos ... y plotlists
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # Si layout is NULL, usar 'cols' para determinar el layout
  if (is.null(layout)) {
    # Hacer el panel
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Configurar la página
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Mostrar cada gráfica en la configuración correcta
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


# Función para graficar un biplot (variables-atributos) -------------------

biplot_atributos <- function(PC,  format = 'analysis', axes=c(1, 2), 
                             pts = TRUE,
                             calidad_rep = ifelse(format == 'analysis', TRUE, FALSE), 
                             ind.lab.col = 'gray30', ind.point.col = 'black', 
                             ind.display = "both", ind.size.lab = 5, 
                             ind.alpha.lab = 1, ind.size.pon = 2, 
                             ind.alpha.pon = .7, varnames = NULL, 
                             var.col = 'red', var.size = 5.5,
                             ms = 0.75, arr.col = c('red', 'red'), 
                             arr.size = c(.3, .5), main = NULL, ind.labs = NULL, 
                             x.lim = NULL, y.lim = NULL){
  # Input: 
  #   PC - Salida de la función PCA.
  #   format - Cadena de caracteres indicando el formato de salida de la gráfica:
  #         'analysis' para el biplot con todos los elementos, o 'report' para el 
  #          formato entregable al equipo de Liss (sin ejes, sin labels, etc.).
  #   axes - Indica las componentes del PCA a utilizar.
  #   pts - Valor lógico. Indica si se graficarán los puntos de atributos en el 
  #         biplot (TRUE) o sólo los vectores (FALSE).
  #   calidad_rep - Valor lógico. Indica si los puntos deben colorearse de acuerdo 
  #                 a qué tanbien representados se encuentran en el gráfico 
  #                 bidimensional (sólo disponible en el formato de análisis)
  #   ind.lab.col - Color de las etiquetas de atributos.
  #   ind.point.col - Color de los puntos que representan los atributos.
  #   ind.display - Cadena de caracteres que indica el formato en que se muestran
  #         los atributos - 'nams' si se desea sólo mostrar las etiquetas,
  #         'points' para mostrar puntos de atributos, 'both' para ambos.
  #   ind.size.lab -Tamaño de letra de las etiquetas de atributos.
  #   ind.alpha.lab - Parámetro para modificar transparencia de las etiquetas de
  #         atributos. 
  #   ind.size.pon - Tamaño de los puntos del biplot.
  #   ind.alpha.pon - Transparencia de los puntos del biplot.
  #   varnames - Etiquetas de las variables.
  #   var.col - Color de las flechas del biplot.
  #   var.size - Tamaño de letra de las etiquetas de las variables.
  #   arr.col - Color de la flecha y de la cabeza de la flecha.
  #   arr.size - Tamaño de la cabeza de la flecha y el segmento que la define.
  #   main - Título del biplot.
  #   ind.labs - Etiquetas de los atributos. Por default se toman los nombres de
  #         fila de la base.
  #   x.lim, y.lim - Límites horizontal y vertical del gráfico.
  #  
  # Output: Biplot con las características deseadas.
  #
  cargar_paquetes(c('grid', 'ggrepel'))
  
  dat.ind <- data.frame(obsnames=row.names(PC$ind$coor), PC$ind$coor[,axes])
  cp.names <- paste0("Comp.",axes)
  names(dat.ind)[2:3] <- cp.names
  
  if(!is.null(x.lim)){
    limx <- xlim(x.lim[1], x.lim[2])
  } else
    limx <- NULL
  
  if(!is.null(y.lim)){
    limy <- ylim(y.lim[1], y.lim[2])
  } else
    limy <- NULL
  
  # Base del gráfico
  if(calidad_rep == T & pts == T){
    calidad <- PC$ind$cos2[,axes] %>% 
      data.frame() %>% 
      rownames_to_column() %>% 
      mutate(repre = rowSums(.[2:3])) %>% 
      dplyr::select(obsnames = rowname, repre)
    
    dat.ind <- suppressWarnings(suppressMessages(dat.ind %>% 
                                                   left_join(calidad)))
    
    if(is.null(ind.labs)) ind.labs <- dat.ind$obsnames
    
    g <- ggplot(dat.ind, 
                aes_string(x = cp.names[1], y = cp.names[2],
                           colour = "repre")) + 
      theme_bw() +
      limx +
      limy +
      geom_point(alpha = ind.alpha.pon, size = 3) +
      geom_text_repel(size = ind.size.lab, label = ind.labs, colour = ind.lab.col, 
                      segment.color = ind.point.col) +
      scale_colour_continuous(labels = scales::percent, high = "midnightblue", 
                              low = 'skyblue') +
      guides(colour = guide_colorbar(title = 'Calidad de \nrepresentación'))
  } else{
    g <- ggplot(dat.ind, 
                aes_string(x = cp.names[1], y = cp.names[2])) + 
      theme_bw() +
      limx +
      limy
  }
  
  # Vectores (flechas)
  if(is.null(varnames)) varnames <- rownames(PC$var$coord)
  dat.pc <- data.frame(varnames=varnames, PC$var$coord[,axes])
  names(dat.pc)[2:3]<- cp.names
  
  mult <- min(
    (max(dat.ind[,3]) - min(dat.ind[,3])/(max(dat.pc[,3])-min(dat.pc[,3]))),
    (max(dat.ind[,2]) - min(dat.ind[,2])/(max(dat.pc[,2])-min(dat.pc[,2])))
  )
  dat.pc2 <- transform(dat.pc,
                       v1 = ms * mult * (get(cp.names[1])),
                       v2 = ms * mult * (get(cp.names[2]))
  )
  
  g <- g + 
    geom_segment(data = dat.pc2, aes(x=0, y=0, xend=v1, yend=v2), 
                 arrow=arrow(length=unit(arr.size[1],"cm")), 
                 alpha=0.7, color=arr.col[1], size = arr.size[2])
  
  g <- g + coord_equal() + 
    geom_text(data = dat.pc2, aes(x=v1, y=v2, label=varnames), size = var.size,
              color=var.col, vjust = 'outward', hjust = 'outward') +
    scale_x_continuous(expand = c(0.2, 0.2))
  
  porc_var_exp <- (PC$eig %>% 
                     setNames(c('eigenvalue', 'porc_var_explicada', 'porc_var_acumulada')) %>% 
                     dplyr::select(porc_var_explicada))[axes,]
  
  lab.names1 <- paste(cp.names, "\n(", round(porc_var_exp, 2), "%)", sep = "")
  
  # Puntos (atributos)
  if(pts == TRUE & calidad_rep == F){
    if(ind.display == "nams"){
      if(is.null(ind.labs)) ind.labs <- dat.ind$obsnames
      g <- g +
        geom_text_repel(size=ind.size.lab, label=ind.labs, colour=ind.lab.col) 
    }
    if(ind.display == "points"){
      g <- g +
        geom_point(alpha = ind.alpha.pon, size=ind.size.pon, colour=ind.point.col)
    }
    if(ind.display == "both"){
      if(is.null(ind.labs)) ind.labs <- dat.ind$obsnames
      g <- g +
        geom_point(alpha = ind.alpha.pon, size=ind.size.pon) +
        geom_text_repel(size=ind.size.lab, label=ind.labs, colour=ind.lab.col, 
                        segment.color = ind.point.col)   
    }
  }
  
  if(format == 'analysis'){  
    g <- g + 
      xlab(lab.names1[1]) +
      ylab(lab.names1[2]) +
      geom_hline(aes(yintercept = 0), size=.2, color = "black") + 
      geom_vline(aes(xintercept = 0), size=.2, color="black") +
      ggtitle(main, subtitle = paste("Porcentaje de varianza explicada = ",
                                     round(sum(porc_var_exp), 2), "%", sep = "" ))
  }else
    if(format == 'report'){
      g <- g +      theme(axis.line=element_blank(),
                          axis.text.x=element_blank(),
                          axis.text.y=element_blank(),
                          axis.ticks=element_blank(),
                          axis.title.x=element_blank(),
                          axis.title.y=element_blank(),
                          legend.position="none",
                          panel.background=element_blank(),
                          panel.border=element_blank(),
                          panel.grid.major=element_blank(),
                          panel.grid.minor=element_blank(),
                          plot.background=element_blank())
    }
  g
}