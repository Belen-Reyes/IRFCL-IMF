# --------------------------------- Funciones ----------------------------------
# Funciones de consulta y gráfica de datos IMF-IRCL

# install.packages(c('dplyr', 'plotly', 'hrbrthemes', 'zoo', 'ggplot2', 'formattable', 'tidyr'))
library(dplyr)
library(plotly)
library(hrbrthemes)
library(zoo)
library(ggplot2)
library(formattable)
library(tidyr)

load('Total.Rda')
load('Indicadores.Rda')
load('Codigos.Rda')
load('Etiquetas.Rda')
load('Paises.Rda')

# Función aux que contabiliza el número de meses entre dos fechas
# Formato dif_mes('YYYY-MM','YYYY-MM')

dif_mes <- function(fecha_inicio, fecha_fin){
  
  f1 <- gsub(" ", "", paste (fecha_inicio,"-01"))
  f2 <- gsub(" ", "", paste (fecha_fin,"-01"))
  
  m1 <- as.yearmon(strptime(f1, format = '%Y-%m-%d'))
  m2 <- as.yearmon(strptime(f2, format = '%Y-%m-%d'))
  
  dif_mes <- (m2-m1)*12+1
  return(dif_mes)
}


# Función auxiliar para buscar los códigos correspondientes
# Formato buscar('string')

buscar <- function(x){

  aux_1 <- Code_C[grep(x, Code_C$description, ignore.case = TRUE),]
  aux_2 <- Code_I[grep(x, Code_I$description, ignore.case = TRUE),]
  
  if(!identical(aux_1[,1], character(0))){
    return(aux_1[,1:2])
  }else{
    if(!identical(aux_2[,1], character(0))){
      return(aux_2[,1:2])
    }else{
      print('No se encontraron coincidencias con la búsqueda')
    }}
}


# Función que arroja el total de un indicador por país
# Formato consulta('CODE','INDICATOR','YYYY-MM','YYYY-MM')

consulta <- function(pais, indicador, fecha_inicio, fecha_fin){
  
  meses <- dif_mes(fecha_inicio,fecha_fin) 
  x <- c()
  
  for (i in 1:meses){
    x[i] <- TOTAL[(which(TOTAL$iso2c == pais & TOTAL$year_month == fecha_inicio))
                  +(i-1),indicador]
  }
  consulta <- sum(x)
  return(consulta)
}


# Funciones axuliares para graficar 
buscar_inv <- function(x){
  
  aux_1 <- Code_C[grep(x, Code_C$x, ignore.case = TRUE),]
  aux_2 <- Code_I[grep(x, Code_I$x, ignore.case = TRUE),]
  
  if(!identical(aux_1[,1], character(0))){
    return(aux_1[,2])
  }else{
    if(!identical(aux_2[,1], character(0))){
      return(aux_2[,2])
    }}
}

wrapper <- function(x, ...){
  paste(strwrap(x, ...), collapse = "\n")
}

consulta_p <- function(pais, indicador, fecha_inicio, fecha_fin){
  
  meses <- dif_mes(fecha_inicio,fecha_fin) 
  x <- c()
  
  for (i in 1:meses){
    x[i] <- TOTAL_P[(which(TOTAL_P$iso2c == pais & TOTAL_P$year_month == 
                             fecha_inicio))+(i-1),indicador]
  }
  consulta_p <- sum(x)
  return(consulta_p)
}



# Función para graficar la serie de tiempo de un indicador para un país
# Formato grafica('INDICADOR','PAIS')
graf_uno <- function(indicador, pais){
  
  aux <- gsub(" ", "", paste(TOTAL[(which(TOTAL$iso2c == pais)),'year_month'],"-01"))
  date <- as.Date(aux)
  
  df <- data.frame(x = date, y = TOTAL[(which(TOTAL$iso2c == pais)), indicador])
  
  b_1 <- buscar_inv(indicador)
  b_2 <- buscar_inv(pais)
  title <- paste(b_1,',' ,b_2)
  
  plot <- ggplot() + 
    geom_area(data = df, aes(x=x, y=y, fill= b_2), alpha=0.4 , size=.8, 
              colour="salmon2", outline.type = "upper")+
    labs(y = 'USD', x = 'Año') + 
    ggtitle(wrapper(title, width = 85)) + theme_minimal() +
    theme(plot.title = element_text(size= 9, hjust = 0.5))+
    scale_fill_discrete(name="Pais")
  
  ggplotly(plot)
  
}

# Funcion aux para las graficas posteriores
graf_aux <- function(indicador, paises, tipo){
  
  if(tipo== 'total'){
    aux_1 <- gsub(" ", "", paste(TOTAL[(which(TOTAL$iso2c == paises[1])),
                                       'year_month'],"-01"))
    aux_2 <- gsub(" ", "", paste(TOTAL[(which(TOTAL$iso2c == paises[2])),
                                       'year_month'],"-01"))
    aux_3 <- gsub(" ", "", paste(TOTAL[(which(TOTAL$iso2c == paises[3])),
                                       'year_month'],"-01"))
    
    date_1 <- as.Date(aux_1)
    date_2 <- as.Date(aux_2)
    date_3 <- as.Date(aux_3)
    
    df_1 <- data.frame(x_1 = date_1, y_1 = TOTAL[(which(TOTAL$iso2c == 
                                                          paises[1])), indicador])
    df_2 <- data.frame(x_2 = date_2, y_2 = TOTAL[(which(TOTAL$iso2c == 
                                                          paises[2])), indicador])
    df_3 <- data.frame(x_3 = date_3, y_3 = TOTAL[(which(TOTAL$iso2c == 
                                                          paises[3])), indicador])
    
    y_l = 'USD'
    
  }else{
    if(tipo== 'porcentaje'){
      aux_1 <- gsub(" ", "", paste(TOTAL_P[(which(TOTAL_P$iso2c == 
                                                    paises[1])),'year_month'],"-01"))
      aux_2 <- gsub(" ", "", paste(TOTAL_P[(which(TOTAL_P$iso2c == 
                                                    paises[2])),'year_month'],"-01"))
      aux_3 <- gsub(" ", "", paste(TOTAL_P[(which(TOTAL_P$iso2c == 
                                                    paises[3])),'year_month'],"-01"))
      
      date_1 <- as.Date(aux_1)
      date_2 <- as.Date(aux_2)
      date_3 <- as.Date(aux_3)
      
      df_1 <- data.frame(x_1 = date_1, y_1 = TOTAL_P[(which(TOTAL_P$iso2c == 
                                                              paises[1])), indicador])
      df_2 <- data.frame(x_2 = date_2, y_2 = TOTAL_P[(which(TOTAL_P$iso2c == 
                                                              paises[2])), indicador])
      df_3 <- data.frame(x_3 = date_3, y_3 = TOTAL_P[(which(TOTAL_P$iso2c == 
                                                              paises[3])), indicador])
      
      y_l <- 'Percentage'
      
    }else{
      print("Ingresa un tipo de dato válido: 'total', 'porcentaje'")
    }}
  
  title <- buscar_inv(indicador)
  values <- list(df_1, df_2, df_3,y_l, title)
  return(values)
}


# Función para graficar las serie de tiempo de un indicador para tres países
# Formato grafica_tres('INDICADOR','PAIS')
graf_tres <- function (indicador, paises, tipo){
  
  values <- graf_aux(indicador, paises, tipo)
  
  plot <- ggplot() +
    geom_area(data = values[[1]], aes(x=x_1, y=y_1, fill= buscar_inv(paises[1])), 
              alpha=0.3 , size=.8, 
              colour="white", outline.type = "upper")+
    geom_area(data = values[[2]], aes(x=x_2, y=y_2, fill= buscar_inv(paises[2])), 
              alpha=0.4 , size=.8, 
              colour="white", outline.type = "upper")+
    geom_area(data = values[[3]], aes(x=x_3, y=y_3, fill= buscar_inv(paises[3])), 
              alpha=0.5 , size=.8, 
              colour="white", outline.type = "upper")+
    scale_fill_brewer(type = 'seq', palette = 16, name = 'Countries')+
    labs(y = values[[4]], x = 'Year')+
    ggtitle(wrapper(values[[5]], width = 85)) +  theme_minimal() +
    theme(plot.title = element_text(size= 9, hjust = 0.5)) 
  
  ggplotly(plot)
}


# Funciones aux para las gráficas, min, max
aux_fechas <- function(pais){
  n <- length(TOTAL[TOTAL$iso2c == pais ,2])
  fechas <- c(fecha_i = TOTAL[TOTAL$iso2c == pais,2][1], 
              fecha_f = TOTAL[TOTAL$iso2c == pais,2][n])
  return(fechas)
}


count_aux <- function (indicador, tipo){
  x <- data.frame()
  
  for (i in 1:length(COUNTRIES)){
    
    fecha <- aux_fechas(COUNTRIES[i])
    x[i,1] <- COUNTRIES[i]
    
    if(tipo == "total"){
      x[i,2] <- consulta(COUNTRIES[i], indicador, fecha[1], 
                         fecha[2])/dif_mes(fecha[1], fecha[2])
    }else{
      if(tipo == "porcentaje"){
        x[i,2] <- consulta_p(COUNTRIES[i], indicador, fecha[1], 
                             fecha[2])/dif_mes(fecha[1], fecha[2])
      }
    }
  }
  return(x)
}

# Función para graficar los tres países con máximos valores en un determinado indicador
# Formato graf_max('INDICADOR', 'tipo: total, porcentaje')

graf_max <- function(indicador, tipo){
  
  df <- count_aux(indicador,tipo)
  df <- na.omit(df)
  
  for (i in 1:3){
    assign(gsub(' ', '', paste('max_',i)), df[df$V2 == max(df[,2]),])
    df <- df[!(df$V2 == max(df[,2])),]
  }
  max <- rbind(max_1, max_2, max_3[1,])
  
  paises <- max$V1
  
  values <- graf_aux(indicador, paises, tipo)
  
  plot <- ggplot() +
    geom_area(data = values[[1]], aes(x=x_1, y=y_1, fill= buscar_inv(paises[1])), 
              alpha=0.8 , size=.8, 
              colour="white", outline.type = "upper")+
    geom_area(data = values[[2]], aes(x=x_2, y=y_2, fill= buscar_inv(paises[2])), 
              alpha=0.3 , size=.8, 
              colour="white", outline.type = "upper")+
    geom_area(data = values[[3]], aes(x=x_3, y=y_3, fill= buscar_inv(paises[3])), 
              alpha=0.5 , size=.8, 
              colour="white", outline.type = "upper")+
    scale_fill_brewer(type = 'seq', palette = 14, name = 'Countries')+
    labs(y = values[[4]], x = 'Year')+
    ggtitle(wrapper(values[[5]], width = 85)) +  theme_minimal() +
    theme(plot.title = element_text(size= 9, hjust = 0.5)) 
  
  ggplotly(plot)
}

# Función para graficar los tres países con mínimos valores en un determinado indicador
# Formato graf_min('INDICADOR', 'tipo: total, porcentaje')

graf_min <- function(indicador, tipo){
  
  df <- count_aux(indicador,tipo)
  df <- na.omit(df)
  
  for (i in 1:3){
    assign(gsub(' ', '', paste('min_',i)), df[df$V2 == min(df[,2]),])
    df <- df[!(df$V2 == min(df[,2])),]
  }
  min <- rbind(min_3[1,], min_2, min_1)
  
  paises <- min$V1
  
  values <- graf_aux(indicador, paises, tipo)
  
  plot <- ggplot() +
    geom_area(data = values[[1]], aes(x=x_1, y=y_1, fill= buscar_inv(paises[1])), 
              alpha=0.25 , size=.8, 
              colour="white", outline.type = "upper")+
    geom_area(data = values[[2]], aes(x=x_2, y=y_2, fill= buscar_inv(paises[2])), 
              alpha=0.75 , size=.8, 
              colour="white", outline.type = "upper")+
    geom_area(data = values[[3]], aes(x=x_3, y=y_3, fill= buscar_inv(paises[3])), 
              alpha=0.6 , size=.8, 
              colour="white", outline.type = "upper")+
    scale_fill_brewer(type = 'seq', palette = 10, name = 'Countries')+
    labs(y = values[[4]], x = 'Year')+
    ggtitle(wrapper(values[[5]], width = 85)) +  theme_minimal() +
    theme(plot.title = element_text(size= 9, hjust = 0.5)) 
  
  ggplotly(plot)
}

# Función para tabular los cinco países con máximos valores en un determinado indicador
# Formato tabla_max('INDICADOR', 'tipo: total, porcentaje')

tabla_max <- function(indicador,tipo){
  
  df <- count_aux(indicador,tipo)
  df <- na.omit(df)
  
  if(length(df$V2)>=5){
    
    for (i in 1:5){
      assign(gsub(' ', '', paste('max_',i)), df[df$V2 == max(df[,2]),])
      df <- df[!(df$V2 == max(df[,2])),]
    }
    max <- rbind(max_1, max_2, max_3, max_4, max_5[1,])
    paises <- max$V1
    
    df_aux <- count_aux(indicador,tipo)
    df_aux <- na.omit(df_aux)
    Country <- c()
    Total <- c()
    df_1 <- count_aux(indicador,'total')
    df_1 <- na.omit(df_1)
    
    for (i in 1:5) {
      Country[i] <- buscar_inv(paises[i])
      Total[i] <- df_1[which(df_1$V1 == paises[i]),2]
      if (tipo == 'total'){
        assign(paste('x_',i,sep=''), summary(TOTAL[which(TOTAL$iso2c == paises[i]),
                                                   indicador]))
      }else{
        if(tipo=='porcentaje'){
          assign(paste('x_',i,sep=''), summary(TOTAL_P[which(TOTAL_P$iso2c == paises[i]),
                                                       indicador]))
        }
      }
    }
    
    stat <- as.data.frame(rbind(x_1,x_2,x_3,x_4,x_5))
    is.num <- sapply(stat, is.numeric)
    stat[is.num] <- lapply(stat[is.num], round, 6)
    
    table_max <- data.frame(Country, 'USD Monthly Av' = Total, Min = stat$Min., 
                            Median = stat$Median, Mean = stat$Mean, 
                            Max = stat$Max.)
    
    formattable(table_max, align = c('l','c','c','c','c','c'), list(
      'Country' = formatter("span"),
      'USD.Monthly.Av' = color_tile('#ffd9cf','#ff9579'),
      area(col = 3:6) ~ color_tile("#efefef", "#bcbcbc")
    ))
  }else{
    print('No hay suficientes países con datos para mostrar')
  }
}


# Función para tabular los cinco países con mínimos valores en un determinado indicador
# Formato tabla_min('INDICADOR', 'tipo: total, porcentaje')
tabla_min <- function(indicador,tipo){
  
  df <- count_aux(indicador,tipo)
  df <- na.omit(df)
  
  if(length(df$V2)>=5){
    
    for (i in 1:5){
      assign(gsub(' ', '', paste('min_',i)), df[df$V2 == min(df[,2]),])
      df <- df[!(df$V2 == min(df[,2])),]
    }
    min <- rbind(min_1, min_2, min_3, min_4, min_5[1,])
    paises <- min$V1
    
    df_aux <- count_aux(indicador,tipo)
    df_aux <- na.omit(df_aux)
    Country <- c()
    Total <- c()
    df_1 <- count_aux(indicador,'total')
    df_1 <- na.omit(df_1)
    
    for (i in 1:5) {
      Country[i] <- buscar_inv(paises[i])
      Total[i] <- df_1[which(df_1$V1 == paises[i]),2]
      if (tipo == 'total'){
        assign(paste('x_',i,sep=''), summary(TOTAL[which(TOTAL$iso2c == paises[i]),
                                                   indicador]))
      }else{
        if(tipo=='porcentaje'){
          assign(paste('x_',i,sep=''), summary(TOTAL_P[
            which(TOTAL_P$iso2c == paises[i]), indicador]))
        }
      }
    }
    
    stat <- as.data.frame(rbind(x_1,x_2,x_3,x_4,x_5))
    is.num <- sapply(stat, is.numeric)
    stat[is.num] <- lapply(stat[is.num], round, 6)
    
    table_min <- data.frame(Country, 'USD Monthly Av' = Total, Min = stat$Min., 
                            Median = stat$Median, Mean = stat$Mean, 
                            Max = stat$Max.)

    
    formattable(table_min, align = c('l','c','c','c','c','c'), list(
      'Country' = formatter("span"),
      'USD.Monthly.Av' = color_tile('#bce58e','#73ac31'),
      area(col = 3:6) ~ color_tile("#efefef", "#bcbcbc")
    ))
  }else{
    print('No hay suficientes países con datos para mostrar')
  }
}


# Función para tabular todos los países respecto a un indicador
# Formato tabla_total('INDICADOR', 'tipo: total, porcentaje')
tabla_total <- function(indicador, tipo){
  
  df <- count_aux(indicador,tipo)
  df <- na.omit(df)
  df_1 <- count_aux(indicador,'total')
  df_1 <- na.omit(df_1)
  aux <- sort(df_1$V2, decreasing = T)
  options(scipen = 999)
  
  for (i in 1:length(df$V2)){
    b <- df[df$V2 == max(df[,2]),]
    df <- df[!(df$V2 == max(df[,2])),]
    if(i==1){
      table <- b
    }else{
      table <- rbind(table,b)
    }
  }
  paises <- table$V1
  Country <- c()
  
  for(i in 1:length(paises)){
    Country[i] <- buscar_inv(paises[i])
    if(tipo =='total'){
      sum <- summary(TOTAL[which(TOTAL$iso2c == paises[i]), indicador])
    }else{
      if(tipo=='porcentaje'){
        sum <- summary(TOTAL_P[which(TOTAL_P$iso2c == paises[i]), indicador])
      }
    }
    
    if(i ==1){
      stat <- sum
    }else{
      stat <- rbind(stat,sum)
    }
  }
  
  stat <- as.data.frame(stat)
  is.num <- sapply(stat, is.numeric)
  stat[is.num] <- lapply(stat[is.num], round, 6)
  
  table <- data.frame(Country, 'USD Monthly Av' = aux, Min = stat$Min., 
                      Median = stat$Median, Mean = stat$Mean, 
                      Max = stat$Max.)
  
  formattable(table, align = c('l','c','c','c','c','c'), list(
    'Country' = formatter("span"),
    'USD.Monthly.Av' = color_tile('#c4f0f2','#40c1c8'),
    area(col = 3:6) ~ color_tile("white", "#bcbcbc")
  ))
}



