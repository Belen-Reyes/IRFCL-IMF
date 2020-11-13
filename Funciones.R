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
library(RColorBrewer)
library(reactable)
library(crosstalk)
library(lubridate)
library(stringr)
library(purrr)

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
  
  dif_mes <- as.integer((m2-m1)*12+1)+1
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
graf_uno <- function(indicador, pais, tipo){
  
  if(tipo == 'total'){
    aux <- paste(TOTAL[(which(TOTAL$iso2c == pais)),'year_month'],"-01", sep = '')
    date <- as.Date(aux)
    df <- data.frame(x = date, y = TOTAL[(which(TOTAL$iso2c == pais)), indicador])
  }else{
    if(tipo=='porcentaje'){
      aux <- paste(TOTAL_P[(which(TOTAL_P$iso2c == pais)),'year_month'],"-01", sep = '')
      date <- as.Date(aux)
      df <- data.frame(x = date, y = TOTAL_P[(which(TOTAL_P$iso2c == pais)), indicador])
    }
  }
  
  b_1 <- buscar_inv(indicador)
  b_2 <- buscar_inv(pais)
  title <- paste(b_1,',' ,b_2)
  
  if(tipo == 'total'){
    plot <- ggplot() + 
      geom_line(data = df, aes(x=x, y=y), colour = '#DDBB75', size=.9)+
      labs(y = 'USD', x = 'Año') + 
      scale_y_continuous(labels = scales::comma)+
      ggtitle(wrapper(title, width = 85)) +  theme_minimal() +
      theme(plot.title = element_text(size= 9, hjust = 0.5)) 
    
    ggplotly(plot)
  }else{
    if(tipo == 'porcentaje'){
      plot <- ggplot() + 
        geom_line(data = df, aes(x=x, y=y), colour = '#DDBB75', size=.9)+
        labs(y = 'USD', x = 'Año') + 
        scale_y_continuous(labels = scales::percent)+
        ggtitle(wrapper(title, width = 85)) +  theme_minimal() +
        theme(plot.title = element_text(size= 9, hjust = 0.5)) 
      
      ggplotly(plot)
    }
  }
}

# Funcion aux para las graficas posteriores
graf_aux <- function(indicador, paises, tipo){
  
  if(tipo== 'total'){
    aux_1 <- paste(TOTAL[(which(TOTAL$iso2c == paises[1])),'year_month'],"-01",sep = '')
    aux_2 <- paste(TOTAL[(which(TOTAL$iso2c == paises[2])),'year_month'],"-01",sep = '')
    aux_3 <- paste(TOTAL[(which(TOTAL$iso2c == paises[3])),'year_month'],"-01",sep = '')
    aux_4 <- paste(TOTAL[(which(TOTAL$iso2c == paises[4])),'year_month'],"-01",sep = '')
    aux_5 <- paste(TOTAL[(which(TOTAL$iso2c == paises[5])),'year_month'],"-01",sep = '')
    
    date_1 <- as.Date(aux_1)
    date_2 <- as.Date(aux_2)
    date_3 <- as.Date(aux_3)
    date_4 <- as.Date(aux_4)
    date_5 <- as.Date(aux_5)
    
    
    df_1 <- data.frame(x_1 = date_1, y_1 = TOTAL[(which(TOTAL$iso2c == 
                                                          paises[1])), indicador])
    df_2 <- data.frame(x_2 = date_2, y_2 = TOTAL[(which(TOTAL$iso2c == 
                                                          paises[2])), indicador])
    df_3 <- data.frame(x_3 = date_3, y_3 = TOTAL[(which(TOTAL$iso2c == 
                                                          paises[3])), indicador])
    df_4 <- data.frame(x_4 = date_4, y_4 = TOTAL[(which(TOTAL$iso2c == 
                                                          paises[4])), indicador])
    df_5 <- data.frame(x_5 = date_5, y_5 = TOTAL[(which(TOTAL$iso2c == 
                                                          paises[5])), indicador])
    if(indicador == 'RAFAGOLDV_OZT'){
      y_l = 'Ounces'
    }else{
      y_l = 'USD'
    }
    
  }else{
    if(tipo == 'porcentaje'){
      aux_1 <- paste(TOTAL_P[(which(TOTAL_P$iso2c == paises[1])),'year_month'],"-01",sep = '')
      aux_2 <- paste(TOTAL_P[(which(TOTAL_P$iso2c == paises[2])),'year_month'],"-01",sep = '')
      aux_3 <- paste(TOTAL_P[(which(TOTAL_P$iso2c == paises[3])),'year_month'],"-01",sep = '')
      aux_4 <- paste(TOTAL_P[(which(TOTAL_P$iso2c == paises[4])),'year_month'],"-01",sep = '')
      aux_5 <- paste(TOTAL_P[(which(TOTAL_P$iso2c == paises[5])),'year_month'],"-01",sep = '')
      
      date_1 <- as.Date(aux_1)
      date_2 <- as.Date(aux_2)
      date_3 <- as.Date(aux_3)
      date_4 <- as.Date(aux_4)
      date_5 <- as.Date(aux_5)
      
      df_1 <- data.frame(x_1 = date_1, y_1 = TOTAL_P[(which(TOTAL_P$iso2c == 
                                                              paises[1])), indicador])
      df_2 <- data.frame(x_2 = date_2, y_2 = TOTAL_P[(which(TOTAL_P$iso2c == 
                                                              paises[2])), indicador])
      df_3 <- data.frame(x_3 = date_3, y_3 = TOTAL_P[(which(TOTAL_P$iso2c == 
                                                              paises[3])), indicador])
      df_4 <- data.frame(x_4 = date_4, y_4 = TOTAL_P[(which(TOTAL_P$iso2c == 
                                                            paises[4])), indicador])
      df_5 <- data.frame(x_5 = date_5, y_5 = TOTAL_P[(which(TOTAL_P$iso2c == 
                                                            paises[5])), indicador])
      
      y_l <- 'Percentage'
      
    }else{
      print("Ingresa un tipo de dato válido: 'total', 'porcentaje'")
    }}
  
  title <- buscar_inv(indicador)
  values <- list(df_1, df_2, df_3,df_4,df_5,y_l, title)
  return(values)
}


# Función para graficar las serie de tiempo de un indicador para cinco países
# Formato grafica_5('INDICADOR','PAIS')
graf_5 <- function (indicador, paises, tipo){
  
  values <- graf_aux(indicador, paises, tipo)
  
  if(tipo == 'total'){
    plot <- ggplot() +
      geom_line(data = values[[1]], aes(x=x_1, y=y_1, color = buscar_inv(paises[1])), size=.7)+
      geom_line(data = values[[2]], aes(x=x_2, y=y_2, color = buscar_inv(paises[2])), size=.7)+
      geom_line(data = values[[3]], aes(x=x_3, y=y_3, color = buscar_inv(paises[3])), size=.7)+
      geom_line(data = values[[4]], aes(x=x_4, y=y_4, color = buscar_inv(paises[4])), size=.7)+
      geom_line(data = values[[5]], aes(x=x_5, y=y_5, color = buscar_inv(paises[5])), size=.75)+
      scale_color_brewer(type = 'qual', palette = 4, name = 'Countries')+
      scale_y_continuous(labels = scales::comma)+
      labs(y = values[[6]], x = 'Year')+
      ggtitle(wrapper(values[[7]], width = 85)) +  theme_minimal() +
      theme(plot.title = element_text(size= 9, hjust = 0.5)) 
    
    ggplotly(plot)
  }else{
    if(tipo == 'porcentaje'){
      plot <- ggplot() +
        geom_line(data = values[[1]], aes(x=x_1, y=y_1, color = buscar_inv(paises[1])), size=.7)+
        geom_line(data = values[[2]], aes(x=x_2, y=y_2, color = buscar_inv(paises[2])), size=.7)+
        geom_line(data = values[[3]], aes(x=x_3, y=y_3, color = buscar_inv(paises[3])), size=.7)+
        geom_line(data = values[[4]], aes(x=x_4, y=y_4, color = buscar_inv(paises[4])), size=.7)+
        geom_line(data = values[[5]], aes(x=x_5, y=y_5, color = buscar_inv(paises[5])), size=.7)+
        scale_color_brewer(type = 'qual', palette = 4, name = 'Countries')+
        scale_y_continuous(labels = scales::percent)+
        labs(y = values[[6]], x = 'Year')+
        ggtitle(wrapper(values[[7]], width = 85)) +  theme_minimal() +
        theme(plot.title = element_text(size= 9, hjust = 0.5)) 
      
      ggplotly(plot)
    }
  }
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
  
  for (i in 1:5){
    assign(paste('max_',i,sep = ''), df[df$V2 == max(df[,2]),])
    df <- df[!(df$V2 == max(df[,2])),]
  }
  max <- rbind(max_1, max_2, max_3, max_4, max_5[1,])
  
  paises <- max$V1
  
  values <- graf_aux(indicador, paises, tipo)
  
  if(tipo == 'total'){
    plot <- ggplot() +
      geom_line(data = values[[1]], aes(x=x_1, y=y_1, color = buscar_inv(paises[1])), size=.7)+
      geom_line(data = values[[2]], aes(x=x_2, y=y_2, color = buscar_inv(paises[2])), size=.7)+
      geom_line(data = values[[3]], aes(x=x_3, y=y_3, color = buscar_inv(paises[3])), size=.7)+
      geom_line(data = values[[4]], aes(x=x_4, y=y_4, color = buscar_inv(paises[4])), size=.7)+
      geom_line(data = values[[5]], aes(x=x_5, y=y_5, color = buscar_inv(paises[5])), size=.7)+
      scale_color_brewer(type = 'seq', palette = 4, direction = -1, name = 'Countries')+
      scale_y_continuous(labels = scales::comma)+
      labs(y = values[[6]], x = 'Year')+
      ggtitle(wrapper(values[[7]], width = 85)) +  theme_minimal() +
      theme(plot.title = element_text(size= 9, hjust = 0.5)) 
    
    ggplotly(plot)
  }else{
    if(tipo == 'porcentaje'){
      plot <- ggplot() +
        geom_line(data = values[[1]], aes(x=x_1, y=y_1, color = buscar_inv(paises[1])), size=.7)+
        geom_line(data = values[[2]], aes(x=x_2, y=y_2, color = buscar_inv(paises[2])), size=.7)+
        geom_line(data = values[[3]], aes(x=x_3, y=y_3, color = buscar_inv(paises[3])), size=.7)+
        geom_line(data = values[[4]], aes(x=x_4, y=y_4, color = buscar_inv(paises[4])), size=.7)+
        geom_line(data = values[[5]], aes(x=x_5, y=y_5, color = buscar_inv(paises[5])), size=.7)+
        scale_color_brewer(type = 'seq', palette = 4, direction = -1, name = 'Countries')+
        scale_y_continuous(labels = scales::percent)+
        labs(y = values[[6]], x = 'Year')+
        ggtitle(wrapper(values[[7]], width = 85)) +  theme_minimal() +
        theme(plot.title = element_text(size= 9, hjust = 0.5)) 
      
      ggplotly(plot)
    }
  }
}

# Función para graficar los cinco países con mínimos valores en un determinado indicador
# Formato graf_min('INDICADOR', 'tipo: total, porcentaje')

graf_min <- function(indicador, tipo){
  
  df <- count_aux(indicador,tipo)
  df <- na.omit(df)
  
  for (i in 1:5){
    assign(gsub(' ', '', paste('min_',i)), df[df$V2 == min(df[,2]),])
    df <- df[!(df$V2 == min(df[,2])),]
  }
  min <- rbind(min_5[1,], min_4, min_3, min_2, min_1)
  
  paises <- min$V1
  
  values <- graf_aux(indicador, paises, tipo)
  
  if(tipo == 'total'){
    plot <- ggplot() +
      geom_line(data = values[[1]], aes(x=x_1, y=y_1, color = buscar_inv(paises[1])), size=.7)+
      geom_line(data = values[[2]], aes(x=x_2, y=y_2, color = buscar_inv(paises[2])), size=.7)+
      geom_line(data = values[[3]], aes(x=x_3, y=y_3, color = buscar_inv(paises[3])), size=.7)+
      geom_line(data = values[[4]], aes(x=x_4, y=y_4, color = buscar_inv(paises[4])), size=.7)+
      geom_line(data = values[[5]], aes(x=x_5, y=y_5, color = buscar_inv(paises[5])), size=.7)+
      scale_color_brewer(type = 'seq', palette = 7, direction = -1, name = 'Countries')+
      scale_y_continuous(labels = scales::comma)+
      labs(y = values[[6]], x = 'Year')+
      ggtitle(wrapper(values[[7]], width = 85)) +  theme_minimal() +
      theme(plot.title = element_text(size= 9, hjust = 0.5)) 
    
    ggplotly(plot)
  }else{
    if(tipo == 'porcentaje'){
      plot <- ggplot() +
        geom_line(data = values[[1]], aes(x=x_1, y=y_1, color = buscar_inv(paises[1])), size=.7)+
        geom_line(data = values[[2]], aes(x=x_2, y=y_2, color = buscar_inv(paises[2])), size=.7)+
        geom_line(data = values[[3]], aes(x=x_3, y=y_3, color = buscar_inv(paises[3])), size=.7)+
        geom_line(data = values[[4]], aes(x=x_4, y=y_4, color = buscar_inv(paises[4])), size=.7)+
        geom_line(data = values[[5]], aes(x=x_5, y=y_5, color = buscar_inv(paises[5])), size=.7)+
        scale_color_brewer(type = 'seq', palette = 7, direction = -1, name = 'Countries')+
        scale_y_continuous(labels = scales::percent)+
        labs(y = values[[6]], x = 'Year')+
        ggtitle(wrapper(values[[7]], width = 85)) +  theme_minimal() +
        theme(plot.title = element_text(size= 9, hjust = 0.5)) 
      
      ggplotly(plot)
    }
  }
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
    Total <- round(Total, digits = 2)
    
    if(tipo == 'total'){
      is.num <- sapply(stat, is.numeric)
      stat[is.num] <- lapply(stat[is.num], round, 2)
      stat[is.num] <- lapply(stat[is.num], formattable::comma,1)
      
      table_max <- data.frame(Country, 'USD Monthly Av' = Total, 
                              Min = stat$Min., 
                              Median = stat$Median, 
                              Mean = stat$Mean, 
                              Max = stat$Max.)
    }else{
      if(tipo == 'porcentaje'){
        is.num <- sapply(stat, is.numeric)
        stat[is.num] <- lapply(stat[is.num], round, 4)
        stat[is.num] <- lapply(stat[is.num], formattable::percent)
        
        table_max <- data.frame(Country, 'USD Monthly Av' = Total, 
                                Min = stat$Min.,
                                Median = stat$Median, 
                                Mean = stat$Mean, 
                                Max = stat$Max.)
      }
    }
    
    formattable(table_max, align = c('l','c','c','c','c','c'), list(
      'Country' = formatter("span"),
      area(col=2,)~ color_tile("#F0F9E8",'#43A2CA')))
    
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
    Total <- round(Total, digits = 2)
    
    if(tipo == 'total'){
      is.num <- sapply(stat, is.numeric)
      stat[is.num] <- lapply(stat[is.num], round, 2)
      stat[is.num] <- lapply(stat[is.num], formattable::comma,1)
      
      table_min <- data.frame(Country, 'USD Monthly Av' = Total, 
                              Min = stat$Min., 
                              Median = stat$Median, 
                              Mean = stat$Mean, 
                              Max = stat$Max.)
    }else{
      if(tipo == 'porcentaje'){
        is.num <- sapply(stat, is.numeric)
        stat[is.num] <- lapply(stat[is.num], round, 4)
        stat[is.num] <- lapply(stat[is.num], formattable::percent)
        
        table_min <- data.frame(Country, 'USD Monthly Av' = Total, 
                                Min = stat$Min.,
                                Median = stat$Median, 
                                Mean = stat$Mean, 
                                Max = stat$Max.)
      }
    }
    
    formattable(table_min, align = c('l','c','c','c','c','c'), list(
      'Country' = formatter("span"),
      area(col=2,)~ color_tile("#FEEDDE",'#FD8D3C')))
    
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
  aux <- round(aux, digits = 2)
  
  if(tipo == 'total'){
    is.num <- sapply(stat, is.numeric)
    stat[is.num] <- lapply(stat[is.num], round, 2)
    stat[is.num] <- lapply(stat[is.num], formattable::comma,1)
    
    table <- data.frame(Country, 'USD Monthly Av' = aux, 
                        Min = stat$Min., 
                        Median = stat$Median, 
                        Mean = stat$Mean, 
                        Max = stat$Max.)
  }else{
    if(tipo == 'porcentaje'){
      is.num <- sapply(stat, is.numeric)
      stat[is.num] <- lapply(stat[is.num], round, 4)
      stat[is.num] <- lapply(stat[is.num], formattable::percent)
      
      table <- data.frame(Country, 'USD Monthly Av' = aux, 
                          Min = stat$Min.,
                          Median = stat$Median, 
                          Mean = stat$Mean, 
                          Max = stat$Max.)
    }
  }
  
  formattable(table, align = c('l','c','c','c','c','c'), list(
    'Country' = formatter("span"),
    area(col=2,)~ color_tile("#FFFBE2",'#6BAA9F')))
}

# Función que imprime una tabla entre dos fechas determinadas respecto a un indicador, 
# para todos los países, ordenando los datos de mayor a menor en la fecha de inicio 
# o de fin dependiendo el caso 

tabla_lapso <- function(indicador,fecha_inicio,fecha_fin,tipo,orden){
  n <- dif_mes(fecha_inicio, fecha_fin)
  fechas <- as.Date(c())
  f_1 <- paste(fecha_inicio,'-01',sep = '')
  
  for (i in 1:n){
    fechas[i] <- ymd(as.Date(f_1)) %m+% months(i-1)
  }
  
  fechas <- as.character(fechas)
  fechas <- str_remove(fechas, '-01')
  
  df <- list()
  
  if(tipo == 'total'){
    if(orden == 'creciente'){
      for (i in 1:n) {
        df[[i]] <- TOTAL[which(TOTAL$year_month == fechas[i]),c('iso2c','year_month',indicador)]
      }
    }else{
      if(orden =='decreciente'){
        for (i in 1:n) {
          df[[(n+1)-i]] <- TOTAL[which(TOTAL$year_month == fechas[i]),c('iso2c','year_month',indicador)]
        }
      }
    }
  }else{
    if(tipo == 'porcentaje'){
      if(orden == 'creciente'){
        for (i in 1:n) {
          df[[i]] <- TOTAL_P[which(TOTAL_P$year_month == fechas[i]),c('iso2c','year_month',indicador)]
        }
      }else{
        if(orden=='decreciente'){
          for (i in 1:n) {
            df[[(n+1)-i]] <- TOTAL_P[which(TOTAL_P$year_month == fechas[i]),c('iso2c','year_month',indicador)]
          }
        }
      }
    }
  }
  
  df[[1]] <- na.omit(df[[1]])
  df_1 <- df[[1]]
  
  aux <- sort(df_1[,3], decreasing = T)
  
  for (i in 1:length(df_1$iso2c)){
    a <- df_1[which(df_1[,3] == aux[i]),]
    if (i == 1){
      t <- a
    }else{
      t <- rbind(t,a)
    }
  }
  
  df_1 <- t
  
  t <- data.frame()
  a <- c()
  for(i in 1:n){
    for(j in 1:length(df_1$iso2c)){
      a <- df[[i]][which(df[[i]][,1] == df_1[,1][j]),]
      if (j == 1){
        t <- a
      }else{
        t <- rbind(t,a)
      }
    }
    df[[i]] <- t
    t <- data.frame()
  }
  
  df <- lapply(df, function(x) { x['year_month'] <- NULL; x })
  DF <- data.frame()
  DF <- purrr::reduce(df, full_join, by = "iso2c") %>% replace(., is.na(.), 0)
  DF <- as.data.frame(DF)
  
  Country <- c()
  for (i in 1:length(df_1$iso2c)){
    Country[i] <- buscar_inv(DF$iso2c[i])
  }
  DF$iso2c <- Country
  
  if(orden == 'creciente'){
    colnames(DF) <- c(indicador,fechas)
  }else{
    if(orden == 'decreciente'){
      colnames(DF) <- c(indicador,rev(fechas)) 
    }
  }
  
  options(scipen = 999)
  DF <- as.data.frame(DF)
  if(n == 1){
    row.names(DF) <- NULL
  }
  DF_1 <- SharedData$new(DF)
  
  if(tipo == 'total'){
    bscols(
      widths = c(2, 10),
      list(
        filter_select("country", "Country", DF_1, ~DF[[indicador]])
      ),
      reactable(DF_1, defaultPageSize = as.integer((length(df_1$iso2c)+1)/2),
                defaultColDef = colDef(format = colFormat(separators = T, digits = 1)),
                bordered = T)
    )
  }else{
    if(tipo == 'porcentaje'){
      bscols(
        widths = c(2, 10),
        list(
          filter_select("country", "Country", DF_1, ~DF[[indicador]])
        ),
        reactable(DF_1, defaultPageSize = as.integer((length(df_1$iso2c)+1)/2),
                  defaultColDef = colDef(format = colFormat(percent = T, digits = 2)),
                  bordered = T)
      )
    }
  }
}

# Función que crea una tabla entre dos fechas determinadas respecto a un indicador, 
# para todos los países, ordenando los datos de mayor a menor en la fecha de inicio 
# o de fin dependiendo el caso 

tabla_lapso_csv <- function(indicador,fecha_inicio,fecha_fin,tipo,orden){
  n <- dif_mes(fecha_inicio, fecha_fin)
  fechas <- as.Date(c())
  f_1 <- paste(fecha_inicio,'-01',sep = '')
  
  for (i in 1:n){
    fechas[i] <- ymd(as.Date(f_1)) %m+% months(i-1)
  }
  
  fechas <- as.character(fechas)
  fechas <- str_remove(fechas, '-01')
  
  df <- vector('list',length = n)
  
  if(tipo == 'total'){
    if(orden == 'creciente'){
      for (i in 1:n) {
        df[[i]] <- TOTAL[which(TOTAL$year_month == fechas[i]),c('iso2c','year_month',indicador)]
      }
    }else{
      if(orden=='decreciente'){
        for (i in 1:n) {
          df[[n+1-i]] <- TOTAL[which(TOTAL$year_month == fechas[i]),c('iso2c','year_month',indicador)]
        }
      }
    }
  }else{
    if(tipo == 'porcentaje'){
      if(orden == 'creciente'){
        for (i in 1:n) {
          df[[i]] <- TOTAL_P[which(TOTAL_P$year_month == fechas[i]),c('iso2c','year_month',indicador)]
        }
      }else{
        if(orden=='decreciente'){
          for (i in 1:n) {
            df[[n+1-i]] <- TOTAL_P[which(TOTAL_P$year_month == fechas[i]),c('iso2c','year_month',indicador)]
          }
        }
      }
    }
  }
  
  df[[1]] <- na.omit(df[[1]])
  df_1 <- df[[1]]
  
  aux <- sort(df_1[,3], decreasing = T)
  
  for (i in 1:length(df_1$iso2c)){
    a <- df_1[which(df_1[,3] == aux[i]),]
    if (i == 1){
      t <- a
    }else{
      t <- rbind(t,a)
    }
  }
  
  df_1 <- t
  
  t <- data.frame()
  a <- c()
  for(i in 1:n){
    for(j in 1:length(df_1$iso2c)){
      a <- df[[i]][which(df[[i]][,1] == df_1[,1][j]),]
      if (j == 1){
        t <- a
      }else{
        t <- rbind(t,a)
      }
    }
    df[[i]] <- t
    t <- data.frame()
  }
  
  df <- lapply(df, function(x) { x['year_month'] <- NULL; x })
  DF <- data.frame()
  DF <- purrr::reduce(df, full_join, by = "iso2c") %>% replace(., is.na(.), 0)
  DF <- as.data.frame(DF)
  
  Country <- c()
  for (i in 1:length(df_1$iso2c)){
    Country[i] <- buscar_inv(DF$iso2c[i])
  }
  DF$iso2c <- Country
  
  if(orden == 'creciente'){
    colnames(DF) <- c(indicador,fechas)
  }else{
    if(orden == 'decreciente'){
      colnames(DF) <- c(indicador,rev(fechas)) 
    }
  }
  
  options(scipen = 999)
  DF <- as.data.frame(DF)
  
  if(n >= 2){
    if(tipo == 'total'){
      DF[,2:(n+1)] <- lapply(DF[,2:(n+1)], formattable::comma, 1)
    }else{
      if(tipo == 'porcentaje'){
        DF[,2:(n+1)] <- lapply(DF[,2:(n+1)], formattable::percent)
      }
    }
  }else{
    if(tipo == 'total'){
      DF[,2] <- formattable::comma(DF[,2])
    }else{
      if(tipo == 'porcentaje'){
        DF[,2] <- formattable::percent(DF[,2])
        row.names(DF) <- NULL
      }
    }
  }

  return(DF)
}
