rm(list = ls())
#primero cargo las librerias necesarias.
require(metR)
require(ncdf4)
require(udunits2)
require(lubridate)
require(ggplot2)
require(maps)
require(gridExtra)
require(cowplot)
require(colorspace)
#ahora abro el archivo con los datos de viento.
path <- "~/Desktop/Labo/trabajo_final/datos_trabajo_final"
archivo <- paste0(path,"/u_v_components_300_500hPa_2010-2012.nc")

GlanceNetCDF(archivo)

# FUNCIONES ---------------------------------------------------------------
#en este apartado defino todas las funciones que voy a usar en el codigo.
mapa <- map_data("world")

mi_mapa <- geom_path(data = mapa, aes(long ,lat , group = group),
                     linewidth = 0.5)

ubicacionvolcan <- data.frame("longitud" = -72,"latitud" = -40.5)

funcion_v_en_u <- function(datosu,datosv) {
  for ( i in 1:length(datosu)) {
    datosu[[i]]$v <- datosv[[i]]$v
  } 
  return(datosu)
}

norm_vec <- function(u,v) {
  pip <- sqrt(u^2 + v^2)
  return(pip)
}

funcion_intensidad <- function(datos) {
  for( i in 1:length(datos)) {
    datos[[i]]$intensidad <- norm_vec(datos[[i]]$u,datos[[i]]$v)
  }
  return(datos)
}

funcion_maximo<- function(datos) {
  max <- c()
  for ( i in 1:length(datos)) {
    max <- max(c(max,max(datos[[i]]$intensidad)))
  }
  return(max)
}

funcion_minimo <- function(datos) {
  min <- c()
  for ( i in 1:length(datos)) {
    min <- min(c(min,min(datos[[i]]$intensidad)))
  }
  return(min)
}

funcion_organizadora_04 <- function(datos) {
  
  cerohrs <- subset(datos,hour(datos$time) == 0 & day(datos$time) == 04)
  seishrs <- subset(datos,hour(datos$time) == 6 & day(datos$time) == 04)
  docehrs <- subset(datos,hour(datos$time) == 12 & day(datos$time) == 04)
  dieciochohrs <- subset(datos,hour(datos$time) == 18 & day(datos$time) == 04)
  
  datosfinales_b <- list("cero" = cerohrs,
                         "seis" = seishrs,
                         "doce" = docehrs,
                         "dieciocho" = dieciochohrs)
  return(datosfinales_b)
}

funcion_organizadora_05 <- function(datos) {
  
  cerohrs <- subset(datos,hour(datos$time) == 0 & day(datos$time) == 05)
  seishrs <- subset(datos,hour(datos$time) == 6 & day(datos$time) == 05)
  docehrs <- subset(datos,hour(datos$time) == 12 & day(datos$time) == 05)
  dieciochohrs <- subset(datos,hour(datos$time) == 18 & day(datos$time) == 05)
  
  datosfinales_b <- list("cero" = cerohrs,
                         "seis" = seishrs,
                         "doce" = docehrs,
                         "dieciocho" = dieciochohrs)
  return(datosfinales_b)
}

funcion_organizadora_06 <- function(datos) {
  
  cerohrs <- subset(datos,hour(datos$time) == 0 & day(datos$time) == 06)
  seishrs <- subset(datos,hour(datos$time) == 6 & day(datos$time) == 06)
  docehrs <- subset(datos,hour(datos$time) == 12 & day(datos$time) == 06)
  dieciochohrs <- subset(datos,hour(datos$time) == 18 & day(datos$time) == 06)
  
  datosfinales_b <- list("cero" = cerohrs,
                         "seis" = seishrs,
                         "doce" = docehrs,
                         "dieciocho" = dieciochohrs)
  return(datosfinales_b)
}

funcion_organizadora_07 <- function(datos) {
  
  cerohrs <- subset(datos,hour(datos$time) == 0 & day(datos$time) == 07)
  seishrs <- subset(datos,hour(datos$time) == 6 & day(datos$time) == 07)
  docehrs <- subset(datos,hour(datos$time) == 12 & day(datos$time) == 07)
  dieciochohrs <- subset(datos,hour(datos$time) == 18 & day(datos$time) == 07)
  
  datosfinales_b <- list("cero" = cerohrs,
                         "seis" = seishrs,
                         "doce" = docehrs,
                         "dieciocho" = dieciochohrs)
  return(datosfinales_b)
}

funcion_organizadora_08 <- function(datos) {
  
  cerohrs <- subset(datos,hour(datos$time) == 0 & day(datos$time) == 08)
  seishrs <- subset(datos,hour(datos$time) == 6 & day(datos$time) == 08)
  docehrs <- subset(datos,hour(datos$time) == 12 & day(datos$time) == 08)
  dieciochohrs <- subset(datos,hour(datos$time) == 18 & day(datos$time) == 08)
  
  datosfinales_b <- list("cero" = cerohrs,
                         "seis" = seishrs,
                         "doce" = docehrs,
                         "dieciocho" = dieciochohrs)
  return(datosfinales_b)
}

mapeo_medias <- function(mes,intensidad,longitud,latitud) {
  radical <- c(maximo300,minimo300,maximo500,minimo500)
  rango_valores <- range(radical)
  num_ticks = 8
  escala <- seq(floor(rango_valores[1]),ceiling(rango_valores[2]),by = num_ticks)
  ggplot(mes,aes(x = longitud,
                 y = latitud)) +
    geom_contour_fill(aes(z = intensidad)) +
    geom_contour(aes(z = intensidad),color = "black",linewidth = 0.1) +
    geom_point(data = ubicacionvolcan, aes(x = longitud, y = latitud),color = "red") +
    geom_arrow(aes(dx = u,dy = v),
               skip = 12,
               size = 1,
               color = "black") +
    scale_mag(name = NULL,
              max_size = 1) +
    scale_fill_distiller(palette = "BuPu",
                         direction = 1,
                         super = ScaleContinuous,
                         limits = c(floor(rango_valores[1]),ceiling(rango_valores[2])),
                         breaks = escala,
                         guide = guide_colorsteps(barheight = 15,
                                                  barwidth = 2)) +
    mi_mapa +
    coord_sf( xlim = range(longitud),
              ylim = range(latitud),
              expand = F) +
    theme_bw() +
    labs(x = NULL,
         y = NULL,
         fill = "m/s") +
    theme(legend.position = "left",
          legend.text = element_text(size = 18),
          legend.title = element_text(size = 20))
}

func_organizadora_u <- function(datos) {
  df_temp <- aggregate(datos$u,
                       list(month(datos$time),datos$level,datos$latitude,datos$longitude),
                       mean)
  colnames(df_temp) <- c("mes","nivel","latitud","longitud","u")
  
  enero <- subset(df_temp,df_temp$mes == 1)
  febrero <- subset(df_temp,df_temp$mes == 2)
  marzo <- subset(df_temp,df_temp$mes == 3)
  abril <- subset(df_temp,df_temp$mes == 4)
  mayo <- subset(df_temp,df_temp$mes == 5)
  junio <- subset(df_temp,df_temp$mes == 6)
  julio <- subset(df_temp,df_temp$mes == 7)
  agosto <- subset(df_temp,df_temp$mes == 8)
  septiembre <- subset(df_temp,df_temp$mes == 9)
  octubre <- subset(df_temp,df_temp$mes == 10)
  noviembre <- subset(df_temp,df_temp$mes == 11)
  diciembre <- subset(df_temp,df_temp$mes == 12)
  
  datosfinales <- list("enero" = enero,
                       "febrero" = febrero,
                       "marzo" = marzo,
                       "abril" = abril,
                       "mayo" = mayo,
                       "junio" = junio,
                       "julio" = julio,
                       "agosto" = agosto,
                       "septiembre" = septiembre,
                       "octubre" = octubre,
                       "noviembre" = noviembre,
                       "diciembre" = diciembre)
  
  return(datosfinales)
}

func_organizadora_v <- function(datos) {
  df_temp <- aggregate(datos$v,
                       list(month(datos$time),datos$level,datos$latitude,datos$longitude),
                       mean)
  colnames(df_temp) <- c("mes","nivel","latitud","longitud","v")
  
  enero <- subset(df_temp,df_temp$mes == 1)
  febrero <- subset(df_temp,df_temp$mes == 2)
  marzo <- subset(df_temp,df_temp$mes == 3)
  abril <- subset(df_temp,df_temp$mes == 4)
  mayo <- subset(df_temp,df_temp$mes == 5)
  junio <- subset(df_temp,df_temp$mes == 6)
  julio <- subset(df_temp,df_temp$mes == 7)
  agosto <- subset(df_temp,df_temp$mes == 8)
  septiembre <- subset(df_temp,df_temp$mes == 9)
  octubre <- subset(df_temp,df_temp$mes == 10)
  noviembre <- subset(df_temp,df_temp$mes == 11)
  diciembre <- subset(df_temp,df_temp$mes == 12)
  
  datosfinales <- list("enero" = enero,
                       "febrero" = febrero,
                       "marzo" = marzo,
                       "abril" = abril,
                       "mayo" = mayo,
                       "junio" = junio,
                       "julio" = julio,
                       "agosto" = agosto,
                       "septiembre" = septiembre,
                       "octubre" = octubre,
                       "noviembre" = noviembre,
                       "diciembre" = diciembre)
  
  return(datosfinales)
}

mapeo_b <- function(hora,intensidad,longitud,latitud) {
  radical_b <- c(maximo300_b,minimo300_b,maximo500_b,minimo500_b)
  rango_valores <- range(radical_b)
  num_ticks = 12
  escala <- seq(floor(rango_valores[1]),ceiling(rango_valores[2]),by = num_ticks)
  ggplot(hora,aes(x = longitud,
                  y = latitud)) +
    geom_contour_fill(aes(z = intensidad)) +
    geom_contour(aes(z = intensidad),color = "black",linewidth = 0.1) +
    geom_point(data = ubicacionvolcan, aes(x = longitud, y = latitud),color = "red",size = 4) +
    geom_arrow(aes(dx = u,dy = v),
               skip = 12,
               size = 1,
               color = "black") +
    scale_mag(name = NULL,
              max_size = 1) +
    scale_fill_distiller(palette = "BuPu",
                         direction = 1,
                         super = ScaleContinuous,
                         limits = c(floor(rango_valores[1]),ceiling(rango_valores[2])),
                         breaks = escala,
                         guide = guide_colorsteps(barheight = 20,
                                                  barwidth = 4)) +
    mi_mapa +
    coord_sf( xlim = range(longitud),
              ylim = range(latitud),
              expand = F) +
    theme_bw() +
    labs(x = NULL,
         y = NULL,
         fill = "m/s") +
    theme(legend.position = "left",
          legend.text = element_text(size = 25),
          legend.title = element_text(size = 30))
}

mapeo_D_ini <- function(hora,intensidad,longitud,latitud) {
  radical_b <- c(max300_D,min300_D)
  rango_valores <- range(radical_b)
  num_ticks = 20
  escala <- seq(floor(rango_valores[1]),ceiling(rango_valores[2]),by = num_ticks)
  ggplot(hora,aes(x = longitud,
                  y = latitud)) +
    geom_contour_fill(aes(z = intensidad)) +
    geom_contour(aes(z = intensidad),color = "black",linewidth = 0.1) +
    geom_point(data = ubicacionvolcan, aes(x = longitud, y = latitud),color = "red",size = 4) +
    geom_arrow(aes(dx = u,dy = v),
               skip = 12,
               size = 1,
               color = "black") +
    scale_mag(name = NULL,
              max_size = 1) +
    scale_fill_distiller(palette = "BuPu",
                         direction = 1,
                         super = ScaleContinuous,
                         limits = c(floor(rango_valores[1]),ceiling(rango_valores[2])),
                         breaks = escala,
                         guide = guide_colorsteps(barheight = 20,
                                                  barwidth = 4)) +
    mi_mapa +
    coord_sf( xlim = range(longitud),
              ylim = range(latitud),
              expand = F) +
    theme_bw() +
    labs(x = NULL,
         y = NULL,
         fill = "m/s") +
    theme(legend.position = "left",
          legend.text = element_text(size = 25),
          legend.title = element_text(size = 30))
}

mapeo_D_ini_500 <- function(hora,intensidad,longitud,latitud) {
  radical_b <- c(max500_D,min500_D)
  rango_valores <- range(radical_b)
  num_ticks = 15
  escala <- seq(floor(rango_valores[1]),ceiling(rango_valores[2]),by = num_ticks)
  ggplot(hora,aes(x = longitud,
                  y = latitud)) +
    geom_contour_fill(aes(z = intensidad)) +
    geom_contour(aes(z = intensidad),color = "black",linewidth = 0.1) +
    geom_point(data = ubicacionvolcan, aes(x = longitud, y = latitud),color = "orange",size = 4) +
    geom_arrow(aes(dx = u,dy = v),
               skip = 12,
               size = 1,
               color = "black") +
    scale_mag(name = NULL,
              max_size = 1) +
    scale_fill_distiller(palette = "BuPu",
                         direction = 1,
                         super = ScaleContinuous,
                         limits = c(floor(rango_valores[1]),ceiling(rango_valores[2])),
                         breaks = escala,
                         guide = guide_colorsteps(barheight = 20,
                                                  barwidth = 4)) +
    mi_mapa +
    coord_sf( xlim = range(longitud),
              ylim = range(latitud),
              expand = F) +
    theme_bw() +
    labs(x = NULL,
         y = NULL,
         fill = "m/s") +
    theme(legend.position = "left",
          legend.text = element_text(size = 25),
          legend.title = element_text(size = 30))
}

mapeo_D <- function(hora,intensidad,longitud,latitud,segmentos) {
  radical_b <- c(max300_D,min300_D)
  rango_valores <- range(radical_b)
  num_ticks = 20
  escala <- seq(floor(rango_valores[1]),ceiling(rango_valores[2]),by = num_ticks)
  ggplot(hora,aes(x = longitud,
                  y = latitud)) +
    geom_contour_fill(aes(z = intensidad)) +
    geom_contour(aes(z = intensidad),color = "black",linewidth = 0.1) +
    geom_point(data = ubicacionvolcan, aes(x = longitud, y = latitud),color = "red",size = 4) +
    geom_segment(data = segmentos,
                 aes(x = lon_1, y = lat_1, xend = lon_2, yend = lat_2),
                 color = "red",linewidth = 2) +
    geom_arrow(aes(dx = u,dy = v),
               skip = 12,
               size = 1,
               color = "black") +
    scale_mag(name = NULL,
              max_size = 1) +
    scale_fill_distiller(palette = "BuPu",
                         direction = 1,
                         super = ScaleContinuous,
                         limits = c(floor(rango_valores[1]),ceiling(rango_valores[2])),
                         breaks = escala,
                         guide = guide_colorsteps(barheight = 20,
                                                  barwidth = 4)) +
    mi_mapa +
    coord_sf( xlim = range(longitud),
              ylim = range(latitud),
              expand = F) +
    theme_bw() +
    labs(x = NULL,
         y = NULL,
         fill = "m/s") +
    theme(legend.position = "left",
          legend.text = element_text(size = 25),
          legend.title = element_text(size = 30))
}

mapeo_D_500 <- function(hora,intensidad,longitud,latitud,segmentos) {
  radical_b <- c(max500_D,min500_D)
  rango_valores <- range(radical_b)
  num_ticks = 15
  escala <- seq(floor(rango_valores[1]),ceiling(rango_valores[2]),by = num_ticks)
  ggplot(hora,aes(x = longitud,
                  y = latitud)) +
    geom_contour_fill(aes(z = intensidad)) +
    geom_contour(aes(z = intensidad),color = "black",linewidth = 0.1) +
    geom_point(data = ubicacionvolcan, aes(x = longitud, y = latitud),color = "orange",size = 4) +
    geom_segment(data = segmentos,
                 aes(x = lon_1, y = lat_1, xend = lon_2, yend = lat_2),
                 color = "orange",linewidth = 2) +
    geom_arrow(aes(dx = u,dy = v),
               skip = 12,
               size = 1,
               color = "black") +
    scale_mag(name = NULL,
              max_size = 1) +
    scale_fill_distiller(palette = "BuPu",
                         direction = 1,
                         super = ScaleContinuous,
                         limits = c(floor(rango_valores[1]),ceiling(rango_valores[2])),
                         breaks = escala,
                         guide = guide_colorsteps(barheight = 20,
                                                  barwidth = 4)) +
    mi_mapa +
    coord_sf( xlim = range(longitud),
              ylim = range(latitud),
              expand = F) +
    theme_bw() +
    labs(x = NULL,
         y = NULL,
         fill = "m/s") +
    theme(legend.position = "left",
          legend.text = element_text(size = 25),
          legend.title = element_text(size = 30))
}

funcion_desplazamiento <- function(datos,uinicial) {
  v_temp <- datos$v[which(datos$longitud == uinicial[1,1] & datos$latitud == uinicial[1,2])]
  u_temp <- datos$u[which(datos$longitud == uinicial[1,1] & datos$latitud == uinicial[1,2])]
  inten_temp <- datos$intensidad[which(datos$longitud == uinicial[1,1] & datos$latitud == uinicial[1,2])]
  tita <- atan(v_temp/u_temp)
  d_temp <- inten_temp * 21600
  d_lat <- d_temp * sin(tita)
  d_lon <- d_temp * cos(tita)
  delta_lat <- ((d_lat/(6357000 + 9000)) * 180) / pi
  delta_lon <- ((d_lon/((6357000 + 9000) * cos(((uinicial[1,1] + 90 ) * pi ) / 180 ))) * 180) / pi
  ubicacionfinal <- data.frame("longitud" = uinicial[1,1] + delta_lon,
                               "latitud" = uinicial[1,2] + delta_lat)
  return(ubicacionfinal)
}

funcion_desplazamiento_ini <- function(datos,uinicial) {
  v_temp <- datos$v[which(datos$longitud == uinicial[1,1] & datos$latitud == uinicial[1,2])]
  u_temp <- datos$u[which(datos$longitud == uinicial[1,1] & datos$latitud == uinicial[1,2])]
  inten_temp <- datos$intensidad[which(datos$longitud == uinicial[1,1] & datos$latitud == uinicial[1,2])]
  tita <- atan(v_temp/u_temp)
  d_temp <- inten_temp * 10800
  d_lat <- d_temp * sin(tita)
  d_lon <- d_temp * cos(tita)
  delta_lat <- ((d_lat/(6357000 + 9000)) * 180) / pi
  delta_lon <- ((d_lon/((6357000 + 9000) * cos(((uinicial[1,1] + 90 ) * pi ) / 180 ))) * 180) / pi
  ubicacionfinal <- data.frame("longitud" = uinicial[1,1] + delta_lon,
                               "latitud" = uinicial[1,2] + delta_lat)
  return(ubicacionfinal)
}

# A) ----------------------------------------------------------------------
#En este item tengo que hacer un grafico que muestre la direccion e intensidad
#del viento medio para todos los meses en el año que sucedio la erupcion.

#Primero separo los valores de viento que necesito en un data_frame, separandolos tambien por alturas.
datos_u_300hPa_2011 <- ReadNetCDF(archivo, vars = "u",
                                  subset = list( level = 300,
                                                 latitude = c(-50,-30),
                                                 longitude = c(-75,-55),
                                                 time = c("2011-01-01 00:00:00","2011-12-31 18:00:00")))
datos_u_500hPa_2011 <- ReadNetCDF(archivo, vars = "u",
                                  subset = list( level = 500,
                                                 latitude = c(-50,-30),
                                                 longitude = c(-75,-55),
                                                 time = c("2011-01-01 00:00:00","2011-12-31 18:00:00")))
datos_v_300hPa_2011 <- ReadNetCDF(archivo, vars = "v",
                                  subset = list( level = 300,
                                                 latitude = c(-50,-30),
                                                 longitude = c(-75,-55),
                                                 time = c("2011-01-01 00:00:00","2011-12-31 18:00:00")))
datos_v_500hPa_2011 <- ReadNetCDF(archivo, vars = "v",
                                  subset = list( level = 500,
                                                 latitude = c(-50,-30),
                                                 longitude = c(-75,-55),
                                                 time = c("2011-01-01 00:00:00","2011-12-31 18:00:00")))

#Con esta funcion calculo la media mensual del viento y creo una lista con 12 dataframes,
#uno por cada mes.

datoslista_300hPa <- func_organizadora_u(datos_u_300hPa_2011)
datoslista_500hPa <- func_organizadora_u(datos_u_500hPa_2011)
datoslista_v_300hPa <- func_organizadora_v(datos_v_300hPa_2011)
datoslista_v_500hPa <- func_organizadora_v(datos_v_500hPa_2011)

#Con estas funciones le agrego a cada dataframe la componente v y la intensidad.
datoslista_300hPa <- funcion_v_en_u(datoslista_300hPa,datoslista_v_300hPa)
datoslista_500hPa <- funcion_v_en_u(datoslista_500hPa,datoslista_v_500hPa)
datoslista_300hPa <- funcion_intensidad(datoslista_300hPa)
datoslista_500hPa <- funcion_intensidad(datoslista_500hPa)

#Con estas funciones calculo los maximos y minimos de viento medio. Esto me va a 
#servir para usar una sola escala para todos los graficos.
minimo300 <- funcion_minimo(datoslista_300hPa)
maximo300 <- funcion_maximo(datoslista_300hPa)
minimo500 <- funcion_minimo(datoslista_500hPa)
maximo500 <- funcion_maximo(datoslista_500hPa)

# a 300 -------------------------------------------------------------------

#Con estas funciones empiezo a graficar. Primero ploteo un mapa de flechas sobre
#uno de contornos, cosa que queden flechas que muestran la direccion del viento arriba de un mapa
#que muestra la intensidad.

enero300 <- mapeo_medias(datoslista_300hPa$enero,
                         datoslista_300hPa$enero$intensidad,
                         datoslista_300hPa$enero$longitud,
                         datoslista_300hPa$enero$latitud)
febrero300 <- mapeo_medias(datoslista_300hPa$febrero,
                           datoslista_300hPa$febrero$intensidad,
                           datoslista_300hPa$febrero$longitud,
                           datoslista_300hPa$febrero$latitud)
marzo300 <- mapeo_medias(datoslista_300hPa$marzo,
                         datoslista_300hPa$marzo$intensidad,
                         datoslista_300hPa$marzo$longitud,
                         datoslista_300hPa$marzo$latitud)
abril300 <- mapeo_medias(datoslista_300hPa$abril,
                         datoslista_300hPa$abril$intensidad,
                         datoslista_300hPa$abril$longitud,
                         datoslista_300hPa$abril$latitud)
mayo300 <- mapeo_medias(datoslista_300hPa$mayo,
                        datoslista_300hPa$mayo$intensidad,
                        datoslista_300hPa$mayo$longitud,
                        datoslista_300hPa$mayo$latitud)
junio300 <- mapeo_medias(datoslista_300hPa$junio,
                         datoslista_300hPa$junio$intensidad,
                         datoslista_300hPa$junio$longitud,
                         datoslista_300hPa$junio$latitud)
julio300 <- mapeo_medias(datoslista_300hPa$julio,
                         datoslista_300hPa$julio$intensidad,
                         datoslista_300hPa$julio$longitud,
                         datoslista_300hPa$julio$latitud)
agosto300 <- mapeo_medias(datoslista_300hPa$agosto,
                          datoslista_300hPa$agosto$intensidad,
                          datoslista_300hPa$agosto$longitud,
                          datoslista_300hPa$agosto$latitud)
septiembre300 <- mapeo_medias(datoslista_300hPa$septiembre,
                              datoslista_300hPa$septiembre$intensidad,
                              datoslista_300hPa$septiembre$longitud,
                              datoslista_300hPa$septiembre$latitud)
octubre300 <- mapeo_medias(datoslista_300hPa$octubre,
                           datoslista_300hPa$octubre$intensidad,
                           datoslista_300hPa$octubre$longitud,
                           datoslista_300hPa$octubre$latitud)
noviembre300 <- mapeo_medias(datoslista_300hPa$noviembre,
                             datoslista_300hPa$noviembre$intensidad,
                             datoslista_300hPa$noviembre$longitud,
                             datoslista_300hPa$noviembre$latitud)
diciembre300 <- mapeo_medias(datoslista_300hPa$diciembre,
                             datoslista_300hPa$diciembre$intensidad,
                             datoslista_300hPa$diciembre$longitud,
                             datoslista_300hPa$diciembre$latitud)
#Luego meto los doce graficos en una sola planilla, de tal forma que se pueda ver la evolucion del 
#viento a traves de los meses.

mapa300 <- grid.arrange(enero300,febrero300,marzo300,abril300,mayo300,junio300,
                        julio300,agosto300,septiembre300,octubre300,noviembre300,diciembre300,
                        ncol = 4,
                        layout_matrix = cbind(c(1,5,9),c(2,6,10),c(3,7,11),c(4,8,12)))
#Finalmente los guardo.
ggsave("mapas_medias_300hPa.png",mapa300, path = "/home/felipe/Desktop/Labo/trabajo_final/mapas_medias/", width = 30, height = 18, dpi = 72)

# a 500 -------------------------------------------------------------------
#mismo proceso para 500hPa
enero500 <- mapeo_medias(datoslista_500hPa$enero,
                         datoslista_500hPa$enero$intensidad,
                         datoslista_500hPa$enero$longitud,
                         datoslista_500hPa$enero$latitud)
febrero500 <- mapeo_medias(datoslista_500hPa$febrero,
                           datoslista_500hPa$febrero$intensidad,
                           datoslista_500hPa$febrero$longitud,
                           datoslista_500hPa$febrero$latitud)
marzo500 <- mapeo_medias(datoslista_500hPa$marzo,
                         datoslista_500hPa$marzo$intensidad,
                         datoslista_500hPa$marzo$longitud,
                         datoslista_500hPa$marzo$latitud)
abril500 <- mapeo_medias(datoslista_500hPa$abril,
                         datoslista_500hPa$abril$intensidad,
                         datoslista_500hPa$abril$longitud,
                         datoslista_500hPa$abril$latitud)
mayo500 <- mapeo_medias(datoslista_500hPa$mayo,
                        datoslista_500hPa$mayo$intensidad,
                        datoslista_500hPa$mayo$longitud,
                        datoslista_500hPa$mayo$latitud)
junio500 <- mapeo_medias(datoslista_500hPa$junio,
                         datoslista_500hPa$junio$intensidad,
                         datoslista_500hPa$junio$longitud,
                         datoslista_500hPa$junio$latitud)
julio500 <- mapeo_medias(datoslista_500hPa$julio,
                         datoslista_500hPa$julio$intensidad,
                         datoslista_500hPa$julio$longitud,
                         datoslista_500hPa$julio$latitud)
agosto500 <- mapeo_medias(datoslista_500hPa$agosto,
                          datoslista_500hPa$agosto$intensidad,
                          datoslista_500hPa$agosto$longitud,
                          datoslista_500hPa$agosto$latitud)
septiembre500 <- mapeo_medias(datoslista_500hPa$septiembre,
                              datoslista_500hPa$septiembre$intensidad,
                              datoslista_500hPa$septiembre$longitud,
                              datoslista_500hPa$septiembre$latitud)
octubre500 <- mapeo_medias(datoslista_500hPa$octubre,
                           datoslista_500hPa$octubre$intensidad,
                           datoslista_500hPa$octubre$longitud,
                           datoslista_500hPa$octubre$latitud)
noviembre500 <- mapeo_medias(datoslista_500hPa$noviembre,
                             datoslista_500hPa$noviembre$intensidad,
                             datoslista_500hPa$noviembre$longitud,
                             datoslista_500hPa$noviembre$latitud)
diciembre500 <- mapeo_medias(datoslista_500hPa$diciembre,
                             datoslista_500hPa$diciembre$intensidad,
                             datoslista_500hPa$diciembre$longitud,
                             datoslista_500hPa$diciembre$latitud)

mapa500 <- grid.arrange(enero500,febrero500,marzo500,abril500,mayo500,junio500,
                        julio500,agosto500,septiembre500,octubre500,noviembre500,diciembre500,
                        ncol = 4,
                        layout_matrix = cbind(c(1,5,9),c(2,6,10),c(3,7,11),c(4,8,12)))
ggsave("mapas_medias_500hPa.png",mapa500, path = "/home/felipe/Desktop/Labo/trabajo_final/mapas_medias/", width = 30, height = 18, dpi = 72)


# B) ----------------------------------------------------------------------

#En este item tengo que hacer graficos que muestren como fue el viento el dia de la erupcion.
#Primero separo los datos en un dataframe.

datos_u_300hPa_2011_b <- ReadNetCDF(archivo, vars = "u",
                                    subset = list( level = 300,
                                                   latitude = c(-50,-20),
                                                   longitude = c(-75,-50),
                                                   time = c("2011-06-04 00:00:00","2011-06-09 18:00:00")))
datos_u_500hPa_2011_b <- ReadNetCDF(archivo, vars = "u",
                                    subset = list( level = 500,
                                                   latitude = c(-50,-20),
                                                   longitude = c(-75,-50),
                                                   time = c("2011-06-04 00:00:00","2011-06-09 18:00:00")))
datos_v_300hPa_2011_b <- ReadNetCDF(archivo, vars = "v",
                                    subset = list( level = 300,
                                                   latitude = c(-50,-20),
                                                   longitude = c(-75,-50),
                                                   time = c("2011-06-04 00:00:00","2011-06-09 18:00:00")))
datos_v_500hPa_2011_b <- ReadNetCDF(archivo, vars = "v",
                                    subset = list( level = 500,
                                                   latitude = c(-50,-20),
                                                   longitude = c(-75,-50),
                                                   time = c("2011-06-04 00:00:00","2011-06-09 18:00:00")))
#Misma idea que para el punto a), organizo los datos en una lista que contiene un dataframe por cada horario
#en el que se registro el viento. En este caso no hizo falta calcular ninguna media.
datoslista_300hPa_b <- funcion_organizadora_04(datos_u_300hPa_2011_b)
datoslista_500hPa_b <- funcion_organizadora_04(datos_u_500hPa_2011_b)
datoslista_v_300hPa_b <- funcion_organizadora_04(datos_v_300hPa_2011_b)
datoslista_v_500hPa_b <- funcion_organizadora_04(datos_v_500hPa_2011_b)

datoslista_300hPa_b <- funcion_v_en_u(datoslista_300hPa_b,datoslista_v_300hPa_b)
datoslista_500hPa_b <- funcion_v_en_u(datoslista_500hPa_b,datoslista_v_500hPa_b)
datoslista_300hPa_b <- funcion_intensidad(datoslista_300hPa_b)
datoslista_500hPa_b <- funcion_intensidad(datoslista_500hPa_b)

minimo300_b <- funcion_minimo(datoslista_300hPa_b)
maximo300_b <- funcion_maximo(datoslista_300hPa_b)
minimo500_b <- funcion_minimo(datoslista_500hPa_b)
maximo500_b <- funcion_maximo(datoslista_500hPa_b)

# b 300 -------------------------------------------------------------------

#Genero los mismos graficos que en el punto a) pero con los datos horarios del dia de la
#erupción.
cero300 <- mapeo_b(datoslista_300hPa_b$cero,
                   datoslista_300hPa_b$cero$intensidad,
                   datoslista_300hPa_b$cero$longitude,
                   datoslista_300hPa_b$cero$latitude)
seis300 <- mapeo_b(datoslista_300hPa_b$seis,
                   datoslista_300hPa_b$seis$intensidad,
                   datoslista_300hPa_b$seis$longitude,
                   datoslista_300hPa_b$seis$latitude)
doce300 <- mapeo_b(datoslista_300hPa_b$doce,
                   datoslista_300hPa_b$doce$intensidad,
                   datoslista_300hPa_b$doce$longitude,
                   datoslista_300hPa_b$doce$latitude)
dieciocho300 <- mapeo_b(datoslista_300hPa_b$dieciocho,
                        datoslista_300hPa_b$dieciocho$intensidad,
                        datoslista_300hPa_b$dieciocho$longitude,
                        datoslista_300hPa_b$dieciocho$latitude)

mapas300_b<- grid.arrange(cero300,seis300,doce300,dieciocho300,
                         ncol = 2,
                         nrow = 2,
                         layout_matrix = cbind(c(1,3),c(2,4)))

ggsave("mapa_horas_300hPa.png",mapas300_b,path = "/home/felipe/Desktop/Labo/trabajo_final/mapas_horarios/", width = 30, height = 18, dpi = 72)

# b 500 -------------------------------------------------------------------
#Lo mismo para 500hPa.
cero500 <- mapeo_b(datoslista_500hPa_b$cero,
                   datoslista_500hPa_b$cero$intensidad,
                   datoslista_500hPa_b$cero$longitude,
                   datoslista_500hPa_b$cero$latitude)
seis500 <- mapeo_b(datoslista_500hPa_b$seis,
                   datoslista_500hPa_b$seis$intensidad,
                   datoslista_500hPa_b$seis$longitude,
                   datoslista_500hPa_b$seis$latitude)
doce500 <- mapeo_b(datoslista_500hPa_b$doce,
                   datoslista_500hPa_b$doce$intensidad,
                   datoslista_500hPa_b$doce$longitude,
                   datoslista_500hPa_b$doce$latitude)
dieciocho500 <- mapeo_b(datoslista_500hPa_b$dieciocho,
                        datoslista_500hPa_b$dieciocho$intensidad,
                        datoslista_500hPa_b$dieciocho$longitude,
                        datoslista_500hPa_b$dieciocho$latitude)
mapas500_b <- grid.arrange(cero500,seis500,doce500,dieciocho500,
                           ncol = 2,
                           nrow = 2,
                           layout_matrix = cbind(c(1,3),c(2,4)))

ggsave("mapa_horas_500hPa.png",mapas500_b,path = "/home/felipe/Desktop/Labo/trabajo_final/mapas_horarios/", width = 30, height = 18, dpi = 72)


# D) ----------------------------------------------------------------------

#En este item tengo que calcular y graficar la trayectoria de la ceniza. Para hacerlo asumo que el grueso de
#la ceniza es un cuerpo puntual que se matiene en una sola capa de la atmosfera y ademas asumo que los vientos
#son constantes y cambian cada 6 horas.

# 300 d -------------------------------------------------------------------

#Primero separo los datos para 4 dias despues de la erupcion en dataframes.
datos_u_300hPa_D <- ReadNetCDF(archivo, vars = "u",
                               subset = list( level = 300,
                                              latitude = c(-70,0),
                                              longitude = c(-100,10),
                                              time = c("2011-06-04 00:00:00","2011-06-07 18:00:00")))
datos_v_300hPa_D <- ReadNetCDF(archivo, vars = "v",
                               subset = list( level = 300,
                                              latitude = c(-70,0),
                                              longitude = c(-100,10),
                                              time = c("2011-06-04 00:00:00","2011-06-07 18:00:00")))
#Ahora aplico funciones para organizar los datos en listas.Generé cuatro listas, una por cada dia. 
#Adentro de cada lista hay cuatro dataframes, uno por cada horario registrado.

#Aplico las funciones ya aplicadas para items anteriores con el objetivo de organizar mis datos.
datoslista_300hPa_04 <- funcion_organizadora_04(datos_u_300hPa_D)
datoslista_v_300hPa_04 <- funcion_organizadora_04(datos_v_300hPa_D)
datoslista_300hPa_05 <- funcion_organizadora_05(datos_u_300hPa_D)
datoslista_v_300hPa_05 <- funcion_organizadora_05(datos_v_300hPa_D)
datoslista_300hPa_06 <- funcion_organizadora_06(datos_u_300hPa_D)
datoslista_v_300hPa_06 <- funcion_organizadora_06(datos_v_300hPa_D)
datoslista_300hPa_07 <- funcion_organizadora_07(datos_u_300hPa_D)
datoslista_v_300hPa_07 <- funcion_organizadora_07(datos_v_300hPa_D)


datoslista_300hPa_04 <- funcion_v_en_u(datoslista_300hPa_04,datoslista_v_300hPa_04)
datoslista_300hPa_05 <- funcion_v_en_u(datoslista_300hPa_05,datoslista_v_300hPa_05)
datoslista_300hPa_06 <- funcion_v_en_u(datoslista_300hPa_06,datoslista_v_300hPa_06)
datoslista_300hPa_07 <- funcion_v_en_u(datoslista_300hPa_07,datoslista_v_300hPa_07)

datoslista_300hPa_04 <- funcion_intensidad(datoslista_300hPa_04)
datoslista_300hPa_05 <- funcion_intensidad(datoslista_300hPa_05)
datoslista_300hPa_06 <- funcion_intensidad(datoslista_300hPa_06)
datoslista_300hPa_07 <- funcion_intensidad(datoslista_300hPa_07)

minimo300_04_D <- funcion_minimo(datoslista_300hPa_04)
minimo300_05_D <- funcion_minimo(datoslista_300hPa_05)
minimo300_06_D <- funcion_minimo(datoslista_300hPa_06)
maximo300_04_D <- funcion_maximo(datoslista_300hPa_04)
maximo300_05_D <- funcion_maximo(datoslista_300hPa_05)
maximo300_06_D <- funcion_maximo(datoslista_300hPa_06)

min300_D <- min(c(minimo300_04_D,minimo300_05_D,minimo300_06_D))
max300_D <- max(c(maximo300_04_D,maximo300_05_D,maximo300_06_D))


# dia 04 ----------------------------------------------------------------------
#Primero grafico los vientos justo antes de que erupcione el volcan.
dia04_ini <- mapeo_D_ini(datoslista_300hPa_04$doce,
                     datoslista_300hPa_04$doce$intensidad,
                     datoslista_300hPa_04$doce$longitude,
                     datoslista_300hPa_04$doce$latitude)
ggsave("mapa_dia04_inicial_300hPa.png",dia04_ini,path = "/home/felipe/Desktop/Labo/trabajo_final/mapas_des_300/", width = 30, height = 18, dpi = 72)
#Para cada dia tuve que calcular la trayectoria de la ceniza, para lo que utilize la funcion_desplazamiento.
#Como el volcan erupcionó a las 15 hs la primer funcion es un poco diferente a las otras.
#Una complicacion que tuve es que las latitudes y longitudes finales que me iba devolviendo la funcion no necesariamente
#terminaban en longitudes o latitudes en la escala que tengo mis datos. Esto significó que tuve que redondear manualmente
#cada ubicacion a las coordenadas que estan en los datos (que terminen en .0,.25,.5,.75).
ubicacion04.18 <- funcion_desplazamiento_ini(datoslista_300hPa_04$doce,ubicacionvolcan)
ubicacion04.18 <- data.frame("lon" = -69.75,"lat" = -42.25)
ubicacion05.00 <- funcion_desplazamiento(datoslista_300hPa_04$dieciocho,ubicacion04.18)
ubicacion05.00 <- data.frame("lon" = -65,"lat" = -45)
#Luego pongo cada ubicacion inicial y final en un dataframe para asi poder generar las lineas que unen esos
#puntos en el mapa con ggplot.
lonlats04 <- data.frame(lat_1 = c(ubicacionvolcan[1,2],ubicacion04.18[1,2]),
                      lat_2 = c(ubicacion04.18[1,2],ubicacion05.00[1,2]),
                      lon_1 = c(ubicacionvolcan[1,1],ubicacion04.18[1,1]),
                      lon_2 = c(ubicacion04.18[1,1],ubicacion05.00[1,1]))
dia04 <- mapeo_D(datoslista_300hPa_05$cero,
                 datoslista_300hPa_05$cero$intensidad,
                 datoslista_300hPa_05$cero$longitude,
                 datoslista_300hPa_05$cero$latitude,
                 lonlats04)
ggsave("mapa_dia04_final_300hPa.png",dia04,path = "/home/felipe/Desktop/Labo/trabajo_final/mapas_des_300/", width = 30, height = 18, dpi = 72)

# dia 05 -------------------------------------------------------------------
#Hago lo mismo para todos los dias y en distintos graficos así se puede apreciar bien la evolución de la treayectoria.
ubicacion05.06 <- funcion_desplazamiento(datoslista_300hPa_05$cero,ubicacion05.00)
ubicacion05.06 <- data.frame("lon" = -61.5,"lat" = -46)
ubicacion05.12 <- funcion_desplazamiento(datoslista_300hPa_05$seis,ubicacion05.06)
ubicacion05.12 <- data.frame("lon" = -56.25,"lat" = -46.75)
ubicacion05.18 <- funcion_desplazamiento(datoslista_300hPa_05$doce,ubicacion05.12)
ubicacion05.18 <- data.frame("lon" = -49,"lat" = -46.75)
ubicacion06.00 <- funcion_desplazamiento(datoslista_300hPa_05$dieciocho,ubicacion05.18)
ubicacion06.00 <- data.frame("lon" = -38.75,"lat" = -46.5)

lonlats05 <- data.frame(lat_1 = c(lonlats04$lat_1,ubicacion05.00[1,2],ubicacion05.06[1,2],ubicacion05.12[1,2],ubicacion05.18[1,2]),
                        lat_2 = c(lonlats04$lat_2,ubicacion05.06[1,2],ubicacion05.12[1,2],ubicacion05.18[1,2],ubicacion06.00[1,2]),
                        lon_1 = c(lonlats04$lon_1,ubicacion05.00[1,1],ubicacion05.06[1,1],ubicacion05.12[1,1],ubicacion05.18[1,1]),
                        lon_2 = c(lonlats04$lon_2,ubicacion05.06[1,1],ubicacion05.12[1,1],ubicacion05.18[1,1],ubicacion06.00[1,1]))
dia05 <- mapeo_D(datoslista_300hPa_06$cero,
                 datoslista_300hPa_06$cero$intensidad,
                 datoslista_300hPa_06$cero$longitude,
                 datoslista_300hPa_06$cero$latitude,
                 lonlats05)
ggsave("mapa_dia05_final_300hPa.png",dia05,path = "/home/felipe/Desktop/Labo/trabajo_final/mapas_des_300/", width = 30, height = 18, dpi = 72)

# dia 06 ------------------------------------------------------------------

ubicacion06.06 <- funcion_desplazamiento(datoslista_300hPa_06$cero,ubicacion06.00)
ubicacion06.06 <- data.frame("lon" = -29,"lat" = -46.75)
ubicacion06.12 <- funcion_desplazamiento(datoslista_300hPa_06$seis,ubicacion06.06)
ubicacion06.12 <- data.frame("lon" = -19.25,"lat" = -44.5)
ubicacion06.18 <- funcion_desplazamiento(datoslista_300hPa_06$doce,ubicacion06.12)
ubicacion06.18 <- data.frame("lon" = -8,"lat" = -45.75)

lonlats06 <- data.frame(lat_1 = c(lonlats05$lat_1,ubicacion06.00[1,2],ubicacion06.06[1,2],ubicacion06.12[1,2]),
                        lat_2 = c(lonlats05$lat_2,ubicacion06.06[1,2],ubicacion06.12[1,2],ubicacion06.18[1,2]),
                        lon_1 = c(lonlats05$lon_1,ubicacion06.00[1,1],ubicacion06.06[1,1],ubicacion06.12[1,1]),
                        lon_2 = c(lonlats05$lon_2,ubicacion06.06[1,1],ubicacion06.12[1,1],ubicacion06.18[1,1]))

dia06 <- mapeo_D(datoslista_300hPa_06$dieciocho,
                 datoslista_300hPa_06$dieciocho$intensidad,
                 datoslista_300hPa_06$dieciocho$longitude,
                 datoslista_300hPa_06$dieciocho$latitude,
                 lonlats06)
ggsave("mapa_dia06_final_300hPa.png",dia06,path = "/home/felipe/Desktop/Labo/trabajo_final/mapas_des_300/", width = 30, height = 18, dpi = 72)

#Llegan hasta aca los datos


# 500 d -------------------------------------------------------------------
#Hago lo mismo suponiendo que la ceniza se mantiene en los 500 hPa de altura.
datos_u_500hPa_D <- ReadNetCDF(archivo, vars = "u",
                               subset = list( level = 500,
                                              latitude = c(-70,0),
                                              longitude = c(-100,10),
                                              time = c("2011-06-04 00:00:00","2011-06-08 18:00:00")))
datos_v_500hPa_D <- ReadNetCDF(archivo, vars = "v",
                               subset = list( level = 500,
                                              latitude = c(-70,0),
                                              longitude = c(-100,10),
                                              time = c("2011-06-04 00:00:00","2011-06-08 18:00:00")))

datoslista_500hPa_04 <- funcion_organizadora_04(datos_u_500hPa_D)
datoslista_v_500hPa_04 <- funcion_organizadora_04(datos_v_500hPa_D)
datoslista_500hPa_05 <- funcion_organizadora_05(datos_u_500hPa_D)
datoslista_v_500hPa_05 <- funcion_organizadora_05(datos_v_500hPa_D)
datoslista_500hPa_06 <- funcion_organizadora_06(datos_u_500hPa_D)
datoslista_v_500hPa_06 <- funcion_organizadora_06(datos_v_500hPa_D)
datoslista_500hPa_07 <- funcion_organizadora_07(datos_u_500hPa_D)
datoslista_v_500hPa_07 <- funcion_organizadora_07(datos_v_500hPa_D)
datoslista_500hPa_08 <- funcion_organizadora_08(datos_u_500hPa_D)
datoslista_v_500hPa_08 <- funcion_organizadora_08(datos_v_500hPa_D)

datoslista_500hPa_04 <- funcion_v_en_u(datoslista_500hPa_04,datoslista_v_500hPa_04)
datoslista_500hPa_05 <- funcion_v_en_u(datoslista_500hPa_05,datoslista_v_500hPa_05)
datoslista_500hPa_06 <- funcion_v_en_u(datoslista_500hPa_06,datoslista_v_500hPa_06)
datoslista_500hPa_07 <- funcion_v_en_u(datoslista_500hPa_07,datoslista_v_500hPa_07)
datoslista_500hPa_08 <- funcion_v_en_u(datoslista_500hPa_08,datoslista_v_500hPa_08)

datoslista_500hPa_04 <- funcion_intensidad(datoslista_500hPa_04)
datoslista_500hPa_05 <- funcion_intensidad(datoslista_500hPa_05)
datoslista_500hPa_06 <- funcion_intensidad(datoslista_500hPa_06)
datoslista_500hPa_07 <- funcion_intensidad(datoslista_500hPa_07)
datoslista_500hPa_08 <- funcion_intensidad(datoslista_500hPa_08)

minimo500_04_D <- funcion_minimo(datoslista_500hPa_04)
minimo500_05_D <- funcion_minimo(datoslista_500hPa_05)
minimo500_06_D <- funcion_minimo(datoslista_500hPa_06)
maximo500_04_D <- funcion_maximo(datoslista_500hPa_04)
maximo500_05_D <- funcion_maximo(datoslista_500hPa_05)
maximo500_06_D <- funcion_maximo(datoslista_500hPa_06)

min500_D <- min(c(minimo500_04_D,minimo500_05_D,minimo500_06_D))
max500_D <- max(c(maximo500_04_D,maximo500_05_D,maximo500_06_D))

# dia 04 ------------------------------------------------------------------

dia04_ini_500 <- mapeo_D_ini_500(datoslista_500hPa_04$doce,
                         datoslista_500hPa_04$doce$intensidad,
                         datoslista_500hPa_04$doce$longitude,
                         datoslista_500hPa_04$doce$latitude)
ggsave("mapa_dia04_inicial_500hPa.png",dia04_ini_500,path = "/home/felipe/Desktop/Labo/trabajo_final/mapas_des_500/", width = 30, height = 18, dpi = 72)

ubicacion04.18_500 <- funcion_desplazamiento_ini(datoslista_500hPa_04$doce,ubicacionvolcan)
ubicacion04.18_500 <- data.frame("lon" = -71.25,"lat" = -42)
ubicacion05.00_500 <- funcion_desplazamiento(datoslista_500hPa_04$dieciocho,ubicacion04.18_500)
ubicacion05.00_500 <- data.frame("lon" = -69.5,"lat" = -45)

lonlats04_500 <- data.frame(lat_1 = c(ubicacionvolcan[1,2],ubicacion04.18_500[1,2]),
                            lat_2 = c(ubicacion04.18_500[1,2],ubicacion05.00_500[1,2]),
                            lon_1 = c(ubicacionvolcan[1,1],ubicacion04.18_500[1,1]),
                            lon_2 = c(ubicacion04.18_500[1,1],ubicacion05.00_500[1,1]))
dia04_500 <- mapeo_D_500(datoslista_500hPa_05$cero,
                 datoslista_500hPa_05$cero$intensidad,
                 datoslista_500hPa_05$cero$longitude,
                 datoslista_500hPa_05$cero$latitude,
                 lonlats04_500)
ggsave("mapa_dia04_final_500hPa.png",dia04_500,path = "/home/felipe/Desktop/Labo/trabajo_final/mapas_des_500/", width = 30, height = 18, dpi = 72)

# dia 05 ------------------------------------------------------------------

ubicacion05.06_500 <- funcion_desplazamiento(datoslista_500hPa_05$cero,ubicacion05.00_500)
ubicacion05.06_500 <- data.frame("lon" = -67.75,"lat" = -46.5)
ubicacion05.12_500 <- funcion_desplazamiento(datoslista_500hPa_05$seis,ubicacion05.06_500)
ubicacion05.12_500 <- data.frame("lon" = -65,"lat" = -46.75)
ubicacion05.18_500 <- funcion_desplazamiento(datoslista_500hPa_05$doce,ubicacion05.12_500)
ubicacion05.18_500 <- data.frame("lon" = -62.5,"lat" = -45.75)
ubicacion06.00_500 <- funcion_desplazamiento(datoslista_500hPa_05$dieciocho,ubicacion05.18_500)
ubicacion06.00_500 <- data.frame("lon" = -60,"lat" = -45)

lonlats05_500 <- data.frame(lat_1 = c(lonlats04_500$lat_1,ubicacion05.00_500[1,2],ubicacion05.06_500[1,2],ubicacion05.12_500[1,2],ubicacion05.18_500[1,2]),
                            lat_2 = c(lonlats04_500$lat_2,ubicacion05.06_500[1,2],ubicacion05.12_500[1,2],ubicacion05.18_500[1,2],ubicacion06.00_500[1,2]),
                            lon_1 = c(lonlats04_500$lon_1,ubicacion05.00_500[1,1],ubicacion05.06_500[1,1],ubicacion05.12_500[1,1],ubicacion05.18_500[1,1]),
                            lon_2 = c(lonlats04_500$lon_2,ubicacion05.06_500[1,1],ubicacion05.12_500[1,1],ubicacion05.18_500[1,1],ubicacion06.00_500[1,1]))
dia05_500 <- mapeo_D_500(datoslista_500hPa_06$cero,
                         datoslista_500hPa_06$cero$intensidad,
                         datoslista_500hPa_06$cero$longitude,
                         datoslista_500hPa_06$cero$latitude,
                         lonlats05_500)
ggsave("mapa_dia05_final_500hPa.png",dia05_500,path = "/home/felipe/Desktop/Labo/trabajo_final/mapas_des_500/", width = 30, height = 18, dpi = 72)

# dia 06 ------------------------------------------------------------------

ubicacion06.06_500 <- funcion_desplazamiento(datoslista_500hPa_06$cero,ubicacion06.00_500)
ubicacion06.06_500 <- data.frame("lon" = -56.5,"lat" = -44.5)
ubicacion06.12_500 <- funcion_desplazamiento(datoslista_500hPa_06$seis,ubicacion06.06_500)
ubicacion06.12_500 <- data.frame("lon" = -52,"lat" = -44.25)
ubicacion06.18_500 <- funcion_desplazamiento(datoslista_500hPa_06$doce,ubicacion06.12_500)
ubicacion06.18_500 <- data.frame("lon" = -46,"lat" = -44)
ubicacion07.00_500 <- funcion_desplazamiento(datoslista_500hPa_06$dieciocho,ubicacion06.18_500)
ubicacion07.00_500 <- data.frame("lon" = -39,"lat" = -44.25)

lonlats06_500 <- data.frame(lat_1 = c(lonlats05_500$lat_1,ubicacion06.00_500[1,2],ubicacion06.06_500[1,2],ubicacion06.12_500[1,2],ubicacion06.18_500[1,2]),
                            lat_2 = c(lonlats05_500$lat_2,ubicacion06.06_500[1,2],ubicacion06.12_500[1,2],ubicacion06.18_500[1,2],ubicacion07.00_500[1,2]),
                            lon_1 = c(lonlats05_500$lon_1,ubicacion06.00_500[1,1],ubicacion06.06_500[1,1],ubicacion06.12_500[1,1],ubicacion06.18_500[1,1]),
                            lon_2 = c(lonlats05_500$lon_2,ubicacion06.06_500[1,1],ubicacion06.12_500[1,1],ubicacion06.18_500[1,1],ubicacion07.00_500[1,1]))

dia06_500 <- mapeo_D_500(datoslista_500hPa_07$cero,
                         datoslista_500hPa_07$cero$intensidad,
                         datoslista_500hPa_07$cero$longitude,
                         datoslista_500hPa_07$cero$latitude,
                         lonlats06_500)
ggsave("mapa_dia06_final_500hPa.png",dia06_500,path = "/home/felipe/Desktop/Labo/trabajo_final/mapas_des_500/", width = 30, height = 18, dpi = 72)









