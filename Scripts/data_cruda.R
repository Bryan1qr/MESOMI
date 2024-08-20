# Automatización de la generación de la tabla para monitoreo --------------
cruda <- function(meteo, gases, pm, fecha_inicio, fecha_fin, estacion, tipo){
  m1 <- read.csv(
    meteo, skip = 3, na.strings = "NAN") %>% 
    select(c(1,4,7,9, 23, 28, 32)) %>% 
    rename_at(
      vars(names(.)),
      ~ c("date", "pres", "pp", 
          "temp", "wd", "ws", "rad")) %>% 
    mutate(date = as.POSIXct(date, format = "%d/%m/%Y %H:%M")) %>% 
    filter(date %in% seq(
      as.POSIXct(paste(fecha_inicio, "00:00:00")),
      as.POSIXct(paste(fecha_fin, "23:00:00")),
      by = "1 hour")) %>% 
    mutate_if(is.character, as.numeric)
  
  g2 <- read.csv(
    gases, skip = 3, na.strings = "NAN") %>% 
    select(c(1,3,4, 10, 17, 20, 27)) %>% 
    rename_at(vars(names(.)),~ c(
      "date", "no", "no2", "so2",
      "h2s", "co", "o3")) %>% 
    mutate(date = as.POSIXct(date, format = "%d/%m/%Y %H:%M")) %>% 
    filter(date %in% seq(
      as.POSIXct(paste(fecha_inicio, "00:00:00")),
      as.POSIXct(paste(fecha_fin, "23:00:00")),
      by = "1 hour")) %>% 
    mutate_if(is.character, as.numeric)
  
  p3 <- read.csv(
    pm, skip = 3, na.strings = "NAN") %>% 
    mutate(date = as.POSIXct(
      paste(Date, Time),
      format = "%Y-%m-%d %H:%M")) %>% 
    select(c(date, 3, 4,7)) %>% 
    rename_at(vars(names(.)),~ c(
      "date", "hr", "pm25", "pm10")) %>% 
    filter(date %in% seq(
      as.POSIXct(paste(fecha_inicio, "00:00:00")),
      as.POSIXct(paste(fecha_fin, "23:00:00")),
      by = "1 hour")) %>% 
    mutate_if(is.character, as.numeric)
  
  df <- cbind(p3, m1[,-1], g2[, -1])
  if (tipo == "save") {
    openxlsx::write.xlsx(
      df, paste0(estacion, ".xlsx"))
  }else if(tipo =="lista"){
    df 
  }
  
}