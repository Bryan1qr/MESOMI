# Automatización de la generación de la tabla para monitoreo --------------
ECA <- function(meteo, gases, fecha_inicio, fecha_fin, estacion, tipo){
  factor_eca <- function(x,y){
    fc <- (x*y*273.15)/(22.41*(273.15+25))
    fc}
  m1 <- read.csv(
    meteo, skip = 3, na.strings = "NAN") %>% 
    select(c(1,4,7,9,20, 23, 28, 32)) %>% 
    rename_at(
      vars(names(.)),
      ~ c("date", "pres", "pp", 
          "temp", "hr", "wd", "ws", "rad")) %>% 
    mutate(date = as.POSIXct(date, format = "%d/%m/%Y %H:%M", tz = "Etc/GMT")) %>% 
    filter(date %in% seq(
      as.POSIXct(paste(fecha_inicio, "00:00:00"), tz = "Etc/GMT"),
      as.POSIXct(paste(fecha_fin, "23:00:00"), tz = "Etc/GMT"),
      by = "1 hour")) %>% 
    mutate_if(is.character, as.numeric) 
  
  g2 <- read.csv(
    gases, skip = 3, na.strings = "NAN") %>% 
    select(c(1,3,4, 10, 17, 20, 27, 56, 57)) %>% 
    rename_at(vars(names(.)),~ c(
      "date", "no", "no2", "so2",
      "h2s", "co", "o3", "pm25", "pm10")) %>% 
    mutate(date = as.POSIXct(date, format = "%d/%m/%Y %H:%M", tz = "Etc/GMT")) %>% 
    filter(date %in% seq(
      as.POSIXct(paste(fecha_inicio, "00:00:00"), tz = "Etc/GMT"),
      as.POSIXct(paste(fecha_fin, "23:00:00"), tz = "Etc/GMT"),
      by = "1 hour")) %>% 
    mutate_if(is.character, as.numeric)%>% 
    mutate(
      no = factor_eca(no, 30.00612),
      no2 = factor_eca(no2, 46.00552),
      so2 = factor_eca(so2, 64.06480),
      h2s = factor_eca(h2s, 34.08196),
      co = factor_eca(co, 28.01055),
      o3 = factor_eca(o3, 47.99820))

  df <- cbind(m1, g2[, -1])
  df <- df %>% 
    mutate(pm25 = if_else(pm25 > -9.7 & pm25 < 0, 0, false = pm25),
           pm25 = if_else(pm25 <= -9.7, NA, pm25),
           pm10 = if_else(is.na(pm25), NA_real_, pm10),
           pm10 = if_else(pm10 > -9.7 & pm10 < 0, 0, false = pm10),
           no = if_else(no > -9.7 & no < 0, 0, false = no),
           no2 = if_else(no2 > -9.7 & no2 < 0, 0, false = no2),
           so2 = if_else(so2 > -9.7 & so2 < 0, 0, false = so2),
           h2s = if_else(h2s > -9.7 & h2s < 0, 0, false = h2s),
           co = if_else(co > -9.7 & co < 0, 0, false = co),
           o3 = if_else(o3 > -9.7 & o3 < 0, 0, false = o3))
  
  a <- df  %>% 
    mutate(
      o3 = rollmean(o3, 6, fill = NA,
                    align = c('right'),
                    na.rm = T, hasNA = T),
      co_1 = rollmean(co,6, fill = NA,
                      align = c('right'),
                      na.rm = T, hasNA = T)) %>% 
    slice(1:7)
  
  b <- df  %>% 
    mutate(o3 = rollmean(o3, 8, fill = NA,
                         align = c('right'),
                         na.rm = T, hasNA = T),
           co_1 = rollmean(co,8, fill = NA,
                           align = c('right'),
                           na.rm = T, hasNA = T)) %>% 
    slice(8:length(df$date))
  
  df <- rbind(a,b)
  eca <- df %>% 
    mutate(fecha = format(date, "%d-%B"),
           fecha2 = as.Date(date)) %>% 
    group_by(fecha) %>% 
    summarise(
      pm25 = mean(pm25, na.rm = T),
      pm10 = mean(pm10, na.rm = T),
      no2 = mean(no2, na.rm = T),
      so2 = mean(so2, na.rm = T),
      h2s = mean(h2s, na.rm = T),
      co = mean(co, na.rm = T),
      co_1 = mean(co_1, na.rm = T),
      o3 = max(o3, na.rm = T)
    )
  if (tipo == "save") {
    openxlsx::write.xlsx(
      lst(df,eca), paste0(estacion, ".xlsx"))
  }else if(tipo =="lista"){
    lst(df, eca) 
  }
  
}
