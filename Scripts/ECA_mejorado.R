# Automatización de la generación de la tabla para monitoreo --------------
ECA <- function(meteo, gases, pm, fecha_inicio, fecha_fin, estacion, tipo){
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
    mutate_if(is.character, as.numeric) %>% 
    mutate(across(-date, ~ ifelse(. < 0, NA, .)))
  
  g2 <- read.csv(
    gases, skip = 3, na.strings = "NAN") %>% 
    select(c(1,3,4,5, 10, 17, 20, 27)) %>% 
    rename_at(vars(names(.)),~ c(
      "date", "no", "no2", "nox", "so2",
      "h2s", "co", "o3")) %>% 
    mutate(date = as.POSIXct(date, format = "%d/%m/%Y %H:%M", tz = "Etc/GMT")) %>% 
    filter(date %in% seq(
      as.POSIXct(paste(fecha_inicio, "00:00:00"), tz = "Etc/GMT"),
      as.POSIXct(paste(fecha_fin, "23:00:00"), tz = "Etc/GMT"),
      by = "1 hour")) %>% 
    mutate_if(is.character, as.numeric)%>% 
    mutate(no2 = if_else((no + no2)/nox >= 0.9 | (no + no2)/nox <= 1.1, no2, NA),
           no2 = if_else(no2 > -500*0.006 & no2 < 0, 0, no2),
           no2 = if_else(no2 < 0, NA, no2),
           no = if_else(no > -500*0.006 & no < 0, 0, no),
           no = if_else(no < 0, NA, no),
           nox = if_else(nox > -500*0.012 & nox < 0, 0, nox),
           nox = if_else(nox < 0, NA, nox),
           so2 = if_else(so2 > -500*0.006 & so2 < 0, 0, so2),
           so2 = if_else(so2 < 0, NA, so2),
           h2s = if_else(h2s > -500*0.006 & h2s < 0, 0, h2s),
           h2s = if_else(h2s < 0, NA, h2s),
           co = if_else(co > -50000*0.008 & co < 0, 0, co),
           co = if_else(co < 0, NA, co),
           o3 = if_else(o3 > -500*0.006 & o3 < 0, 0, o3),
           o3 = if_else(o3 < 0, NA, o3),
           no = factor_eca(no, 30.00612),
           no2 = factor_eca(no2, 46.00552),
           so2 = factor_eca(so2, 64.06480),
           h2s = factor_eca(h2s, 34.08196),
           co = factor_eca(co, 28.01055),
           o3 = factor_eca(o3, 47.99820)) %>% select(-nox)
  
  
  p3 <- read.csv(
    pm, skip = 3, na.strings = "NAN") %>% 
    mutate(date = as.POSIXct(
      paste(Date, Time),
      format = "%Y-%m-%d %H:%M", tz = "Etc/GMT")) %>% 
    select(c(date,4,7)) %>% 
    rename_at(vars(names(.)),~ c(
      "date", "pm25", "pm10")) %>% 
    filter(date %in% seq(
      as.POSIXct(paste(fecha_inicio, "00:00:00"), tz = "Etc/GMT"),
      as.POSIXct(paste(fecha_fin, "23:00:00"), tz = "Etc/GMT"),
      by = "1 hour")) %>% 
    mutate_if(is.character, as.numeric) %>% 
    mutate(pm25 = if_else(pm25 >= 0, pm25, NA),
           pm10 = if_else(pm10 >= 0, pm10, NA),
           pm25 = if_else(pm25/pm10 <= 1 , pm25, NA),
           pm10 = if_else(pm25/pm10 <= 1 , pm10, NA))
  
  df <- cbind(p3, m1[,-1], g2[, -1])
  
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
    reframe(
      pm25 = mean(pm25, na.rm = T),
      pm10 = mean(pm10, na.rm = T),
      no2 = mean(no2, na.rm = T),
      so2 = mean(so2, na.rm = T),
      h2s = mean(h2s, na.rm = T),
      co = mean(co, na.rm = T),
      co_1 = mean(co_1, na.rm = T),
      o3 = max(o3, na.rm = T), .by = fecha)
  if (tipo == "save") {
    openxlsx::write.xlsx(
      lst(df,eca), paste0(estacion, ".xlsx"))
  }else if(tipo =="lista"){
    lst(df, eca) 
  }
  
}
