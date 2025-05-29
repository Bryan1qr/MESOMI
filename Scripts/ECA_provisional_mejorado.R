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
    select(c(1,3,4,5, 10, 17, 20, 27, 49, 62)) %>% 
    rename_at(vars(names(.)),~ c(
      "date", "no", "no2", "nox", "so2",
      "h2s", "co", "o3", "pm25", "pm10")) %>% 
    mutate(date = as.POSIXct(date, format = "%d/%m/%Y %H:%M", tz = "Etc/GMT")) %>% 
    filter(date %in% seq(
      as.POSIXct(paste(fecha_inicio, "00:00:00"), tz = "Etc/GMT"),
      as.POSIXct(paste(fecha_fin, "23:00:00"), tz = "Etc/GMT"),
      by = "1 hour")) %>% 
    mutate_if(is.character, as.numeric)%>% 
    mutate(no2 = if_else((no + no2)/nox >= 0.9 & (no + no2)/nox <= 1.1, no2, NA),
           no2 = if_else(no2 > -0.4 & no2 < 0, 0, no2),
           no2 = if_else(no2 < 0, NA, no2),
           no = if_else(no > -0.4 & no < 0, 0, no),
           no = if_else(no < 0, NA, no),
           nox = if_else(nox > -0.4 & nox < 0, 0, nox),
           nox = if_else(nox < 0, NA, nox),
           so2 = if_else(so2 > -1 & so2 < 0, 0, so2),
           so2 = if_else(so2 < 0, NA, so2),
           h2s = if_else(h2s > -1 & h2s < 0, 0, h2s),
           h2s = if_else(h2s < 0, NA, h2s),
           co = if_else(co > -40 & co < 0, 0, co),
           co = if_else(co < 0, NA, co),
           o3 = if_else(o3 > -0.5 & o3 < 0, 0, o3),
           o3 = if_else(o3 < 0, NA, o3),
           
           
           no = factor_eca(no, 30.00612),
           no2 = factor_eca(no2, 46.00552),
           so2 = factor_eca(so2, 64.06480),
           h2s = factor_eca(h2s, 34.08196),
           co = factor_eca(co, 28.01055),
           o3 = factor_eca(o3, 47.99820),
           
           pm25 = if_else(pm25 >= 0, pm25, NA),
           pm10 = if_else(pm10 >= 0, pm10, NA),
           pm25 = if_else(pm25/pm10 <= 1 , pm25, NA),
           pm10 = if_else(pm25/pm10 <= 1 , pm10, NA))
  
  g2[104:105, -c(9,10)] <- NA # solo para odría!! luego borrar
  g2 <- rollingMean(mydata = g2, pollutant = "o3", width = 8, new.name = "o3_8h", align = "right")
  g2 <- rollingMean(mydata = g2, pollutant = "co", width = 8, new.name = "co_8h", align = "right")
  
  g2 <- data.frame(date = seq(min(m1$date), max(m1$date), by = "1 hour")) %>%
    left_join(g2, by = "date")
  df <- cbind(m1, g2[, -1])
  eca <- df %>% 
    mutate(fecha = as.Date(date)) %>% 
    group_by(fecha) %>% 
    summarise(
      pm25 = if (sum(!is.na(pm25)) >= 18) mean(pm25, na.rm = TRUE) else NA_real_,
      pm10 = if (sum(!is.na(pm10)) >= 18) mean(pm10, na.rm = TRUE) else NA_real_,
      no2 = if (sum(!is.na(no2)) >= 18) mean(no2, na.rm = TRUE) else NA_real_,
      so2 = if (sum(!is.na(so2)) >= 18) mean(so2, na.rm = TRUE) else NA_real_,
      h2s = if (sum(!is.na(h2s)) >= 18) mean(h2s, na.rm = TRUE) else NA_real_,
      co = if (sum(!is.na(co)) >= 18) mean(co, na.rm = TRUE) else NA_real_,
      co_8h = if (sum(!is.na(co_8h)) >= 18) mean(co_8h, na.rm = TRUE) else NA_real_,
      o3_8h = if (sum(!is.na(o3_8h)) >= 18) max(o3_8h, na.rm = TRUE) else NA_real_
    )
  if (tipo == "save") {
    openxlsx::write.xlsx(
      lst(df,eca), paste0(estacion, ".xlsx"))
  }else if(tipo =="lista"){
    lst(df, eca) 
  }
  
  
}