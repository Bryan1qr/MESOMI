aqi_index <- function(df){
  
  factor_usepa <- function(g,h){
    ((273.15 + 25) * 22.41*g)/(273.15*h)
  }
  
  df <- df %>% 
    mutate(o3 = factor_usepa(o3, 47.99820)/1000,
           no2 = factor_usepa(no2, 46.00552),
           so2 = factor_usepa(so2, 64.06480),
           co_1 = factor_usepa(co_1, 28.01055)/1000)
    
  
  aqi_tabla <- tibble(category = c(
    "Bueno", "Moderado", "No saludable para grupos sensibles",
    "No saludable", "Muy No saludable","Peligroso"),
    min_pm10 = c(0, 55, 155, 255, 355, 425),
    max_pm10 = c(54, 154, 254, 354, 424, Inf),
    min_pm25 = c(0, 12.1, 35.5, 55.5, 150.4, 250.4),
    max_pm25 = c(12, 35.4, 55.4, 150.3, 250.3, Inf),
    min_o3 = c(0, 0.055, 0.071, 0.086, 0.106, 0.201),
    max_o3 = c(0.054, 0.070, 0.085, 0.105, 0.200, Inf),
    min_no2 = c(0, 53, 100, 360, 650, 1250),
    max_no2 = c(52, 100, 359, 649, 1249, Inf),
    min_so2 = c(0, 35, 75, 185, 304, 605),
    max_so2 = c(34, 75, 184, 303, 604, Inf),
    min_co = c(0, 4.4, 9.4, 12.4, 15.4, 30.4),
    max_co = c(4.3, 9.3, 12.3, 15.3, 30.3, Inf),
    min_aqi = c(0, 51, 101, 151, 201, 301),
    max_aqi = c(50, 100, 150, 200, 300, 500))
  
  truncc <- function(x, n){
    as.integer(trunc(x*10^n))/10^n
  }
  
  pm10 <- truncc(df$pm10, 0)
  pm25 <- truncc(df$pm25, 1)
  o3 <- truncc(df$o3, 3)
  no2 <- truncc(df$no2, 0)
  so2 <- truncc(df$so2, 0)
  co <- truncc(df$co_1, 1)
  
  # pm10
  tab1 <- c()
  for (i in seq_along(pm10)) {
    if (is.na(pm10[i])) {
      tab1[[i]] <- tibble(min_pm10 = NA, max_pm10 = NA, min_aqi = NA, max_aqi = NA)
    } else {
      tab1[[i]] <- aqi_tabla %>%
        select(min_pm10, max_pm10, min_aqi, max_aqi, category) %>%
        filter(pm10[i] >= min_pm10 & pm10[i] <= max_pm10) %>%
        slice(1)
    }
  }
  
  # pm25
  tab2 <- c()
  for (i in seq_along(pm25)) {
    if (is.na(pm25[i])) {
      tab2[[i]] <- tibble(min_pm25 = NA, max_pm25 = NA, min_aqi = NA, max_aqi = NA)
    } else {
      tab2[[i]] <- aqi_tabla %>%
        select(min_pm25, max_pm25, min_aqi, max_aqi, category) %>%
        filter(pm25[i] >= min_pm25 & pm25[i] <= max_pm25) %>%
        slice(1)
    }
  }
  
  # o3
  tab3 <- c()
  for (i in seq_along(o3)) {
    if (is.na(o3[i])) {
      tab3[[i]] <- tibble(min_o3 = NA, max_o3 = NA, min_aqi = NA, max_aqi = NA)
    } else {
      tab3[[i]] <- aqi_tabla %>%
        select(min_o3, max_o3, min_aqi, max_aqi, category) %>%
        filter(o3[i] >= min_o3 & o3[i] <= max_o3) %>%
        slice(1)
    }
  }
  
  # no2
  tab4 <- c()
  for (i in seq_along(no2)) {
    if (is.na(no2[i])) {
      tab4[[i]] <- tibble(min_no2 = NA, max_no2 = NA, min_aqi = NA, max_aqi = NA)
    } else {
      tab4[[i]] <- aqi_tabla %>%
        select(min_no2, max_no2, min_aqi, max_aqi, category) %>%
        filter(no2[i] >= min_no2 & no2[i] <= max_no2) %>%
        slice(1)
    }
  }
  
  # so2
  tab5 <- c()
  for (i in seq_along(so2)) {
    if (is.na(so2[i])) {
      tab5[[i]] <- tibble(min_so2 = NA, max_so2 = NA, min_aqi = NA, max_aqi = NA)
    } else {
      tab5[[i]] <- aqi_tabla %>%
        select(min_so2, max_so2, min_aqi, max_aqi, category) %>%
        filter(so2[i] >= min_so2 & so2[i] <= max_so2) %>%
        slice(1)
    }
  }
  
  # co
  tab6 <- c()
  for (i in seq_along(co)) {
    if (is.na(co[i])) {
      tab6[[i]] <- tibble(min_co = NA, max_co = NA, min_aqi = NA, max_aqi = NA)
    } else {
      tab6[[i]] <- aqi_tabla %>%
        select(min_co, max_co, min_aqi, max_aqi, category) %>%
        filter(co[i] >= min_co & co[i] <= max_co) %>%
        slice(1)
    }
  }
  
  indexx <- function(z){
    z %>% 
      mutate(aqi_ind = (
        ((.[[6]] - .[[1]])/(.[[2]] - .[[1]])) * (.[[4]] - .[[3]]) + .[[3]]))
  }

  pm__10 <- bind_rows(tab1) %>% 
    mutate(val = pm10, tipo = "pm10", fecha = df$fecha) %>% 
    indexx() %>% select(fecha, tipo, category, val, aqi_ind)
  
  pm__25 <- bind_rows(tab2) %>% 
    mutate(val = pm25, tipo = "pm25", fecha = df$fecha) %>% 
    indexx() %>% select(fecha, tipo, category, val, aqi_ind)
  
  o__3 <- bind_rows(tab3) %>% 
    mutate(val = o3, tipo = "o3", fecha = df$fecha) %>% 
    indexx() %>% select(fecha, tipo, category, val, aqi_ind)
  
  no__2 <- bind_rows(tab4) %>% 
    mutate(val = no2, tipo = "no2", fecha = df$fecha) %>% 
    indexx() %>% select(fecha, tipo, category, val, aqi_ind)
  
  so__2 <- bind_rows(tab5) %>% 
    mutate(val = so2, tipo = "so2", fecha = df$fecha) %>% 
    indexx() %>% select(fecha, tipo, category, val, aqi_ind)
  
  co__0 <- bind_rows(tab6) %>% 
    mutate(val = co, tipo = "co", fecha = df$fecha) %>% 
    indexx() %>% select(fecha, tipo, category, val, aqi_ind)
  
  rbind(pm__10, pm__25, o__3, no__2, so__2, co__0)
}
