# Tabla de datos anuales --------------------------------------------------
tabla <- function(df, distrito){
  a <- resultante1(df)
  b <- df %>% 
    summarise_all(.funs = mean, na.rm = T) %>% 
    select(-c(date, wd, ws, pp))
  c <- data.frame(tmax = max(df$temp, na.rm = T),
                  tmin = min(df$temp, na.rm = T))
  pp <- sum(df$pp)
  cbind(a,b,c,pp)  %>% pivot_longer(cols = 1:18, names_to = "params", values_to = distrito) %>% arrange(params)
}