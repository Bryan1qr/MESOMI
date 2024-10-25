# Función para obtener la dirección de viento vectorial promedio ----------

resultante1 <- function(df){
  a <- windRose(df, plot = FALSE)
  a$data %>% 
    filter(freqs == max(freqs)) %>%
    select(mean.wd) %>% pull() + 360 -> wdprom
  
  a$data %>% 
    filter(freqs == max(freqs)) %>%
    select(panel.fun) %>% pull() %>% as.numeric() -> wsprom
  
  data.frame(wdprom, wsprom)
}
