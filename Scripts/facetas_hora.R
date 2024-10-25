# Gráfico con facetas -----------------------------------------------------
faceta <- function(lista){
  parametros <- c(
    co_1 = "CO",
    h2s = "H[2]*S",
    no2 = "NO[2]",
    o3 = "O[3]",
    pm10 = "PM[10]",
    pm25 = "PM[2.5]",
    so2 = 'SO[2]')
  df1 <- lista$df
  
  df1 <- df1 %>% 
    mutate(dia = format(date, "%A"),
           dia1 = format(date, "%W"))
  
  semana <- unique(df1$dia1)

  df1 %>% 
    mutate(dia1 = case_when(dia1 == semana[1] ~ "Semana 1",
                            dia1 == semana[2] ~ "Semana 2",
                            dia1 == semana[3] ~ "Semana 3",
                            dia1 == NA ~ NA),
           dia2 = case_when(dia == "Monday" ~ "Lunes",
                            dia == "Tuesday" ~ "Martes",
                            dia == "Wednesday" ~ "Miércoles",
                            dia == "Thursday" ~ "Jueves",
                            dia == "Friday" ~ "Viernes",
                            dia == "Saturday" ~ "Sábado",
                            dia == "Sunday" ~ "Domingo",
                            dia == NA ~ NA),
           date2 = format(date, format = "%H"),
           fecha = format(date, format = "%Y-%m-%d"),
           dia2 = fct_relevel(
             dia2, "Lunes","Martes", "Miércoles",
             "Jueves", "Viernes", "Sábado", "Domingo")) %>%
    select(-c(hr, pres, pp, temp, wd, ws, rad, no, co)) %>%
    pivot_longer(values_to = "val", names_to = "param", cols = 2:8) %>% 
    ggplot(aes(x = date2, y = val, group = dia1)) +
    geom_smooth(aes(fill = dia1), alpha = 0.25, color = "gray",
                lty = 2, lwd = 0.01) +
    geom_line(aes(color = dia1)) +
    scale_x_discrete(
      breaks = sprintf("%02d", seq(0,23,5) ))+
    scale_fill_manual(values = c("orange", "darkblue", "darkgreen")) +
    scale_color_manual(values = c("orange", "darkblue", "darkgreen")) +
    labs(x = "Hora del día", y = expression("Concentración (ug/m"^{3}*")"),
         color = "", fill = "") +
    facet_grid(param~dia2, , scales = "free_y",
               labeller = labeller(param = as_labeller(parametros, label_parsed))) +
    theme_bw()+
    theme(legend.position = "top",
          strip.background = element_rect(fill = "whitesmoke"),
          strip.text = element_text(
            color = "black", face = "bold", size = 16),
          legend.text = element_text(size = 16),
          axis.title = element_text(size = 15, face = "bold"),
          axis.text.y = element_text(size = 12),
          axis.text.x = element_text(size = 11))
}