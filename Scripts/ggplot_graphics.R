gg_grafico <- function(lista, estacion, file_name, color_linea){

  temita <- theme(panel.border = element_rect(
    color = "black", fill = "transparent"), 
    plot.background = element_rect(fill = "white"),
    panel.background = element_blank(),
    plot.title = element_text(
      hjust = 0.5,face = "bold", size = 13),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.major = element_line(colour = "gray"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(face = "bold", size = 8, angle = 90),
    plot.tag = element_text(face = "bold"),
    legend.direction = "horizontal", legend.background = element_blank())
  
  fechas <- with(df$df,
                 format(seq(min(date), max(date),
                            by = "1 day"), format = "%b-%d"))
  
  df1 <- lista$df
  
  a <- df1 %>% ggplot(
    aes(x = date, y = pm25)) +
    geom_line(color = color_linea) +
    geom_hline(yintercept = 50,lwd=1, lty = 2,
               color="hotpink") +
    geom_label(aes(x = mean(df$df$date, na.rm = T),
                   y = 49,label = "ECA"),
               fill = "#f9f0ca") +
    labs(x = "",
         y = expression("PM"[2.5]*" (ug/m"^{3}*")"),
         title = "Material particulado menor a 2.5 micras",
         subtitle = "Promedio horario", tag = "A") +
    scale_x_continuous(
      breaks = seq(min(df$df$date),
                   max(df$df$date),
                   by = "1 day"), labels = fechas) + 
    temita
  
  b <- df1 %>% ggplot(
    aes(x = date, y = pm10)) +
    geom_line(color = color_linea) +
    geom_hline(yintercept = 100,lwd=1, lty = 2,
               color="hotpink") +
    geom_label(aes(x = mean(df$df$date, na.rm = T),
                   y = 99,label = "ECA"),
               fill = "#f9f0ca") +
    labs(x = "",
         y = expression("PM"[10]*" (ug/m"^{3}*")"),
         title = "Material particulado menor a 10 micras",
         subtitle = "Promedio horario", tag = "B") +
    scale_x_continuous(
      breaks = seq(min(df$df$date),
                   max(df$df$date),
                   by = "1 day"), labels = fechas) + 
    temita
  
  c <- df1 %>% ggplot(
    aes(x = date, y = no2)) +
    geom_line(color = color_linea) +
    geom_hline(yintercept = 200,lwd=1, lty = 2,
               color="hotpink") +
    geom_label(aes(x = mean(df$df$date, na.rm = T),
                   y = 199,label = "ECA"),
               fill = "#f9f0ca") +
    labs(x = "",
         y = expression("NO"[2]*" (ug/m"^{3}*")"),
         title = "Dióxido de nitrógeno",
         subtitle = "Promedio horario", tag = "C") +
    scale_x_continuous(
      breaks = seq(min(df$df$date),
                   max(df$df$date),
                   by = "1 day"), labels = fechas) + 
    temita
  
  
  d <- df1 %>% ggplot(
    aes(x = date, y = h2s)) +
    geom_line(color = color_linea) +
    geom_hline(yintercept = 150,lwd=1, lty = 2,
               color="hotpink") +
    geom_label(aes(x = mean(df$df$date, na.rm = T),
                   y = 148,label = "ECA"),
               fill = "#f9f0ca") +
    labs(x = "",
         y = expression("H"[2]*"S (ug/m"^{3}*")"),
         title = "Sulfuro de hidrógeno",
         subtitle = "Promedio horario", tag = "E") +
    scale_x_continuous(
      breaks = seq(min(df$df$date),
                   max(df$df$date),
                   by = "1 day"), labels = fechas) + 
    temita
  
  
  e <- df1 %>% ggplot(
    aes(x = date, y = o3)) +
    geom_line(color = color_linea) +
    geom_hline(yintercept = 100,lwd=1, lty = 2,
               color="hotpink") +
    geom_label(aes(x = mean(df$df$date, na.rm = T),
                   y = 98,label = "ECA"),
               fill = "#f9f0ca") +
    labs(x = "",
         y = expression("O"[3]*" (ug/m"^{3}*")"),
         title = "Ozono troposférico",
         subtitle = "Promedio horario", tag = "E") +
    scale_x_continuous(
      breaks = seq(min(df$df$date),
                   max(df$df$date),
                   by = "1 day"), labels = fechas) + 
    facet_zoom(ylim = c(0, 55), zoom.size = 1, split = T) +
    temita
  
  f <- df1 %>% ggplot(
    aes(x = date, y = co_1)) +
    geom_line(aes(color = "co_1")) +
    geom_hline(yintercept = 30000,lwd=1, lty = 2,
               color="hotpink") +
    geom_line(aes(x = date, y = co, color = "co")) +
    geom_hline(yintercept = 10000,lwd=1, lty = 2,
               color="hotpink") +
    geom_label(aes(x = mean(df$df$date, na.rm = T),
                   y = 29050),
               fill = "#f9f0ca",label = "ECA de 1 hora (media aritmética)") +
    geom_label(aes(x = mean(df$df$date, na.rm = T),
                   y = 9990),
               fill = "#f9f0ca",label = "ECA de 8 horas (media móvil)") +
    labs(x = "",
         y = expression("CO"*" (ug/m"^{3}*")"),
         title = "Monóxido de carbono",
         subtitle = "Promedio horario", tag = "F") +
    scale_x_continuous(
      breaks = seq(min(df$df$date),
                   max(df$df$date),
                   by = "1 day"), labels = fechas) + 
    scale_color_manual(name = "",values = c("co_1" = color_linea, "co" = "red"),
                       labels = c("Media móvil", "Media aritmética")) +
    facet_zoom(ylim = c(0, 1500), zoom.size = 1, split = T) +
    temita + theme(legend.position = c(0.25, 0.85))
  
  g <- df1 %>% ggplot(
    aes(x = date, y = so2)) +
    geom_line(color = color_linea) +
    geom_hline(yintercept = 250,lwd=1, lty = 2,
               color="hotpink") +
    geom_label(aes(x = mean(df$df$date, na.rm = T),
                   y = 248,label = "ECA"),
               fill = "#f9f0ca") +
    labs(x = "",
         y = expression("SO"[2]*" (ug/m"^{3}*")"),
         title = "Dióxido de azufre",
         subtitle = "Promedio horario", tag = "G") +
    scale_x_continuous(
      breaks = seq(min(df$df$date),
                   max(df$df$date),
                   by = "1 day"), labels = fechas) + 
    facet_zoom(ylim = c(0, 10), zoom.size = 1, split = T) +
    temita
  
  # Primeros gráficos:
  diarios <- (a | b) / (c | d) +
    plot_annotation(
      title = "Estación de calidad de aire (mes de julio)",
      subtitle = estacion,
      theme = theme(
        plot.title = element_text(size = 35, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 25, face = "italic", hjust = 0.5),
        plot.background = element_rect(fill = "#f9f0ca")
      )
    )
  
  ggsave(filename = paste0(file_name, ".png"),
         plot = diarios,
         width = 350,
         height = 220, units = "mm", dpi = 400)
  
  # Segundos gráficos:
  diarios2 <- e / f / g +
    plot_annotation(
      title = "Estación de calidad de aire (mes de julio)",
      subtitle = estacion,
      theme = theme(
        plot.title = element_text(size = 35, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 25, face = "italic", hjust = 0.5),
        plot.background = element_rect(fill = "#f9f0ca")
      )
    )
  
  ggsave(filename = paste0(file_name, "2.png"),
         plot = diarios2,
         width = 350,
         height = 250, units = "mm", dpi = 400)
  
# Gráfico con facetas -----------------------------------------------------
  parametros <- c(
    co_1 = "CO",
    h2s = "H[2]*S",
    no2 = "NO[2]",
    o3 = "O[3]",
    pm10 = "PM[10]",
    pm25 = "PM[2.5]",
    so2 = 'SO[2]')

  df1 %>% 
    mutate(dia = format(date, "%A"),
           dia1 = format(date, "%W"),
           dia1 = case_when(dia1 == "29" ~ "Semana 1",
                            dia1 == "30" ~ "Semana 2",
                            dia1 == "31" ~ "Semana 3",
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
          axis.text.x = element_text(size = 11)) -> fct1
  
  ggsave(filename = paste0(file_name, "3.png"),
         plot = fct1, width = 250,
         height = 300, units = "mm", dpi = 600)
}