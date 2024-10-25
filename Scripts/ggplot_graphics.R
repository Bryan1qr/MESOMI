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
    axis.text.x = element_text(face = "bold", size = 7),
    plot.tag = element_text(face = "bold"),
    legend.direction = "horizontal", legend.background = element_blank())
  
  fechas <- with(df$df,
                 format(seq(min(date), max(date),
                            by = "1 day"), format = "%d"))
  
  df1 <- lista$df
  
  a <- df1 %>% ggplot(
    aes(x = date, y = pm25)) +
    geom_line(color = color_linea) +
    geom_hline(aes(color = "OMS", yintercept = 15),lwd=1, lty = 1) +
    geom_hline(aes(color = "ECA", yintercept = 50),lwd=1, lty = 2)+
    labs(x = "",
         y = expression("PM"[2.5]*" (ug/m"^{3}*")"),
         title = "Material particulado menor a 2.5 micras",
         subtitle = "Promedio horario", tag = "A",
         color = "Estándar") +
    scale_x_continuous(
      breaks = seq(min(df$df$date),
                   max(df$df$date),
                   by = "1 day"), labels = fechas) +
    scale_color_manual(values = c("ECA" = "blue",
                                  "OMS" = "green"),
                       labels = c("ECA",
                                  "OMS"))+
    temita + theme(legend.position = "none")
    

  
  b <- df1 %>% ggplot(
    aes(x = date, y = pm10)) +
    geom_line(color = color_linea) +
    geom_hline(aes(color = "OMS", yintercept = 45),lwd=1, lty = 1) +
    geom_hline(aes(color = "ECA", yintercept = 100),lwd=1, lty = 2)+
    labs(x = "",
         y = expression("PM"[10]*" (ug/m"^{3}*")"),
         title = "Material particulado menor a 10 micras",
         subtitle = "Promedio horario", tag = "B",
         color = "Estándar") +
    scale_x_continuous(
      breaks = seq(min(df$df$date),
                   max(df$df$date),
                   by = "1 day"), labels = fechas) +
    scale_color_manual(values = c("ECA" = "blue",
                                  "OMS" = "green"),
                       labels = c("ECA",
                                  "OMS"))+
    temita + theme(legend.position = "none")
  

  c <- df1 %>% ggplot(
    aes(x = date, y = no2)) +
    geom_line(color = color_linea) +
    geom_hline(aes(color = "OMS", yintercept = 25),lwd=1, lty = 1) +
    geom_hline(aes(color = "ECA", yintercept = 200),lwd=1, lty = 2)+
    labs(x = "",
         y = expression("NO"[2]*" (ug/m"^{3}*")"),
         title = "Dióxido de nitrógeno",
         subtitle = "Promedio horario", tag = "C",
         color = "Estándar") +
    scale_x_continuous(
      breaks = seq(min(df$df$date),
                   max(df$df$date),
                   by = "1 day"), labels = fechas) +
    scale_color_manual(values = c("ECA" = "blue",
                                  "OMS" = "green"),
                       labels = c("ECA",
                                  "OMS"))+
    temita + theme(legend.position = "none")


  d <- df1 %>% ggplot(
    aes(x = date, y = h2s)) +
    geom_line(color = color_linea) +
    geom_hline(aes(color = "ECA", yintercept = 150),lwd=1, lty = 2)+
    labs(x = "",
         y = expression("H"[2]*"S (ug/m"^{3}*")"),
         title = "Sulfuro de hidrógeno",
         subtitle = "Promedio horario", tag = "F", 
         color = "Estándar") +
    scale_x_continuous(
      breaks = seq(min(df$df$date),
                   max(df$df$date),
                   by = "1 day"), labels = fechas) +
    facet_zoom(ylim = c(min(df$df$h2s, na.rm = T) - 1,
                        max(df$df$h2s, na.rm = T) + 1),
               zoom.size = 1, split = T)+
    scale_color_manual(values = c("ECA" = "blue",
                                  "OMS" = "green"),
                       labels = c("ECA",
                                  "OMS"))+
    temita + theme(legend.position = "none")
  
  e <- df1 %>% ggplot(
    aes(x = date, y = o3)) +
    geom_line(color = "red") +
    geom_hline(aes(color = "OMS", yintercept = 100),lwd=1, lty = 1) +
    geom_hline(aes(color = "ECA (8h)", yintercept = 100),lwd=1, lty = 3)+
    labs(x = "",
         y = expression("O"[3]*" (ug/m"^{3}*")"),
         title = "Ozono troposférico",
         subtitle = "Promedio horario", tag = "D",
         color = "Estándar") +
    scale_x_continuous(
      breaks = seq(min(df$df$date),
                   max(df$df$date),
                   by = "1 day"), labels = fechas) +
    scale_color_manual(values = c("ECA (8h)" = "darkblue",
                                  "OMS" = "green"),
                       labels = c("ECA (8h)",
                                  "OMS")) +
    temita + theme(legend.position = "none")

  f <- df1 %>% ggplot(
    aes(x = date)) +
    geom_line(aes(y = co, color = "co")) +
    geom_line(aes(y = co_1, color = "co_1")) +
    geom_hline(aes(yintercept = 10000,
                   color="ECA(8h)"),lwd=1, lty = 3) +
    geom_hline(aes(yintercept = 30000, color = "ECA"),lwd=1, lty = 2) +
    geom_hline(aes(color = "OMS", yintercept = 35000),lwd=1, lty = 1) +
    geom_hline(aes(color = "OMS(8h)", yintercept = 10000),lwd=1, lty = 3) +
    labs(x = "",
         y = expression("CO"*" (ug/m"^{3}*")"),
         title = "Monóxido de carbono",
         subtitle = "Promedio horario", tag = "G",
         color = "Leyenda") +
    scale_x_continuous(
      breaks = seq(min(df$df$date),
                   max(df$df$date),
                   by = "1 day"), labels = fechas) +
    scale_color_manual(values = c("co" = color_linea,
                                  "co_1" = "red", 
                                  "ECA" = "blue",
                                  "ECA(8h)" = "darkblue",
                                  "OMS" = "green",
                                  "OMS(8h)" = "darkgreen"),
                       labels = c("Media aritmética", "Media móvil","ECA",
                                  "ECA(8h)","OMS", "OMS(8h)"))+
    facet_zoom(ylim = c(min(df$df$co, na.rm = T) - 50,
                        max(df$df$co, na.rm = T) + 100),
               zoom.size = 1, split = T) +
    temita + theme(legend.position = "bottom",
                   legend.title = element_text(face = "bold"))

  g <- df1 %>% ggplot(
    aes(x = date, y = so2)) +
    geom_line(color = color_linea) +
    geom_hline(aes(color = "OMS", yintercept = 40),lwd=1, lty = 1) +
    geom_hline(aes(color = "ECA", yintercept = 250),lwd=1, lty = 2)+
    labs(x = "",
         y = expression("SO"[2]*" (ug/m"^{3}*")"),
         title = "Dióxido de azufre",
         subtitle = "Promedio horario", tag = "E",
         color = "Leyenda") +
    scale_x_continuous(
      breaks = seq(min(df$df$date),
                   max(df$df$date),
                   by = "1 day"), labels = fechas) +
    scale_color_manual(values = c("ECA" = "blue",
                                  "OMS" = "green"),
                       labels = c("ECA",
                                  "OMS"))+
    facet_zoom(ylim = c(min(df$df$so2, na.rm = T) - 1,
                        max(df$df$so2, na.rm = T) + 1),
               zoom.size = 1, split = T) +
    temita + theme(legend.position = "none")


  ploteo <- (a + b) / (c + e) / g / d / f
  ploteo
}