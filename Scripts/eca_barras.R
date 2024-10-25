barras_eca <- function(lista, coloreo, ruta){
  
  # Tema de los gráficos:
  temita <- theme(panel.border = element_rect(
    color = "black", fill = "transparent"), 
    plot.background = element_rect(fill = "white", color = "white"),
    panel.background = element_blank(),
    plot.title = element_text(
      hjust = 0.5,face = "bold", size = 13),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.major.y = element_line(colour = "gray"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 12),
    plot.tag = element_text(face = "bold"),
    legend.background = element_blank())
  
  db <- lista$eca %>% 
    mutate(fecha = as.Date(fecha, "%d-%B"),
           fecha = format(fecha, format = "%d"),
           fecha = factor(fecha, levels = unique(fecha)))
  
  
  db1 <- db %>% ggplot(
    aes(x = fecha, y = pm25)) +
    geom_bar(fill = coloreo, stat = "identity") +
    geom_hline(aes(yintercept = 15, color = "OMS"),lwd=1, lty = 1) +
    geom_hline(aes(yintercept = 50, color = "ECA"),lwd=1, lty = 2) +
    geom_text(size = 3.5,aes(label = round(pm25,2)),
              angle = 90, color = "black",
              nudge_y = max(db$pm25, na.rm = T)/6, fontface = "bold") +
    labs(x = "",
         y = expression("PM"[2.5]*" (ug/m"^{3}*")"),
         title = "Material particulado menor a 2.5 micras",
         subtitle = "Promedio diario", tag = "A",
         color = "Estándar") + 
    scale_color_manual(values = c("ECA" = "blue",
                                  "OMS" = "green")) +
    temita
  
  
  db2 <- db %>% ggplot(
    aes(x = fecha, y = pm10)) +
    geom_bar(fill = coloreo, stat = "identity") +
    geom_hline(aes(yintercept = 45,
                   color="OMS"),lwd=1, lty = 1) +
    geom_hline(aes(yintercept = 100,
               color="ECA"),lwd=1, lty = 2) +
    geom_text(size = 3.5,aes(label = round(pm10,2)),
              angle = 90, color = "black",
              nudge_y = max(db$pm10, na.rm = T)/6, fontface = "bold") +
    labs(x = "",
         y = expression("PM"[10]*" (ug/m"^{3}*")"),
         title = "Material particulado menor a 10 micras",
         subtitle = "Promedio diario", tag = "B",
         color = "Estándar") +
    scale_color_manual(values = c("ECA" = "blue",
                                  "OMS" = "green")) +
    temita
  
  
  db3 <- db %>% ggplot(
    aes(x = fecha, y = no2)) +
    geom_bar(fill = coloreo, stat = "identity") +
    geom_hline(aes(yintercept = 25,
                   color="OMS"),lwd=1, lty = 1) +
    geom_hline(aes(yintercept = 200,
                   color="ECA"),lwd=1, lty = 2) +
    geom_text(size = 3.5,aes(label = round(no2,2)),
              angle = 90, color = "black",
              nudge_y = max(db$no2, na.rm = T)/6, fontface = "bold") +
    labs(x = "",
         y = expression("NO"[2]*" (ug/m"^{3}*")"),
         title = "Dióxido de nitrógeno",
         subtitle = "Promedio diario", tag = "C",
         color = "Estándar") +
    scale_color_manual(values = c("ECA" = "blue",
                                  "OMS" = "green")) +
    temita
  
  db5 <- db %>% ggplot(
    aes(x = fecha, y = o3)) +
    geom_bar(fill = coloreo, stat = "identity") +
    geom_hline(aes(yintercept = 100,
                   color="OMS"),lwd=1, lty = 1) +
    geom_hline(aes(yintercept = 100,
                   color="ECA"),lwd=1, lty = 2) +
    geom_text(size = 3.5,aes(label = round(o3,2)),
              angle = 90, color = "black",
              nudge_y = max(db$o3, na.rm = T)/6, fontface = "bold") +
    labs(x = "",
         y = expression("O"[3]*" (ug/m"^{3}*")"),
         title = "Ozono troposférico",
         subtitle = "Promedio diario", tag = "D",
         color = "Estándar") +
    scale_color_manual(values = c("ECA" = "blue",
                                  "OMS" = "green")) +
    temita
  
  ((db1 | db2)/(db3 | db5)) + plot_layout(guides = 'collect') & 
    theme(legend.position = 'bottom')
  # grafs2 <- (db4 | db6)/ db7
}
