barras_eca <- function(lista, coloreo, ruta){
  
  # Tema de los gráficos:
  temita <- theme(panel.border = element_rect(
    color = "black", fill = "transparent"), 
    plot.background = element_rect(fill = "white", color = "gray"),
    panel.background = element_blank(),
    plot.title = element_text(
      hjust = 0.5,face = "bold", size = 13),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.major = element_line(colour = "gray"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 12),
    plot.tag = element_text(face = "bold"),
    legend.direction = "horizontal",
    legend.background = element_blank())
  
  db <- lista$eca %>% 
    mutate(fecha = as.Date(fecha, "%d-%B"),
           fecha = format(fecha, format = "%d"))
  
  
  db1 <- db %>% ggplot(
    aes(x = fecha, y = pm25)) +
    geom_bar(fill = coloreo, stat = "identity") +
    geom_hline(yintercept = 50,lwd=1, lty = 2,
               color="hotpink") +
    geom_label(aes(x = length(fecha)/2,
                   y = 49,label = "ECA"),
               fill = "#f9f0ca") +
    geom_text(size = 3.5,aes(label = round(pm25,2)),
              angle = 90, color = "black",
              nudge_y = -max(db$pm25, na.rm = T)/4) +
    labs(x = "",
         y = expression("PM"[2.5]*" (ug/m"^{3}*")"),
         title = "Material particulado menor a 2.5 micras",
         subtitle = "Promedio diario", tag = "A") +
    temita
  
  
  db2 <- db %>% ggplot(
    aes(x = fecha, y = pm10)) +
    geom_bar(fill = coloreo, stat = "identity") +
    geom_hline(yintercept = 100,lwd=1, lty = 2,
               color="hotpink") +
    geom_label(aes(x = length(fecha)/2,
                   y =  98,label = "ECA"),
               fill = "#f9f0ca") +
    geom_text(size = 3.5,aes(label = round(pm10,2)),
              angle = 90, color = "black",
              nudge_y = -max(db$pm10, na.rm = T)/4) +
    labs(x = "",
         y = expression("PM"[10]*" (ug/m"^{3}*")"),
         title = "Material particulado menor a 10 micras",
         subtitle = "Promedio diario", tag = "B") +
    temita
  
  
  db3 <- db %>% ggplot(
    aes(x = fecha, y = no2)) +
    geom_bar(fill = coloreo, stat = "identity") +
    geom_hline(yintercept = 200,lwd=1, lty = 2,
               color="hotpink") +
    geom_label(aes(x = length(fecha)/2,
                   y =  198,label = "ECA"),
               fill = "#f9f0ca") +
    geom_text(size = 3.5,aes(label = round(no2,2)),
              angle = 90, color = "black",
              nudge_y = max(db$no2, na.rm = T)/4) +
    labs(x = "",
         y = expression("NO"[2]*" (ug/m"^{3}*")"),
         title = "Dióxido de nitrógeno",
         subtitle = "Promedio diario", tag = "C") +
    temita
  
  db4 <- db %>% ggplot(
    aes(x = fecha, y = h2s)) +
    geom_bar(fill = coloreo, stat = "identity") +
    geom_hline(yintercept = 150,lwd=1, lty = 2,
               color="hotpink") +
    geom_label(aes(x = length(fecha)/2,
                   y =  148,label = "ECA"),
               fill = "#f9f0ca") +
    geom_text(size = 3.5,aes(label = round(h2s,2)),
              angle = 90, color = "black",
              nudge_y = -max(db$h2s, na.rm = T)/4) +
    labs(x = "",
         y = expression("H"[2]*"S (ug/m"^{3}*")"),
         title = "Sulfuro de hidrógeno",
         subtitle = "Promedio diario", tag = "D") +
    temita
  
  db5 <- db %>% ggplot(
    aes(x = fecha, y = o3)) +
    geom_bar(fill = coloreo, stat = "identity") +
    geom_hline(yintercept = 100,lwd=1, lty = 2,
               color="hotpink") +
    geom_label(aes(x = length(fecha)/2,
                   y =  98,label = "ECA"),
               fill = "#f9f0ca") +
    geom_text(size = 3.5,aes(label = round(o3,2)),
              angle = 90, color = "black",
              nudge_y = -max(db$o3, na.rm = T)/4) +
    labs(x = "",
         y = expression("O"[3]*" (ug/m"^{3}*")"),
         title = "Ozono troposférico",
         subtitle = "Promedio diario", tag = "D") +
    temita
  
  db6 <- db %>% ggplot(
    aes(x = fecha, y = so2)) +
    geom_bar(fill = coloreo, stat = "identity") +
    geom_hline(yintercept = 250,lwd=1, lty = 2,
               color="hotpink") +
    geom_label(aes(x = length(fecha)/2,
                   y =  248,label = "ECA"),
               fill = "#f9f0ca") +
    geom_text(size = 3.5,aes(label = round(so2,2)),
              angle = 90, color = "black",
              nudge_y = -max(db$so2, na.rm = T)/4) +
    labs(x = "",
         y = expression("SO"[2]*" (ug/m"^{3}*")"),
         title = "Dióxido de azufre",
         subtitle = "Promedio diario", tag = "F") +
    temita
  
  db7 <- db %>% ggplot(
    aes(x = fecha, y = co)) +
    geom_bar(fill = coloreo, stat = "identity") +
    geom_hline(yintercept = 10000,lwd=1, lty = 2,
               color="hotpink") +
    geom_label(aes(x = length(fecha)/2,
                   y =  9998,label = "ECA"),
               fill = "#f9f0ca") +
    geom_text(size = 3.5,aes(label = round(co,2)),
              angle = 90, color = "black",
              nudge_y = -max(db$co, na.rm = T)/4) +
    labs(x = "",
         y = expression("CO (ug/m"^{3}*")"),
         title = "Monóxido de carbono",
         subtitle = "Promedio diario", tag = "G") +
    temita
  
  
  
  (db1 | db2)/(db3 | db5)
  # grafs2 <- (db4 | db6)/ db7
}
