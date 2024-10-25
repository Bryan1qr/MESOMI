library(lubridate)

# Función para crear la tabla diaria con porcentaje de datos existentes
crear_tabla_diaria <- function(datos_horarios) {
  datos_diarios <- datos_horarios %>%
    # Agrupar por la fecha (sin hora)
    group_by(date = as.Date(date)) %>%
    # Calcular el porcentaje de datos no NA para cada parámetro
    summarise(across(everything(), ~ sum(!is.na(.)) / n() * 100, .names = "%{col}"),
              .groups = "drop")
  
  return(datos_diarios)
}
