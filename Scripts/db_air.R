db_air <- function(
    año,mes,inicio,fin,ruta){
  db <- read.delim(
    ruta, na.strings = c("***", "*", "**"))
  db$date <- seq(
    as.POSIXct(
      paste0(año,'-' ,mes,'-',
             inicio, " 00:00:00"),
      format = "%Y-%m-%d %H:%M:%S"),
    as.POSIXct(
      paste0(año,'-' ,mes,'-',
             fin, " 23:00:00"),
      format = "%Y-%m-%d %H:%M:%S"),
    by = "1 hour")
  # 
  # names(db) <- c("temp",'hr',
  #                'wd','ws', "date")
  db
}