

library(plumber)

source("modelo_prophet.R")

#* @apiTitle API de pronóstico de ventas minoristas
#* @apiDescription API construida con plumber para pronosticar la serie RSAFS de FRED mediante Prophet.

#* Verificar que la API está activa
#* @get /health
function() {
  list(
    status = "ok",
    message = "API funcionando correctamente",
    timestamp = as.character(Sys.time())
  )
}

#* Generar pronóstico de ventas minoristas
#* @param periods Número de meses a pronosticar
#* @get /forecast
function(periods = 12) {

  periods <- as.integer(periods)

  if (is.na(periods) || periods <= 0) {
    stop("El parámetro 'periods' debe ser un número entero positivo.")
  }

  pronostico <- generar_pronostico(periods = periods)

  list(
    serie = "RSAFS",
    descripcion = "Ventas minoristas mensuales",
    periods = periods,
    forecast = pronostico
  )
}
