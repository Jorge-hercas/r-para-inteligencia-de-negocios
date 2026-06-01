

library(dplyr)
library(lubridate)
library(jsonlite)
library(prophet)
library(readr)

obtener_datos_rsafs <- function() {

  fecha_inicial <- "2010-01-01"
  fecha_final <- today()
  api_key <- "TU_API_KEY"

  prefijo <- "https://api.stlouisfed.org/fred/series/observations?"
  formato <- "json"
  serie <- "RSAFS"

  consulta <- paste0(
    prefijo,
    "series_id=", serie,
    "&api_key=", api_key,
    "&file_type=", formato,
    "&observation_start=", fecha_inicial,
    "&observation_end=", fecha_final
  )

  rsafs_json <- fromJSON(consulta)

  rsafs_json$observations |>
    select(date, value) |>
    mutate(
      date = as.Date(date),
      value = parse_number(as.character(value))
    ) |>
    filter(!is.na(date), !is.na(value)) |>
    arrange(date)
}

preparar_datos_prophet <- function(datos) {

  datos |>
    transmute(
      ds = as.Date(date),
      y = as.numeric(value)
    ) |>
    filter(!is.na(ds), !is.na(y)) |>
    arrange(ds)
}

ajustar_modelo_prophet <- function(prophet_df) {

  prophet(
    df = prophet_df,
    yearly.seasonality = TRUE,
    weekly.seasonality = FALSE,
    daily.seasonality = FALSE
  )
}


generar_pronostico <- function(periods = 12) {

  datos <- obtener_datos_rsafs()

  prophet_df <- preparar_datos_prophet(datos)

  modelo <- ajustar_modelo_prophet(prophet_df)

  fechas_futuras <- make_future_dataframe(
    modelo,
    periods = periods,
    freq = "month"
  )

  pronostico <- predict(modelo, fechas_futuras)

  pronostico |>
    select(ds, yhat, yhat_lower, yhat_upper) |>
    filter(ds > max(prophet_df$ds)) |>
    mutate(
      ds = as.character(ds),
      yhat = round(yhat, 2),
      yhat_lower = round(yhat_lower, 2),
      yhat_upper = round(yhat_upper, 2)
    )
}
