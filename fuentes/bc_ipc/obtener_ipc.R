library(rvest)
library(dplyr)
library(janitor)
library(tidyr)
library(stringr)

# Definir las funciones necesarias
mes_a_numeric <- function(mes) {
  meses <- c("Ene" = 1, "Feb" = 2, "Mar" = 3, "Abr" = 4, "May" = 5, "Jun" = 6,
             "Jul" = 7, "Ago" = 8, "Sep" = 9, "Oct" = 10, "Nov" = 11, "Dic" = 12)
  return(meses[mes])
}

extraer_año <- function(fecha) {
  return(as.numeric(str_extract(fecha, "\\d{4}$")))
}

cifra_comas_a_numeric <- function(valor) {
  return(as.numeric(gsub(",", "", valor)))
}

# URL de los datos
url = "https://si3.bcentral.cl/Siete/ES/Siete/Cuadro/CAP_PRECIOS/MN_CAP_PRECIOS/IPC_EMP_2023/638415285164039007"

# Extraer y procesar los datos
dato_0 <- session(url) |> 
  read_html() |> 
  html_table()

dato_1 <- dato_0[[1]] |> 
  janitor::clean_names() |> 
  select(-1)

dato_2 <- dato_1 |> 
  tidyr::pivot_longer(cols = 2:length(dato_1), names_to = "fecha", values_to = "valor") |> 
  mutate(mes = str_extract(fecha, "^\\w+(?=_)"),
         mes = mes_a_numeric(mes),
         año = extraer_año(fecha)) |> 
  mutate(valor = cifra_comas_a_numeric(valor)) |> 
  mutate(fecha = paste(año, mes, "1", sep = "-"))
