# Cargar las librerías necesarias
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

# Definir la URL de la cual se van a extraer los datos
url = "https://si3.bcentral.cl/Siete/ES/Siete/Cuadro/CAP_ESTADIST_MACRO/MN_EST_MACRO_IV/PEM_ACTyDDA_IMACEC_2_2018/637807927445790326"

# Extraer la tabla de la página web
dato_0 <- session(url) |> 
  read_html() |> 
  html_table()

# Limpiar los nombres de las columnas y eliminar la primera columna
dato_1 <- dato_0[[1]] |> 
  janitor::clean_names() |> 
  select(-1)

# Transformar los datos y realizar las operaciones necesarias
dato_2 <- dato_1 |> 
  tidyr::pivot_longer(cols = 2:length(dato_1), names_to = "fecha", values_to = "valor") |> 
  mutate(mes = str_extract(fecha, "^\\w+(?=_)"),
         mes = mes_a_numeric(mes),
         año = extraer_año(fecha)) |> 
  mutate(valor = cifra_comas_a_numeric(valor)) |> 
  mutate(fecha = paste(año, mes, "1", sep = "-"))

# Guardar los datos transformados en un archivo CSV en la ruta especificada
write.csv(dato_2, "C:\\Users\\cesar\\OneDrive\\Documentos\\GitHub\\economia_chile\\fuentes\\bc_imacec\\datos_imacec.csv", row.names = FALSE)
