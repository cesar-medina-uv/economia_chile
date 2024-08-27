# https://si3.bcentral.cl/Siete
# https://si3.bcentral.cl/Siete/ES/Siete/Cuadro/CAP_PRECIOS/MN_CAP_PRECIOS/IPC_EMP_2023/638415285164039007
# Precios > IPC General y medidas subyacentes, INE > Índice empalmado - 2023

library(readxl)
library(tidyverse)
#install.packages("tidyverse")

# Definir la ruta completa al archivo
file_path <- "C:\\Users\\cesar\\OneDrive\\Documentos\\GitHub\\economia_chile\\fuentes\\bc_ipc\\Cuadro_28032024131518.xlsx"

# Verificar si el archivo existe
if (file.exists(file_path)) {
  ipc <- read_excel(file_path) |> 
    rename(fecha = 1, valor = 2) |> 
    mutate(fecha = str_extract(fecha, "\\d{4}-\\d{2}-\\d{2}"),
           fecha = ymd(fecha)) |> 
    filter(!is.na(fecha)) |> 
    mutate(valor = as.numeric(valor)) |> 
    # Calcular variación mensual
    arrange(fecha) |> 
    mutate(variacion = valor/lag(valor)-1,
           variacion = replace_na(variacion, 0))
} else {
  message("El archivo no existe en la ruta especificada: ", file_path)
}

ipc |> 
  ggplot(aes(fecha, valor)) +
  geom_line()

ipc |> 
  ggplot(aes(fecha, variacion)) +
  geom_line()


# guardar ----
arrow::write_parquet(ipc, "C:\\Users\\cesar\\OneDrive\\Documentos\\GitHub\\economia_chile\\fuentes\\bc_ipc\\bc_ipc.parquet")

