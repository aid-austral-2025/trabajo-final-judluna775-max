# ─────────────────────────────────────────
# 01_limpieza_toneladas.R
# Lee el Excel "toneladas historicas.xlsx" y genera tabla larga
# ─────────────────────────────────────────

# Paquetes necesarios ----------------------------------------------------------

#install.packages("readxl")
#install.packages("dplyr")
#install.packages("purrr")
#install.packages("stringr")
#install.packages("tidyr")
#install.packages("readr")
#install.packages("here")
#install.packages("tidyverse")



library(readxl)
library(dplyr)
library(purrr)
library(stringr)
library(tidyr)
library(tidyverse)
library(readr)
library(here)
library(tidyverse)


# ─────────────────────────────────────────
# 1. Ruta del archivo original
# ─────────────────────────────────────────


datos <- read_excel(
  here("Datos", "Toneladas historicas.xlsx"),
  col_names = FALSE,
  sheet = "Toneladas",
  range = cell_rows(4:113)
)

# ─────────────────────────────────────────
# 2. Detectar columnas donde empieza cada acopio
# ─────────────────────────────────────────
# columnas donde hay nombres de acopio en la primera fila

start_cols <- which(!is.na(datos[1, ]))

start_cols

# Extraer nombres de acopios

acopios <- as.character(unlist(datos[1, start_cols]))

acopios
#GUARDIA ESCOLTA" "ITIN"            "LAS VARILLAS"    "PIQUETE CABADO"  "ALVEAR"          "BALCARCE"        "GRANADA"        
#"VILLEGAS"        "VICTORIA"        "TOTAL ACOPIOS"  

# ─────────────────────────────────────────
# 3. Preparar cuerpo de datos (sin fila de nombres)
# ─────────────────────────────────────────

datos_cuerpo <- datos[-1, ]   # todas menos la primera fila

# Cada bloque tiene:
# col +0 = mes
# col +1 = Orig Mes
# col +2 = Orig YTD
# col +3 = Equiv MES
# col +4 = Equiv YTD
# col +5 = año


# ─────────────────────────────────────────
# 4. Función para limpiar un bloque por acopio
# ─────────────────────────────────────────

limpiar_bloque <- function(start_col, nombre_acopio, df) {
  tibble(
    fecha      = df[[1]],
    mes        = df[[start_col]],
    orig_mes   = df[[start_col + 1]],
    orig_ytd   = df[[start_col + 2]],
    equiv_mes  = df[[start_col + 3]],
    equiv_ytd  = df[[start_col + 4]],
    anio       = df[[start_col + 5]],
    acopio     = nombre_acopio
  ) %>% 
    # sacar filas donde está todo NA
    filter(
      !(is.na(mes) & is.na(orig_mes) & is.na(orig_ytd) &
          is.na(equiv_mes) & is.na(equiv_ytd) & is.na(anio))
    ) %>% 
    mutate(
      mes        = as.integer(mes),
      anio       = as.integer(anio),
      orig_mes   = as.numeric(orig_mes),
      orig_ytd   = as.numeric(orig_ytd),
      equiv_mes  = as.numeric(equiv_mes),
      equiv_ytd  = as.numeric(equiv_ytd),
      fecha      = as.Date(fecha)   # por si quedó como texto
    )
}

# ─────────────────────────────────────────
# 5. Eliminar "TOTAL ACOPIOS" (no es un acopio)
# ─────────────────────────────────────────

# saco "TOTAL ACOPIOS" proque no es un acopio :
idx_total <- which(acopios == "TOTAL ACOPIOS")


if (length(idx_total) > 0) {
  acopios    <- acopios[-idx_total]
  start_cols <- start_cols[-idx_total]
}

#chequeamos que se haya borrado 
acopios
start_cols


# ─────────────────────────────────────────
# 6. Aplicar la limpieza a todos los bloques
# ─────────────────────────────────────────

lista_bloques <- map2(
  start_cols,
  acopios,
  ~ limpiar_bloque(start_col = .x, nombre_acopio = .y, df = datos_cuerpo)
)

# ─────────────────────────────────────────
# 7. Unir todo en formato largo
# ─────────────────────────────────────────

toneladas_long <- bind_rows(lista_bloques)

toneladas_long <- toneladas_long %>% 
  select(acopio, anio, mes, fecha,
         orig_mes, orig_ytd,
         equiv_mes, equiv_ytd)

toneladas_long <- toneladas_long %>% 
  filter(!is.na(anio))


toneladas_long <- toneladas_long %>% 
  filter(!(anio == 2025 & mes %in% 11:12))

# ─────────────────────────────────────────
# 8. Guardar datos procesados
# ─────────────────────────────────────────

dir.create(here("Datos_procesados"), showWarnings = FALSE)

write_csv(toneladas_long, here("Datos_procesados", "toneladas_limpias.csv"))

# ─────────────────────────────────────────
# 9. Verificación final
# ─────────────────────────────────────────

glimpse(toneladas_long)

