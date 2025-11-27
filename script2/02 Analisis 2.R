
# ─────────────────────────────────────────
# 02_exploracion_toneladas.R
# Exploración de toneladas históricas por acopio
# ─────────────────────────────────────────

# Paquetes necesarios ----------------------------------------------------------

##ARREGLAR QUE ESTOY SACANDO 11 Y 12 DE TODOS LOS AÑOS Y SOLO HAY QUE SACARLO DEL 2025 


#install.packages("tidyverse")
#install.packages("here")
#install.packages("readr")
#install.packages("dplyr")
#install.packages("plotly")
#install.packages("colorspace")


# Paquetes ---------------------------------------------------------------------

library(tidyverse)
library(here)
library(scales)
library(RColorBrewer)
library(plotly)      # lo usaremos luego
library(ggrepel)     # para etiquetas más prolijas si las usamos

# ─────────────────────────────────────────
# 1. Lectura y preprocesamiento
# ─────────────────────────────────────────

toneladas <- read_csv(
  here("Datos_procesados", "toneladas_limpias.csv")
)

# Para asegurar comparabilidad: análisis desde 2020
toneladas <- toneladas %>% 
  filter(anio >= 2020)

# Enriquecer la tabla
toneladas <- toneladas %>% 
  mutate(
    mes_nombre = factor(
      mes,
      levels = 1:12,
      labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
                 "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
    ),
    anio_mes = paste(anio, sprintf("%02d", mes), sep = "-")
  )


# ─────────────────────────────────────────
# 2. Resumen anual por acopio
# ─────────────────────────────────────────

resumen_acopio_anio <- toneladas %>% 
  group_by(acopio, anio) %>% 
  summarise(
    orig_tn_anio   = sum(orig_mes,   na.rm = TRUE),
    equiv_tn_anio  = sum(equiv_mes,  na.rm = TRUE),
    .groups = "drop"
  )


# ─────────────────────────────────────────
# 3. Resumen histórico total
# ─────────────────────────────────────────

resumen_acopio_total <- resumen_acopio_anio %>% 
  group_by(acopio) %>% 
  summarise(
    orig_tn_total  = sum(orig_tn_anio),
    equiv_tn_total = sum(equiv_tn_anio),
    .groups = "drop"
  ) %>% 
  arrange(desc(orig_tn_total))


# ─────────────────────────────────────────
# 4. Ranking histórico de acopios (barras horizontales)
# ─────────────────────────────────────────

ggplot(resumen_acopio_total, 
       aes(x = reorder(acopio, orig_tn_total),
           y = orig_tn_total)) +
  geom_col(fill = "#2E86C1") +
  geom_text(
    aes(label = comma(orig_tn_total, big.mark = ".", decimal.mark = ",")),
    hjust = -0.1,
    size = 3.8,
    fontface = "bold"
  ) +
  coord_flip() +
  labs(
    title = "Ranking histórico de toneladas originadas por acopio",
    subtitle = "Totales acumulados 2020–2025",
    x = "Acopio",
    y = "Toneladas originadas"
  ) +
  theme_minimal(base_size = 13)


# ─────────────────────────────────────────
# 5. Tendencia mensual por acopio
# ─────────────────────────────────────────

ggplot(toneladas, aes(x = fecha, y = orig_mes)) +
  geom_line(color = "#2E86C1", linewidth = 0.8) +
  facet_wrap(~ acopio, scales = "free_y") +
  labs(
    title = "Evolución mensual de toneladas originadas por acopio",
    subtitle = "Período 2020–2025 (meses 1 a 10)",
    x = "Fecha",
    y = "Toneladas originadas"
  ) +
  theme_minimal(base_size = 13)


# ─────────────────────────────────────────
# 6. Comparación anual por acopio
# ─────────────────────────────────────────

ggplot(resumen_acopio_anio, 
       aes(x = factor(anio),
           y = orig_tn_anio,
           fill = factor(anio))) +
  geom_col() +
  facet_wrap(~ acopio, scales = "free_y") +
  scale_fill_brewer(palette = "Blues") +
  scale_y_continuous(labels = ~ comma(.x, big.mark = ".", decimal.mark = ",")) +
  labs(
    title = "Comparación anual de toneladas originadas por acopio",
    x = "Año",
    y = "Toneladas originadas",
    fill = "Año"
  ) +
  theme_minimal(base_size = 13)


# ─────────────────────────────────────────
# 7. Mejor año por acopio
# ─────────────────────────────────────────

ggplot(mejor_anio,
       aes(x = orig_tn_anio,
           y = reorder(acopio, orig_tn_anio))) +
  
  geom_col(fill = "#2E86C1") +
  
  geom_label(
    aes(
      label = paste0(
        anio, ": ",
        scales::comma(orig_tn_anio, big.mark = ".", decimal.mark = ",")
      )
    ),
    
    hjust = 1.5,          # mueve el texto hacia adentro de la barra
    fill = "white",     
    color = "black",,
    fontface = "bold",
    size = 4
  ) +
  
  labs(
    title = "Mejor año de originación por acopio",
    x = "Toneladas originadas",
    y = "Acopio"
  ) +
  
  scale_x_continuous(
    labels = function(x) scales::comma(x, big.mark = ".", decimal.mark = ",")
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.text = element_text(size = 11)
  )


# ─────────────────────────────────────────
# 8. Barras apiladas en miles por año
# ─────────────────────────────────────────

resumen_acopio_anio <- resumen_acopio_anio %>%
  mutate(
    orig_tn_miles = orig_tn_anio / 1000,
    anio = factor(anio, levels = 2020:2025)
  )

orden_acopios <- resumen_acopio_anio %>% 
  group_by(acopio) %>% 
  summarise(total = sum(orig_tn_miles), .groups = "drop") %>% 
  arrange(total) %>% 
  pull(acopio)

ggplot(resumen_acopio_anio,
       aes(x = orig_tn_miles,
           y = factor(acopio, levels = orden_acopios),
           fill = anio)) +
  geom_col(position = position_stack(reverse = TRUE)) +
  geom_text(
    aes(label = paste0(comma(orig_tn_miles, big.mark = ".", decimal.mark = ","), "k")),
    position = position_stack(vjust = 0.5, reverse = TRUE),
    size = 2.8,
    fontface = "bold"
  ) +
  scale_fill_brewer(palette = "Set2") +
  scale_x_continuous(
    labels = ~ paste0(comma(.x, big.mark = ".", decimal.mark = ","), "k")
  ) +
  labs(
    title = "Toneladas originadas por acopio (2020–2025)",
    subtitle = "Valores expresados en miles de toneladas",
    x = "Ton (miles)",
    y = "Acopio",
    fill = "Año"
  ) +
  theme_minimal(base_size = 13)


# ─────────────────────────────────────────
# 9. Barras 100% apiladas (participación % por año)
# ─────────────────────────────────────────

resumen_acopio_pct <- resumen_acopio_anio %>%
  group_by(acopio) %>%
  mutate(pct = orig_tn_anio / sum(orig_tn_anio)) %>%
  ungroup()

ggplot(resumen_acopio_pct,
       aes(x = pct,
           y = factor(acopio, levels = orden_acopios),
           fill = anio)) +
  geom_col(position = position_stack(reverse = TRUE)) +
  geom_text(
    aes(label = percent(pct, accuracy = 1, decimal.mark = ",")),
    position = position_stack(vjust = 0.5, reverse = TRUE),
    size = 3
  ) +
  scale_fill_brewer(palette = "Set2") +
  scale_x_continuous(labels = percent_format(accuracy = 1, decimal.mark = ",")) +
  labs(
    title = "Participación porcentual de cada año",
    subtitle = "Barras 100% apiladas (2020–2025)",
    x = "Participación (%)",
    y = "Acopio",
    fill = "Año"
  ) +
  theme_minimal(base_size = 13)


#__________________________________________ 

ton_2025_ytd <- toneladas %>%
  filter(anio == 2025) %>%
  arrange(acopio, mes) %>%
  group_by(acopio) %>%
  mutate(
    orig_ytd_anim  = cumsum(orig_mes),
    equiv_ytd_anim = cumsum(equiv_mes),
    anio_mes = factor(anio_mes, levels = unique(anio_mes))
  ) %>%
  ungroup()



library(plotly)
library(colorspace)

# Paleta por acopio (más linda y consistente)
paleta_acopios <- qualitative_hcl(
  n = length(unique(ton_2025_ytd$acopio)),
  palette = "Set2"
)

plot_ytd_2025 <- ton_2025_ytd %>%
  plot_ly(
    x = ~orig_ytd_anim,
    y = ~acopio,
    size = ~orig_ytd_anim,
    color = ~acopio,
    frame = ~anio_mes,
    type = "scatter",
    mode = "markers",
    customdata = ~equiv_ytd_anim,
    hovertemplate = paste(
      "<b>%{y}</b><br>",
      "Mes: %{frame}<br>",
      "Ton YTD: %{x:.0f}<br>",
      "Equivalentes YTD: %{customdata:.0f}<extra></extra>"
    ),
    marker = list(
      sizemode = "area",
      opacity = 0.8,
      line = list(width = 1, color = "#333")
    ),
    colors = paleta_acopios
  ) %>%
  layout(
    title = list(
      text = "Evolución acumulada YTD de toneladas originadas – Año 2025",
      y = 0.95,
      x = 0.5,
      xanchor = "center"
    ),
    xaxis = list(
      title = "Toneladas acumuladas (YTD)",
      separatethousands = TRUE,
      tickformat = ",."
    ),
    yaxis = list(title = "Acopio"),
    legend = list(title = list(text = "<b>Acopio</b>"))
  ) %>%
  animation_opts(
    frame = 900,
    easing = "linear",
    redraw = FALSE
  ) %>%
  animation_slider(
    currentvalue = list(prefix = "Mes: ")
  )

plot_ytd_2025

#________ otra opcion 


library(dplyr)

# Datos base 2025 con YTD
ton_2025_ytd <- toneladas %>%
  filter(anio == 2025) %>%
  arrange(acopio, mes) %>%
  group_by(acopio) %>%
  mutate(
    orig_ytd_anim  = cumsum(orig_mes),
    equiv_ytd_anim = cumsum(equiv_mes),
    anio_mes = factor(anio_mes, levels = unique(anio_mes)),
    frame_num = as.numeric(mes)   # para secuencia
  ) %>%
  ungroup()

# ─────────────────────────────
# Construir la tabla con “trail”
# ─────────────────────────────

ton_2025_ytd <- toneladas %>%
  filter(anio == 2025) %>%
  arrange(acopio, mes) %>%
  group_by(acopio) %>%
  mutate(
    orig_ytd_anim  = cumsum(orig_mes),
    equiv_ytd_anim = cumsum(equiv_mes),
    anio_mes = factor(anio_mes, levels = unique(anio_mes))
  ) %>%
  ungroup()



library(plotly)

# Animación con líneas que unen puntos por acopio
library(plotly)

# Escala más razonable para el tamaño de burbuja
escala_tamano <- 3000   # <-- ajustar si querés más grande/chica

plot_ytd_2025 <- ton_2025_ytd %>%
  plot_ly(
    x = ~orig_ytd_anim,
    y = ~acopio,
    frame = ~anio_mes,
    
    # LÍNEAS + PUNTOS (acá garantizamos visibilidad)
    type = "scatter",
    mode = "lines+markers",
    split = ~acopio,
    
    # BURBUJA controlada
    marker = list(
      sizemode = "area",
      size = ~orig_ytd_anim / escala_tamano,
      color = "#2E86C1",
      opacity = 0.85,
      line = list(width = 1, color = "#1B4F72")
    ),
    
    # LÍNEA claramente visible
    line = list(
      width = 3,
      color = "rgba(46,134,193,0.6)"
    ),
    
    hovertemplate = paste(
      "<b>%{y}</b><br>",
      "Ton YTD: %{x:,.0f}<extra></extra>"
    )
  ) %>%
  layout(
    title = "Toneladas originadas YTD 2025 (con línea de tendencia visible)",
    xaxis = list(
      title = "Toneladas acumuladas (YTD)",
      tickformat = ".",
      separatethousands = TRUE
    ),
    yaxis = list(title = "Acopio"),
    separators = ".,"
    ,
  )%>%
  animation_opts(
    frame = 800,
    easing = "linear",
    redraw = FALSE
  ) %>%
  animation_slider(currentvalue = list(prefix = "Mes: "))

plot_ytd_2025


plot_ytd_2025 %>% 
  layout(showlegend = FALSE)


