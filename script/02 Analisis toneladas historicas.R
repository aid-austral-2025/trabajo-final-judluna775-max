
# ─────────────────────────────────────────
# 02_exploracion_toneladas.R
# Exploración de toneladas históricas por acopio
# ─────────────────────────────────────────

# Paquetes necesarios ----------------------------------------------------------


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

# Orden base de acopios (de menor a mayor total originado)
orden_acopios <- resumen_acopio_anio %>% 
  group_by(acopio) %>% 
  summarise(total = sum(orig_tn_anio), .groups = "drop") %>% 
  arrange(total) %>% 
  pull(acopio)

# ─────────────────────────────────────────
# 4. Ranking histórico de acopios (barras horizontales)
# ─────────────────────────────────────────

ggplot(resumen_acopio_total, 
       aes(x = reorder(acopio, orig_tn_total),
           y = orig_tn_total)) +
  geom_col(fill = "#2E86C1") +
  geom_text(
    aes(label = comma(orig_tn_total, big.mark = ".", decimal.mark = ",")),
    hjust = 1.1,
    size = 4,
    fontface = "bold"
  ) +
  coord_flip() +
  
  scale_y_continuous(
    labels = function(x) scales::comma(x, big.mark = ".", decimal.mark = ",")
  ) +
  
  labs(
    title = "Ranking histórico de toneladas originadas por acopio",
    subtitle = "Totales acumulados 2020–2025",
    x = "Acopio",
    y = "Toneladas originadas"
  ) +
  theme_minimal(base_size = 13)

# ─────────────────────────────────────────
# 5. Equivalentes vs originadas por acopio
# ─────────────────────────────────────────


resumen_equiv <- toneladas %>%
  group_by(acopio) %>%
  summarise(
    orig_total  = sum(orig_mes,  na.rm = TRUE),
    equiv_total = sum(equiv_mes, na.rm = TRUE),
    pct_equiv   = equiv_total / orig_total,
    .groups = "drop"
  )

ggplot(resumen_equiv, aes(y = reorder(acopio, orig_total))) +
  
  # Barra total originada
  geom_col(aes(x = orig_total),
           fill = "#2E86C1",
           alpha = 0.5) +
  
  # Barra equivalente dentro de la barra total
  geom_col(aes(x = equiv_total),
           fill = "#117A65") +
  
  # Etiqueta del % equivalente
  geom_label(
    aes(
      x = equiv_total * 0.90,     
      label = scales::percent(pct_equiv, accuracy = 1,
                              decimal.mark = ",")
    ),
    fill = "#FDFEFE",             # fondo blanco suave
    color = "black",
    fontface = "bold",
    label.r = unit(0.15, "lines"), # bordes más redondeados
    size = 3.5,
    label.size = 0                # sin borde
  ) +
  
  
    # ⭐ NUEVO: valor absoluto equivalente
    geom_text(
      aes(x = equiv_total * 0.5,
          label = scales::comma(equiv_total,
                                big.mark=".", decimal.mark=",")
      ),
      color = "white",
      fontface = "bold",
      size = 3.8
    ) +
    
      # ⭐ NUEVO: valor absoluto originado
      geom_text(
        aes(x = orig_total,
            label = scales::comma(orig_total,
                                  big.mark=".", decimal.mark=",")),
        hjust = 1.1,
        color = "black",
        fontface = "bold",
        size = 3.5
      ) +
  
  # Formato del eje
  scale_x_continuous(
    labels = ~ scales::comma(.x, big.mark = ".", decimal.mark = ",")
  ) +
  
  labs(
    title = "Porción equivalente dentro de la originación total",
    subtitle = "Equivalentes (verde) como parte de originadas (azul)",
    x = "Toneladas",
    y = "Acopio"
  ) +
  theme_minimal(base_size = 13)

# ─────────────────────────────────────────
# 6. Tendencia mensual por acopio (orig vs equiv)
# ─────────────────────────────────────────

ggplot(toneladas, aes(x = fecha)) +
  
  # Línea de originadas
  geom_line(aes(y = orig_mes, color = "Originadas"), linewidth = 0.8) +
  
  # Línea de equivalentes
  geom_line(aes(y = equiv_mes, color = "Equivalentes"), linewidth = 0.8) +
  
  facet_wrap(~ acopio, scales = "free_y") +
  
  scale_color_manual(
    values = c(
      "Originadas" = "#2E86C1",     # azul
      "Equivalentes" = "#117A65"    # verde
    )
  ) +
  
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y",
    limits = as.Date(c("2020-01-01", "2025-12-31")),
    expand = c(0,0)
  ) +
  
  labs(
    title = "Evolución mensual de toneladas originadas y equivalentes por acopio",
    subtitle = "Período 2020–2025",
    x = "Año",
    y = "Toneladas",
    color = "Serie"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# ─────────────────────────────────────────
# 7. Comparación anual por acopio (barras por año)
# ──

#tabla para una mejor visualizacion de los datos 

resumen_anual_simple <- toneladas |>
  dplyr::group_by(acopio, anio) |>
  dplyr::summarise(
    orig_tn_anio = sum(orig_mes, na.rm = TRUE),
    .groups = "drop"
  )
tabla_anual <- resumen_anual_simple |>
  tidyr::pivot_wider(
    names_from = anio,
    values_from = orig_tn_anio
  ) |>
  dplyr::arrange(desc(2025)) # ordeno por el año más reciente

tabla_anual


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
# 8. Rankings por año (tabla base de rankings)
# ─────────────────────────────────────────

rank_orig <- resumen_acopio_anio %>%
  group_by(anio) %>%
  arrange(desc(orig_tn_anio)) %>%
  mutate(
    rank = row_number(),               # ranking 1..n
    perc = orig_tn_anio / sum(orig_tn_anio)  # participación %
  ) %>%
  ungroup()

rank_orig


# ─────────────────────────────────────────
# 9. Podio de originación por año
# ─────────────────────────────────────────


rank_podio <- rank_orig %>%
  filter(rank <= 3) %>%
  mutate(rank_label = factor(rank, levels = c(1,2,3),
                             labels = c("🥇 1°", "🥈 2°", "🥉 3°")))

ggplot(rank_podio,
       aes(x = factor(anio), 
           y = orig_tn_anio,
           fill = rank_label)) +
  
  geom_col(position = "dodge") +
  
  geom_text(aes(label = acopio),
            position = position_dodge(width = 0.9),
            angle = 90,
            hjust = 1.5,
            vjust = 0.5,
            color = "white",
            fontface = "bold",
            size = 3.5
  ) +
  
  geom_text(
    aes(label = paste0(round(orig_tn_anio/1000, 0), "k")),
    position = position_dodge(width = 0.9),
    vjust = -0.4,
    color = "black",
    size = 3.2,
    fontface = "bold"
  ) +
  
    scale_fill_manual(values = c(
    "🥇 1°" = "gold",
    "🥈 2°" = "grey70",
    "🥉 3°" = "#CD7F32"
  )) +
  
  labs(
    title = "Podio de originación por año",
    x = "Año",
    y = "Toneladas originadas",
    fill = "Ranking"
  ) +
  theme_minimal(base_size = 13)

# ─────────────────────────────────────────
# 10. Bump chart: evolución del ranking
# ─────────────────────────────────────────

ggplot(rank_orig,
       aes(x = anio,
           y = rank,
           color = acopio,
           group = acopio)) +
  
  geom_line(linewidth = 1.4) +
  geom_point(size = 4) +
  
  scale_y_reverse(breaks = 1:max(rank_orig$rank)) +  # 1 arriba, n abajo
  
  labs(
    title = "Evolución del ranking anual de originación por acopio",
    x = "Año",
    y = "Ranking (1 = mejor)"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )

# ─────────────────────────────────────────
# 11. Barras apiladas en miles por año
# ─────────────────────────────────────────

resumen_acopio_anio <- resumen_acopio_anio %>%
  mutate(
    anio = factor(anio),
    orig_tn_miles = orig_tn_anio / 1000,
    orig_tn_miles_round = round(orig_tn_miles, 0)
  )

ggplot(resumen_acopio_anio,
       aes(x = orig_tn_miles,
           y = factor(acopio, levels = orden_acopios),
           fill = anio)) +
  
  geom_col(position = position_stack(reverse = TRUE)) +
  
  geom_text(
    aes(label = paste0(
      comma(orig_tn_miles_round,
            big.mark = ".",
            decimal.mark = ","),
      "k"
    )),
    position = position_stack(vjust = 0.5, reverse = TRUE),
    size = 2.8,
    fontface = "bold",
    color = "black"
  ) +
  
  scale_fill_brewer(palette = "Set2") +
  
  scale_x_continuous(
    labels = ~ paste0(
      comma(round(.x, 0),
            big.mark = ".",
            decimal.mark = ","),
      "k"
    )
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
# 12. Barras 100% apiladas (participación % por año)
# ─────────────────────────────────────────

resumen_acopio_pct <- resumen_acopio_anio %>%
  group_by(acopio) %>%
  mutate(
    pct  = orig_tn_anio / sum(orig_tn_anio),
    anio = factor(anio)       # <<--- ACÁ SE CORRIGE EL ERROR
  ) %>%
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
    title = "Participación porcentual de cada año en la originación",
    subtitle = "Barras 100% apiladas (2020–2025)",
    x = "Participación (%)",
    y = "Acopio",
    fill = "Año"
  ) +
  theme_minimal(base_size = 13)


# ─────────────────────────────────────────
# 13.Participación porcentual de cada acopio en el total anual
# ─────────────────────────────────────────

participacion_anual <- resumen_acopio_anio %>%
  group_by(anio) %>%
  mutate(
    pct = orig_tn_anio / sum(orig_tn_anio)
  ) %>%
  ungroup() %>%
  mutate(
    pct_label = scales::percent(pct, accuracy = 1, decimal.mark = ",")
  )

# Ordenar acopios por participación total para una paleta consistente
orden_acopios_global <- participacion_anual %>%
  group_by(acopio) %>%
  summarise(total = sum(orig_tn_anio), .groups = "drop") %>%
  arrange(desc(total)) %>%
  pull(acopio)

ggplot(participacion_anual,
       aes(x = factor(anio),
           y = pct,
           fill = factor(acopio, levels = orden_acopios_global))) +
  
  geom_col(width = 0.85, color = "white") +
  
  # Etiquetas centradas dentro de la barra
  geom_text(
    aes(label = pct_label),
    position = position_stack(vjust = 0.5),
    size = 3,
    fontface = "bold",
    color = "white"
  ) +
  
  scale_y_continuous(labels = percent_format(decimal.mark = ",")) +
  scale_fill_manual(values = c(
    "PIQUETE CABADO"   = "#66C2A5",  # verde soft
    "GUARDIA ESCOLTA"  = "#FC8D62",  # coral
    "ITIN"             = "#8DA0CB",  # lavanda
    "LAS VARILLAS"     = "#E78AC3",  # rosa pastel
    "BALCARCE"         = "#A6D854",  # verde lima suave
    "GRANADA"          = "#FFD92F",  # amarillo pastel
    "VILLEGAS"         = "#E5C494",  # beige cálido
    "ALVEAR"           = "#B3B3B3",  # gris suave
    "VICTORIA"         = "#A1C3D1"   # celeste pastel added (similar Set2)
  )) +
  
  labs(
    title = "Participación de cada acopio en la originación total anual",
    subtitle = "Barras 100% apiladas – origen anual (2020–2025)",
    x = "Año",
    y = "Participación (%)",
    fill = "Acopio"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face="bold", size=16),
    plot.subtitle = element_text(size=13),
    axis.text.x = element_text(size=12),
    axis.text.y = element_text(size=12),
    legend.position = "right"
  )

# ─────────────────────────────────────────
# 14. Animación YTD 2025 
# ─────────────────────────────────────────

ton_2025_ytd <- toneladas %>%
  filter(anio == 2025) %>%
  arrange(acopio, mes) %>%
  group_by(acopio) %>%
  mutate(
    orig_ytd_anim  = cumsum(orig_mes),      # acumulado origen
    equiv_ytd_anim = cumsum(equiv_mes),     # acumulado equivalentes
    anio_mes = factor(anio_mes, 
                      levels = unique(anio_mes))  # orden correcto para animación
  ) %>%
  ungroup()

escala_tamano <- 3000   # ajustable (más grande → burbujas más chicas)


plot_ytd_2025 <- ton_2025_ytd %>%
  plot_ly(
    x = ~orig_ytd_anim,
    y = ~acopio,
    frame = ~anio_mes,
    
    # Línea + puntos
    type = "scatter",
    mode = "lines+markers",
    split = ~acopio,   # una línea por acopio
    
    # Marker (burbujas)
    marker = list(
      sizemode = "area",
      size = ~orig_ytd_anim / escala_tamano,  # crecimiento progresivo
      color = "#2E86C1",
      opacity = 0.85,
      line = list(width = 1, color = "#1B4F72")
    ),
    
    # Línea visible
    line = list(
      width = 3,
      color = "rgba(46,134,193,0.6)"
    ),
    
    hovertemplate = paste(
      "<b>%{y}</b><br>",
      "Mes: %{frame}<br>",
      "Ton YTD: %{x:,.0f}<br>",
      "Equiv YTD: %{customdata:,.0f}<extra></extra>"
    ),
    customdata = ~equiv_ytd_anim
  ) %>%
  layout(
    title = "Toneladas originadas YTD 2025 (acumulado por mes)",
    xaxis = list(
      title = "Toneladas acumuladas (YTD)",
      separatethousands = TRUE,
      tickformat = ",."
    ),
    yaxis = list(title = "Acopio"),
    separators = ".,"
  ) %>%
  animation_opts(
    frame = 800,
    easing = "linear",
    redraw = FALSE
  ) %>%
  animation_slider(currentvalue = list(prefix = "Mes: ")) %>%
  layout(showlegend = FALSE)  # ocultamos la leyenda porque no suma valor

# Ejecutar
plot_ytd_2025

# ─────────────────────────────────────────
# 15. Animación global YTD 2020–2025 (carrera histórica)
# ─────────────────────────────────────────

ton_ytd_global <- toneladas %>%
  arrange(acopio, fecha) %>%              # Orden temporal real
  group_by(acopio) %>%                   
  mutate(
    orig_ytd_global  = cumsum(orig_mes),   # ACUMULADO GLOBAL desde 2020
    equiv_ytd_global = cumsum(equiv_mes),
    frame_mes = factor(fecha, levels = unique(fecha))  # frames reales: 2020-01-01 ... 2025-10-01
  ) %>%
  ungroup()

escala_tamano <- 6000   


plot_ytd_global <- ton_ytd_global %>%
  plot_ly(
    x = ~orig_ytd_global,
    y = ~acopio,
    frame = ~frame_mes,
    
    type = "scatter",
    mode = "markers",        # SOLO burbujas, sin líneas
    
    # BURBUJAS
    marker = list(
      sizemode = "area",
      size = ~orig_ytd_global / escala_tamano,
      color = ~acopio,        # color por acopio
      opacity = 0.85,
      line = list(width = 1, color = "#000")
    ),
    
    hovertemplate = paste(
      "<b>%{y}</b><br>",
      "Fecha: %{frame}<br>",
      "Ton acumuladas: %{x:,.0f}<extra></extra>"
    )
  ) %>%
  layout(
    title = "Evolución histórica acumulada (2020–2025)",
    xaxis = list(
      title = "Toneladas acumuladas desde 2020",
      separatethousands = TRUE,
      tickformat = ",."
    ),
    yaxis = list(title = "Acopio"),
    separators = ".,"
  ) %>%
  animation_opts(
    frame = 600,
    easing = "linear",
    redraw = FALSE
  ) %>%
  animation_slider(
    currentvalue = list(prefix = "Mes: ")
  )

plot_ytd_global

