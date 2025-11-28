# ─────────────────────────────────────────
# 02_exploracion_toneladas.R
# Exploración de toneladas históricas por acopio
# ─────────────────────────────────────────

# Paquetes necesarios ----------------------------------------------------------

#install.packages("tidyverse")
#install.packages("here")


library(tidyverse)
library(here)
library(ggplot2)
library(scales) # para formatear números
library(plotly)
library(ggrepel)

# ─────────────────────────────────────────
# 1. Lectura de datos procesados
# ─────────────────────────────────────────

toneladas <- read_csv(
  here("Datos_procesados", "toneladas_limpias.csv")
)

glimpse(toneladas)


# ─────────────────────────────────────────
# 2. Enriquecimiento básico de la tabla
# ─────────────────────────────────────────

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
# 3. Resumen anual por acopio
# ─────────────────────────────────────────

resumen_acopio_anio <- toneladas %>% 
  group_by(acopio, anio) %>% 
  summarise(
    orig_tn_anio   = sum(orig_mes,   na.rm = TRUE),
    equiv_tn_anio  = sum(equiv_mes,  na.rm = TRUE),
    .groups = "drop"
  )

glimpse(resumen_acopio_anio)


# ─────────────────────────────────────────
# 4. Resumen histórico por acopio
# ─────────────────────────────────────────

resumen_acopio_total <- resumen_acopio_anio %>% 
  group_by(acopio) %>% 
  summarise(
    orig_tn_total  = sum(orig_tn_anio,  na.rm = TRUE),
    equiv_tn_total = sum(equiv_tn_anio, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  arrange(desc(orig_tn_total))

resumen_acopio_total


# ─────────────────────────────────────────
# 1. Gráfico: ranking histórico de acopios
# ─────────────────────────────────────────

ggplot(resumen_acopio_total, 
       aes(x = reorder(acopio, orig_tn_total),
           y = orig_tn_total)) +
  
  geom_col(fill = "#2E86C1") +
  
  geom_text(aes(label = comma(orig_tn_total)),
            hjust = -0.1,
            size = 3.8,
            color = "black",
            fontface = "bold") +
  
  coord_flip() +
  
  labs(
    title = "Ranking histórico de toneladas originadas por acopio",
    subtitle = "Suma total de toneladas originadas entre 2017–2025",
    x = "Acopio",
    y = "Toneladas originadas"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray30"),
    axis.title.y = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    axis.text = element_text(size = 11),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 20, 10, 10)
  ) +
  
  expand_limits(y = max(resumen_acopio_total$orig_tn_total) * 1.15)



# ─────────────────────────────────────────
# Gráfico 2: Tendencia histórica por acopio
# ─────────────────────────────────────────

ggplot(toneladas, aes(x = fecha, y = orig_mes)) +
  geom_line(color = "#2E86C1", linewidth = 0.8) +
  facet_wrap(~ acopio, scales = "free_y") +
  labs(
    title = "Evolución mensual de toneladas originadas por acopio",
    subtitle = "Período 2017–2025 (meses 1 a 10)",
    x = "Fecha",
    y = "Toneladas originadas"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray30"),
    strip.text = element_text(face = "bold", size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )


# ─────────────────────────────────────────
# Gráfico 3: Estacionalidad promedio por mes
# ─────────────────────────────────────────

estacionalidad <- toneladas %>% 
  group_by(mes, mes_nombre) %>% 
  summarise(
    promedio_origen = mean(orig_mes, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(estacionalidad, aes(x = mes_nombre, y = promedio_origen)) +
  geom_col(fill = "#2E86C1") +
  geom_text(aes(label = round(promedio_origen, 0)),
            vjust = -0.3, size = 3.5) +
  labs(
    title = "Estacionalidad promedio de toneladas originadas",
    subtitle = "Promedio de todos los acopios y años",
    x = "Mes",
    y = "Toneladas (promedio)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray30"),
    panel.grid.minor = element_blank()
  )

# ─────────────────────────────────────────
# Gráfico 4: Toneladas por año y acopio
# ─────────────────────────────────────────

resumen_acopio_anio <- toneladas %>% 
  group_by(acopio, anio) %>% 
  summarise(
    orig_tn_anio = sum(orig_mes, na.rm = TRUE),
    .groups = "drop"
  )

#mejor año por acopio 
resumen_acopio_anio <- resumen_acopio_anio %>% 
  group_by(acopio) %>% 
  mutate(
    max_anio = orig_tn_anio == max(orig_tn_anio)
  ) %>% 
  ungroup()




ggplot(resumen_acopio_anio, aes(x = factor(anio),
                                y = orig_tn_anio,
                                fill = factor(anio))) +
  geom_col() +
  facet_wrap(~ acopio, scales = "free_y") +
  scale_fill_brewer(palette = "Blues") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Comparación anual de toneladas originadas por acopio",
    x = "Año",
    y = "Toneladas originadas",
    fill = "Año"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# ─────────────────────────────────────────
# Gráfico 5: Mejor año por acopio
# ─────────────────────────────────────────


library(scales)

orden_acopios <- resumen_acopio_anio %>% 
  group_by(acopio) %>% 
  summarise(max_valor = max(orig_tn_anio)) %>% 
  arrange(max_valor) %>% 
  pull(acopio)


# marcar mejor año
plot_base <- ggplot(
  resumen_acopio_anio,
  aes(
    x = orig_tn_anio,
    y = factor(acopio, levels = orden_acopios),
    color = max_anio,
    text = paste0(
      "Acopio: ", acopio,
      "<br>Año: ", anio,
      "<br>Toneladas: ", comma(orig_tn_anio)
    )
  )
) +
  geom_point(size = 4) +
  
  geom_text_repel(
    data = resumen_acopio_anio %>% filter(max_anio),
    aes(label = paste0(anio, ": ", comma(orig_tn_anio))),
    color = "#2E86C1",
    nudge_x = 20000,
    size = 4,
    fontface = "bold",
    segment.color = NA
  ) +
  
  scale_color_manual(
    values = c("FALSE" = "gray70", "TRUE" = "#2E86C1"),
    guide = "none"
  ) +
  
  scale_x_continuous(labels = comma) +
  
  labs(
    title = "Toneladas originadas por acopio según año",
    subtitle = "El punto azul indica el mejor año. Etiquetas fijas.",
    x = "Toneladas originadas",
    y = "Acopio"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray30"),
    axis.text = element_text(size = 11),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  ) +
  
  coord_cartesian(clip = "off",
                  xlim = c(0, max(resumen_acopio_anio$orig_tn_anio) * 1.15))

# Convertir a plotly sin legendas
plot_interactivo <- ggplotly(plot_base, tooltip = "text") %>% 
  style(showlegend = FALSE)

plot_interactivo