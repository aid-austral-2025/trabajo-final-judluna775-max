# Trabajo Final de la materia: Analisis Inteligente de Datos

# Alumna: Judith Luna

## Etapas del Análisis Inteligente de Datos

### 1. Selección de datos

El conjunto de datos utilizado corresponde a información histórica de **toneladas originadas y toneladas equivalentes por acopio** del sistema comercial de la empresa.\
Los datos abarcan el período **2017–2025**, aunque para este trabajo se seleccionó el rango **2020–2025**, ya que:

-   antes de 2020 no existía información completa y homogénea;\
-   la estructura de acopios proviene de un proceso de fusiones y no todos contaban con registros previos;\
-   a partir de 2020 la operatoria y disponibilidad de datos se vuelve estable y comparable.

La base incluye las siguientes variables principales:

-   **acopio**\
-   **año y mes**\
-   **toneladas originadas (orig_mes)**\
-   **toneladas equivalentes (equiv_mes)**\
-   **fecha** en formato YYYY-MM-DD para análisis temporal\
-   nombres de meses y etiquetas de período para visualización

La problemática analizada es:\
**comprender la evolución histórica de los volúmenes de originación y operación física de cada acopio, detectar patrones, variaciones por año y cambios operativos relevantes**.

------------------------------------------------------------------------

### 2. Limpieza y preparación de los datos

El archivo original con la información de toneladas provenía de un **Excel estructurado de forma no tabular**, con múltiples bloques de columnas por acopio, encabezados repetidos, columnas combinadas y métricas agrupadas horizontalmente.\
Este formato no permitía analizar los datos directamente, por lo que fue necesario un proceso de limpieza y reestructuración relativamente extenso.

A continuación se detallan los pasos realizados.

------------------------------------------------------------------------

#### **2.1. Lectura del archivo original**

El archivo `"Toneladas historicas.xlsx"` se leyó utilizando `read_excel()`, especificando:

-   ausencia de nombres de columnas (`col_names = FALSE`),\
-   rango específico de filas útiles,\
-   y la hoja correspondiente (`sheet = "Toneladas"`).

Esto se debió a que el archivo contenía encabezados en varias filas superiores no útiles para el análisis.

------------------------------------------------------------------------

#### **2.2. Identificación automática de los bloques de columnas por acopio**

Cada acopio tenía un bloque de **6 columnas consecutivas**, con la siguiente estructura:

-   Mes\
-   Orig Mes\
-   Orig YTD\
-   Equiv Mes\
-   Equiv YTD\
-   Año

Como estos bloques estaban dispuestos horizontalmente, se detectó **en qué columnas comenzaba cada acopio** usando:

\`\`\`r start_cols \<- which(!is.na(datos[1, ]))

Esto permitió detectar automáticamente los acopios

Se eliminó el bloque correspondiente a TOTAL ACOPIOS, ya que no representa un acopio real.

#### **2.3. Construcción de una función de limpieza por bloque**

Para estandarizar la limpieza se implementó limpiar_bloque(), que:

-extrae las columnas correspondientes a un acopio,

-asigna nombres correctos a las variables,

-convierte los tipos de datos,

-elimina filas vacías,

-añade el nombre del acopio al bloque.

Esto transformó cada bloque horizontal en una tabla rectangular adecuada para su posterior análisis.

#### **2.4. Unión de bloques y construcción del dataset final**

Usando purrr::map2(), se procesaron todos los acopios y se unieron con:

toneladas_long \<- bind_rows(lista_bloques)

Luego se:

-seleccionaron solo las columnas relevantes,

-filtraron filas sin año,

-excluyeron los registros incompletos de noviembre y diciembre 2025,

-ordenaron las variables para su uso en las visualizaciones.

El resultado es un dataset largo, limpio y completamente tabular, guardado en:

**Datos_procesados/toneladas_limpias.csv**

### 3. Exploración de los datos

La fase exploratoria incluyó cálculos descriptivos y una amplia variedad de visualizaciones:

-   Ranking histórico de toneladas originadas (2020–2025).\
-   Comparación entre **originadas vs equivalentes** por acopio.\
-   Series temporales mensuales para detectar estacionalidades y efectos operativos.\
-   Comparación anual por acopio (barras por año).\
-   Evolución del **ranking anual** (gráfico tipo *bump chart*).\
-   **Podio anual** de los 3 acopios líderes por año.\
-   Barras apiladas en miles para ver tamaño relativo de cada acopio.\
-   Barras apiladas 100% para ver cómo se distribuye cada acopio en el tiempo.\
-   Participación de cada acopio dentro del total anual del sistema.

La exploración permitió detectar:

-   estabilidad de ciertos acopios líderes,\
-   caídas operativas explicadas por obras,\
-   crecimiento marcado en 2025,\
-   variaciones estructurales como la salida de Victoria del sistema,\
-   comportamiento especial de Villegas pese a no tener planta física.

------------------------------------------------------------------------

### 4. Comunicación y reproducibilidad

El informe se generó utilizando **Quarto**, lo que garantiza:

-   automatización del procesamiento y visualización,\
-   **reproducibilidad total** del análisis,\
-   estructuración clara del reporte,\
-   integración entre código, texto y gráficos,\
-   salida en formato **HTML** con recursos embebidos o en carpeta `_files`.

Además:

-   se utilizó `here::here()` para asegurar rutas reproducibles;\
-   el proyecto completo se alojó en **GitHub** siguiendo lo solicitado por la materia;\
-   el HTML final permite visualizar el análisis sin necesidad de instalar R.

Esta herramienta cumple el rol de comunicación del análisis de forma clara, estética y completamente replicable.
