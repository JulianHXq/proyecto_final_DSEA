# Proyecto de Minería de Texto --- **Stock News Recommendation**

Este documento describe la estructura, propósito y flujo de ejecución
del proyecto de minería de texto aplicado al análisis de noticias
financieras (GDELT + RSS), incluyendo la construcción de matrices
TF--IDF, wordclouds, modelos LDA, CAR y GRF.

⚠ **Importante:**\
Si ya cuentas con la base de datos descargada (`gdelt_apple.csv`),
**solo necesitas:**

1.  Cargar las librerías\
2.  Ir directamente a la línea **235+** del script para leer la base y
    continuar el pipeline

Esto evita las **6--8 horas** de scraping completo.

------------------------------------------------------------------------

# 🧰 0. Carga de Paquetes

``` r
require(pacman)
packages <- c(
  "tidyverse","lubridate","data.table","httr","jsonlite","stringi",
  "xml2","tidyquant","purrr","tidyRSS","text2vec","topicmodels",
  "tokenizers","stopwords","tidytext","SnowballC","grf","fixest",
  "tibble","cld3","rvest","wordcloud","Matrix","coop","tidyr",
  "stringr","patchwork","tm","textstem","maptpx","ranger"
)
lapply(packages, require, character.only = TRUE)
```

👉 Si tienes el CSV, **salta a la línea 235**.

------------------------------------------------------------------------

# 📰 1. Descarga de Noticias RSS

Scraping de:

-   MarketWatch\
-   Nasdaq\
-   Yahoo Finance

Se extraen títulos, fechas, URL y cuerpos preliminares.

------------------------------------------------------------------------

# 🌐 2. Descarga desde GDELT DOC API

Incluye:

-   Manejo de rangos de fechas\
-   Fix a JSON corruptos\
-   Scraping del cuerpo real\
-   Duración: **6--8 horas**

Guardar base:

``` r
write.csv(gdelt_apple, ".../gdelt_apple.csv", row.names = FALSE)
```

------------------------------------------------------------------------

# 📄 3. Cargar Base Guardada

``` r
gdelt_apple <- read_csv(".../gdelt_apple.csv")
```

------------------------------------------------------------------------

# 🧹 4. Limpieza Avanzada del Texto

-   Eliminación de copyright\
-   Detección de idioma\
-   Eliminación de newsletters\
-   Filtro de *market commentary*\
-   Remoción de símbolos y ruido

------------------------------------------------------------------------

# 🧠 5. Normalización Semántica

Diccionario financiero:

-   stock, shares → stock\
-   earnings, profit → profit\
-   fall, drop → fall

``` r
body_fixed <- normalize_financial_terms(body_fixed)
```

------------------------------------------------------------------------

# 🍎 Filtrado de Noticias Relevantes de Apple

``` r
gdelt_apple_only <- subset(
  gdelt_apple_clean,
  grepl(pattern, body_fixed, ignore.case = TRUE)
)
```

------------------------------------------------------------------------

# 🔤 6. Tokenización y Stemming

Pipeline:

1.  Tokenización\
2.  Stopwords\
3.  Números y tokens cortos\
4.  Stemming\
5.  Reconstrucción del texto limpio

------------------------------------------------------------------------

# 📊 7. Análisis Exploratorio

-   Wordcloud\
-   Frecuencia por proveedor\
-   Series de tiempo\
-   Distribuciones

Guardados en `/outcomes/`.

------------------------------------------------------------------------

# 🧮 8. Matriz TF--IDF

1.  Conteo de términos\
2.  DF\
3.  Filtrado ≥5%\
4.  Matriz dispersa\
5.  Similitud del coseno

------------------------------------------------------------------------

# 📚 9. Modelos

-   CAR\
-   GRF\
-   Clasificación UP/DOWN

------------------------------------------------------------------------

# ✔ Recomendación de Ejecución

  Tarea                 Ejecutar   Comentario
  --------------------- ---------- ----------------------
  Cargar librerías      ✔          Siempre
  Descargar RSS         Opcional   Rápido
  Descargar GDELT       ❌         Tarda 8 horas
  Cargar CSV            ✔          Iniciar en línea 235
  Procesamiento texto   ✔          Automático
  TF--IDF y modelos     ✔          Con base cargada

------------------------------------------------------------------------

# 👨‍💻 Autor

Proyecto desarrollado para minería de texto aplicada al análisis
financiero.
