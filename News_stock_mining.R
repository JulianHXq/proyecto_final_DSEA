# ============================================================
#    PIPELINE COMPLETO AAPL + GDELT (BODY FIXED)
# ============================================================

# --- 0. PAQUETES -----------------------------------------------------------
require(pacman)
#p_load()
packages <- c(
  "tidyverse","lubridate","data.table","httr","jsonlite","stringi",
  "xml2","tidyquant","purrr","tidyRSS","text2vec","topicmodels",
  "tokenizers","stopwords","tidytext","SnowballC","grf","fixest",
  "tibble","cld3","rvest","wordcloud","Matrix","coop","tidytext","tidyr","stringr"
)
lapply(packages, require, character.only = TRUE)


# --- 1. CONFIGURACIÓN -----------------------------------------------------
TICKER <- "AAPL"
START  <- as.Date(Sys.Date() - 365*2)
END    <- Sys.Date()

# ============================================================
#  2. DESCARGA DE PRECIOS
# ============================================================

prices_xts <- tryCatch({
  getSymbols(
    Symbols = TICKER,
    src = "yahoo",
    from = START,
    to = END,
    auto.assign = FALSE
  )
}, error = function(e){
  message("Error descargando precios: ", e$message)
  return(NULL)
})

if (is.null(prices_xts)) {
  prices <- tibble(
    date = seq.Date(START, END, by = "day"),
    adjusted = NA_real_
  )
} else {
  prices <- data.frame(
    date = index(prices_xts),
    adjusted = as.numeric(prices_xts[, paste0(TICKER, ".Adjusted")])
  ) %>%
    arrange(date)
}

prices <- prices %>%
  mutate(
    logprice = log(adjusted),
    logret   = logprice - lag(logprice)
  ) %>%
  select(date, adjusted, logret)


# ================================================================
#  3. DESCARGA RSS EXTRA (Really Simple Syndication-Feeds de news)
# ================================================================

build_rss_list <- function(ticker) {
  c(
    paste0("https://www.marketwatch.com/rss/company/US/", ticker),
    paste0("https://www.nasdaq.com/feed/rssoutbound?symbol=", ticker),
    paste0("https://feeds.finance.yahoo.com/rss/2.0/headline?s=", ticker, "&region=US&lang=en-US")
  )
}


fetch_rss2 <- function(url) {
  
  message("Descargando RSS: ", url)
  
  txt <- tryCatch(httr::GET(url, timeout(5)), error = function(e) NULL)
  if (is.null(txt)) {
    message("  ✖ Error GET: ", url)
    return(tibble())
  }
  
  xml_txt <- tryCatch(httr::content(txt, "text", encoding = "UTF-8"), error = function(e) "")
  if (xml_txt == "") {
    message("  ✖ RSS vacío: ", url)
    return(tibble())
  }
  
  xml <- tryCatch(read_xml(xml_txt), error = function(e) {
    message("  ✖ XML inválido: ", url)
    NULL
  })
  if (is.null(xml)) return(tibble())
  
  items <- xml_find_all(xml, ".//item")
  if (length(items) == 0) {
    message("  ✖ Sin items: ", url)
    return(tibble())
  }
  
  tibble(
    id = items %>% map_chr(~ xml_text(xml_find_first(.x, "guid")) %||%
                             xml_text(xml_find_first(.x, "link"))),
    title = items %>% map_chr(~ xml_text(xml_find_first(.x, "title"))),
    body = items %>% map_chr(~ xml_text(xml_find_first(.x, "description"))),
    datetime_utc = items %>% 
      map_chr(~ xml_text(xml_find_first(.x, "pubDate"))) %>%
      parse_date_time(orders = c("a, d b Y H:M:S z", "d b Y H:M:S z")),
    provider = url
  )
}


rss_list <- build_rss_list("AAPL")
rss_raw <- map_dfr(rss_list, fetch_rss2)


# ============================================================
#  4. GDELT HISTÓRICO (BODY FIXED)
# ============================================================

# --- Fallback: si GDELT no trae body, scrapear HTML -----------------------------------

safely_get_html_body <- function(url){
  tryCatch({
    h <- httr::GET(url, timeout(5))
    html <- httr::content(h, as = "text", encoding = "UTF-8")
    doc <- read_html(html)
    p <- xml2::xml_find_all(doc, "//p")
    txt <- paste(xml_text(p), collapse = "\n")
    if (nchar(txt) < 100) NA_character_ else txt
  }, error = function(e) NA_character_)
}

# --- Función principal corregida ------------------------------------------------------
get_gdelt_historico <- function(query = "apple", years = 1, chunk_days = 30) { 
  
  end_date <- Sys.Date()
  start_date <- end_date - years * 365
  
  fechas <- seq(start_date, end_date, by = chunk_days)
  fechas_fin <- c(fechas[-1] - 1, end_date)
  
  message("Descargando datos históricos de GDELT…")
  
  scrapear_body <- function(url) {
    tryCatch({
      if(is.na(url) || url == "") return(NA_character_)
      
      message("Scraping: ", url)
      res <- httr::GET(
        url,
        httr::add_headers(
          "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"
        ),
        timeout(10)
      )
      
      if (res$status_code != 200) return(NA_character_)
      
      html <- httr::content(res, as = "text", encoding = "UTF-8")
      doc  <- read_html(html)
      
      p <- doc %>% html_nodes("p") %>% html_text(trim = TRUE)
      body <- paste(p, collapse = "\n")
      
      if (nchar(body) < 150) return(NA_character_)
      return(body)
    }, error = function(e) NA_character_)
  }
  
  get_chunk <- function(i) {
    s <- fechas[i]
    e <- fechas_fin[i]
    
    message(" → Chunk ", i, ": ", s, " → ", e)
    
    base_url <- "https://api.gdeltproject.org/api/v2/doc/doc"
    params <- list(
      query = query,
      mode = "ArtList",
      startdatetime = paste0(format(s, "%Y%m%d"), "000000"),
      enddatetime   = paste0(format(e, "%Y%m%d"), "235959"),
      maxrecords    = 250,
      format        = "json"
    )
    
    headers <- add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)")
    
    res <- tryCatch(GET(base_url, headers, query = params, timeout(20)), error = function(e) NULL)
    if (is.null(res) || res$status_code != 200) return(tibble())
    
    raw_txt <- tryCatch(content(res, as = "text", encoding = "UTF-8"), error = function(e) "")
    if (nchar(raw_txt) == 0) return(tibble())
    
    raw_txt <- gsub("\\\\(?![nrt\"\\\\])", "", raw_txt, perl = TRUE)
    raw_txt <- stringi::stri_encode(raw_txt, "", "UTF-8")
    
    json <- tryCatch(fromJSON(raw_txt, simplifyVector = TRUE), error = function(e) NULL)
    if (is.null(json) || is.null(json$articles)) return(tibble())
    
    df <- as_tibble(json$articles)
    
    # ===========================
    # Columna cuerpo y URL
    # ===========================
    text_candidates <- c("excerpt","content","snippet","summary","body","text")
    text_col <- text_candidates[text_candidates %in% names(df)]
    df$body_raw <- if(length(text_col) > 0) df[[text_col[1]]] else NA_character_
    
    url_candidates <- c("url","documentidentifier")
    url_col <- url_candidates[url_candidates %in% names(df)]
    df$url <- if(length(url_col) > 0) df[[url_col[1]]] else NA_character_
    
    title_candidates <- c("title","title_original")
    title_col <- title_candidates[title_candidates %in% names(df)]
    df$title <- if(length(title_col) > 0) df[[title_col[1]]] else NA_character_
    
    date_candidates <- c("seendate","date","pubdate")
    date_col <- date_candidates[date_candidates %in% names(df)]
    df$datetime_utc <- if(length(date_col) > 0) suppressWarnings(ymd_hms(df[[date_col[1]]], quiet = TRUE)) else NA
    
    prov_candidates <- c("domain","source","publisher")
    prov_col <- prov_candidates[prov_candidates %in% names(df)]
    df$provider <- if(length(prov_col) > 0) df[[prov_col[1]]] else "gdelt"
    
    df <- df %>% select(url, title, body_raw, datetime_utc, provider)
    
    # ===========================
    # Scraper seguro para el body
    # ===========================
    df$body_fixed <- map_chr(df$url, scrapear_body)
    
    df
  }
  
  resultado <- map_dfr(seq_along(fechas), get_chunk)
  
  message("✔ Descarga histórica completa → ", nrow(resultado), " artículos.")
  resultado
}

# ======= EJEMPLO =======
tickers <- c("AAPL")
gdelt_apple <- get_gdelt_historico("AAPL", years = 5)
gdelt_apple %>% select(url, title, body_raw, body_fixed) %>% head()

#gdelt_apple$title 
nrow(gdelt_apple)
head(gdelt_apple)

gdelt_apple <- gdelt_apple %>%
  mutate(lang_title = cld3::detect_language(title)) %>%
  filter(lang_title == "en")
nrow(gdelt_apple)


scrapear_body <- function(url) {
  
  tryCatch({
    message("Scraping: ", url)
    
    res <- httr::GET(
      url,
      httr::add_headers(
        "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"
      ),
      timeout(10)
    )
    
    if (res$status_code != 200) return(NA_character_)
    
    html <- httr::content(res, as = "text", encoding = "UTF-8")
    doc  <- read_html(html)
    
    # Extraer párrafos
    p <- doc %>% html_nodes("p") %>% html_text(trim = TRUE)
    body <- paste(p, collapse = "\n")
    
    if (nchar(body) < 150) return(NA_character_)  # texto demasiado corto, probablemente error
    
    return(body)
  },
  error = function(e) {
    return(NA_character_)
  })
}

# APLICAR SCRAPER A TODOS LOS IDS DE GDELT
gdelt_apple$body_fixed <- map_chr(gdelt_apple$id, scrapear_body)

# Mostrar los primeros cuerpos descargados
gdelt_apple %>% select(id, title, body_raw, body_fixed) %>% head()

#Para guardar la base de datos dado que demora 8 horas haciendo web-scraping
write.csv(gdelt_apple, "C:/Users/User/Desktop/CienciaDatos_Econometria/P.Final/proyecto_final_DSEA/gdelt_apple.csv", row.names = FALSE)

#Cargar la base de datos
gdelt_apple <- read_csv("C:/Users/User/Desktop/CienciaDatos_Econometria/P.Final/proyecto_final_DSEA/gdelt_apple.csv")

gdelt_apple <- gdelt_apple %>%
  select(-body_raw) %>%      # elimina la columna body_raw
  filter(!is.na(body_fixed)) # mantiene solo filas donde body_fixed tiene texto

nrow(gdelt_apple)
head(gdelt_apple)

# Limpiar errores y copyrights
gdelt_apple_clean <- gdelt_apple %>%
  mutate(
    body_fixed = body_fixed %>%
      # quitar "Oops, something went wrong" al inicio
      sub("^Oops, something went wrong\\s*", "", .) %>%
      # quitar líneas de copyright genéricas
      gsub("© [0-9]{4} .*?\\.\\s+ALL RIGHTS RESERVED\\.", "", .) %>%
      # quitar copyright específico de Benzinga
      gsub("© 2025 Benzinga\\.com\\. Benzinga does not provide investment advice\\. All rights reserved\\.", "", ., fixed = TRUE) %>%
      # quitar espacios iniciales y finales
      str_trim(),
    # detectar idioma de título y cuerpo
    lang_title = cld3::detect_language(title),
    lang_body  = cld3::detect_language(body_fixed)
  ) %>%
  # mantener solo inglés y body_fixed no vacío
  filter(
    lang_title == "en",
    lang_body  == "en",
    !is.na(body_fixed),
    nchar(body_fixed) > 0
  )

gdelt_apple_clean <- gdelt_apple %>%
  filter(!str_detect(body_fixed, "Benzinga"))

# Revisar resultados
nrow(gdelt_apple_clean)
#colnames(gdelt_apple)

# Asegurarse de que body_fixed sea de tipo character
gdelt_apple$body_fixed <- as.character(gdelt_apple$body_fixed)

# Lista de términos relacionados con Apple
terms_to_include <- c("Apple", "AAPL", "iPhone", "iPad", "Mac", "Apple Inc")  # Palabras clave
pattern <- paste(terms_to_include, collapse = "|")  # "Apple|AAPL|iPhone|..."

# Filtrar filas que contengan al menos una palabra clave
gdelt_apple_only <- subset(gdelt_apple_clean, grepl(pattern, body_fixed, ignore.case = TRUE))


# Convertir cada fila en palabras individuales
words_df <- gdelt_apple_only %>%
  unnest_tokens(word, body_fixed)

word_counts <- words_df %>%
  count(word, sort = TRUE)

data("stop_words")  # Stop words en inglés
word_counts <- word_counts %>%
  anti_join(stop_words, by = "word")  # Elimina palabras comunes tipo "the", "and", etc.

head(word_counts, 20)  # Top 20 palabras más repetidas


set.seed(123)  # Para reproducibilidad
par(mar = c(0,0,0,0))  # Márgenes mínimos
wordcloud(words = word_counts$word,
          freq = word_counts$n,
          max.words = 100,   # Más palabras permitidas
          colors = brewer.pal(8, "Dark2"))

#-------------Grafica con frecuencia de noticias por proveedor--------------
# Calcular top 5 proveedores por número total de noticias
top_providers <- gdelt_apple_only %>%
  group_by(provider) %>%
  summarise(total_news = n()) %>%
  arrange(desc(total_news)) %>%
  slice_head(n = 5) %>%
  pull(provider)

# Filtrar solo esos proveedores
news_counts_top <- gdelt_apple_only %>%
  filter(provider %in% top_providers) %>%
  mutate(date = as.Date(datetime_utc)) %>%
  group_by(provider, date) %>%
  summarise(n_news = n(), .groups = "drop")

# Gráfico de línea
line_plot <- ggplot(news_counts_top, aes(x = date, y = n_news, color = provider)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 1.5) +
  labs(title = "Top 5 proveedores por número de noticias",
       x = "Fecha",
       y = "Número de noticias") +
  theme_light() +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background  = element_rect(fill = "white"),
    legend.background = element_rect(fill = "white"),
    legend.key = element_rect(fill = "white")
  )

ggsave("C:/Users/User/Desktop/CienciaDatos_Econometria/P.Final/proyecto_final_DSEA/outcomes/grafico_linea.png",
       plot = line_plot, width = 10, height = 6, dpi = 300)

# Gráfico de boxplot
box_plot <- ggplot(news_counts_top, aes(x = provider, y = n_news, fill = provider)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, linewidth = 0.8) +
  labs(title = "Distribución diaria del número de noticias por proveedor",
       x = "Proveedor",
       y = "Número de noticias por día") +
  theme_light() +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background  = element_rect(fill = "white"),
    legend.background = element_rect(fill = "white"),
    legend.key = element_rect(fill = "white")
  )

ggsave("C:/Users/User/Desktop/CienciaDatos_Econometria/P.Final/proyecto_final_DSEA/outcomes/grafico_boxplot.png",
       plot = box_plot, width = 8, height = 6, dpi = 300)


#----------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------
# modelo de procesamiento de texto: GDELT 5 años -> TF-IDF -> dedupe -> LDA -> CAR -> GRF
# ---------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------
# Asegurarnos que body_fixed sea character
gdelt_apple_only$body_fixed <- as.character(gdelt_apple_only$body_fixed)

# Limpiar texto y remover copyright / errores

# =======================================
# 1) Limpieza inicial del texto
# =======================================
gdelt_apple_clean_text <- gdelt_apple_only %>%
  mutate(
    text = str_to_lower(body_fixed),
    text = str_replace_all(text, "<.*?>", " "),
    text = str_replace_all(text, "[^a-z0-9\\s]", " "),
    text = str_replace_all(text, "\\b([0-9]+)(st|nd|rd|th)\\b", "\\1"),
    text = str_replace_all(text, "(?<=[a-z])(?=[0-9])|(?<=[0-9])(?=[a-z])", " "),
    text = str_squish(text)
  )

# =======================================
# 2) Detectar idioma inglés
# =======================================
gdelt_apple_clean_text <- gdelt_apple_clean_text %>%
  mutate(lang = cld3::detect_language(text)) %>%
  filter(lang == "en") %>%
  select(-lang)

# =======================================
# 3) Tokenizar
# =======================================
tokens <- gdelt_apple_clean_text %>%
  unnest_tokens(word, text)

# =======================================
# 4) Eliminar stopwords, números, tokens con números, palabras cortas
# =======================================
sw <- stopwords("en")

tokens <- tokens %>%
  filter(!word %in% sw) %>%
  filter(!str_detect(word, "^[0-9]+$")) %>%   # elimina números puros
  filter(!str_detect(word, "[0-9]")) %>%      # elimina tokens con números mezclados
  filter(str_length(word) >= 3) %>%
  mutate(word = wordStem(word))               # stemming

# =======================================
# 5) Reconstruir texto final por documento (IMPORTANTE)
# =======================================
news_stemmed <- tokens %>%
  group_by(url) %>%
  summarise(text_final = paste(word, collapse = " "), .groups = "drop")

# =======================================
# 6) Unir con datos originales limpios
# =======================================
gdelt_apple_clean <- gdelt_apple_clean_text %>%
  left_join(news_stemmed, by = "url") %>%
  mutate(text_final = trimws(text_final)) %>%
  filter(!is.na(text_final), text_final != "")

#------------------------------------------
#-------------Matriz TF-IDF----------------
#------------------------------------------
# Calcular document frequency
df_tbl <- term_counts %>%
  group_by(word) %>%
  summarise(doc_freq = n_distinct(url), .groups = "drop")

D <- n_distinct(term_counts$url)       # total documentos
min_frac <- 0.05                       # mantener términos presentes en >= 5% docs
min_docs <- ceiling(min_frac * D)

keep_words <- df_tbl %>%
  filter(doc_freq >= min_docs) %>%
  pull(word)


#======================================================
#--- 3. Crear TF–IDF SOLO con términos filtrados ------
#======================================================

tfidf_tbl <- term_counts %>%
  filter(word %in% keep_words) %>%       # <---- FILTRO CLAVE
  count(url, word) %>% 
  bind_tf_idf(term = word, document = url, n = n)

#======================================================
#--- 4. Crear sparse matrix (TF–IDF) ------------------
#======================================================
docs  <- unique(tfidf_tbl$url)
terms <- unique(tfidf_tbl$word)

doc_index  <- match(tfidf_tbl$url, docs)
term_index <- match(tfidf_tbl$word, terms)

M <- sparseMatrix(
  i = doc_index,
  j = term_index,
  x = tfidf_tbl$tf_idf,
  dims = c(length(docs), length(terms)),
  dimnames = list(docs, terms)
)

rownames(M)[1:5]


#======================================================
#--- 5. Similitud de coseno ----------------------------
#======================================================

# M es la matriz TF-IDF dispersa (sparseMatrix)
sim_matrix <- coop::cosine(t(M))

# Añadir los resultados según tu necesidad

#======================================================
#--- 6. Creacion de pares de noticias similares--------
#======================================================

sim_df <- as.data.frame(as.table(sim_matrix))
colnames(sim_df) <- c("url1", "url2", "sim")

sim_df <- sim_df %>%
  mutate(
    url1 = as.character(url1),
    url2 = as.character(url2)
  ) %>%
  filter(url1 < url2) %>%   # evitar duplicados (A,B) y (B,A)
  filter(sim >= 0.6)       # umbral de similitud

#======================================================
#--- 7. AÑADIR FECHAS Y AGRUPAR POR SEMANA--------
#======================================================

# --- 1. Dejar news_dates con una fila única por URL ---
news_dates_unique <- news_dates %>%
  distinct(url, .keep_all = TRUE)


# --- 2. Hacer el join limpio sin duplicaciones ---
sim_df <- sim_df %>%
    mutate(
      url1 = as.character(url1),
      url2 = as.character(url2)
    ) %>%
    
    # -------- JOIN para url1 --------
  left_join(
    news_dates_unique %>%
      select(url, datetime_utc, text_final) %>%
      rename(date1 = datetime_utc, text1 = text_final),
    by = c("url1" = "url")
  ) %>%
    
    # -------- JOIN para url2 --------
  left_join(
    news_dates_unique %>%
      select(url, datetime_utc, text_final) %>%
      rename(date2 = datetime_utc, text2 = text_final),
    by = c("url2" = "url")
  )

#======================================================
#--- 8. REGLA PARA ELEGIR CUÁL CONSERVAR--------
#======================================================
#1-)Si una noticia es más antigua → conservarla
#2-)Si tienen la misma fecha → conservar la más 

sim_df <- sim_df %>%
  mutate(
    len1 = nchar(text1 %||% ""),
    len2 = nchar(text2 %||% ""),
    keep = case_when(
      date1 < date2 ~ url1,
      date2 < date1 ~ url2,
      len1 >= len2 ~ url1,
      TRUE ~ url2
    ),
    drop = ifelse(keep == url1, url2, url1)
  )


#======================================================
#--- 9. LISTA FINAL DE URLs A ELIMINAR-----------------
#======================================================

urls_to_drop <- unique(sim_df$drop)
length(urls_to_drop)

#======================================================
#--- 10. CREAR DATASET FINAL SIN DUPLICADOS------------
#======================================================

gdelt_apple_dedup <- gdelt_apple_clean %>%
  filter(!url %in% urls_to_drop)
dim(gdelt_apple_dedup)
colnames(gdelt_apple_dedup)
#======================================================
#--- 11. Gráfica de la frecuencia de las noticias------
#======================================================
gdelt_apple_dedup <- gdelt_apple_dedup %>%
  mutate(date = as.Date(datetime_utc))

# Conteo por fecha y proveedor
news_counts <- gdelt_apple_dedup %>%
  group_by(provider, date) %>%
  summarise(n_news = n(), .groups = "drop")

top_providers <- news_counts %>%
  group_by(provider) %>%
  summarise(total = sum(n_news)) %>%
  arrange(desc(total)) %>%
  slice(1:5) %>%
  pull(provider)

news_counts_top <- news_counts %>%
  filter(provider %in% top_providers)


line_plot2 <- ggplot(news_counts_top,
                     aes(x = date, y = n_news, color = provider)) +
  geom_line(linewidth = 1) +    
  geom_point(size = 1.5) +        
  labs(
    title = "Top 5 proveedores limpio por número de noticias",
    x = "Fecha",
    y = "Número de noticias"
  ) +
  scale_y_continuous(limits = c(0, 10)) +   # <- fijar eje Y de 0 a 10
  theme_light() +
  theme(
    panel.background  = element_rect(fill = "white"),
    plot.background   = element_rect(fill = "white"),
    legend.background = element_rect(fill = "white"),
    legend.key        = element_rect(fill = "white"),
    axis.text.x       = element_text(angle = 45, hjust = 1),
    plot.title        = element_text(face = "plain", size = 14)
  )

line_plot2
library(patchwork)

combined_plot <- line_plot + line_plot2
combined_plot
# Guardar la imagen
ggsave("C:/Users/User/Desktop/CienciaDatos_Econometria/P.Final/proyecto_final_DSEA/outcomes/comparacion_proveedores.png", combined_plot, width = 12, height = 6, dpi = 300)
#======================================================
#             LDA Model to topics
#======================================================
#---------------------------------------------
# Paquetes necesarios
#---------------------------------------------
require("pacman")
p_load("tidyverse", "tidytext", "tm", "stopwords", "textstem", "maptpx", "stringi")

#---------------------------------------------
# Preparamos el corpus con tus noticias deduplicadas
#---------------------------------------------
# Asegúrate de tener: gdelt_apple_dedup con columnas text_final y date (o datetime_utc)
library(tm)
library(stringi)

# Convertir a caracteres y eliminar NAs/vacíos
docs <- as.character(gdelt_apple_dedup$text_final)
docs <- docs[!is.na(docs)]
docs <- docs[docs != ""]

# Normalizar acentos y caracteres especiales
docs <- stri_trans_general(docs, id = "Latin-ASCII")

# Crear corpus
corpus <- Corpus(VectorSource(docs))

# Limpieza robusta
corpus <- tm_map(corpus, content_transformer(function(x) tolower(as.character(x))))
corpus <- tm_map(corpus, content_transformer(function(x) removePunctuation(as.character(x))))
corpus <- tm_map(corpus, content_transformer(function(x) removeNumbers(as.character(x))))
corpus <- tm_map(corpus, content_transformer(function(x) stripWhitespace(as.character(x))))

sw_en <- stopwords("en")
# Palabras adicionales a eliminar
custom_words <- c("nvda", "google", "stock", "semiconductors")
# Combinar stopwords
stopwords_total <- union(sw_en, custom_words)
# Aplicar al corpus
corpus <- tm_map(corpus, removeWords, stopwords_total)
# Lematización
corpus <- tm_map(corpus, lemmatize_strings)

#==============================================================================
# Contar palabras por documento
doc_word_counts <- sapply(corpus, function(doc) {
  length(unlist(strsplit(as.character(doc), "\\s+")))
})
# Convertir a data.frame
df_counts <- data.frame(
  document = seq_along(doc_word_counts),
  word_count = doc_word_counts
)

# Histograma de palabras por documento
ggplot(df_counts, aes(x = word_count)) +
  geom_histogram(binwidth = 10, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Distribución del número de palabras por documento",
       x = "Cantidad de palabras",
       y = "Número de documentos") +
  theme_minimal()

# Boxplot opcional
ggplot(df_counts, aes(y = word_count)) +
  geom_boxplot(fill = "orange", color = "black", alpha = 0.7) +
  labs(title = "Boxplot del número de palabras por documento",
       y = "Cantidad de palabras") +
  theme_minimal()
#==============================================================================
#---------------------------------------------
# Crear DocumentTermMatrix
#---------------------------------------------

# Filtrar documentos con >= 100 palabras
corpus_filtered <- corpus[doc_word_counts >= 250]
# Ver cuántos documentos quedan
length(corpus_filtered) #corpus #============================================

dtm <- DocumentTermMatrix(corpus_filtered)

# Convertir a matriz
dtm_matrix <- as.matrix(dtm)

#---------------------------------------------
# Ajustar modelo LDA con maptpx
#---------------------------------------------
set.seed(123)

# Elegimos, por ejemplo, K=3 tópicos
lda_model <- topics(dtm_matrix, K = 5)

#---------------------------------------------
# Probabilidades palabra-tópico (theta)
#---------------------------------------------
theta_matrix <- lda_model$theta

theta_tidy <- as.data.frame(theta_matrix) %>%
  mutate(term = rownames(theta_matrix)) %>%
  pivot_longer(cols = -term, names_to = "topic", values_to = "beta") %>%
  mutate(topic = as.numeric(gsub("V","",topic)))

# Top 10 términos por tópico
top_terms <- theta_tidy %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Visualización
top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales="free") +
  scale_y_reordered() +
  labs(title="Top 10 términos por tópico",
       x="Beta (probabilidad palabra-tópico)",
       y="Término")

#---------------------------------------------
# Probabilidades documento-tópico (omega)
#---------------------------------------------
omega_matrix <- lda_model$omega

omega_tidy <- as.data.frame(omega_matrix) %>%
  mutate(document = row_number()) %>%
  pivot_longer(cols = -document, names_to = "topic", values_to = "gamma") %>%
  mutate(topic = as.numeric(gsub("V","",topic)))

# Visualización distribución gamma por tópico
omega_tidy %>%
  ggplot(aes(x=factor(topic), y=gamma, fill=factor(topic))) +
  geom_boxplot() +
  labs(title="Distribución de probabilidades γ por tópico",
       x="Tópico", y="Probabilidad documento-tópico", fill="Tópico")

#---------------------------------------------
# Matriz documento-tópico para similitud
#---------------------------------------------
gamma_wide <- omega_tidy %>%
  pivot_wider(names_from=topic, values_from=gamma, names_prefix="topic_")

gamma_matrix <- as.matrix(gamma_wide[, -1])
rownames(gamma_matrix) <- gamma_wide$document

# Función de similitud coseno
cosine_similarity <- function(mat){
  norm_mat <- mat / sqrt(rowSums(mat^2))
  sim <- norm_mat %*% t(norm_mat)
  return(sim)
}

similarity_matrix <- cosine_similarity(gamma_matrix)

# Ejemplo: top 5 noticias más similares al primer documento
doc1_sim <- similarity_matrix[1, ]
top5 <- sort(doc1_sim, decreasing = TRUE)[1:6]
print("Top 5 noticias más similares al documento 1:")
print(top5)

#===============================================================================
#===============================================================================
#===============================================================================

# Crear nombres cortos
short_names <- paste0("Doc", seq_len(nrow(sim_matrix)))

# Guardamos un diccionario para ti (opcional)
url_map <- data.frame(
  short = short_names,
  url = rownames(sim_matrix)
)

# Reemplazar rownames y colnames
rownames(cosine_sim) <- short_names
colnames(cosine_sim) <- short_names

# --- Ahora sí crear el block 20x20 ---
block <- as.matrix(cosine_sim[1:100, 1:100])

df_plot <- as.data.frame(as.table(block))
colnames(df_plot) <- c("Doc1", "Doc2", "Similarity")

ggplot(df_plot, aes(Doc1, Doc2, fill = Similarity)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  labs(title = "Cosine Similarity (20x20)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

#----------------------------------------- LDA Model----------------------------------------- 
#--------------------------------------------------------------------------------------------
library(topicmodels)
library(tidytext)

# Crear DocumentTermMatrix
dtm <- term_counts %>%
  rename(document = id, term = word, count = n) %>%
  cast_dtm(document, term, count)

# Número de topics
NUM_TOPICS <- 5
set.seed(123)
lda_model <- LDA(dtm, k = NUM_TOPICS, control = list(seed = 123))

# Obtener topic dominante por documento
doc_topics <- tidy(lda_model, matrix = "gamma") %>%
  group_by(document) %>%
  slice_max(gamma, n = 1) %>%
  ungroup() %>%
  transmute(id = document, topic = as.integer(topic))

gdelt_apple_clean <- gdelt_apple_clean %>%
  left_join(doc_topics, by = "id") %>%
  mutate(topic = ifelse(is.na(topic), 0L, topic))

#------------------------------------------
#----Visualizar las palabras por topic-----
#------------------------------------------
terms_by_topic <- tidy(lda_model, matrix = "beta") %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  summarise(terms = paste(term, collapse = ", "), .groups = "drop")

write.csv(terms_by_topic, "lda_terms_by_topic.csv", row.names = FALSE)

#------------------------------------------
#------Wordcloud global o por topic--------
#------------------------------------------

library(wordcloud)
library(RColorBrewer)

word_counts <- tokens %>%
  count(word, sort = TRUE)

set.seed(123)
wordcloud(words = word_counts$word,
          freq = word_counts$n,
          max.words = 100,
          colors = brewer.pal(8, "Dark2"))







