# ============================================================
#    PIPELINE COMPLETO AAPL + GDELT (BODY FIXED)
# ============================================================

# --- 0. PAQUETES -----------------------------------------------------------
require(pacman)
#p_load()
require(tidyverse,lubridate,data.table,httr,jsonlite,stringi,xml2,tidyquant,
        purrr,tidyRSS,text2vec,topicmodels,tokenizers,stopwords,tidytext,
        SnowballC,grf,fixest,tibble,cld3,rvest)

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
library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(rvest)
library(stringi)

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

write.csv(gdelt_apple, "C:/Users/User/Desktop/CienciaDatos_Econometria/P.Final/gdelt_apple.csv", row.names = FALSE)

#Cargar la base de datos
gdelt_apple <- read_csv("C:/Users/User/Desktop/CienciaDatos_Econometria/P.Final/gdelt_apple.csv")

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
colnames(gdelt_apple)

# Asegurarse de que body_fixed sea de tipo character
gdelt_apple$body_fixed <- as.character(gdelt_apple$body_fixed)

# Convertir cada fila en palabras individuales
words_df <- gdelt_apple %>%
  unnest_tokens(word, body_fixed)

word_counts <- words_df %>%
  count(word, sort = TRUE)

data("stop_words")  # Stop words en inglés
word_counts <- word_counts %>%
  anti_join(stop_words, by = "word")  # Elimina palabras comunes tipo "the", "and", etc.

head(word_counts, 50)  # Top 20 palabras más repetidas

library(wordcloud)

set.seed(123)  # Para reproducibilidad
wordcloud(words = word_counts$word,
          freq = word_counts$n,
          max.words = 100,
          colors = brewer.pal(8, "Dark2"))



# Lista de términos relacionados con Apple
terms_to_exclude <- c("Apple", "AAPL", "iPhone", "iPad", "Mac", "Apple Inc","aaple's",'apple','aapl')
pattern <- paste(terms_to_exclude, collapse = "|")  # Crea expresión regular "Apple|AAPL|iPhone|..."
gdelt_no_apple <- subset(gdelt_apple, !grepl(pattern, body_fixed, ignore.case = TRUE))





#--------------------------------------------------------------------------#
#-------------------------------Continuar acá------------------------------#
#--------------------------------------------------------------------------#
# --- 4. Preprocesamiento y tokens -----------------------------------------
news_clean <- news_raw %>%
  mutate(text = str_to_lower(str_replace_all(body, "<.*?>", " ")) %>%
           str_replace_all("[^[:alnum:]\\s]", " ")) %>%
  select(id, title, text, datetime_utc, provider)


tokens <- news_clean %>% unnest_tokens(word, text)
# remover stopwords en inglés (ajusta si tus noticias son en español)
tokens <- tokens %>% filter(!word %in% stopwords::stopwords("en"))
# stem
tokens <- tokens %>% mutate(word = wordStem(word))


# reconstruir documentos (palabras stemmed separadas por espacio)


#----------------------------------------------------------------------------
# ---------------------------------------------------------------------
# PIPELINE COMPLETO: GDELT 5 años -> TF-IDF -> dedupe -> LDA -> CAR -> GRF
# ---------------------------------------------------------------------
# Requiere: instalar paquetes si no los tienes:
# install.packages(c("tidyverse","lubridate","tidytext","text2vec","grf",
#                    "data.table","quantmod","tokenizers","SnowballC",
#                    "stopwords","topicmodels","httr","jsonlite","purrr",
#                    "Matrix","igraph","tidyquant","tm"))
#
# Nota: adapta memory/objetos si tienes muchísimos artículos (miles).
# ---------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(tidytext)
library(text2vec)
library(grf)
library(data.table)
library(quantmod)
library(tokenizers)
library(SnowballC)
library(stopwords)
library(topicmodels)
library(httr)
library(jsonlite)
library(purrr)
library(Matrix)
library(igraph)
library(tm)
library(tibble)

# ---------------------------
# 0. Parámetros
# ---------------------------
QUERY <- "apple"
YEARS <- 5
CHUNK_DAYS <- 30     # dividir 5 años en chunks (30 días)
SIM_THRESH <- 0.85   # umbral similitud coseno para deduplicar
NUM_TOPICS <- 12     # número de topics LDA (ajustable)
TICKER <- "AAPL"
START_PRICE <- as.Date(Sys.Date() - YEARS*365)
END_PRICE <- Sys.Date()

# ---------------------------
# 1. Función robusta: descargar histórico GDELT en chunks
# ---------------------------
get_gdelt_historico <- function(query = "apple", years = 5, chunk_days = 30) {
  library(httr); library(jsonlite); library(dplyr); library(lubridate)
  library(purrr); library(stringi)
  
  end_date <- min(Sys.Date(), as.Date("2024-11-28"))
  start_date <- end_date - years * 365
  fechas <- seq(start_date, end_date, by = chunk_days)
  fechas_fin <- c(fechas[-1] - 1, end_date)
  
  message("Descargando datos históricos de GDELT (chunks): ", start_date, " → ", end_date)
  
  get_chunk <- function(i) {
    s <- fechas[i]; e <- fechas_fin[i]
    message("  Chunk ", i, ": ", s, " -> ", e)
    base_url <- "https://api.gdeltproject.org/api/v2/doc/doc"
    params <- list(
      query = paste(query, "AND news"),
      mode = "ArtList",
      startdatetime = paste0(format(s, "%Y%m%d"), "000000"),
      enddatetime = paste0(format(e, "%Y%m%d"), "235959"),
      maxrecords = 250,
      format = "json"
    )
    headers <- httr::add_headers(
      "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"
    )
    res <- tryCatch(httr::GET(base_url, headers, query = params, timeout(30)),
                    error = function(e) { message("  GET error: ", e$message); return(NULL) })
    if (is.null(res) || res$status_code != 200) {
      message("  No data for chunk (HTTP != 200).")
      return(tibble())
    }
    raw_txt <- tryCatch(httr::content(res, as = "text", encoding = "UTF-8"),
                        error = function(e) "")
    if (nchar(raw_txt) == 0) return(tibble())
    # limpiar JSON sucio
    raw_txt <- gsub("\\\\(?![nrt\"\\\\])", "", raw_txt, perl = TRUE)
    raw_txt <- stringi::stri_encode(raw_txt, "", "UTF-8")
    json <- tryCatch(jsonlite::fromJSON(raw_txt, simplifyVector = TRUE), error = function(e) NULL)
    if (is.null(json) || is.null(json$articles) || length(json$articles) == 0) return(tibble())
    
    df <- as_tibble(json$articles)
    # elegir columna de texto dinámicamente
    text_col <- intersect(c("excerpt","content","snippet","summary","body","text"), names(df))
    df$body <- if (length(text_col)==0) NA_character_ else df[[text_col[1]]]
    # id
    id_col <- intersect(c("url","documentidentifier"), names(df))
    df$id <- if (length(id_col)==0) paste0("id_", seq_len(nrow(df))) else df[[id_col[1]]]
    # title
    title_col <- intersect(c("title","title_original"), names(df))
    df$title <- if (length(title_col)==0) NA_character_ else df[[title_col[1]]]
    # date
    date_col <- intersect(c("seendate","date","pubdate"), names(df))
    df$datetime_utc <- if (length(date_col)==0) NA else ymd_hms(df[[date_col[1]]], quiet = TRUE)
    prov_col <- intersect(c("domain","source","publisher"), names(df))
    df$provider <- if (length(prov_col)==0) "gdelt" else df[[prov_col[1]]]
    df %>% select(id, title, body, datetime_utc, provider)
  }
  
  resultado <- map_dfr(seq_along(fechas), get_chunk)
  message("Descarga GDELT finalizada. Artículos:", nrow(resultado))
  return(resultado)
}

# ---------------------------
# 2. Bajar noticias históricas (GDELT)
# ---------------------------
news_raw <- get_gdelt_historico(QUERY, YEARS, CHUNK_DAYS)

if (nrow(news_raw) == 0) {
  stop("GDELT no devolvió artículos. Revisa conexión o parámetros.")
}

# ---------------------------
# 3. Filtrar por menciones explícitas de Apple (reduce ruido)
# ---------------------------
kw <- "apple|aapl|iphone|ipad|mac|tim cook|cupertino|ios|app store|appstore|macbook|watch"

news_raw <- news_raw %>%
  mutate(
    title = ifelse(is.na(title), "", title),
    body = ifelse(is.na(body), "", body),
    datetime_utc = ifelse(is.na(datetime_utc), as.character(Sys.time()), as.character(datetime_utc)),
    datetime_utc = as_datetime(datetime_utc, tz = "UTC")
  ) %>%
  filter(
    str_detect(str_to_lower(title), kw) |
      str_detect(str_to_lower(body), kw)
  ) %>%
  distinct(id, .keep_all = TRUE) %>%
  arrange(datetime_utc)

message("Después de filtrar por keywords, artículos:", nrow(news_raw))

# ---------------------------
# 4. Descargar precios AAPL (Yahoo via quantmod)
# ---------------------------
prices_xts <- tryCatch({
  getSymbols(Symbols = TICKER, src = "yahoo", from = START_PRICE, to = END_PRICE, auto.assign = FALSE)
}, error = function(e) {
  message("quantmod getSymbols failed: ", e$message)
  return(NULL)
})

if (is.null(prices_xts)) {
  stop("No se pudieron descargar precios AAPL. Revisa conexión o considera API alternativa.")
}

prices <- data.frame(date = index(prices_xts),
                     adjusted = as.numeric(prices_xts[, paste0(TICKER, ".Adjusted")])) %>%
  arrange(date) %>%
  mutate(logprice = log(adjusted),
         logret = logprice - lag(logprice)) %>%
  filter(!is.na(logret))

# ---------------------------
# 5. Preprocesamiento texto (clean -> tokens -> stemming)
# ---------------------------
news_clean <- news_raw %>%
  mutate(text = str_to_lower(body),
         text = str_replace_all(text, "<.*?>", " "),
         text = str_replace_all(text, "[^[:alnum:]\\s]", " "),
         text = str_squish(text)) %>%
  select(id, title, text, datetime_utc, provider)

# tokenizar (unnest_tokens)
tokens <- news_clean %>%
  unnest_tokens(word, text)

# remover stopwords en inglés + palabras cortas
sw <- stopwords::stopwords("en")
tokens <- tokens %>%
  filter(!word %in% sw) %>%
  filter(str_length(word) >= 3) %>%
  mutate(word = wordStem(word))

# contar términos por documento
term_counts <- tokens %>%
  count(id, word, sort = FALSE) %>%
  ungroup()

# reconstruir texto stemmed por documento (útil para features)
news_stemmed <- term_counts %>%
  group_by(id) %>%
  summarise(text_final = paste(word, collapse = " "), .groups = "drop")

news_clean <- news_clean %>% left_join(news_stemmed, by = "id")

# ---------------------------
# 6. TF-IDF matrix (sparse) y similitud coseno
# ---------------------------
# calcular tf-idf usando tidytext
tfidf_tbl <- term_counts %>%
  rename(document = id, term = word, n = n) %>%
  bind_tf_idf(term, document, n) %>%
  select(document, term, tf_idf)

# crear índices para sparse matrix
docs <- tfidf_tbl$document %>% unique()
terms <- tfidf_tbl$term %>% unique()
doc_index <- match(tfidf_tbl$document, docs)
term_index <- match(tfidf_tbl$term, terms)

# construir matriz sparse (dgCMatrix)
M <- sparseMatrix(i = doc_index, j = term_index, x = tfidf_tbl$tf_idf,
                  dims = c(length(docs), length(terms)),
                  dimnames = list(docs, terms))

# similitud coseno (text2vec::sim2)
# normalización L2 ya es soportada por sim2
sim_mat <- sim2(M, method = "cosine", norm = "l2")

# ---------------------------
# 7. Dedupe por clusters de similitud > SIM_THRESH
# ---------------------------
# construir grapho de similitud (solo pares > umbral)
pairs <- which(sim_mat > SIM_THRESH & lower.tri(sim_mat), arr.ind = TRUE)
if (nrow(pairs) > 0) {
  g <- graph_from_edgelist(as.matrix(pairs), directed = FALSE)
  comps <- components(g)$membership
  # map node id -> doc name
  doc_names <- rownames(sim_mat)
  keep_ids <- character(0)
  # para cada componente, escoger el representante (más largo texto_final)
  comp_list <- split(names(comps), comps)
  for (comp_id in unique(comps)) {
    members <- names(comps[comps == comp_id])
    # pick longest text_final
    lens <- news_clean %>% filter(id %in% members) %>% mutate(len = nchar(text_final))
    rep_id <- lens %>% arrange(desc(len)) %>% slice(1) %>% pull(id)
    keep_ids <- c(keep_ids, rep_id)
  }
  news_dedupe <- news_clean %>% filter(id %in% keep_ids)
} else {
  news_dedupe <- news_clean
}

message("Noticias después de deduplicar:", nrow(news_dedupe))

# ---------------------------
# 8. Construir DTM de conteos para LDA (topicmodels requiere DocumentTermMatrix)
# ---------------------------
# usar term_counts pero solo con documentos en news_dedupe
term_counts_d <- term_counts %>% filter(document %in% news_dedupe$id)

# crear DocumentTermMatrix via tidytext::cast_dtm
dtm <- term_counts_d %>%
  rename(document = document, term = word, count = n) %>%
  cast_dtm(document, term, count)

# ---------------------------
# 9. LDA (topicmodels)
# ---------------------------
k <- NUM_TOPICS
set.seed(123)
lda_model <- LDA(dtm, k = k, control = list(seed = 123, verbose = 0))

# extraer topic por documento (gamma)
library(tidytext)
doc_topics <- tidy(lda_model, matrix = "gamma") %>%
  group_by(document) %>%
  slice_max(gamma, n = 1) %>%
  ungroup() %>%
  transmute(id = document, topic = as.integer(topic))

# agregar topic a news_dedupe
news_topics <- news_dedupe %>%
  left_join(doc_topics, by = c("id"))

# si hay docs sin topic (NA), asignar 0
news_topics <- news_topics %>% mutate(topic = ifelse(is.na(topic), 0L, topic))

# ---------------------------
# 10. Alinear noticias con precios y calcular CAR (0-1)
# ---------------------------
# convertir fecha de noticia a fecha del mercado (ajustar a fecha)
news_topics <- news_topics %>%
  mutate(date = as.Date(datetime_utc))

# juntar con precios (match por date)
news_prices <- news_topics %>%
  left_join(prices %>% select(date, logret), by = "date") %>%
  arrange(date)

# CAR 0-1 = logret_t + logret_{t+1}
news_prices <- news_prices %>%
  group_by(id) %>%
  mutate(
    logret_t = logret,
    logret_t1 = lead(logret, 1),
    CAR_0_1 = coalesce(logret_t, 0) + coalesce(logret_t1, 0)
  ) %>%
  ungroup()

# quedan NA donde no hay precios; quitar antes de GRF
news_for_grf <- news_prices %>%
  filter(!is.na(CAR_0_1))

message("Observaciones disponibles para GRF:", nrow(news_for_grf))

# ---------------------------
# 11. Preparar covariables y correr causal_forest por cada topic
# ---------------------------
# crear variables: text length, numeric day, provider dummies
news_for_grf <- news_for_grf %>%
  mutate(
    text_len = nchar(text_final),
    daynum = as.numeric(date - min(date))
  ) %>%
  mutate(provider = ifelse(is.na(provider) | provider == "", "unknown", provider))

# codificar proveedor como dummies
provider_dummies <- model.matrix(~ provider - 1, data = news_for_grf) %>% as.data.frame()
grf_df <- bind_cols(news_for_grf %>% select(id, CAR_0_1, topic, text_len, daynum), provider_dummies)

# función para ejecutar causal forest para un topic j
run_causal_by_topic <- function(j, df) {
  dfj <- df %>%
    mutate(W = ifelse(topic == j, 1, 0)) %>%
    filter(!is.na(CAR_0_1))
  if (sum(dfj$W) < 5) {
    return(tibble(topic = j, n_treated = sum(dfj$W), ATE = NA_real_, se = NA_real_))
  }
  Y <- dfj$CAR_0_1
  W <- dfj$W
  X <- dfj %>% select(-id, -CAR_0_1, -topic, -W) %>% as.matrix()
  # quitar columnas constantes
  keep_cols <- which(apply(X, 2, sd, na.rm = TRUE) > 0)
  X <- X[, keep_cols, drop = FALSE]
  cf <- causal_forest(X = X, Y = Y, W = W, seed = 123)
  ate_est <- tryCatch(average_treatment_effect(cf), error = function(e) c(NA, NA))
  tibble(topic = j, n_treated = sum(W), ATE = ate_est[1], se = ate_est[2])
}

# correr para todos los topics (1..k)
res_list <- map_dfr(1:NUM_TOPICS, ~ run_causal_by_topic(.x, grf_df))

# también calcular ATE global para "any topic" vs none
grf_df2 <- grf_df %>% mutate(W_any = ifelse(topic > 0, 1, 0))
if (sum(grf_df2$W_any) >= 5) {
  X_all <- grf_df2 %>% select(-id, -CAR_0_1, -topic, -W_any) %>% as.matrix()
  cf_all <- causal_forest(X_all, grf_df2$CAR_0_1, grf_df2$W_any, seed = 123)
  ate_all <- average_treatment_effect(cf_all)
  res_any <- tibble(topic = 0, n_treated = sum(grf_df2$W_any), ATE = ate_all[1], se = ate_all[2])
  res_list <- bind_rows(res_any, res_list)
}

# ---------------------------
# 12. Resultados y diagnóstico
# ---------------------------
message("Resultados ATE por topic:")
print(res_list)

# exportar resultados
write_csv(res_list, "gdelt_aapl_grf_ate_by_topic.csv")

# Topicos: inspeccionar palabras por topic (beta)
terms_by_topic <- tidy(lda_model, matrix = "beta") %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  summarise(terms = paste(term, collapse = ", "), .groups = "drop")

write_csv(terms_by_topic, "lda_terms_by_topic.csv")
message("Top terms por topic guardados en 'lda_terms_by_topic.csv'")

# Guardar noticias procesadas con topic y CAR
write_csv(news_for_grf, "news_aapl_with_topics_car.csv")
message("Noticias procesadas guardadas en 'news_aapl_with_topics_car.csv'")

# FIN
message("PIPELINE COMPLETO FINALIZADO.")
