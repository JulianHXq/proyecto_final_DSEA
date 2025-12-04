# ============================================================
#   Proyecto de mineria de texto - Stock news recommendation
# ============================================================

#USAR EL PROCESO DE WEB-SCRAPING -SOLO- SI NO SE CUENTA CON LAS
#BASES DE DATOS

# ============================================================
# 0. CARGA DE PAQUETES 
# ============================================================

require(pacman)
#p_load()
packages <- c(
  "tidyverse","lubridate","data.table","httr","jsonlite","stringi",
  "xml2","tidyquant","purrr","tidyRSS","text2vec","topicmodels",
  "tokenizers","stopwords","tidytext","SnowballC","grf","fixest",
  "tibble","cld3","rvest","wordcloud","Matrix","coop","tidytext","tidyr",
  "stringr","patchwork","tm","textstem","maptpx","ranger"
)
lapply(packages, require, character.only = TRUE)

# ================================================================
#  1. DESCARGA RSS EXTRA (Really Simple Syndication-Feeds de news)
#   <PROCESO DE WEB-SCRAPING>
# ================================================================
Ticker = "AAPL"
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
#  2. GDELT HISTÓRICO (BODY FIXED)
# ============================================================

#descargar el cuerpo (body) de una noticia desde un URL, de 
#manera robusta y sin que el script se caiga si algo falla.
safely_get_html_body <- function(url){
  tryCatch({
    if(is.na(url) || url == "") return(NA_character_)
    h <- httr::GET(
      url,
      httr::add_headers(
        "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"
      ),
      timeout(10)
    )
    html <- httr::content(h, as = "text", encoding = "UTF-8")
    doc <- read_html(html)
    p <- xml2::xml_find_all(doc, "//p")
    txt <- paste(xml_text(p), collapse = "\n")
    if (nchar(txt) < 100) NA_character_ else txt
  }, error = function(e) NA_character_)
}

#función que permite descargar la base de noticias
get_gdelt_historico <- function(keyword, years = 0.1, chunk_days = 30){
  
  message("📡 Descargando datos históricos desde GDELT Doc API…")
  
  end <- Sys.Date()
  start <- end - as.integer(years * 365)
  
  chunk_starts <- seq(start, end, by = paste0(chunk_days, " days"))
  chunk_ends   <- pmin(chunk_starts + (chunk_days - 1), end)
  
  ranges <- tibble(start = chunk_starts, end = chunk_ends)
  
  # ---- SCRAPER BODY ----
  scrapear_body <- function(url){
    tryCatch({
      if(is.na(url) || url == "") return(NA_character_)
      
      res <- GET(url, timeout(10))
      if(res$status_code != 200) return(NA_character_)
      
      html <- rawToChar(res$content)
      Encoding(html) <- "UTF-8"
      
      doc <- read_html(html)
      p <- doc %>% html_nodes("p") %>% html_text(trim=TRUE)
      body <- paste(p, collapse="\n")
      
      if(nchar(body) < 150) return(NA_character_)
      body
    }, error = function(e) NA_character_)
  }
  
  # ---- PARSE JSON DEL DOC API ----
  get_chunk <- function(i){
    
    s <- ranges$start[i]
    e <- ranges$end[i]
    
    message("→ Chunk ", i, ": ", s, " → ", e)
    
    params <- list(
      query = keyword,
      mode = "ArtList",
      startdatetime = paste0(format(s, "%Y%m%d"), "000000"),
      enddatetime   = paste0(format(e, "%Y%m%d"), "235959"),
      maxrecords = 250,
      format = "json"
    )
    
    res <- tryCatch(GET("https://api.gdeltproject.org/api/v2/doc/doc",
                        query = params, timeout(20)),
                    error = function(e) NULL)
    
    if(is.null(res) || res$status_code != 200){
      message("⚠ Error o sin datos.")
      return(tibble())
    }
    
    # === FIX UNIVERSAL PARA content() ===
    txt <- rawToChar(res$content)
    Encoding(txt) <- "UTF-8"
    
    # Arreglo JSON roto
    txt <- gsub("\\\\(?![nrt\"\\\\])", "", txt, perl=TRUE)
    txt <- stri_encode(txt, "", "UTF-8")
    
    json <- tryCatch(fromJSON(txt), error=function(e) NULL)
    if(is.null(json) || is.null(json$articles)){
      message("⚠ Sin artículos.")
      return(tibble())
    }
    
    df <- as_tibble(json$articles)
    
    get_col <- function(df, candidates){
      col <- candidates[candidates %in% names(df)]
      if(length(col)==0) return(NA_character_)
      df[[col[1]]]
    }
    
    tib <- tibble(
      url        = get_col(df, c("url","documentidentifier")),
      title      = get_col(df, c("title","title_original")),
      body_raw   = get_col(df, c("content","body","excerpt","summary")),
      datetime_utc = suppressWarnings(ymd_hms(get_col(df, c("seendate","date","pubdate")), quiet=TRUE)),
      provider     = get_col(df, c("source","publisher","domain"))
    )
    
    # Scraping cuerpo real
    tib$body_fixed <- map_chr(tib$url, scrapear_body)
    
    tib
  }
  
  # ---- Ejecutar todo ----
  result <- map_dfr(seq_len(nrow(ranges)), get_chunk)
  
  message("✔ Descarga completa: ", nrow(result), " artículos.")
  result
}

# ======= Aplicación del web-scaping =======
gdelt_apple <- get_gdelt_historico("apple", years = 0.01)
gdelt_apple %>% select(url, title, body_raw, body_fixed) %>% head()

#Verificación de las dimensiones de la matriz 
nrow(gdelt_apple)
head(gdelt_apple)

#Detencción del lenguaje para quedarnos solo con Ingles
gdelt_apple <- gdelt_apple %>%
  mutate(lang_title = cld3::detect_language(title)) %>%
  filter(lang_title == "en")
nrow(gdelt_apple)

gdelt_apple <- gdelt_apple %>%
  rename(id = url) 
gdelt_apple <- gdelt_apple %>%
  select(-body_raw)

# Mostrar los primeros cuerpos descargados
gdelt_apple %>% select(id, title, body_fixed) %>% head()

#Para guardar la base de datos dado que demora 8 horas haciendo web-scraping
write.csv(gdelt_apple, "C:/Users/User/Desktop/CienciaDatos_Econometria/P.Final/proyecto_final_DSEA/gdelt_apple.csv", row.names = FALSE)

# ============================================================
#  3. CARGAR EL HISTÓRICO (BODY FIXED) 
# ============================================================

#Cargar la base de datos
gdelt_apple <- read_csv("C:/Users/User/Desktop/CienciaDatos_Econometria/P.Final/proyecto_final_DSEA/gdelt_apple.csv")

gdelt_apple <- gdelt_apple %>%
  select(-body_raw) %>%      # elimina la columna body_raw
  filter(!is.na(body_fixed)) # mantiene solo filas donde body_fixed tiene texto

nrow(gdelt_apple)
head(gdelt_apple)

# ============================================================
#  4. LIMPIEZA DE LA BASE DE DATOS DE TERMINOS FRECUENTES
# ============================================================

gdelt_apple_clean <- gdelt_apple %>%
  mutate(
    # Asegurar que body_fixed sea character
    body_fixed = as.character(body_fixed),
    
    # Limpieza inicial de errores y copyrights
    body_fixed = body_fixed %>%
      sub("^Oops, something went wrong\\s*", "", .) %>%
      gsub("© [0-9]{4} .*?\\.\\s+ALL RIGHTS RESERVED\\.", "", .) %>%
      gsub("© 2025 Benzinga\\.com\\. Benzinga does not provide investment advice\\. All rights reserved\\.", "", ., fixed = TRUE) %>%
      str_trim()
  ) %>%
  mutate(
    # Detectar idioma de título y cuerpo
    lang_title = cld3::detect_language(title),
    lang_body  = cld3::detect_language(body_fixed)
  ) %>%
  # Mantener solo inglés y body_fixed no vacío
  filter(
    lang_title == "en",
    lang_body  == "en",
    !is.na(body_fixed),
    nchar(body_fixed) > 0
  ) %>%
  # Filtrar filas que sean publicitarias, educativas, de descuento o meramente numéricas
  filter(
    !grepl(
      "(?i)for educational purposes only|consult your financial advisor|follow him on twitter|get instant access.*?for only \\$|subscribe now|newsletter|IBD Digital|IBD Live|Investor's Business Daily|Leaderboard|MarketDiem|MarketSurge|#1|Futures Fall|Top stories|discount|deal|offer|sale",
      body_fixed
    )
  ) %>%
  # Limpiar números, símbolos y espacios
  mutate(
    body_fixed = body_fixed %>%
      str_remove_all("\\$[0-9]+(\\.[0-9]{1,2})?") %>%   # valores tipo $124.38
      str_remove_all("[0-9]+(\\.[0-9]+)?%") %>%         # porcentajes tipo 33%
      str_remove_all("[0-9]+-week") %>%                 # ejemplo 52-week
      str_remove_all("[0-9]+") %>%                      # números sueltos
      str_remove_all("[-#*•]") %>%                      # símbolos comunes
      str_squish() %>%
      str_trim()
  )

# Función para marcar filas tipo market commentary / tips de trading / newsletter
is_market_commentary <- function(text) {
  text <- as.character(text)
  
  # Patrón de frases típicas de este contenido
  patterns <- c(
    "(?i)we are long",                        # menciona posiciones propias
    "(?i)join our investing",                 # invita a desafíos o suscripciones
    "(?i)compete for rewards",                # promociones o gamificación
    "(?i)newsletter",                         # menciona newsletter
    "(?i)subscribe now",                       # call-to-action de suscripción
    "(?i)tips? for investors",                # consejos de inversión genéricos
    "(?i)low[- ]?risk buy",                   # recomendaciones de compra
    "(?i)analysis:",                           # encabezados tipo “analysis: ...”
    "(?i)shares are moving well",             # frases genéricas sobre precios
    "(?i)macd remains below",                 # indicadores técnicos
    "(?i)join our .*?challenge"               # promoción de participación
  )
  
  # Revisar si alguno coincide
  match <- sapply(patterns, function(p) str_detect(text, p))
  
  # Si coincide alguno, devolver NA
  if(any(match, na.rm = TRUE)) {
    return(NA_character_)
  } else {
    return(text)
  }
}

# Ejemplo de uso con tu dataframe
gdelt_apple_clean <- gdelt_apple_clean %>%
  mutate(
    body_fixed = sapply(body_fixed, is_market_commentary)
  ) %>%
  # Eliminar filas que quedaron NA
  filter(!is.na(body_fixed))

# =========================================================
# 4. LIMPIEZA DE LOS SINONIMOS DE LA BASE DE DATOS 
# =========================================================

financial_synonyms <- list(
  stock = c("stock", "share", "equity", "securities", "equities"),
  profit = c("profit", "earnings", "income", "revenue", "gains", "proceeds"),
  loss = c("loss", "losses", "deficit", "shortfall", "decline"),
  growth = c("growth", "expansion", "increase", "rise", "surge", "uptick", "rally"),
  fall = c("fall", "drop", "decrease", "plunge", "tumble", "slump", "downturn"),
  market = c("market", "marketplace", "exchange", "trading", "bourse"),
  investment = c("investment", "investing", "portfolio", "holdings", "stake"),
  debt = c("debt", "liability", "obligation", "borrowing", "liabilities"),
  merger = c("merger", "acquisition", "takeover", "buyout", "consolidation", "m&a"),
  valuation = c("valuation", "value", "worth", "appraisal", "assessment"),
  ceo = c("ceo", "chief executive", "executive", "president", "director"),
  bank = c("bank", "banking", "financial institution", "lender", "creditor"),
  interest_rate = c("interest rate", "rate", "rates", "yield", "return"),
  inflation = c("inflation", "price increase", "rising prices", "cpi", "consumer prices"),
  recession = c("recession", "slump", "contraction", "economic decline"),
  dividend = c("dividend", "payout", "distribution", "return"),
  ipo = c("ipo", "initial public offering", "public offering", "flotation", "listing"),
  sales = c("sales", "revenue", "turnover", "proceeds", "receipts"),
  volatility = c("volatility", "fluctuation", "instability", "uncertainty", "turbulence"),
  regulation = c("regulation", "regulatory", "compliance", "oversight", "supervision")
)

# =========================================================
# 5. FUNCIÓN PARA NORMALIZAR TEXTO
# =========================================================

normalize_financial_terms <- function(text_clean, synonym_dict = financial_synonyms) {
  text_lower <- tolower(text_clean)
  
  for (standard_term in names(synonym_dict)) {
    synonyms <- synonym_dict[[standard_term]]
    
    for (synonym in synonyms) {
      pattern <- paste0("\\b", synonym, "\\b")
      text_lower <- gsub(pattern, standard_term, text_lower, perl = TRUE)
    }
  }
  
  return(text_lower)
}

# =========================================================
# 6. APLICAR NORMALIZACIÓN SOLO AL BODY
# =========================================================

gdelt_apple_clean <- gdelt_apple_clean %>%
  mutate(
    body_fixed = normalize_financial_terms(body_fixed)
  )

gdelt_apple_clean <- gdelt_apple_clean %>%
  filter(!str_detect(body_fixed, "Benzinga"))

# Revisar resultados
nrow(gdelt_apple_clean)

# Asegurarse de que body_fixed sea de tipo character
gdelt_apple_clean$body_fixed <- as.character(gdelt_apple_clean$body_fixed)

# Lista de términos relacionados con Apple
terms_to_include <- c("Apple", "AAPL", "iPhone", "iPad", "Mac", "Apple Inc","aapl",
                      "apple","iphone","ipad")  # Palabras clave
pattern <- paste(terms_to_include, collapse = "|")  # "Apple|AAPL|iPhone|..."

# Filtrar filas que contengan al menos una palabra clave
gdelt_apple_only <- subset(gdelt_apple_clean, grepl(pattern, body_fixed, ignore.case = TRUE))
#gdelt_apple_excluded <- subset(gdelt_apple_clean, !grepl(pattern, body_fixed, ignore.case = TRUE))

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
gdelt_apple_clean_text <- gdelt_apple_clean_text %>%
  left_join(news_stemmed, by = "url") %>%
  mutate(text_final = trimws(text_final)) %>%
  filter(!is.na(text_final), text_final != "")

#------------------------------------------
#-------------Matriz TF-IDF----------------
#------------------------------------------
# Crear tabla de conteo de términos por documento
term_counts <- tokens %>%
  group_by(url, word) %>%
  summarise(term_count = n(), .groups = "drop")

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
  filter(word %in% keep_words) %>%      
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
news_dates_unique <- gdelt_apple_clean %>%
  select(url, datetime_utc,text_final) %>%           # tomar columnas necesarias
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
dim(gdelt_apple_clean)
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

combined_plot <- line_plot + line_plot2
combined_plot
# Guardar la imagen
ggsave("C:/Users/User/Desktop/CienciaDatos_Econometria/P.Final/proyecto_final_DSEA/outcomes/comparacion_proveedores.png", combined_plot, width = 12, height = 6, dpi = 300)

#======================================================
#--- 12. Análisis de sentimiento de las noticias------
#======================================================

#a) Tokenizar y limpiar---------------------------------------------------------

# Tokenizar y quitar stopwords
tokens_sent <- gdelt_apple_dedup %>%
  unnest_tokens(word, text_final) %>%
  filter(!word %in% sw) %>%
  count(url, word)



#b) Unir lexico de sentimiento-------------------------------------------------
sentiments <- get_sentiments("bing")
tokens_sent <- tokens_sent %>% inner_join(sentiments, by = "word")

#c) Calcular score por documento------------------------------------------------
sent_doc <- tokens_sent %>%
  group_by(url) %>%
  summarise(sent_score = sum(ifelse(sentiment=="positive",1,-1)), .groups="drop") %>%
  left_join(gdelt_apple_dedup %>% select(url, datetime_utc), by = "url")
colnames(sent_doc)

#======================================================
#--- 13. Gráfica de sentimiento de las noticias------
#======================================================

#a) Promedio diario------------------------------------------------------------

sent_diario <- sent_doc %>%
  group_by(date) %>%
  summarise(sent_score = mean(sent_score, na.rm = TRUE)) %>%
  mutate(color = ifelse(sent_score >= 0, "positivo", "negativo"))

#b) Promedio cada 2 semanas-----------------------------------------------------

sent_2sem <- sent_diario %>%
  mutate(
    year = year(date),
    week = isoweek(date),
    periodo2 = paste0(year, "-Q", (week - 1) %/% 2)   # agrupa cada 2 semanas
  ) %>%
  group_by(periodo2) %>%
  summarise(
    start_date = min(date),
    end_date   = max(date),
    sent_score_2sem = mean(sent_score, na.rm = TRUE)
  )

#c) Creación del gráfico--------------------------------------------------------

ggplot() +
  geom_rect(data = sent_2sem,aes(xmin = start_date,xmax = end_date,ymin = 0,
      ymax = sent_score_2sem,fill = sent_score_2sem >= 0),alpha = 0.25) +
  geom_col(data = sent_diario,aes(x = date, y = sent_score, fill = sent_score >= 0)
  ) + 
  geom_line(data = sent_2sem,aes( x = start_date + (end_date - start_date)/2,
      y = sent_score_2sem),size = 1.2,color = "black") +
  scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red"),
    labels = c("FALSE" = "Negativo", "TRUE" = "Positivo")
  ) +
  labs(
    title = "Sentimiento Diario y Promedio Cada 2 Semanas",
    x = "Fecha",y = "Sentiment Score",fill = "Sentimiento") + theme_minimal()

#======================================================
#-----------14. Análisis precio sentimiento------------
#======================================================

# a) Descargar precios de AAPL-------------
TICKER <- "AAPL"
START <- as.Date("2020-12-01")
END   <- as.Date("2025-11-30")

# Precios AAPL
prices_xts <- getSymbols(TICKER, src="yahoo", from=START, to=END, auto.assign=FALSE)
prices_df <- data.frame(
  date = index(prices_xts),
  adjusted = as.numeric(prices_xts$AAPL.Adjusted)
) %>%
  arrange(date) %>%
  mutate(
    logret = log(adjusted) - log(lag(adjusted))
  )

# b) Unir el sentimiento de la noticia con los precios
sent_price_daily <- sent_diario %>%
  inner_join(prices_df, by = "date") %>%
  filter(!is.na(logret))

# c) Evaluación de la correlación
value_corr <- cor(sent_price_daily$sent_score, sent_price_daily$logret)
cat("El", round(value_corr*100,2), "% de las noticias logró mover el precio")
#======================================================
#-15. Limpieza del efecto dado el movimiento del indice-
#======================================================

# a) Descargar precios de S&P500-----------
SP500 <- getSymbols("^GSPC", src="yahoo", from=START, to=END, auto.assign=FALSE)
SP500_df <- data.frame(
  date = index(SP500),
  adj_SP = as.numeric(SP500$GSPC.Adjusted)) %>%  arrange(date) %>%
  mutate(logret_SP = log(adj_SP) - log(lag(adj_SP)))

# b.) Merge AAPL + S&P500
prices_merged <- prices_df %>%
  inner_join(SP500_df %>% select(date, logret_SP), by = "date") %>%
  filter(!is.na(logret), !is.na(logret_SP))


# c.) Calcular beta histórico (CAPM)
beta_model <- lm(logret ~ logret_SP, data = prices_merged)
alpha <- coef(beta_model)[1]
beta  <- coef(beta_model)[2]

cat("Beta AAPL =", round(beta, 3), "\n")
cat("Alpha =", round(alpha, 6), "\n")

# d.) Retorno esperado + retorno anormal
threshold <- 0.03  # 1%

#  Ajustar precios AAPL por CAPM y crear targets con threshold
prices_adj <- prices_merged %>%
  mutate(
    ret_expected  = alpha + beta * logret_SP,        # retorno esperado según CAPM
    abnormal_ret  = logret - ret_expected,           # retorno anormal
    target_up     = factor(as.integer(abnormal_ret > threshold)),   # arriba si > threshold
    target_down   = factor(as.integer(abnormal_ret < -threshold))  # abajo si < -threshold
  )
# e.) Preparar matriz TF

tokens_tf_filtered_valid <- tokens_sent %>%
  group_by(word, url) %>%
  summarise(n = sum(n), .groups="drop") %>%
  filter(n >= 5) %>%
  pivot_wider(
    names_from = word,
    values_from = n,
    values_fill = 0,
    names_repair = "unique"
  ) %>%
  left_join(
    sent_doc %>% select(url, datetime_utc),
    by = "url"
  ) %>%
  mutate(date = as.Date(datetime_utc)) %>%
  inner_join(
    prices_adj %>% select(date, target_up, target_down),
    by = "date"
  )

# Matriz X
# Matriz X correcta, sin columnas que no sean palabras
X <- tokens_tf_filtered_valid %>%   select(-url, -date, -datetime_utc, -target_up, -target_down)
y_up <- tokens_tf_filtered_valid$target_up
y_down <- tokens_tf_filtered_valid$target_down

rf_up <- ranger(
  dependent.variable.name = "target_up",
  data = cbind(target_up = y_up, X),
  num.trees = 500,
  importance = "impurity",
  probability = TRUE
)

# Modelo clasificación DOWN
rf_down <- ranger(
  formula = target_down ~ .,
  data    = data.frame(target_down = tokens_tf_filtered_valid$target_down, X),
  num.trees = 500,
  importance = "impurity",
  probability = TRUE
)

# Importancia de variables
imp_up <- importance(rf_up)
imp_down <- importance(rf_down)

plot_top_words <- function(imp, top_n = 50) {
  df <- data.frame(word = names(imp), importance = as.numeric(imp)) %>%
    arrange(desc(importance)) %>%
    slice(1:top_n)
  
  wordcloud(
    words = df$word,
    freq  = df$importance,
    max.words = top_n,
    scale = c(7,1.7),
    colors = brewer.pal(8, "Dark2"),
    random.order = FALSE
  )
}

plot_top_words(imp_up)
plot_top_words(imp_down)

# Categorizar movimientos
prices_adj <- prices_adj %>%
  mutate(
    move_category = case_when(
      logret > 0.04  ~ "subida fuerte",
      logret > 0     ~ "subida leve",
      logret < -0.04 ~ "caida fuerte",
      logret < 0     ~ "caida leve",
      TRUE               ~ "sin cambio"
    )
  )

# Top fechas
top_up_dates   <- prices_adj %>% slice_max(logret, n = 30) %>% pull(date)
top_down_dates <- prices_adj %>% slice_min(logret, n = 30) %>% pull(date)

tokens_tf_filtered_valid <- tokens_tf_filtered_valid %>%
  rename(date = datetime_utc)


#  Un solo título por URL
gdelt_unique <- gdelt_apple_dedup %>%
  group_by(url) %>%
  slice(1) %>%   # tomar la primera fila por URL
  ungroup() %>%
  select(url, title)

# Filtrar tokens para fechas top y URLs únicas
news_top <- tokens_tf_filtered_valid %>%
  filter(date %in% c(top_up_dates, top_down_dates)) %>%
  distinct(url, date) %>%   # una fila por URL y fecha
  left_join(gdelt_unique, by = "url") %>%
  select(date, url, title)



# Separar si quieres
news_top_up   <- news_top %>% filter(date %in% top_up_dates)
news_top_down <- news_top %>% filter(date %in% top_down_dates)

# Top 10 noticias “up” con fecha y título
news_top_up[1:10, c("date", "title")]
# Top 10 noticias “down” con fecha y título
news_top_down[1:10, c("date", "title")]

#------------------------------------Funcion predictiva-------------------------------------- 
#--------------------------------------------------------------------------------------------
predict_news_movement_weighted <- function(news_text, imp_up, imp_down, max_pct = 5) {
  library(stringr)
  
  news_tokens <- str_split(tolower(news_text), "\\W+")[[1]]
  news_tokens <- news_tokens[news_tokens != ""]
  
  common_up   <- intersect(news_tokens, names(imp_up))
  common_down <- intersect(news_tokens, names(imp_down))
  
  score_up   <- sum(imp_up[common_up])
  score_down <- sum(imp_down[common_down])
  
  # Normalizar los scores
  total_score <- score_up + score_down
  if(total_score > 0) {
    expected_pct <- max_pct * (score_up - score_down) / total_score
  } else {
    expected_pct <- 0
  }
  
  # Determinar dirección
  direction <- if(expected_pct > 0) "UP" else if(expected_pct < 0) "DOWN" else "NEUTRAL"
  
  return(list(
    direction = direction,
    expected_pct = expected_pct,
    score_up = score_up,
    score_down = score_down
  ))
}


news_example_down <- paste(
  "Despite strong products, the company faces concern over debt and the risk of layoffs.",
  "Investors worry about a possible fall in stock prices due to flawed strategies and loss of market share.",
  "Analysts recommend caution, as leadership may not lead the company effectively.",
  "There is a risk of a bust in some segments, and free cash flow is under pressure.",
  "Top competitors outperform, and the overall momentum appears weak.",
  "Like many tech firms, uncertainty remains, and some smart moves may be needed to avoid further losses."
)


result_down <- predict_news_movement_weighted(news_example_down, imp_up, imp_down)
print(result_down)


news_example_up <- paste(
  "Apple reports stellar quarterly earnings, beating analyst expectations.",
  "Investors are bullish about the company’s strong products and robust strategy.",
  "The company is expected to outperform competitors and gain market share.",
  "Analysts recommend buying the stock, citing smart moves and top management leadership.",
  "Overall, the outlook looks super, with potential for significant growth."
)

result_up <- predict_news_movement_weighted(news_example_up, imp_up, imp_down)
print(result_up)



