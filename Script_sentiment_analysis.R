##### analisis de sentimientos 


## instalación de paquetes 
install.packages("syuzhet") # emociones
install.packages("RColorBrewer") # paleta de colores
install.packages("wordcloud") # Nube de palabras
install.packages("tm") # text mining

library(syuzhet)
library(RColorBrewer)
library(wordcloud)
library(tm)

# fijar el path 
setwd("C:/Users/danie/OneDrive/Documents/GitHub/Orgullo-y-Prejuicio")

# Leer el archivo de texto de Orgullo y Prejuicio
texto_OYP <- read.table("OYP.txt", fileEncoding = "UTF-8", sep = "\n", allowEscapes = T)

# Obtener lista de stopwords en español
stopwords <- stopwords("spanish")

# Convertir texto a corpus
corpus_OYP <- Corpus(VectorSource(texto_OYP))

# Eliminar stopwords del corpus
corpus_OYP_sin_stopwords <- tm_map(corpus_OYP, removeWords, stopwords)

# Convertir corpus procesado a texto
texto_sin_stopwords <- sapply(corpus_OYP_sin_stopwords, as.character)

# tokenizar las palabras sin stopwords
texto_palabrasOYP <- get_tokens(texto_sin_stopwords)

# extraer los sentimientos asociados a cada token
sentimientosOYP_df <- get_nrc_sentiment(texto_palabrasOYP, lang="spanish")


# vector de los nombres de las columnas
colnames(sentimientosOYP_df) <- c('Rabia', 'Anticipacion', 'disgusto', 'miedo', "Alegria","Tristeza","sorpresa", 
                                  "confianza", "negativo", "positivo")

# barplot de los sentimientos
barplot(
  colSums(prop.table(sentimientosOYP_df[, 1:8])),
  space = 0.2,
  horiz = FALSE,
  las = 1,
  cex.names = 0.7,
  col = brewer.pal(n = 8, name = "Set3"),
  main = "Analisis de emociones en el libro: 'ORGULLO y PREJUICIO' de Jane Austen",
  xlab="Emociones", ylab = "Frecuencia relativa de la emoción")


## plot sobre el desarrollo de los sentimientos a traves del tiempo de la novela
sentimientos_valencia <- (sentimientosOYP_df$negativo *-1) + sentimientosOYP_df$positivo
simple_plot(sentimientos_valencia)

# Calcular puntuaciones de sentimientos
sentimientosOYP_df <- get_nrc_sentiment(texto_palabrasOYP, lang="spanish")


# vector de los nombres de las columnas
colnames(sentimientosOYP_df) <- c('Rabia', 'Anticipacion', 'disgusto', 'miedo', "Alegria","Tristeza","sorpresa", 
                                  "confianza", "negativo", "positivo")

head(sentimientosOYP_df)

palabras_tristes <- texto_palabrasOYP[sentimientosOYP_df$Tristeza> 0]


# Extraer palabras por emoción
nube_emociones_vector <- c(
  paste(texto_palabrasOYP[sentimientosOYP_df$Rabia > 0], collapse = " "),
  paste(texto_palabrasOYP[sentimientosOYP_df$Tristeza > 0], collapse = " "),
  paste(texto_palabrasOYP[sentimientosOYP_df$miedo > 0], collapse = " "),
  paste(texto_palabrasOYP[sentimientosOYP_df$Alegria > 0], collapse = " "))


# Convertir el vector de palabras a codificación UTF-8
nube_emociones_vector_2 <- iconv(nube_emociones_vector, "latin1", "UTF-8")

# Crear corpus y matriz de términos-documento
nube_corpus <- Corpus(VectorSource(nube_emociones_vector_2))
nube_tdm <- TermDocumentMatrix(nube_corpus)
nube_tdm <- as.matrix(nube_tdm)

# Asignar nombres de columnas
colnames(nube_tdm) <- c('Rabia', 'Tristeza', 'Miedo', "Alegría")

# Generar nube de palabras
comparison.cloud(nube_tdm, random.order = FALSE,
                 colors = c("green", "red", "orange", "blue"),
                 title.size = 1, max.words = 200, scale = c(2.5, 1), rot.per = 0.4)

###############
## Gracias! ##
##############