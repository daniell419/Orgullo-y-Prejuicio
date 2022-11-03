##### analisis de sentimientos 

install.packages("syuzhet")
install.packages("RColorBrewer")
install.packages("wordcloud")
install.packages("tm")

library(syuzhet)
library(RColorBrewer)
library(wordcloud)
library(tm)

# cambiar el path a la librería antes de empezar
texto_OYP <- read.table("D:/OYP/Orgullo-y-Prejuicio/OYP.txt", fileEncoding = "UTF-8", sep = "\n", allowEscapes = T)
texto_palabrasOYP <- get_tokens(texto_OYP)
sentimientosOYP_df <- get_nrc_sentiment(texto_palabrasOYP, lang="spanish")

colnames(sentimientosOYP_df) <- c('Rabia', 'Anticipaci?n', 'disgusto', 'miedo', "Alegr?a","Tristeza","sorpresa", "confianza", "negative", "positive")

barplot(
  colSums(prop.table(sentimientosOYP_df[, 1:8])),
  space = 0.2,
  horiz = FALSE,
  las = 1,
  cex.names = 0.7,
  col = brewer.pal(n = 8, name = "Set3"),
  main = "An?lisis de emociones en el libro: 'ORGULLO y PREJUICIO' de Jane Austen",
  xlab="Emociones", ylab = "Frecuencia relativa de aparici?n")

sentimientos_valencia <- (sentimientosOYP_df$negative *-1) + sentimientosOYP_df$positive
simple_plot(sentimientos_valencia)

sentimientos_OvsP <- (sentimientosOYP_df$disgusto*-1) + sentimientosOYP_df$confianza
simple_plot(sentimientos_OvsP)

palabras_tristeza <- texto_OYP[sentimientosOYP_df$Rabia > 0, 1]

palabras_tristeza_orden <- sort(table(unlist(palabras_tristeza)), decreasing = TRUE)
head(palabras_tristeza_orden, n = 12)
## extraer las palabras que generan cada una de las emociones, hay que a?adir al codigo el n?mero de la columna. 
nube_emociones_vector <- c(
  paste(texto_OYP[sentimientosOYP_df$confianza > 0, 8], collapse = " "),
  paste(texto_OYP[sentimientosOYP_df$Rabia > 0, 1], collapse = " "),
  paste(texto_OYP[sentimientosOYP_df$Tristeza > 0, 6], collapse = " "),
  paste(texto_OYP[sentimientosOYP_df$miedo > 0, 4], collapse = " "),
  paste(texto_OYP[sentimientosOYP_df$"Alegr?a" > 0, 5], collapse = " "))


nube_emociones_vector <- iconv(nube_emociones_vector, "latin1", "UTF-8")
nube_tdm <- gsub(words, pattern = '[[:punct:][:digit:]]', replacement = '') 
nube_corpus <- Corpus(VectorSource(nube_emociones_vector))
nube_tdm <- TermDocumentMatrix(nube_corpus)
nube_tdm <- as.matrix(nube_tdm)
colnames(nube_tdm) <- c('Confianza', 'Rab?a', 'Tristeza', 'Miedo', "Alegr?a")
comparison.cloud(nube_tdm, random.order = FALSE,
                 colors = c("green", "red", "orange", "blue"),
                 title.size = 1, max.words = 50, scale = c(2.5, 1), rot.per = 0.4)

## Cargar la base en inglés:
texto_OYPingl <- read.table("D:/OYP/Orgullo-y-Prejuicio/OYPingles.txt", fileEncoding = "UTF-8", sep = "\n", allowEscapes = T)
texto_palabrasOYPingl <- get_tokens(texto_OYP)
sentimientosOYPingl_df <- get_nrc_sentiment(texto_palabrasOYP, lang="english")

colnames(sentimientosOYPingl_df) <- c('Rabia', 'Anticipaci?n', 'disgusto', 'miedo', "Alegr?a","Tristeza","sorpresa", "confianza", "negative", "positive")

barplot(
  colSums(prop.table(sentimientosOYPingl_df[, 1:8])),
  space = 0.2,
  horiz = FALSE,
  las = 1,
  cex.names = 0.7,
  col = brewer.pal(n = 8, name = "Set3"),
  main = "An?lisis de emociones en el libro: 'ORGULLO y PREJUICIO' de Jane Austen",
  xlab="Emociones", ylab = "Frecuencia relativa de aparici?n")

## Gracias!
