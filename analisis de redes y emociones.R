install.packages("sna")
install.packages("tsna")
install.packages("ndtv")

library(sna)
library(tsna)
library(ndtv)
y <- load(x)

githubURL <- "https://github.com/jessesadler/intro-to-r/blob/master/data/correspondence-data-1585.csv"
load(url(githubURL))


edges <- read.csv(choose.files())
nodes <- read.csv(file.choose(), stringsAsFactors = FALSE)

la_red <- network(
  nodes,
  vertex.attr = edges,
  vertex.attrnames = c("id.vertice", "nombre", "region"), directed = FALSE,
  bipartite = FALSE,
  multiple = TRUE
)
plot(la_red)
network <- network(edges, vertex.attr = nodes, matrix.type = "edgelist", ignore.eval = FALSE, multiple = TRUE)
detach(package:network)

library(igraph)

routes_igraph <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
plot(routes_igraph, edge.arrow.size=0.2)
library(networkD3)

##paquete NetworkD3
### tiene que empezar desde 0 el id 
nodes_d3 <- mutate(nodes, id.vertice = id.vertice - 1)
edges_d3 <- mutate(edges, emisor = emisor - 1, receptor = receptor - 1)


sankeyNetwork(Links = edges_d3, Nodes = nodes_d3, Source = "emisor", Target = "receptor", 
              NodeID = "nombre", Value="weight", fontSize = 16, units = "nombre")

##### analisis de sentimientos 

install.packages("syuzhet")
install.packages("RColorBrewer")
install.packages("wordcloud")
install.packages("tm")

library(syuzhet)
library(RColorBrewer)
library(wordcloud)
library(tm)


texto_cadena <- scan(file = "https://raw.githubusercontent.com/programminghistorian/jekyll/gh-pages/assets/galdos_miau.txt", fileEncoding = "UTF-8", what = character(), sep = "\n", allowEscapes = T)
texto_palabras <- get_tokens(texto_cadena)
sentimientos_df <- get_nrc_sentiment(texto_palabras, lang="spanish")
barplot(
  colSums(prop.table(sentimientos_df[, 1:8])),
  space = 0.2,
  horiz = FALSE,
  las = 1,
  cex.names = 0.7,
  col = brewer.pal(n = 8, name = "Set3"),
  main = "'Miau' de Benito Pérez Galdós, edición de 1907",
  sub = "Análisis realizado por Jennifer Isasi, PhD",
  xlab="emociones", ylab = NULL)

sentimientos_valencia <- (sentimientos_df$negative *-1) + sentimientos_df$positive
simple_plot(sentimientos_valencia)

barplot(colSums(prop.table(sentimientosOYP_df[, 1:8])))

texto_OYP1 <- scan(url("https://archive.org/stream/orgullo-y-prejuicio/orgullo-y-prejuicio_djvu.txt"), fileEncoding = "UTF-8", sep = "\n", allowEscapes = T)

texto_OYP <- read.table(choose.files(), fileEncoding = "UTF-8", sep = "\n", allowEscapes = T)
texto_palabrasOYP <- get_tokens(texto_OYP)
sentimientosOYP_df <- get_nrc_sentiment(texto_palabrasOYP, lang="spanish")

colnames(sentimientosOYP_df) <- c('Rabia', 'Anticipación', 'disgusto', 'miedo', "Alegría","Tristeza","sorpresa", "confianza", "negative", "positive")

barplot(
  colSums(prop.table(sentimientosOYP_df[, 1:8])),
  space = 0.2,
  horiz = FALSE,
  las = 1,
  cex.names = 0.7,
  col = brewer.pal(n = 8, name = "Set3"),
  main = "Análisis de emociones en el libro: 'ORGULLO y PREJUICIO' de Jane Austen",
  xlab="Emociones", ylab = "Frecuencia relativa de aparición")

sentimientos_valencia <- (sentimientosOYP_df$negative *-1) + sentimientosOYP_df$positive
simple_plot(sentimientos_valencia)

sentimientos_OvsP <- (sentimientosOYP_df$disgusto*-1) + sentimientosOYP_df$confianza
simple_plot(sentimientos_OvsP)

palabras_tristeza <- texto_OYP[sentimientosOYP_df$Rabia > 0, 1]

palabras_tristeza_orden <- sort(table(unlist(palabras_tristeza)), decreasing = TRUE)
head(palabras_tristeza_orden, n = 12)
## extraer las palabras que generan cada una de las emociones, hay que añadir al codigo el número de la columna. 
nube_emociones_vector <- c(
  paste(texto_OYP[sentimientosOYP_df$confianza > 0, 8], collapse = " "),
  paste(texto_OYP[sentimientosOYP_df$Rabia > 0, 1], collapse = " "),
  paste(texto_OYP[sentimientosOYP_df$Tristeza > 0, 6], collapse = " "),
  paste(texto_OYP[sentimientosOYP_df$miedo > 0, 4], collapse = " "),
  paste(texto_OYP[sentimientosOYP_df$"Alegría" > 0, 5], collapse = " "))


nube_emociones_vector <- iconv(nube_emociones_vector, "latin1", "UTF-8")
nube_tdm <- gsub(words, pattern = '[[:punct:][:digit:]]', replacement = '') 
nube_corpus <- Corpus(VectorSource(nube_emociones_vector))
nube_tdm <- TermDocumentMatrix(nube_corpus)
nube_tdm <- as.matrix(nube_tdm)
colnames(nube_tdm) <- c('Confianza', 'Rabía', 'Tristeza', 'Miedo', "Alegría")
comparison.cloud(nube_tdm, random.order = FALSE,
                 colors = c("green", "red", "orange", "blue"),
                 title.size = 1, max.words = 50, scale = c(2.5, 1), rot.per = 0.4)

## en ingles 
texto_OYPingl <- read.table(choose.files(), fileEncoding = "UTF-8", sep = "\n", allowEscapes = T)
texto_palabrasOYPingl <- get_tokens(texto_OYP)
sentimientosOYPingl_df <- get_nrc_sentiment(texto_palabrasOYP, lang="english")
