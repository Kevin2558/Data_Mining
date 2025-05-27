Needed <- c("tm", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust",
            "cluster", "igraph", "fpc")
install.packages(Needed, dependencies = TRUE)

library("tm")

# Con lo siguiente buscamos la carpeta donde se ubican los archivos
# tenemos que seleccionar si o si un archivo y cuando nos entregue la
# ubicacion le borramos la parte donde nombra el archivo

a=file.choose()
a

cname<-"C:\\Users\\kevin\\OneDrive\\Escritorio\\R\\data mining\\Discursos Bach y Piñ"
dir(cname) # dir sirve para ver que archivos tenemos dentro de la carpeta

# Procedemos a crear el corpus de documentos.

docs<-VCorpus(DirSource(directory = cname), # Apunta a la carpeta
              readerControl = list(reader=readPDF,language='lat')) # Lee pdf

inspect(docs[[1]])  # Mostrar el contenido del primer documento
names(docs)         # Nombres de los documentos cargados

# Metadatos de los documentos

meta(docs[[1]])
meta(docs[[2]])

# Creamos una matriz con los nombres de los documentos (innecesario)

docs_names<-matrix(as.vector(unlist(strsplit(names(docs), "[[]"))),byrow=T,ncol=2)
docs_names[1,]
writeLines(as.character(docs[[1]])) # Misma funcion que inspect

# Preprocesamiento de los documentos

docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeWords, stopwords("spanish"))
docs <- tm_map(docs, stripWhitespace)

# Corección específica de algunos errores luego del preprocesamiento

for (j in seq(docs)){
  docs[[j]] <-gsub("“apunt","apunt", docs[[j]])
  docs[[j]] <-gsub("alto”","alto", docs[[j]])
  docs[[j]] <-gsub("tiempo”","tiempo",docs[[j]])
}

# La siguiente transformacion trata de transformar las palabras a su lexema
# (unidad minima con significado lexico). Por ejemplo, pan es el lexema de las
# palabras panaderia, panecillo, empanar, etc.

docs <- tm_map(docs, stemDocument, language = "spanish")

# Esto sirve para que luego de hacer las transformaciones el texto vuelva
# a su estructura original
docs <- tm_map(docs, PlainTextDocument)
docs

# Construccion de la matriz a través de los textos luego de la limpieza

dtms <- DocumentTermMatrix(docs)
dim(dtms)
m <- as.matrix(dtms) # Matriz numérica

v <- sort(colSums(m),decreasing=TRUE) # Suma de las frecuencias
head(v) # Primeras 6 filas
class(v) # Tipo de datos dentro del vector v

d <- data.frame(word = names(v), freq = v) 
d

# No recuerdo cual era el fin de los discursos pero haré una nube de palabras
# y una nube de comparación :P.

# Nube de palabras

wordcloud(words = d$word,
          freq = d$freq,
          scale = c(2,.2),
          min.freq = 2,          # Ajusta según tu corpus
          max.words = 200,       # Máximo número de palabras
          random.order = FALSE,  # Las más frecuentes al centro
          rot.per = 0.35,        # Proporción rotadas
          colors = brewer.pal(9, "BrBG"))

# Nube de comparacion

tdm <- TermDocumentMatrix(docs)
tdm<-as.matrix(tdm)
colnames(tdm)<-c("Bachelet","Piñera")

comparison.cloud(tdm, # Ahora se entregan los datos como matriz
                 max.words=Inf, # Incluir todas las palabras
                 random.order=FALSE, # Las palabras mas frecuentes al centro
                 color=brewer.pal(8,"Dark2"), # Paleta de colores
                 scale=c(2,.1), # Tamaño maximo y minimo de las palabras
                 title.size=1) # Tamaño del titulo

# Ahora hagamos una nube de palabras de las plabras comunes entre los dos textos

palabras_doc1 <- rownames(tdm)[tdm[,1] > 0]
palabras_doc2 <- rownames(tdm)[tdm[,2] > 0]

palabras_comunes <- intersect(palabras_doc1, palabras_doc2)

tdm_comun <- tdm[rownames(tdm) %in% palabras_comunes, ] # Filtrar

frecuencias_comunes <- rowSums(tdm_comun)
frecuencias_ordenadas <- sort(frecuencias_comunes, decreasing = TRUE)

# Convertir en data frame
d <- data.frame(word = names(frecuencias_ordenadas),
                freq = frecuencias_ordenadas)

wordcloud(words = d$word,
          freq = d$freq,
          min.freq = 1,
          max.words = 200,
          random.order = FALSE,
          rot.per = 0,  # 0 = todo horizontal
          colors = brewer.pal(4, "Dark2"),
          scale = c(4, 0.5))



