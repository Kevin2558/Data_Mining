install.packages("wordcloud",dependencies=TRUE)
library(tm)
library(wordcloud)
library(RColorBrewer)

# Creamos un vector de texto.

input<-c("¡Hola mundo! Este es un vector de caracteres , hola !")

# Creamos la fuente. VectorSource() es una función del paquete tm que transforma
# el vector de texto en una estructura que puede ser usada para crear un corpus.

vecSource<-VectorSource(input)

# Creamos el corpus; un corpus es una colección de textos que puedes limpiar,
# transformar y analizar. Esto lo realizamos a través de VCorpus.

corpus<-VCorpus(vecSource)

# El vector puede contener varias cadenas de texto.

texto<-c("Hola mundo! mundo, mundo","Hola! segundo vector de caracteres en R .")
corpus<-VCorpus(VectorSource(texto))
inspect(corpus) # inspect nos dará la información del corpus

# Lo siguiente nos mostrará las diferentes cadenas de texto presentes.

corpus[[1]]$content
corpus[[2]]$content

# Lo siguiente entrega los metadatos del vector o documento, tales como el tipo
# de documento, idioma, fechas, autores, etc.

meta(corpus[[1]])
meta(corpus[[2]])

# Proceso de limpieza del texto, en el pdf salen los distintos tipos de limpieza
# que ejecuta la funcion. En este caso:

# content_transformer(tolower) : convierte el texto a minusculas
# removePunctuation : elimina los signos de puntuación
# removeNumbers : elimina los numeros
# removeWords, stopwords("spanish") : elimina las stopwords (en español), tales
# como el, de, y, es, etc. Tambien se pueden eliminar palabras especificas.
# stripWhitespace : elimina los espacios innecesarios

limpieza<-function(corpus){
  corpus<-tm_map(corpus,content_transformer(tolower))
  corpus<-tm_map(corpus,removePunctuation)
  corpus<-tm_map(corpus,removeNumbers)
  corpus<-tm_map(corpus,removeWords, stopwords("spanish"))
  corpus<-tm_map(corpus,stripWhitespace)
  return(corpus)
}

# Con estas transformaciones el texto queda preparado para el análisis. Veamos,
# el resultado de la limpieza.

corpus_clean <- limpieza(corpus)

corpus_clean[[1]]$content
corpus_clean[[2]]$content

# Lo siguiente será construir una matriz a partir del corpus limpio, donde cada
# fila representa una palabra única del corpus, cada columna representa cada
# documento y cada celda de la matriz la frecuencia con la que cada palabra
# aparece en el respectivo documento.

tdm <- TermDocumentMatrix(corpus_clean)
inspect(tdm)

# Ahora, la transformaremos a una matriz numérica para realizar cálculos.

tdm<-as.matrix(tdm)
tdm

# Lo siguiente será crear un vector de frecuencias totales de las palabras en
# los dos textos. sort(,decreasing=TRUE) los ordena de mayor a menor, 
# rowSums() suma las filas.

vector.terminos<-sort(rowSums(tdm),decreasing=TRUE)
vector.terminos # Frecuencia de los terminos

# Ahora procedemos a crear un dataframe para poder manipular los datos.
# names() extrae los nombres del vector y en freq le entregamos directamente el
# vector con las frequencias

dataframe.terminos<-data.frame(word=names(vector.terminos),freq=vector.terminos)

# Por último, procedemos a crear una nube de palabras. 

wordcloud(dataframe.terminos$word, # Palabras que se mostrarán
          dataframe.terminos$freq, # Frecuencias asociadas
          scale=c(2,.2), # Tamaño de la palabra mas grande y de la mas pequeña
          min.freq=1, # Solo incluir palabras que aparecen 1 vez
          max.words=Inf, # No limita el numero de palabras
          random.order=FALSE, # Las palabras mas frecuentes aparecen en el centro
          rot.per=.15, # Proporcion de palabras que se rotan
          colors=brewer.pal(8,"Dark2")) # Paleta de colores

# En el codigo sale scale(8,.2) lo cual era demasiado grande, por lo
# que se cambio para que se pudieran ver todas las palabras

# Ahora veremos la comparacion de las frecuencias entre dos textos haciendo una
# nube de ideas donde cada texto tendra un color. Procedemos de la misma forma.

documentos<-c("Este es mi primer primer primer documento mundo",
              "Hola ! segundo segundo segundo documento mundo , adios .")
corpus<-VCorpus(VectorSource(documentos))
corpus_clean <- limpieza(corpus)

tdm<-TermDocumentMatrix(corpus_clean)
inspect(tdm)

tdm<-as.matrix(tdm)
colnames(tdm)<-c("Documento1","documento2")

# Procedemos a hacer la nube de comparacion.

comparison.cloud(tdm, # Ahora se entregan los datos como matriz
                 max.words=Inf, # Incluir todas las palabras
                 random.order=FALSE, # Las palabras mas frecuentes al centro
                 color=brewer.pal(8,"Dark2"), # Paleta de colores
                 scale=c(4,1), # Tamaño maximo y minimo de las palabras
                 title.size=1,5) # Tamaño del titulo
