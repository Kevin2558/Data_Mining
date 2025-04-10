# METODO DE K-MEDIAS

Felinos<-data.frame(tipopiel=c(1,3,2,2,2,2,1,4,2,2,2,1,2,1,1,1,2,1,3,1,2,2,1,1,2,1,4,2,1,2),
                    longpoill=c(0,0,0,0,1,0,0,0,0,0,1,0,0,0,1,0,0,1,0,0,0,0,1,0,0,1,0,0,0,1),
                    retract=c(1,1,1,1,1,0,rep(1,24)),
                    comport=c(1,3,2,3,1,1,2,3,1,2,2,2,2,2,3,3,3,2,3,2,3,2,3,3,2,3,3,3,3,3),
                    orielles=c(1,1,1,1,1,1,1,1,2,1,2,2,1,1,2,1,1,1,1,2,1,1,1,1,1,1,1,1,1,1),
                    larynx=c(1,1,1,1,1,0,0,1,rep(0,22)),
                    tailler=c(3,3,3,3,2,3,2,2,2,2,2,2,rep(1,18)),
                    poids=c(3,3,3,3,2,2,3,2,2,2,2,2,1,2,2,rep(1,15)),
                    longueurs=c(3,3,rep(2,9),1,2,2,rep(1,15),2),
                    queue=c(2,2,1,2,3,3,3,3,1,2,1,1,2,3,2,2,2,2,2,1,2,2,1,2,1,1,3,2,2,2),
                    dents=c(rep(1,5),0,1,1,0,0,1,rep(0,19)),
                    typproie=c(1,1,1,2,2,2,2,3,3,3,2,rep(3,18),2),
                    arbre=c(0,0,1,1,1,0,rep(1,6),0,1,1,1,1,0,rep(1,12)),
                    chasse=c(1,0,0,0,0,1,0,0,1,0,0,1,rep(0,6),1,rep(0,5),1,rep(0,5))
)
rownames(Felinos)<-c("leon","tigre","jaguar","leopardo","once","guepardo","puma","nebul","serval","ocelot","lynx","caracal","viverrin","yaguarun","chaus","dore","merguay","margerit","cafer","chine","bengale","rouilleu","malaid","borneo","nigripes","manul","marbre","tigrin","temminck","andes")
head(Felinos)
group<-kmeans(Felinos,centers=4,nstart=5) # nosotros dijimos que queremos 4 grupos
print(group)
group1<-kmeans(Felinos,centers=5,nstart=5)
print(group)
group1$cluster # muestra solamente los grupos
group2<-kmeans(Felinos,centers=5,nstart=5)
print(group)
group2$cluster # lo volvemos a realizar porque no siempre entrega los mismos resultados
group3<-kmeans(Felinos[1:10,],centers=5,nstart=5) # aqui solamente lo hace para los primero 10
group3$cluster
# k-medias no es para datos categoricos, está hecho para datos numéricos, para
# categoricos está hecho k-modas que es un metodo mejorado.

################################################################################
################################################################################

# Metodo de agrupamiento Condorcet

install.packages("amap")
library("amap")
Felinos<-data.frame(tipopiel=c(1,3,2,2,2,2,1,4,2,2,2,1,2,1,1,1,2,1,3,1,2,2,1,1,2,1,4,2,1,2),
                    longpoill=c(0,0,0,0,1,0,0,0,0,0,1,0,0,0,1,0,0,1,0,0,0,0,1,0,0,1,0,0,0,1),
                    retract=c(1,1,1,1,1,0,rep(1,24)),
                    comport=c(1,3,2,3,1,1,2,3,1,2,2,2,2,2,3,3,3,2,3,2,3,2,3,3,2,3,3,3,3,3),
                    orielles=c(1,1,1,1,1,1,1,1,2,1,2,2,1,1,2,1,1,1,1,2,1,1,1,1,1,1,1,1,1,1),
                    larynx=c(1,1,1,1,1,0,0,1,rep(0,22)),
                    tailler=c(3,3,3,3,2,3,2,2,2,2,2,2,rep(1,18)),
                    poids=c(3,3,3,3,2,2,3,2,2,2,2,2,1,2,2,rep(1,15)),
                    longueurs=c(3,3,rep(2,9),1,2,2,rep(1,15),2),
                    queue=c(2,2,1,2,3,3,3,3,1,2,1,1,2,3,2,2,2,2,2,1,2,2,1,2,1,1,3,2,2,2),
                    dents=c(rep(1,5),0,1,1,0,0,1,rep(0,19)),
                    typproie=c(1,1,1,2,2,2,2,3,3,3,2,rep(3,18),2),
                    arbre=c(0,0,1,1,1,0,rep(1,6),0,1,1,1,1,0,rep(1,12)),
                    chasse=c(1,0,0,0,0,1,0,0,1,0,0,1,rep(0,6),1,rep(0,5),1,rep(0,5))
)
rownames(Felinos)<-c("leon","tigre","jaguar","leopardo","once","guepardo","puma","nebul","serval","ocelot","lynx","caracal","viverrin","yaguarun","chaus","dore","merguay","margerit","cafer","chine","bengale","rouilleu","malaid","borneo","nigripes","manul","marbre","tigrin","temminck","andes")

Felinos1<-as.matrix(Felinos)

# diss calcula la matriz de disimilitud de un dataset y pop entrega la solución
# óptima de la partición.

res<-pop(diss(Felinos1))

# El res$y entrega un vector de 900 de longitus, lo cual lo pasaremos a una
# matriz. Esta matriz tendrá la solución en la diagonal, es decir, los grupos
# donde se encuentran los individuos. Hacemos esto ya que desde res no se
# puede extraer bien la información.

ss<-matrix(res$y,byrow=T,ncol=30)
a<-diag(ss)

# Matriz de las opiniones colectivas: esta matriz le da valor 1 a los individuos
# que se comparan y 0 a los que no.

Y<-matrix(rep(0,900),ncol=30,nrow=30)
for (i in 1:4) {
  s=which(a==i)
  Y[s,s]=1
}

# Matriz de Condorcet: esta matriz cada elemento es el número de variables que
# comparten.

C<-matrix(rep(0,900),ncol=30)
for (i in 1:30) {
  for (j in 1:30) {
    C[i,j]=sum(as.numeric(Felinos1[i,]==Felinos1[j,]))
  }
}

# Tasa para Condorcet
Ta=(sum((2*C-dim(Felinos)[2])*Y+(dim(Felinos)[2]-C)))/(dim(Felinos)[1]^2*dim(Felinos)[2])

# La tasa va entre 0 y 1, acercarse a 1 es que los grupos son perfectos, en este
# caso los grupos comparten todas las características.

# Para usar la tasa para k-medias podemos reutilizar C porque está basado en
# la tabla. Para Y hay que volver a calcularlo porque k-medias varia en sus
# resultados.

# Calculo del Y para k-medias, solo cambiamos la diagonal por el resultado entregado
# por k-medias. Recordar que cada vez que ejecutamos nos puede entregar una
# solución diferente:
group1<-kmeans(Felinos,centers=4,nstart=5)
print(group)
b<-group1$cluster # muestra solamente los grupos
for (i in 1:4) {
  s=which(b==i)
  Y[s,s]=1      # se puede escribir en una linea
}

# Tasa para K-medias
Ta=(sum((2*C-dim(Felinos)[2])*Y+(dim(Felinos)[2]-C)))/(dim(Felinos)[1]^2*dim(Felinos)[2])
