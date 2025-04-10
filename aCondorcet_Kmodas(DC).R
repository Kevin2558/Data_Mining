################################################################################
################################################################################

# Recordar que ahora estamos en datos categoricos

install.packages('klaR')
library(klaR)

data<-Felinos
k=4 # Número de grupos

res<-alfa19(data,4) # Solucion método de alpha-Condorcet
fclus<-kmodes(data,4,iter.max = 10)$cluster # Solucion metodo k-modas

fclus # Recordar que k-modas entrega una solucion diferente cada vez que ejecutamos
res

C<-matrix(0,ncol=dim(data)[1],nrow=dim(data)[1])

for(i in 1:dim(data)[1]){
  for(j in 1:dim(data)[1]){
    if(i<j){
      C[i,j]=sum(as.numeric(data[i,]==data[j,]))
    }else
      C[i,j]=sum(as.numeric(data[i,]==data[j,]))
  }
  C[i,i]=dim(data)[2]
}
#data<-Felinos
M<-dim(data)[2]

# Estos dos siguientes bloques de codigo vienen de formulas calculadas en el
# paper pagina 7 

p=0
for(i in 1:dim(data)[2]) p<-p+length(levels(as.factor(data[,i])))

# Calculo del c gorro

hat_c<-matrix(0,ncol=dim(data)[1],nrow=dim(data)[1])
for(i in 1:dim(data)[1]){
  for(j in 1:dim(data)[1]){
    a<-as.numeric(data[i,]==data[j,])
    aa<-which(a==1)
    if(length(aa)==0){aa=0}
    if(aa[1]!=0){
      #val<-data.frame(var=aa,m=data[j,aa])
      v<-rep(1,length(a))
      for(k in aa){
        v[k]<-sum(as.numeric(data[,k]==data[j,k]))
        #print(paste("v",k))
        #print(v)
      }
      #print(sum(v))
      hat_c[i,j]<-sum(as.numeric(data[i,]==data[j,])/v)
    }
  }} 

M<-dim(data)[2]

Y_c<-matrix(0,ncol=dim(data)[1],nrow=dim(data)[1]) # Condorcet
Y_km<-matrix(0,ncol=dim(data)[1],nrow=dim(data)[1]) # K-modas

#Matriz Y para Condorcet

for (i in 1:length(res)) {
  Y_c[res[[i]],res[[i]]]<-1
}

# Matriz Y para k-modas

# Para encontrar las posiciones de los grupos usamos la funcion which y hacemos
# fcluss == y el numero del grupo que queremos buscar

# Ej. which(fclus==1) para el grupo 1, así para recorrer todos los grupos usamos
# un ciclo for.

for (i in 1:k) {
  Y_km[which(fclus==i),which(fclus==i)]<-1
}

# Calculamos el indice de intraclase

Y_I_c<-Y_c*matrix(rep(1/colSums(Y_c),dim(data)[1]),ncol=dim(data)[1])
Y_I_km<-Y_km*matrix(rep(1/colSums(Y_km),dim(data)[1]),ncol=dim(data)[1])

Iw_c<-(p-sum(hat_c*Y_I_c))/M
Iw_km<-(p-sum(hat_c*Y_I_km))/M

Iw_c 
Iw_km

# Comparamos los indices y el que posee menor indice es la mejor solucion.
# Cuando usemos indice para interclase el de mayor indice será mejor.