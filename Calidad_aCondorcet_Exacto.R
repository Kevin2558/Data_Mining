data<-Felinos[1:10,]
r=4
##### Calculo la matriz de Condorcet


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

###### Algoritmo exacto 

All.k3.n5.par<- find.all.par(n=dim(data)[1],k=4)
z1<-0
a=0
H<-list()
f<-numeric()
x<-0
for(i in 1: dim(All.k3.n5.par)[1]){
  Y<-matrix(0,ncol=dim(data)[1],nrow=dim(data)[1])
  
  for(k in 1:r){ ###start for Q
    Y[which(All.k3.n5.par[i,]==k),which(All.k3.n5.par[i,]==k)]=1
    diag(Y)=1
  }  #### fin for Q
  
  Z<-sum(C*Y+(1-Y)*(dim(data)[2]-C))
  
  if(Z>z1){
    H<-list()
    x=1
    f<-numeric()
    z1<-Z
    Y1<-Y
    f[1]<-Z
    H[[1]]=Y1
  }
  if(Z==z1){
    ##H<-list(H,Y1)
    x=x+1
    f[x]<-Z
    H[[x]]<-Y1
  }
}
#######Luego Calculo indice de ajuste o calidad de la ###partición


Y<-H[[1]] ### algoritmo exacto

C1=dim(data)[2]-C
Y_c1<-(1-Y)
diag(Y_c1)<-0
diag(C1)=0

Calidad_c<- sum(C*Y+C1*Y_c1)/(dim(data)[1]^2*dim(data)[2])

Calidad_c

#########generalizar el calculo de todas las soluciones que están almacenadas en H
Calidad_exacto<-numeric()
for(i in 1:length(H)){
  
  Y<-H[[i]] ### algoritmo exacto
  
  C1=dim(data)[2]-C
  Y_c1<-(1-Y)
  diag(Y_c1)<-0
  diag(C1)=0
  
  Calidad_exacto[i]<- sum(C*Y+C1*Y_c1)/(dim(data)[1]^2*dim(data)[2])
  
  
}


########solucion mediante alfa19(algoritmo heurístico)

res<-alfa19(data,4)

dim(data)
Y<-matrix(0,ncol=10,nrow=10)


for(i in 1:length(res)){
  
  Y[res[[i]],res[[i]]]=1
  
}

#########Me toca calcular la calidad para la solución del alfa19


C1=dim(data)[2]-C
Y_c1<-(1-Y)
diag(Y_c1)<-0
diag(C1)=0

Calidad_alfa<- sum(C*Y+C1*Y_c1)/(dim(data)[1]^2*dim(data)[2])

Calidad_alfa