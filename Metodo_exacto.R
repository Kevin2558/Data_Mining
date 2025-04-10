## Algoritmo metodo exacto etodo exacto

data<-Felinos[1:10,] # Para entregarle un conjunto random usamos sample
r=4 # Numero de grupos

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
# Entrega todas las soluciones posibles en H