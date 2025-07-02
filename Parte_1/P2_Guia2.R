# EJERCICIO 2 GUIA 2

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
help(head)
Felinos1 = head(Felinos,n=5L)
find.all.par(n=5,k=4)
remove.double(find.all.par(n=5,k=4))

install.packages('klaR')
library(klaR)

data=head(Felinos,n=10L)
res<-alfa19(data,4) # Solucion método de alpha-Condorcet
fclus<-kmodes(data,4,iter.max = 10)$cluster # Solucion metodo k-modas

res
fclus

# MATRIZ DE LAS OPINIONES COLECTIVAS

Y_c<-matrix(0,ncol=dim(data)[1],nrow=dim(data)[1]) # Condorcet
Y_km<-matrix(0,ncol=dim(data)[1],nrow=dim(data)[1]) # K-modas

for (i in 1:length(res)) {
  Y_c[res[[i]],res[[i]]]<-1
}

for (i in 1:k) {
  Y_km[which(fclus==i),which(fclus==i)]<-1
}

# MATRIZ DE CONDORCET

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

M<-dim(data)[2]

# INDICE DE AJUSTE

# CONDORCET
C1=dim(data)[2]-C
Y_c1<-(1-Y_c)
diag(Y_c1)<-0
diag(C1)=0

Calidad_alfa<- sum(C*Y_c+C1*Y_c1)/(dim(data)[1]^2*dim(data)[2])

Calidad_alfa

# K-MODES

C1=dim(data)[2]-C
Y_km1<-(1-Y_km)
diag(Y_km1)<-0
diag(C1)=0

Calidad_km<- sum(C*Y_km+C1*Y_c1)/(dim(data)[1]^2*dim(data)[2])

Calidad_km

# INERCIA INTRACLASE

p=0
for(i in 1:dim(data)[2]) p<-p+length(levels(as.factor(Felinos2[,i])))

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

Y_I_c<-Y_c*matrix(rep(1/colSums(Y_c),dim(data)[1]),ncol=dim(data)[1])
Y_I_km<-Y_km*matrix(rep(1/colSums(Y_km),dim(data)[1]),ncol=dim(data)[1])

Iw_c<-(p-sum(hat_c*Y_I_c))/M
Iw_km<-(p-sum(hat_c*Y_I_km))/M

Iw_c
Iw_km

# EJERCICIO PERO USANDO CONDORCET PARA COMPARAR

install.packages("amap")
library("amap")
Felinos1<-as.matrix(data)
res<-pop(diss(Felinos1))
ss<-matrix(res$y,byrow=T,ncol=10)
a<-diag(ss)

Y<-matrix(rep(0,100),ncol=10,nrow=10)
for (i in 1:4) {
  s=which(a==i)
  Y[s,s]=1
}

C1=dim(data)[2]-C
Y_c1<-(1-Y)
diag(Y)<-0
diag(C1)=0

Calidad_alfa<- sum(C*Y+C1*Y_c1)/(dim(data)[1]^2*dim(data)[2])

Calidad_alfa

Y_I_c<-Y*matrix(rep(1/colSums(Y),dim(data)[1]),ncol=dim(data)[1])
Iw_c<-(p-sum(hat_c*Y_I_c))/M
Iw_c
