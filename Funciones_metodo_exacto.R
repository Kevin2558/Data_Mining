find.all.par<-function( #suitable for only very small networks and ks (complexity is k^n)

            n,         #number of units

            k,         #number of clusters

            only.k.groups=TRUE,  #do we damand that a partition has exactly 
#k groups, or are less groups also allowed

            switch.names=TRUE   #should partitions that only differ in group 
#names be considert equal (is c(1,1,2)==c(2,2,1))

){

            groups<-rep(list(1:k),n)

            comb<-as.matrix(expand.grid(groups))

            comb<-comb[,dim(comb)[2]:1]

            dimnames(comb)<-NULL

            if(switch.names) comb<-comb[1:(dim(comb)[1]/2),]

            comb<-comb[apply(comb,1,function(x)length(table(x)))>=ifelse(only.k.groups,k,2),]

            return(comb)

}

remove.double<-function(M) #removes duplicated partitios (when rand = 1) from matrix M

{

            new.M<-M[1,, drop = FALSE]

            for(i in 2:dim(M)[1]){

                        new<-TRUE

                        for(i2 in 1:dim(new.M)[1])

                        {

                                   if(rand(table(as.numeric(M[i,]),as.numeric(new.M[i2,])))==1)new<-FALSE

                        }

                        if(new) new.M<-rbind(new.M,M[i,])

            }

            return(new.M)

}



#a function used by function "remove.double"

rand<-function (tab) #extracted from function classAgreement from packcage  'e1071'

{

    n <- sum(tab)

    ni <- apply(tab, 1, sum)

    nj <- apply(tab, 2, sum)

    n2 <- choose(n, 2)

    1 + (sum(tab^2) - (sum(ni^2) + sum(nj^2))/2)/n2

}

All.k3.n5.par<- find.all.par(n=9,k=4)
All.k3.n5.par<-remove.double(All.k3.n5.par)
dim(All.k3.n5.par)
r=10
data<-Felinos[1:9,]
tiempo9<-numeric()
for(i in 2:7){
a<-Sys.time()
find.all.par(n=9,k=i)
tiempo9[i]<-difftime(Sys.time(),a)
}
a<-Sys.time()
jj<-find.all.par(n=9,k=6)
difftime(Sys.time(),a)
plot(2:4,tiempo9[-1],pch=17,axes=F,type="l", las=1,ylab="Time (in seconds)",las=2,xlab=expression(paste(alpha,"-Groups")),ylim=c(0,16),lwd=2,lty=1,col="green",bg="red")
#plot(as.data.frame(CVR_Binom)$N,as.data.frame(CVR_Binom)$CVR_binom,axes=F,type="l", las=1,ylab=expression(paste("Razon de Validez de Contenido cr?tico(  ", CVR[cr],")",sep=" ")),las=2,xlab="N (N?mero de panelistas)",lwd=2,ylim=c(0,1),lty=4,col="green",bg="red")
lines(2:4,tiempo5[-1], pch ="4", col = "red",lty=2,cex=.6)#
  lines(2:4,tiempo6[-1], pch=16, col = "blue",lty=3)
 lines(2:4,tiempo7[-1], pch=5, col = "black",lty=4)
 lines(2:4,tiempo8[-1], pch=6, col = "gray",lty=5)
 lines(2:4,tiempo4[-1], pch=7, col = "deepskyblue",lty=6)
legend(2,13,paste("Data size n=",c(9,5,6,7,8,4)),lty=c(1:6), pch=c(NA,NA,NA,NA,NA,NA),
col=c("green","red","blue","black","gray","deepskyblue"),cex=.75)
#legend(c(10,13),paste("data size  n=",c(110,90,70,50,30)),lty=c(1:5), pch=c(NA,NA,NA,NA,NA),col=c("green","red","blue","black","gray"))

#legend("bottomright",c(expression(paste(chi^2,"(",alpha==.05,",",nu==1,")"))," Binomial exacta ","Lawshe"),lty=c(4,1,NA),
#pch=c(NA,NA,19),col=c("green","blue","red"))
axis(1,at=seq(2,4,1))

axis(2,at=seq(0,16,5),las=2)
r=5
All.k3.n5.par<- find.all.par(n=dim(data)[1],k=r)
###################################################
##########################################################  Datos Felinos


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
rownames(Felinos)<-c("Lion","Tiger","Jaguar","Leopard","Oncilla","Cheetah","Puma","nebul","Serval","Ocelot","Lynx","Caracal","viverrin","yaguarun","chaus","dore","Margay","margerit","cafer","chine","Bengal cat","rouilleu","Malai","Bornean bay cat","Black-footed cat","Manul","Marbled cat","tigrin","Temminck","Andean mountain cat")
tiempo45_50<-numeric()
data<-Felinos[1:7]
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

enlace<-3*C-2*M   ############################Debo cambiar ese criterio 
#}r=4
All.k3.n5.par<- find.all.par(n=dim(data)[1],k=r)
z1<-0
a=0
for(i in 1: dim(All.k3.n5.par)[1]){
Y<-matrix(0,ncol=dim(data)[1],nrow=dim(data)[1])
for(k in 1:r){ ###start for Q

Y[which(All.k3.n5.par[i,]==k),which(All.k3.n5.par[i,]==k)]=1
diag(Y)=1
}  #### fin for Q
Z<-sum(enlace*Y)

if(Z>z1){z1<-Z
a<-i
Y1<-Y}

}
tiempo45_50[i]<-difftime(Sys.time(),a)


data<-Felinos[sample(1:30,9),]

data<-file.choose()
data<-Felinos[1:9,]

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

enlace<-3*C-2*M   ############################Debo cambiar ese criterio 
#}r=2
All.k3.n5.par<- find.all.par(n=dim(data)[1],k=r)
z1<-0
a=0
for(i in 1: dim(All.k3.n5.par)[1]){
Y<-matrix(0,ncol=dim(data)[1],nrow=dim(data)[1])
for(k in 1:r){ ###start for Q

Y[which(All.k3.n5.par[i,]==k),which(All.k3.n5.par[i,]==k)]=1
diag(Y)=1
}  #### fin for Q
Z<-sum(enlace*Y)

if(Z>z1){z1<-Z
a<-i
Y1<-Y}

}
All.k3.n5.par[a,]
Y1
z1


##############################3
###################################### TA: Tasa de ajuste

library(amap)
dim(Felinos)
pop(diss(as.matrix(data)))
C<-diss(as.matrix(data))
C
C<-matrix(0,ncol=dim(data)[1],nrow=dim(data)[1])
for(i in 1:dim(data)[1]){
   for(j in 1:dim(data)[1]){
e=0
      for(v in 1:dim(data)[2]){
       if(data[i,v]==data[j,v]){
          e=e+1
 }
                     }
C[i,j]=e
                }
}

p=0
for(i in 1:dim(data)[2]) p<-p+length(levels(as.factor(data[,i])))

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
######################################Tasa de ajuste
set.seed(1234)
n=1
#data<-Felinos[1:9,]
M<-dim(data)[2]
enlace<-3*C-2*M
Calidad_km<-numeric()
Calidad_c<-numeric()
Iw_c<-numeric()
Iw_km<-numeric()
for(r in 2:(dim(data)[1]-n)){
#r=3
                     ####Condorcet (Y_c)
All.k3.n5.par<- find.all.par(n=dim(data)[1],k=r)
z1<-0
a=0
for(i in 1: dim(All.k3.n5.par)[1]){
Y<-matrix(0,ncol=dim(data)[1],nrow=dim(data)[1])
for(k in 1:r){ ###start for Q

Y[which(All.k3.n5.par[i,]==k),which(All.k3.n5.par[i,]==k)]=1
diag(Y)=1
}  #### fin for Q
Z<-sum(enlace*Y)

if(Z>z1){z1<-Z
a<-i
Y1<-Y}

}
                       #########fin condorcet (Y_c)
fclus<- kmeans(data, r, nstart = 5)$cluster

Y_km<-matrix(0,ncol=dim(data)[1],nrow=dim(data)[1])
for(i in 1:dim(data)[1]){
   for(j in 1:dim(data)[1]){
Y_km[i,j]=ifelse(fclus[i]==fclus[j],1,0)
 
}}
Y_c<-Y1
Y_km



Y_I_c<-Y_c*matrix(rep(1/colSums(Y_c),dim(data)[1]),ncol=dim(data)[1])
Y_I_km<-Y_km*matrix(rep(1/colSums(Y_km),dim(data)[1]),ncol=dim(data)[1])
Iw_c[r-1]<-(p-sum(hat_c*Y_I_c))/M
Iw_km[r-1]<-(p-sum(hat_c*Y_I_km))/M
diag(C)=dim(data)[2]
C1<-(dim(data)[2]-C)

Y_c1<-(1-Y_c)
diag(Y_c1)<-0
diag(C1)=0
Calidad_c[r-1]<- sum(C*Y_c+C1*Y_c1)/(dim(data)[1]^2*dim(data)[2])
Calidad_km[r-1]<-sum(C*Y_km+C1*Y_km)/(dim(data)[1]^2*dim(data)[2])
}

                                        #####################plot
plot(2:(dim(data)[1]-n),Calidad_c,pch=17,axes=F,type="l", las=1,ylab="Partition's quality",las=2,xlab=expression(paste(alpha,"-Groups")),lwd=2,ylim=c(0,1),lty=4,col="green",bg="red")
#plot(as.data.frame(CVR_Binom)$N,as.data.frame(CVR_Binom)$CVR_binom,axes=F,type="l", las=1,ylab=expression(paste("Razon de Validez de Contenido cr?tico(  ", CVR[cr],")",sep=" ")),las=2,xlab="N (N?mero de panelistas)",lwd=2,ylim=c(0,1),lty=4,col="green",bg="red")
lines(2:(dim(data)[1]-n),Calidad_km, pch ="4", col = "red",lty=3,cex=.6)#
 # lines(t,h, pch=16, col = "blue",lty=1)
 #lines(t,e, pch=18, col = "black",lty=2)

legend("topright",c(expression(paste(alpha,"-Condorcet")),expression(paste("Kmeans"))),lty=c(4,3),
pch=c(NA,NA),col=c("green","red"))

#legend("topright",c(expression(paste(chi^2,"(",alpha==.05,",",nu==1,")"))," Binomial exacta ","Lawshe"),lty=c(4,1,NA),
#pch=c(NA,NA,19),col=c("green","blue","red"))
axis(1,at=seq(2,(dim(data)[1]-n),1))

axis(2,at=seq(0,1,.1),las=2)
                       #################33Plot Inertia
plot(2:(dim(data)[1]-n),Iw_c,pch=17,axes=F,type="l", las=1,ylab="Intra-class cluster",las=2,xlab=expression(paste(alpha,"-Groups")),lwd=2,ylim=c(0,2),lty=4,col="green",bg="red")
#plot(as.data.frame(CVR_Binom)$N,as.data.frame(CVR_Binom)$CVR_binom,axes=F,type="l", las=1,ylab=expression(paste("Razon de Validez de Contenido cr?tico(  ", CVR[cr],")",sep=" ")),las=2,xlab="N (N?mero de panelistas)",lwd=2,ylim=c(0,1),lty=4,col="green",bg="red")
lines(2:(dim(data)[1]-n),Iw_km, pch ="4", col = "red",lty=3,cex=.6)#

legend("topright",c(expression(paste(alpha,"-Condorcet")),expression(paste("Kmeans"))),lty=c(4,3),
pch=c(NA,NA),col=c("green","red"))

axis(1,at=seq(2,(dim(data)[1]-n),1))

axis(2,at=seq(0,2,.2),las=2)
                                  #######################
Calidad_c<-2*sum((C)*Y_c)/(dim(data)[1]^2*dim(data)[2])
Calidad_km<-2*sum(C*Y_km+C1*Y_km)/(dim(data)[1]^2*dim(data)[2])
Calidad_km<-2*sum((C)*Y_km)/(dim(data)[1]^2*dim(data)[2])


library(amap)
dim(Felinos)

cond<-pop(diss(as.matrix(data)))
fpop<-c(1,1,2,1,1,1,1,1)
Y_pop<-matrix(0,ncol=dim(data)[1],nrow=dim(data)[1])
for(i in 1:dim(data)[1]){
   for(j in 1:dim(data)[1]){
Y_pop[i,j]=ifelse(fpop[i]==fpop[j],1,0)
 
}}
Calidad_pop<-sum(C*Y_pop+C1*(1-Y_pop))/(dim(data)[1]^2*dim(data)[2])

################################Inertia Total: Lo correcto
p=0
for(i in 1:dim(data)[2]) p<-p+length(levels(as.factor(data[,i])))

hat_c<-matrix(0,ncol=dim(data)[1],nrow=dim(data)[1])
for(i in 1:dim(data)[1]){
for(j in 1:dim(data)[1]){
a<-as.numeric(data[i,]==data[j,])
aa<-which(a==1)
v<-rep(1,dim(data)[2])
for(k in aa){
v[k]<-as.numeric(data[,k]==data[j,k])
}
print(sum(v))
hat_c[i,j]<-sum(as.numeric(data[i,]==data[j,]))/(sum(v))

}}
Y_I<-Y*matrix(rep(1/colSums(Y),dim(data)[1]),ncol=dim(data)[1])
I_w[]<-(p-sum(hat_c*Y_I))/M
Iw_km
Iw_c



