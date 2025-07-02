##clase martes 15 de abril##
install.packages("arules")
library(arules)
###para generar un vector:
v<-c(1,0,4,-45)
v
####para generar lista:

#esto fue lo k hizo él xd
#está malo porque no se pueden hacer una tabla con arreglos con largo distintos, por eso es mas sencillo hacer lista!!
clientes<-data.frame(c1<-c("Agua","Choco"),
                     c2<-c("Pan","Café","Mermelada","Queso","Palta"),
                     c3<-c("Pan","Carne","Queso","Palta","Bebida"),
                     c4<-c("Pan","Té","Mermelada","Queso","Palta"),
                     c5<-c("Agua","Pan","Café","Mermelada","Queso"),
                     c6<-c("Agua","Café","Choco"))

#esto es lo bueno, es una lista con listas
Trans<-list(
c("Agua","Choco"),
c("Pan","Café","Mermelada","Queso","Palta"),
c("Pan","Carne","Queso","Palta","Bebida"),
c("Pan","Té","Mermelada","Queso","Palta"),
c("Agua","Pan","Café","Mermelada","Queso"),
c("Agua","Café","Choco")
)

###
names(Trans)=c("T1","T2","T3","T4","T5","T6")
### Otra alternativa para nombrar las transacciones es:

names(Trans)<-paste("T",c(1:6),sep="") #el sep nos dice con qué queremos separar, como en este caso no hay nada, no debe haber separacion y por eso abajo no funcionaba
Trans$T1 #en el original estaba con _ y no funcionaba
        #el $ es para ver cosas especificas


############ Luego se transforma la lista
############Trans a un objeto de transacciones:

Tabla<-as(Trans,"transactions")
inspect(Tabla)
class(Tabla)
nombre=sort(c("Agua","Pan","Cafe","Té","Choco",
"Carne","Mermelada","Queso","Palta","Bebida"))



##nombre=sort(unique(unlist(Trans)))de aquí rescata los nombres supongo, sin el unique da los nombres repetidos y con sort las ordena por orden alfabético

######Para visualizar las transacciones, se grafica lo siguiente:


image(Tabla,srt=35,axes=FALSE,ylab=names(Trans)[length(Trans):1],xlab=nombre,useRaster=F,las=2) #en ylab pone k va de desde 6 hasta 1, porque antes salian al revés!!
                                                                                                #las lo usaba para invertir los cositos, pero con images no funcionó
#no lo usa!!!!
#axis(1, at = 1:10, labels=nombre,srt=45,tick=FALSE)
#axis(2, at = 1:6, labels=paste("T",1:6),srt=45,tick=FALSE)
##############se procede
Regla<-apriori(Tabla,parameter=list(supp=.5,conf=.85)) #apriori es una función del paquete

inspect(Regla)
