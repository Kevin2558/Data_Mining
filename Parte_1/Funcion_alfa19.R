library(gtools)

alfa19<-function(data,alpha){


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


C_prima<-dim(data)[2]-C

M<-dim(data)[2]


#enlace<-2*C-M

#enlace<-enlace-diag(diag(enlace))


enlace<-C
enlace<-enlace-diag(diag(enlace))

enlace2<-enlace

grupos<-list()
#alpha2<-alpha
A<-dim(data)[1]
B<-1:dim(data)[1]
suma<-1



while(A>alpha && suma!=0){




k<-numeric(dim(data)[1])
 for(j in 1:dim(data)[1]){
 k[j]<-max(enlace[,j])
  }

#tengo que cambiar ese A por un B
 if(A==dim(data)[1]){
  ini<-which.max(k)

}else{

k[ab]=0

 ini<-which.max(k)
}

 







  posicion<-which.max(enlace[ini,])
  a<-enlace[ini,posicion]
 
  di<-which(enlace[ini,]==a)



 
  enlace[ini,posicion]=0
  posicion_columna<-which.max(enlace[,posicion])
  b<-enlace[posicion_columna,posicion]
  enlace[ini,posicion]<-a






















 if(a>b){


if(length(di)>1){

     if(dim(data)[1]==A){

     grupos[[1]]<-c(ini,posicion)
     ab<-unlist(grupos)
     largo<-length(B[-ab])
     A<-length(grupos)+largo
     enlace[,posicion]=0
     enlace[posicion,]=0
    }else{

      
     
      grupos[[length(grupos)+1]]<-c(ini,posicion)

   
      ab<-unlist(grupos)
      dup<-ab[!(!duplicated(ab) & rev(!duplicated(rev(ab))))]

        if(length(dup)>=2){

           if(length(dup)==4){
            ak<-which(sapply(grupos, function(e) is.element(dup[1], e)))
            grupos[[ak[1]]]<-unique(unlist(grupos[ak]))
            grupos[[ak[2]]]<-NULL
             largo<-length(B[-ab])
             A<-length(grupos)+largo
            

            ab<-unlist(grupos)
            dup<-ab[!(!duplicated(ab) & rev(!duplicated(rev(ab))))]
            ak<-which(sapply(grupos, function(e) is.element(dup[1], e)))
            grupos[[ak[1]]]<-unique(unlist(grupos[ak]))
            grupos[[ak[2]]]<-NULL
             largo<-length(B[-ab])
             A<-length(grupos)+largo


         }else{

         az<-which(sapply(grupos, function(e) is.element(dup[1], e)))  
         grupos[[az[1]]]<-unique(unlist(grupos[az]))
         grupos[[az[length(az)]]]<-NULL  
         largo<-length(B[-ab])
         A<-length(grupos)+largo}

        }else{

         largo<-length(B[-ab])
         A<-length(grupos)+largo



      }
     




      enlace[,posicion]=0
      enlace[posicion,]=0




 }

    di2<-which(enlace[ini,]==a)

     for(j in 1:length(di2)){


       if(A>alpha){

     posicion<-which.max(enlace[ini,])
     a2<-enlace[ini,posicion]
     enlace[ini,posicion]=0
     posicion_columna<-which.max(enlace[,posicion])
     b2<-enlace[posicion_columna,posicion]
     enlace[ini,posicion]<-a2

      if(a2>=b2){
       enlace[,posicion]=0
       enlace[posicion,]=0
       grupos[[length(grupos)+1]]<-c(ini,posicion)

        
        ab<-unlist(grupos)
        dup<-ab[!(!duplicated(ab) & rev(!duplicated(rev(ab))))]
       
        if(length(dup)>=2){


     if(length(dup)==4){
             ak<-which(sapply(grupos, function(e) is.element(dup[1], e)))
             grupos[[ak[1]]]<-unique(unlist(grupos[ak]))
             grupos[[ak[2]]]<-NULL
             largo<-length(B[-ab])
             A<-length(grupos)+largo
             
 
             ab<-unlist(grupos)
             dup<-ab[!(!duplicated(ab) & rev(!duplicated(rev(ab))))]
             ak<-which(sapply(grupos, function(e) is.element(dup[1], e)))
             grupos[[ak[1]]]<-unique(unlist(grupos[ak]))
             grupos[[ak[2]]]<-NULL
             largo<-length(B[-ab])
             A<-length(grupos)+largo

       
      }else{



         az<-which(sapply(grupos, function(e) is.element(dup[1], e)))  
         grupos[[az[1]]]<-unique(unlist(grupos[az]))
         grupos[[az[length(az)]]]<-NULL  
         largo<-length(B[-ab])
         A<-length(grupos)+largo

        }


     

       } #termina el if(a>=b)
}


 } #termina el if(A>alpha)

} # aca termina el for


    

 #aca terminar el if(length)>=2
 }else{


  


 if(dim(data)[1]==A){

  grupos[[1]]<-c(ini,posicion)
  ab<-unlist(grupos)
  largo<-length(B[-ab])
  A<-length(grupos)+largo
  enlace[,posicion]=0
  enlace[posicion,]=0
 }else{

   grupos[[length(grupos)+1]]<-c(ini,posicion)

   enlace[,posicion]=0
   enlace[posicion,]=0

    
    ab<-unlist(grupos)
    dup<-ab[!(!duplicated(ab) & rev(!duplicated(rev(ab))))]

        if(length(dup)>=2){

        if(length(dup)==4){
            ak<-which(sapply(grupos, function(e) is.element(dup[1], e)))
            grupos[[ak[1]]]<-unique(unlist(grupos[ak]))
            grupos[[ak[2]]]<-NULL
             largo<-length(B[-ab])
             A<-length(grupos)+largo
            

            ab<-unlist(grupos)
            dup<-ab[!(!duplicated(ab) & rev(!duplicated(rev(ab))))]
            ak<-which(sapply(grupos, function(e) is.element(dup[1], e)))
            grupos[[ak[1]]]<-unique(unlist(grupos[ak]))
            grupos[[ak[2]]]<-NULL
             largo<-length(B[-ab])
             A<-length(grupos)+largo



         }else{



         az<-which(sapply(grupos, function(e) is.element(dup[1], e)))  
         grupos[[az[1]]]<-unique(unlist(grupos[az]))
         grupos[[az[length(az)]]]<-NULL  
         largo<-length(B[-ab])
         A<-length(grupos)+largo

        } 

       
         




     } #termina if(lengt(dup)>=2)

        largo<-length(B[-ab])
         A<-length(grupos)+largo
    
  
} #termina el else de  if(dim(data)[1]==A)

  



}


}


 #fin del if(a>b)













if(a==b){


if(length(di)>1){

     if(dim(data)[1]==A){

     grupos[[1]]<-c(ini,posicion)
     ab<-unlist(grupos)
     largo<-length(B[-ab])
     A<-length(grupos)+largo
     enlace[,posicion]=0
     enlace[posicion,]=0
    }else{

      
     
      grupos[[length(grupos)+1]]<-c(ini,posicion)

      ab<-unlist(grupos)
      dup<-ab[!(!duplicated(ab) & rev(!duplicated(rev(ab))))]

        if(length(dup)>=2){

           if(length(dup)==4){
            ak<-which(sapply(grupos, function(e) is.element(dup[1], e)))
            grupos[[ak[1]]]<-unique(unlist(grupos[ak]))
            grupos[[ak[2]]]<-NULL
             largo<-length(B[-ab])
             A<-length(grupos)+largo
            

            ab<-unlist(grupos)
            dup<-ab[!(!duplicated(ab) & rev(!duplicated(rev(ab))))]
            ak<-which(sapply(grupos, function(e) is.element(dup[1], e)))
            grupos[[ak[1]]]<-unique(unlist(grupos[ak]))
            grupos[[ak[2]]]<-NULL
             largo<-length(B[-ab])
             A<-length(grupos)+largo


         }else{

         az<-which(sapply(grupos, function(e) is.element(dup[1], e)))   
         grupos[[az[1]]]<-unique(unlist(grupos[az]))
         grupos[[az[length(az)]]]<-NULL  
         largo<-length(B[-ab])
         A<-length(grupos)+largo}

        }else{

         largo<-length(B[-ab])
         A<-length(grupos)+largo



      }
     




      enlace[,posicion]=0
      enlace[posicion,]=0




 }

    di2<-which(enlace[ini,]==a)

     for(j in 1:length(di2)){
    

     
     if(A>alpha){

     posicion<-which.max(enlace[ini,])
     a2<-enlace[ini,posicion]
     enlace[ini,posicion]=0
     posicion_columna<-which.max(enlace[,posicion])
     b2<-enlace[posicion_columna,posicion]
     enlace[ini,posicion]<-a2

      if(a2>=b2){
       enlace[,posicion]=0
       enlace[posicion,]=0
       grupos[[length(grupos)+1]]<-c(ini,posicion)

        
        ab<-unlist(grupos)
        dup<-ab[!(!duplicated(ab) & rev(!duplicated(rev(ab))))]
        
        if(length(dup)>=2){


      if(length(dup)==4){
             ak<-which(sapply(grupos, function(e) is.element(dup[1], e)))
             grupos[[ak[1]]]<-unique(unlist(grupos[ak]))
             grupos[[ak[2]]]<-NULL
             largo<-length(B[-ab])
             A<-length(grupos)+largo
             
 
             ab<-unlist(grupos)
             dup<-ab[!(!duplicated(ab) & rev(!duplicated(rev(ab))))]
             ak<-which(sapply(grupos, function(e) is.element(dup[1], e)))
             grupos[[ak[1]]]<-unique(unlist(grupos[ak]))
             grupos[[ak[2]]]<-NULL
             largo<-length(B[-ab])
             A<-length(grupos)+largo

       
      }else{


         az<-which(sapply(grupos, function(e) is.element(dup[1], e)))  
         grupos[[az[1]]]<-unique(unlist(grupos[az]))
         grupos[[az[length(az)]]]<-NULL  
         largo<-length(B[-ab])
         A<-length(grupos)+largo

        }

       } #termina el if(a>=b)

}


    
} #termina if(A>alpha)

} #termina el for

 #aca terminar el if(length)>=2
 }else{


  


 if(dim(data)[1]==A){

  grupos[[1]]<-c(ini,posicion)
  ab<-unlist(grupos)
  largo<-length(B[-ab])
  A<-length(grupos)+largo
  enlace[,posicion]=0
  enlace[posicion,]=0
 }else{

   grupos[[length(grupos)+1]]<-c(ini,posicion)

   enlace[,posicion]=0
   enlace[posicion,]=0

 
    ab<-unlist(grupos)
    dup<-ab[!(!duplicated(ab) & rev(!duplicated(rev(ab))))]

        if(length(dup)>=2){

        if(length(dup)==4){
            ak<-which(sapply(grupos, function(e) is.element(dup[1], e)))
            grupos[[ak[1]]]<-unique(unlist(grupos[ak]))
            grupos[[ak[2]]]<-NULL
             largo<-length(B[-ab])
             A<-length(grupos)+largo
            

            ab<-unlist(grupos)
            dup<-ab[!(!duplicated(ab) & rev(!duplicated(rev(ab))))]
            ak<-which(sapply(grupos, function(e) is.element(dup[1], e)))
            grupos[[ak[1]]]<-unique(unlist(grupos[ak]))
            grupos[[ak[2]]]<-NULL
             largo<-length(B[-ab])
             A<-length(grupos)+largo



         }else{



         az<-which(sapply(grupos, function(e) is.element(dup[1], e)))  
         grupos[[az[1]]]<-unique(unlist(grupos[az]))
         grupos[[az[length(az)]]]<-NULL  
         largo<-length(B[-ab])
         A<-length(grupos)+largo

        }

       
         




     }  #termina el if(length(dup)>=2)


         largo<-length(B[-ab])
         A<-length(grupos)+largo
    
  
} #termina el else de  if(dim(data)[1]==A)

  



}


}

# termina el if(a==b)



if(a<b){


if(length(di)>1){

     if(dim(data)[1]==A){

     grupos[[1]]<-c(ini,posicion)
     ab<-unlist(grupos)
     largo<-length(B[-ab])
     A<-length(grupos)+largo
     enlace[,posicion]=0
     enlace[posicion,]=0
    }else{

      
     
      grupos[[length(grupos)+1]]<-c(ini,posicion)

   
      ab<-unlist(grupos)
      dup<-ab[!(!duplicated(ab) & rev(!duplicated(rev(ab))))]

        if(length(dup)>=2){

           if(length(dup)==4){
            ak<-which(sapply(grupos, function(e) is.element(dup[1], e)))
            grupos[[ak[1]]]<-unique(unlist(grupos[ak]))
            grupos[[ak[2]]]<-NULL
             largo<-length(B[-ab])
             A<-length(grupos)+largo
            

            ab<-unlist(grupos)
            dup<-ab[!(!duplicated(ab) & rev(!duplicated(rev(ab))))]
            ak<-which(sapply(grupos, function(e) is.element(dup[1], e)))
            grupos[[ak[1]]]<-unique(unlist(grupos[ak]))
            grupos[[ak[2]]]<-NULL
             largo<-length(B[-ab])
             A<-length(grupos)+largo


         }else{

         az<-which(sapply(grupos, function(e) is.element(dup[1], e)))  
         grupos[[az[1]]]<-unique(unlist(grupos[az]))
         grupos[[az[length(az)]]]<-NULL  
         largo<-length(B[-ab])
         A<-length(grupos)+largo}

        }else{

         largo<-length(B[-ab])
         A<-length(grupos)+largo



      }
     




      enlace[,posicion]=0
      enlace[posicion,]=0




 }

    di2<-which(enlace[ini,]==a)

     for(j in 1:length(di2)){


       if(A>alpha){

     posicion<-which.max(enlace[ini,])
     a2<-enlace[ini,posicion]
     enlace[ini,posicion]=0
     posicion_columna<-which.max(enlace[,posicion])
     b2<-enlace[posicion_columna,posicion]
     enlace[ini,posicion]<-a2

      if(a2>=b2){
       enlace[,posicion]=0
       enlace[posicion,]=0
       grupos[[length(grupos)+1]]<-c(ini,posicion)

        
        ab<-unlist(grupos)
        dup<-ab[!(!duplicated(ab) & rev(!duplicated(rev(ab))))]
       
        if(length(dup)>=2){


     if(length(dup)==4){
             ak<-which(sapply(grupos, function(e) is.element(dup[1], e)))
             grupos[[ak[1]]]<-unique(unlist(grupos[ak]))
             grupos[[ak[2]]]<-NULL
             largo<-length(B[-ab])
             A<-length(grupos)+largo
             
 
             ab<-unlist(grupos)
             dup<-ab[!(!duplicated(ab) & rev(!duplicated(rev(ab))))]
             ak<-which(sapply(grupos, function(e) is.element(dup[1], e)))
             grupos[[ak[1]]]<-unique(unlist(grupos[ak]))
             grupos[[ak[2]]]<-NULL
             largo<-length(B[-ab])
             A<-length(grupos)+largo

       
      }else{



         az<-which(sapply(grupos, function(e) is.element(dup[1], e)))  
         grupos[[az[1]]]<-unique(unlist(grupos[az]))
         grupos[[az[length(az)]]]<-NULL  
         largo<-length(B[-ab])
         A<-length(grupos)+largo

        }


     

       } #termina el if(a>=b)
}


 } #termina el if(A>alpha)

} # aca termina el for


    

 #aca terminar el if(length)>=2
 }else{


  


 if(dim(data)[1]==A){

  grupos[[1]]<-c(ini,posicion)
  ab<-unlist(grupos)
  largo<-length(B[-ab])
  A<-length(grupos)+largo
  enlace[,posicion]=0
  enlace[posicion,]=0
 }else{

   grupos[[length(grupos)+1]]<-c(ini,posicion)

   enlace[,posicion]=0
   enlace[posicion,]=0

    
    ab<-unlist(grupos)
    dup<-ab[!(!duplicated(ab) & rev(!duplicated(rev(ab))))]

        if(length(dup)>=2){

        if(length(dup)==4){
            ak<-which(sapply(grupos, function(e) is.element(dup[1], e)))
            grupos[[ak[1]]]<-unique(unlist(grupos[ak]))
            grupos[[ak[2]]]<-NULL
             largo<-length(B[-ab])
             A<-length(grupos)+largo
            

            ab<-unlist(grupos)
            dup<-ab[!(!duplicated(ab) & rev(!duplicated(rev(ab))))]
            ak<-which(sapply(grupos, function(e) is.element(dup[1], e)))
            grupos[[ak[1]]]<-unique(unlist(grupos[ak]))
            grupos[[ak[2]]]<-NULL
             largo<-length(B[-ab])
             A<-length(grupos)+largo



         }else{



         az<-which(sapply(grupos, function(e) is.element(dup[1], e)))  
         grupos[[az[1]]]<-unique(unlist(grupos[az]))
         grupos[[az[length(az)]]]<-NULL  
         largo<-length(B[-ab])
         A<-length(grupos)+largo

        } 

       
         




     } #termina if(lengt(dup)>=2)

        largo<-length(B[-ab])
         A<-length(grupos)+largo
    
  
} #termina el else de  if(dim(data)[1]==A)

  



}


}


 #fin del if(a<b)





k<-numeric(dim(data)[1])
 for(j in 1:dim(data)[1]){
 k[j]<-max(enlace[,j])
  }

ab<-unlist(grupos)
k[ab]=0

largo<-length(B[-ab])



if(sum(k)==0 || largo==0){



 suma<-0



}else{


suma<-sum(k)

}

k2<-length(k[k!=0])

if(k2==1){

suma<-0


}



} # Termina el while







while(A>alpha && suma==0){


ab3<-length(unlist(grupos))


 

if(ab3==dim(data)[1]){



af2<-1:length(grupos)

combinaciones<-list()

combi <- combinations(length(af2), 2, af2)

for(an in 1:dim(combi)[1]){

combinaciones[[an]]<-combi[an,]

}









Calidad_c<-rep(0,length(combinaciones))

for(kp in 1:length(Calidad_c)){

grupos2<-grupos
grupos2[[length(grupos2)+1]]<-unlist(grupos2[combinaciones[[kp]]])

grupos2[combinaciones[[kp]]]<-NULL

       

class_C<-numeric()

for(n in 1:length(grupos2)){
class_C[grupos2[[n]]]<-n
}







Y_c<-matrix(0,ncol=dim(data)[1],nrow=dim(data)[1])
for(i in 1:dim(data)[1]){
   for(j in 1:dim(data)[1]){
Y_c[i,j]=ifelse(class_C[i]==class_C[j],1,0)
 
}}



Y_c1<-(1-Y_c)
C1<-(dim(data)[2]-C)
Calidad_c[kp]<-sum(C*Y_c+C1*Y_c1)


} # termina el for kp



max_calidad<-which.max(Calidad_c)

posi_calidad<-unlist(combinaciones[max_calidad])
grupos[[posi_calidad[1]]]<-unlist(grupos[posi_calidad])
grupos[posi_calidad[2]]<-NULL





ab<-unlist(grupos)
largo<-length(B[-ab])
A<-length(grupos)+largo


}else{
ab<-unlist(grupos)
largo2<-B[-ab]

grupos[[length(grupos)+1]]<-largo2

af2<-1:length(grupos)

combinaciones<-list()

combi <- combinations(length(af2), 2, af2)

for(an in 1:dim(combi)[1]){

combinaciones[[an]]<-combi[an,]

}









Calidad_c<-rep(0,length(combinaciones))

for(kp in 1:length(Calidad_c)){

grupos2<-grupos
grupos2[[length(grupos2)+1]]<-unlist(grupos2[combinaciones[[kp]]])

grupos2[combinaciones[[kp]]]<-NULL

       

class_C<-numeric()

for(n in 1:length(grupos2)){
class_C[grupos2[[n]]]<-n
}







Y_c<-matrix(0,ncol=dim(data)[1],nrow=dim(data)[1])
for(i in 1:dim(data)[1]){
   for(j in 1:dim(data)[1]){
Y_c[i,j]=ifelse(class_C[i]==class_C[j],1,0)
 
}}



Y_c1<-(1-Y_c)
C1<-(dim(data)[2]-C)
Calidad_c[kp]<-sum(C*Y_c+C1*Y_c1)


} # termina el for kp



max_calidad<-which.max(Calidad_c)

posi_calidad<-unlist(combinaciones[max_calidad])
grupos[[posi_calidad[1]]]<-unlist(grupos[posi_calidad])
grupos[posi_calidad[2]]<-NULL





ab<-unlist(grupos)
largo<-length(B[-ab])
A<-length(grupos)+largo









}#termina el else de if(ab3==dim(data)[1]). Falta colocar un for





} #termina el while










z<-list()
z<-grupos


ab<-unlist(grupos)
ac<-B[-ab]

if(length(grupos)==alpha){

   z<-grupos


}else{

for(i in 1:length(ac)){

  z[[length(grupos)+i]]<-ac[i]


}
}


return(z)

}

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

datos=Felinos
dataLenses<-dataLenses[-1,-c(1,6)]
ballones<-read.csv2(file.choose(),h=F)
b<-ballones[1,]
ballones<-ballones[-1,]
data<-ballones
colnames(ballones)<-b
ballones<-balones[-1,]
datos1<-ballones[,-5]
res<-dataLenses[-1,6]
dataLenses<-read.csv2(file.choose(),h=F)
colnames(dataLenses)
colnames(dataLenses)=c("Name","Age", "spectacle prescription", "astigmatic", "tear production rate","class")
dato<-dataLenses
alfa19(datos,2)
alfa16(datos1,2)
alfa14(datos1,2)
ballones[,5]
library(klaR)
fclus<- kmodes(datos, 4, iter.max = 10)$cluster
alfa(dato,3)
dataLenses[-1,6]
