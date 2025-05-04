# Calculo de la razon de la ganancia para variables categóricas y nominales con 
# umbrales

# Si hay variables numericas pero que representan categorías escribirlas entre
# comillas

# Para las variables nominales, al entregar un umbral, la función entregará la
# razon de la ganancia para cada umbral en un vector

RGan<-function(X,Var,clas,Um){
  
    p1 = sum(X[[clas]]=="Si")/dim(X)[1]
    p2 = sum(X[[clas]]=="No")/dim(X)[1]
    
    H = 0
    
    if(p1>0) H = H-p1*log2(p1)
    
    if(p2>0) H = H-p2*log2(p2)
  
  if(is.character(X[[Var]])){  
    Val = unique(X[[Var]])
    p <- numeric()
    H1 <- numeric()
    
    for (i in 1:length(Val)) {
      p[i] = sum(as.numeric(X[[Var]]==Val[i]))/dim(X)[1]
      X1 = X[which(X[[Var]]==Val[i]),]
      p1 = sum(X1[[clas]]=='Si')/dim(X1)[1]
      p2 = sum(X1[[clas]]=='No')/dim(X1)[1]
      
      h1=0
      
      if(p1>0) h1 = h1-p1*log2(p1)
      
      if(p2>0) h1 = h1-p2*log2(p2)
      
      H1[i] = h1
    }
    gan = H - sum(p*H1)
    
    rgan = gan/InfoSplit(X,Var)
    return(rgan)
  }
  else {
    Val = unique(X[[Var]])
    p1 <- numeric()
    p2 <- numeric()
    gan <- numeric()
    
    for (i in 1:length(Um)) {
      p1[i] = sum(as.numeric(X[[Var]]<Um[i]))/dim(X)[1]
      p2[i] = sum(as.numeric(X[[Var]]>=Um[i]))/dim(X)[1]
      
      X1 = X[which(X[[Var]]<Um[i]),]
      X2 = X[which(X[[Var]]>=Um[i]),]
      
      p11 = sum(X1[[clas]]=='Si')/dim(X1)[1]
      p12 = sum(X1[[clas]]=='No')/dim(X1)[1]
      
      p21 = sum(X2[[clas]]=='Si')/dim(X2)[1]
      p22 = sum(X2[[clas]]=='No')/dim(X2)[1]
      
      h1=0
      
      if(p11>0) h1 = h1-p11*log2(p11)
      if(p12>0) h1 = h1-p12*log2(p12)
      
      h2=0
      
      if(p21>0) h2 = h2-p21*log2(p21)
      if(p22>0) h2 = h2-p22*log2(p22)
      
      gan[i] = H - p1[i]*h1-p2[i]*h2
    }
    IS = InfoSplit(X,Var,Um)
    rgan = gan/IS
    return(rgan)
  }
}

X2 <- data.frame(Cielo = c("Sole","Sole","Nub","Lluv","Lluv","Lluv","Nub","Sole",
                           "Sole","Lluv","Sole","Nub","Nub","Lluv"),
                 Temp = c(27.5,25,26.5,20,19,17.5,17,21,19.5,22.5,22.5,21,25.5,20.5),
                 Hum = c("Alta","Baja","Baja","Baja","Alta","Alta","Baja","Baja",
                         "Alta","Baja","Alta","Baja","Alta","Alta"),
                 Vien = c("D","F","D","D","D","F","F","D","D","D","F","F","D","F"),
                 Des = c("No","No","Si","Si","Si","No","Si","No","Si","Si","Si",
                         "Si","Si","No"))

RGan(X2,"Temp","Des",Um)