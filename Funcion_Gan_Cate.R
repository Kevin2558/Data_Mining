# Calculo de la ganancia para variables categóricas

# Si hay variables numericas, darle categorias y escribirla entre comillas

Gan<-function(X,Var,clas){
if(is.character(X[[Var]])){
    p1 = sum(X[[clas]]=="Si")/dim(X)[1]
    p2 = sum(X[[clas]]=="No")/dim(X)[1]
    
    H = 0
    
    if(p1>0) H = H-p1*log2(p1)
    
    if(p2>0) H = H-p2*log2(p2)
    
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
    return(gan)
}
else {return(print("La varible no es categorica"))}
}

X1 <- data.frame(Cielo = c("Sole","Sole","Nub","Lluv","Lluv","Lluv","Nub","Sole",
                           "Sole","Lluv","Sole","Nub","Nub","Lluv"),
                 Temp = c("Alta","Alta","Alta","Nor","Nor","Baja","Baja","Nor",
                          "Nor","Nor","Nor","Nor","Alta","Nor"),
                 Hum = c("Alta","Baja","Baja","Baja","Alta","Alta","Baja","Baja",
                         "Alta","Baja","Alta","Baja","Alta","Alta"),
                 Vien = c("D","F","D","D","D","F","F","D","D","D","F","F","D","F"),
                 Des = c("No","No","Si","Si","Si","No","Si","No","Si","Si","Si",
                         "Si","Si","No"))

Gan(X1,"Temp","Des")

X2 <- data.frame(Cielo = c("Sole","Sole","Nub","Lluv","Lluv","Lluv","Nub","Sole",
                          "Sole","Lluv","Sole","Nub","Nub","Lluv"),
                Temp = c(27.5,25,26.5,20,19,17.5,17,21,19.5,22.5,22.5,21,25.5,20.5),
                Hum = c("Alta","Baja","Baja","Baja","Alta","Alta","Baja","Baja",
                        "Alta","Baja","Alta","Baja","Alta","Alta"),
                Vien = c("D","F","D","D","D","F","F","D","D","D","F","F","D","F"),
                Des = c("No","No","Si","Si","Si","No","Si","No","Si","Si","Si",
                        "Si","Si","No"))

Gan(X2,"Temp","Des")
