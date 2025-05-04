# Funcion InfoSplit categorica y nominal

InfoSplit <- function(X,Var,Um){
  if(is.character(X[[Var]])){
    Val = unique(X[[Var]])
    p<-numeric()
    for (i in 1:length(Val)) {
      p[i] = sum(as.numeric(X[[Var]]==Val[i]))/dim(X)[1]
    }
    IS = -sum(p*log2(p))
    return(IS)
  }
  else{
    IS <- numeric()
    for (i in 1:length(Um)) {
      p1 = sum(as.numeric(X[[Var]]<Um[i]))/dim(X)[1]
      p2 = sum(as.numeric(X[[Var]]>=Um[i]))/dim(X)[1]
      IS[i]=-p1*log2(p1)-p2*log2(p2)
    }
    return(IS)
  }
}

InfoSplit(X1,"Cielo")

Um = c(17.5,19,20.5,21,25,25.5,27.5)
InfoSplit(X2,"Temp",Um)
