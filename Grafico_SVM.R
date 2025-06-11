#######SVM (pdf)########
install.packages("svmpath")
library(svmpath)
##generamos el dataset, en este caso para dos gaussianas

#damos los parámetros
n <- 150 # number of data points
p <- 2 # dimension

sigma <- 1 # variance of the distribution
meanpos <- 0 # centre of the distribution of positive examples
meanneg <- 3 # centre of the distribution of negative examples
npos <- round(n/2) # number of positive examples
nneg <- n-npos # number of negative examples

# generamos los puntos
xpos <- matrix(rnorm(npos*p,mean=meanpos,sd=sigma),npos,p)
xneg <- matrix(rnorm(nneg*p,mean=meanneg,sd=sigma),npos,p)
x <- rbind(xpos,xneg)
# generamos las etiquetas
y <- matrix(c(rep(1,npos),rep(-1,nneg)))
# Visualizamos el data
plot(x,col=ifelse(y>0,1,2),mar=c(4,4,2,1)) #agregué lo del margen
legend("topleft",c('Positive','Negative'),col=seq(2),pch=1,text.col=seq(2))


#ahora sepramos el data en 80% datos de entrenamiento y 20% de datos test

ntrain <- round(n*0.8) # number of training examples
tindex <- sample(n,ntrain) # indices of training samples
xtrain <- x[tindex,]
xtest <- x[-tindex,]
ytrain <- y[tindex]
ytest <- y[-tindex]
istrain=rep(0,n)
istrain[tindex]=1

#visualizamos
plot(x,col=ifelse(y>0,1,2),pch=ifelse(istrain==1,1,2))
legend("topleft",c('Positive Train','Positive Test','Negative Train','Negative Test'),
       col=c(1,1,2,2),pch=c(1,2,1,2),text.col=c(1,1,2,2))



##Entrenamos el modelo
library(kernlab)

svp <- ksvm(xtrain,ytrain,type="C-svc",kernel='vanilladot',C=100,scaled=c())
#ponemos c=100 para tener una baja toleracia a los errores.
svp

attributes(svp)

alpha(svp) #los coeficientes de los vectores de soporte
#cuanto contribuye cada vector soporte al modelo final
alphaindex(svp)#las posiciones de los vectores soporte en los datos de entrenamiento
b(svp) #termino independiente.

###aqui va la pregunta 1

plotlinearsvm <- function(svp, xtrain, ytrain) {
  # Obtener alphas y vectores de soporte
  alpha_vals <- alpha(svp)[[1]]
  sv_indices <- alphaindex(svp)[[1]]
  svs <- as.matrix(xtrain[sv_indices, ])
  y_svs <- ytrain[sv_indices]
  
  # Asegurar que alpha_vals y y_svs sean columnas
  coef <- alpha_vals * y_svs  # Vector de coeficientes
  w <- t(coef) %*% svs        # Producto matricial para obtener w (1x2)
  
  # Convertir w a vector
  w <- as.vector(w)
  
  # Calcular b usando la media de los SVs
  b_val <- mean(y_svs - svs %*% w)
  
  # Dibujar puntos
  plot(xtrain, col=ifelse(ytrain > 0, "blue", "red"), pch=19,
       xlab="x1", ylab="x2", main="SVM Lineal con Frontera y Márgenes")
  
  # Dibujar vectores de soporte
  points(svs, pch=1, cex=2, lwd=2)
  
  # Rango de x
  x_vals <- seq(min(xtrain[,1]), max(xtrain[,1]), length.out = 100)
  
  # Calcular líneas del hiperplano y los márgenes
  y_decision <- -(w[1]*x_vals + b_val) / w[2]
  y_margin1  <- -(w[1]*x_vals + b_val - 1) / w[2]
  y_margin2  <- -(w[1]*x_vals + b_val + 1) / w[2]
  
  # Dibujar las líneas
  lines(x_vals, y_decision, col="black", lwd=2)
  lines(x_vals, y_margin1, col="black", lty=2)
  lines(x_vals, y_margin2, col="black", lty=2)
  
  # Leyenda
  legend("topright", legend=c("Clase +1", "Clase -1", "Vectores de soporte"),
         col=c("blue", "red", "black"), pch=c(19, 19, 1), pt.cex=c(1, 1, 2))
}


plotlinearsvm(svp, xtrain, ytrain)

#predecimos con el modelo entrenado.

ypred = predict(svp,xtest)
table(ytest,ypred)

sum(ypred==ytest)/length(ytest) #lo hizo rekio

ypredscore = predict(svp,xtest,type="decision")

# Check that the predicted labels are the signs of the scores
table(ypredscore > 0,ypred)
# Package to compute ROC curve, precision-recall etc...
install.packages('ROCR')
library(ROCR)
pred <- prediction(ypredscore,ytest)
# Plot ROC curve
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf) #vemos que clasifica bien los positivos
# Plot precision/recall curve
perf <- performance(pred, measure = "prec", x.measure = "rec")
plot(perf) #como está tan cerca de 1, significa que ditingue
# Plot accuracy as function of threshold
perf <- performance(pred, measure = "acc")
plot(perf)# muestra el umbral donde el modelo alcanza su maxima precision.









#funcion mala por el el termino independiente b.
#lo intenté y no dio xdddddddddd




plotlinearsvm <- function(svp, xtrain, ytrain) {
  Obtener alphas y vectores de soporte
  alpha_vals <- alpha(svp)[[1]]
  sv_indices <- alphaindex(svp)[[1]]
  svs <-as.matrix(xtrain[sv_indices, ])
  y_svs <- ytrain[sv_indices]
  
  # Calcular el vector de pesos w y el sesgo b
  w <- colSums(alpha_vals * y_svs * svs)
  b_val <- b(svp)
  
  # Dibujar puntos
  plot(xtrain, col=ifelse(ytrain > 0, "blue", "red"), pch=19,
       xlab="x1", ylab="x2", main="SVM Lineal con Frontera y Márgenes")
  
  # Dibujar vectores de soporte
  points(svs, pch=1, cex=2, lwd=2)
  
  # Rango de x para trazar las líneas
  x_vals <- seq(min(xtrain[,1]), max(xtrain[,1]), length.out = 100)
  
  # Frontera de decisión: f(x) = 0 → y = -(w1*x + b)/w2
  y_decision <- -(w[1]*x_vals + b_val) / w[2]
  y_margin1 <- -(w[1]*x_vals + b_val - 1) / w[2]  # f(x) = +1
  y_margin2 <- -(w[1]*x_vals + b_val + 1) / w[2]  # f(x) = -1
  
  # Trazar líneas
  lines(x_vals, y_decision, col="black", lwd=2)
  lines(x_vals, y_margin1, col="black", lty=2)
  lines(x_vals, y_margin2, col="black", lty=2)
  
  # Leyenda
  legend("topright", legend=c("Clase +1", "Clase -1", "Vectores de soporte"),
         col=c("blue", "red", "black"), pch=c(19, 19, 1), pt.cex=c(1, 1, 2))
}
