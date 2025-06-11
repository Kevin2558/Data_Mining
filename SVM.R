# SvmPath works with binary problems only
install.packages("svmpath")
library(svmpath)
data(svmpath)
# Dataset: La variable Y trae las clases (- y +) de los datos
X = mixture.data$x
Y = mixture.data$y
head(X)
head(Y)
# Notemos que los datos no son linealmente separables
 plot(X, col = Y+3, pch = Y+4, main = "Mixture data", xlab = "X1", ylab ="X2")
class(X)

# Split entre datos de entrenamiento y testeo
# Para la eleccion del tamaño de los datos de testeo usamos la formula
# (1-1/N)^N 
 hasard = sample(1:200)
Xapp = X[hasard[1: 150], ] # Aprendizaje
Yapp = Y[hasard[1: 150]]
Xtest = X[hasard[151: 200], ] # Prueba
Ytest = Y[hasard[151: 200]]

# Aplicamos svm y graficamos
result.svmpath = svmpath(Xapp, Yapp, trace = FALSE, plot = TRUE)
	
#Taux de bonne classification en apprentissage
# Vamos a ver si el modelo tiene buena bondad de ajuste. Si el valor se ajusta
# a 1 entonces el modelo tiene buena bondad de ajuste a los datos.

# Prediccion
prediction = predict(result.svmpath)
Y_pred = sign(prediction[ , dim(prediction)[2]])
length(Y_pred)
# Vector de valores que entregará caules están bien y mal clasificados
performance = Yapp-Y_pred
# Mal clasificados
length(performance[performance=!0])
# Bien clasificados
length(performance[performance==0])
# Porcentaje de bien clasificados
 length(performance[performance == 0])/length(Yapp)

#Taux de bonne classication en test
# Ahora volvemos a ser lo mismo que lo anterior pero con los datos de testeo
prediction = predict(result.svmpath, Xtest)
Ytest_pred = sign(prediction[ ,dim(prediction)[2]])
performance = Ytest-Ytest_pred
pourcentage2 = length(performance[performance == 0])/(length(Ytest)-1)
########################## iris################


###########################
?svmpath
 result.svmpath = svmpath( Xapp, Yapp,
kernel=radial.kernel,
param.kernel = 3,
trace = TRUE,
plot = TRUE
)
names(result.svmpath)
prediction = predict(result.svmpath,lambda=1.722304,matrix(c(0.5,1.5,1.8,-0.5),ncol=2),type="class")
prediction3 = predict(result.svmpath)
Y_pred3 = sign(prediction3[ , dim(prediction3)[2]])
length(Y_pred3)
performance3 = Yapp-Y_pred3
 pourcentage3=length(performance3[performance3 == 0])/(length(Yapp)-1)
 pourcentage3
