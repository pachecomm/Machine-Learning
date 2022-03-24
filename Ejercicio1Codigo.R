library(MASS)
library(e1071)

# ADVERTENCIA: ALGUNOS MODELOS Y SIMULACIONES PUEDEN COMPROMETER EL ESTADO 
# DE SU EQUIPO. REBICE LOS MODELOS ANTES DE CORRERLOS!!

options(digits=4, width = 120)

# Importamos los datos
Pima<- rbind(Pima.tr, Pima.te)
Pima[, c(1:7)] <- sapply(Pima[, c(1:7)], as.numeric)
str(Pima)
attach(Pima)

# Análisis de Componentes Principales -------------------------------------
# Matriz de correlaciones
cor(Pima[-8])
# Gráfica de proyecciones 2 a 2 en el espacio original
plot(Pima, pch=as.numeric(Pima$type), col=Pima$type, cex=1,
	 main="Gráficas Pima")
# Componentes Principales
Pima_pca <- prcomp(Pima[-8], scale=TRUE)
# Gráfica proyecciones 2 a 2 en las componentes principales
plot(as.data.frame(Pima_pca$x),pch=as.numeric(Pima$type),
	 col=Pima$type, cex=1, main="Análisis de Componentes Principales")
# Primeras dos componentes
plot(Pima_pca$x[,1], Pima_pca$x[,2],pch=as.numeric(Pima$type),
	 col=Pima$type, cex=1, xlab="PC1", ylab="PC2",
	 main="Análisis de Componentes Principales")
# Graficas de varianza explicada y acumulada por componente
par(mfrow=c(1,2))
plot(Pima_pca$sdev^2/sum(Pima_pca$sdev^2), xlab="Componente Principal", 
	 ylab="Proporción de la Varianza Explicada", 
	 ylim=c(0,.4), type='b', pch=20, col="blue") 
plot(cumsum(Pima_pca$sdev^2/sum(Pima_pca$sdev^2)), xlab="Componente Principal", 
	 ylab="Proporción Acumulada de la Varianza Explicada", 
	 ylim=c(0,1), type='b',pch=20,col="blue") 

# Tasas de Error ----------------------------------------------------------
# Tasas de error por grupos 
tasa_grupos <- function(etq,pred,typ){
	return(mean(etq[etq == typ] != pred[etq == typ])) }
# Tasas de error junto a la global 
tasas_error <- function(etq,pred){
	return(c(mean(etq != pred),
			 sapply(levels(type),function(typ){tasa_grupos(etq,pred,typ)})))}

# Análisis de Discriminante Lineal ----------------------------------------
# 5 modelos
lda_Pima1 <- lda(type~ npreg + glu + bp + skin + bmi + ped + age, Pima)
lda_Pima2 <- lda(type~.^2, Pima)
lda_Pima3 <- lda(type~ npreg + glu + bmi + ped, Pima)
lda_Pima4 <- lda(type~.^3, Pima)
lda_Pima5 <- lda(type~.^4, Pima)

# Lista de modelos
models_lda <- list(lda_Pima1, lda_Pima2, lda_Pima3, lda_Pima4, lda_Pima5)

# Predicciones aparentes
lda_pred <- lapply(models_lda, function(model){predict(model, Pima)})
# Matrices de confusión
lda_tab <- lapply(lda_pred, function(model){
	table(Pima$type, model$class, dnn = c("Real", "Predicho"))})
# Tasas de error
lda_tasas <- lapply(lda_pred, function(model){tasas_error(type, model$class)})

# Naive Bayes -------------------------------------------------------------
# 2 modelos
nB_Pima1 <- naiveBayes(type~ npreg + glu + bp + skin + bmi + ped + age, Pima)
nB_Pima2 <- naiveBayes(type~ npreg + glu + bmi + ped, Pima)

# Lista de modelos
models_nB <- list(nB_Pima1, nB_Pima2)

# Predicciones aparentes
nB_pred <- lapply(models_nB, function(model){predict(model, Pima)})
# Matrices de confusión
nB_tab <- lapply(nB_pred, function(model){
	table(Pima$type, model,dnn = c("Real", "Predicho"))})
# Tasas de error
nB_tasas <- lapply(nB_pred, function(model){tasas_error(type, model)})

# Regresión Logistica -----------------------------------------------------
# 7 modelos principales
reglog_Pima1 <- glm(type~., data = Pima, family = "binomial"(link = "logit"))

# 3 auxiliares (correlacion)
reglog_Pima11 <- glm(type~bp, data = Pima, family = "binomial"(link = "logit"))
reglog_Pima12 <- glm(type~skin, data = Pima, family = "binomial"(link = "logit"))
reglog_Pima13 <- glm(type~age, data = Pima, family = "binomial"(link = "logit"))

# Los 6 principales restantes junto con su optimización usando step()
# AIC
reglog_Pima2 <- step(glm(type~., data = Pima,family = "binomial"(link = "logit")), k = 2, trace = F)
# BIC
reglog_Pima3 <- step(glm(type~., data = Pima, family = "binomial"(link = "logit")), k = log(dim(Pima)[1]), trace=F)
# AIC  int grado 2
reglog_Pima4 <- step(glm(type~.^2, data = Pima, family = "binomial"(link = "logit")), k = 2, trace = F)
# BIC int grado 2
reglog_Pima5 <- step(glm(type~.^2, data = Pima, family = "binomial"(link = "logit")), k = log(dim(Pima)[1]), trace=F)
# AIC  int grado 3
reglog_Pima6 <- step(glm(type~.^3, data = Pima, family = "binomial"(link = "logit")), k = 2, trace = F)
# BIC int grado 3
reglog_Pima7 <- step(glm(type~.^3, data = Pima, family = "binomial"(link = "logit")), k = log(dim(Pima)[1]), trace=F)

# Lista de modelos
models_reglog <- list(reglog_Pima1, reglog_Pima2, reglog_Pima3, reglog_Pima4, reglog_Pima5, reglog_Pima6, reglog_Pima7)

# Predicciones aparentes
pred1 <- sapply(predict(reglog_Pima1,type = 'response'), function(x){if(x>0.5){'Yes'} else{'No'}})
pred2 <- sapply(predict(reglog_Pima2,type = 'response'), function(x){if(x>0.5){'Yes'} else{'No'}})
pred3 <- sapply(predict(reglog_Pima3,type = 'response'), function(x){if(x>0.5){'Yes'} else{'No'}})
pred4 <- sapply(predict(reglog_Pima4,type = 'response'), function(x){if(x>0.5){'Yes'} else{'No'}})
pred5 <- sapply(predict(reglog_Pima5,type = 'response'), function(x){if(x>0.5){'Yes'} else{'No'}})
pred6 <- sapply(predict(reglog_Pima6,type = 'response'), function(x){if(x>0.5){'Yes'} else{'No'}})
pred7 <- sapply(predict(reglog_Pima7,type = 'response'), function(x){if(x>0.5){'Yes'} else{'No'}})

# Lista de predicciones
reglog_pred <- list(pred1, pred2, pred3, pred4, pred5, pred6, pred7)
# Matrices de confusión
reglog_tab <- lapply(reglog_pred, function(model){table(Pima$type, model, dnn = c("Real", "Predicho"))})
# Tasas de error
reglog_tasas <- lapply(reglog_pred, function(model){tasas_error(type, model)})

# SVM ---------------------------------------------------------------------
set.seed(07042020)
# Optimizacion de kernel y seleccion de un modelo por kernel. 3 modelos
# principales
svm_Pima_lin <- tune(svm, type~., data=Pima , kernel ="linear",
					 ranges = list(cost=seq(0.3, 0.4, 0.005)))
svm_Pima_pol <- tune(svm, type~., data=Pima, kernel="polynomial",
					 ranges=list(cost=seq(0.25,0.35, 0.001), degree=c(2:4)))
svm_Pima_rad <- tune(svm, type~., data=Pima,
					 kernel="radial", 
					 ranges=list(cost=seq(0.5, 0.7, 0.005), 
					 			gamma=seq(0.05, 0.3, 0.05)))

# Lista de modelos
models_svm <- list(svm_Pima_lin$best.model, svm_Pima_pol$best.model, svm_Pima_rad$best.model)
# Predicciones aparentes
svm_pred <- lapply(models_svm, function(model){predict(model, Pima)})
# Matrices de confusion
svm_tab <- lapply(svm_pred, function(model){table(Pima$type, model, dnn = c("Real", "Predicho"))})
# Tasas de error
svm_tasas <- lapply(svm_pred, function(model){tasas_error(type, model)})

# Tasas_No_Aparentes ------------------------------------------------------
# Funcion que calcula las tasas no aparentes a partir de los mejores modelos 
# seleccionados. La funcion recibe dos argumentos: repets es el numero de 
# repeticiones para calcular la tasa y modelo es el modelo al cual hay que
# calcularle la tasa

tasas_nap <- function(repets, modelo){
	for (i in 1:repets) {
		# Partimos la base en train (0.75) y test (0.25)
		train <- sample(1:532, 399)
		# matriz que tendrá las tazas en cada iteración
		mat_tasas <- matrix(0, nrow = repets, ncol=3)
		
		# Si elegimos el modelo lda 
		if (modelo =="lda"){
			# Modelo auxiliar creado con train
			modaux<- lda(type~.^3, data=Pima[train,])
			# Calculamos las tasas de error con test
			mat_tasas[i,] <- tasas_error(Pima[-train,]$type, predict(modaux, Pima[-train,])$class)
			
			# Si elegimos el modelo naive Bayes
		} else if (modelo=="naiveBayes"){
			# Modelo auxiliar creado con train
			modaux<- naiveBayes(type~npreg + glu + bmi + ped, data=Pima[train,])
			# Tasa de error con test
			mat_tasas[i,] <- tasas_error(Pima[-train,]$type, predict(modaux, Pima[-train,]))
			
			# Si elegimos regresión logística
		} else if (modelo=="glm"){
			# Modelo auxiliar con train
			modaux <- glm(type ~ npreg + glu + bmi + ped + glu:ped,
						  family = "binomial"(link = "logit"),
						  data = Pima[train,])
			# Calculamos las predicciones con test
			predaux <- sapply(predict(modaux,Pima[-train,],type = 'response'), function(x){if(x>0.5){'Yes'} else{'No'}})
			# Tasa de error con test
			mat_tasas[i,] <- tasas_error(Pima[-train,]$type, predaux)
			
			# Si elegimos svm
		} else if (modelo == "svm"){
			modaux<- svm(type~.,data=Pima[train,], kernel = "radial", cost = svm_Pima_rad$best.parameters["cost"], gamma = svm_Pima_rad$best.parameters["gamma"])
			mat_tasas[i,] <- tasas_error(Pima[-train,]$type, predict(modaux, Pima[-train,]))
		}
	}
	# La función retorna el promedio de los valores que han tomado las tasas en 
	# cada repeticion
	return(colMeans(mat_tasas))
}

# Fijamos semilla
set.seed(07042020)

# Calculamos tasas (300 repeticiones)
lda_aux <-tasas_nap(300, "lda")
nB_aux <- tasas_nap(300, "naiveBayes")
glm_aux <- tasas_nap(300, "glm")
svm_aux <- tasas_nap(300, "svm")

# Tabla_Final -------------------------------------------------------------
tabla_final <- data.frame(rbind(lda_tasas[[4]], lda_aux, nB_tasas[[2]], nB_aux,
								reglog_tasas[[5]], glm_aux, svm_tasas[[3]], 
								svm_aux), 
						  row.names = c("LDA_Aparente", "LDA_No_Aparente", 
						  			  "Naive_Bayes_Aparente", 
						  			  "Naive_Bayes_No_Aparente", 
						  			  "Regresión_Logística_Aparente", 
						  			  "Regresión_Logística_No_Aparente", 
						  			  "SVM_Aparente", "SVM_No_Aparente"))
colnames(tabla_final) <- c("Tasa de Error Global", 
						   "Tasa de Error NO", "Tasa de Error YES")
round(tabla_final, 4)
