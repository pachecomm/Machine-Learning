##Cargamos los datos
data=read.csv("~/FAC/8vo/Sem.Est/Tarea 3/Glucose1.txt")
View(data)

##Para saber cuantos pacientes son de cada clase
sum(data$Class==1) ##=33-overt d
sum(data$Class==2) ##=36-chemical d
sum(data$Class==3) ##=76-normal


str(data) ##vemos que toma Class como _int_, entonces lo pasamos a factor
data$Class<-factor(data$Class)#lo volvemos factor para poder aplicar relevel y que tome el 3 como referencia
data$Class<-relevel(data$Class, ref="3") ##hacemos que el paciente normal sea el de referencia
str(data)
data

##ordenamos
library(tidyverse)
data1=data %>% arrange(Class)
data1

###hacer los subdata frame
d.train=data1[c(1:51,77:98,110:133),]
d.train
#comprobamos que si sean, 51 normal, 22overt y 24chemical
sum(d.train$Class==1) #over
sum(d.train$Class==2)#chemical 
sum(d.train$Class==3)#normal

d.test=data1[c(52:76,99:109,134:145),]
##aquí deben ser 25, 11 y 12
sum(d.test$Class==1) 
sum(d.test$Class==2) 
sum(d.test$Class==3)
###descriptivo
library(nnet)
modelo<-multinom(data$Class~Weight+Fglucose+GlucoseInt+InsulinResp+InsulineResist, data=data)
summary(modelo)
step(modelo)



####predictivo
library(nnet)
modelo2<-multinom(d.train$Class~Fglucose+GlucoseInt+InsulineResist, data=d.train)
summary(modelo2)
### tasas no parentes
p<-predict(modelo2,d.test)##predecimos la clasificación de _d.test_ con el modelo de train
p
dc<-data1[c(52:76,99:109,134:145),]$Class ##vector de clasificación real de esos datos
dc
sum(dc==p) ##saber cuantos coinciden
p2<-predict(modelo2,d.test, type="prob")
round(p2, digits = 5)
cm<-table(p,dc) ##nos dice cuantos predice mal
cm
##error
1-sum(diag(cm))/sum(cm)


###tasas aparentes, entrenas con todos los datos, y pruebas en todos los datos
modelo3<-multinom(data1$Class~Fglucose+GlucoseInt+InsulineResist, data=data1)
summary(modelo3)

p1<-predict(modelo3,data)
dc1<-data$Class
cm1<-table(p1,dc1) ##nos dice cuantos predice mal
cm1

##predecir todos los datos con modelo2
p2<-predict(modelo2,data1)
cm2<-table(p2,dc1)
cm2
#predecir d.test con todos los datos
p3<-predict(modelo3,d.test)
cm3<-table(p3,dc)
cm3
#hasta aquí todo chido
#___________________________________________________________

