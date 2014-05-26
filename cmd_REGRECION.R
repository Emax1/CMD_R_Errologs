-------------------------------------------------------------------------------------------------------
#* leer datos  *#
-------------------------------------------------------------------------------------------------------
datos<-read.delim('clipboard',T,sep='',dec=',')
attach(datos)
detach(datos)
names(datos)
str(datos)

-------------------------------------------------------------------------------------------------------
#*  Modelos  *#
-------------------------------------------------------------------------------------------------------
mdl1<-lm(datos[,1] ~ datos[,2] )
#m1<-lm(y~x)
m1<-lm(Tiempo ~ Temperatura )
#Grafico de Disperción plot(x,y)
plot(Temperatura,Tiempo)
#Tamaño, modificar Escala
plot(x,y,xlim=c(0,250),ylim=c(0,250))


-------------------------------------------------------------------------------------------------------
#*  ec recta  *#
-------------------------------------------------------------------------------------------------------
m1
Coefficients:
(Intercept)  Temperatura 

abline(m1)

-------------------------------------------------------------------------------------------------------
#* Matriz de Dispersión
-------------------------------------------------------------------------------------------------------

pairs(Puntuacion~Peso2 + Talla2 + Peso9 + Talla9 + Diametro9 + Fuerza9 + Peso18 + Talla18 + Diametro18 + Fuerza18 + Puntuacion)




-------------------------------------------------------------------------------------------------------
#summary
-------------------------------------------------------------------------------------------------------
summary(m1)
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 2184.003     75.022   29.11 1.45e-08 ***
Temperatura  -54.459      3.015  -18.06 3.94e-07 ***

Multiple R-squared:  0.979
 7 DF


-------------------------------------------------------------------------------------------------------
#intervalo de Confianza del intercepto y la pendiente (B0 y B1)
-------------------------------------------------------------------------------------------------------
confint(m1,level=.96)
                  2 %                                      98 %
(Intercept) 	1995.19169   	<= 	B0 	<= 	2372.81440
Temperatura  	-62.04713 	<=	B1 	<=	-46.87055




-------------------------------------------------------------------------------------------------------
#Hipotesis
-------------------------------------------------------------------------------------------------------
#B0
#paso1
#H0: B0<=2100
#H1: B0>2100 hipotesis alternativa (del Investigador)
#paso2
#alfa=0.05
#paso3  T CALCULAD
tc<-function(Bestiamdo,Bhipotetico,ErrorEstandar){
tc<-(Bestiamdo-Bhipotetico)/ErrorEstandar
return(tc)
tc
}
tcalculado<-tc(2184.003,2100,75.002)
#Lower.tail=FALSE  para mayor
pt(tcalculado,df= ,lower.tail=FALSE)
0.1498305
Como el pvalor obtnido es mayor que el alfa se acepta la hipotesis H0 (Nula)
Rechazamos la hipotes Alternativa


#B1  pendiente
#paso1
#ojo palabra disminuye significa menos
H0: B1<=-55
H1: B1>-55
#paso2
alfa=0.05
#paso3
tc<-function(Bestiamdo,Bhipotetico,ErrorEstandar){
tc<-(Bestiamdo-Bhipotetico)/ErrorEstandar
return(tc)
tc
}
tcalculado<-tc(-54.459,-55,3.015)
#Lower.tail=TRUE  para menor
pt(tcalculado,df= ,lower.tail=TRUE)
0.5686606
Como el pvalor obtnido es mayor que el alfa se acepta la hipotesis H0



-------------------------------------------------------------------------------------------------------
###Pronosticar o predecir
##confidence ->valor promedio
-------------------------------------------------------------------------------------------------------
predict(m1,data.frame(Temperatura=30),level=0.95,interval="confidence")
##prediction ->valor puntual
predict(m1,data.frame(Temperatura=25),level=0.94,interval="prediction")


-------------------------------------------------------------------------------------------------------
##Grafico de Residuales
-------------------------------------------------------------------------------------------------------
gfresi<-function(y,x1){
par(mfrow=c(2,2))
m1<-lm(y~x1)
plot(x1,y,main="a).")
abline(m1)
plot(x1,residuals(m1),main="b). REsidial")
abline(h=0, lty=2)
m2<-lm(log(y)~x1)
plot(x1,log(y),main="c).Transformada")
abline(m2)
plot(x1,residuals(m2),main="d).Residual")
abline(h=0,lty=2)
}
gfresi(Puntuacion,Peso9)
#para mejorar la relación lineal verifiacar que los residuales esten disperso
#para aplicar la transformacipon como el logaritmo 
#si los residuales tienen un orden no se realiza la transformacipon



resiEs<-function(y,x1){
m1<-lm(y~x1)
plot(fitted(m1),residuals(m1))
abline(lm(residuals(m1)~fitted(m1)))
abline(v=0, h=0,lty=2,col='red')
}
resiEs(Presion,Temperatua)
#-12 el registro 12
resiES(Presion[-12],Temperatua[-12])

-------------------------------------------------------------------------------------------------------
##grafico de la variable añadida
-------------------------------------------------------------------------------------------------------
m01<-lm(y~x1+x2)
m02<-lm(y~x3)
m03<-lm(x3~x2+x1)
plot(residuals(m03),residuals(m1))
m05<-lm(residuals(m01)~residuals(m3))
abline(m05)
m05

y=x1+x2

vadd2<-function(y,x1,x2){
par(mfrow=c(2,2))
m1<-lm(y~x1)
plot(x1,y,main="a).")
abline(m1)
m2<-lm(y~x2)
plot(x2,y,main="b).")
abline(m2)
m3<-lm(x2~x1)
plot(x1,x2,main="c).")
abline(m3)
m4<-lm(residuals(m1)~residuals(m3))
plot(residuals(m3),residuals(m1),main="d).",xlab="é (x2 vs x1)",ylab="(é y vs x1)")
abline(m4,col="red")
abline(h=0,v=0,lty=2, col='blue')
}
vadd2(Puntuacion,Peso9,Diametro9)



###Si se observa una tendencia en el grafico de variable añadida d)
#mayor que el grafico acomparar b), entonces vale la pena añadir
#la variable en evaluación

#*  O  *#
install.packages("car", dependencies=T) 
library(car)
#m1 modelo 
m1<-lm(y+x1+x2+x3)
#evalua todo los casos para la variable añadida
#buscamos la nuestro interes
avPlots() 


-------------------------------------------------------------------------------------------------------
##Matriz de Correlación
-------------------------------------------------------------------------------------------------------
round(cor(cbind(log(Fertilidad),log2(PBIpp),Purban)),4)
round(cor(cbind(y,x1,x2,x3)),4)



-------------------------------------------------------------------------------------------------------
##Matriz de Covarianza
-------------------------------------------------------------------------------------------------------
round(cov(cbind(log(Fertilidad),log2(PBIpp),Purban)),4)
round(cor(cbind(y,x1,x2,x3)),4)

#X representa todos los términos en la función media.
Intercepto=rep(1,51)
X=cbind(Intercepto,Impuesto,TasaLic,Ingreso,LogMillas)
round(solve(t(X)%*%X),6)

-------------------------------------------------------------------------------------------------------
###ANOVA RESUMIDO O GENERAL
-------------------------------------------------------------------------------------------------------
a<-anova(lm(TasaComb~X))
print(a,signif.stars=FALSE)

-------------------------------------------------------------------------------------------------------
##Analisis de Varianza (verifica que tan significativa son las variables dentro del modelo)
-------------------------------------------------------------------------------------------------------
#analizamos los ***
summary(m1)
#mas resumidos
avona(m1)


-------------------------------------------------------------------------------------------------------
#Prueba t y gráficos de variable añadida
-------------------------------------------------------------------------------------------------------



-------------------------------------------------------------------------------------------------------
#Prueba F Parcial
-------------------------------------------------------------------------------------------------------
m1<-lm(y~x1+x2+x3)
m2<-lm(y~x1+x2)
print(anova(m2,m1),signif.stars=FALSE)




-------------------------------------------------------------------------------------------------------
Tablas de análisis de variancia secuenciales (Tablas de análisis de variancia con diferente orden de estimación)
#Significancia de la variable dentro del modelo con relación de las otras
#es importante la ubicación
-------------------------------------------------------------------------------------------------------
m1<-lm(y~x1+x2+x3)
m1<-lm(y~x3+x1+x1)
print(anova(m1)[,1:3],signif.start=FALSE)
print(anova(m2)[,1:3],signif.start=FALSE)




-------------------------------------------------------------------------------------------------------
Use la prueba de Anderson­Darling para verificar que los errores tienen distribución normal. 
Busque la función correspondiente para obtener los resultados con R. 
-------------------------------------------------------------------------------------------------------

modelo12<­lm(log2(edad)~altura+ log2(pesosinconcha) +log2( pesoconcha)) 

residualesmodelo12<­residuals(modelo12) 
library(nortest) 
ad.test(residualesmodelo12) 
#observar el p­value < 2.2e­16 
#si es mayor igual a 0.05 en normal caso contrario no lo es

-------------------------------------------------------------------------------------------------------
Obtener un intervalo de confianza al 97% para el valor medio de cada una de las variables 
predictoras. 
-------------------------------------------------------------------------------------------------------
x1<­altura 
x2<­log2(pesosinconcha) 
x3<­log2( pesoconcha) 
 
Modelo planteado final 
mean(x1)=0.1395164 
mean(x2)=­1.862397 
mean(x3)=­2.411909 
modelo12<­lm(log2(edad)~x1+ x2 +x3) 

 
predict(modelo12, data.frame(x1 = 0.1395, x2 = ­1.862397, x3 = ­2.411909),level = 0.97, interval = "confidence") 

-------------------------------------------------------------------------------------------------------
Obtener el intervalo de predicción al 92% para el valor mediano de cada una de las variables 
predictoras. 
------------------------------------------------------------------------------------------------------- 
median(x1) 
median(x2) 
median(x3) 
 
predict(m12, data.frame(x1 = 0.14, x2 = ­1.573467, x3 = ­2.09542),level = 0.92, interval = "confidence") 
#al final entre estas dos prediciones se puede conluir cual de las dos tiene menor amplitud es mas preciso
#Calculamos la direncia
