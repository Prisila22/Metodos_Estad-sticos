#Examen 3
#Metodos Estadadisticos
#Prisila Hurtado Elizondo
#30/11/22

sueloz <- read.csv("https://raw.githubusercontent.com/Prisila22/Metodos_Estad-sticos/main/su.csv")
library(repmis)

#ejercicio 1
boxplot(sueloz$Nem ~ sueloz$Suelo,
        xlab = "Suelo",
        ylab = "Nematodos",
        col = "Blue")
#se observa que son muy variados y muestran datos fuera de lugar,
#así como que 3 tipos de suelo son casi parecidos y otros muy diferentes

#tapply 
tapply(sueloz$Nem, sueloz$Suelo, var)
#es 6.28 veces la diferencia entre la varianza mas pequeña y la mas grande
tapply(sueloz$Nem, sueloz$Suelo, median)
#ANOVA
par.aov <- aov (sueloz$Nem ~ sueloz$Suelo)
summary(par.aov)
#H0= que la mayoría de los suelos no tengan valores altos
#H1= que la mayoría de los suelos tengan buen contenido de nematodos, mostrando valores altos
#el valor estadístico de contraste (F) =0.000207, grados de libertad del factor 4, 
#grados de libertado residuales 20, y valor de P= 9.287

TukeyHSD(par.aov)
plot(TukeyHSD(par.aov))
#hay diferencia significativa en el 3,6 y 10
bartlett.test(sueloz$Nem, sueloz$Suelo)
# el p-value da .5444, mostrando que a pesar de la gran variacion los datos son homogeneos en varianza


#ejercicio 2
riegoz <- read.csv("https://raw.githubusercontent.com/Prisila22/Metodos_Estad-sticos/main/exx.csv")
boxplot(riegoz$Crecimiento ~ riegoz$Riego,
        xlab = "Riego",
        ylab = "Crecimiento",
        col = "Blue")
# se observa que dos se parecen y uno se encuentra más apartado, pero ninguno muestra datos fuera de lugar
#tapply 
tapply(riegoz$Crecimiento, riegoz$Riego, var)
#es 8.4 veces la diferencia entre la varianza mas pequeña y la mas grande
tapply(riegoz$Crecimiento, riegoz$Riego, median)
#ANOVA
par.aov <- aov (riegoz$Crecimiento ~ riegoz$Riego)
summary(par.aov)
#H0= que la mayoría de los suelos no tengan valores altos
#H1= que la mayoría de los suelos tengan buen contenido de nematodos, mostrando valores altos
#el valor estadístico de contraste (F) =6.84e-09, grados de libertad del factor 2, 
#grados de libertado residuales 15, y valor de P= 84.48

bartlett.test(riegoz$Crecimiento, riegoz$Riego)
# el p-value da .03658, mostrando que  los datos no son homogeneos en varianza
TukeyHSD(par.aov)
plot(TukeyHSD(par.aov))
#hay diferencia significativa en los 3


