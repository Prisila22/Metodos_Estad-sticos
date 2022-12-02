#Examen 1
#Metodos Estadadisticos
#Prisila Hurtado Elizondo
#12/10/22

sueloz <- read.csv("https://raw.githubusercontent.com/Prisila22/Metodos_Estad-sticos/main/su.csv")
library(repmis)
boxplot(sueloz$Nem ~ sueloz$Suelo,
        xlab = "Suelo",
        ylab = "Nematodos",
        col = "Red")

#tapply 
tapply(sueloz$Nem, sueloz$Suelo, var)
#    S1    S2    S3    S4    S5 
#  571.7 302.7 285.8 189.3  90.8  
#la dif entre la mas pequeña y la más grande es de 480.9 (571.7 a 90.8), 
#es decir, 6.29 veces más

#ANOVA
par.aov <- aov (sueloz$Nem ~ sueloz$Suelo)
summary(par.aov)
#               Df Sum Sq  Mean Sq   F value   Pr(>F)    
# sueloz$Suelo  4  10701   2675.2     9.287     0.000207 ***
# Residuals    20   5761   288.1                     
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#hipotesis nula:
#hipotesis alternativa:
#valor de p = 0.000207
#grados de libertad = 4
#grados de libertad residuales = 20
#contraste (f) =  9.287

#shapiro
shapiro.test(sueloz$Nem)
#Shapiro-Wilk normality test
#   data:  sueloz$Nem
#  W = 0.96032, p-value = 0.421
#son datos normales

bartlett.test(sueloz$Nem, sueloz$Suelo)
# a pesar de la gran variacion los datos son homogeneos en varianza

#Se observa una diferencia significativa en el S4,
#en valor promedio tanto como variabilidad con los demás grupos, también 
#en el valor promedio de S1, y en la variabilidad del S5.

#turkey
TukeyHSD(par.aov)
# Tukey multiple comparisons of means
# 95% family-wise confidence level
# Fit: aov(formula = sueloz$Nem ~ sueloz$Suelo)
# $`sueloz$Suelo`
#        diff         lwr        upr     p adj
# S2-S1  -8.0 -40.1208794  24.120879 0.9429980
# S3-S1 -18.4 -50.5208794  13.720879 0.4481002
# S4-S1 -48.4 -80.5208794 -16.279121 0.0017871 si hay dif
# S5-S1  12.8 -19.3208794  44.920879 0.7555248
# S3-S2 -10.4 -42.5208794  21.720879 0.8658492
# S4-S2 -40.4 -72.5208794  -8.279121 0.0095500 si hay dif
# S5-S2  20.8 -11.3208794  52.920879 0.3307073
# S4-S3 -30.0 -62.1208794   2.120879 0.0743745
# S5-S3  31.2  -0.9208794  63.320879 0.0595156
# S5-S4  61.2  29.0791206  93.320879 0.0001237 si hay dif

#tabla turkey
plot(TukeyHSD(par.aov))
# Si existen diferencias en 3 comparaciones, 
# y esas 3 comparaciones están involucradas con el "S4" (s4-s1, s4-s2, s5-s4), 
# en la tabla esos 3 no tocan la línea del 0


