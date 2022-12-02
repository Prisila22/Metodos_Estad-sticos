#Analisis de varianza
#ANOVA
#05/10/2022

library(repmis)

paraje <- source_data("https://www.dropbox.com/s/fbrwxypacjgeayj/Datos_Rascon_Anova.csv?dl=1")

tapply(paraje$DAP, paraje$Paraje, mean)
tapply(paraje$DAP, paraje$Paraje, sd)
tapply(paraje$DAP, paraje$Paraje, median)
tapply(paraje$DAP, paraje$Paraje, var)
tapply(paraje$DAP, paraje$Paraje, length)

boxplot(paraje$DAP ~ paraje$Paraje)
boxplot(paraje$DAP ~ paraje$Paraje,
        xlab = "Paraje",
        ylab = "DAP (cm)",
        col = "green")

head(paraje)

#la fuente de variación hará dif la variable que queremos medir(los 4 lugares)
#variabilidad interna(lo que hay dentro de cada lugar) y externa(los lugares)
#los tratamientos son la fuente de variacion (los 4 lugares o paraje) 
#y ocupamos el error por si acaso (que no lo puedo explicar)
#grados de libertad (tratamiento x repeticiones -1) (n-1)

#homogeneidad de varianzas barlett.test
bartlett.test(paraje$DAP, paraje$Paraje)
# H0 nula = las varianzas son homogeneas (.05 o + en p-value) normales
# H1 alternativa = las varianzas no son homogeneas (.05 o - en p-value) no normales

# Normalidad de la variable Diámetro

shapiro.test(paraje$DAP)
hist(paraje$DAP)
#esta sesgado a la izq, no tiene forma acampanada, y tiene H1 no normal el p-value

library(dplyr)
chinatu <- paraje %>%
  filter(Paraje == "Chinatu")

shapiro.test(chinatu$DAP)
#son datos normales pq p-value es mas de .05

trinidad <- paraje %>%
  filter(Paraje == "Trinidad")

shapiro.test(trinidad$DAP)
#son datos normales pq p-value es mas de .05
#si casi todos son normales si hacemos el analisis de varianza

laguna <- paraje %>%
  filter(Paraje == "Laguna")

shapiro.test(laguna$DAP)

tule <- paraje %>%
  filter(Paraje == "Tule")

shapiro.test(tule$DAP)
hist(tule$DAP)
#el sitio tule da problem pq no me da p-value normal, pero 
#los otros 3 tan bien, asi q si hacemos el analisis de varianza
#si todos estuvieran mal tendriamos que modificar los datos

#analisis de varianza
par.aov <- aov (paraje$DAP ~ paraje$Paraje)
summary(par.aov)

#grados de libertad(fuentes de variacion menos 1, así que si son 4 lugares, da 3)
#residuales(n- el # de tratamientos, da 116)
#total es la sumatoria(119)
#suma de cuadrados 9892, luego eso entre 3 es mean SQ
#F = 3297/90 = 36.51
#si el error es mas grande que el tratamiento, no hay diferencias
#prob. F si hay diferencias pq tengo un valor menos que .05 en Pr(>F)

                 Df Sum Sq Mean Sq F value Pr(>F)    
paraje$Paraje     3   9892    3297   36.51 <2e-16 ***
  Residuals     116  10476      90                   
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#este analisis de vrnza (aov o anova) me dice si hay o no de difrncias en los sitios
  #si si hay, hacemos prueba de Tukey, dice cuales son dif 
    #en que sitio es mejor el diametro?

TukeyHSD(par.aov)
#si pasa de .05 el "p adj" no hay diferencia
#si hay valores altos de dif en cm puede que si haya dif pq sign que esta muy alejado

                    diff(cm)     lwr         upr       p adj
Laguna-Chinatu    -6.7866667 -13.1825132  -0.3908202 0.0329929 #aqui hay dif
Trinidad-Chinatu  17.5766667  11.1808202  23.9725132 0.0000000 #aqui hay dif
Tule-Chinatu      -0.6533333  -7.0491798   5.7425132 0.9933587
Trinidad-Laguna   24.3633333  17.9674868  30.7591798 0.0000000 #aqui hay dif
Tule-Laguna        6.1333333  -0.2625132  12.5291798 0.0652237
Tule-Trinidad    -18.2300000 -24.6258465 -11.8341535 0.0000000 #aqui hay dif

plot(TukeyHSD(par.aov))
#si toca la linea del 0 no hay diferencia, si no la toca es diferente
#si esto pasa en un reporte, escribimos que hay dif y ponemos tabla
#letras dif entre ellos si es que hay dif

a    ab     abc     ab





