#Examen 2
#Metodos Estadadisticos
#Prisila Hurtado Elizondo
#26/10/22
#                   Ejercicio 1
InsectSprays
#1-. tiene 6 tipos de insecticida
#2-. tiene 12 observaciones cada tipo de insecticida
#3-. tiene 72 datos (n) el experimento
boxplot(InsectSprays$count ~ InsectSprays$spray,
        xlab = "spray",
        ylab = "count",
        col = "Red")
#                   Ejercicio 2
#4-. sugiere, que de los 6 insecticidas, 3 muestran valores altos, y 3 muestran valores bajos
#5-. Si
#6-. en el A, B y F, en comparación al C,D y E en sus valores
#7-. el C y el D
#8-. el C a simple vista por estar más abajo que todos
#tapply
tapply(InsectSprays$count, InsectSprays$spray, median)
tapply(InsectSprays$count, InsectSprays$spray, var)
#9-. la media de cada ins es: A= 14.0, B= 16.5,  C= 1.5,  D= 5,  E= 3, y el F= 15.
#10-. la var de cada ins es: A=22.272727, B=18.242424, C=3.901515, D=6.265152, E=3.000000, y el F=38.606061.
#11-. presenta mayor variación el F
#12-. presenta menor variación el E

#ANOVA
par.aov <- aov (InsectSprays$count ~ InsectSprays$spray)
summary(par.aov)
#13-. H0= que la mayoría de los insecticidas no tengan valores bajos, mostrando ser ineficaces 
#     H1= que la mayoría de los insecticidas sean buenos contra los insectos, mostrando valores bajos
#14-. tiene 5 grados de libertad
#15-. tiene 66 grados de libertad residuales
#16-. tiene 71 grados de libertad totales
#17-. la suma de cuadrados del tratamiento es 2669
#18-. el valor de p es 34.7
#19-. el valor de probabilidad de F es <2e-16 
#20-. si
#21-. aplicamos la prueba de Tukey
#                   Ejercicio 3
TukeyHSD(par.aov)
plot(TukeyHSD(par.aov))
#22-. arroja 15 comparaciones
#23-. si existen dif sign en el 2,3,4,6,7,8,12,14,15
#24-. no existen dif sign en el 1,5,9,10,11,13
#25-. el C, ya que tiene la media más baja, y mientras menor sea el número de insectos, más efectivo es el insecticida.

