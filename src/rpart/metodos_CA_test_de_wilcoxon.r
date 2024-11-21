# Prueba TEST-WILCOXON para las diferentes metodos implementados
# Se va a utilizar NA como base (ya que era el que está por defecto en el WorkFlow)

#Resultados
ML <- c(6.23725, 6.194, 6.06325, 6.0095, 5.95175)
Ninguno <- c(6.1495, 6.2905, 6.307, 6.2945, 6.14925)
EC <- c(5.595, 5.726, 5.58925, 5.682, 5.4755)
MICE <- c(5.77275, 5.91725, 5.97225, 6.012, 5.9785)

C1 <- wilcox.test( ML, Ninguno, paired = TRUE)
print(C1)

C2 <- wilcox.test( ML, EC, paired = TRUE)
print(C2)

C3 <- wilcox.test( ML, MICE, paired = TRUE)
print(C3)

C4 <- wilcox.test( Ninguno, EC, paired = TRUE)
print(C4)

C5 <- wilcox.test( Ninguno, MICE, paired = TRUE)
print(C5)

C6 <- wilcox.test( EC, MICE, paired = TRUE)
print(C6)
