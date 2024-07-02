cat("PRACTICA 1\n")
cat("UNIVERSITARIO: Alan Israel Arnez Flores\n")
cat("RU: 1758342\n")
cat("CI: 13764464 L.P.\n")
cat("=========================================================================\n")
# Instalar paquete necesario si no está instalado
if (!require(jsonlite)) {
  install.packages("jsonlite")
  library(jsonlite)
}

cat("Capitulo 1 ----> VARIABLES ALEATORIAS BIVARIANTES O BIDIMENSIONALES\n")
# -------------------------------------------------------------------------
# Ejercicio 1
# Consideremos una situaci´on en la cu´al se miden la tensi´on superficial (X) y la acidez de un producto
# qu´ımico (Y ). Estas variables son codificadas de tal manera que la tensi´on superficial se mide en una
# escala de 0 a 2, y la acidez se mide en una escala de 2 a 4. La funci´on de densidad conjunta de (X, Y )
# es:
# f(x, y) = (
# k(6 − x − y) , 0 ≤ x ≤ 2, 2 ≤ y ≤ 4
# 0 , e.o.c.
# a) Hallar el valor de k, que haga que f(x, y) una densidad conjunta.
# b) Calcular P(X < 1, Y > 3)
# c) Hallar las densidades marginales de X e Y .
# d) Calcular P(X < 1.5), P(Y < 2.5) y P(X + Y ≤ 4)
# SOLUCION EJERCICIO 1
cat("\nEJERCICIO 1\n")
cat("===============\n")
# Definir la función de densidad conjunta
f_xy <- function(x, y) {
  k <- 1/8
  # Usar condiciones vectorizadas en lugar de operaciones lógicas directas
  cond <- (0 <= x & x <= 2 & 2 <= y & y <= 4)
  return(ifelse(cond, k * (6 - x - y), 0))
}

# Calcular la constante k
k <- 1/8

# Parte b: P(X < 1, Y > 3)
p_b <- integrate(function(x) {
  sapply(x, function(x) {
    integrate(function(y) f_xy(x, y), 3, 4)$value
  })
}, 0, 1)$value

# Parte c: Densidades marginales
f_x <- function(x) {
  sapply(x, function(x) {
    integrate(function(y) f_xy(x, y), 2, 4)$value
  })
}

f_y <- function(y) {
  sapply(y, function(y) {
    integrate(function(x) f_xy(x, y), 0, 2)$value
  })
}

# Parte d: Probabilidades marginales y conjuntas
p_x_1_5 <- integrate(f_x, 0, 1.5)$value
p_y_2_5 <- integrate(f_y, 2, 2.5)$value

p_x_y_4 <- integrate(function(x) {
  sapply(x, function(x) {
    integrate(function(y) f_xy(x, y), 2, 4 - x)$value
  })
}, 0, 2)$value

# Imprimir los resultados
cat("a) k =", k, "\n")
cat("b) P(X < 1, Y > 3) =", p_b, "\n")
cat("c) Densidad marginal de X: f_X(x) =", "3 - x / 4", "\n")
cat("   Densidad marginal de Y: f_Y(y) =", "5 - y / 4", "\n")
cat("d) P(X < 1.5) =", p_x_1_5, "\n")
cat("   P(Y < 2.5) =", p_y_2_5, "\n")
cat("   P(X + Y <= 4) =", p_x_y_4, "\n")
# Ejercicio 2
# Para una variable aleatoria bidimensional (X, Y ), se define la siguiente funci´on de probabilidad conjunta:
# p(x, y) = k(x + 2y) x = 1, 2, ..., 8. y = 1, 2, ..., 5.
# Determinar la constante k y el valor esperado de cada variable.
cat("EJERCICIO 2\n")
cat("===============\n")
# Calcular la constante k
x_vals <- 1:8
y_vals <- 1:5
sum_prob <- sum(sapply(x_vals, function(x) sapply(y_vals, function(y) x + 2*y)))
k <- 1 / sum_prob
cat("Constante k =", k, "\n")

# Calcular el valor esperado de X
E_X <- sum(sapply(x_vals, function(x) {
  sapply(y_vals, function(y) {
    x * k * (x + 2 * y)
  })
}))
cat("Valor esperado de X (E[X]) =", E_X, "\n")

# Calcular el valor esperado de Y
E_Y <- sum(sapply(x_vals, function(x) {
  sapply(y_vals, function(y) {
    y * k * (x + 2 * y)
  })
}))
cat("Valor esperado de Y (E[Y]) =", E_Y, "\n")
# Ejercicio 3
# Suponga que X e Y , las proporciones de un d´ıa de trabajo de 8 horas que dos dependientes de una
# gasolinera realmente ocupan en desempe˜nar sus labores asignadas, tienen la densidad de probabilidad
# conjunta:
# f(x, y) = x + y para 0 ≤ x ≤ 1 0 ≤ y ≤ 1
# Si la proporci´on D de “tiempo de descanso” para los dos dependientes est´a dada por la expresi´on:
# D = 1 −
# X + Y
# 2
# Calcular E(D) y V [D]
cat("EJERCICIO 3\n")
cat("===============\n")
# Definir la densidad conjunta
f_xy <- function(x, y) {
  return(x + y)
}

# Calcular E(X)
E_X <- integrate(function(x) {
  sapply(x, function(x) {
    integrate(function(y) x * f_xy(x, y), 0, 1)$value
  })
}, 0, 1)$value

# Calcular E(Y)
E_Y <- integrate(function(y) {
  sapply(y, function(y) {
    integrate(function(x) y * f_xy(x, y), 0, 1)$value
  })
}, 0, 1)$value

# Calcular E(XY)
E_XY <- integrate(function(x) {
  sapply(x, function(x) {
    integrate(function(y) x * y * f_xy(x, y), 0, 1)$value
  })
}, 0, 1)$value

# Calcular E(X^2)
E_X2 <- integrate(function(x) {
  sapply(x, function(x) {
    integrate(function(y) x^2 * f_xy(x, y), 0, 1)$value
  })
}, 0, 1)$value

# Calcular E(Y^2)
E_Y2 <- integrate(function(y) {
  sapply(y, function(y) {
    integrate(function(x) y^2 * f_xy(x, y), 0, 1)$value
  })
}, 0, 1)$value

# Valores esperados y varianza
E_X <- E_X
E_Y <- E_Y
E_XY <- E_XY
E_X2 <- E_X2
E_Y2 <- E_Y2

# Calcular varianzas y covarianza
Var_X <- E_X2 - E_X^2
Var_Y <- E_Y2 - E_Y^2
Cov_XY <- E_XY - E_X * E_Y

# Calcular E(D) y V(D)
E_D <- 1 - 0.5 * (E_X + E_Y)
Var_D <- 0.25 * (Var_X + Var_Y + 2 * Cov_XY)

# Imprimir resultados
cat("E(X) =", E_X, "\n")
cat("E(Y) =", E_Y, "\n")
cat("E(D) =", E_D, "\n")
cat("V(D) =", Var_D, "\n")
# Ejercicio 4
# Si X e Y son variables aleatorias normales independientes con:
# µX = 1 σ
# 2
# X = 9 µY = 5 σ
# 2
# Y = 4
# Calcule:
# a) E[5X − Y ]
# b) V [5X − Y ]
# c) P(5X − Y > 3)
cat("EJERCICIO 4\n")
cat("===============\n")
# Definir parámetros
mu_X <- 1
sigma2_X <- 9
mu_Y <- 5
sigma2_Y <- 4

# Calcular E[5X - Y]
E_5X_minus_Y <- 5 * mu_X - mu_Y

# Calcular V[5X - Y]
V_5X_minus_Y <- 25 * sigma2_X + sigma2_Y

# Calcular P(5X - Y > 3)
sigma_Z <- sqrt(V_5X_minus_Y)
prob <- 1 - pnorm(3 / sigma_Z)

# Imprimir resultados
cat("a) E[5X - Y] =", E_5X_minus_Y, "\n")
cat("b) V[5X - Y] =", V_5X_minus_Y, "\n")
cat("c) P(5X - Y > 3) =", prob, "\n")

# Ejercicio 5
cat("EJERCICIO 5\n")
cat("===============\n")
# Parámetros
n <- 10
p_r <- 0.3
p_n <- 0.2

# Función de probabilidad conjunta
P_joint <- function(x, y) {
  dbinom(x, size = n, prob = p_r) * dbinom(y, size = n, prob = p_n)
}

# Calcular probabilidades conjuntas para todos los posibles valores de x y y
joint_probabilities <- matrix(0, nrow = n + 1, ncol = n + 1)
for (x in 0:n) {
  for (y in 0:n) {
    joint_probabilities[x + 1, y + 1] <- P_joint(x, y)
  }
}

# Función de distribución marginal de X
P_X <- function(x) {
  dbinom(x, size = n, prob = p_r)
}

# Función de distribución marginal de Y
P_Y <- function(y) {
  dbinom(y, size = n, prob = p_n)
}

# Calcular las distribuciones marginales para todos los posibles valores de x y y
marginal_X <- dbinom(0:n, size = n, prob = p_r)
marginal_Y <- dbinom(0:n, size = n, prob = p_n)

# Imprimir resultados
cat("Función de probabilidad conjunta P(X, Y):\n")
print(joint_probabilities)

cat("Distribución marginal de X:\n")
print(marginal_X)

cat("Distribución marginal de Y:\n")
print(marginal_Y)

cat("\nCapitulo 2 ----> DISTRIBUCIONES MUESTRALES\n")
# Ejercicio 6
# Las alturas de 5000 estudiantes son normalmente distribuidas con media 172 cm y desviaci´on est´andar
# de 7.5 cm. Si fueron obtenidas 100 muestras con 36 estudiantes cada una, en cu´antas muestras se puede
# esperar que la media muestral se encuentre.
# a) entre 169 y 174
# b) superior a 170
# Suponga que el muestreo es sin reemplazamiento.
cat("\nEJERCICIO 6\n")
cat("===============\n")

# Parámetros
mu <- 172
sigma <- 7.5
n <- 36
k <- 100

# Desviación estándar de la media muestral
sigma_x_bar <- sigma / sqrt(n)

# a) Probabilidad de que la media muestral esté entre 169 y 174
z_169 <- (169 - mu) / sigma_x_bar
z_174 <- (174 - mu) / sigma_x_bar

prob_169_174 <- pnorm(z_174) - pnorm(z_169)
num_samples_169_174 <- k * prob_169_174

# b) Probabilidad de que la media muestral sea superior a 170
z_170 <- (170 - mu) / sigma_x_bar

prob_above_170 <- 1 - pnorm(z_170)
num_samples_above_170 <- k * prob_above_170

# Imprimir resultados
cat("a) Número de muestras con media muestral entre 169 y 174:", num_samples_169_174, "\n")
cat("b) Número de muestras con media muestral superior a 170:", num_samples_above_170, "\n")
# Ejercicio 7
# El coeficiente intelectual de los estudiantes se distribuye normalmente con media 100 y desviaci´on
# t´ıpica 11.
# a) Si elegimos una persona al azar calcular la probabilidad de que su CI est´e entre 100 y 103.
# b) Se elige al azar una muestra de 25 personas. Calcular la probabilidad de que la media de sus
# coeficientes intelectuales est´e entre 100 y 103.
cat("EJERCICIO 7\n")
cat("===============\n")
# Parámetros
mu <- 100
sigma <- 11
n <- 25
sigma_x_bar <- sigma / sqrt(n)

# a) Probabilidad de que el CI esté entre 100 y 103 para una persona al azar
z_100_a <- (100 - mu) / sigma
z_103_a <- (103 - mu) / sigma

prob_100_103_a <- pnorm(z_103_a) - pnorm(z_100_a)

# b) Probabilidad de que la media de una muestra de 25 personas esté entre 100 y 103
z_100_b <- (100 - mu) / sigma_x_bar
z_103_b <- (103 - mu) / sigma_x_bar

prob_100_103_b <- pnorm(z_103_b) - pnorm(z_100_b)

# Imprimir resultados
cat("a) Probabilidad de que el CI esté entre 100 y 103:", prob_100_103_a, "\n")
cat("b) Probabilidad de que la media de 25 CI esté entre 100 y 103:", prob_100_103_b, "\n")
# Ejericio 8
# Si la desviaci´on est´andar del peso de los ni˜nos del Hospital del ni˜no es de 2.5 kgr., ¿cu´al es la probabilidad de que el peso medio de una muestra al azar de 100 de estos ni˜nos, difieran en m´as de medio
# kilogramo, con respecto al peso medio para todos los ni˜nos del hospital?
cat("EJERCICIO 8\n")
cat("===============\n")
# Parámetros
sigma <- 2.5
n <- 100
sigma_x_bar <- sigma / sqrt(n)
threshold <- 0.5

# Calcular Z-scores
z_upper <- threshold / sigma_x_bar
z_lower <- -threshold / sigma_x_bar

# Probabilidad P(|X_bar - mu| > 0.5)
prob_upper <- 1 - pnorm(z_upper)
prob_lower <- pnorm(z_lower)

# Sumar las probabilidades de los dos eventos
prob_diff_more_than_0.5 <- prob_upper + prob_lower

# Imprimir resultados
cat("La probabilidad de que el peso medio difiera en más de 0.5 kg del peso medio poblacional es:", prob_diff_more_than_0.5, "\n")
# Ejericio 9
# En cierta poblaci´on de alcoh´olicos, la duraci´on promedio del abuso del alcohol es de 12 a˜nos y la
# desviaci´on est´andar de 6 a˜nos. ¿Cu´al es la probabilidad de que una m.a. de 36 individuos de esta
# poblaci´on tenga una duraci´on promedio de abuso del alcohol entre 10 y 11 a˜nos?
cat("EJERCICIO 9\n")
cat("===============\n")
# Parámetros
mu <- 12
sigma <- 6
n <- 36
sigma_x_bar <- sigma / sqrt(n)

# Estandarización de los valores
z_10 <- (10 - mu) / sigma_x_bar
z_11 <- (11 - mu) / sigma_x_bar

# Cálculo de probabilidades
prob_10_11 <- pnorm(z_11) - pnorm(z_10)

# Imprimir resultados
cat("La probabilidad de que la media muestral esté entre 10 y 11 años es:", prob_10_11, "\n")
# Ejercicio 10 
#  La media de una distribuci´on muestral de medias es 50, y su desviaci´on est´andar es 10. Suponga que
# la distribuci´on de la poblaci´on original es normal.
# a) ¿Qu´e porcentaje de las medias de la muestra estar´a entre 45 y 55?
# b) ¿Qu´e porcentaje de los valores medios de la muestra ser´a menor que la media de la poblaci´on?
cat("EJERCICIO 10\n")
cat("===============\n")
# Parámetros
mu_x_bar <- 50
sigma_x_bar <- 10

# a) Probabilidad de que la media muestral esté entre 45 y 55
z_45 <- (45 - mu_x_bar) / sigma_x_bar
z_55 <- (55 - mu_x_bar) / sigma_x_bar

prob_45_55 <- pnorm(z_55) - pnorm(z_45)

# b) Probabilidad de que la media muestral sea menor que la media de la población
z_50 <- (50 - mu_x_bar) / sigma_x_bar

prob_less_than_mean <- pnorm(z_50)

# Imprimir resultados
cat("a) Porcentaje de las medias de la muestra entre 45 y 55:", prob_45_55 * 100, "%\n")
cat("b) Porcentaje de las medias de la muestra menor que la media de la población:", prob_less_than_mean * 100, "%\n")
