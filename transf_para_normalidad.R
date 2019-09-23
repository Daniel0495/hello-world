datos <- read.csv2(file.choose(), header = T, sep=";")

precio <- bdapartamentos$`precio por metro millones`
summary(precio)

# instalamos paquete para pruebas de normalidad

require(nortest)

# si no les da de esta manera, deben hacerlo desde 'Tools', luego 'Install Packages', y luego buscan el paquete
# nortest. Después lo deben activar desde 'Packages' (que está en el recuadro de abajo a la derecha)

# creamos un vector que contenga los valores p de diferentes pruebas de normalidad

valoresp <- c(shapiro.test(precio)$p.value, ad.test(precio)$p.value, cvm.test(precio)$p.value, lillie.test(precio)$p.value, pearson.test(precio)$p.value)
valoresp

# Como notamos que la variable precio no es normal porque los valores p de todas las pruebas de normalidad
# son muy pequeños, transformamos la variable para buscar normalidad.

# Creamos un vector vacío, para luego en este guardar los datos transformados
raizprecio <- c()

for (i in 1:length(precio)) {
  raizprecio[i] <- sqrt(precio[i])
}

# Ahora aplicamos las pruebas de normalidad a la nueva variable
valoresp <- c(shapiro.test(raizprecio)$p.value, ad.test(raizprecio)$p.value, cvm.test(raizprecio)$p.value, lillie.test(raizprecio)$p.value, pearson.test(raizprecio)$p.value)
valoresp

# Seguimos rechazando la normalidad, pero ahora los valores p no son tan pequeños como antes.
# Buscamos otra transformación. Como la variable original tiene asimetría de cola derecha, es ideal una 
# transformación creciente y cóncava hacia abajo.
# tangente inverso a la posición i del vector precio: (atan(precio[i])

precio1 <- c()
for (i in 1:length(precio)) {
  precio1[i] <- (precio[i])^(1/2.7)
}

valoresptransf <- c(shapiro.test(precio1)$p.value, ad.test(precio1)$p.value, cvm.test(precio1)$p.value, lillie.test(precio1)$p.value, pearson.test(precio1)$p.value)
valoresptransf

# Notamos que con la transformación precio^(1/2.7), hay dos pruebas que no rechazan normalidad con alpha = 0.05. 
# Si se escoge alpha = 0.04, ya son 3 pruebas que no rechazan normalidad. Como los valores no son tan grandes como
# se quisiera, aún no puedo aceptar H0 con seguridad, pero al menos no la rechazo en todas las pruebas. 
# Ya es un avance pasar de valores p del orden de 2.6*10^(-15) o 7.4*10^-10, a valores p del orden de 0.17 o 0.056.
# Si bien 

