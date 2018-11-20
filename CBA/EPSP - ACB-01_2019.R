############################################################
# CÓDIGO PARA REALIZACIÓN DE ANÁLISIS COSTE-BENEFICIO EN R #
############################################################
#                          ACB-01                          #
############################################################

# ------------------------------ #
# CÁLCULO VALOR ACTUALIZADO NETO #
# ------------------------------ #

# COSTES #
# ------ #
# Construction costs
cons.cost <- 30000000
# Años 1998, 1999, 2000, 2001, 2002
cons.cost.dist <- c(0.20, 0.20, 0.20, 0.20, 0.20)
# Operation and maintenance
oper.cost <- 860000
# Forgone land rent
land.cost <- 1000000


# INGRESOS #
# -------- #
# Improved land alocation
land.inco    <- 1000000
#Flood risk
flood.inco <- 3000000
# Hunting
hunt.inco <- 750000


# PARÁMETROS BÁSICOS #
# ------------------ #
# Tasa de descuento
disc.rate <- 0.03
# Horizonte temporal
hozt.temp <- 25


# SERIES TEMPORALES #
# ----------------- #
# Años
year.vector <- c(0:hozt.temp)

# Construction costs
cons.cost.vector <- rep(0, hozt.temp+1)
for (i in 1:length(cons.cost.dist)) {
  cons.cost.vector[i] <- cons.cost.dist[i]*cons.cost
}
# Operation and maintenance
oper.cost.vector <- rep(0, hozt.temp+1)
for (i in length(cons.cost.dist):hozt.temp+1) {
  oper.cost.vector[i] <- oper.cost
}
# Forgone land rent
land.cost.vector <- rep(0, hozt.temp+1)
for (i in length(cons.cost.dist):hozt.temp+1) {
  land.cost.vector[i] <- land.cost
}

# Improved land alocation
land.inco.vector <- rep(0, hozt.temp+1)
for (i in length(cons.cost.dist):hozt.temp+1) {
  land.inco.vector[i] <- land.inco
}
#Flood risk
flood.inco.vector <- rep(0, hozt.temp+1)
for (i in length(cons.cost.dist):hozt.temp+1) {
  flood.inco.vector[i] <- flood.inco
}
# Hunting
hunt.inco.vector <- rep(0, hozt.temp+1)
for (i in length(cons.cost.dist):hozt.temp+1) {
  hunt.inco.vector[i] <- hunt.inco
}

# --------------------- #
# BENEFITS / Beneficios #
# --------------------- #
ben.vector <- rep(0, hozt.temp+1)
netben.vector <- rep(0, hozt.temp+1)
for (i in (1):(hozt.temp+1)) {
  ben.vector[i] <- land.inco.vector[i]+flood.inco.vector[i]+hunt.inco.vector[i]-
    cons.cost.vector[i]-oper.cost.vector[i]-land.cost.vector[i]
  netben.vector[i] <- ben.vector[i]/((1+disc.rate)^(i-1))
}

# --------------------- #
# NPV + IRR / VAN + TIR #
# --------------------- #
npv <- sum(netben.vector)
# To calculate irr, both negative and positive values in benefits
library(FinCal)
irr <- irr(ben.vector)

npv.result <- c(npv, irr)
npv.result

