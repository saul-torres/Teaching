# ----------------------------------- #
# 1A.- FUNCIÓN VALOR ACTUALIZADO NETO # Devuelve el npv
# ----------------------------------- #

cc.npv.fun <- function (vector.data) { 
  # VECTOR DATA : vector en el cual se incluyan los parámetros siguientes
  # (fijar, dejar como parámetros variables según deseos)
  # (se introducen como vector para el posterior análisis de sensibilidad)
  
  # VARIABLES #
  # --------- #
  # Costes Iniciales
  cost.inv1 <- vector.data[1]
  cost.inv2 <- vector.data[2]
  cost.inv3 <- vector.data[3]
  
  # Mantenimiento
  cost.mant <- vector.data[4]
  # Explotacion
  cost.expl.ffcc <- vector.data[5]
  cost.expl.auto <- vector.data[6]
  
  # DEMANDA #
  # ------- #
  # Ferrocarril
  dem1.ffcc <- vector.data[7]
  dem2.ffcc <- vector.data[8]
  # Autopista 
  dem1.auto <- vector.data[9]
  dem2.auto <- vector.data[10]
  
  # TIEMPOS #
  # ------- #
  # Ferrocarril
  time.ffcc.inic <- vector.data[11]
  time.ffcc.fina <- vector.data[12]
  # Autopista
  time.auto.inic <- vector.data[13]
  time.auto.fina <- vector.data[14]
  # Coste del tiempo
  time.cost <- vector.data[15]
  # Ferrocarril
  cost.ffcc.inic <- vector.data[16]
  cost.ffcc.fina <- vector.data[17]
  # Autopista
  cost.auto <- vector.data[18]
  
  
  # SERIES TEMPORALES #
  # ----------------- #
  # Años
  year.vector <- c(1:hozt.temp)
  
  # Construction costs
  cost.inve.vector <- rep(0, hozt.temp)
  cost.inve.vector[1] <- cost.inv1
  cost.inve.vector[2] <- cost.inv2
  cost.inve.vector[3] <- cost.inv3
  
  # Costes explotacion
  cost.expl.vector <- rep(0, hozt.temp)
  cost.mant.vector <- rep(0, hozt.temp)
  
  # Demanda
  dem1.ffcc.vector <- rep(0, hozt.temp)
  dem2.ffcc.vector <- rep(0, hozt.temp)
  dem1.auto.vector <- rep(0, hozt.temp)
  dem2.auto.vector <- rep(0, hozt.temp)
  
  # Ingresos
  ingr.expl.vector <- rep(0, hozt.temp)
  ingr.soci.vector <- rep(0, hozt.temp)
  
  
  for (i in 4:hozt.temp) {
    cost.expl.vector[i] <- cost.expl.ffcc
    cost.mant.vector[i] <- cost.mant
    dem1.ffcc.vector[i] <- dem1.ffcc
    dem2.ffcc.vector[i] <- max(dem2.ffcc.vector[i-1]*1.05, dem2.ffcc)
    ingr.expl.vector[i] <- dem2.ffcc.vector[i]*cost.ffcc.fina
    ingr.soci.vector[i] <- dem2.ffcc.vector[i]*(cost.ffcc.inic-cost.ffcc.fina) + #Ahorros coste ffcc
      dem2.ffcc.vector[i]*(time.ffcc.inic-time.ffcc.fina)*time.cost + #Ahorros tiempo ffcc
      dem2.auto.vector[i]*(time.auto.inic-time.auto.fina)*time.cost #Ahorros tiempo autopista
  }
  
  
  # --------------------- # 
  # BENEFITS / Beneficios #
  # --------------------- #
  ben.vector <- rep(0, hozt.temp)
  netben.vector <- rep(0, hozt.temp)
  for (i in (1):(hozt.temp)) {
    ben.vector[i] <- -cost.inve.vector[i]-cost.expl.vector[i]-cost.mant.vector[i]+
      ingr.expl.vector[i]+ingr.soci.vector[i]
    netben.vector[i] <- ben.vector[i]/((1+disc.rate)^i)
  }
  
  # --------- # 
  # NPV / VAN #
  # --------- #
  npv.result <- sum(netben.vector)
  return(npv.result)
}



# ----------------------------------- #
# 1B.- FUNCIÓN VALOR ACTUALIZADO NETO # Devuelve vector de net.benefits
# ----------------------------------- #

cc.netben.fun <- function (vector.data) { 
  # VECTOR DATA : vector en el cual se incluyan los parámetros siguientes
  # (fijar, dejar como parámetros variables según deseos)
  # (se introducen como vector para el posterior análisis de sensibilidad)
  
  # VARIABLES #
  # --------- #
  # Costes Iniciales
  cost.inv1 <- vector.data[1]
  cost.inv2 <- vector.data[2]
  cost.inv3 <- vector.data[3]
  
  # Mantenimiento
  cost.mant <- vector.data[4]
  # Explotacion
  cost.expl.ffcc <- vector.data[5]
  cost.expl.auto <- vector.data[6]
  
  # DEMANDA #
  # ------- #
  # Ferrocarril
  dem1.ffcc <- vector.data[7]
  dem2.ffcc <- vector.data[8]
  # Autopista 
  dem1.auto <- vector.data[9]
  dem2.auto <- vector.data[10]
  
  # TIEMPOS #
  # ------- #
  # Ferrocarril
  time.ffcc.inic <- vector.data[11]
  time.ffcc.fina <- vector.data[12]
  # Autopista
  time.auto.inic <- vector.data[13]
  time.auto.fina <- vector.data[14]
  # Coste del tiempo
  time.cost <- vector.data[15]
  # Ferrocarril
  cost.ffcc.inic <- vector.data[16]
  cost.ffcc.fina <- vector.data[17]
  # Autopista
  cost.auto <- vector.data[18]
  
  
  # SERIES TEMPORALES #
  # ----------------- #
  # Años
  year.vector <- c(1:hozt.temp)
  
  # Construction costs
  cost.inve.vector <- rep(0, hozt.temp)
  cost.inve.vector[1] <- cost.inv1
  cost.inve.vector[2] <- cost.inv2
  cost.inve.vector[3] <- cost.inv3
  
  # Costes explotacion
  cost.expl.vector <- rep(0, hozt.temp)
  cost.mant.vector <- rep(0, hozt.temp)
  
  # Demanda
  dem1.ffcc.vector <- rep(0, hozt.temp)
  dem2.ffcc.vector <- rep(0, hozt.temp)
  dem1.auto.vector <- rep(0, hozt.temp)
  dem2.auto.vector <- rep(0, hozt.temp)
  
  # Ingresos
  ingr.expl.vector <- rep(0, hozt.temp)
  ingr.soci.vector <- rep(0, hozt.temp)
  
  
  for (i in 4:hozt.temp) {
    cost.expl.vector[i] <- cost.expl.ffcc
    cost.mant.vector[i] <- cost.mant
    dem1.ffcc.vector[i] <- dem1.ffcc
    dem2.ffcc.vector[i] <- max(dem2.ffcc.vector[i-1]*1.05, dem2.ffcc)
    ingr.expl.vector[i] <- dem2.ffcc.vector[i]*cost.ffcc.fina
    ingr.soci.vector[i] <- dem2.ffcc.vector[i]*(cost.ffcc.fina-cost.ffcc.inic) + #Ahorros coste ffcc
      dem2.ffcc.vector[i]*(time.ffcc.inic-time.ffcc.fina)*time.cost + #Ahorros tiempo ffcc
      dem2.auto.vector[i]*(time.auto.inic-time.auto.fina)*time.cost #Ahorros tiempo autopista
  }
  
  
  # --------------------- # 
  # BENEFITS / Beneficios #
  # --------------------- #
  ben.vector <- rep(0, hozt.temp)
  netben.vector <- rep(0, hozt.temp)
  acum.netben.vector <- rep(0, hozt.temp)
  for (i in (1):(hozt.temp)) {
    ben.vector[i] <- -cost.inve.vector[i]-cost.expl.vector[i]-cost.mant.vector[i]+
      ingr.expl.vector[i]+ingr.soci.vector[i]
    netben.vector[i] <- ben.vector[i]/((1+disc.rate)^i)
    if (i==1) {acum.netben.vector[i] <- netben.vector[i]}
    else {acum.netben.vector[i] <- acum.netben.vector[i-1]+netben.vector[i]}
  }
  
  # --------- # 
  # NPV / VAN #
  # --------- #
  npv.result <- acum.netben.vector
  return(npv.result)
}



# ----------------------------------- #
# 1C.- FUNCIÓN VALOR ACTUALIZADO NETO # Devuelve dataframe con flujos
# ----------------------------------- #

cc.table.fun <- function (vector.data) { 
  # VECTOR DATA : vector en el cual se incluyan los parámetros siguientes
  # (fijar, dejar como parámetros variables según deseos)
  # (se introducen como vector para el posterior análisis de sensibilidad)
  
  # VARIABLES #
  # --------- #
  # Costes Iniciales
  cost.inv1 <- vector.data[1]
  cost.inv2 <- vector.data[2]
  cost.inv3 <- vector.data[3]
  
  # Mantenimiento
  cost.mant <- vector.data[4]
  # Explotacion
  cost.expl.ffcc <- vector.data[5]
  cost.expl.auto <- vector.data[6]
  
  # DEMANDA #
  # ------- #
  # Ferrocarril
  dem1.ffcc <- vector.data[7]
  dem2.ffcc <- vector.data[8]
  # Autopista 
  dem1.auto <- vector.data[9]
  dem2.auto <- vector.data[10]
  
  # TIEMPOS #
  # ------- #
  # Ferrocarril
  time.ffcc.inic <- vector.data[11]
  time.ffcc.fina <- vector.data[12]
  # Autopista
  time.auto.inic <- vector.data[13]
  time.auto.fina <- vector.data[14]
  # Coste del tiempo
  time.cost <- vector.data[15]
  # Ferrocarril
  cost.ffcc.inic <- vector.data[16]
  cost.ffcc.fina <- vector.data[17]
  # Autopista
  cost.auto <- vector.data[18]
  
  hozt.temp <- vector.data[19]
  disc.rate <- vector.data[20]
  
  
  # SERIES TEMPORALES #
  # ----------------- #
  # Años
  year.vector <- c(1:hozt.temp)
  
  # Construction costs
  cost.inve.vector <- rep(0, hozt.temp)
  cost.inve.vector[1] <- cost.inv1
  cost.inve.vector[2] <- cost.inv2
  cost.inve.vector[3] <- cost.inv3
  
  # Costes explotacion
  cost.expl.vector <- rep(0, hozt.temp)
  cost.mant.vector <- rep(0, hozt.temp)
  
  # Demanda
  dem1.ffcc.vector <- rep(0, hozt.temp)
  dem2.ffcc.vector <- rep(0, hozt.temp)
  dem1.auto.vector <- rep(0, hozt.temp)
  dem2.auto.vector <- rep(0, hozt.temp)
  
  # Ingresos
  ingr.expl.vector <- rep(0, hozt.temp)
  ingr.soci.vector <- rep(0, hozt.temp)
  
  
  for (i in 4:hozt.temp) {
    cost.expl.vector[i] <- cost.expl.ffcc
    cost.mant.vector[i] <- cost.mant
    dem1.ffcc.vector[i] <- dem1.ffcc
    dem2.ffcc.vector[i] <- max(dem2.ffcc.vector[i-1]*1.05, dem2.ffcc)
    ingr.expl.vector[i] <- dem2.ffcc.vector[i]*cost.ffcc.fina
    ingr.soci.vector[i] <- dem2.ffcc.vector[i]*(cost.ffcc.fina-cost.ffcc.inic) + #Ahorros coste ffcc
      dem2.ffcc.vector[i]*(time.ffcc.inic-time.ffcc.fina)*time.cost + #Ahorros tiempo ffcc
      dem2.auto.vector[i]*(time.auto.inic-time.auto.fina)*time.cost #Ahorros tiempo autopista
  }
  
  
  # --------------------- # 
  # BENEFITS / Beneficios #
  # --------------------- #
  ben.vector <- rep(0, hozt.temp)
  netben.vector <- rep(0, hozt.temp)
  acum.netben.vector <- rep(0, hozt.temp)
  inco.suma.vector <- rep(0, hozt.temp)
  cost.suma.vector <- rep(0, hozt.temp)
  for (i in (1):(hozt.temp)) {
    inco.suma.vector[i] <- ingr.expl.vector[i]+ingr.soci.vector[i]
    cost.suma.vector[i] <- -cost.inve.vector[i]-cost.expl.vector[i]-cost.mant.vector[i]
    ben.vector[i] <- -cost.inve.vector[i]-cost.expl.vector[i]-cost.mant.vector[i]+
      ingr.expl.vector[i]+ingr.soci.vector[i]
    netben.vector[i] <- ben.vector[i]/((1+disc.rate)^i)
    if (i==1) {acum.netben.vector[i] <- netben.vector[i]}
    else {acum.netben.vector[i] <- acum.netben.vector[i-1]+netben.vector[i]}
  }
  
  table.result <- cbind(year.vector, inco.suma.vector, cost.suma.vector,
                        ben.vector, netben.vector, acum.netben.vector)
  return(table.result[])
}


# ---------------- #
# 2.- VALORES BASE #
# ---------------- #

cost.inv1 <- 25000000
cost.inv2 <- 50000000
cost.inv3 <- 40000000
cost.mant <- 1500000
cost.expl.ffcc <- 20000000
cost.expl.auto <- 0.5
dem1.ffcc <- 300000
dem2.ffcc <- 2000000
dem1.auto <- 5700000
dem2.auto <- 5000000
time.ffcc.inic <- 55/60
time.ffcc.fina <- 30/60
time.auto.inic <- 43/60
time.auto.fina <- 35/60
time.cost <- 15
cost.ffcc.inic <- 1
cost.ffcc.fina <- 1.55
cost.auto <- 0.055
disc.rate <- 0.045
hozt.temp <- 50


# -------------------- #
# 3.- VAN ALTERNATIVAS #
# -------------------- #

# Alternativa 1 - Caso base
Alt.1.vector <- c(cost.inv1, cost.inv2, cost.inv3, cost.mant, cost.expl.ffcc, cost.expl.auto,
                  dem1.ffcc, dem2.ffcc, dem1.auto, dem2.auto,
                  time.ffcc.inic, time.ffcc.fina, time.auto.inic, time.auto.fina, time.cost,
                  cost.ffcc.inic, cost.ffcc.fina, cost.auto,
                  disc.rate, hozt.temp)
Alt.1.npv <- cc.npv.fun(Alt.1.vector)
Alt.1.netben.vector <- cc.netben.fun(Alt.1.vector)
Alt.1.table <- cc.table.fun(Alt.1.vector)

# DATA PREPARATION
Alt.1.netben.df <- cbind(Alt.1.netben.vector, c(1:hozt.temp))
colnames(Alt.1.netben.df) <- c("values", "years")
Alt.1.netben.df <- as.data.frame(Alt.1.netben.df)
# REPRESENTATION
library(ggplot2)
ggplot(Alt.1.netben.df,aes(years, values)) + 
  geom_line(alpha=.5) + 
  xlab("Años") + 
  ylab("Euros")


# ----------------------- #
# 4.- ANÁLISIS MONTECARLO #
# ----------------------- #

# Alternativa 1 - Caso base
model_1.mc.df <- data.frame(npv=numeric(0))
model_1.mc.netben.df <- data.frame(npv=numeric(0), years=numeric(0), simul=numeric(0))
for (index in 1:500) {
  # Distribuciones de probabilidad de cada parámetro
  p1 <- runif(1, min = Alt.1.vector[1]*1.00, max = Alt.1.vector[1]*1.00) #cost.inv1
  p2 <- runif(1, min = Alt.1.vector[2]*0.95, max = Alt.1.vector[2]*1.05) #cost.inv2
  p3 <- runif(1, min = Alt.1.vector[3]*0.95, max = Alt.1.vector[3]*1.05) #cost.inv3
  p4 <- runif(1, min = Alt.1.vector[4]*0.95, max = Alt.1.vector[4]*1.05) #cost.mant
  p5 <- runif(1, min = Alt.1.vector[5]*0.95, max = Alt.1.vector[5]*1.05) #cost.expl.ffcc
  p6 <- runif(1, min = Alt.1.vector[6]*0.95, max = Alt.1.vector[6]*1.05) #cost.mant 
  p7 <- runif(1, min = Alt.1.vector[7]*0.95, max = Alt.1.vector[7]*1.05) #dem1.ffcc
  p8 <- runif(1, min = Alt.1.vector[8]*0.95, max = Alt.1.vector[8]*1.05) #dem2.ffcc
  p9 <- runif(1, min = Alt.1.vector[9]*0.95, max = Alt.1.vector[9]*1.05) #dem1.auto
  p10 <- runif(1, min = Alt.1.vector[10]*0.95, max = Alt.1.vector[10]*1.05) #dem2.auto
  p11 <- runif(1, min = Alt.1.vector[11]*0.95, max = Alt.1.vector[11]*1.05) #time.ffcc.inic
  p12 <- runif(1, min = Alt.1.vector[12]*0.95, max = Alt.1.vector[12]*1.05) #time.ffcc.fina
  p13 <- runif(1, min = Alt.1.vector[13]*0.95, max = Alt.1.vector[13]*1.05) #time.auto.inic
  p14 <- runif(1, min = Alt.1.vector[14]*0.95, max = Alt.1.vector[14]*1.05) #time.auto.fina
  p15 <- runif(1, min = Alt.1.vector[15]*0.95, max = Alt.1.vector[15]*1.05) #time.cost
  p16 <- runif(1, min = Alt.1.vector[16]*0.95, max = Alt.1.vector[16]*1.05) #cost.ffcc.inic
  p17 <- runif(1, min = Alt.1.vector[17]*0.95, max = Alt.1.vector[17]*1.05) #cost.ffcc.fina
  p18 <- runif(1, min = Alt.1.vector[18]*0.95, max = Alt.1.vector[18]*1.05) #cost.auto
  p19 <- runif(1, min = Alt.1.vector[19]*0.95, max = Alt.1.vector[19]*1.05) #disc.rate
  p20 <- runif(1, min = Alt.1.vector[20]*1.00, max = Alt.1.vector[20]*1.00) #hori.temp
  
  model_1.param <- c(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10,
                     p11, p12, p13, p14, p15, p16, p17, p18, p19, p20)
  # Llamada a la función y almacenar datos de npv
  model_1.mc.df <- rbind(model_1.mc.df, cc.npv.fun(model_1.param))
  
  # Llamada a la función y almacenar datos de paths
  model_1b.mc.netben.df <- cbind(cc.netben.fun(model_1.param), c(1:hozt.temp), index)
  colnames(model_1b.mc.netben.df) <- c("values", "years", "simul")
  model_1b.mc.netben.df <- as.data.frame(model_1b.mc.netben.df)
  model_1.mc.netben.df <- rbind(model_1.mc.netben.df, model_1b.mc.netben.df)
  
}

# DATA PREPARATION
colnames(model_1.mc.df) <- c("npv")
model_1.mc.sort <- (model_1.mc.df)
model_1.mc.sort$npv <- model_1.mc.sort[order(model_1.mc.sort$npv),]
# Sequence by 1/num.cases, 1/500 = 0.004
model_1.mc.sort <- data.frame(model_1.mc.sort, seq(from = 0.002, to = 1, by = 0.002))
model_1.mc.sort$proj <- rep('Caso Base', 500)
colnames(model_1.mc.sort) <- c("npv", "prob", "model")

# REPRESENTATION
library(ggplot2)
library(scales)
library(grid)

# Plot de funciones de densidad
ggplot(model_1.mc.sort, aes(npv, fill = model, colour = model)) +
  geom_density(alpha = 0.1) +
  labs(x="Valor Actualizado Neto", 
       y="Probabilidad",
       title="Nueva Línea FFCC",
       subtitle="Alternativa 1") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  geom_vline(xintercept = 0, colour="red", size=0.65, linetype = "longdash") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  guides(colour=FALSE, fill=FALSE)



# Plot de paths
ggplot(model_1.mc.netben.df,aes(years, values, group=simul, colour=simul)) + 
  geom_line(alpha=.5) +
  labs(x="Años", 
       y="Valor Actualizado Neto",
       title="Nueva Línea FFCC",
       subtitle="Alternativa 1") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +geom_hline(yintercept = 0, colour="red", size=0.65, linetype = "longdash") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  guides(colour=FALSE, fill=FALSE)



# Plot combinado
plot_main <- ggplot(model_1.mc.netben.df,aes(years, values, group=simul, colour=simul)) + 
  geom_line(alpha=.5) +labs(x="Años", 
                            y="Valor Actualizado Neto") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  guides(colour=FALSE) + 
  geom_hline(yintercept = 0, colour="red", size=0.65, linetype = "longdash") +
  geom_vline(xintercept = 30, colour="orangered4", size=0.65, linetype = "longdash") +
  geom_vline(xintercept = 40, colour="goldenrod4", size=0.65, linetype = "longdash") +
  geom_vline(xintercept = 50, colour="dodgerblue4", size=0.65, linetype = "longdash")

# Gráfico secundario. Función de densidad del VAN (año 21)
plot_right <- ggplot(data=model_1.mc.netben.df) + 
  geom_density(aes(ifelse(years==25,values,NA)), size = 0.65, colour="orangered4", fill='#fc8d59', alpha=.25) +
  geom_density(aes(ifelse(years==40,values,NA)), size = 0.65, colour="goldenrod4", fill='#ffffbf', alpha=.25) +
  geom_density(aes(ifelse(years==50,values,NA)), size = 0.65, colour="dodgerblue4", fill='#2b83ba', alpha=.25) + 
  coord_flip() +
  theme(legend.position=c(0.75,0.25)) +
  xlab("") +
  ylab("Probabilidad") +
  geom_vline(xintercept = 0, colour="red", size=0.65, linetype = "longdash") +
  scale_x_continuous(labels = comma, limits = c(min(model_1.mc.netben.df$values), max(model_1.mc.netben.df$values)))

# Unión de los dos gráficos
library(gridExtra)
plot_mix <- grid.arrange(plot_main, plot_right, ncol=2, nrow=1,
                         widths = c(3, 1),
                         top=textGrob("Nueva Línea FFCC - Alternativa 1", gp=gpar(fontsize=13)))
plot_mix


