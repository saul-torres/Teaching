############################################################
# CÓDIGO PARA REALIZACIÓN DE ANÁLISIS COSTE-BENEFICIO EN R #
############################################################
#                          ACB-02                          #
############################################################

# ---------------------------------- #
# 1.- FUNCIÓN VALOR ACTUALIZADO NETO #
# ---------------------------------- #

npv.fun <- function (vector.data) { 
  # VECTOR DATA : vector en el cual se incluyan los parámetros siguientes
  # (fijar, dejar como parámetros variables según deseos)
  # (se introducen como vector para el posterior análisis de sensibilidad)
  
  # COSTES #
  # ------ #
  # Construction costs
  cons.cost <- vector.data[1]
  # Años 1998, 1999, 2000, 2001, 2002
  cons.cost.dist <- c(0.05, 0.17, 0.33, 0.25, 0.20)
  # Operation and maintenance
  oper.cost <- vector.data[2]
  # Forgone land rent
  land.cost <- vector.data[3]
  
  
  # INGRESOS #
  # -------- #
  # Improved land alocation
  land.inco	<- vector.data[4]
  #Flood risk
  flood.inco <- vector.data[5]
  # Hunting
  hunt.inco <- vector.data[6]
  
  
  # PARÁMETROS BÁSICOS #
  # ------------------ #
  # Tasa de descuento
  disc.rate <- vector.data[7]
  # Horizonte temporal 
  hozt.temp <- vector.data[8]
  
  
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
  npv.result <- npv
  npv.result
}

# ------------------------------------ #
# 2.- FUNCIÓN ANÁLISIS DE SENSIBILIDAD #
# ------------------------------------ #

anal.sensit <- function( model, pars.vector.values, pars.vector.names) {
  # Inputs:
      # función modelo
      # vector con los parámetros base del modelo
      # vector con los nombres de los parámetros
  pars.dim <- length(pars.vector.values)
  
  # Steps del análisis. -5%, -2.5%, 0, 2.5%, 5%
  steps <- c(-0.05, -0.025, 0, 0.025, 0.05)
  abs.df <- data.frame(steps)
  perc.df <- data.frame(steps)
  
  column.names <- rep(0, pars.dim+1)
  column.names[1] = "Change in parameter"
  # Este ciclo está ajustado para cinco steps (a, b, c, d, e)
  for (i in 1:pars.dim){
    column.names[i+1] = pars.vector.names[i]
    pars.vector.sensit <- pars.vector.values
    pars.vector.sensit[i] <- pars.vector.values[i]*(1+steps[1])
    a <- model(pars.vector.sensit)
    pars.vector.sensit[i] <- pars.vector.values[i]*(1+steps[2])
    b <- model(pars.vector.sensit)
    pars.vector.sensit[i] <- pars.vector.values[i]*(1+steps[3])
    c <- model(pars.vector.sensit)
    pars.vector.sensit[i] <- pars.vector.values[i]*(1+steps[4])
    d <- model(pars.vector.sensit)
    pars.vector.sensit[i] <- pars.vector.values[i]*(1+steps[5])
    e <- model(pars.vector.sensit)
    abs.df <- cbind(abs.df, c(a,b,c,d,e))
    perc.df <- cbind(perc.df, c(100*(a-c)/c, 100*(b-c)/c, 0, 100*(d-c)/c, 100*(e-c)/c))
  }
  colnames(abs.df) <- column.names
  colnames(perc.df) <- column.names
  perc.df
}

# ------------------------------------------ #
# 3.- EJECUCIÓN DEL ANÁLISIS DE SENSIBILIDAD #
# ------------------------------------------ #

# Copiar y pegar para analizar varias alternativas
model_1.param <- c(30000000, 860000, 1000000, 1000000, 3000000, 750000, 0.03, 25)
model_1.names <- c("cons.cost", "oper.cost", "land.cost",
                   "land.inco", "flood.inco", "hunt.inco",
                   "disc.rate", "hozt.temp")
model_1.sa.df <- anal.sensit(npv.fun, model_1.param, model_1.names)
model_1.sa.df$project <- "model_1"


# Manipular los datos para presentarlos
sa.data.df <- model_1.sa.df[-c(1,2,3,5),]
sa.data.df <- sa.data.df[,-c(1)]

sa.data.df.melted <- reshape2::melt(sa.data.df)
sa.data.df.melted$value <- abs(sa.data.df.melted$value)

# Representación gráfica
library(ggplot2)

# Transformación polar
coord_radar <- function (theta = "x", start = 0, direction = 1) 
{
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") 
    "y"
  else "x"
  ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start, 
          direction = sign(direction),
          is_linear = function(coord) TRUE)
}

#Colores por defecto
ggplot(sa.data.df.melted, aes(x = variable, y = value)) +
  geom_polygon(aes(group = project, color = project), fill = NA, size = 2, show.legend = FALSE) +
  geom_line(aes(group = project, color = project), size = 2) +
  theme(strip.text.x = element_text(size = rel(0.8)),
        axis.text.x = element_text(size = rel(0.8)),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  xlab("") + ylab("") +
  guides(color = guide_legend(ncol=2, title=NULL)) +
  coord_radar()

ggplot(sa.data.df.melted, aes(x = variable, y = value)) +
  geom_polygon(aes(group = project, color = project), fill = NA, size = 2) +
  facet_wrap(~ project) +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  xlab("") + ylab("") +
  guides(color = "none") +
  coord_radar()


# ----------------------- #
# 4.- ANÁLISIS MONTECARLO #
# ----------------------- #

model_1.mc.df <- data.frame(npv=numeric(0), irr=numeric(0))
for (i in 1:500) {
  # Distribuciones de probabilidad de cada parámetro
  a <- runif(1, min = 30000000*0.75, max = 30000000*1.25)
  b <- runif(1, min = 860000*0.75, max = 860000*1.25)
  c <- runif(1, min = 1000000*0.75, max = 1000000*1.25)
  d <- runif(1, min = 1000000*0.75, max = 1000000*1.25)
  e <- runif(1, min = 3000000*0.75, max = 3000000*1.25)
  f <- runif(1, min = 750000*0.75, max = 750000*1.25)
  g <- runif(1, min = 0.03*0.75, max = 0.03*1.25)
  h <- runif(1, min = 25, max = 25)
  model_1.param <- c(a, b, c, d, e, f, g, h)
  # Llamada a la función y almacenar datos
  model_1.mc.df <- rbind(model_1.mc.df, npv.fun(model_1.param))
}

# DATA PREPARATION
colnames(model_1.mc.df) <- c("npv")

model_1.mc.sort <- (model_1.mc.df)
model_1.mc.sort$npv <- model_1.mc.sort[order(model_1.mc.sort$npv),]

# Sequence by 1/num.cases, 1/500 = 0.004
model_1.mc.sort <- data.frame(model_1.mc.sort, seq(from = 0.002, to = 1, by = 0.002))
model_1.mc.sort$proj <- rep('model_1', 500)
colnames(model_1.mc.sort) <- c("npv", "prob", "model")

npv.df.mix <- model_1.mc.sort

# REPRESENTATION
library(ggplot2)
# Plot de funciones de densidad
ggplot(npv.df.mix, aes(npv, fill = model, colour = model)) +
  geom_density(alpha = 0.1) +
  ggtitle("Net Present Value density function") +
  xlab("Net Present Value") + ylab("Probability")

# Plot de histograma
ggplot(npv.df.mix, aes(npv, fill = model)) +
  geom_histogram(binwidth = 1000000) +
  ggtitle("Net Present Value histogram") +
  xlab("Net Present Value") + ylab("Num. Cases")

# Plot de polígono de frecuencias
ggplot(npv.df.mix, aes(npv, colour = model)) +
  geom_freqpoly(binwidth = 1000000) +
  ggtitle("Net Present Value histogram") +
  xlab("Net Present Value") + ylab("Num. Cases")

# Plot de funciones de distribución
ggplot(npv.df.mix, aes(npv, colour = model)) +
  stat_ecdf() +
  ggtitle("Net Present Value distribution function") +
  xlab("Net Present Value") + ylab("Probability")

# Plot de funciones de densidad con más parámetros
ggplot(npv.df.mix) +
  geom_density(aes(x = npv, colour = model, fill = model), alpha=.75) +
  geom_line(aes(x = npv, colour = model), stat="density", size=0.65) + 
  geom_vline(xintercept=0, alpha=.75, size=0.5) +
  ggtitle("Net Present Value density function, by project") +
  xlab("Net Present Value") + ylab("Probability") +
  theme(legend.position=c(0.25,0.65)) +
  scale_colour_discrete(guide=FALSE) + 
  scale_fill_discrete(labels=c("Model 1", "Model 2")) +
guides(fill=guide_legend(title=NULL))
