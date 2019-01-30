############################################################
# CÓDIGO PARA REALIZACIÓN DE ANÁLISIS COSTE-BENEFICIO EN R #
############################################################
#                   ORGEMPLEG-210                          #
############################################################

# ---------------------------------- #
# 1.- FUNCIÓN VALOR ACTUALIZADO NETO #
# ---------------------------------- #

mine.npv.fun <- function (vector.data) { 
  # VECTOR DATA : vector en el cual se incluyan los parámetros siguientes
  # (fijar, dejar como parámetros variables según deseos)
  # (se introducen como vector para el posterior análisis de sensibilidad)
  
  # COSTES #
  # ------ #
  # Land costs
  land.cost <- vector.data[1]
  # Construction costs
  cons.cost <- vector.data[2]
  # General management costs
  mgmt.cost <- vector.data[3]
  # Operation costs
  oper.cost <- vector.data[4]
  # Human resources costs
  humr.cost <- vector.data[5]
  # Recovery costs
  reco.cost <- vector.data[6]
  
  # INGRESOS #
  # -------- #
  # Selling price
  pric.inco <- vector.data[7]
  
  # PRODUCCION #
  # ---------- #
  prod.data <- vector.data[8]
  
  # PARÁMETROS BÁSICOS #
  # ------------------ #
  # Tasa de descuento
  disc.rate <- vector.data[9]
  # Horizonte temporal 
  hozt.temp <- vector.data[10]
  
  
  # SERIES TEMPORALES #
  # ----------------- #
  # Años
  year.vector <- c(2019:(2019+3+hozt.temp+1))
  
  # Land costs
  land.cost.vector <- rep(0, length(year.vector))
  land.cost.vector[1] <- land.cost
  
  # Construction costs
  cons.cost.vector <- rep(0, length(year.vector))
  for (i in 1:3){
    cons.cost.vector[i] <- cons.cost/3
  }
  
  # General management costs
  mgmt.cost.vector <- rep(0, length(year.vector))
  for (i in 4:length(year.vector)){
    mgmt.cost.vector[i] <- mgmt.cost
  }
  
  # Operation costs
  oper.cost.vector <- rep(0, length(year.vector))
  for (i in 4:length(year.vector)){
    oper.cost.vector[i] <- oper.cost*prod.data
  }
  
  # Human resources costs
  humr.cost.vector <- rep(0, length(year.vector))
  for (i in 1:length(year.vector)){
    humr.cost.vector[i] <- humr.cost
  }
  
  # Recovery costs
  reco.cost.vector <- rep(0, length(year.vector))
  reco.cost.vector[length(year.vector)] <- reco.cost
  
  
  # Incomes
  inco.cost.vector <- rep(0, length(year.vector))
  for (i in 4:(length(year.vector)-1)){
    inco.cost.vector[i] <- pric.inco*prod.data
  }
  
  # --------------------- # 
  # BENEFITS / Beneficios #
  # --------------------- #
  ben.vector <- rep(0, length(year.vector))
  netben.vector <- rep(0, length(year.vector))
  for (i in (1):(length(year.vector))) {
    ben.vector[i] <- -land.cost.vector[i]-cons.cost.vector[i]-mgmt.cost.vector[i]-
      oper.cost.vector[i]-humr.cost.vector[i]-reco.cost.vector[i]+
      inco.cost.vector[i]
    netben.vector[i] <- ben.vector[i]/((1+disc.rate)^i)
  }
  
  # --------- # 
  # NPV / VAN #
  # --------- #
  npv.result <- sum(netben.vector)
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
  
  # Steps del análisis. -1%, 0, +1%
  steps <- c(-0.01, 0, 0.01)
  perc.df <- data.frame(steps)
  
  column.names <- rep(0, pars.dim+1)
  column.names[1] = "Change in parameter"
  # Este ciclo está ajustado para tres steps (a, b, c)
  for (i in 1:pars.dim){
    column.names[i+1] = pars.vector.names[i]
    pars.vector.sensit <- pars.vector.values
    pars.vector.sensit[i] <- pars.vector.values[i]*(1+steps[1])
    a <- model(pars.vector.sensit)
    pars.vector.sensit[i] <- pars.vector.values[i]*(1+steps[2])
    b <- model(pars.vector.sensit)
    pars.vector.sensit[i] <- pars.vector.values[i]*(1+steps[3])
    c <- model(pars.vector.sensit)
    perc.df <- cbind(perc.df, c(100*(a-b)/b, 0, 100*(c-b)/b))
  }
  colnames(perc.df) <- column.names
  perc.df
}

# --------------------- #
# 3.- VALORES CASO BASE #
# --------------------- #
# Land costs
land.cost <- 12345
# Construction costs
cons.cost <- 123456
# General management costs
mgmt.cost <- 12345
# Operation costs
oper.cost <- 10
# Human resources costs
humr.cost <- 1234567
# Recovery costs
reco.cost <- 12345
# Selling price
pric.inco <- 15
# Production
prod.data <- 12345
# Tasa de descuento
disc.rate <- 0.035
# Horizonte temporal 
hozt.temp <- 50

case.base.vector <- c(land.cost,cons.cost,mgmt.cost,oper.cost,humr.cost,reco.cost,pric.inco,prod.data,disc.rate,hozt.temp)

npv.case.base <- mine.npv.fun(case.base.vector)

# -------------------- #
# 4.- VAN ALTERNATIVAS #
# -------------------- #

# Alternativa 0 - Case Base
Alt.0.vector <- c(land.cost,cons.cost,mgmt.cost,oper.cost,humr.cost,reco.cost,pric.inco,prod.data,disc.rate,hozt.temp)
Alt.0.npv <- mine.npv.fun(Alt.0.vector)

# Alternativa 1 - Alternativa 



# ------------------------------------------ #
# 5.- EJECUCIÓN DEL ANÁLISIS DE SENSIBILIDAD #
# ------------------------------------------ #

# Alternativa 0 - Case Base
model_0.param <- Alt.0.vector
model_0.names <- c("land.cost","cons.cost","mgmt.cost","oper.cost",
                   "humr.cost","reco.cost","pric.inco","prod.data",
                   "disc.rate", "hozt.temp")
model_0.sa.df <- anal.sensit(mine.npv.fun, model_0.param, model_0.names)
model_0.sa.df$project <- "Case Base"

# Alternativa 1 - Alternativa



# Manipular los datos para presentarlos
sa.data.df.melted <- reshape2::melt(model_0.sa.df)
sa.data.df.melted$value <- abs(sa.data.df.melted$value)

# Para varias alternativas
#sa.data.df <- rbind(model_0.sa.df, model_1.sa.df)
#sa.data.df.melted <- reshape2::melt(sa.data.df)
#sa.data.df.melted$value <- abs(sa.data.df.melted$value)

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
# 6.- ANÁLISIS MONTECARLO #
# ----------------------- #

# Alternativa 0 - Case Base
model_0.mc.df <- data.frame(npv=numeric(0), irr=numeric(0))
for (i in 1:500) {
  # Distribuciones de probabilidad de cada parámetro
  a <- runif(1, min = 36204000, max = 36204000) #land.cost
  b <- runif(1, min = cons.cost*0.75, max = cons.cost*1.25) #cons.cost
  c <- runif(1, min = mgmt.cost*0.75, max = mgmt.cost*1.25) #mgmt.cost
  d <- runif(1, min = oper.cost*0.75, max = oper.cost*1.25) #oper.cost
  e <- runif(1, min = humr.cost*0.75, max = humr.cost*1.25) #humr.cost
  f <- runif(1, min = reco.cost*0.75, max = reco.cost*1.25) #reco.cost
  g <- runif(1, min = pric.inco*0.75, max = pric.inco*1.25) #pric.inco
  h <- runif(1, min = prod.data*0.75, max = prod.data*1.25) #prod.data
  i <- runif(1, min = 0.03*0.75, max = 0.03*1.25) #disc.rate
  j <- runif(1, min = 25, max = 25) #h0zt.temp
  model_0.param <- c(a, b, c, d, e, f, g, h, i, j)
  # Llamada a la función y almacenar datos
  model_0.mc.df <- rbind(model_0.mc.df, mine.npv.fun(model_0.param))
}
# DATA PREPARATION
colnames(model_0.mc.df) <- c("npv")
model_0.mc.sort <- (model_0.mc.df)
model_0.mc.sort$npv <- model_0.mc.sort[order(model_0.mc.sort$npv),]
# Sequence by 1/num.cases, 1/500 = 0.002
model_0.mc.sort <- data.frame(model_0.mc.sort, seq(from = 0.002, to = 1, by = 0.002))
model_0.mc.sort$proj <- rep('Base', 500)
colnames(model_0.mc.sort) <- c("npv", "prob", "model")


# Mezclar las distintas alternativas
npv.df.mix <- model_0.mc.sort
#npv.df.mix <- rbind(model_1.mc.sort, model_2.mc.sort, model_3.mc.sort)

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
  guides(fill=guide_legend(title=NULL)) +
  geom_vline(xintercept = Alt.0.npv, colour="red", size=0.65, linetype = "longdash")
