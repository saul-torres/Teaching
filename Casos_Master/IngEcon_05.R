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
  for (i in 0:2){
    cons.cost.vector[i] <- cons.cost/3
  }
  
  # General management costs
  mgmt.cost.vector <- rep(0, length(year.vector))
  for (i in 3:length(year.vector)){
    mgmt.cost.vector[i] <- mgmt.cost
  }
  
  # Operation costs
  oper.cost.vector <- rep(0, length(year.vector))
  for (i in 3:length(year.vector)){
    oper.cost.vector[i] <- oper.cost
  }
  
  # Human resources costs
  humr.cost.vector <- rep(0, length(year.vector))
  for (i in 1:length(year.vector)){
    humr.cost.vector[i] <- humr.cost
  }
  
  # Recovery costs
  reco.cost.vector <- rep(0, length(year.vector))
  reco.cost.vector[3:length(year.vector)] <- reco.cost
  
  
  # Incomes
  inco.cost.vector <- rep(0, length(year.vector))
  for (i in 3:(length(year.vector)-1)){
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
  # library(FinCal)
  # vector.data <- case.base.vector
  # irr.result <- irr(ben.vector)
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
land.cost <- 2300000
# Construction costs
cons.cost <- 10000000
# General management costs
mgmt.cost <- 2000000
# Operation costs
oper.cost <- 2000000
# Human resources costs
humr.cost <- 1000000
# Recovery costs
reco.cost <- 0
# Selling price
pric.inco <- 40
# Production
prod.data <- 175200
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
Alt.1.vector <- c(1500000,
                  45000000,
                  1000000,
                  10000000,
                  500000,
                  0,
                  148.71,
                  91920,
                  0.035,
                  50)
Alt.1.npv <- mine.npv.fun(Alt.1.vector)


# ------------------------------------------ #
# 5.- EJECUCIÓN DEL ANÁLISIS DE SENSIBILIDAD #
# ------------------------------------------ #

# Alternativa 0 - Case Base
model_0.param <- Alt.0.vector
model_0.names <- c("land.cost","cons.cost","mgmt.cost","oper.cost",
                   "humr.cost","no.data","pric.inco","prod.data",
                   "disc.rate", "hozt.temp")
model_0.sa.df <- anal.sensit(mine.npv.fun, model_0.param, model_0.names)
model_0.sa.df$project <- "Central Hidroeléctrica"

# Alternativa 1 - Alternativa
model_1.param <- Alt.1.vector
model_1.names <- c("land.cost","cons.cost","mgmt.cost","oper.cost",
                   "humr.cost","no.data","pric.inco","prod.data",
                   "disc.rate", "hozt.temp")
model_1.sa.df <- anal.sensit(mine.npv.fun, model_1.param, model_1.names)
model_1.sa.df$project <- "Parque Eólico"


# Manipular los datos para presentarlos
sa.data.df.melted <- reshape2::melt(model_0.sa.df)
sa.data.df.melted$value <- abs(sa.data.df.melted$value)

# Para varias alternativas
sa.data.df <- rbind(model_0.sa.df, model_1.sa.df)
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
# 6.- ANÁLISIS MONTECARLO #
# ----------------------- #

# Alternativa 0 - Case Base
model_0.mc.df <- data.frame(npv=numeric(0), irr=numeric(0))
for (i in 1:500) {
  # Distribuciones de probabilidad de cada parámetro
  a <- runif(1, min = Alt.0.vector[1], max = Alt.0.vector[1]) #land.cost
  b <- runif(1, min = Alt.0.vector[2]*0.95, max = Alt.0.vector[2]*1.25) #cons.cost
  c <- runif(1, min = Alt.0.vector[3]*0.95, max = Alt.0.vector[3]*1.25) #mgmt.cost
  d <- runif(1, min = Alt.0.vector[4]*0.95, max = Alt.0.vector[4]*1.25) #oper.cost
  e <- runif(1, min = Alt.0.vector[5]*0.95, max = Alt.0.vector[5]*1.25) #humr.cost
  f <- runif(1, min = Alt.0.vector[6]*0.95, max = Alt.0.vector[6]*1.25) #reco.cost
  g <- runif(1, min = Alt.0.vector[7]*0.75, max = Alt.0.vector[7]*1.05) #pric.inco
  h <- runif(1, min = Alt.0.vector[8]*0.75, max = Alt.0.vector[8]*1.05) #prod.data
  i <- runif(1, min = 0.035*0.75, max = 0.035*1.25) #disc.rate
  j <- runif(1, min = 50, max = 50) #h0zt.temp
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
model_0.mc.sort$proj <- rep('Central Hidroeléctrica', 500)
colnames(model_0.mc.sort) <- c("npv", "prob", "model")

# Alternativa 1 - Case Base
model_1.mc.df <- data.frame(npv=numeric(0), irr=numeric(0))
for (i in 1:500) {
  # Distribuciones de probabilidad de cada parámetro
  a <- runif(1, min = Alt.1.vector[1], max = Alt.1.vector[1]) #land.cost
  b <- runif(1, min = Alt.1.vector[2]*0.95, max = Alt.1.vector[2]*1.25) #cons.cost
  c <- runif(1, min = Alt.1.vector[3]*0.95, max = Alt.1.vector[3]*1.25) #mgmt.cost
  d <- runif(1, min = Alt.1.vector[4]*0.95, max = Alt.1.vector[4]*1.25) #oper.cost
  e <- runif(1, min = Alt.1.vector[5]*0.95, max = Alt.1.vector[5]*1.25) #humr.cost
  f <- runif(1, min = Alt.1.vector[6]*0.95, max = Alt.1.vector[6]*1.25) #reco.cost
  g <- runif(1, min = Alt.1.vector[7]*0.75, max = Alt.1.vector[7]*1.05) #pric.inco
  h <- runif(1, min = Alt.1.vector[8]*0.85, max = Alt.1.vector[8]*1.05) #prod.data
  i <- runif(1, min = 0.035*0.75, max = 0.035*1.25) #disc.rate
  j <- runif(1, min = 50, max = 50) #h0zt.temp
  model_1.param <- c(a, b, c, d, e, f, g, h, i, j)
  # Llamada a la función y almacenar datos
  model_1.mc.df <- rbind(model_1.mc.df, mine.npv.fun(model_1.param))
}
# DATA PREPARATION
colnames(model_1.mc.df) <- c("npv")
model_1.mc.sort <- (model_1.mc.df)
model_1.mc.sort$npv <- model_1.mc.sort[order(model_0.mc.sort$npv),]
# Sequence by 1/num.cases, 1/500 = 0.002
model_1.mc.sort <- data.frame(model_1.mc.sort, seq(from = 0.002, to = 1, by = 0.002))
model_1.mc.sort$proj <- rep('Parque Eólico', 500)
colnames(model_1.mc.sort) <- c("npv", "prob", "model")


# Mezclar las distintas alternativas
#npv.df.mix <- model_0.mc.sort
npv.df.mix <- rbind(model_0.mc.sort, model_1.mc.sort)

# REPRESENTATION
library(ggplot2)

# Plot de funciones de densidad con más parámetros
ggplot(npv.df.mix) +
  geom_density(aes(x = npv, colour = model, fill = model), alpha=.75) +
  geom_line(aes(x = npv, colour = model), stat="density", size=0.65) + 
  geom_vline(xintercept=0, alpha=.75, size=0.5) +
  ggtitle("Valor Actualizado Neto, por fase de proyecto - Caso Base") +
  xlab("Net Present Value") + ylab("Probability") +
  theme(legend.position=c(0.25,0.65)) +
  scale_colour_discrete(guide=FALSE) +
  guides(fill=guide_legend(title=NULL)) +
  geom_vline(xintercept = Alt.0.npv, colour="red", size=0.65, linetype = "longdash") + 
  geom_vline(xintercept = Alt.1.npv, colour="green", size=0.65, linetype = "longdash")
