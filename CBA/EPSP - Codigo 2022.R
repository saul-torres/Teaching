############################################################
# CÓDIGO PARA REALIZACIÓN DE ANÁLISIS COSTE-BENEFICIO EN R #
############################################################
#                  ACB-ENTREGA-CC-02                       #
############################################################

# ---------------------------------- #
# 1.- FUNCIÓN VALOR ACTUALIZADO NETO #
# ---------------------------------- #

cc.npv.fun <- function (vector.data) { 
  # VECTOR DATA : vector en el cual se incluyan los parámetros siguientes
  # (fijar, dejar como parámetros variables según deseos)
  # (se introducen como vector para el posterior análisis de sensibilidad)
  
  # COSTES #
  # ------ #
  # Construction costs
  cons.cost <- vector.data[1]
  
  # VALORES EXPUESTOS #
  # ----------------- #
  # Playa #1
  exp.beach1 <- vector.data[2]
  # Playa #2
  exp.beach2 <- vector.data[3]
  # Puerto
  exp.port <- vector.data[4]
  # Zona roja
  exp.red <- vector.data[5]
  # Zona amarilla
  exp.yell <- vector.data[6]
  # Zona verde
  exp.gree <- vector.data[7]
  
  
  # PARÁMETROS BÁSICOS #
  # ------------------ #
  # Tasa de descuento
  disc.rate <- vector.data[8]
  # Horizonte temporal 
  hozt.temp <- vector.data[9]
  
  
  # SERIES TEMPORALES #
  # ----------------- #
  # Años
  year.vector <- c(1:hozt.temp)
  
  # Construction costs
  cons.cost.vector <- rep(0, hozt.temp)
  cons.cost.vector[1] <- cons.cost
  
  # Expuestos
  exp.beach1.vector <- rep(0, hozt.temp)
  exp.beach2.vector <- rep(0, hozt.temp)
  exp.port.vector <- rep(0, hozt.temp)
  exp.red.vector <- rep(0, hozt.temp)
  exp.yell.vector <- rep(0, hozt.temp)
  exp.gree.vector <- rep(0, hozt.temp)
  for (i in 2:hozt.temp) {
    exp.beach1.vector[i] <- exp.beach1
    exp.beach2.vector[i] <- exp.beach2
    exp.port.vector[i] <- exp.port
    exp.red.vector[i] <- exp.red
    exp.yell.vector[i] <- exp.yell
    exp.gree.vector[i] <- exp.gree
  }
  
  
  # --------------------- # 
  # BENEFITS / Beneficios #
  # --------------------- #
  ben.vector <- rep(0, hozt.temp)
  netben.vector <- rep(0, hozt.temp)
  for (i in (1):(hozt.temp)) {
    ben.vector[i] <- -cons.cost.vector[i]-exp.beach1.vector[i]-exp.beach2.vector[i]-exp.port.vector[i]-
                     exp.red.vector[i]-exp.yell.vector[i]-exp.gree.vector[i]
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
# 3.- VALORES EXPUESTOS #
# --------------------- #

exp.beach1 <- 150000
exp.beach2 <- 95000
exp.port <- 0.85*2814.50*7.5
exp.red <- 0.85*((5*60*60*365*0.4)+(13*40*50*365*0.5)+(4*250000)+(300*25000))
exp.yell <- 0.75*((5*40*50*365*0.5)+(250*25000))
exp.gree <- 0.60*(1500000+(280*25000))

# -------------------- #
# 4.- VAN ALTERNATIVAS #
# -------------------- #

# Alternativa 0 - Do nothing
Alt.0.vector <- c(0, exp.beach1, exp.beach2, 
                  exp.port, exp.red, exp.yell, exp.gree,
                  0.03, 50)
Alt.0.npv <- cc.npv.fun(Alt.0.vector)

# Alternativa 1 - Ampliacion Paseo
Alt.1.vector <- c(25000000, exp.beach1, exp.beach2, 
                   exp.port, (1-0.80)*exp.red, (1-0.95)*exp.yell, (1-0.95)*exp.gree,
                   0.03, 50)
Alt.1.npv <- cc.npv.fun(Alt.1.vector)

# Alternativa 2 - Diques
Alt.2.vector <- c(15000000, exp.beach1, exp.beach2, 
                  exp.port, (1-0.75)*exp.red, (1-0.90)*exp.yell, (1-0.90)*exp.gree,
                  0.03, 50)
Alt.2.npv <- cc.npv.fun(Alt.2.vector)

# Alternativa 3 - Algas
Alt.3.vector <- c(800000, exp.beach1, exp.beach2, 
                  exp.port, (1-0.50)*exp.red, (1-0.50)*exp.yell, (1-0.50)*exp.gree,
                  0.03, 50)
Alt.3.npv <- cc.npv.fun(Alt.3.vector)

# Alternativa 4 - Ampliacion Paseo + Diques
Alt.4.vector <- c(25000000+15000000, exp.beach1, exp.beach2, 
                  exp.port, (1-0.80)*(1-0.75)*exp.red, (1-0.95)*(1-0.90)*exp.yell, (1-0.95)*(1-0.90)*exp.gree,
                  0.03, 50)
Alt.4.npv <- cc.npv.fun(Alt.4.vector)

# Alternativa 5 - Ampliacion Paseo + Algas
Alt.5.vector <- c(25000000+800000, exp.beach1, exp.beach2, 
                  exp.port, (1-0.80)*(1-0.50)*exp.red, (1-0.95)*(1-0.5)*exp.yell, (1-0.95)*(1-0.50)*exp.gree,
                  0.03, 50)
Alt.5.npv <- cc.npv.fun(Alt.5.vector)

# Alternativa 6 - Diques + Algas
Alt.6.vector <- c(15000000+800000, exp.beach1, exp.beach2, 
                  exp.port, (1-0.75)*(1-0.50)*exp.red, (1-0.90)*(1-0.5)*exp.yell, (1-0.90)*(1-0.50)*exp.gree,
                  0.03, 50)
Alt.6.npv <- cc.npv.fun(Alt.6.vector)

# Alternativa 7 - Ampliacion Paseo + Diques + Algas
Alt.7.vector <- c(25000000+15000000+800000, exp.beach1, exp.beach2, 
                  exp.port, (1-0.80)*(1-0.75)*(1-0.50)*exp.red, (1-0.95)*(1-0.90)*(1-0.5)*exp.yell, (1-0.95)*(1-0.90)*(1-0.50)*exp.gree,
                  0.03, 50)
Alt.7.npv <- cc.npv.fun(Alt.7.vector)

# ------------------------------------------ #
# 5.- EJECUCIÓN DEL ANÁLISIS DE SENSIBILIDAD #
# ------------------------------------------ #

# Alternativa 1 - Ampliacion Paseo
model_1.param <- Alt.1.vector
model_1.names <- c("cons.cost", "exp.beach1", "exp.beach2",
                   "exp.port", "exp.red", "exp.yell", "exp.gree",
                   "disc.rate", "hozt.temp")
model_1.sa.df <- anal.sensit(cc.npv.fun, model_1.param, model_1.names)
model_1.sa.df$project <- "Paseo"

# Alternativa 2 - Diques
model_2.param <- Alt.2.vector
model_2.names <- c("cons.cost", "exp.beach1", "exp.beach2",
                   "exp.port", "exp.red", "exp.yell", "exp.gree",
                   "disc.rate", "hozt.temp")
model_2.sa.df <- anal.sensit(cc.npv.fun, model_2.param, model_2.names)
model_2.sa.df$project <- "Diques"

# Alternativa 3 - Algas
model_3.param <- Alt.3.vector
model_3.names <- c("cons.cost", "exp.beach1", "exp.beach2",
                   "exp.port", "exp.red", "exp.yell", "exp.gree",
                   "disc.rate", "hozt.temp")
model_3.sa.df <- anal.sensit(cc.npv.fun, model_3.param, model_3.names)
model_3.sa.df$project <- "Algas"


# Manipular los datos para presentarlos
sa.data.df <- model_1.sa.df[-c(1,2,3,5),]
sa.data.df <- sa.data.df[,-c(1)]

sa.data.df <- rbind(model_1.sa.df, model_2.sa.df, model_3.sa.df)


# ----------------------- #
# 6.- ANÁLISIS MONTECARLO #
# ----------------------- #

# Alternativa 1 - Ampliacion Paseo
model_1.mc.df <- data.frame(npv=numeric(0), irr=numeric(0))
for (i in 1:500) {
  # Distribuciones de probabilidad de cada parámetro
  a <- runif(1, min = Alt.1.vector[1], max = Alt.1.vector[1]) #cons.cost
  b <- runif(1, min = Alt.1.vector[2]*0.75, max = Alt.1.vector[2]*1.25) #exp.beach1
  c <- runif(1, min = Alt.1.vector[3]*0.75, max = Alt.1.vector[3]*1.25) #exp.beach2
  d <- runif(1, min = Alt.1.vector[4]*0.75, max = Alt.1.vector[4]*1.25) #exp.port
  e <- runif(1, min = Alt.1.vector[5]*0.75, max = Alt.1.vector[5]*1.25) #exp.red
  f <- runif(1, min = Alt.1.vector[6]*0.75, max = Alt.1.vector[6]*1.25) #exp.yell
  g <- runif(1, min = Alt.1.vector[7]*0.75, max = Alt.1.vector[7]*1.25) #exp.gree
  h <- runif(1, min = Alt.1.vector[8]*0.75, max = Alt.1.vector[8]*1.25) #disc.rate
  i <- runif(1, min = Alt.1.vector[9], max = Alt.1.vector[9]) #h0zt.temp
  model_1.param <- c(a, b, c, d, e, f, g, h, i)
  # Llamada a la función y almacenar datos
  model_1.mc.df <- rbind(model_1.mc.df, cc.npv.fun(model_1.param))
}
# DATA PREPARATION
colnames(model_1.mc.df) <- c("npv")
model_1.mc.sort <- (model_1.mc.df)
model_1.mc.sort$npv <- model_1.mc.sort[order(model_1.mc.sort$npv),]
# Sequence by 1/num.cases, 1/500 = 0.004
model_1.mc.sort <- data.frame(model_1.mc.sort, seq(from = 0.002, to = 1, by = 0.002))
model_1.mc.sort$proj <- rep('Paseo', 500)
colnames(model_1.mc.sort) <- c("npv", "prob", "model")


# Alternativa 2 - Diques
model_2.mc.df <- data.frame(npv=numeric(0), irr=numeric(0))
for (i in 1:500) {
  # Distribuciones de probabilidad de cada parámetro
  a <- runif(1, min = Alt.2.vector[1], max = Alt.2.vector[1]) #cons.cost
  b <- runif(1, min = Alt.2.vector[2]*0.75, max = Alt.2.vector[2]*1.25) #exp.beach1
  c <- runif(1, min = Alt.2.vector[3]*0.75, max = Alt.2.vector[3]*1.25) #exp.beach2
  d <- runif(1, min = Alt.2.vector[4]*0.75, max = Alt.2.vector[4]*1.25) #exp.port
  e <- runif(1, min = Alt.2.vector[5]*0.75, max = Alt.2.vector[5]*1.25) #exp.red
  f <- runif(1, min = Alt.2.vector[6]*0.75, max = Alt.2.vector[6]*1.25) #exp.yell
  g <- runif(1, min = Alt.2.vector[7]*0.75, max = Alt.2.vector[7]*1.25) #exp.gree
  h <- runif(1, min = Alt.2.vector[8]*0.75, max = Alt.2.vector[8]*1.25) #disc.rate
  i <- runif(1, min = Alt.2.vector[9], max = Alt.2.vector[9]) #h0zt.temp
  model_2.param <- c(a, b, c, d, e, f, g, h, i)
  # Llamada a la función y almacenar datos
  model_2.mc.df <- rbind(model_2.mc.df, cc.npv.fun(model_2.param))
}
# DATA PREPARATION
colnames(model_2.mc.df) <- c("npv")
model_2.mc.sort <- (model_2.mc.df)
model_2.mc.sort$npv <- model_2.mc.sort[order(model_2.mc.sort$npv),]
# Sequence by 1/num.cases, 1/500 = 0.004
model_2.mc.sort <- data.frame(model_2.mc.sort, seq(from = 0.002, to = 1, by = 0.002))
model_2.mc.sort$proj <- rep('Diques', 500)
colnames(model_2.mc.sort) <- c("npv", "prob", "model")


# Alternativa 3 - Algas
model_3.mc.df <- data.frame(npv=numeric(0), irr=numeric(0))
for (i in 1:500) {
  # Distribuciones de probabilidad de cada parámetro
  a <- runif(1, min = Alt.3.vector[1], max = Alt.3.vector[1]) #cons.cost
  b <- runif(1, min = Alt.3.vector[2]*0.75, max = Alt.3.vector[2]*1.25) #exp.beach1
  c <- runif(1, min = Alt.3.vector[3]*0.75, max = Alt.3.vector[3]*1.25) #exp.beach2
  d <- runif(1, min = Alt.3.vector[4]*0.75, max = Alt.3.vector[4]*1.25) #exp.port
  e <- runif(1, min = Alt.3.vector[5]*0.75, max = Alt.3.vector[5]*1.25) #exp.red
  f <- runif(1, min = Alt.3.vector[6]*0.75, max = Alt.3.vector[6]*1.25) #exp.yell
  g <- runif(1, min = Alt.3.vector[7]*0.75, max = Alt.3.vector[7]*1.25) #exp.gree
  h <- runif(1, min = Alt.3.vector[8]*0.75, max = Alt.3.vector[8]*1.25) #disc.rate
  i <- runif(1, min = Alt.3.vector[9], max = Alt.3.vector[9]) #h0zt.temp
  model_3.param <- c(a, b, c, d, e, f, g, h, i)
  # Llamada a la función y almacenar datos
  model_3.mc.df <- rbind(model_3.mc.df, cc.npv.fun(model_3.param))
}
# DATA PREPARATION
colnames(model_3.mc.df) <- c("npv")
model_3.mc.sort <- (model_3.mc.df)
model_3.mc.sort$npv <- model_3.mc.sort[order(model_3.mc.sort$npv),]
# Sequence by 1/num.cases, 1/500 = 0.004
model_3.mc.sort <- data.frame(model_3.mc.sort, seq(from = 0.002, to = 1, by = 0.002))
model_3.mc.sort$proj <- rep('Algas', 500)
colnames(model_3.mc.sort) <- c("npv", "prob", "model")


# Alternativa 4 - Ampliacion Paseo + Diques
model_4.mc.df <- data.frame(npv=numeric(0), irr=numeric(0))
for (i in 1:500) {
  # Distribuciones de probabilidad de cada parámetro
  a <- runif(1, min = Alt.4.vector[1], max = Alt.4.vector[1]) #cons.cost
  b <- runif(1, min = Alt.4.vector[2]*0.75, max = Alt.4.vector[2]*1.25) #exp.beach1
  c <- runif(1, min = Alt.4.vector[3]*0.75, max = Alt.4.vector[3]*1.25) #exp.beach2
  d <- runif(1, min = Alt.4.vector[4]*0.75, max = Alt.4.vector[4]*1.25) #exp.port
  e <- runif(1, min = Alt.4.vector[5]*0.75, max = Alt.4.vector[5]*1.25) #exp.red
  f <- runif(1, min = Alt.4.vector[6]*0.75, max = Alt.4.vector[6]*1.25) #exp.yell
  g <- runif(1, min = Alt.4.vector[7]*0.75, max = Alt.4.vector[7]*1.25) #exp.gree
  h <- runif(1, min = Alt.4.vector[8]*0.75, max = Alt.4.vector[8]*1.25) #disc.rate
  i <- runif(1, min = Alt.4.vector[9], max = Alt.4.vector[9]) #h0zt.temp
  model_4.param <- c(a, b, c, d, e, f, g, h, i)
  # Llamada a la función y almacenar datos
  model_4.mc.df <- rbind(model_4.mc.df, cc.npv.fun(model_4.param))
}
# DATA PREPARATION
colnames(model_4.mc.df) <- c("npv")
model_4.mc.sort <- (model_4.mc.df)
model_4.mc.sort$npv <- model_4.mc.sort[order(model_4.mc.sort$npv),]
# Sequence by 1/num.cases, 1/500 = 0.004
model_4.mc.sort <- data.frame(model_4.mc.sort, seq(from = 0.002, to = 1, by = 0.002))
model_4.mc.sort$proj <- rep('Paseo + Diques', 500)
colnames(model_4.mc.sort) <- c("npv", "prob", "model")


# Alternativa 5 - Ampliacion Paseo + Algas
model_5.mc.df <- data.frame(npv=numeric(0), irr=numeric(0))
for (i in 1:500) {
  # Distribuciones de probabilidad de cada parámetro
  a <- runif(1, min = Alt.5.vector[1], max = Alt.5.vector[1]) #cons.cost
  b <- runif(1, min = Alt.5.vector[2]*0.75, max = Alt.5.vector[2]*1.25) #exp.beach1
  c <- runif(1, min = Alt.5.vector[3]*0.75, max = Alt.5.vector[3]*1.25) #exp.beach2
  d <- runif(1, min = Alt.5.vector[4]*0.75, max = Alt.5.vector[4]*1.25) #exp.port
  e <- runif(1, min = Alt.5.vector[5]*0.75, max = Alt.5.vector[5]*1.25) #exp.red
  f <- runif(1, min = Alt.5.vector[6]*0.75, max = Alt.5.vector[6]*1.25) #exp.yell
  g <- runif(1, min = Alt.5.vector[7]*0.75, max = Alt.5.vector[7]*1.25) #exp.gree
  h <- runif(1, min = Alt.5.vector[8]*0.75, max = Alt.5.vector[8]*1.25) #disc.rate
  i <- runif(1, min = Alt.5.vector[9], max = Alt.5.vector[9]) #h0zt.temp
  model_5.param <- c(a, b, c, d, e, f, g, h, i)
  # Llamada a la función y almacenar datos
  model_5.mc.df <- rbind(model_5.mc.df, cc.npv.fun(model_5.param))
}
# DATA PREPARATION
colnames(model_5.mc.df) <- c("npv")
model_5.mc.sort <- (model_5.mc.df)
model_5.mc.sort$npv <- model_5.mc.sort[order(model_5.mc.sort$npv),]
# Sequence by 1/num.cases, 1/500 = 0.004
model_5.mc.sort <- data.frame(model_5.mc.sort, seq(from = 0.002, to = 1, by = 0.002))
model_5.mc.sort$proj <- rep('Paseo + Algas', 500)
colnames(model_5.mc.sort) <- c("npv", "prob", "model")


# Alternativa 6 - Diques + Algas
model_6.mc.df <- data.frame(npv=numeric(0), irr=numeric(0))
for (i in 1:500) {
  # Distribuciones de probabilidad de cada parámetro
  a <- runif(1, min = Alt.6.vector[1], max = Alt.6.vector[1]) #cons.cost
  b <- runif(1, min = Alt.6.vector[2]*0.75, max = Alt.6.vector[2]*1.25) #exp.beach1
  c <- runif(1, min = Alt.6.vector[3]*0.75, max = Alt.6.vector[3]*1.25) #exp.beach2
  d <- runif(1, min = Alt.6.vector[4]*0.75, max = Alt.6.vector[4]*1.25) #exp.port
  e <- runif(1, min = Alt.6.vector[5]*0.75, max = Alt.6.vector[5]*1.25) #exp.red
  f <- runif(1, min = Alt.6.vector[6]*0.75, max = Alt.6.vector[6]*1.25) #exp.yell
  g <- runif(1, min = Alt.6.vector[7]*0.75, max = Alt.6.vector[7]*1.25) #exp.gree
  h <- runif(1, min = Alt.6.vector[8]*0.75, max = Alt.6.vector[8]*1.25) #disc.rate
  i <- runif(1, min = Alt.6.vector[9], max = Alt.6.vector[9]) #h0zt.temp
  model_6.param <- c(a, b, c, d, e, f, g, h, i)
  # Llamada a la función y almacenar datos
  model_6.mc.df <- rbind(model_6.mc.df, cc.npv.fun(model_6.param))
}
# DATA PREPARATION
colnames(model_6.mc.df) <- c("npv")
model_6.mc.sort <- (model_6.mc.df)
model_6.mc.sort$npv <- model_6.mc.sort[order(model_6.mc.sort$npv),]
# Sequence by 1/num.cases, 1/500 = 0.004
model_6.mc.sort <- data.frame(model_6.mc.sort, seq(from = 0.002, to = 1, by = 0.002))
model_6.mc.sort$proj <- rep('Diques + Algas', 500)
colnames(model_6.mc.sort) <- c("npv", "prob", "model")


# Alternativa 7 - Ampliación Paseo + Diques + Algas
model_7.mc.df <- data.frame(npv=numeric(0), irr=numeric(0))
for (i in 1:500) {
  # Distribuciones de probabilidad de cada parámetro
  a <- runif(1, min = Alt.7.vector[1], max = Alt.7.vector[1]) #cons.cost
  b <- runif(1, min = Alt.7.vector[2]*0.75, max = Alt.7.vector[2]*1.25) #exp.beach1
  c <- runif(1, min = Alt.7.vector[3]*0.75, max = Alt.7.vector[3]*1.25) #exp.beach2
  d <- runif(1, min = Alt.7.vector[4]*0.75, max = Alt.7.vector[4]*1.25) #exp.port
  e <- runif(1, min = Alt.7.vector[5]*0.75, max = Alt.7.vector[5]*1.25) #exp.red
  f <- runif(1, min = Alt.7.vector[6]*0.75, max = Alt.7.vector[6]*1.25) #exp.yell
  g <- runif(1, min = Alt.7.vector[7]*0.75, max = Alt.7.vector[7]*1.25) #exp.gree
  h <- runif(1, min = Alt.7.vector[8]*0.75, max = Alt.7.vector[8]*1.25) #disc.rate
  i <- runif(1, min = Alt.7.vector[9], max = Alt.7.vector[9]) #h0zt.temp
  model_7.param <- c(a, b, c, d, e, f, g, h, i)
  # Llamada a la función y almacenar datos
  model_7.mc.df <- rbind(model_7.mc.df, cc.npv.fun(model_7.param))
}
# DATA PREPARATION
colnames(model_7.mc.df) <- c("npv")
model_7.mc.sort <- (model_7.mc.df)
model_7.mc.sort$npv <- model_7.mc.sort[order(model_7.mc.sort$npv),]
# Sequence by 1/num.cases, 1/500 = 0.004
model_7.mc.sort <- data.frame(model_7.mc.sort, seq(from = 0.002, to = 1, by = 0.002))
model_7.mc.sort$proj <- rep('Paseo + Diques + Algas', 500)
colnames(model_7.mc.sort) <- c("npv", "prob", "model")



# Mezclar las distintas alternativas
npv.df.mix <- rbind(model_1.mc.sort, model_2.mc.sort, model_3.mc.sort,
                    model_4.mc.sort, model_5.mc.sort, model_6.mc.sort, model_7.mc.sort)

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
