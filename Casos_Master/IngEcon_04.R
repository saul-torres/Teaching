CPLEX <- function(model, method="CPLEX_LP", decimal=0){
  
  library(Rglpk)
  
  x <- Rglpk_read_file(model, type = method, verbose=F)
  
  x.sol <- Rglpk_solve_LP(x$objective, x$constraints[[1]], x$constraints[[2]], x$constraints[[3]], x$bounds, x$types, x$maximum)
  
  if (x.sol$status > 0) {return ("Status: Optimal solution has not been found")}
  
  x.sol.df <- as.data.frame(x.sol$solution)
  x.sol.df <- rbind(x.sol.df, (x.sol$optimum))
  
  rownames(x.sol.df) <- c(attr(x, "objective_vars_names"),"obj")
  colnames(x.sol.df) <- "Solution"
  
  return(x.sol.df)
}

#CPLEX("ruta/ProgLineal.txt")
