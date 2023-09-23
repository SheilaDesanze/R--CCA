########################################
#Nombre y apellido: Sheila Desanze
#Numero de registro: 881575
########################################

#install.packages("calculus")
library(calculus)
#install.packages("rootSolve")
library(rootSolve)
library(ggplot2)


#PUNTO 6:
#Sean 𝑓(𝑥) = −𝑥^3 − cos (𝑥) 
#con 𝑝0 = −1. 
#Aplique la fórmula de iteración de Newton para encontrar 𝑝2.
#¿Podríamos utilizar 𝑝0 = 0?
  

#datos:
f <- function(x){
  return(-x^3 -cos(x))
}
exactitud = (10^(-2)) #tolerancia
p0dato = -1



#!--------------------------------------------- Newton
Newton <- function(p0, tol, n = 100){
  #Donde p0 es la aproximación inicial
  #El número máximo de iteraciones n viene por default en 100
  #Y tol es la toleranacia al error
  
  for (i in 1:n) {
    
    #Calculo p
    p <- p0 - (f(p0)/fprima(p0))
    
    if(abs(p-p0) <= tol){
      return(p)
    }
    
    p0 <- p
    
  }
  
  #En el caso de que falle el método
  return(paste('El método falla luego de: ', n, ' iteraciones'))
}

#--------------------------------------------- 





#!--------------------------------------------- PASO : derivada
# Calculo la derivada
(fprima <- D(expression(-x^3 -cos(x)), "x"))

# Defino la funcion
fprima <- function(x) {
  return(-(3 * x^2 - sin(x)))
}

#--------------------------------------------- 








#!---------------------------------------------Newton---------------------------
Newton(p0 = p0dato, tol = exactitud)

this_could_go_wrong <- tryCatch(
  Newton(p0 = p0dato, tol = exactitud),
  error = function(e){print("Error")})

if (this_could_go_wrong != "Error"){
  raiz <- this_could_go_wrong
  print(paste("La raiz es: ", raiz)) 
}

raiz
#-------------------------------------------------------------------------------







#-x^3 -cos(x))
#-(3 * x^2 - sin(x))
lim_a = -3
lim_b = 3

#---------Grafico F'''(x):---------------------
gprima <- function(x) {
  return(-x^3 -cos(x))           #---------------------------------------------ingresar funcion a mano!!!!!!!!!!!!
}
# Crea un data frame con valores de x y f(x)
dfgraf2 <- data.frame(x = seq(-5, 10, by = 0.01),
                      gprimax = gprima(seq(-5, 10, by = 0.01)))
# Crea el gráfico con ggplot2
ggplot(data = dfgraf2 , aes(x = x, y = gprimax)) +
  geom_line(color = "blue") +
  geom_hline(yintercept = 0, color = "gray") +  # Agrega línea horizontal en y = 0
  geom_vline(xintercept = 0, color = "gray") +  # Agrega línea vertical en x = 0
  labs(x = "x", y = "F'''(x)", title = "Gráfico de la funcion DERIVADA 3ra= exp(x) + 2^(-x) * log(2) * log(2) - 2 * cos(x)")+
  geom_point(data = data.frame(x = c(lim_a, lim_b), gprimax = c(0, 0)),
             aes(x = x, y = gprimax), color = "red", size = 3) +
  geom_text(data = data.frame(x = c(lim_a, lim_b), gprimax = c(0, 0),
                              label = c(paste0("a ", round(lim_a, 2)), paste0("b ", round(lim_b, 2)))),
            aes(x = x, y = gprimax, label = label), vjust = -0.5, color = "red")+
  scale_x_continuous(limits = c(-3, 3), breaks = seq(-3, 3, by = 0.5)) +
  scale_y_continuous(limits = c(-3, 3), breaks = seq(-3, 3, by = 0.5))

#----------------------------------------


