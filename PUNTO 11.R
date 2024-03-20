https://us02web.zoom.us/j/83341839817?pwd=cjFndVN6WVdDVldnN21wQStMdmJuUT09

########################################
#Nombre y apellido: Sheila Desanze
#Numero de registro: 881575
########################################

#install.packages("calculus")
library(calculus)
#install.packages("rootSolve")
library(rootSolve)
library(ggplot2)




#### Punto 11 ----
#11) El precio de un bono es una función de la tasa de interés de acuerdo con la siguiente 
#relación. Utilizando el método de Newton-Raphson, halle la tasa que hace que el Precio sea 
#$101,50 (utilice una precisión de 10^-6)

#p(r) = 2.5 * ∑((1+0,5*r)^(-t),i,1,10) +100*(1+0.5*r)^(-10)= PM





### Planteo el precio de un bono es una función de la tasa de interés:

##El precio de un bono, como funcion en R:
punto11 <- function(r){
  
  primer_termino = 0
  
  for (i in 1:10){
    primer_termino = primer_termino + 2.5*(1+0.5*r)^(-i)
  }
  
  
  segunda_termino = 100*(1+0.5*r)^(-10)
  
  
  PM = primer_termino + segunda_termino 
}



##El precio de un bono, como expresion matematica:
funcionpunto11 <- expression(2.5*(1+0.5*r)^-1 + 2.5*(1+0.5*r)^-2 + 2.5*(1+0.5*r)^-3 + 
                     2.5*(1+0.5*r)^-4 + 2.5*(1+0.5*r)^-5 + 2.5*(1+0.5*r)^-6 + 
                     2.5*(1+0.5*r)^-7 + 2.5*(1+0.5*r)^-8 + 2.5*(1+0.5*r)^-9 + 
                     2.5*(1+0.5*r)^-10 + 100*(1+0.5*r)^(-10))


## DERIVADA del precio de un bono, como expresion matematica:
D(funcionpunto11,"r")


## DERIVADA del precio de un bono, como funcion en R:
derfuncionpunto11 <- function(r) {100 * ((1 + 0.5 * r)^((-10) - 1) * ((-10) * 0.5)) - 
                                  (2.5 * ((1 + 0.5 * r)^-(10 + 1) * (10 * 0.5)) + 
                                     (2.5 * ((1 + 0.5 * r)^-(9 +  1) * (9 * 0.5)) + 
                                        (2.5 * ((1 + 0.5 * r)^-(8 + 1) * (8 * 0.5)) + 
                                           (2.5 * ((1 + 0.5 * r)^-(7 + 1) * (7 * 0.5)) + 
                                              (2.5 * ((1 + 0.5 * r)^-(6 + 1) * (6 * 0.5)) + 
                                                 (2.5 * ((1 + 0.5 * r)^-(5 +  1) * (5 * 0.5)) +
                                                    (2.5 * ((1 + 0.5 * r)^-(4 + 1) * (4 * 0.5)) + 
                                                       (2.5 * ((1 + 0.5 * r)^-(3 + 1) * (3 * 0.5)) + 
                                                          (2.5 * ((1 + 0.5 * r)^-(2 + 1) * (2 * 0.5)) + 
                                                             2.5 * ((1 + 0.5 * r)^-(1 + 1) * 0.5))))))))))}



### Grafico el precio de un bono es una función de la tasa de interés:
tasas = seq(from = 0, to =0.3, by = 0.001)

P = punto11(tasas) #Precio del periodo 

plot(tasas,P, type = "l")
abline( h = 0)






### Resolucion de la ecuación con mediante Biseccion 
#Precio Teorico(TIR) = Precio de mercado
#f(r) = -PM + sum(CFt_i)*(1+r)^-t_i
Precio_punto11 <- 101.5

funcionTIR<-function(r){return(-Precio_punto11 + punto11(r))}

plot(tasas, funcionTIR(tasas),type = "l", col =  "red")
abline(h=0)













#!--------------------------------------------- Newton
Newton <- function(p0, tol, n = 100){
  #Donde p0 es la aproximación inicial
  #El número máximo de iteraciones n viene por default en 100
  #Y tol es la toleranacia al error
  
  for (i in 1:n) {
    
    #Calculo p
    p <- p0 - (funcionTIR(p0)/derfuncionpunto11(p0))
    
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
derfuncionpunto11

#--------------------------------------------- 








#!---------------------------------------------Newton---------------------------
Newton(p0 = 0.03, tol = 10^(-6))

this_could_go_wrong <- tryCatch(
  Newton(p0 = 0.03, tol = 10^(-6)),
  error = function(e){print("Error")})

if (this_could_go_wrong != "Error"){
  raiz <- this_could_go_wrong
  print(paste("La raiz es: ", raiz)) 
}

raiz
#-------------------------------------------------------------------------------


#na.omit(newton_raphson(f,df11,0.03,10^-6,10))

