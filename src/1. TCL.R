
#----------------------------------------------------
#- Teorema Central Limite
#-------------------------------------------------

# Material preparado por: Giovany Babativa-Márquez
# Noviembre del 2024

#--------------------------------------------------

rm(list = ls())

library(pacman)
p_load(tidyverse, patchwork, glue, TeachingSampling)

#-------

filas <- 1000
columnas <- 100

set.seed(123456)

df <- as.data.frame(matrix(runif(filas * columnas), 
                           nrow = filas, ncol = columnas))

####### Distribución de X
dev.off()
dev.new()

p1 <- df |> ggplot(aes(x=V1)) + geom_histogram(bins = 10, color = "#000000", fill = "#0099F8")
p2 <- df |> ggplot(aes(x=V2)) + geom_histogram(bins = 10, color = "#000000", fill = "#0099F8")
p3 <- df |> ggplot(aes(x=V99)) + geom_histogram(bins = 10, color = "#000000", fill = "#0099F8")


p1 | (p2 / p3)


####### TCL: Distribución de Sum(X)

df2 <- df |> 
       summarise(across(where(is.numeric), ~sum(.))) |> 
       pivot_longer(everything(), names_to = "variable", values_to = "suma")

densidad <- function(data, n){
  data |> 
    slice(1:n) |> 
    ggplot(aes(x=suma)) + 
    geom_density(color = "#000000", fill = "#0099F8") +
    labs(title = glue("n = {n}"))
}

h5 <- densidad(df2, n = 5);      h10 <- densidad(df2, n = 10)
h15 <- densidad(df2, n = 15);    h20 <- densidad(df2, n = 20)
h25 <- densidad(df2, n = 25);    h30 <- densidad(df2, n = 30)
h50 <- densidad(df2, n = 50);    h60 <- densidad(df2, n = 60)
h80 <- densidad(df2, n = 80);    h100 <- densidad(df2, n = 100)

(h5 | h10 | h15 | h20) / (h25 | h30 | h50 | h60) / (h80 | h100)


#### ------- Giovany Babativa, PhD
#-- Este material ha sido creado por [Giovany Babativa-Márquez](https://github.com/jgbabativam) y es 
#-- de libre distribución bajo la licencia [Creative Commons Attribution-ShareAlike 4.0](https://creativecommons.org/licenses/by-sa/4.0/).
#
#
#-- Si se copia parcial o totalmente, debe citar la fuente como:
#   
# Babativa-Márquez, J.G. *Diapositivas del curso de muestreo probabilístico*. URL: https://jgbabativam.github.io/Muestreo-I/Semana2.html. 2024.