################################----
### Tarea 1. Selección de la muestra de hogares
###  Diseño MAS(35, 10)


rm(list = ls())

library(pacman)
p_load(tidyverse, janitor, readxl, writexl)

#### Tamaño de la muestra

n <- 10

#### Marco

marco <- read_excel("data/Ejemplo Hogares - Marco Muestral.xls")

set.seed(2024-11-22)
marco_ord <- marco |> 
             mutate(aleatorio = runif(n = nrow(marco))) |> 
             arrange(aleatorio)

save(marco_ord, file = "output/marco_ordenado.rds")

muestra <- marco_ord |> 
           slice(1:n)

write_xlsx(muestra, "output/Muestra Seleccionada.xlsx")
