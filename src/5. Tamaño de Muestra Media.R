
### Tamaño de muestra para una media o un total


########################## PRUEBA PILOTO
library(pacman)
p_load(tidyverse, survey, srvyr,  haven, skimr,
       readxl, writexl, samplesize4surveys)

censo <- "https://github.com/jgbabativam/Muestreo-I/raw/main/datos/agpop.sav"
encuesta <- "https://github.com/jgbabativam/Muestreo-I/raw/main/datos/Agrsrs.sav"

marco <- read_sav(censo)
datos <- read_sav(encuesta)

####----- Seleccionar la muestra

set.seed(123456)

marco_ord <- marco |> 
  mutate(aleatorio = runif(n = nrow(marco))) |> 
  arrange(aleatorio) |> 
  mutate(consecutivo = 1:n())

muestra <- marco_ord |> 
  filter(consecutivo <=40)


sd <- sd(muestra$acres92)
media <- mean(muestra$acres92)

CV <- sd/media

### Este no usa el nivel de confianza

ss4m(N = nrow(marco), 
     mu = media, 
     sigma = sd, 
     error = "cve", 
     delta = 0.1)

### Con el nivel de confianza

n <- (1.96^2*sd^2)/((0.1*media)^2+(1.96^2*sd^2)/3078)
