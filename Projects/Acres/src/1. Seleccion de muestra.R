#remotes::install_github("tidy-survey-r/srvyrexploR")
library(pacman)
p_load(tidyverse, survey, srvyr, remotes, haven, srvyrexploR, skimr,
       readxl, writexl)

censo <- "https://github.com/jgbabativam/Muestreo-I/raw/main/datos/agpop.sav"
encuesta <- "https://github.com/jgbabativam/Muestreo-I/raw/main/datos/Agrsrs.sav"

marco <- read_sav(censo)
datos <- read_sav(encuesta)

####----- Seleccionar la muestra

set.seed(2024-11-22)


marco_ord <- marco |> 
             mutate(aleatorio = runif(n = nrow(marco))) |> 
             arrange(aleatorio) |> 
             mutate(consecutivo = 1:n())

save(marco_ord, file="output/marco_ordenado.rds")

muestra <- marco_ord |> 
           filter(consecutivo <=300)
                    
write_xlsx(muestra, "output/muestra_MAS.xlsx")
             