#----------------------------------------------------
#- Muestreo MAS
#-------------------------------------------------

# Material preparado por: Giovany Babativa-Márquez
# Noviembre del 2024

#--------------------------------------------------


marco <- tribble(
  ~consec,          ~nombre,    ~gasto,
  1,      "Alejandro",        53400,
  2,          "Jairo",       100000,
  3,         "Andrea",         7000,
  4,       "Katerine",        10000,
  5,          "Yeimy",        70000,
  6,          "David",        50000,
  7,          "Thais",        10000,
  8,     "John Jairo",       125000,
  9,          "Karol",        29000,
  10,        "Daniel",         2000
)

(verdad <- mean(marco$gasto)) 

N <- nrow(marco)
n <- 3            #tamaño de la muestra

choose(N, n)      #Cantidad de muestras posibles

##--- espacio muestral
omega <- data.frame(combn(1:N, n) |> t()) 

###===================== ALGORITMOS DE SELECCION
set.seed(2024-11-09)

#-- Coordinado negativo
marco$aleatorio <- runif(nrow(marco))

marco_ord <- marco |> 
  arrange(aleatorio) |> 
  mutate(ord_sel = 1:n())

muestra <- marco_ord |> 
  filter(ord_sel <=  n)

(estimacion <- mean(muestra$gasto))

#--- Fan-Muller
set.seed(12345)
muestra_FM <- TeachingSampling::S.SI(N = 10, n = 3)  

muestra <- marco |> 
  filter(row_number() %in% muestra_FM)

#--- sample
set.seed(12345)
sample(marco$nombre, size = 3)

##=========================== PROBABILIDAD DE INCLUSION

(Ik <-  omega |> 
    mutate(p_s = 1/choose(N, n)) |> 
    mutate(Ik = ifelse(X1 == 1 | X2 == 1 | X3== 1, 1, 0)))


(pi_1 <- sum(Ik$Ik*Ik$p_s))

n/N == pi_1