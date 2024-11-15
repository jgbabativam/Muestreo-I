
#----------------------------------------------------
#- Muestreo BERNOULLI
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


# Espacio Muestral
omega_ber <- data.frame(SupportRS(10))

# Seleccion
pi <- 0.3

### Cantidad de elementos en la muestra
omega_ber$ns <- rowSums(!is.na(omega_ber))

### Probabilidad de la muestra
dis_ber <- omega_ber |> 
           mutate(p_s = pi^ns*(1-pi)^(10-ns))


sum(dis_ber$p_s)

##=========================== PROBABILIDAD DE INCLUSION

(Ik <- dis_ber |> 
       mutate(Ik = if_any(X1:X10, ~ . == 1) * 1) |> 
       replace_na(list(Ik = 0)))


(pi_1 <- sum(Ik$Ik*Ik$p_s))

##---- Estimador Ybarra

rees <- Ik |> 
        mutate(X1 = ifelse(row_number() == 1, 0, X1)) |> 
        mutate(muestra = 1:n()) |> 
        pivot_longer(cols = starts_with("X"),
                     names_to = "Ind",
                     values_to = "consec",
                     values_drop_na = T) |> 
        left_join(marco) |> 
        group_by(muestra) |> 
        mutate(y = paste0("y", 1:n())) |> 
        ungroup()



df1 <- rees |> 
  select(-consec, -gasto, -y) |> 
  pivot_wider(names_from = Ind,  values_from = nombre)

df2 <- rees |> 
  select(muestra, y, gasto) |> 
  pivot_wider(names_from = y,  values_from = gasto)


omega_ber <- left_join(df1, df2, by = "muestra")

####.----- Estimador

estim <- omega_ber |> 
  rowwise() |> 
  mutate(media = mean(c(y1,y2,y3), na.rm = T)) 

sum(estim$p_s * estim$media, na.rm = T)

verdad
