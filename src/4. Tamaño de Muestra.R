
rm(list = ls())
#----------------------

library(samplesize4surveys)


###############

p <- seq(0, 1, by = 0.02)
q <- p*(1-p)


df <- data.frame(p = p, q = q)


df |> 
  ggplot(aes(x = p, y = q, group = 1)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  labs(x = "P", y = "P*(1-P)") +
  geom_vline(xintercept = 0.5, linetype = 2)


####-------------------------

samplesize4surveys::ss4p(N = 1251, P = 0.5, error = "me", delta = 0.03)



##--------------------

error <- seq(0.01, 0.15, 0.005)

df1 <- data.frame(error = error) |> 
       mutate(nMuestra = samplesize4surveys::ss4p(N = 1251, 
                                                  P = 0.5, 
                                                  error = "me", 
                                                  delta = error))


df1 |> 
  ggplot(aes(x = nMuestra, y = error, group = 1)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 0.15, by = 0.005)) +
  scale_x_continuous(breaks = seq(0, 1200, by = 100))
  



N <- seq(1000, 50000, by = 500)

df2 <- data.frame(N = N) |> 
       mutate(nMuestra = samplesize4surveys::ss4p(N = N, 
                                                  P = 0.5, 
                                                  error = "me", 
                                                  delta = 0.03))


df2 |> 
  ggplot(aes(x = N, y = nMuestra, group = 1)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 50000, by = 5000)) +
  labs(title = "Tamaños de muestra con error del 3% cuando cambia N ")
