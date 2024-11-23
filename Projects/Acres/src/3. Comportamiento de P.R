

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


