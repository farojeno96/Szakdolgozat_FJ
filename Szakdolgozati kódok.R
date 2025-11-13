library(tidyverse)
library(reshape2)
library(gridExtra)
library(latex2exp)

setwd("C:/Users/Jenő/Desktop/ELTE/2025 ősz - Survey statisztika/Szakdolgozat")

# Entrópiák előkészítése

g_1 <- function(x){
  return((1 - x)/x)
}

g_1_inv <- function(x){
  return(1/(1 + x))
}

# Ellenőrzés:
g_1_inv(g_1(0.95))

g_2 <- function(x) {
  return(-log(x))
}

g_2_inv <- function(x) {
  return(exp(-x))
}

# Ellenőrzés:
g_2_inv(g_2(0.45))

g_3 <- function(x, alpha){
  return(((1 - x)/x)^alpha)
}

g_3_inv <- function(x, alpha){
  return(1/(1 + x^(1/alpha)))
}

# Ellenőrzés:
g_3_inv(g_3(0.275, alpha = 2), alpha = 2)


# A fuzzy-entrópiát megvalósító függvény

Fuzzy_entropy <- function(x, generator_name, alpha = NULL){
  if(generator_name == "g_1"){
    g <- g_1
    g_inv <- g_1_inv
    result <- 2 * g_inv(0.5 * (g(x) + g(1 - x)))
  } else if(generator_name == "g_2"){
    g <- g_2
    g_inv <- g_2_inv
    result <- 2 * g_inv(0.5 * (g(x) + g(1 - x)))
  } else if(generator_name == "g_3"){
    if(is.null(alpha)) stop("Az 'alpha' paraméter kötelező a g_3 esetén.")
    g <- function(x) g_3(x, alpha)
    g_inv <- function(x) g_3_inv(x, alpha)
    result <- 2 * g_inv(0.5 * (g(x) + g(1 - x)))
  } else {
    stop("Ismeretlen generátorfüggvény.")
  }
  return(result)
}

# Ellenőrzés:
Fuzzy_entropy(0.5, generator_name = "g_1")
Fuzzy_entropy(0.45, generator_name = "g_2")
Fuzzy_entropy(0.275, generator_name = "g_3", alpha = 2)


# 2. ábra -----------------------------------------------------------------


x_s <- seq(0,1, by = 0.01)
F_x_s_1 <- Fuzzy_entropy(x_s, generator_name = "g_3", alpha = 1)
F_x_s_2 <- Fuzzy_entropy(x_s, generator_name = "g_3", alpha = 1/2)
#plot(x_s, F_x_s, type = "l")
#lines(x_s, Fuzzy_entropy(x_s, generator_name = "g_3", alpha = 5), col = "green")
#lines(x_s, Fuzzy_entropy(x_s, generator_name = "g_3", alpha = 1/2), col = "red")

data_1 <- as.data.frame(cbind(x_s, F_x_s_1, F_x_s_2))

data_long <- data_1 %>% melt(id.vars = "x_s")

data_long %>% ggplot(aes(x_s, value, color = variable)) + geom_line(lwd = 1.2) + 
  labs(x = TeX("$\\x$"), y = TeX("$\\F(x)$"), color = "Jelmagyarázat") + 
  scale_color_manual(
    values = c(
      "F_x_s_1" = "red", 
      "F_x_s_2" = "green"
    ),
    labels = TeX(c(
      "F_x_s_1" = "\\F(x) = 4x(1-x)",
      "F_x_s_2" = "\\F(x) = 2\\sqrt{x(1-x)}"
    ))
  ) +
  theme(
    axis.title.x = element_text(size = 20), # X tengely cím
    axis.title.y = element_text(size = 20), # Y tengely cím
    axis.text.x = element_text(size = 18),                # X tengely feliratok
    axis.text.y = element_text(size = 18),                # Y tengely feliratok
    legend.title = element_text(size = 18),# Legenda cím
    legend.text = element_text(size = 18),                # Legenda
    panel.background = element_rect(fill = "white"),
    panel.grid = element_line(color = "grey"),
    axis.line = element_line(color = "black", linewidth = 1.2, arrow = arrow(ends = "last", type = "closed", length = unit(0.1, "inches"))),
    legend.background = element_rect(linewidth = 1.2, colour = "black")
  )


# nu-maximális vagueness entrópia -----------------------------------------


eta_g_nu_x <- function(x, generator_name, nu){
  if(generator_name == "g_1"){
    g <- g_1
    g_inv <- g_1_inv
  } else if(generator_name == "g_2"){
    g <- g_2
    g_inv <- g_2_inv
  } else if(generator_name == "g_3"){
    if(is.null(alpha)) stop("Az 'alpha' paraméter kötelező a g_3 esetén.")
    g <- function(x) g_3(x, alpha)
    g_inv <- function(x) g_3_inv(x, alpha)
  } else {
    stop("Ismeretlen generátorfüggvény.")
  }
  return(g_inv((g(nu)^2)/g(x)))
}

# Ellenőrzés:
eta_g_nu_x(0.9, generator_name = "g_1", nu = 0.5)
# tagadás tagadása:
eta_g_nu_x(eta_g_nu_x(0.9, generator_name = "g_1", nu = 0.5), generator_name = "g_1", nu = 0.5)



Fuzzy_entropy_nu <- function(x, generator_name, alpha = NULL, nu = 0.5){
  if(generator_name == "g_1"){
    g <- g_1
    g_inv <- g_1_inv
  } else if(generator_name == "g_2"){
    g <- g_2
    g_inv <- g_2_inv
  } else if(generator_name == "g_3"){
    if(is.null(alpha)) stop("Az 'alpha' paraméter kötelező a g_3 esetén.")
    g <- function(x) g_3(x, alpha)
    g_inv <- function(x) g_3_inv(x, alpha)
  } else {
    stop("Ismeretlen generátorfüggvény.")
  }
  eta_nu_x <- eta_g_nu_x(x, generator_name, nu = nu)
  result <- (1/nu)*g_inv(0.5*(g(x) + g(eta_nu_x)))
  return(result)
}

# Ellenőrzés:
Fuzzy_entropy_nu(0.1, generator_name = "g_1", alpha = 2, nu = 0.2)



# 4. ábra -----------------------------------------------------------------


x_s_1 <- seq(0,1, by = 0.01)
F_x_nu_s_1 <- Fuzzy_entropy_nu(x_s_1, generator_name = "g_3", alpha = 1, nu = 0.5)
F_x_nu_s_2 <- Fuzzy_entropy_nu(x_s_1, generator_name = "g_3", alpha = 1, nu = 0.1)
F_x_nu_s_3 <- Fuzzy_entropy_nu(x_s_1, generator_name = "g_3", alpha = 1, nu = 0.9)

plot(x_s_1, F_x_s_1, type = "l")
lines(x_s_1, F_x_s_1 <- Fuzzy_entropy_nu(x_s_1, generator_name = "g_3", alpha = 1, nu = 0.1), col = "green")
lines(x_s_1, F_x_s_1 <- Fuzzy_entropy_nu(x_s_1, generator_name = "g_3", alpha = 1, nu = 0.9), col = "red")


data_2 <- as.data.frame(cbind(x_s_1, F_x_nu_s_1, F_x_nu_s_2, F_x_nu_s_3))

data_long <- data_2 %>% melt(id.vars = "x_s_1")

data_long %>% ggplot(aes(x_s_1, value, color = variable)) + geom_line(lwd = 1.2) + 
  labs(x = TeX("$\\x$"), y = TeX("$\\F_{nu}(x)$"), color = "Jelmagyarázat") + 
  scale_color_manual(
    values = c(
      "F_x_nu_s_1" = "red", 
      "F_x_nu_s_2" = "green",
      "F_x_nu_s_3" = "blue"
    ),
    labels = TeX(c(
      "F_x_nu_s_1" = "\\nu = 0.5",
      "F_x_nu_s_2" = "\\nu = 0.1",
      "F_x_nu_s_3" = "\\nu = 0.9"
    ))
  ) +
  theme(
    axis.title.x = element_text(size = 20), # X tengely cím
    axis.title.y = element_text(size = 20), # Y tengely cím
    axis.text.x = element_text(size = 18),                # X tengely feliratok
    axis.text.y = element_text(size = 18),                # Y tengely feliratok
    legend.title = element_text(size = 18),# Legenda cím
    legend.text = element_text(size = 18),                # Legenda
    panel.background = element_rect(fill = "white"),
    panel.grid = element_line(color = "grey"),
    axis.line = element_line(color = "black", linewidth = 1.2, arrow = arrow(ends = "last", type = "closed", length = unit(0.1, "inches"))),
    legend.background = element_rect(linewidth = 1.2, colour = "black")
  )

# Két alaki paraméterrel ellátott entrópia --------------------------------


f <- function(nu_1, x){
  value <- 1/(1 + (1-nu_1)*(1-2*x)/(nu_1*2*x))
  return(min(max(value,0), 1))
}

g <- function(nu_2, x){
  value <- 1/(1 + (1-nu_2)*(2*x - 1)/(nu_2*2*(1 - x)))
  return(min(max(value,0), 1))
}

h <- function(x, nu_1, nu_2) {
  a <- f(nu_1, x)
  b <- g(nu_2, x)
  value <- 1/(1 + (1 - a)/a + (1 - b)/b)
  return(value)
}

h(0.3, 0.5, 0.7)

x_values <- seq(0,1,0.001)
F_values_1 <- NULL
for(i in 1:length(x_values)) {
  F_values_1[i] <- h(x_values[i], nu_1 = 0.9, nu_2 = 0.35)
}

plot(x_values, F_values_1, type = "l")


# 3. ábra -----------------------------------------------------------------


x_s_2 <- seq(0,1, by = 0.01)
F_x_nu_1_nu_2_s_1 <- NULL
for(i in 1:length(x_s_2)) {
  F_x_nu_1_nu_2_s_1[i] <- h(x_s_2[i], nu_1 = 0.99, nu_2 = 0.35)
}
F_x_nu_1_nu_2_s_2 <- NULL
for(i in 1:length(x_s_2)) {
  F_x_nu_1_nu_2_s_2[i] <- h(x_s_2[i], nu_1 = 0.95, nu_2 = 0.65)
}
F_x_nu_1_nu_2_s_3 <- NULL
for(i in 1:length(x_s_2)) {
  F_x_nu_1_nu_2_s_3[i] <- h(x_s_2[i], nu_1 = 0.9, nu_2 = 0.45)
}
F_x_nu_1_nu_2_s_4 <- NULL
for(i in 1:length(x_s_2)) {
  F_x_nu_1_nu_2_s_4[i] <- h(x_s_2[i], nu_1 = 0.85, nu_2 = 0.55)
}


data_3 <- as.data.frame(cbind(x_s_2, F_x_nu_1_nu_2_s_1, F_x_nu_1_nu_2_s_2, F_x_nu_1_nu_2_s_3, F_x_nu_1_nu_2_s_4))

data_long <- data_3 %>% melt(id.vars = "x_s_2")

data_long %>% ggplot(aes(x_s_2, value, color = variable)) + geom_line(lwd = 1.2) + 
  labs(x = TeX("$\\x$"), y = TeX("$\\F_{nu_1,nu_2}(x)$"), color = "Jelmagyarázat") + 
  scale_color_manual(
    values = c(
      "F_x_nu_1_nu_2_s_1" = "red", 
      "F_x_nu_1_nu_2_s_2" = "green",
      "F_x_nu_1_nu_2_s_3" = "blue",
      "F_x_nu_1_nu_2_s_4" = "orange"
    ),
    labels = TeX(c(
      "F_x_nu_1_nu_2_s_1" = "$\\nu_{1}=0.99,\\ \\nu_{2}=0.35$",
      "F_x_nu_1_nu_2_s_2" = "$\\nu_{1}=0.95,\\ \\nu_{2}=0.65$",
      "F_x_nu_1_nu_2_s_3" = "$\\nu_{1}=0.90,\\ \\nu_{2}=0.45$",
      "F_x_nu_1_nu_2_s_4" = "$\\nu_{1}=0.85,\\ \\nu_{2}=0.55$"
    ))
  ) +
  theme(
    axis.title.x = element_text(size = 20), # X tengely cím
    axis.title.y = element_text(size = 20), # Y tengely cím
    axis.text.x = element_text(size = 18),                # X tengely feliratok
    axis.text.y = element_text(size = 18),                # Y tengely feliratok
    legend.title = element_text(size = 18),# Legenda cím
    legend.text = element_text(size = 18),                # Legenda
    panel.background = element_rect(fill = "white"),
    panel.grid = element_line(color = "grey"),
    axis.line = element_line(color = "black", linewidth = 1.2, arrow = arrow(ends = "last", type = "closed", length = unit(0.1, "inches"))),
    legend.background = element_rect(linewidth = 1.2, colour = "black")
  )



# 6. ábra -----------------------------------------------------------------

x_s <- seq(-30,50,0.0001)
mu <- 5
sigma_1 <- 1
sigma_2 <- 5
sigma_3 <- 15
sigma_4 <- 100

y_1 <- pnorm(x_s, mu, sigma_1)
y_2 <- pnorm(x_s, mu, sigma_2)
y_3 <- pnorm(x_s, mu, sigma_3)
y_4 <- pnorm(x_s, mu, sigma_4)

#data <- as.data.frame(cbind(x = seq(1, length(x_s), by = 1), x_s, y_1, y_2, y_3, y_4))

data_1 <- as.data.frame(cbind(x_s, y_1, y_2, y_3, y_4))

data_long <- data_1 %>% melt(id.vars = "x_s")


data_long %>% ggplot(aes(x_s, value, color = variable)) + geom_line(lwd = 1.2) + 
  labs(x = TeX("$\\x$"), y = TeX("$\\G(x)$"), color = "Jelmagyarázat") + 
  scale_color_manual(
    values = c(
      "y_1" = "red", 
      "y_2" = "brown", 
      "y_3" = "green", 
      "y_4" = "purple"
    ),
    labels = TeX(c(
      "y_1" = "\\sigma = 1",
      "y_2" = "\\sigma = 5",
      "y_3" = "\\sigma = 15",
      "y_4" = "\\sigma = 1000"
    ))
  ) +
  theme(
    axis.title.x = element_text(size = 20), # X tengely cím
    axis.title.y = element_text(size = 20), # Y tengely cím
    axis.text.x = element_text(size = 18),                # X tengely feliratok
    axis.text.y = element_text(size = 18),                # Y tengely feliratok
    legend.title = element_text(size = 18),# Legenda cím
    legend.text = element_text(size = 18),                # Legenda
    panel.background = element_rect(fill = "white"),
    panel.grid = element_line(color = "grey"),
    axis.line = element_line(color = "black", linewidth = 1.2, arrow = arrow(ends = "last", type = "closed", length = unit(0.1, "inches"))),
    legend.background = element_rect(linewidth = 1.2, colour = "black")
  ) 



# 7. ábra -----------------------------------------------------------------


# Írjunk függvényt, amely a megadott normális eloszláshoz, c paraméterhez és fuzzy entrópiához elvégzi 
# a kockázatossági index definíciójában szereplő integrálást numerikusan.

fuzzy_entropy_based_riskiness_index <- function(mu, sigma, c, number_of_intervals = 1000, fuzzy_entropy = NULL){
  lower_limit <- mu - c
  upper_limit <- mu + c
  endpoints <- seq(lower_limit, upper_limit, length.out = number_of_intervals)
  data <- as.data.frame(matrix(rep(0,length(endpoints)*5), nrow = length(endpoints), ncol = 5))
  data[ ,1] <- endpoints
  for (i in 1:length(endpoints)) {
    data[i, 2] <- pnorm(data[i, 1], mean = mu, sd = sigma)
    data[i, 3] <- fuzzy_entropy(data[i, 2])
  }
  for (j in 2:length(endpoints)) {
    data[j, 4] <- mean(data[j - 1, 3], data[j, 3])
    data[j, 5] <- data[j, 4] * (data[j, 1] - data[j - 1, 1])
  }
  return(sum(data[, 5])/(2*c))
}

# Ellenőrzés
fuzzy_entropy_based_riskiness_index(4,1,150,1000, fuzzy_entropy = function(x) {4*x*(1-x)})


# Vessük össze a különböző c-k melletti ábrát.

sigma_values <- seq(0,150, 0.5)
data_1 <- as.data.frame(matrix(0, nrow = length(sigma_values), ncol = 4))
data_1[,1] <- sigma_values
for (i in 1:length(sigma_values)) {
  data_1[i,2] <- fuzzy_entropy_based_riskiness_index(4, data_1[i, 1], 150, 1000, fuzzy_entropy = function(x) {4*x*(1-x)})
  data_1[i,3] <- fuzzy_entropy_based_riskiness_index(4, data_1[i, 1], 100, 1000, fuzzy_entropy = function(x) {4*x*(1-x)})
  data_1[i,4] <- fuzzy_entropy_based_riskiness_index(4, data_1[i, 1], 15, 1000, fuzzy_entropy = function(x) {4*x*(1-x)})
}

names(data_1) <- c("sigma", "R_1", "R_2", "R_3")

data_1_long <- data_1 %>% melt(id.vars = "sigma")


plot_1 <- data_1_long %>% ggplot(aes(sigma, value, color = variable)) + geom_line(lwd = 1.2) + 
  labs(title = TeX("$\\F(x) = 4x(1-x)$"), x = TeX("$\\sigma$"), y = TeX("$\\R_{F, c}(xi_{mu_0, sigma})$"), color = "Jelmagyarázat") + 
  scale_color_manual(
    values = c(
      "R_1" = "red",
      "R_2" = "green", 
      "R_3" = "purple"
    ),
    labels = TeX(c(
      "R_1" = "\\c = 150",
      "R_2" = "\\c = 100",
      "R_3" = "\\c = 15"
    ))
  ) +
  theme(
    axis.title.x = element_text(size = 20), # X tengely cím
    axis.title.y = element_text(size = 20), # Y tengely cím
    axis.text.x = element_text(size = 18),                # X tengely feliratok
    axis.text.y = element_text(size = 18),                # Y tengely feliratok
    legend.title = element_text(size = 18),# Legenda cím
    legend.text = element_text(size = 18),                # Legenda
    panel.background = element_rect(fill = "white"),
    panel.grid = element_line(color = "grey"),
    axis.line = element_line(color = "black", linewidth = 1.2, arrow = arrow(ends = "last", type = "closed", length = unit(0.1, "inches"))),
    legend.background = element_rect(linewidth = 1.2, colour = "black")
  )


sigma_values <- seq(0,150, 0.5)
data_2 <- as.data.frame(matrix(0, nrow = length(sigma_values), ncol = 4))
data_2[,1] <- sigma_values
for (i in 1:length(sigma_values)) {
  data_2[i,2] <- fuzzy_entropy_based_riskiness_index(4, data_1[i, 1], 150, 1000, fuzzy_entropy = function(x) {2*sqrt(x*(1-x))})
  data_2[i,3] <- fuzzy_entropy_based_riskiness_index(4, data_1[i, 1], 100, 1000, fuzzy_entropy = function(x) {2*sqrt(x*(1-x))})
  data_2[i,4] <- fuzzy_entropy_based_riskiness_index(4, data_1[i, 1], 15, 1000, fuzzy_entropy = function(x) {2*sqrt(x*(1-x))})
}


names(data_2) <- c("sigma", "R_1", "R_2", "R_3")
data_2_long <- data_2 %>% melt(id.vars = "sigma")


plot_2 <- data_2_long %>% ggplot(aes(sigma, value, color = variable)) + geom_line(lwd = 1.2) + 
  labs(title = TeX("$\\F(x) = 2sqrt(x(1-x))$"), x = TeX("$\\sigma$"), y = TeX("$\\R_{F, c}(xi_{mu_0, sigma})$"), color = "Jelmagyarázat") + 
  scale_color_manual(
    values = c(
      "R_1" = "red",
      "R_2" = "green", 
      "R_3" = "purple"
    ),
    labels = TeX(c(
      "R_1" = "\\c = 150",
      "R_2" = "\\c = 100",
      "R_3" = "\\c = 15"
    ))
  ) +
  theme(
    axis.title.x = element_text(size = 20), # X tengely cím
    axis.title.y = element_text(size = 20), # Y tengely cím
    axis.text.x = element_text(size = 18),                # X tengely feliratok
    axis.text.y = element_text(size = 18),                # Y tengely feliratok
    legend.title = element_text(size = 18),# Legenda cím
    legend.text = element_text(size = 18),                # Legenda
    panel.background = element_rect(fill = "white"),
    panel.grid = element_line(color = "grey"),
    axis.line = element_line(color = "black", linewidth = 1.2, arrow = arrow(ends = "last", type = "closed", length = unit(0.1, "inches"))),
    legend.background = element_rect(linewidth = 1.2, colour = "black")
  )

combined_plot <- grid.arrange(plot_1, plot_2, ncol = 2)






# 8. ábra -----------------------------------------------------------------



# Próbáljuk meg numerikusan ellenőrizni a szabadditivitást.
# Nézzük meg, hogy két eloszlás esetén hogy alakul az összeg kockázatossága a 
# kockázatosságok összegéhez képest.

mu_1 <- 4
mu_2 <- 5
sigma_1 <- 4
sigma_2 <- 3
mu_sum <- mu_1 + mu_2
r_1_2 <- seq(-1,1, 0.01)
sigma_sum <- sqrt(sigma_1^2 + sigma_2^2 + 2*r_1_2*sigma_1*sigma_2)
max(sigma_sum) # ez legfeljebb a két szórás összege tud lenni.



data_1 <- as.data.frame(matrix(0, nrow = length(r_1_2), ncol = 2))
data_1[,1] <- sigma_sum
for (i in 1:length(r_1_2)) {
  data_1[i,2] <- fuzzy_entropy_based_riskiness_index(mu_sum, data_1[i, 1], 150, 1000, fuzzy_entropy = function(x) Fuzzy_entropy(x, generator_name = "g_1"))
}

data_1 <- data_1 %>% mutate(r = r_1_2)

names(data_1) <- c("sigma", "R_sum", "r")


R_sigma_1 <- fuzzy_entropy_based_riskiness_index(mu_1, sigma_1, 150, 1000, fuzzy_entropy = function(x) Fuzzy_entropy(x, generator_name = "g_1"))
R_sigma_2 <- fuzzy_entropy_based_riskiness_index(mu_2, sigma_2, 150, 1000, fuzzy_entropy = function(x) Fuzzy_entropy(x, generator_name = "g_1"))
sum_of_Rs <- R_sigma_1 + R_sigma_2


data_1 %>% ggplot(aes(x = r, y = R_sum)) + geom_line() +geom_hline(yintercept = sum_of_Rs, col = "red") + 
  labs(x = TeX("$\\r$"), y = TeX("$\\R_{F,c}(X + Y)$"), color = "Jelmagyarázat") +
  theme(
    axis.title.x = element_text(size = 20), # X tengely cím
    axis.title.y = element_text(size = 20), # Y tengely cím
    axis.text.x = element_text(size = 18),                # X tengely feliratok
    axis.text.y = element_text(size = 18),                # Y tengely feliratok
    legend.title = element_text(size = 18),# Legenda cím
    legend.text = element_text(size = 18),                # Legenda
    panel.background = element_rect(fill = "white"),
    panel.grid = element_line(color = "grey"),
    axis.line = element_line(color = "black", linewidth = 1.2, arrow = arrow(ends = "last", type = "closed", length = unit(0.1, "inches"))),
    legend.background = element_rect(linewidth = 1.2, colour = "black")
  )



# 9. ábra -----------------------------------------------------------------




skewed_fuzzy_entropy_based_riskiness_index <- function(mu, sigma, c, number_of_intervals = 1000, nu_1, nu_2, fuzzy_entropy = NULL){
  lower_limit <- mu - c
  upper_limit <- mu + c
  endpoints <- seq(lower_limit, upper_limit, length.out = number_of_intervals)
  data <- as.data.frame(matrix(rep(0,length(endpoints)*5), nrow = length(endpoints), ncol = 5))
  data[ ,1] <- endpoints
  for (i in 1:length(endpoints)) {
    data[i, 2] <- pnorm(data[i, 1], mean = mu, sd = sigma)
    data[i, 3] <- fuzzy_entropy(data[i, 2], nu_1, nu_2)
  }
  for (j in 2:length(endpoints)) {
    data[j, 4] <- mean(data[j - 1, 3], data[j, 3])
    data[j, 5] <- data[j, 4] * (data[j, 1] - data[j - 1, 1])
  }
  return(sum(data[, 5])/(2*c))
}

skewed_fuzzy_entropy_based_riskiness_index(4, 3, c = 10, nu_1 = 0.99, nu_2 = 0.25, fuzzy_entropy = h)



# Grafikus példázat -------------------------------------------------------


  # 9/1. részábra -------------------------------------------------------------



sigma_values <- seq(0,150, 0.5)
data_1 <- as.data.frame(matrix(0, nrow = length(sigma_values), ncol = 4))
data_1[,1] <- sigma_values
for (i in 1:length(sigma_values)) {
  data_1[i,2] <- skewed_fuzzy_entropy_based_riskiness_index(4, data_1[i, 1], 150, 1000, nu_1 = 0.99, nu_2 = 0.35, fuzzy_entropy = h)
  data_1[i,3] <- skewed_fuzzy_entropy_based_riskiness_index(4, data_1[i, 1], 100, 1000, nu_1 = 0.99, nu_2 = 0.35, fuzzy_entropy = h)
  data_1[i,4] <- skewed_fuzzy_entropy_based_riskiness_index(4, data_1[i, 1], 15, 1000, nu_1 = 0.99, nu_2 = 0.35, fuzzy_entropy = h)
}

names(data_1) <- c("sigma", "R_1", "R_2", "R_3")

data_1_long <- data_1 %>% melt(id.vars = "sigma")


plot_1 <- data_1_long %>% ggplot(aes(sigma, value, color = variable)) + geom_line(lwd = 1.2) + 
  labs(title = TeX(""), x = TeX("$\\sigma$"), y = TeX("$\\R_{F_{nu_1 = 0.99, nu_2 = 0.35}, c}(xi_{mu_0, sigma})$"), color = "Jelmagyarázat") + 
  scale_color_manual(
    values = c(
      "R_1" = "red",
      "R_2" = "green", 
      "R_3" = "purple"
    ),
    labels = TeX(c(
      "R_1" = "\\c = 150",
      "R_2" = "\\c = 100",
      "R_3" = "\\c = 15"
    ))
  ) +
  theme(
    axis.title.x = element_text(size = 20), # X tengely cím
    axis.title.y = element_text(size = 20), # Y tengely cím
    axis.text.x = element_text(size = 18),                # X tengely feliratok
    axis.text.y = element_text(size = 18),                # Y tengely feliratok
    legend.title = element_text(size = 18),# Legenda cím
    legend.text = element_text(size = 18),                # Legenda
    panel.background = element_rect(fill = "white"),
    panel.grid = element_line(color = "grey"),
    axis.line = element_line(color = "black", linewidth = 1.2, arrow = arrow(ends = "last", type = "closed", length = unit(0.1, "inches"))),
    legend.background = element_rect(linewidth = 1.2, colour = "black")
  )


  # 9/2. részábra -------------------------------------------------------------



sigma_values <- seq(0,150, 0.5)
data_2 <- as.data.frame(matrix(0, nrow = length(sigma_values), ncol = 4))
data_2[,1] <- sigma_values
for (i in 1:length(sigma_values)) {
  data_2[i,2] <- skewed_fuzzy_entropy_based_riskiness_index(4, data_1[i, 1], 150, 1000, nu_1 = 0.95, nu_2 = 0.65, fuzzy_entropy = h)
  data_2[i,3] <- skewed_fuzzy_entropy_based_riskiness_index(4, data_1[i, 1], 100, 1000, nu_1 = 0.95, nu_2 = 0.65, fuzzy_entropy = h)
  data_2[i,4] <- skewed_fuzzy_entropy_based_riskiness_index(4, data_1[i, 1], 15, 1000, nu_1 = 0.95, nu_2 = 0.65, fuzzy_entropy = h)
}


names(data_2) <- c("sigma", "R_1", "R_2", "R_3")
data_2_long <- data_2 %>% melt(id.vars = "sigma")


plot_2 <- data_2_long %>% ggplot(aes(sigma, value, color = variable)) + geom_line(lwd = 1.2) + 
  labs(title = TeX(""), x = TeX("$\\sigma$"), y = TeX("$\\R_{F_{{nu_1 = 0.95, nu_2 = 0.65}}, c}(xi_{mu_0, sigma})$"), color = "Jelmagyarázat") + 
  scale_color_manual(
    values = c(
      "R_1" = "red",
      "R_2" = "green", 
      "R_3" = "purple"
    ),
    labels = TeX(c(
      "R_1" = "\\c = 150",
      "R_2" = "\\c = 100",
      "R_3" = "\\c = 15"
    ))
  ) +
  theme(
    axis.title.x = element_text(size = 20), # X tengely cím
    axis.title.y = element_text(size = 20), # Y tengely cím
    axis.text.x = element_text(size = 18),                # X tengely feliratok
    axis.text.y = element_text(size = 18),                # Y tengely feliratok
    legend.title = element_text(size = 18),# Legenda cím
    legend.text = element_text(size = 18),                # Legenda
    panel.background = element_rect(fill = "white"),
    panel.grid = element_line(color = "grey"),
    axis.line = element_line(color = "black", linewidth = 1.2, arrow = arrow(ends = "last", type = "closed", length = unit(0.1, "inches"))),
    legend.background = element_rect(linewidth = 1.2, colour = "black")
  )


  # 9/3. részábra -------------------------------------------------------------



sigma_values <- seq(0,150, 0.5)
data_3 <- as.data.frame(matrix(0, nrow = length(sigma_values), ncol = 4))
data_3[,1] <- sigma_values
for (i in 1:length(sigma_values)) {
  data_3[i,2] <- skewed_fuzzy_entropy_based_riskiness_index(4, data_1[i, 1], 150, 1000, nu_1 = 0.9, nu_2 = 0.45, fuzzy_entropy = h)
  data_3[i,3] <- skewed_fuzzy_entropy_based_riskiness_index(4, data_1[i, 1], 100, 1000, nu_1 = 0.9, nu_2 = 0.45, fuzzy_entropy = h)
  data_3[i,4] <- skewed_fuzzy_entropy_based_riskiness_index(4, data_1[i, 1], 15, 1000, nu_1 = 0.9, nu_2 = 0.45, fuzzy_entropy = h)
}

names(data_3) <- c("sigma", "R_1", "R_2", "R_3")

data_3_long <- data_3 %>% melt(id.vars = "sigma")


plot_3 <- data_3_long %>% ggplot(aes(sigma, value, color = variable)) + geom_line(lwd = 1.2) + 
  labs(title = TeX(""), x = TeX("$\\sigma$"), y = TeX("$\\R_{F_{nu_1 = 0.9, nu_2 = 0.45}, c}(xi_{mu_0, sigma})$"), color = "Jelmagyarázat") + 
  scale_color_manual(
    values = c(
      "R_1" = "red",
      "R_2" = "green", 
      "R_3" = "purple"
    ),
    labels = TeX(c(
      "R_1" = "\\c = 150",
      "R_2" = "\\c = 100",
      "R_3" = "\\c = 15"
    ))
  ) +
  theme(
    axis.title.x = element_text(size = 20), # X tengely cím
    axis.title.y = element_text(size = 20), # Y tengely cím
    axis.text.x = element_text(size = 18),                # X tengely feliratok
    axis.text.y = element_text(size = 18),                # Y tengely feliratok
    legend.title = element_text(size = 18),# Legenda cím
    legend.text = element_text(size = 18),                # Legenda
    panel.background = element_rect(fill = "white"),
    panel.grid = element_line(color = "grey"),
    axis.line = element_line(color = "black", linewidth = 1.2, arrow = arrow(ends = "last", type = "closed", length = unit(0.1, "inches"))),
    legend.background = element_rect(linewidth = 1.2, colour = "black")
  )


  # 9/4. részábra -------------------------------------------------------------


sigma_values <- seq(0,150, 0.5)
data_4 <- as.data.frame(matrix(0, nrow = length(sigma_values), ncol = 4))
data_4[,1] <- sigma_values
for (i in 1:length(sigma_values)) {
  data_4[i,2] <- skewed_fuzzy_entropy_based_riskiness_index(4, data_1[i, 1], 150, 1000, nu_1 = 0.85, nu_2 = 0.55, fuzzy_entropy = h)
  data_4[i,3] <- skewed_fuzzy_entropy_based_riskiness_index(4, data_1[i, 1], 100, 1000, nu_1 = 0.85, nu_2 = 0.55, fuzzy_entropy = h)
  data_4[i,4] <- skewed_fuzzy_entropy_based_riskiness_index(4, data_1[i, 1], 15, 1000, nu_1 = 0.85, nu_2 = 0.55, fuzzy_entropy = h)
}

names(data_4) <- c("sigma", "R_1", "R_2", "R_3")

data_4_long <- data_4 %>% melt(id.vars = "sigma")


plot_4 <- data_4_long %>% ggplot(aes(sigma, value, color = variable)) + geom_line(lwd = 1.2) + 
  labs(title = TeX(""), x = TeX("$\\sigma$"), y = TeX("$\\R_{F_{nu_1 = 0.85, nu_2 = 0.55}, c}(xi_{mu_0, sigma})$"), color = "Jelmagyarázat") + 
  scale_color_manual(
    values = c(
      "R_1" = "red",
      "R_2" = "green", 
      "R_3" = "purple"
    ),
    labels = TeX(c(
      "R_1" = "\\c = 150",
      "R_2" = "\\c = 100",
      "R_3" = "\\c = 15"
    ))
  ) +
  theme(
    axis.title.x = element_text(size = 20), # X tengely cím
    axis.title.y = element_text(size = 20), # Y tengely cím
    axis.text.x = element_text(size = 18),                # X tengely feliratok
    axis.text.y = element_text(size = 18),                # Y tengely feliratok
    legend.title = element_text(size = 18),# Legenda cím
    legend.text = element_text(size = 18),                # Legenda
    panel.background = element_rect(fill = "white"),
    panel.grid = element_line(color = "grey"),
    axis.line = element_line(color = "black", linewidth = 1.2, arrow = arrow(ends = "last", type = "closed", length = unit(0.1, "inches"))),
    legend.background = element_rect(linewidth = 1.2, colour = "black")
  )

# Most pedig egyesítjük a részábrákat

combined_plot <- grid.arrange(plot_1, plot_2, plot_3, plot_4, ncol = 2)






# 10. ábra ----------------------------------------------------------------


# A kockáztaossági index közelítése nem x = 1/2 maximumhellyel rendelkező entrópia esetén


skewed_fuzzy_entropy_based_riskiness_index_nu <- function(mu, sigma, c, number_of_intervals = 1000, nu, generator_name, alpha){
  lower_limit <- mu - c
  upper_limit <- mu + c
  endpoints <- seq(lower_limit, upper_limit, length.out = number_of_intervals)
  data <- as.data.frame(matrix(rep(0,length(endpoints)*5), nrow = length(endpoints), ncol = 5))
  data[ ,1] <- endpoints
  for (i in 1:length(endpoints)) {
    data[i, 2] <- pnorm(data[i, 1], mean = mu, sd = sigma)
    data[i, 3] <- Fuzzy_entropy_nu(data[i, 2], generator_name, alpha, nu)
  }
  for (j in 2:length(endpoints)) {
    data[j, 4] <- mean(data[j - 1, 3], data[j, 3])
    data[j, 5] <- data[j, 4] * (data[j, 1] - data[j - 1, 1])
  }
  return(sum(data[, 5])/(2*c))
}




# Ábrázolás


sigma_values <- seq(0,150, 0.5)
data_1 <- as.data.frame(matrix(0, nrow = length(sigma_values), ncol = 4))
data_1[,1] <- sigma_values
for (i in 1:length(sigma_values)) {
  data_1[i,2] <- skewed_fuzzy_entropy_based_riskiness_index_nu(4, data_1[i, 1], c = 15, number_of_intervals = 1000, nu = 0.1, generator_name = "g_3", alpha = 1)
  data_1[i,3] <- skewed_fuzzy_entropy_based_riskiness_index_nu(4, data_1[i, 1], c = 15, number_of_intervals = 1000, nu = 0.5, generator_name = "g_3", alpha = 1)
  data_1[i,4] <- skewed_fuzzy_entropy_based_riskiness_index_nu(4, data_1[i, 1], c = 15, number_of_intervals = 1000, nu = 0.9, generator_name = "g_3", alpha = 1)
}




names(data_1) <- c("sigma", "R_1", "R_2", "R_3")

data_1_long <- data_1 %>% melt(id.vars = "sigma")


plot_1 <- data_1_long %>% ggplot(aes(sigma, value, color = variable)) + geom_line(lwd = 1.2) + 
  labs(title = TeX(""), x = TeX("$\\sigma$"), y = TeX("$\\R_{F_{nu}, c}(xi_{mu_0, sigma})$"), color = "Jelmagyarázat") + 
  scale_color_manual(
    values = c(
      "R_1" = "red",
      "R_2" = "green", 
      "R_3" = "purple"
    ),
    labels = TeX(c(
      "R_1" = "\\nu = 0.1",
      "R_2" = "\\nu = 0.5",
      "R_3" = "\\nu = 0.9"
    ))
  ) +
  theme(
    axis.title.x = element_text(size = 20), # X tengely cím
    axis.title.y = element_text(size = 20), # Y tengely cím
    axis.text.x = element_text(size = 18),                # X tengely feliratok
    axis.text.y = element_text(size = 18),                # Y tengely feliratok
    legend.title = element_text(size = 18),# Legenda cím
    legend.text = element_text(size = 18),                # Legenda
    panel.background = element_rect(fill = "white"),
    panel.grid = element_line(color = "grey"),
    axis.line = element_line(color = "black", linewidth = 1.2, arrow = arrow(ends = "last", type = "closed", length = unit(0.1, "inches"))),
    legend.background = element_rect(linewidth = 1.2, colour = "black")
  )






