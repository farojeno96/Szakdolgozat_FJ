#install.packages("e1071")
#install.packages(c("PerformanceAnalytics", "PortfolioAnalytics"))
#install.packages("xts")

library(readxl)
library(lubridate)
library(haven)
library(tidyverse)
library(e1071)
library(xts)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(psych)
library(reshape2)
library(MASS)
library(latex2exp)


# Adatok beolvasása és alakítgatása ---------------------------------------



# Könyvtár beállítása
setwd("C:/Users/Jenő/Desktop/ELTE/2025 ősz - Survey statisztika/Szakdolgozat")
# Beolvasás
data_0 <- read_excel("C:/Users/Jenő/Desktop/ELTE/2025 ősz - Survey statisztika/Szakdolgozat/BET_data_1.xlsx")
# Felesleges oszlopok elhagyása
data_1 <- data_0 %>% dplyr::select(Name, Date, Close)
# Váltás széles formátumra
data_2 <- data_1 %>% spread(key = Name, value = Close)
# Hozamszámítás
continuous_return <- function(x)(c(NA, diff(log(x), lag = 1)))
data_3 <- data_2 %>% mutate_at(c("MASTERPLAST", "MOL", "MTELEKOM", "OTP", "PANNERGY", "RICHTER", "WABERERS"), continuous_return)

# Valós adatok cseréje szimulált adatokra

set.seed(123)

# Paraméterek
mu    <- rep(0,7)                             # várható érték vektor (d=7)
Sigma <- matrix(c(
  1.000,  0.300, -0.200,  0.100,  0.050, -0.050,  0.100,
  0.300,  1.530,  0.240, -0.150,  0.135,  0.045, -0.030,
  -0.200,  0.240,  0.913,  0.123, -0.075,  0.068, -0.033,
  0.100, -0.150,  0.123,  2.322,  0.420, -0.302,  0.168,
  0.050,  0.135, -0.075,  0.420,  1.323,  0.103, -0.080,
  -0.050,  0.045,  0.068, -0.302,  0.103,  0.710,  0.118,
  0.100, -0.030, -0.033,  0.168, -0.080,  0.118,  1.763
), nrow = 7, byrow = TRUE)

write.csv(Sigma, "sigma.csv")


# Szimuláció

n  <- dim(data_2)[1]
X  <- mvrnorm(n = n, mu = mu, Sigma = Sigma)

# Gyors ellenőrzés
colMeans(X)          # ≈ mu
cov(X)               # ≈ Sigma

data_3[,2:8] <- X



# ---- hozam mátrix xts-ként ----
tickers <- c("MASTERPLAST","MOL","MTELEKOM","OTP","PANNERGY","RICHTER","WABERERS")

# csak a hozam oszlopokat és a dátumot tartjuk meg
ret_df <- data_3 %>%
  #select(Date, all_of(tickers)) %>%
  mutate(
    Date = ymd(Date)
  ) %>%
  arrange(Date) %>%
  drop_na()

# Két részre bontás
ret_df_learning <- ret_df %>% filter(Date <= "2024-12-31")
ret_df_testing <- ret_df %>% filter(Date > "2024-12-31")

R_xts_learning <- xts(as.matrix(ret_df_learning[,2:8]), order.by = ret_df_learning$Date)

ret_df_learning %>% ggplot(aes(x = Date, y = MASTERPLAST)) + geom_line()


# Gyors ellenőrzés
print(dim(R_xts_learning))
print(colMeans(R_xts_learning))      # átlagos (napi) hozamok
print(apply(R_xts_learning, 2, sd))  # szórások


# Saját függvények a kockázatossági index számításához --------------------


# Szimmetrikus entrópiára építő
fuzzy_entropy_based_riskiness_index <- function(inputs, weights = rep(1/length(inputs), length(inputs)), 
                                                fuzzy_entropy = NULL) {
  n <- length(inputs)
  c <- max(inputs) - min(inputs)
  ordered_inputs <- c(0, inputs[order(inputs)], 1)
  ordered_weights <- c(NA, weights[order(inputs)], NA)
  sum_of_ordered_weights <- NA
  fuzzy_entropy_values <- NA
  weighted_fuzzy_entropy_values <- NA
  df <- as.data.frame(cbind(ordered_inputs, ordered_weights, sum_of_ordered_weights,
                            fuzzy_entropy_values, weighted_fuzzy_entropy_values))
  for (i in 1:n) {
    df[(i + 1),"sum_of_ordered_weights"] <- sum(df$ordered_weights[2:i])
    df[(i + 1), "fuzzy_entropy_values"] <- fuzzy_entropy(df[(i + 1),"sum_of_ordered_weights"])
    df[(i + 1), "weighted_fuzzy_entropy_values"] <- df[(i + 1), "fuzzy_entropy_values"]*(df[(i + 1), "ordered_inputs"] - df[(i), "ordered_inputs"])
  }
  weighted_dissensus = sum(df$weighted_fuzzy_entropy_values, na.rm = T)/(c)
  #return(df)
  return(weighted_dissensus)
}


fuzzy_entropy_based_riskiness_index(ret_df_learning$MASTERPLAST, fuzzy_entropy = function(x) {4*x*(1-x)})

# Aszimmetrikus, de x = 1/2 maximumhelyű entrópiára építő

f <- function(nu_1, x){
  value <- 1/(1 + (1-nu_1)*(1-2*x)/(nu_1*2*x))
  return(min(value, 1))
}

g <- function(nu_2, x){
  value <- 1/(1 + (1-nu_2)*(2*x - 1)/(nu_2*2*(1 - x)))
  return(min(value, 1))
}

h <- function(x, nu_1, nu_2) {
  a <- f(nu_1, x)
  b <- g(nu_2, x)
  value <- 1/(1 + (1 - a)/a + (1 - b)/b)
  return(value)
}

skewed_fuzzy_entropy_based_riskiness_index <- function(inputs, nu_1, nu_2, weights = rep(1/length(inputs), length(inputs))) {
  n <- length(inputs)
  c <- max(inputs) - min(inputs)
  ordered_inputs <- c(0, inputs[order(inputs)], 1)
  ordered_weights <- c(NA, weights[order(inputs)], NA)
  sum_of_ordered_weights <- NA
  fuzzy_entropy_values <- NA
  weighted_fuzzy_entropy_values <- NA
  df <- as.data.frame(cbind(ordered_inputs, ordered_weights, sum_of_ordered_weights,
                            fuzzy_entropy_values, weighted_fuzzy_entropy_values))
  for (i in 1:n) {
    df[(i + 1),"sum_of_ordered_weights"] <- sum(df$ordered_weights[2:i])
    df[(i + 1), "fuzzy_entropy_values"] <- h(df[(i + 1),"sum_of_ordered_weights"], nu_1, nu_2)
    df[(i + 1), "weighted_fuzzy_entropy_values"] <- df[(i + 1), "fuzzy_entropy_values"]*(df[(i + 1), "ordered_inputs"] - df[(i), "ordered_inputs"])
  }
  weighted_dissensus = sum(df$weighted_fuzzy_entropy_values, na.rm = T)/(c)
  #return(df)
  return(weighted_dissensus)
}

skewed_fuzzy_entropy_based_riskiness_index(ret_df_learning$MASTERPLAST, nu_1 = 0.99, nu_2 = 0.35)




# ---- Portfólió specifikáció (long-only, teljes befektetés) SD alapon ----
port_spec <- portfolio.spec(assets = colnames(R_xts_learning))
port_spec <- add.constraint(portfolio = port_spec, type = "full_investment")  # súlyok összege = 1
port_spec <- add.constraint(portfolio = port_spec, type = "box", min = 0, max = 1)  # nincs short

# ===== 1) Minimum kockázatú portfólió =====
mv_spec <- add.objective(portfolio = port_spec, type = "risk", name = "StdDev")
#mv_spec <- add.objective(portfolio = port_spec, type = "return", name = "mean")
set.seed(123)
opt_minvar <- optimize.portfolio(
  R               = R_xts_learning,
  portfolio       = mv_spec,
  optimize_method = "pso",
  search_size     = 5000,
  trace           = TRUE
)

print(round(extractWeights(opt_minvar), 4))
final_weights_SD <- extractWeights(opt_minvar)

# Ellenőrzés
sum(extractWeights(opt_minvar)) # 1-re összegződnek a súlyok

# Portfólió hozamának és kumulált hozamának számítása:
ret_df_testing <- ret_df_testing %>% mutate(portfolio_return_benchmark_SD = MASTERPLAST*final_weights_SD[1] + MOL*final_weights_SD[2] + MTELEKOM*final_weights_SD[3] + 
                            OTP*final_weights_SD[4] + PANNERGY*final_weights_SD[5] + RICHTER*final_weights_SD[6] + WABERERS*final_weights_SD[7]) %>% 
  mutate(cumulated_return_benchmark_SD = cumsum(portfolio_return_benchmark_SD))

# Ábrázolás
ret_df_testing %>% ggplot(aes(x = Date, y = portfolio_return_benchmark_SD)) + geom_line()
ret_df_testing %>% ggplot(aes(x = Date, y = cumulated_return_benchmark_SD)) + geom_line()





# ---- Portfólió specifikáció (long-only, teljes befektetés) VaR alapon ----
port_spec <- portfolio.spec(assets = colnames(R_xts_learning))
port_spec <- add.constraint(portfolio = port_spec, type = "full_investment")  # súlyok összege = 1
port_spec <- add.constraint(portfolio = port_spec, type = "box", min = 0, max = 1)  # nincs short

# ===== 1) Minimum kockázatú portfólió =====
mv_spec <- add.objective(portfolio = port_spec, type = "risk", name = "VaR", arguments = list(p = 0.95)
                         )
set.seed(123)
opt_minvar <- optimize.portfolio(
  R               = R_xts_learning,
  portfolio       = mv_spec,
  optimize_method = "pso",
  search_size     = 5000,
  trace           = TRUE
)

print(round(extractWeights(opt_minvar), 4))
final_weights_VaR <- extractWeights(opt_minvar)

# Ellenőrzés
sum(extractWeights(opt_minvar)) # 1-re összegződnek a súlyok

# Portfólió hozamának és kumulált hozamának számítása:
ret_df_testing <- ret_df_testing %>% mutate(portfolio_return_benchmark_VaR = MASTERPLAST*final_weights_VaR[1] + MOL*final_weights_VaR[2] + MTELEKOM*final_weights_VaR[3] + 
                                              OTP*final_weights_VaR[4] + PANNERGY*final_weights_VaR[5] + RICHTER*final_weights_VaR[6] + WABERERS*final_weights_VaR[7]) %>% 
  mutate(cumulated_return_benchmark_VaR = cumsum(portfolio_return_benchmark_VaR))

# Ábrázolás
ret_df_testing %>% ggplot(aes(x = Date, y = portfolio_return_benchmark_VaR)) + geom_line()
ret_df_testing %>% ggplot(aes(x = Date, y = cumulated_return_benchmark_VaR)) + geom_line()


# ---- Portfólió specifikáció (long-only, teljes befektetés) ES alapon ----
port_spec <- portfolio.spec(assets = colnames(R_xts_learning))
port_spec <- add.constraint(portfolio = port_spec, type = "full_investment")  # súlyok összege = 1
port_spec <- add.constraint(portfolio = port_spec, type = "box", min = 0, max = 1)  # nincs short

# ===== 1) Minimum kockázatú portfólió =====
mv_spec <- add.objective(portfolio = port_spec, type = "risk", name = "ES", arguments = list(p = 0.95)
                         )
set.seed(123)
opt_minvar <- optimize.portfolio(
  R               = R_xts_learning,
  portfolio       = mv_spec,
  optimize_method = "pso",
  search_size     = 5000,
  trace           = TRUE
)


print(round(extractWeights(opt_minvar), 4))
final_weights_ES <- extractWeights(opt_minvar)

# Ellenőrzés
sum(extractWeights(opt_minvar)) # 1-re összegződnek a súlyok

# Portfólió hozamának és kumulált hozamának számítása:
ret_df_testing <- ret_df_testing %>% mutate(portfolio_return_benchmark_ES = MASTERPLAST*final_weights_ES[1] + MOL*final_weights_ES[2] + MTELEKOM*final_weights_ES[3] + 
                                              OTP*final_weights_ES[4] + PANNERGY*final_weights_ES[5] + RICHTER*final_weights_ES[6] + WABERERS*final_weights_ES[7]) %>% 
  mutate(cumulated_return_benchmark_ES = cumsum(portfolio_return_benchmark_ES))

# Ábrázolás
ret_df_testing %>% ggplot(aes(x = Date, y = portfolio_return_benchmark_ES)) + geom_line()
ret_df_testing %>% ggplot(aes(x = Date, y = cumulated_return_benchmark_ES)) + geom_line()




# ChatGPT javaslata kockázati célfüggvényre-------------------------------------------------------

# -- Portfólió kockázati célfüggvény: SZIMMETRIKUS verzió --
obj_fuzzy_risk <- function(R, weights, 
                           fuzzy_entropy = function(x) 4*x*(1-x),
                           fe_weights = NULL) {
  # portfólió hozam-idősor (nincs rebalansz, sima súlyozott összeg)
  port_ret <- PerformanceAnalytics::Return.portfolio(R, weights = weights, 
                                                     rebalance_on = NA, wealth.index = FALSE)
  r <- as.numeric(port_ret)
  if (is.null(fe_weights)) fe_weights <- rep(1/length(r), length(r))
  # a saját mutatód meghívása a portfólió hozamokra
  fuzzy_entropy_based_riskiness_index(inputs = r, 
                                      weights = fe_weights, 
                                      fuzzy_entropy = fuzzy_entropy)
}


# -- Portfólió kockázati célfüggvény: ASZIMMETRIKUS (nu1, nu2) --
obj_skewed_fuzzy_risk <- function(R, weights, nu_1, nu_2, fe_weights = NULL) {
  port_ret <- PerformanceAnalytics::Return.portfolio(R, weights = weights, 
                                                     rebalance_on = NA, wealth.index = FALSE)
  r <- as.numeric(port_ret)
  if (is.null(fe_weights)) fe_weights <- rep(1/length(r), length(r))
  skewed_fuzzy_entropy_based_riskiness_index(inputs = r, 
                                             nu_1 = nu_1, nu_2 = nu_2, 
                                             weights = fe_weights)
}



# ---- Portfólió specifikáció (long-only, teljes befektetés) entrópia alapon_1 ----
port_spec <- portfolio.spec(assets = colnames(R_xts_learning))
port_spec <- add.constraint(portfolio = port_spec, type = "full_investment")
port_spec <- add.constraint(portfolio = port_spec, type = "box", min = 0, max = 1)

# ---- Saját kockázati célfüggvény hozzáadása ----

my_spec <- add.objective(
  portfolio = port_spec,
  type      = "risk",
  name      = "obj_fuzzy_risk",             # <- EZ a fenti R-függvény neve!
  arguments = list(                          # <- ide mennek a célfüggvény további arg-jai
    fuzzy_entropy = function(x) 4*x*(1-x)    # példa: szimmetrikus entrópia
    # fe_weights = ...                       # opcionális: súlyok az "inputs" diszkretizációhoz
  )
)

# ---- Optimalizálás ----
set.seed(123)
opt_custom_sym <- optimize.portfolio(
  R                = R_xts_learning,
  portfolio        = my_spec,
  optimize_method  = "pso",      
  search_size      = 5000,          
  trace            = TRUE
)

w_ent <- extractWeights(opt_custom_sym)

ret_df_testing <- ret_df_testing %>% mutate(portfolio_return_entropy = MASTERPLAST*w_ent[1] + MOL*w_ent[2] + MTELEKOM*w_ent[3] + 
                                              OTP*w_ent[4] + PANNERGY*w_ent[5] + RICHTER*w_ent[6] + WABERERS*w_ent[7]) %>% 
  mutate(cumulated_return_entropy = cumsum(portfolio_return_entropy))


# ---- Portfólió specifikáció (long-only, teljes befektetés) entrópia alapon_2 ----
port_spec <- portfolio.spec(assets = colnames(R_xts_learning))
port_spec <- add.constraint(portfolio = port_spec, type = "full_investment")
port_spec <- add.constraint(portfolio = port_spec, type = "box", min = 0, max = 1)

# ---- Saját kockázati célfüggvény hozzáadása ----

my_spec <- add.objective(
  portfolio = port_spec,
  type      = "risk",
  name      = "obj_fuzzy_risk",             # <- EZ a fenti R-függvény neve!
  arguments = list(                          # <- ide mennek a célfüggvény további arg-jai
    fuzzy_entropy = function(x) 2*sqrt(x*(1-x))    # példa: szimmetrikus entrópia
    # fe_weights = ...                       # opcionális: súlyok az "inputs" diszkretizációhoz
  )
)

# ---- Optimalizálás ----
set.seed(123)
opt_custom_sym <- optimize.portfolio(
  R                = R_xts_learning,
  portfolio        = my_spec,
  optimize_method  = "pso",      
  search_size      = 5000,          
  trace            = TRUE
)

w_ent_sqrt <- extractWeights(opt_custom_sym)

ret_df_testing <- ret_df_testing %>% mutate(portfolio_return_entropy_sqrt = MASTERPLAST*w_ent_sqrt[1] + MOL*w_ent_sqrt[2] + MTELEKOM*w_ent_sqrt[3] + 
                                              OTP*w_ent_sqrt[4] + PANNERGY*w_ent_sqrt[5] + RICHTER*w_ent_sqrt[6] + WABERERS*w_ent_sqrt[7]) %>% 
  mutate(cumulated_return_entropy_sqrt = cumsum(portfolio_return_entropy_sqrt))




# ---- Portfólió specifikáció (long-only, teljes befektetés) aszimmetrikus entrópia alapon_1 ----
port_spec <- portfolio.spec(assets = colnames(R_xts_learning))
port_spec <- add.constraint(portfolio = port_spec, type = "full_investment")
port_spec <- add.constraint(portfolio = port_spec, type = "box", min = 0, max = 1)

skew_spec <- add.objective(
  portfolio = port_spec,
  type      = "risk",
  name      = "obj_skewed_fuzzy_risk",    # <- az aszimmetrikus objektív
  arguments = list(
    nu_1 = 0.99,                          
    nu_2 = 0.35
    # fe_weights = ...
  )
)

set.seed(123)
opt_custom_skew <- optimize.portfolio(
  R               = R_xts_learning,
  portfolio       = skew_spec,
  optimize_method = "pso",
  search_size     = 5000,
  trace           = TRUE
)


w_skew_ent <- extractWeights(opt_custom_skew)

ret_df_testing <- ret_df_testing %>% mutate(portfolio_return_skew_entropy = MASTERPLAST*w_skew_ent[1] + MOL*w_skew_ent[2] + MTELEKOM*w_skew_ent[3] + 
                                              OTP*w_skew_ent[4] + PANNERGY*w_skew_ent[5] + RICHTER*w_skew_ent[6] + WABERERS*w_skew_ent[7]) %>% 
  mutate(cumulated_return_skew_entropy = cumsum(portfolio_return_skew_entropy))



# ---- Portfólió specifikáció (long-only, teljes befektetés) aszimmetrikus entrópia alapon_2 ----
port_spec <- portfolio.spec(assets = colnames(R_xts_learning))
port_spec <- add.constraint(portfolio = port_spec, type = "full_investment")
port_spec <- add.constraint(portfolio = port_spec, type = "box", min = 0, max = 1)

skew_spec <- add.objective(
  portfolio = port_spec,
  type      = "risk",
  name      = "obj_skewed_fuzzy_risk",    # <- az aszimmetrikus objektív
  arguments = list(
    nu_1 = 0.90,                          
    nu_2 = 0.35
    # fe_weights = ...
  )
)

set.seed(123)
opt_custom_skew <- optimize.portfolio(
  R               = R_xts_learning,
  portfolio       = skew_spec,
  optimize_method = "pso",
  search_size     = 5000,
  trace           = TRUE
)


w_skew_ent_2 <- extractWeights(opt_custom_skew)

ret_df_testing <- ret_df_testing %>% mutate(portfolio_return_skew_entropy_2 = MASTERPLAST*w_skew_ent_2[1] + MOL*w_skew_ent_2[2] + MTELEKOM*w_skew_ent_2[3] + 
                                              OTP*w_skew_ent_2[4] + PANNERGY*w_skew_ent_2[5] + RICHTER*w_skew_ent_2[6] + WABERERS*w_skew_ent_2[7]) %>% 
  mutate(cumulated_return_skew_entropy_2 = cumsum(portfolio_return_skew_entropy_2))









# Portfólióhozamok ábrázolása

ret_df_testing %>% dplyr::select(Date, portfolio_return_benchmark_SD, portfolio_return_benchmark_VaR, portfolio_return_benchmark_ES,
                          portfolio_return_entropy, portfolio_return_entropy_sqrt, portfolio_return_skew_entropy, portfolio_return_skew_entropy_2) %>% melt(id.vars = "Date") %>% 
  ggplot(aes(Date, value, color = variable)) + geom_line(lwd = 1.2) + 
  labs(x = "2025", y = "Portfólió-hozam", color = "Jelmagyarázat") + 
  scale_color_manual(
    values = c(
      "portfolio_return_benchmark_SD" = "red", 
      "portfolio_return_benchmark_VaR" = "green",
      "portfolio_return_benchmark_ES" = "blue",
      "portfolio_return_entropy" = "orange",
      "portfolio_return_entropy_sqrt" = "yellow",
      "portfolio_return_skew_entropy" = "black",
      "portfolio_return_skew_entropy_2" = "purple"
    ),
    labels = TeX(c(
      "portfolio_return_benchmark_SD" = "SD", 
      "portfolio_return_benchmark_VaR" = "VaR",
      "portfolio_return_benchmark_ES" = "ES",
      "portfolio_return_entropy" = "$\\F_1(x) = 4x(1-x)$",
      "portfolio_return_entropy_sqrt" = "$\\F_2(x) = 2\\sqrt{x(1-x)}$",
      "portfolio_return_skew_entropy" = "$\\nu_{1}=0.99,\\ \\nu_{2}=0.35$",
      "portfolio_return_skew_entropy_2" = "$\\nu_{1}=0.90,\\ \\nu_{2}=0.35$"
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



# Kumulált portfólióhozamok ábrázolása

ret_df_testing %>% dplyr::select(Date, cumulated_return_benchmark_SD, cumulated_return_benchmark_VaR, cumulated_return_benchmark_ES,
                          cumulated_return_entropy, cumulated_return_entropy_sqrt, cumulated_return_skew_entropy, cumulated_return_skew_entropy_2) %>% melt(id.vars = "Date") %>% 
  ggplot(aes(Date, value, color = variable)) + geom_line(lwd = 1.2) + 
  labs(x = "2025", y = "Kumulált portfólió-hozam", color = "Jelmagyarázat") + 
  scale_color_manual(
    values = c(
      "cumulated_return_benchmark_SD" = "red", 
      "cumulated_return_benchmark_VaR" = "green",
      "cumulated_return_benchmark_ES" = "blue",
      "cumulated_return_entropy" = "orange",
      "cumulated_return_entropy_sqrt" = "yellow",
      "cumulated_return_skew_entropy" = "black",
      "cumulated_return_skew_entropy_2" = "purple"
    ),
    labels = TeX(c(
      "cumulated_return_benchmark_SD" = "SD", 
      "cumulated_return_benchmark_VaR" = "VaR",
      "cumulated_return_benchmark_ES" = "ES",
      "cumulated_return_entropy" = "$\\F_1(x) = 4x(1-x)$",
      "cumulated_return_entropy_sqrt" = "$\\F_2(x) = 2\\sqrt{x(1-x)}$",
      "cumulated_return_skew_entropy" = "$\\nu_{1}=0.99,\\ \\nu_{2}=0.35$",
      "cumulated_return_skew_entropy_2" = "$\\nu_{1}=0.90,\\ \\nu_{2}=0.35$"
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





#write.csv(ret_df_testing, "testing_sim.csv")


weights_sim <- as.data.frame(rbind(final_weights_SD,
                               final_weights_VaR,
                               final_weights_ES,
                               w_ent,
                               w_ent_sqrt,
                               w_skew_ent,
                               w_skew_ent_2))


#write.csv(weights_sim, "weights_sim.csv")














ret_df_testing <- ret_df_testing %>% mutate(portfolio_return_entropy = MASTERPLAST*w_ent[1] + MOL*w_ent[2] + MTELEKOM*w_ent[3] + 
                                              OTP*w_ent[4] + PANNERGY*w_ent[5] + RICHTER*w_ent[6] + WABERERS*w_ent[7]) %>% 
  mutate(cumulated_return_entropy = cumsum(portfolio_return_entropy))

# Ábrázolás
ret_df_testing %>% ggplot(aes(x = Date, y = portfolio_return_entropy)) + geom_line()
ret_df_testing %>% ggplot(aes(x = Date, y = cumulated_return_entropy)) + geom_line()

plot(x = ret_df_testing$Date, y = ret_df_testing$cumulated_return_benchmark_SD, type = "l")
lines(x = ret_df_testing$Date, y = ret_df_testing$cumulated_return_benchmark_VaR, col = "red")
lines(x = ret_df_testing$Date, y = ret_df_testing$cumulated_return_benchmark_ES, col = "green")
lines(x = ret_df_testing$Date, y = ret_df_testing$cumulated_return_entropy, col = "orange")













