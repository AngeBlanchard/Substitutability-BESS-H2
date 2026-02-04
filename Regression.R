setwd("C:/Users/angeb/Nextcloud/H2 SDDP/CODE/RESULTS")
dec = "."

# Packages
library(readxl)
library(dplyr)
library(ggplot2)
library(stargazer)
# -----------------------
# Paramètres
# -----------------------
scenarios <- c(0.8, 1, 1.25, 1.5, 2)  # facteurs de coût BESS
technos <- c("PtG", "H2T", "UHS")       # technos d'intérêt
path <- "C:/Users/angeb/Nextcloud/H2 SDDP/CODE/RESULTS/"

# -----------------------
# Charger les données CSV
# -----------------------
data_list <- lapply(scenarios, function(s){
  if (s == 1) {
    fname <- paste0(path, "base_objective_1.csv")
  } else {
    fname <- paste0(path, "base_objective_BESS_", s, ".csv")
  }
  df <- read.csv2(fname, stringsAsFactors = FALSE)
  df <- df %>% 
    mutate(x4 = as.numeric(x4)) %>%   # conversion en numérique
    select(Technology, Investment = x4) %>%
    mutate(scenario = s)
  return(df)
})

data <- bind_rows(data_list)
data$Investment <- as.numeric(data$Investment)
# -----------------------
# Préparer les données
# -----------------------
df_bess <- data %>% filter(Technology == "BESS_E") %>% 
  select(scenario, BESS = Investment)

df_long <- data %>% 
  filter(Technology %in% technos) %>%
  left_join(df_bess, by = "scenario") %>%
  mutate(
    ratio = Investment / BESS,
    log_ratio = log(ratio + 1e-6),   # petit epsilon pour éviter log(0)
    log_cost = log(scenario)
  )

# -----------------------
# Régressions
# -----------------------
models <- lapply(technos, function(tech){
  lm(log_ratio ~ log_cost, data = df_long %>% filter(Technology == tech))
})

names(models) <- technos

# -----------------------
# Graphiques
# -----------------------
for (tech in technos){
  df_plot <- df_long %>% filter(Technology == tech)
  gg <- ggplot(df_plot, aes(x = log_cost, y = log_ratio)) +
    geom_point(size=3, colour="darkblue") +
    geom_smooth(method="lm", se=FALSE, colour="red") +
    theme_minimal(base_size = 14) +
    labs(
      title = paste("Substitution elasticity:", tech, "vs BESS"),
      x = expression(log("Cost of BESS")),
      y = bquote(log(frac(K[.(tech)], K[BESS])))
    )
  print(gg)
}

# -----------------------
# Tableau LaTeX
# -----------------------
stargazer(models,
          type = "latex",
          title = "Morishima Elasticities of Substitution",
          dep.var.labels = c("log(K_tech / K_BESS)"),
          column.labels = technos,
          digits = 3,
          intercept.bottom = FALSE,
          omit.stat = c("f", "ser"))




######################################################################
# FOR WHEN H2 COST CHANGES
######################################################################

scenarios <- c(0.5, 0.6, 0.8, 1, 1.25) 
technos <- c("PtG", "H2T", "UHS")   # technos à étudier
# -----------------------
# Charger les données CSV (avec cas particulier pour s = 1)
# -----------------------
data_list <- lapply(scenarios, function(s){
  if (s == 1) {
    fname <- paste0(path, "base_objective_1.csv")
  } else {
    fname <- paste0(path, "base_objective_H2_", s, ".csv")
  }
  
  df <- read.csv2(fname, stringsAsFactors = FALSE)
  df <- df %>% 
    mutate(x4 = as.numeric(x4)) %>%   # conversion en numérique
    select(Technology, Investment = x4) %>%
    mutate(scenario = s)
  return(df)
})

data <- bind_rows(data_list)

# -----------------------
# Extraire BESS_E
# -----------------------
df_bess <- data %>%
  filter(Technology == "BESS_E") %>%
  select(scenario, BESS = Investment)

# -----------------------
# Construire ratios BESS / techno
# -----------------------
df_long <- data %>%
  filter(Technology %in% technos) %>%
  left_join(df_bess, by = "scenario") %>%
  mutate(
    ratio = BESS / Investment,
    log_ratio = log(ratio + 1e-6),  # éviter log(0)
    log_cost = log(scenario)
  )

# -----------------------
# Régressions
# -----------------------
models <- lapply(technos, function(tech){
  lm(log_ratio ~ log_cost, data = df_long %>% filter(Technology == tech))
})
names(models) <- technos

# -----------------------
# Graphiques avec pente
# -----------------------
for (tech in technos){
  df_plot <- df_long %>% filter(Technology == tech)
  model <- lm(log_ratio ~ log_cost, data = df_plot)
  slope <- round(coef(model)[2], 3)  # pente = élasticité
  
  gg <- ggplot(df_plot, aes(x = log_cost, y = log_ratio)) +
    geom_point(size=3, colour="darkblue") +
    geom_smooth(method="lm", se=FALSE, colour="red") +
    theme_minimal(base_size = 14) +
    labs(
      title = paste("Elasticity of substitution: BESS vs", tech),
      x = expression(log("Cost of H2 technologies")),
      y = bquote(log(frac(K[BESS], K[.(tech)])))
    ) +
    annotate("text", 
             x = min(df_plot$log_cost) + 0.1, 
             y = max(df_plot$log_ratio, na.rm=TRUE), 
             label = paste("Slope =", slope),
             hjust = 0, vjust = 1, size=5, colour="black")
  
  print(gg)
}

# -----------------------
# Tableau LaTeX
# -----------------------
stargazer(models,
          type = "latex",
          title = "Morishima Elasticities of Substitution (Hydrogen cost shock)",
          dep.var.labels = c("log(K_BESS / K_tech))"),
          column.labels = technos,
          digits = 3,
          intercept.bottom = FALSE,
          omit.stat = c("f", "ser"))




######################################################################
# ELASTICITY OF TOTAL SYSTEM COST TO BESS AND H2 COSTS
######################################################################
scenarios <- c(0.5, 0.6, 0.8, 1, 1.25, 1.5, 2)
# -----------------------
# Fonction utilitaire pour lire le coût système (col B ligne 2)
# -----------------------
read_system_cost <- function(fname) {
  df <- read.csv2(fname, stringsAsFactors = FALSE, header = FALSE)
  as.numeric(df[2,2])  # ligne 2, colonne 2
}

# -----------------------
# Charger données : variation du coût BESS
# -----------------------
cost_bess <- data.frame(
  scenario = scenarios,
  sys_cost = sapply(scenarios, function(s){
    if (s == 1) {
      fname <- paste0(path, "base_objective_1.csv")
    } else {
      fname <- paste0(path, "base_objective_BESS_", s, ".csv")
    }
    read_system_cost(fname)
  })
)

# -----------------------
# Charger données : variation du coût H2
# -----------------------
cost_h2 <- data.frame(
  scenario = scenarios,
  sys_cost = sapply(scenarios, function(s){
    if (s == 1) {
      fname <- paste0(path, "base_objective_1.csv")
    } else {
      fname <- paste0(path, "base_objective_H2_", s, ".csv")
    }
    read_system_cost(fname)
  })
)

# -----------------------
# Régressions log-log
# -----------------------
model_bess <- lm(log(sys_cost) ~ log(scenario), data = cost_bess)
model_h2   <- lm(log(sys_cost) ~ log(scenario), data = cost_h2)

# -----------------------
# Tableau LaTeX
# -----------------------
stargazer(list(model_h2, model_bess),
          type = "latex",
          title = "Elasticity of Total System Cost to Technology Costs",
          dep.var.labels = c("log(System cost)"),
          column.labels = c("H2 cost", "BESS cost"),
          digits = 3,
          intercept.bottom = FALSE,
          omit.stat = c("f", "ser"))


