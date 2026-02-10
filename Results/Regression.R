# Packages
library(readxl)
library(dplyr)
library(ggplot2)
library(xtable) 
library(gridExtra) 
setwd("C:/Users/angeb/kDrive/thèse/H2 SDDP/CODE/RESULTS/")
# -----------------------
# Parameters
# -----------------------
scenarios_bess <- c(0.5, 0.6, 0.8, 1, 1.25, 1.5, 2)
technos <- c("PtG", "H2T", "UHS")
path <- "C:/Users/angeb/kDrive/thèse/H2 SDDP/CODE/RESULTS/"

# -----------------------
# Loading data
# -----------------------
load_data <- function(s, type="BESS") {
  prefix <- if(type == "BESS") "base_objective_BESS_" else "base_objective_H2_"
  fname <- if(s==1) paste0(path, "base_objective_1.csv") else paste0(path, prefix, s, ".csv")
  
  if(!file.exists(fname)) return(NULL)
  
  read.csv2(fname, stringsAsFactors = FALSE) %>% 
    mutate(x4 = as.numeric(x4)) %>% 
    select(Technology, Investment = x4) %>%
    mutate(scenario = s)
}

# ==============================================================================
# SECTION 1 : Elasticity to BESS costs
# ==============================================================================

data_bess <- bind_rows(lapply(scenarios_bess, load_data, type="BESS"))
df_ref_bess <- data_bess %>% filter(Technology == "BESS_E") %>% select(scenario, BESS = Investment)

df_long_bess <- data_bess %>% 
  filter(Technology %in% technos) %>%
  left_join(df_ref_bess, by = "scenario") %>%
  mutate(log_ratio = log((Investment / BESS) + 1e-6), 
         log_cost = log(scenario))

# A list to store the results
res_bess <- data.frame(Technology = character(), Elasticity = numeric(), R2 = numeric())
plots_bess <- list()

for (tech in technos) {
  # Filtration on very low investment levels (we assimilate any investment below 100MW to zero)
  df_sub <- df_long_bess %>% filter(Technology == tech & Investment>1e-1)
  model <- lm(log_ratio ~ log_cost, data = df_sub)
  
  # Table
  res_bess <- rbind(res_bess, data.frame(
    Technology = tech,
    Elasticity = round(coef(model)[2], 3),
    R2 = round(summary(model)$r.squared, 3)
  ))
  
  # Figure
  plots_bess[[tech]] <- ggplot(df_sub, aes(x = log_cost, y = log_ratio)) +
    geom_point(size=3, colour="#2c3e50") +
    geom_smooth(method="lm", se=FALSE, colour="#e74c3c") +
    theme_bw(base_size = 10) +
    labs(title = tech, x = "log(BESS Cost factor)", y = "log(K_i / K_BESS)") +
    annotate("text", x = -Inf, y = Inf, label = paste("R² =", round(summary(model)$r.squared, 2)), 
             hjust = -0.1, vjust = 1.5, size = 3)
}

# Latex table export
print(xtable(res_bess, caption="Morishima Elasticities (BESS cost shock)", label="tab:bess_res"), 
      include.rownames=FALSE, booktabs=TRUE)

# Figure exports
grid_bess <- grid.arrange(grobs = plots_bess, ncol = 3)
ggsave("sensitivity_BESS.pdf", grid_bess, width = 8, height = 3)


# ==============================================================================
# SECTION 2 : Elasticity relative to H2 costs
# ==============================================================================

scenarios_h2 <- c(0.5, 0.6, 0.8, 1, 1.25, 1.5, 2)
data_h2 <- bind_rows(lapply(scenarios_h2, load_data, type="H2"))
df_ref_h2 <- data_h2 %>% filter(Technology == "BESS_E") %>% select(scenario, BESS = Investment)

df_long_h2 <- data_h2 %>%
  filter(Technology %in% technos) %>%
  left_join(df_ref_h2, by = "scenario") %>%
  mutate(log_ratio = log((BESS / Investment) + 1e-6), 
         log_cost = log(scenario))

res_h2 <- data.frame(Technology = character(), Elasticity = numeric(), R2 = numeric())
plots_h2 <- list()

for (tech in technos) {
  # Filtration on very low investment levels (we assimilate any investment below 100MW to zero)
  df_sub <- df_long_h2 %>% filter(Technology == tech & Investment>1e-1)
  
  # Regression if multiple points left
  if (nrow(df_sub) > 1) {
    model <- lm(log_ratio ~ log_cost, data = df_sub)
    
    # Filling results table
    res_h2 <- rbind(res_h2, data.frame(
      Technology = tech,
      Elasticity = round(coef(model)[2], 3),
      R2 = round(summary(model)$r.squared, 3)
    ))
    
    # Figure
    plots_h2[[tech]] <- ggplot(df_sub, aes(x = log_cost, y = log_ratio)) +
      geom_point(size=3, colour="#2c3e50") +
      geom_smooth(method="lm", se=FALSE, colour="#2980b9") +
      theme_bw(base_size = 10) +
      labs(title = tech, x = "log(H2 Cost factor)", y = "log(K_BESS / K_i)") +
      annotate("text", x = -Inf, y = Inf, 
               label = paste("R² =", round(summary(model)$r.squared, 2)), 
               hjust = -0.1, vjust = 1.5, size = 3)
  }
}

# Latex Table export
print(xtable(res_h2, caption="Morishima Elasticities (H2 cost shock)", label="tab:h2_res"), 
      include.rownames=FALSE, booktabs=TRUE)

# Figure export
grid_h2 <- grid.arrange(grobs = plots_h2, ncol = 3)
ggsave("sensitivity_H2.pdf", grid_h2, width = 8, height = 3)


# ==============================================================================
# SECTION 3 : System cost elasticity
# ==============================================================================

# Function to read the right excel cell
read_sys_cost <- function(s, type) {
  prefix <- if(type == "BESS") "base_objective_BESS_" else "base_objective_H2_"
  fname <- if(s==1) paste0(path, "base_objective_1.csv") else paste0(path, prefix, s, ".csv")
  if(!file.exists(fname)) return(NA)
  df <- read.csv2(fname, header=FALSE)
  as.numeric(df[2,2])
}

scenarios_all <- c(0.5, 0.6, 0.8, 1, 1.25, 1.5, 2)
cost_bess <- data.frame(s = scenarios_all, val = sapply(scenarios_all, read_sys_cost, "BESS"))
cost_h2   <- data.frame(s = scenarios_all, val = sapply(scenarios_all, read_sys_cost, "H2"))

m_bess <- lm(log(val) ~ log(s), data = cost_bess %>% filter(!is.na(val)))
m_h2   <- lm(log(val) ~ log(s), data = cost_h2 %>% filter(!is.na(val)))

sys_res <- data.frame(
  Driver = c("BESS Cost", "H2 Cost"),
  Elasticity = c(round(coef(m_bess)[2], 3), round(coef(m_h2)[2], 3)),
  R2 = c(round(summary(m_bess)$r.squared, 3), round(summary(m_h2)$r.squared, 3))
)

print(xtable(sys_res, caption="System Cost Elasticity", label="tab:sys_res"), 
      include.rownames=FALSE, booktabs=TRUE)
