# Load required libraries
library(ggplot2)
library(dplyr)
library(scales)
library(tidyr)

# Read the CSV files for all three scenarios
high_cost <- read.csv("base_objective_BESS_1.5.csv", header = TRUE, sep=';')  # +50% BESS cost
baseline <- read.csv("base_objective_1.csv", header = TRUE, sep=';')   # Baseline BESS cost  
low_cost <- read.csv("base_objective_BESS_0.5.csv", header = TRUE, sep=';')   # -50% BESS cost

# Clean and prepare the data with grouping
prepare_data <- function(df, scenario_name) {
  df %>%
    select(Technology, x4) %>%
    rename(Investment = x4) %>%
    mutate(
      Investment = as.numeric(Investment),
      # Group technologies as requested
      Tech_Group = case_when(
        Technology %in% c("OCGT", "CCGT") ~ "CCGT+OCGT",
        Technology %in% c("ONWIND", "OFFWIND") ~ "Wind",
        Technology == "UHS" ~ "UHS",
        Technology == "BESS_E" ~ "BESS (Energy)",
        Technology == "BESS_P" ~ "BESS (Power)", 
        Technology == "PtG" ~ "PtG",
        Technology == "H2T" ~ "H2T",
        TRUE ~ Technology
      ),
      Scenario = scenario_name
    ) %>%
    # Sum investments for grouped technologies
    group_by(Tech_Group, Scenario) %>%
    summarise(Investment = sum(Investment, na.rm = TRUE), .groups = 'drop')
}

# Définir un ordre fixe des technologies
tech_order <- c("CCGT+OCGT", "Wind", "PV", "SMR", "BESS (Energy)", "BESS (Power)", 
                "UHS", "PtG", "H2T")

# Combine all scenarios
all_data <- rbind(
  prepare_data(low_cost, "Low Cost\n(-50%)"),
  prepare_data(baseline, "Baseline"),
  prepare_data(high_cost, "High Cost\n(+50%)")
)


# Order scenarios and filter out zero investments
all_data <- all_data %>%
  mutate(
    Scenario = factor(Scenario, levels = c("Low Cost\n(-50%)", "Baseline", "High Cost\n(+50%)")),
    Tech_Group = factor(Tech_Group)
  ) %>%
  # Filter out technologies with zero investment across all scenarios
  group_by(Tech_Group) %>%
  filter(sum(Investment) > 0) %>%
  ungroup()

# Create clean, professional color palette for scenarios
scenario_colors <- c(
  "Low Cost\n(-50%)" = "#2ecc71",    # Green for low cost
  "Baseline" = "#3498db",            # Blue for baseline  
  "High Cost\n(+50%)" = "#e74c3c"    # Red for high cost
)

# Create the main chart - grouped bar chart
p_main <- all_data %>%
  ggplot(aes(x = Tech_Group, y = Investment, fill = Scenario)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75, alpha = 0.9) +
  scale_fill_manual(values = scenario_colors, name = "BESS Cost Scenario") +
  scale_y_continuous(
    labels = comma_format(scale = 1, accuracy = 1),
    breaks = pretty_breaks(n = 6),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = NULL,
    y = "Investment Capacity (GW(h))",
    caption = "Investment levels across BESS cost scenarios: -50%, baseline, and +50% cost"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    # Clean, professional styling
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "#ecf0f1", size = 0.4),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    
    # Typography
    axis.text.x = element_text(size = 11, color = "#2c3e50", angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10, color = "#2c3e50"),
    axis.title.y = element_text(size = 12, color = "#2c3e50", margin = margin(r = 15)),
    plot.caption = element_text(size = 9, color = "#7f8c8d", hjust = 0),
    
    # Legend
    legend.position = "bottom",
    legend.title = element_text(size = 11, color = "#2c3e50"),
    legend.text = element_text(size = 10, color = "#2c3e50"),
    legend.margin = margin(t = 15),
    legend.key.size = unit(0.8, "cm"),
    
    # Spacing and margins
    plot.margin = margin(20, 25, 20, 25),
    
    # Remove axis lines for cleaner look
    axis.line = element_blank(),
    panel.border = element_blank()
  ) +
  guides(fill = guide_legend(
    title.position = "top",
    title.hjust = 0.5,
    nrow = 1,
    byrow = TRUE
  ))

# Create a focused version for technologies with significant changes
# Calculate percentage changes for sorting
tech_changes <- all_data %>%
  pivot_wider(names_from = Scenario, values_from = Investment) %>%
  mutate(
    Change_LowToHigh = `High Cost\n(+50%)` - `Low Cost\n(-50%)`,
    Pct_Change = ifelse(`Low Cost\n(-50%)` == 0, 0, 
                        (Change_LowToHigh / `Low Cost\n(-50%)`) * 100),
    Max_Investment = pmax(`Low Cost\n(-50%)`, Baseline, `High Cost\n(+50%)`, na.rm = TRUE)
  ) %>%
  arrange(desc(Max_Investment))

all_data$Tech_Group <- factor(all_data$Tech_Group, levels = tech_order)

# Create the main chart - faceted by technology with free scales
p_final <- all_data %>%
  ggplot(aes(x = Scenario, y = Investment, fill = Scenario)) +
  geom_col(width = 0.7, alpha = 0.9) +
  facet_wrap(~Tech_Group, scales = "free_y", ncol = 4) +
  scale_fill_manual(values = scenario_colors, name = "BESS Cost Scenario") +
  scale_y_continuous(
    labels = function(x) ifelse(abs(x) >= 1000, paste0(round(x/1000, 1), "k"), comma(x, accuracy = 1)),
    breaks = pretty_breaks(n = 4),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = NULL,
    y = "Investment Capacity (GW(h))",
#    caption = "Investment levels across BESS cost scenarios with individual scales per technology"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    # Clean, professional styling
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "#ecf0f1", size = 0.4),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    
    # Typography
    axis.text.x = element_text(size = 9, color = "#2c3e50", angle = 45, hjust = 1),
    axis.text.y = element_text(size = 9, color = "#2c3e50"),
    axis.title.y = element_text(size = 11, color = "#2c3e50", margin = margin(r = 15)),
    plot.caption = element_text(size = 9, color = "#7f8c8d", hjust = 0),
    
    # Facet styling
    strip.text = element_text(size = 10, face = "bold", color = "#2c3e50"),
    strip.background = element_rect(fill = "#f8f9fa", color = NA),
    
    # Legend
    legend.position = "bottom",
    legend.title = element_text(size = 10, color = "#2c3e50"),
    legend.text = element_text(size = 9, color = "#2c3e50"),
    legend.margin = margin(t = 10),
    legend.key.size = unit(0.6, "cm"),
    
    # Spacing and margins
    panel.spacing = unit(0.8, "lines"),
    plot.margin = margin(15, 20, 20, 20),
    
    # Remove axis lines for cleaner look
    axis.line = element_blank(),
    panel.border = element_blank()
  ) +
  guides(fill = guide_legend(
    title.position = "top",
    title.hjust = 0.5,
    nrow = 1,
    byrow = TRUE
  ))

# Display the plot
print(p_final)

# Save high-quality PDF
ggsave("bess_investment_comparison.pdf", plot = p_final, 
       width = 8, height = 5, dpi = 300, device = "pdf",
       bg = "white")

# Print summary table
cat("Investment Summary by Technology and Scenario (GW):\n")
summary_table <- all_data %>%
  pivot_wider(names_from = Scenario, values_from = Investment) %>%
  arrange(desc(`Baseline`)) %>%
  mutate(across(where(is.numeric), ~ round(.x, 1)))

print(summary_table)

# Print key insights
cat("\nKey Investment Changes (Low Cost to High Cost):\n")
changes <- tech_changes %>%
  select(Tech_Group, `Low Cost\n(-50%)`, `High Cost\n(+50%)`, Change_LowToHigh, Pct_Change) %>%
  arrange(desc(abs(Change_LowToHigh))) %>%
  mutate(across(where(is.numeric), ~ round(.x, 1)))

print(changes)




#########################################################################
## H2
#########################################################################


# Read the CSV files for all three scenarios
high_cost <- read.csv("base_objective_H2_1.5.csv", header = TRUE, sep=';')  # +50% BESS cost
baseline <- read.csv("base_objective_1.csv", header = TRUE, sep=';')   # Baseline BESS cost  
low_cost <- read.csv("base_objective_H2_0.5.csv", header = TRUE, sep=';')   # -50% BESS cost

# Clean and prepare the data with grouping
prepare_data <- function(df, scenario_name) {
  df %>%
    select(Technology, x4) %>%
    rename(Investment = x4) %>%
    mutate(
      Investment = as.numeric(Investment),
      # Group technologies as requested
      Tech_Group = case_when(
        Technology %in% c("OCGT", "CCGT") ~ "CCGT+OCGT",
        Technology %in% c("ONWIND", "OFFWIND") ~ "Wind",
        Technology == "UHS" ~ "UHS",
        Technology == "BESS_E" ~ "BESS (Energy)",
        Technology == "BESS_P" ~ "BESS (Power)", 
        Technology == "PtG" ~ "PtG",
        Technology == "H2T" ~ "H2T",
        TRUE ~ Technology
      ),
      Scenario = scenario_name
    ) %>%
    # Sum investments for grouped technologies
    group_by(Tech_Group, Scenario) %>%
    summarise(Investment = sum(Investment, na.rm = TRUE), .groups = 'drop')
}

# Combine all scenarios
all_data <- rbind(
  prepare_data(low_cost, "Low Cost\n(-50%)"),
  prepare_data(baseline, "Baseline"),
  prepare_data(high_cost, "High Cost\n(+50%)")
)

# Order scenarios and filter out zero investments
all_data <- all_data %>%
  mutate(
    Scenario = factor(Scenario, levels = c("Low Cost\n(-50%)", "Baseline", "High Cost\n(+50%)")),
    Tech_Group = factor(Tech_Group)
  ) %>%
  # Filter out technologies with zero investment across all scenarios
  group_by(Tech_Group) %>%
  filter(sum(Investment) > 0) %>%
  ungroup()

# Create clean, professional color palette for scenarios
scenario_colors <- c(
  "Low Cost\n(-50%)" = "#2ecc71",    # Green for low cost
  "Baseline" = "#3498db",            # Blue for baseline  
  "High Cost\n(+50%)" = "#e74c3c"    # Red for high cost
)

# Create the main chart - grouped bar chart
p_main <- all_data %>%
  ggplot(aes(x = Tech_Group, y = Investment, fill = Scenario)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75, alpha = 0.9) +
  scale_fill_manual(values = scenario_colors, name = "BESS Cost Scenario") +
  scale_y_continuous(
    labels = comma_format(scale = 1, accuracy = 1),
    breaks = pretty_breaks(n = 6),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = NULL,
    y = "Investment Capacity (GW(h))",
    caption = "Investment levels across H2 cost scenarios: -50%, baseline, and +50% cost"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    # Clean, professional styling
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "#ecf0f1", size = 0.4),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    
    # Typography
    axis.text.x = element_text(size = 11, color = "#2c3e50", angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10, color = "#2c3e50"),
    axis.title.y = element_text(size = 12, color = "#2c3e50", margin = margin(r = 15)),
    plot.caption = element_text(size = 9, color = "#7f8c8d", hjust = 0),
    
    # Legend
    legend.position = "bottom",
    legend.title = element_text(size = 11, color = "#2c3e50"),
    legend.text = element_text(size = 10, color = "#2c3e50"),
    legend.margin = margin(t = 15),
    legend.key.size = unit(0.8, "cm"),
    
    # Spacing and margins
    plot.margin = margin(20, 25, 20, 25),
    
    # Remove axis lines for cleaner look
    axis.line = element_blank(),
    panel.border = element_blank()
  ) +
  guides(fill = guide_legend(
    title.position = "top",
    title.hjust = 0.5,
    nrow = 1,
    byrow = TRUE
  ))

# Create a focused version for technologies with significant changes
# Calculate percentage changes for sorting
tech_changes <- all_data %>%
  pivot_wider(names_from = Scenario, values_from = Investment) %>%
  mutate(
    Change_LowToHigh = `High Cost\n(+50%)` - `Low Cost\n(-50%)`,
    Pct_Change = ifelse(`Low Cost\n(-50%)` == 0, 0, 
                        (Change_LowToHigh / `Low Cost\n(-50%)`) * 100),
    Max_Investment = pmax(`Low Cost\n(-50%)`, Baseline, `High Cost\n(+50%)`, na.rm = TRUE)
  ) %>%
  arrange(desc(Max_Investment))

all_data$Tech_Group <- factor(all_data$Tech_Group, levels = tech_order)

# Create the main chart - faceted by technology with free scales
p_final <- all_data %>%
  ggplot(aes(x = Scenario, y = Investment, fill = Scenario)) +
  geom_col(width = 0.7, alpha = 0.9) +
  facet_wrap(~Tech_Group, scales = "free_y", ncol = 4) +
  scale_fill_manual(values = scenario_colors, name = "H2 Cost Scenario") +
  scale_y_continuous(
    labels = function(x) ifelse(abs(x) >= 1000, paste0(round(x/1000, 1), "k"), comma(x, accuracy = 1)),
    breaks = pretty_breaks(n = 4),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = NULL,
    y = "Investment Capacity (GW(h))",
    #    caption = "Investment levels across H2 cost scenarios with individual scales per technology"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    # Clean, professional styling
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "#ecf0f1", size = 0.4),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    
    # Typography
    axis.text.x = element_text(size = 9, color = "#2c3e50", angle = 45, hjust = 1),
    axis.text.y = element_text(size = 9, color = "#2c3e50"),
    axis.title.y = element_text(size = 11, color = "#2c3e50", margin = margin(r = 15)),
    plot.caption = element_text(size = 9, color = "#7f8c8d", hjust = 0),
    
    # Facet styling
    strip.text = element_text(size = 10, face = "bold", color = "#2c3e50"),
    strip.background = element_rect(fill = "#f8f9fa", color = NA),
    
    # Legend
    legend.position = "bottom",
    legend.title = element_text(size = 10, color = "#2c3e50"),
    legend.text = element_text(size = 9, color = "#2c3e50"),
    legend.margin = margin(t = 10),
    legend.key.size = unit(0.6, "cm"),
    
    # Spacing and margins
    panel.spacing = unit(0.8, "lines"),
    plot.margin = margin(15, 20, 20, 20),
    
    # Remove axis lines for cleaner look
    axis.line = element_blank(),
    panel.border = element_blank()
  ) +
  guides(fill = guide_legend(
    title.position = "top",
    title.hjust = 0.5,
    nrow = 1,
    byrow = TRUE
  ))

# Display the plot
print(p_final)

# Save high-quality PDF
ggsave("H2_investment_comparison.pdf", plot = p_final, 
       width = 8, height = 5, dpi = 300, device = "pdf",
       bg = "white")

# Print summary table
cat("Investment Summary by Technology and Scenario (GW):\n")
summary_table <- all_data %>%
  pivot_wider(names_from = Scenario, values_from = Investment) %>%
  arrange(desc(`Baseline`)) %>%
  mutate(across(where(is.numeric), ~ round(.x, 1)))

print(summary_table)

# Print key insights
cat("\nKey Investment Changes (Low Cost to High Cost):\n")
changes <- tech_changes %>%
  select(Tech_Group, `Low Cost\n(-50%)`, `High Cost\n(+50%)`, Change_LowToHigh, Pct_Change) %>%
  arrange(desc(abs(Change_LowToHigh))) %>%
  mutate(across(where(is.numeric), ~ round(.x, 1)))

print(changes)




