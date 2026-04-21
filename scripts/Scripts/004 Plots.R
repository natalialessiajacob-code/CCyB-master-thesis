# Descriptive Plots: Volume & Growth Analysis

library(tidyverse)
library(scales)
library(patchwork)

df <- readRDS("../processed_data/04_data_final_for_regression.rds")

# bank category colours
col_full    <- "#000000"
col_no_gsib <- "#4A4A4A"
col_cat1    <- "#1A5276"
col_cat2    <- "#2E86C1"
col_cat3    <- "#85C1E9"
col_cat4    <- "#A9A9A9"

# Werte in CHF thousands → scale 1e-6 = Billions

# Hilfsfunktion: CCyB event lines + annotations 
add_ccyb_lines <- function(p, y_pos) {
  p +
    geom_vline(xintercept = 2020, linetype = "dotted", color = "grey40", alpha = 0.7) +
    geom_vline(xintercept = 2022, linetype = "dotted", color = "grey40", alpha = 0.7) +
    annotate("text", x = 2020.1, y = y_pos * 0.95,
             label = "CCyB Release", hjust = 0, size = 3, color = "grey40") +
    annotate("text", x = 2022.1, y = y_pos * 0.90,
             label = "CCyB Reactivation", hjust = 0, size = 3, color = "grey40")
}

sample_colors <- c(
  "Full Sample (N=25)" = col_full,
  "Excl. G-SIBs"       = col_no_gsib,
  "Category 1"         = col_cat1,
  "Category 2"         = col_cat2,
  "Category 3"         = col_cat3,
  "Category 4"         = col_cat4
)

theme_thesis <- function() {
  theme_minimal() +
    theme(legend.position  = "bottom",
          legend.box       = "horizontal",
          panel.grid.minor = element_blank(),
          plot.background  = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA))
}


# Aggregation nach Jahr + Sample + Kategorie
agg_mortgages <- df %>%
  group_by(year) %>%
  summarise(res_full = sum(residential_mortgages, na.rm=T),
            other_full = sum(other_mortgages, na.rm=T), .groups = "drop") %>%
  mutate(sample = "Full Sample (N=25)")

agg_mortgages_no_gsib <- df %>%
  filter(kategorie != 1) %>%
  group_by(year) %>%
  summarise(res_full = sum(residential_mortgages, na.rm=T),
            other_full = sum(other_mortgages, na.rm=T), .groups = "drop") %>%
  mutate(sample = "Excl. G-SIBs")

agg_by_cat <- df %>%
  group_by(year, kategorie) %>%
  summarise(res_full = sum(residential_mortgages, na.rm=T),
            other_full = sum(other_mortgages, na.rm=T), .groups = "drop") %>%
  mutate(sample = paste0("Category ", kategorie))

agg_combined <- bind_rows(
  agg_mortgages,
  agg_mortgages_no_gsib,
  agg_by_cat %>% select(year, res_full, other_full, sample)
)


# Plot 1a: Residential + Other Mortgages  
p1a <- agg_combined %>%
  pivot_longer(c(res_full, other_full), names_to = "type", values_to = "volume") %>%
  mutate(type = if_else(type == "res_full", "Residential Mortgages", "Other Mortgages")) %>%
  ggplot(aes(x = year, y = volume, color = sample, linetype = type)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "B CHF")) +
  scale_x_continuous(breaks = 2018:2024) +
  scale_color_manual(values = sample_colors) +
  scale_linetype_manual(values = c("Residential Mortgages" = "solid",
                                   "Other Mortgages" = "dashed")) +
  labs(y = "Volume (CHF Billions)", x = "", color = "Sample", linetype = "Mortgage Type") +
  theme_thesis()

p1a <- add_ccyb_lines(p1a, max(agg_combined$res_full, na.rm=T))
print(p1a)


# Plot 1b: Residential mortgages
p1b <- agg_combined %>%
  ggplot(aes(x = year, y = res_full, color = sample)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "B CHF")) +
  scale_x_continuous(breaks = 2018:2024) +
  scale_color_manual(values = sample_colors) +
  labs(y = "Volume (CHF Billions)", x = "Year", color = "Sample") +
  theme_thesis()

p1b <- add_ccyb_lines(p1b, max(agg_combined$res_full, na.rm=T))
print(p1b)


# Plot 1c: Other Mortgages
p1c <- agg_combined %>%
  ggplot(aes(x = year, y = other_full, color = sample)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "B CHF")) +
  scale_x_continuous(breaks = 2018:2024) +
  scale_color_manual(values = sample_colors) +
  labs(y = "Volume (CHF Billions)", x = "Year", color = "Sample") +
  theme_thesis()

p1c <- add_ccyb_lines(p1c, max(agg_combined$other_full, na.rm=T))
print(p1c)


# Plot 2: Non-residential loans
agg_nonres_combined <- bind_rows(
  df %>% mutate(non_res = gross_loans - residential_mortgages) %>%
    group_by(year) %>%
    summarise(vol_full = sum(non_res, na.rm=T), .groups="drop") %>%
    mutate(sample = "Full Sample (N=25)"),
  df %>% filter(kategorie != 1) %>%
    mutate(non_res = gross_loans - residential_mortgages) %>%
    group_by(year) %>%
    summarise(vol_full = sum(non_res, na.rm=T), .groups="drop") %>%
    mutate(sample = "Excl. G-SIBs"),
  df %>% mutate(non_res = gross_loans - residential_mortgages) %>%
    group_by(year, kategorie) %>%
    summarise(vol_full = sum(non_res, na.rm=T), .groups="drop") %>%
    mutate(sample = paste0("Category ", kategorie)) %>%
    select(year, vol_full, sample)
)

p2 <- agg_nonres_combined %>%
  ggplot(aes(x = year, y = vol_full, color = sample)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "B CHF")) +
  scale_x_continuous(breaks = 2018:2024) +
  scale_color_manual(values = sample_colors) +
  labs(y = "Volume (CHF Billions)", x = "", color = "Sample") +
  theme_thesis()

p2 <- add_ccyb_lines(p2, max(agg_nonres_combined$vol_full, na.rm=T))
print(p2)


# Plot 3: RWA 
agg_rwa_combined <- bind_rows(
  df %>% group_by(year) %>%
    summarise(rwa_full = sum(rwa, na.rm=T), .groups="drop") %>%
    mutate(sample = "Full Sample (N=25)"),
  df %>% filter(kategorie != 1) %>%
    group_by(year) %>%
    summarise(rwa_full = sum(rwa, na.rm=T), .groups="drop") %>%
    mutate(sample = "Excl. G-SIBs"),
  df %>% group_by(year, kategorie) %>%
    summarise(rwa_full = sum(rwa, na.rm=T), .groups="drop") %>%
    mutate(sample = paste0("Category ", kategorie)) %>%
    select(year, rwa_full, sample)
)

p3 <- agg_rwa_combined %>%
  ggplot(aes(x = year, y = rwa_full, color = sample)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "B CHF")) +
  scale_x_continuous(breaks = 2018:2024) +
  scale_color_manual(values = sample_colors) +
  labs(y = "RWA (CHF Billions)", x = "", color = "Sample") +
  theme_thesis()

p3 <- add_ccyb_lines(p3, max(agg_rwa_combined$rwa_full, na.rm=T))
print(p3)


# Plot 4: Dual axis – Volume (Balken diagramm) + YoY growth residential mortgages
dual_full <- df %>%
  group_by(year) %>%
  summarise(volume = sum(residential_mortgages, na.rm=T), .groups="drop") %>%
  mutate(growth = (volume / lag(volume) - 1) * 100, sample = "Full Sample (N=25)")

dual_no_gsib <- df %>%
  filter(kategorie != 1) %>%
  group_by(year) %>%
  summarise(volume = sum(residential_mortgages, na.rm=T), .groups="drop") %>%
  mutate(growth = (volume / lag(volume) - 1) * 100, sample = "Excl. G-SIBs")

scale_factor <- max(dual_full$volume, na.rm=T) /
  max(abs(dual_full$growth), na.rm=T) * 0.4

p4 <- ggplot() +
  geom_col(data = dual_full, aes(x = year, y = volume),
           fill = "grey85", width = 0.6, alpha = 0.7) +
  geom_line(data = dual_full,
            aes(x = year, y = growth * scale_factor, color = "Full Sample (N=25)"),
            linewidth = 1) +
  geom_point(data = dual_full,
             aes(x = year, y = growth * scale_factor, color = "Full Sample (N=25)"),
             size = 2.5) +
  geom_line(data = dual_no_gsib,
            aes(x = year, y = growth * scale_factor, color = "Excl. G-SIBs"),
            linewidth = 1, linetype = "dashed") +
  geom_point(data = dual_no_gsib,
             aes(x = year, y = growth * scale_factor, color = "Excl. G-SIBs"),
             size = 2.5) +
  geom_vline(xintercept = 2020, linetype = "dotted", color = "grey40", alpha = 0.7) +
  geom_vline(xintercept = 2022, linetype = "dotted", color = "grey40", alpha = 0.7) +
  annotate("text", x = 2020.1, y = max(dual_full$volume, na.rm=T) * 0.95,
           label = "CCyB\nRelease", hjust = 0, size = 2.8, color = "grey40") +
  annotate("text", x = 2022.1, y = max(dual_full$volume, na.rm=T) * 0.95,
           label = "CCyB\nReactivation", hjust = 0, size = 2.8, color = "grey40") +
  scale_y_continuous(
    name = "Volume (CHF Billions)",
    labels = label_number(scale = 1e-6, suffix = "B CHF"),
    sec.axis = sec_axis(~ . / scale_factor, name = "YoY Growth Rate (%)",
                        labels = function(x) paste0(round(x, 1), "%"))
  ) +
  scale_x_continuous(breaks = 2018:2024) +
  scale_color_manual(values = c("Full Sample (N=25)" = col_full,
                                "Excl. G-SIBs" = col_no_gsib)) +
  labs(x = "Year", color = "Sample") +
  theme_thesis() +
  theme(axis.title.y.right = element_text(color = col_no_gsib),
        axis.text.y.right  = element_text(color = col_no_gsib))

print(p4)


# Stacked Bar + Growth Lines nach Kategorie
stacked_data <- df %>%
  mutate(kategorie_label = case_when(
    kategorie == 1 ~ "Category 1 (G-SIBs)",
    kategorie == 2 ~ "Category 2 (D-SIBs)",
    kategorie == 3 ~ "Category 3",
    kategorie == 4 ~ "Category 4"
  ),
  kategorie_label = factor(kategorie_label,
                           levels = c("Category 4", "Category 3",
                                      "Category 2 (D-SIBs)", "Category 1 (G-SIBs)"))) %>%
  group_by(year, kategorie_label) %>%
  summarise(volume = sum(residential_mortgages, na.rm=T), .groups="drop")

total_volume <- stacked_data %>%
  group_by(year) %>%
  summarise(total = sum(volume), .groups="drop") %>%
  mutate(growth = (total / lag(total) - 1) * 100)

no_gsib_volume <- df %>%
  filter(kategorie != 1) %>%
  group_by(year) %>%
  summarise(total = sum(residential_mortgages, na.rm=T), .groups="drop") %>%
  mutate(growth = (total / lag(total) - 1) * 100)

max_vol    <- max(total_volume$total, na.rm=T)
scale_factor_stack <- max_vol / max(abs(total_volume$growth), na.rm=T) * 0.4

cat_colors_stack <- c("Category 1 (G-SIBs)"  = col_cat1,
                      "Category 2 (D-SIBs)"  = col_cat2,
                      "Category 3"            = col_cat3,
                      "Category 4"            = col_cat4)

p_stacked <- ggplot() +
  geom_col(data = stacked_data,
           aes(x = year, y = volume, fill = kategorie_label),
           width = 0.6, alpha = 0.85) +
  geom_line(data = total_volume,
            aes(x = year, y = growth * scale_factor_stack, color = "Full Sample (N=25)"),
            linewidth = 1.1) +
  geom_point(data = total_volume,
             aes(x = year, y = growth * scale_factor_stack, color = "Full Sample (N=25)"),
             size = 2.5) +
  geom_line(data = no_gsib_volume,
            aes(x = year, y = growth * scale_factor_stack, color = "Excl. G-SIBs"),
            linewidth = 1.1, linetype = "dashed") +
  geom_point(data = no_gsib_volume,
             aes(x = year, y = growth * scale_factor_stack, color = "Excl. G-SIBs"),
             size = 2.5) +
  geom_vline(xintercept = 2020, linetype = "dotted", color = "grey40", alpha = 0.8) +
  geom_vline(xintercept = 2022, linetype = "dotted", color = "grey40", alpha = 0.8) +
  annotate("text", x = 2020.1, y = max_vol * 0.97,
           label = "CCyB\nRelease", hjust = 0, size = 2.8, color = "grey40") +
  annotate("text", x = 2022.1, y = max_vol * 0.97,
           label = "CCyB\nReactivation", hjust = 0, size = 2.8, color = "grey40") +
  scale_y_continuous(
    name = "Residential Mortgage Volume (CHF Billions)",
    labels = label_number(scale = 1e-6, suffix = "B"),
    sec.axis = sec_axis(~ . / scale_factor_stack, name = "YoY Growth Rate (%)",
                        labels = function(x) paste0(round(x, 1), "%"))
  ) +
  scale_x_continuous(breaks = 2018:2024) +
  scale_fill_manual(values = cat_colors_stack, name = "Bank Category") +
  scale_color_manual(values = c("Full Sample (N=25)" = col_full,
                                "Excl. G-SIBs" = col_no_gsib),
                     name = "Growth Rate") +
  labs(x = "") +
  theme_thesis() +
  theme(axis.title.y.right = element_text(color = col_no_gsib),
        axis.text.y.right  = element_text(color = col_no_gsib)) +
  guides(fill  = guide_legend(nrow = 2, title.position = "top", order = 1),
         color = guide_legend(nrow = 1, title.position = "top", order = 2))

print(p_stacked)


# Portfolio Composition
port_combined <- bind_rows(
  df %>% group_by(year) %>%
    summarise(total_mort = sum(residential_mortgages + other_mortgages, na.rm=T),
              res_mort   = sum(residential_mortgages, na.rm=T),
              loans      = sum(gross_loans, na.rm=T), .groups="drop") %>%
    mutate(mort_share = (total_mort / loans) * 100,
           res_share  = (res_mort / loans) * 100,
           sample = "Full Sample (N=25)"),
  df %>% filter(kategorie != 1) %>% group_by(year) %>%
    summarise(total_mort = sum(residential_mortgages + other_mortgages, na.rm=T),
              res_mort   = sum(residential_mortgages, na.rm=T),
              loans      = sum(gross_loans, na.rm=T), .groups="drop") %>%
    mutate(mort_share = (total_mort / loans) * 100,
           res_share  = (res_mort / loans) * 100,
           sample = "Excl. G-SIBs (N=23)")
) %>%
  pivot_longer(c(mort_share, res_share), names_to = "metric", values_to = "share") %>%
  mutate(metric_label = if_else(metric == "mort_share",
                                "Total Mortgages / Total Loans",
                                "Residential Mortgages / Total Loans"))

p_portfolio <- ggplot(port_combined,
                      aes(x = year, y = share, color = metric_label, linetype = sample)) +
  geom_line(linewidth = 1.0) +
  geom_point(size = 2.5) +
  geom_vline(xintercept = 2020, linetype = "dotted", color = "grey40", alpha = 0.8) +
  geom_vline(xintercept = 2022, linetype = "dotted", color = "grey40", alpha = 0.8) +
  annotate("text", x = 2020.1, y = max(port_combined$share, na.rm=T) * 0.99,
           label = "CCyB\nRelease", hjust = 0, size = 2.8, color = "grey40") +
  annotate("text", x = 2022.1, y = max(port_combined$share, na.rm=T) * 0.99,
           label = "CCyB\nReactivation", hjust = 0, size = 2.8, color = "grey40") +
  scale_color_manual(values = c("Total Mortgages / Total Loans" = col_cat1,
                                "Residential Mortgages / Total Loans" = col_cat3),
                     name = "Metric") +
  scale_linetype_manual(values = c("Full Sample (N=25)"   = "solid",
                                   "Excl. G-SIBs (N=23)" = "dashed"),
                        name = "Sample") +
  scale_x_continuous(breaks = 2018:2024) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(x = "Year", y = "Share of Total Loans (%)") +
  theme_thesis() +
  theme(legend.box = "vertical") +
  guides(color    = guide_legend(nrow = 2, title.position = "top", order = 1),
         linetype = guide_legend(nrow = 2, title.position = "top", order = 2))

print(p_portfolio)


# Loan Breakdown: Stacked Bars (Full vs Excl. G-SIBs nebeneinander)
loan_combined <- bind_rows(
  df %>% group_by(year) %>%
    summarise(residential = sum(residential_mortgages, na.rm=T),
              other_mort  = sum(other_mortgages, na.rm=T),
              gross_loans = sum(gross_loans, na.rm=T), .groups="drop") %>%
    mutate(rest = gross_loans - residential - other_mort,
           sample = "Full Sample (N=25)"),
  df %>% filter(kategorie != 1) %>% group_by(year) %>%
    summarise(residential = sum(residential_mortgages, na.rm=T),
              other_mort  = sum(other_mortgages, na.rm=T),
              gross_loans = sum(gross_loans, na.rm=T), .groups="drop") %>%
    mutate(rest = gross_loans - residential - other_mort,
           sample = "Excl. G-SIBs (N=23)")
) %>%
  pivot_longer(c(residential, other_mort, rest), names_to = "segment", values_to = "volume") %>%
  mutate(
    segment_label = case_when(
      segment == "residential" ~ "Residential Mortgages",
      segment == "other_mort"  ~ "Other Mortgages",
      segment == "rest"        ~ "Other Loans"
    ),
    segment_label = factor(segment_label,
                           levels = c("Other Loans", "Other Mortgages", "Residential Mortgages")),
    year_offset = if_else(sample == "Full Sample (N=25)", year - 0.2, year + 0.2)
  )

max_loan_vol <- loan_combined %>% group_by(year, sample) %>%
  summarise(total = sum(volume), .groups="drop") %>% pull(total) %>% max(na.rm=T)

p_loans <- ggplot(loan_combined,
                  aes(x = year_offset, y = volume, fill = segment_label, alpha = sample)) +
  geom_col(width = 0.35) +
  geom_vline(xintercept = 2020, linetype = "dotted", color = "grey40", alpha = 0.8) +
  geom_vline(xintercept = 2022, linetype = "dotted", color = "grey40", alpha = 0.8) +
  annotate("text", x = 2020.1, y = max_loan_vol * 0.97,
           label = "CCyB\nRelease", hjust = 0, size = 2.8, color = "grey40") +
  annotate("text", x = 2022.1, y = max_loan_vol * 0.97,
           label = "CCyB\nReactivation", hjust = 0, size = 2.8, color = "grey40") +
  scale_fill_manual(values = c("Residential Mortgages" = col_cat1,
                               "Other Mortgages"       = col_cat2,
                               "Other Loans"           = col_cat4),
                    name = "Loan Segment") +
  scale_alpha_manual(values = c("Full Sample (N=25)"   = 0.90,
                                "Excl. G-SIBs (N=23)" = 0.55),
                     name = "Sample") +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "B"),
                     name = "Volume (CHF Billions)") +
  scale_x_continuous(breaks = 2018:2024) +
  labs(x = "Year") +
  theme_thesis() +
  theme(legend.box = "vertical") +
  guides(fill  = guide_legend(nrow = 1, title.position = "top", order = 1),
         alpha = guide_legend(nrow = 1, title.position = "top", order = 2))

print(p_loans)


# Loan Growth Rates 
prep_loan_growth <- function(data, sample_label) {
  data %>%
    group_by(year) %>%
    summarise(total_loans           = sum(gross_loans, na.rm=T),
              total_mortgages       = sum(residential_mortgages + other_mortgages, na.rm=T),
              residential_mortgages = sum(residential_mortgages, na.rm=T),
              other_mortgages       = sum(other_mortgages, na.rm=T), .groups="drop") %>%
    mutate(across(c(total_loans, total_mortgages, residential_mortgages, other_mortgages),
                  ~ (.x / lag(.x) - 1) * 100),
           sample = sample_label)
}

loan_growth_combined <- bind_rows(
  prep_loan_growth(df, "Full Sample (N=25)"),
  prep_loan_growth(df %>% filter(kategorie != 1), "Excl. G-SIBs (N=23)")
) %>%
  pivot_longer(c(total_loans, total_mortgages, residential_mortgages, other_mortgages),
               names_to = "metric", values_to = "growth") %>%
  mutate(
    metric_label = case_when(
      metric == "total_loans"           ~ "Total Loans",
      metric == "total_mortgages"       ~ "Total Mortgages",
      metric == "residential_mortgages" ~ "Residential Mortgages",
      metric == "other_mortgages"       ~ "Other Mortgages"
    ),
    metric_label = factor(metric_label,
                          levels = c("Total Loans", "Total Mortgages",
                                     "Residential Mortgages", "Other Mortgages"))
  ) %>%
  filter(!is.na(growth))

loan_colors <- c("Total Loans" = col_full, "Total Mortgages" = col_cat1,
                 "Residential Mortgages" = col_cat2, "Other Mortgages" = col_cat3)

# alle zusammen (linetype = sample)
p_loan_growth <- ggplot(loan_growth_combined,
                        aes(x = year, y = growth, color = metric_label, linetype = sample)) +
  geom_line(linewidth = 1.0) +
  geom_point(size = 2.5) +
  geom_hline(yintercept = 0, color = "grey60", linewidth = 0.5) +
  geom_vline(xintercept = 2020, linetype = "dotted", color = "grey40", alpha = 0.8) +
  geom_vline(xintercept = 2022, linetype = "dotted", color = "grey40", alpha = 0.8) +
  annotate("text", x = 2020.1, y = max(loan_growth_combined$growth, na.rm=T) * 0.97,
           label = "CCyB\nRelease", hjust = 0, size = 2.8, color = "grey40") +
  annotate("text", x = 2022.1, y = max(loan_growth_combined$growth, na.rm=T) * 0.97,
           label = "CCyB\nReactivation", hjust = 0, size = 2.8, color = "grey40") +
  scale_color_manual(values = loan_colors, name = "Loan Category") +
  scale_linetype_manual(values = c("Full Sample (N=25)"   = "solid",
                                   "Excl. G-SIBs (N=23)" = "dashed"),
                        name = "Sample") +
  scale_y_continuous(labels = function(x) paste0(x, "%"), name = "YoY Growth Rate (%)") +
  scale_x_continuous(breaks = 2018:2024) +
  labs(x = "") +
  theme_thesis() +
  guides(color    = guide_legend(nrow = 1, title.position = "top", order = 1),
         linetype = guide_legend(nrow = 1, title.position = "top", order = 2))

print(p_loan_growth)

# Full vs Excl. G-SIBs separat mit patchwork
make_growth_plot <- function(data, title_label) {
  ggplot(data, aes(x = year, y = growth, color = metric_label)) +
    geom_line(linewidth = 1.0) +
    geom_point(size = 2.5) +
    geom_hline(yintercept = 0, color = "grey60", linewidth = 0.5) +
    geom_vline(xintercept = 2020, linetype = "dotted", color = "grey40", alpha = 0.8) +
    geom_vline(xintercept = 2022, linetype = "dotted", color = "grey40", alpha = 0.8) +
    annotate("text", x = 2020.1, y = max(loan_growth_combined$growth, na.rm=T) * 0.97,
             label = "CCyB\nRelease", hjust = 0, size = 2.5, color = "grey40") +
    annotate("text", x = 2022.1, y = max(loan_growth_combined$growth, na.rm=T) * 0.97,
             label = "CCyB\nReactivation", hjust = 0, size = 2.5, color = "grey40") +
    scale_color_manual(values = loan_colors, name = "Loan Category") +
    scale_y_continuous(labels = function(x) paste0(x, "%"), name = "YoY Growth Rate (%)") +
    scale_x_continuous(breaks = 2018:2024) +
    labs(title = title_label, x = "Year") +
    theme_thesis() +
    guides(color = guide_legend(nrow = 2, title.position = "top"))
}

p_growth_full   <- make_growth_plot(filter(loan_growth_combined, sample == "Full Sample (N=25)"),
                                    "Full Sample (N=25)")
p_growth_nocat1 <- make_growth_plot(filter(loan_growth_combined, sample == "Excl. G-SIBs (N=23)"),
                                    "Excl. G-SIBs (N=23)")

print(p_growth_full)
print(p_growth_nocat1)


# save
dir.create("../figures", showWarnings = FALSE, recursive = TRUE)

ggsave("../figures/01_mortgages_combined.png",      p1a,            width=12, height=7, dpi=300, bg="white")
ggsave("../figures/02_residential_mortgages.png",   p1b,            width=12, height=7, dpi=300, bg="white")
ggsave("../figures/03_other_mortgages.png",         p1c,            width=12, height=7, dpi=300, bg="white")
ggsave("../figures/04_nonres_loans.png",            p2,             width=12, height=7, dpi=300, bg="white")
ggsave("../figures/05_rwa.png",                     p3,             width=12, height=7, dpi=300, bg="white")
ggsave("../figures/06_dual_axis_volume_growth.png", p4,             width=12, height=7, dpi=300, bg="white")
ggsave("../figures/07_combined_balance_sheet.png",  p2 + p4 + plot_layout(guides="collect"),
       width=16, height=7, dpi=300, bg="white")
ggsave("../figures/08_stacked_volume_growth.png",   p_stacked,      width=12, height=7, dpi=300, bg="white")
ggsave("../figures/09_portfolio_composition.png",   p_portfolio,    width=12, height=7, dpi=300, bg="white")
ggsave("../figures/10_loan_breakdown.png",          p_loans,        width=12, height=7, dpi=300, bg="white")
ggsave("../figures/11_loan_growth_rates.png",       p_loan_growth,  width=12, height=7, dpi=300, bg="white")
ggsave("../figures/12_loan_growth_full.png",        p_growth_full,  width=12, height=7, dpi=300, bg="white")
ggsave("../figures/13_loan_growth_nocat1.png",      p_growth_nocat1,width=12, height=7, dpi=300, bg="white")


#--------------
# Plots: Growth Variables High vs Low Exposure, mit und ohne G-SIBs
# plus CET1 Scatterplot

library(tidyverse)
library(patchwork)

setwd("/Users/natalialessia/Desktop/Masterarbeit/CCYB Analyse/MasterThesis_CCyB_Switzerland/Data/Data Raw/imported_data/")

df <- readRDS("../processed_data/04_data_final_for_regression.rds")

dir.create("../figures/final_plots", showWarnings = FALSE, recursive = TRUE)

# Farben
col_full    <- "#000000"
col_no_gsib <- "#4A4A4A"
col_cat1    <- "#1A5276"
col_cat2    <- "#2E86C1"
col_cat3    <- "#85C1E9"
col_cat4    <- "#A9A9A9"

exposure_colors <- c("High Exposure" = col_cat1, "Low Exposure" = col_cat3)


# Daten vorbereiten
df_final <- df %>%
  mutate(
    total_mortgages                = residential_mortgages + other_mortgages,
    non_residential_mortgage_loans = other_mortgages + (gross_loans - total_mortgages)
  ) %>%
  group_by(bank_id) %>%
  arrange(year) %>%
  mutate(
    total_mortgage_growth_yoy           = (total_mortgages / lag(total_mortgages) - 1) * 100,
    non_residential_mortgage_growth_yoy = (non_residential_mortgage_loans / lag(non_residential_mortgage_loans) - 1) * 100
  ) %>%
  ungroup() %>%
  mutate(
    exposure_group = case_when(
      high_exposure_2019 == 1 ~ "High Exposure",
      high_exposure_2019 == 0 ~ "Low Exposure"
    ),
    sample_type = "All Banks (N=25)"
  )

df_no_cat1 <- df_final %>%
  filter(kategorie != "1") %>%
  mutate(sample_type = "Excl. Cat 1 (N=23)")

df_combined <- bind_rows(df_final, df_no_cat1) %>%
  mutate(sample_type = factor(sample_type,
                              levels = c("All Banks (N=25)", "Excl. Cat 1 (N=23)")))


# Plotting Funktion
create_comparison_plot <- function(data, var_name, filename_base) {
  
  plot_data <- data %>%
    filter(!is.na(exposure_group), !is.na(.data[[var_name]])) %>%
    group_by(year, exposure_group, sample_type) %>%
    summarise(mean_growth = mean(.data[[var_name]], na.rm=T), .groups="drop")
  
  y_max <- max(plot_data$mean_growth, na.rm=T)
  
  p <- ggplot(plot_data, aes(x = year, y = mean_growth,
                             color = exposure_group,
                             linetype = sample_type,
                             group = interaction(exposure_group, sample_type))) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "gray50") +
    geom_vline(xintercept = 2020, linetype = "dotted", color = "grey40", alpha = 0.7) +
    geom_vline(xintercept = 2022, linetype = "dotted", color = "grey40", alpha = 0.7) +
    annotate("text", x = 2020.1, y = y_max * 0.95,
             label = "CCyB Release", hjust = 0, size = 3, color = "grey40") +
    annotate("text", x = 2022.1, y = y_max * 0.90,
             label = "CCyB Reactivation", hjust = 0, size = 3, color = "grey40") +
    scale_color_manual(values = exposure_colors, name = "Exposure Group") +
    scale_linetype_manual(values = c("All Banks (N=25)"   = "solid",
                                     "Excl. Cat 1 (N=23)" = "dashed"),
                          name = "Sample") +
    scale_x_continuous(breaks = 2018:2024) +
    labs(x = "", y = "Average YoY Growth (%)") +
    theme_minimal(base_size = 13) +
    theme(legend.position  = "bottom",
          legend.box       = "horizontal",
          panel.grid.minor = element_blank(),
          plot.background  = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA))
  
  ggsave(paste0("../figures/final_plots/", filename_base, ".png"), p,
         width = 12, height = 7, dpi = 300, bg = "white")
  
  return(p)
}

p1 <- create_comparison_plot(df_combined, "residential_mortgage_growth_yoy",
                             "residential_mortgage_growth_comparison")

p2 <- create_comparison_plot(df_combined, "non_residential_mortgage_growth_yoy",
                             "non_residential_mortgage_growth_comparison")

p3 <- create_comparison_plot(df_combined, "total_mortgage_growth_yoy",
                             "total_mortgage_growth_comparison")

# kombinierter Plot
p_combined <- (p1 / p2 / p3) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave("../figures/final_plots/COMBINED_all_three_comparisons.png", p_combined,
       width = 14, height = 18, dpi = 300, bg = "white")


# CET1 Scatterplot 2019 vs 2021
cet1_scatter_data <- df %>%
  filter(year %in% c(2019, 2021)) %>%
  select(bank_name, year, cet1_ratio_pct, kategorie) %>%
  pivot_wider(names_from = year, values_from = cet1_ratio_pct, names_prefix = "cet1_") %>%
  mutate(kategorie = as.factor(kategorie))

cat_colors <- c("1" = col_cat1, "2" = col_cat2, "3" = col_cat3, "4" = col_cat4)

p_cet1 <- ggplot(cet1_scatter_data,
                 aes(x = cet1_2019, y = cet1_2021, color = kategorie)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "gray50", linewidth = 0.8) +
  geom_point(size = 3, alpha = 0.85) +
  geom_text(aes(label = bank_name), hjust = -0.1, vjust = 0.5,
            size = 2.5, check_overlap = TRUE, color = "black") +
  scale_color_manual(values = cat_colors, name = "Bank Category") +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(x = "CET1 Ratio 2019 (%)", y = "CET1 Ratio 2021 (%)") +
  theme_minimal(base_size = 13) +
  theme(legend.position  = "bottom",
        panel.grid.minor = element_blank(),
        plot.background  = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA))

print(p_cet1)
ggsave("../figures/final_plots/cet1_ratio_scatter_2019_vs_2021.png", p_cet1,
       width = 10, height = 8, dpi = 300, bg = "white")


# summary tabelle
df_combined %>%
  filter(!is.na(exposure_group)) %>%
  group_by(sample_type, exposure_group) %>%
  summarise(
    n_banks           = n_distinct(bank_id),
    res_mort_mean     = mean(residential_mortgage_growth_yoy,         na.rm=T),
    non_res_mort_mean = mean(non_residential_mortgage_growth_yoy,     na.rm=T),
    total_mort_mean   = mean(total_mortgage_growth_yoy,               na.rm=T),
    .groups = "drop"
  ) %>%
  write.csv("../tables/growth_comparison_with_without_cat1.csv", row.names = FALSE)