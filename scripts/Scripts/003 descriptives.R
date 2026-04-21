# Script 3: Descriptive Statistics and Balance table

library(tidyverse)
library(knitr)
library(kableExtra)

df <- readRDS("../processed_data/04_data_final_for_regression.rds")


# Variablen vorbereiten
df_stats <- df %>%
  mutate(
    res_mortgages_mio    = residential_mortgages / 1000,
    other_mortgages_mio  = other_mortgages / 1000,
    loans_excl_res_mio   = (gross_loans - residential_mortgages) / 1000,
    rwa_mio              = rwa / 1000,
    res_share_assets     = residential_share_of_assets,
    res_share_loans      = residential_share_of_loans,
    other_mortgage_share = other_mortgage_share_of_loans,
    mortgage_share_loans = mortgage_share_of_loans,
    mortgage_vol_ch      = mortgages_CH_change,
    property_price_growth = property_price_change,
    rent_price_growth    = rent_price_change,
    credit_gdp_ch        = credit_gdp_change,
    gdp_growth_ch        = gdp_growth,
    unemployment_ch      = unemployment_change
  )


# helper functions
compute_stats <- function(data, var) {
  x <- data[[var]]
  x <- x[!is.na(x)]
  tibble(
    n_   = length(x),
    mean_ = mean(x),
    sd_   = sd(x),
    p10_  = as.numeric(quantile(x, 0.10)),
    p50_  = as.numeric(quantile(x, 0.50)),
    p90_  = as.numeric(quantile(x, 0.90))
  )
}

compute_group_stats <- function(var_df, data) {
  var_df %>%
    rowwise() %>%
    mutate(stats = list(compute_stats(data, variable))) %>%
    unnest(stats) %>%
    ungroup()
}

format_row <- function(stats_df) {
  stats_df %>%
    mutate(across(c(mean_, sd_, p10_, p50_, p90_),
                  ~ formatC(.x, format = "f", digits = 2)),
           n_ = as.integer(n_)) %>%
    rename(n = n_, mean = mean_, sd = sd_, p10 = p10_, p50 = p50_, p90 = p90_) %>%
    select(label, n, mean, sd, p10, p50, p90)
}

header_row <- function(title) {
  tibble(label = paste0("\\textit{", title, "}"),
         n = NA_integer_, mean = NA_character_, sd = NA_character_,
         p10 = NA_character_, p50 = NA_character_, p90 = NA_character_)
}


# Variablengruppen definieren
outcome_vars <- tribble(
  ~variable,                          ~label,
  "residential_mortgage_growth_yoy",  "Residential mortgage growth",
  "mortgage_growth_yoy",              "Total mortgage growth",
  "non_mortgage_growth_yoy",          "Non-mortgage credit growth",
  "other_mortgage_growth_yoy",        "Other mortgage growth"
)

volume_vars <- tribble(
  ~variable,              ~label,
  "res_mortgages_mio",    "Residential mortgages",
  "other_mortgages_mio",  "Other mortgages",
  "loans_excl_res_mio",   "Total loans excl. residential mortgages",
  "rwa_mio",              "Risk-weighted assets"
)

portfolio_vars <- tribble(
  ~variable,               ~label,
  "res_share_assets",      "Residential mortgages / Total assets",
  "res_share_loans",       "Residential mortgages / Total loans",
  "mortgage_share_loans",  "Total mortgages / Total loans",
  "other_mortgage_share",  "Other mortgages / Total loans"
)

capital_vars <- tribble(
  ~variable,         ~label,
  "cet1_ratio",      "CET1 ratio",
  "tier1_ratio",     "Tier 1 ratio",
  "leverage_ratio",  "Leverage ratio",
  "roaa",            "Return on average assets",
  "operating_roaa",  "Operating return on average assets",
  "roae",            "Return on average equity"
)

risk_vars <- tribble(
  ~variable,          ~label,
  "npl_ratio",        "NPL ratio",
  "loan_loss_ratio",  "Loan loss allowances / Gross loans"
)

macro_vars <- tribble(
  ~variable,                ~label,
  "mortgage_vol_ch",        "Change in mortgage volume, Switzerland",
  "property_price_growth",  "Growth rate residential property prices",
  "rent_price_growth",      "Growth rate rent prices",
  "credit_gdp_ch",          "Change in credit-to-GDP ratio",
  "gdp_growth_ch",          "Real GDP growth",
  "unemployment_ch",        "Change in unemployment rate"
)


# stats berechnen
stats_outcome   <- compute_group_stats(outcome_vars,   df_stats)
stats_volume    <- compute_group_stats(volume_vars,    df_stats)
stats_portfolio <- compute_group_stats(portfolio_vars, df_stats)
stats_capital   <- compute_group_stats(capital_vars,   df_stats)
stats_risk      <- compute_group_stats(risk_vars,      df_stats)
stats_macro     <- compute_group_stats(macro_vars,     df_stats)

# treatment vars separat – nur Referenzjahre (N=25)
stats_treatment <- bind_rows(
  compute_group_stats(
    tribble(~variable, ~label, "ccyb_exposure_2019", "CCyB exposure (end-2019)"),
    df_stats %>% filter(year == 2019)
  ),
  compute_group_stats(
    tribble(~variable, ~label, "ccyb_exposure_2021", "CCyB exposure (end-2021)"),
    df_stats %>% filter(year == 2021)
  )
)


# alles zusammenführen
all_stats <- bind_rows(
  header_row("Outcome Variables (\\%)"),
  format_row(stats_outcome),
  header_row("Absolute Volumes (CHF millions)"),
  format_row(stats_volume),
  header_row("Portfolio Composition (\\%)"),
  format_row(stats_portfolio),
  header_row("Treatment Variables"),
  format_row(stats_treatment),
  header_row("Capital \\& Profitability (\\%)"),
  format_row(stats_capital),
  header_row("Credit Risk (\\%)"),
  format_row(stats_risk),
  header_row("Macroeconomic Controls (\\%)"),
  format_row(stats_macro)
)


# LaTeX output
body_lines <- c()

for (i in seq_len(nrow(all_stats))) {
  row <- all_stats[i, ]
  if (is.na(row$n)) {
    line <- paste0("\\multicolumn{7}{l}{", row$label, "} \\\\")
  } else {
    line <- paste0(
      "\\hspace{1em}", row$label, " & ",
      row$n, " & ", row$mean, " & ", row$sd, " & ",
      row$p10, " & ", row$p50, " & ", row$p90, " \\\\"
    )
  }
  body_lines <- c(body_lines, line)
}

latex_output <- paste0(
  "\\begin{table}[htbp]
\\centering
\\caption{Descriptive Statistics}
\\label{tab:descriptive_stats}
\\small
\\begin{tabular}{p{7.5cm} r r r r r r}
\\toprule
 & N & Mean & Std.\\ Dev. & p10 & p50 & p90 \\\\
\\midrule
",
  paste(body_lines, collapse = "\n"),
  "
\\bottomrule
\\end{tabular}
\\end{table}"
)

dir.create("../tables/", showWarnings = FALSE, recursive = TRUE)
writeLines(latex_output, "../tables/descriptive_stats.tex")
# preview
cat(latex_output)

# Balance table
#full sample period with median split dividing in high and low exposure bank, based on 2019


library(tidyverse)

# Load data
df <- readRDS("../processed_data/04_data_final_for_regression.rds")


median_2019 <- median(df %>% filter(year == 2019) %>% pull(ccyb_exposure_2019), na.rm = TRUE)

df_bal <- df %>%
  mutate(
    exposure_group      = if_else(ccyb_exposure_2019 > median_2019, "High Exposure", "Low Exposure"),
    res_mortgages_mio   = residential_mortgages / 1000,
    other_mortgages_mio = other_mortgages / 1000,
    loans_excl_res_mio  = (gross_loans - residential_mortgages) / 1000,
    rwa_mio             = rwa / 1000,
    deposit_ratio       = (customer_deposits / total_assets) * 100
  )

data_high <- df_bal %>% filter(exposure_group == "High Exposure")
data_low  <- df_bal %>% filter(exposure_group == "Low Exposure")

data_high_2019 <- df_bal %>% filter(exposure_group == "High Exposure", year == 2019)
data_low_2019  <- df_bal %>% filter(exposure_group == "Low Exposure",  year == 2019)
data_high_2021 <- df_bal %>% filter(exposure_group == "High Exposure", year == 2021)
data_low_2021  <- df_bal %>% filter(exposure_group == "Low Exposure",  year == 2021)

fmt <- function(x, digits = 2) {
  if (is.na(x)) return("")
  formatC(x, format = "f", digits = digits)
}

compute_group <- function(var_df, data_high, data_low) {
  var_df %>%
    rowwise() %>%
    mutate(
      n_high    = sum(!is.na(data_high[[variable]])),
      mean_high = mean(data_high[[variable]], na.rm = TRUE),
      sd_high   = sd(data_high[[variable]],   na.rm = TRUE),
      n_low     = sum(!is.na(data_low[[variable]])),
      mean_low  = mean(data_low[[variable]],  na.rm = TRUE),
      sd_low    = sd(data_low[[variable]],    na.rm = TRUE)
    ) %>%
    ungroup()
}

header_row_bal <- function(title) {
  tibble(label = paste0("\\textit{", title, "}"),
         mean_high = NA_character_, sd_high = NA_character_, n_high = NA_integer_,
         mean_low  = NA_character_, sd_low  = NA_character_, n_low  = NA_integer_)
}

format_group <- function(stats_df) {
  stats_df %>%
    mutate(
      mean_high = map_chr(mean_high, ~ fmt(.x)),
      sd_high   = map_chr(sd_high,   ~ fmt(.x)),
      n_high    = as.integer(n_high),
      mean_low  = map_chr(mean_low,  ~ fmt(.x)),
      sd_low    = map_chr(sd_low,    ~ fmt(.x)),
      n_low     = as.integer(n_low)
    ) %>%
    select(label, mean_high, sd_high, n_high, mean_low, sd_low, n_low)
}

stats_treat_bal <- bind_rows(
  compute_group(tribble(~variable, ~label, "ccyb_exposure_2019", "CCyB exposure (end-2019)"),
                data_high_2019, data_low_2019),
  compute_group(tribble(~variable, ~label, "ccyb_exposure_2021", "CCyB exposure (end-2021)"),
                data_high_2021, data_low_2021)
)

all_stats_bal <- bind_rows(
  header_row_bal("Outcome Variables (\\%)"),
  format_group(compute_group(outcome_vars,   data_high, data_low)),
  header_row_bal("Absolute Volumes (CHF millions)"),
  format_group(compute_group(volume_vars,    data_high, data_low)),
  header_row_bal("Portfolio Composition (\\%)"),
  format_group(compute_group(portfolio_vars, data_high, data_low)),
  header_row_bal("Treatment Variables"),
  format_group(stats_treat_bal),
  header_row_bal("Capital \\& Profitability (\\%)"),
  format_group(compute_group(capital_vars,   data_high, data_low)),
  header_row_bal("Credit Risk (\\%)"),
  format_group(compute_group(risk_vars,      data_high, data_low))
)

dim(all_stats_bal) # quick check

body_lines_bal <- c()
for (i in seq_len(nrow(all_stats_bal))) {
  row <- all_stats_bal[i, ]
  if (is.na(row$n_high)) {
    body_lines_bal <- c(body_lines_bal,
                        paste0("\\multicolumn{7}{l}{", row$label, "} \\\\"))
  } else {
    body_lines_bal <- c(body_lines_bal,
                        paste0("\\hspace{1em}", row$label, " & ",
                               row$mean_high, " & ", row$sd_high, " & ", row$n_high, " & ",
                               row$mean_low,  " & ", row$sd_low,  " & ", row$n_low,  " \\\\"))
  }
}

latex_bal <- paste0(
  "\\begin{table}[htbp]
\\centering
\\caption{Balance Table: High vs.\\ Low CCyB Exposure Banks}
\\label{tab:balance}
\\small
\\begin{tabular}{p{6.5cm} rr r rr r}
\\toprule
 & \\multicolumn{3}{c}{High Exposure (N\\,=\\,12)}
 & \\multicolumn{3}{c}{Low Exposure (N\\,=\\,13)} \\\\
\\cmidrule(lr){2-4}\\cmidrule(lr){5-7}
 & Mean & SD & Obs. & Mean & SD & Obs. \\\\
\\midrule
",
  paste(body_lines_bal, collapse = "\n"),
  "
\\bottomrule
\\end{tabular}
\\end{table}")

writeLines(latex_bal, "../tables/balance_table_revised.tex")
cat(latex_bal)

# Mortgage Concentration: domestic banks vs G-SIBs
# Motivation der Stichprobe (ohne UBS & CS), was ist Anteil mortgages in Markt?

library(tidyverse)

df <- readRDS("../processed_data/02_data_with_treatment.rds")


gsibs <- c("UBS Switzerland AG", "Credit Suisse AG")

df_full <- df %>%
  mutate(mortgage_to_assets = (residential_mortgages / total_assets) * 100) %>%
  filter(!is.na(mortgage_to_assets), !is.na(total_assets))

df_domestic <- df_full %>% filter(!bank_name %in% gsibs)

# check
n_distinct(df_full$bank_name)
n_distinct(df_domestic$bank_name)


# summary statistics
df_full %>%
  summarise(n = n(), n_banks = n_distinct(bank_name),
            mean = mean(mortgage_to_assets, na.rm=T),
            median = median(mortgage_to_assets, na.rm=T),
            sd = sd(mortgage_to_assets, na.rm=T))

df_domestic %>%
  summarise(n = n(), n_banks = n_distinct(bank_name),
            mean = mean(mortgage_to_assets, na.rm=T),
            median = median(mortgage_to_assets, na.rm=T),
            sd = sd(mortgage_to_assets, na.rm=T))

# G-SIBs zum Vergleich
df_full %>%
  filter(bank_name %in% gsibs) %>%
  group_by(bank_name) %>%
  summarise(avg = mean(mortgage_to_assets, na.rm=T), n = n())

# nach Bank (domestic)
by_bank <- df_domestic %>%
  group_by(bank_name) %>%
  summarise(avg_mortgage_to_assets = mean(mortgage_to_assets, na.rm=T)) %>%
  arrange(desc(avg_mortgage_to_assets)) %>%
  mutate(avg_mortgage_to_assets = round(avg_mortgage_to_assets, 1))

print(by_bank, n = Inf)
median(by_bank$avg_mortgage_to_assets) # für Thesis-Text


# plot: domestic vs G-SIBs über Zeit
plot_data <- df_full %>%
  mutate(bank_type = if_else(bank_name %in% gsibs, "G-SIBs", "Domestic Banks")) %>%
  group_by(bank_type, year) %>%
  summarise(mean = mean(mortgage_to_assets, na.rm=T), .groups = "drop")

p <- ggplot(plot_data, aes(x = year, y = mean, color = bank_type, group = bank_type)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Domestic Banks" = "#2c7bb6", "G-SIBs" = "#d7191c")) +
  scale_x_continuous(breaks = 2018:2024) +
  labs(x = "Year", y = "Residential Mortgages / Total Assets (%)", color = NULL) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

print(p)
ggsave("../figures/mortgage_concentration_domestic_vs_gsibs.png", p,
       width = 10, height = 6, dpi = 300)


# save
dir.create("../tables", showWarnings = FALSE, recursive = TRUE)
write.csv(by_bank, "../tables/mortgage_concentration_domestic_banks.csv", row.names = FALSE)