# Script 5: Main Regressions, Robustness, Event Study

library(tidyverse)
library(fixest)

df <- readRDS("../processed_data/04_data_final_for_regression.rds")


# Interaktionsterme 
df_reg <- df %>%
  mutate(
    treat_release      = ccyb_exposure_2019_std * post_2020,
    treat_reactivation = ccyb_exposure_2021_std * post_2022,
    
    # Median Split Dummies
    treat_release_d50  = high_exposure_2019 * post_2020,
    treat_react_d50    = high_exposure_2021 * post_2022
  )

df_no_cat1   <- df_reg %>% filter(kategorie != 1)
df_no_cat1_2 <- df_reg %>% filter(kategorie %in% c(3, 4))

n_distinct(df_reg$bank_id)
n_distinct(df_no_cat1$bank_id)
n_distinct(df_no_cat1_2$bank_id)


# Baseline Regressionen: Full Sample
m1_full <- feols(residential_mortgage_growth_yoy ~
                   treat_release + treat_reactivation | bank_id + year,
                 data = df_reg, cluster = ~bank_id)

m2_full <- feols(mortgage_growth_yoy ~
                   treat_release + treat_reactivation | bank_id + year,
                 data = df_reg, cluster = ~bank_id)

m3_full <- feols(other_mortgage_growth_yoy ~
                   treat_release + treat_reactivation | bank_id + year,
                 data = df_reg, cluster = ~bank_id)

m4_full <- feols(non_mortgage_growth_yoy ~
                   treat_release + treat_reactivation | bank_id + year,
                 data = df_reg, cluster = ~bank_id)

m5_full <- feols(npl_ratio ~
                   treat_release + treat_reactivation | bank_id + year,
                 data = df_reg, cluster = ~bank_id)

summary(m1_full)
summary(m2_full)
summary(m3_full)
summary(m4_full)
summary(m5_full)


# Baseline: Excl. G-SIBs
m1_nocat1 <- feols(residential_mortgage_growth_yoy ~
                     treat_release + treat_reactivation | bank_id + year,
                   data = df_no_cat1, cluster = ~bank_id)

m2_nocat1 <- feols(mortgage_growth_yoy ~
                     treat_release + treat_reactivation | bank_id + year,
                   data = df_no_cat1, cluster = ~bank_id)

m3_nocat1 <- feols(other_mortgage_growth_yoy ~
                     treat_release + treat_reactivation | bank_id + year,
                   data = df_no_cat1, cluster = ~bank_id)

m4_nocat1 <- feols(non_mortgage_growth_yoy ~
                     treat_release + treat_reactivation | bank_id + year,
                   data = df_no_cat1, cluster = ~bank_id)

m5_nocat1 <- feols(npl_ratio ~
                     treat_release + treat_reactivation | bank_id + year,
                   data = df_no_cat1, cluster = ~bank_id)

summary(m1_nocat1)


# Robustness: Bank Controls
m_rob_ctrl_full <- feols(
  residential_mortgage_growth_yoy ~
    treat_release + treat_reactivation +
    log(total_assets) + roaa + liquid_assets_ratio +
    I(customer_deposits / total_assets * 100) | bank_id + year,
  data = df_reg, cluster = ~bank_id)

m_rob_ctrl_nocat1 <- feols(
  residential_mortgage_growth_yoy ~
    treat_release + treat_reactivation +
    log(total_assets) + roaa + liquid_assets_ratio +
    I(customer_deposits / total_assets * 100) | bank_id + year,
  data = df_no_cat1, cluster = ~bank_id)

# Robustness: Median Dummy
m_rob_d50_full <- feols(
  residential_mortgage_growth_yoy ~
    treat_release_d50 + treat_react_d50 | bank_id + year,
  data = df_reg, cluster = ~bank_id)

m_rob_d50_nocat1 <- feols(
  residential_mortgage_growth_yoy ~
    treat_release_d50 + treat_react_d50 | bank_id + year,
  data = df_no_cat1, cluster = ~bank_id)

# Robustness: Only Cat 3+4
m_rob_nocat12 <- feols(
  residential_mortgage_growth_yoy ~
    treat_release + treat_reactivation | bank_id + year,
  data = df_no_cat1_2, cluster = ~bank_id)

summary(m_rob_ctrl_full)
summary(m_rob_d50_full)
summary(m_rob_nocat12)


# P80 Threshold
p80_2019 <- quantile(df_reg %>% filter(year == 2019) %>%
                       pull(ccyb_exposure_2019), 0.80, na.rm=T)
p80_2021 <- quantile(df_reg %>% filter(year == 2021) %>%
                       pull(ccyb_exposure_2021), 0.80, na.rm=T)

round(p80_2019, 4); round(p80_2021, 4)

df_reg <- df_reg %>%
  mutate(
    high_exp_2019_p80 = if_else(ccyb_exposure_2019 > p80_2019, 1L, 0L),
    high_exp_2021_p80 = if_else(ccyb_exposure_2021 > p80_2021, 1L, 0L),
    treat_release_p80 = high_exp_2019_p80 * post_2020,
    treat_react_p80   = high_exp_2021_p80 * post_2022
  )

df_no_cat1 <- df_no_cat1 %>%
  mutate(
    high_exp_2019_p80 = if_else(ccyb_exposure_2019 > p80_2019, 1L, 0L),
    high_exp_2021_p80 = if_else(ccyb_exposure_2021 > p80_2021, 1L, 0L),
    treat_release_p80 = high_exp_2019_p80 * post_2020,
    treat_react_p80   = high_exp_2021_p80 * post_2022
  )

# welche Banken sind über P80?
df_reg %>% filter(year == 2019, high_exp_2019_p80 == 1) %>%
  select(bank_name, ccyb_exposure_2019) %>% arrange(desc(ccyb_exposure_2019))

df_reg %>% filter(year == 2021, high_exp_2021_p80 == 1) %>%
  select(bank_name, ccyb_exposure_2021) %>% arrange(desc(ccyb_exposure_2021))


# P80 Regressionen, Full und ohne G-SIBs
m_p80_res     <- feols(residential_mortgage_growth_yoy ~
                         treat_release_p80 + treat_react_p80 | bank_id + year,
                       data = df_reg, cluster = ~bank_id)
m_p80_mort    <- feols(mortgage_growth_yoy ~
                         treat_release_p80 + treat_react_p80 | bank_id + year,
                       data = df_reg, cluster = ~bank_id)
m_p80_other   <- feols(other_mortgage_growth_yoy ~
                         treat_release_p80 + treat_react_p80 | bank_id + year,
                       data = df_reg, cluster = ~bank_id)
m_p80_nonmort <- feols(non_mortgage_growth_yoy ~
                         treat_release_p80 + treat_react_p80 | bank_id + year,
                       data = df_reg, cluster = ~bank_id)
m_p80_npl     <- feols(npl_ratio ~
                         treat_release_p80 + treat_react_p80 | bank_id + year,
                       data = df_reg, cluster = ~bank_id)

m_p80_nc_res     <- feols(residential_mortgage_growth_yoy ~
                            treat_release_p80 + treat_react_p80 | bank_id + year,
                          data = df_no_cat1, cluster = ~bank_id)
m_p80_nc_mort    <- feols(mortgage_growth_yoy ~
                            treat_release_p80 + treat_react_p80 | bank_id + year,
                          data = df_no_cat1, cluster = ~bank_id)
m_p80_nc_other   <- feols(other_mortgage_growth_yoy ~
                            treat_release_p80 + treat_react_p80 | bank_id + year,
                          data = df_no_cat1, cluster = ~bank_id)
m_p80_nc_nonmort <- feols(non_mortgage_growth_yoy ~
                            treat_release_p80 + treat_react_p80 | bank_id + year,
                          data = df_no_cat1, cluster = ~bank_id)
m_p80_nc_npl     <- feols(npl_ratio ~
                            treat_release_p80 + treat_react_p80 | bank_id + year,
                          data = df_no_cat1, cluster = ~bank_id)

summary(m_p80_res)
summary(m_p80_nc_res)


# Bank Controls
m_rob1_res     <- feols(residential_mortgage_growth_yoy ~
                          treat_release + treat_reactivation +
                          log(total_assets) + roaa + liquid_assets_ratio +
                          I(customer_deposits / total_assets * 100) | bank_id + year,
                        data = df_reg, cluster = ~bank_id)
m_rob1_mort    <- feols(mortgage_growth_yoy ~
                          treat_release + treat_reactivation +
                          log(total_assets) + roaa + liquid_assets_ratio +
                          I(customer_deposits / total_assets * 100) | bank_id + year,
                        data = df_reg, cluster = ~bank_id)
m_rob1_other   <- feols(other_mortgage_growth_yoy ~
                          treat_release + treat_reactivation +
                          log(total_assets) + roaa + liquid_assets_ratio +
                          I(customer_deposits / total_assets * 100) | bank_id + year,
                        data = df_reg, cluster = ~bank_id)
m_rob1_nonmort <- feols(non_mortgage_growth_yoy ~
                          treat_release + treat_reactivation +
                          log(total_assets) + roaa + liquid_assets_ratio +
                          I(customer_deposits / total_assets * 100) | bank_id + year,
                        data = df_reg, cluster = ~bank_id)
m_rob1_npl     <- feols(npl_ratio ~
                          treat_release + treat_reactivation +
                          log(total_assets) + roaa + liquid_assets_ratio +
                          I(customer_deposits / total_assets * 100) | bank_id + year,
                        data = df_reg, cluster = ~bank_id)

m_rob2_res     <- feols(residential_mortgage_growth_yoy ~
                          treat_release + treat_reactivation +
                          log(total_assets) + roaa + liquid_assets_ratio +
                          I(customer_deposits / total_assets * 100) | bank_id + year,
                        data = df_no_cat1, cluster = ~bank_id)
m_rob2_mort    <- feols(mortgage_growth_yoy ~
                          treat_release + treat_reactivation +
                          log(total_assets) + roaa + liquid_assets_ratio +
                          I(customer_deposits / total_assets * 100) | bank_id + year,
                        data = df_no_cat1, cluster = ~bank_id)
m_rob2_other   <- feols(other_mortgage_growth_yoy ~
                          treat_release + treat_reactivation +
                          log(total_assets) + roaa + liquid_assets_ratio +
                          I(customer_deposits / total_assets * 100) | bank_id + year,
                        data = df_no_cat1, cluster = ~bank_id)
m_rob2_nonmort <- feols(non_mortgage_growth_yoy ~
                          treat_release + treat_reactivation +
                          log(total_assets) + roaa + liquid_assets_ratio +
                          I(customer_deposits / total_assets * 100) | bank_id + year,
                        data = df_no_cat1, cluster = ~bank_id)
m_rob2_npl     <- feols(npl_ratio ~
                          treat_release + treat_reactivation +
                          log(total_assets) + roaa + liquid_assets_ratio +
                          I(customer_deposits / total_assets * 100) | bank_id + year,
                        data = df_no_cat1, cluster = ~bank_id)

# Median Dummy
m_rob3_res     <- feols(residential_mortgage_growth_yoy ~
                          treat_release_d50 + treat_react_d50 | bank_id + year,
                        data = df_reg, cluster = ~bank_id)
m_rob3_mort    <- feols(mortgage_growth_yoy ~
                          treat_release_d50 + treat_react_d50 | bank_id + year,
                        data = df_reg, cluster = ~bank_id)
m_rob3_other   <- feols(other_mortgage_growth_yoy ~
                          treat_release_d50 + treat_react_d50 | bank_id + year,
                        data = df_reg, cluster = ~bank_id)
m_rob3_nonmort <- feols(non_mortgage_growth_yoy ~
                          treat_release_d50 + treat_react_d50 | bank_id + year,
                        data = df_reg, cluster = ~bank_id)
m_rob3_npl     <- feols(npl_ratio ~
                          treat_release_d50 + treat_react_d50 | bank_id + year,
                        data = df_reg, cluster = ~bank_id)

m_rob4_res     <- feols(residential_mortgage_growth_yoy ~
                          treat_release_d50 + treat_react_d50 | bank_id + year,
                        data = df_no_cat1, cluster = ~bank_id)
m_rob4_mort    <- feols(mortgage_growth_yoy ~
                          treat_release_d50 + treat_react_d50 | bank_id + year,
                        data = df_no_cat1, cluster = ~bank_id)
m_rob4_other   <- feols(other_mortgage_growth_yoy ~
                          treat_release_d50 + treat_react_d50 | bank_id + year,
                        data = df_no_cat1, cluster = ~bank_id)
m_rob4_nonmort <- feols(non_mortgage_growth_yoy ~
                          treat_release_d50 + treat_react_d50 | bank_id + year,
                        data = df_no_cat1, cluster = ~bank_id)
m_rob4_npl     <- feols(npl_ratio ~
                          treat_release_d50 + treat_react_d50 | bank_id + year,
                        data = df_no_cat1, cluster = ~bank_id)

# Kategorie 3+4
m_rob6_res     <- feols(residential_mortgage_growth_yoy ~
                          treat_release + treat_reactivation | bank_id + year,
                        data = df_no_cat1_2, cluster = ~bank_id)
m_rob6_mort    <- feols(mortgage_growth_yoy ~
                          treat_release + treat_reactivation | bank_id + year,
                        data = df_no_cat1_2, cluster = ~bank_id)
m_rob6_other   <- feols(other_mortgage_growth_yoy ~
                          treat_release + treat_reactivation | bank_id + year,
                        data = df_no_cat1_2, cluster = ~bank_id)
m_rob6_nonmort <- feols(non_mortgage_growth_yoy ~
                          treat_release + treat_reactivation | bank_id + year,
                        data = df_no_cat1_2, cluster = ~bank_id)
m_rob6_npl     <- feols(npl_ratio ~
                          treat_release + treat_reactivation | bank_id + year,
                        data = df_no_cat1_2, cluster = ~bank_id)


# Event Study Modelle
m_ev_rel_full <- feols(
  residential_mortgage_growth_yoy ~
    i(year, ccyb_exposure_2019_std, ref = 2019) | bank_id + year,
  data = df_reg, cluster = ~bank_id)

m_ev_rel_nocat1 <- feols(
  residential_mortgage_growth_yoy ~
    i(year, ccyb_exposure_2019_std, ref = 2019) | bank_id + year,
  data = df_no_cat1, cluster = ~bank_id)

m_ev_react_full <- feols(
  residential_mortgage_growth_yoy ~
    i(year, ccyb_exposure_2021_std, ref = 2021) | bank_id + year,
  data = df_reg, cluster = ~bank_id)

m_ev_react_nocat1 <- feols(
  residential_mortgage_growth_yoy ~
    i(year, ccyb_exposure_2021_std, ref = 2021) | bank_id + year,
  data = df_no_cat1, cluster = ~bank_id)

# Event Study Plots
extract_iplot_data <- function(model) {
  coef_data <- coef(model)
  ci_data   <- confint(model, level = 0.90)
  tibble(
    term     = names(coef_data),
    estimate = as.numeric(coef_data),
    ci_low   = as.numeric(ci_data[, 1]),
    ci_high  = as.numeric(ci_data[, 2])
  ) %>%
    mutate(year = as.numeric(str_extract(term, "\\d{4}"))) %>%
    filter(!is.na(year)) %>%
    arrange(year)
}

make_event_plot <- function(model, ref_year, filename) {
  
  df_plot <- extract_iplot_data(model)
  
  ref_row <- tibble(term = "reference", estimate = 0,
                    ci_low = 0, ci_high = 0, year = ref_year)
  
  df_plot <- bind_rows(df_plot, ref_row) %>% arrange(year)
  
  y_max <- max(abs(c(df_plot$ci_high, df_plot$ci_low)), na.rm=T) * 1.2
  y_lim <- c(-y_max, y_max)
  
  p <- ggplot(df_plot, aes(x = year, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed",
               color = "grey50", linewidth = 0.6) +
    geom_vline(xintercept = 2020, linetype = "dotted",
               color = "grey40", alpha = 0.7) +
    geom_vline(xintercept = 2022, linetype = "dotted",
               color = "grey40", alpha = 0.7) +
    geom_ribbon(aes(ymin = ci_low, ymax = ci_high),
                fill = "#85C1E9", alpha = 0.4) +
    geom_line(color = "#1A5276", linewidth = 1.0) +
    geom_point(color = "#1A5276", size = 3, shape = 19) +
    geom_point(data = ref_row, color = "#1A5276", size = 3, shape = 1) +
    annotate("text", x = 2020.1, y = y_lim[2] * 0.9,
             label = "CCyB Release", hjust = 0, size = 3, color = "grey40") +
    annotate("text", x = 2022.1, y = y_lim[2] * 0.9,
             label = "CCyB Reactivation", hjust = 0, size = 3, color = "grey40") +
    scale_x_continuous(breaks = sort(unique(df_plot$year))) +
    scale_y_continuous(limits = y_lim) +
    labs(x = "", y = "Coefficient \u00d7 CCyB Exposure") +
    theme_minimal(base_size = 12) +
    theme(plot.background  = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA),
          panel.grid.minor = element_blank())
  
  ggsave(filename, p, width = 10, height = 6, dpi = 300, bg = "white")
  return(p)
}

p_ev_rel_full    <- make_event_plot(m_ev_rel_full,    2019,
                                    "../figures/event_study_release_full.png")
p_ev_rel_nocat1  <- make_event_plot(m_ev_rel_nocat1,  2019,
                                    "../figures/event_study_release_nocat1.png")
p_ev_react_full  <- make_event_plot(m_ev_react_full,  2021,
                                    "../figures/event_study_react_full.png")
p_ev_react_nocat1 <- make_event_plot(m_ev_react_nocat1, 2021,
                                     "../figures/event_study_react_nocat1.png")


# LaTeX Tabellen
dir.create("../tables", showWarnings = FALSE, recursive = TRUE)

get_stats <- function(model, b1_name = "treat_release",
                      b2_name = "treat_reactivation") {
  coefs <- coef(model)
  ses   <- se(model)
  pvals <- pvalue(model)
  r2_val <- tryCatch(as.numeric(fitstat(model, "r2")$r2),
                     error = function(e) NA_real_)
  list(
    b1_est = as.numeric(coefs[b1_name]),
    b1_se  = as.numeric(ses[b1_name]),
    b1_p   = as.numeric(pvals[b1_name]),
    b2_est = as.numeric(coefs[b2_name]),
    b2_se  = as.numeric(ses[b2_name]),
    b2_p   = as.numeric(pvals[b2_name]),
    n      = nobs(model),
    r2     = r2_val
  )
}

format_est <- function(est, p) {
  if (is.na(est)) return("")
  stars <- case_when(p < 0.01 ~ "***", p < 0.05 ~ "**",
                     p < 0.10 ~ "*",   TRUE ~ "")
  paste0(formatC(est, format = "f", digits = 3), stars)
}

format_se <- function(se) {
  if (is.na(se)) return("")
  paste0("(", formatC(se, format = "f", digits = 3), ")")
}

col_headers_main <- c("Res. Mort.", "Tot. Mort.", "Oth. Mort.",
                      "Non-Mort.", "NPL")


write_rob_table <- function(models, col_headers, caption, label, filename,
                            b1_name = "treat_release",
                            b2_name = "treat_reactivation",
                            note_extra = "") {
  
  stats   <- map(models, ~ get_stats(.x, b1_name, b2_name))
  b1_ests <- map_chr(stats, ~ format_est(.x$b1_est, .x$b1_p))
  b1_ses  <- map_chr(stats, ~ format_se(.x$b1_se))
  b2_ests <- map_chr(stats, ~ format_est(.x$b2_est, .x$b2_p))
  b2_ses  <- map_chr(stats, ~ format_se(.x$b2_se))
  ns      <- map_chr(stats, ~ as.character(.x$n))
  r2s     <- map_chr(stats, ~ ifelse(is.na(.x$r2), "--",
                                     formatC(.x$r2, format="f", digits=3)))
  n_cols  <- length(models)
  fe_row  <- paste(rep("\\checkmark", n_cols), collapse = " & ")
  
  b1_label <- if (b1_name == "treat_release") {
    "$\\hat{\\beta}_1$: CCyB Exp $\\times$ Post 2020"
  } else {
    "$\\hat{\\beta}_1$: High $\\times$ Post 2020"
  }
  b2_label <- if (b2_name == "treat_reactivation") {
    "$\\hat{\\beta}_2$: CCyB Exp $\\times$ Post 2022"
  } else {
    "$\\hat{\\beta}_2$: High $\\times$ Post 2022"
  }
  
  latex <- paste0(
    "\\begin{table}[htbp]\n\\centering\n",
    "\\caption{", caption, "}\n\\label{", label, "}\n\\small\n",
    "\\begin{tabular}{l ", paste(rep("c", n_cols), collapse=" "), "}\n",
    "\\toprule\n",
    " & ", paste(paste0("(", seq_len(n_cols), ")"), collapse=" & "), " \\\\\n",
    " & ", paste(col_headers, collapse=" & "), " \\\\\n",
    "\\midrule\n",
    b1_label, "\n",
    " & ", paste(b1_ests, collapse=" & "), " \\\\\n",
    " & ", paste(b1_ses,  collapse=" & "), " \\\\[4pt]\n",
    b2_label, "\n",
    " & ", paste(b2_ests, collapse=" & "), " \\\\\n",
    " & ", paste(b2_ses,  collapse=" & "), " \\\\\n",
    "\\midrule\n",
    "Observations   & ", paste(ns,  collapse=" & "), " \\\\\n",
    "$R^2$          & ", paste(r2s, collapse=" & "), " \\\\\n",
    "Bank FE        & ", fe_row, " \\\\\n",
    "Year FE        & ", fe_row, " \\\\\n",
    "\\bottomrule\n\\end{tabular}\n",
    "\\begin{minipage}{\\linewidth}\n\\vspace{4pt}\n\\footnotesize\n",
    "\\textit{Note:} Standard errors clustered at the bank level in parentheses. ",
    "CCyB exposure is standardised (mean 0, SD 1). ",
    "Column~(1): residential mortgage growth. ",
    "Column~(2): total mortgage growth. ",
    "Column~(3): other mortgage growth. ",
    "Column~(4): non-mortgage loan growth. ",
    "Column~(5): NPL ratio.\n",
    note_extra,
    "* \\(p<0.1\\), ** \\(p<0.05\\), *** \\(p<0.01\\)\n",
    "\\end{minipage}\n\\end{table}"
  )
  
  writeLines(latex, filename)
}

# Merged Main Table (Full and Excl. G-SIBs)
write_merged_table <- function(models_full, models_nocat1, col_headers,
                               caption, label, filename, note_extra = "") {
  
  n_out <- length(models_full)
  sf    <- map(models_full,   get_stats)
  snc   <- map(models_nocat1, get_stats)
  
  fmt_row <- function(stats_list, field_est, field_p, field_se) {
    list(
      ests = map_chr(stats_list, ~ format_est(.x[[field_est]], .x[[field_p]])),
      ses  = map_chr(stats_list, ~ format_se(.x[[field_se]]))
    )
  }
  
  b1f  <- fmt_row(sf,  "b1_est", "b1_p", "b1_se")
  b2f  <- fmt_row(sf,  "b2_est", "b2_p", "b2_se")
  b1nc <- fmt_row(snc, "b1_est", "b1_p", "b1_se")
  b2nc <- fmt_row(snc, "b2_est", "b2_p", "b2_se")
  
  ns_f  <- map_chr(sf,  ~ as.character(.x$n))
  ns_nc <- map_chr(snc, ~ as.character(.x$n))
  r2_f  <- map_chr(sf,  ~ ifelse(is.na(.x$r2), "--",
                                 formatC(.x$r2, format="f", digits=3)))
  r2_nc <- map_chr(snc, ~ ifelse(is.na(.x$r2), "--",
                                 formatC(.x$r2, format="f", digits=3)))
  
  fe_row <- paste(rep("\\checkmark", n_out * 2), collapse = " & ")
  
  latex <- paste0(
    "\\begin{table}[htbp]\n\\centering\n",
    "\\caption{", caption, "}\n\\label{", label, "}\n\\small\n",
    "\\begin{tabular}{l ", paste(rep("c", n_out*2), collapse=" "), "}\n",
    "\\toprule\n",
    " & \\multicolumn{", n_out, "}{c}{Full Sample}",
    " & \\multicolumn{", n_out, "}{c}{Excl. G-SIBs} \\\\\n",
    "\\cmidrule(lr){2-", n_out+1, "}",
    "\\cmidrule(lr){", n_out+2, "-", n_out*2+1, "}\n",
    " & ", paste(paste0("(", seq_len(n_out), ")"), collapse=" & "),
    " & ", paste(paste0("(", seq_len(n_out)+n_out, ")"), collapse=" & "), " \\\\\n",
    " & ", paste(rep(col_headers, 2), collapse=" & "), " \\\\\n",
    "\\midrule\n",
    "$\\hat{\\beta}_1$: CCyB Exp $\\times$ Post 2020\n",
    " & ", paste(b1f$ests,  collapse=" & "),
    " & ", paste(b1nc$ests, collapse=" & "), " \\\\\n",
    " & ", paste(b1f$ses,   collapse=" & "),
    " & ", paste(b1nc$ses,  collapse=" & "), " \\\\[4pt]\n",
    "$\\hat{\\beta}_2$: CCyB Exp $\\times$ Post 2022\n",
    " & ", paste(b2f$ests,  collapse=" & "),
    " & ", paste(b2nc$ests, collapse=" & "), " \\\\\n",
    " & ", paste(b2f$ses,   collapse=" & "),
    " & ", paste(b2nc$ses,  collapse=" & "), " \\\\\n",
    "\\midrule\n",
    "Observations & ", paste(ns_f,  collapse=" & "),
    " & ",            paste(ns_nc, collapse=" & "), " \\\\\n",
    "$R^2$        & ", paste(r2_f,  collapse=" & "),
    " & ",            paste(r2_nc, collapse=" & "), " \\\\\n",
    "Bank FE      & ", fe_row, " \\\\\n",
    "Year FE      & ", fe_row, " \\\\\n",
    "\\bottomrule\n\\end{tabular}\n",
    "\\begin{minipage}{\\linewidth}\n\\vspace{4pt}\n\\footnotesize\n",
    "\\textit{Note:} Standard errors clustered at the bank level in parentheses. ",
    "CCyB exposure is standardised (mean 0, SD 1). ",
    "Columns~(1)--(5): full sample. ",
    "Columns~(6)--(10): excluding G-SIBs (UBS and Credit Suisse). ",
    "Column~(1)/(6): residential mortgage growth. ",
    "Column~(2)/(7): total mortgage growth. ",
    "Column~(3)/(8): other mortgage growth. ",
    "Column~(4)/(9): non-mortgage loan growth. ",
    "Column~(5)/(10): NPL ratio.\n",
    note_extra,
    "* $p<0.1$, ** $p<0.05$, *** $p<0.01$\n",
    "\\end{minipage}\n\\end{table}"
  )
  
  writeLines(latex, filename)
}


# save tables
write_merged_table(
  models_full   = list(m1_full, m2_full, m3_full, m4_full, m5_full),
  models_nocat1 = list(m1_nocat1, m2_nocat1, m3_nocat1, m4_nocat1, m5_nocat1),
  col_headers   = col_headers_main,
  caption       = "Main Results: Full Sample and Excluding G-SIBs",
  label         = "tab:main_merged",
  filename      = "../tables/tab_main_merged.tex"
)

write_rob_table(
  models      = list(m_rob1_res, m_rob1_mort, m_rob1_other,
                     m_rob1_nonmort, m_rob1_npl),
  col_headers = col_headers_main,
  caption     = "Robustness: Bank-Level Controls --- Full Sample",
  label       = "tab:rob_ctrl_full",
  filename    = "../tables/tab_rob_ctrl_full.tex",
  note_extra  = note_ctrl
)

write_rob_table(
  models      = list(m_rob2_res, m_rob2_mort, m_rob2_other,
                     m_rob2_nonmort, m_rob2_npl),
  col_headers = col_headers_main,
  caption     = "Robustness: Bank-Level Controls --- Excluding G-SIBs",
  label       = "tab:rob_ctrl_nocat1",
  filename    = "../tables/tab_rob_ctrl_nocat1.tex",
  note_extra  = paste0(note_ctrl,
                       "Sample excludes Category~1 banks (UBS and Credit Suisse).\n")
)

write_rob_table(
  models      = list(m_rob3_res, m_rob3_mort, m_rob3_other,
                     m_rob3_nonmort, m_rob3_npl),
  col_headers = col_headers_main,
  caption     = "Robustness: Median Dummy --- Full Sample",
  label       = "tab:rob_d50_full",
  filename    = "../tables/tab_rob_d50_full.tex",
  b1_name     = "treat_release_d50",
  b2_name     = "treat_react_d50",
  note_extra  = note_d50
)

write_rob_table(
  models      = list(m_rob4_res, m_rob4_mort, m_rob4_other,
                     m_rob4_nonmort, m_rob4_npl),
  col_headers = col_headers_main,
  caption     = "Robustness: Median Dummy --- Excluding G-SIBs",
  label       = "tab:rob_d50_nocat1",
  filename    = "../tables/tab_rob_d50_nocat1.tex",
  b1_name     = "treat_release_d50",
  b2_name     = "treat_react_d50",
  note_extra  = paste0(note_d50,
                       "Sample excludes Category~1 banks (UBS and Credit Suisse).\n")
)

write_rob_table(
  models      = list(m_p80_res, m_p80_mort, m_p80_other,
                     m_p80_nonmort, m_p80_npl),
  col_headers = col_headers_main,
  caption     = "Robustness: 80th Percentile Dummy --- Full Sample",
  label       = "tab:rob_p80_full",
  filename    = "../tables/tab_rob_p80_full.tex",
  b1_name     = "treat_release_p80",
  b2_name     = "treat_react_p80",
  note_extra  = note_p80_full
)

write_rob_table(
  models      = list(m_p80_nc_res, m_p80_nc_mort, m_p80_nc_other,
                     m_p80_nc_nonmort, m_p80_nc_npl),
  col_headers = col_headers_main,
  caption     = "Robustness: 80th Percentile Dummy --- Excluding G-SIBs",
  label       = "tab:rob_p80_nocat1",
  filename    = "../tables/tab_rob_p80_nocat1.tex",
  b1_name     = "treat_release_p80",
  b2_name     = "treat_react_p80",
  note_extra  = note_p80_nocat1
)

write_rob_table(
  models      = list(m_rob6_res, m_rob6_mort, m_rob6_other,
                     m_rob6_nonmort, m_rob6_npl),
  col_headers = col_headers_main,
  caption     = "Robustness: Only Category 3 and 4 Banks",
  label       = "tab:rob_cat34",
  filename    = "../tables/tab_rob_cat34.tex",
  note_extra  = note_cat34
)