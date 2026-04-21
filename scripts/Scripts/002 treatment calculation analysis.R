# Script 2: Treatment Variables
# CCyB Exposure berechnen für Release 2020 und Reactivation 2022

df <- readRDS("../processed_data/01_data_merged.rds")


# Treatment 1: CCyB Exposure 2019 for 2020 Release
# Wer war 2019 stärker durch 2% CCyB belastet?

treatment_2019 <- df %>%
  filter(year == 2019) %>%
  mutate(
    ccyb_req_2019    = (ccyb_rate_pct / 100) * rwa,
    cet1_min_2019    = (cet1_min_pct / 100) * rwa,
    cet1_capital_2019 = (cet1_ratio_finma / 100) * rwa,
    excess_cap_2019  = cet1_capital_2019 - cet1_min_2019,
    ccyb_exposure_2019 = ccyb_req_2019 / excess_cap_2019
  ) %>%
  select(bank_name, bank_id, ccyb_req_2019, cet1_min_2019,
         excess_cap_2019, ccyb_exposure_2019)

summary(treatment_2019$ccyb_exposure_2019)

# check: negative excess capital
treatment_2019 %>% filter(excess_cap_2019 < 0)

# NAs / Inf
treatment_2019 %>% filter(is.na(ccyb_exposure_2019) | is.infinite(ccyb_exposure_2019))


# Treatment 2: CCyB Exposure 2021 für 2022 Reactivation
# 2021 Data mit 2022 CCyB Rate (da CCyB in 2021 = 0) -> wer wird stärker belastet?

data_2021 <- df %>%
  filter(year == 2021) %>%
  select(bank_name, bank_id,
         rwa_2021              = rwa,
         cet1_ratio_2021       = cet1_ratio,
         cet1_min_pct_2021     = cet1_min_pct,
         cet1_ratio_finma_2021 = cet1_ratio_finma)

ccyb_2022 <- df %>%
  filter(year == 2022) %>%
  select(bank_name, rwa_2022 = rwa, ccyb_rate_pct_2022 = ccyb_rate_pct)

treatment_2021 <- data_2021 %>%
  left_join(ccyb_2022, by = "bank_name") %>%
  mutate(
    ccyb_req_2021      = (ccyb_rate_pct_2022 / 100) * rwa_2021,
    cet1_capital_2021  = (cet1_ratio_finma_2021 / 100) * rwa_2021,
    cet1_min_2021      = (cet1_min_pct_2021 / 100) * rwa_2021,
    excess_cap_2021    = cet1_capital_2021 - cet1_min_2021,
    ccyb_exposure_2021 = ccyb_req_2021 / excess_cap_2021
  ) %>%
  select(bank_name, bank_id, ccyb_req_2021, cet1_capital_2021,
         cet1_min_2021, excess_cap_2021, ccyb_exposure_2021)

summary(treatment_2021$ccyb_exposure_2021)

# nochmal check negative excess capital
treatment_2021 %>% filter(excess_cap_2021 < 0)


# Treatment Variablen ins Panel mergen
df <- df %>%
  left_join(
    treatment_2019 %>% select(bank_id, ccyb_req_2019, excess_cap_2019, ccyb_exposure_2019),
    by = "bank_id"
  ) %>%
  left_join(
    treatment_2021 %>% select(bank_id, ccyb_req_2021, excess_cap_2021, ccyb_exposure_2021),
    by = "bank_id"
  )

dim(df)
n_distinct(df$bank_id)
# View(df)


# save
saveRDS(df, "../processed_data/02_data_with_treatment.rds")
write.xlsx(df, "../processed_data/02_data_with_treatment.xlsx")



#Part 2: analysis of treatment and groups -> descriptives and plots

# summary exposure 2019
summary(treatment_2019$ccyb_exposure_2019)

treatment_2019 %>%
  select(bank_name, ccyb_exposure_2019, ccyb_req_2019, excess_cap_2019) %>%
  arrange(desc(ccyb_exposure_2019))

# summary exposure 2021
summary(treatment_2021$ccyb_exposure_2021)

treatment_2021 %>%
  select(bank_name, ccyb_exposure_2021, ccyb_req_2021, excess_cap_2021) %>%
  arrange(desc(ccyb_exposure_2021))


# plots
dir.create("../figures", showWarnings = FALSE, recursive = TRUE)

# histogram 2019
p1_1 <- ggplot(treatment_2019, aes(x = ccyb_exposure_2019)) +
  geom_histogram(bins = 25, fill = "#2c7bb6", color = "white", alpha = 0.8) +
  geom_vline(aes(xintercept = median(ccyb_exposure_2019, na.rm=T)),
             color = "#2c7bb6", linetype = "dashed", size = 1.2) +
  geom_vline(aes(xintercept = quantile(ccyb_exposure_2019, 0.25, na.rm=T)),
             color = "orange", linetype = "dotted", size = 1) +
  geom_vline(aes(xintercept = quantile(ccyb_exposure_2019, 0.75, na.rm=T)),
             color = "orange", linetype = "dotted", size = 1) +
  labs(x = "CCyB Exposure (CCyB Required / Excess Capital)", y = "Number of Banks") +
  theme_minimal(base_size = 14)

print(p1_1)
ggsave("../figures/treatment_distribution_2019.png", p1_1, width = 10, height = 6, dpi = 300)

# histogram 2021
p2_1 <- ggplot(treatment_2021, aes(x = ccyb_exposure_2021)) +
  geom_histogram(bins = 25, fill = "#2c7bb6", color = "white", alpha = 0.8) +
  geom_vline(aes(xintercept = median(ccyb_exposure_2021, na.rm=T)),
             color = "#2c7bb6", linetype = "dashed", size = 1.2) +
  geom_vline(aes(xintercept = quantile(ccyb_exposure_2021, 0.25, na.rm=T)),
             color = "orange", linetype = "dotted", size = 1) +
  geom_vline(aes(xintercept = quantile(ccyb_exposure_2021, 0.75, na.rm=T)),
             color = "orange", linetype = "dotted", size = 1) +
  labs(x = "CCyB Exposure (CCyB Required / Excess Capital)", y = "Number of Banks") +
  theme_minimal(base_size = 14)

print(p2_1)
ggsave("../figures/treatment_distribution_2021.png", p2_1, width = 10, height = 6, dpi = 300)


# Scatterplot 2019 vs 2021 nach Bankkategorie

# Farben
cat_colors <- c("1" = "#1A5276", "2" = "#2E86C1", "3" = "#85C1E9", "4" = "#A9A9A9")
cat_labels <- c("1" = "Category 1 (G-SIBs)", "2" = "Category 2",
                "3" = "Category 3", "4" = "Category 4")

# Abkürzungen für Labels
bank_labels <- c(
  "Aargauische Kantonalbank"              = "Aargau KB",
  "Banca Dello Stato Del Cantone Ticino"  = "BancaStato",
  "Bank Cler AG"                          = "Bank Cler",
  "Banque Cantonale De Geneve"            = "Geneva KB",
  "Banque Cantonale Neuchateloise"        = "Neuchatel KB",
  "Banque Cantonale Vaudoise"             = "Vaud KB",
  "Banque Cantonale De Fribourg"          = "Fribourg KB",
  "Banque Cantonale Du Valais"            = "Valais KB",
  "Basellandschaftliche Kantonalbank"     = "Baselland KB",
  "Basler Kantonalbank"                   = "Basel KB",
  "Berner Kantonalbank AG"                = "Bern KB",
  "Credit Agricole next bank (Suisse) SA" = "CA Next Bank",
  "Credit Suisse AG"                      = "CS",
  "Graubundner Kantonalbank"              = "Graubunden KB",
  "LLB (Schweiz) AG"                      = "LLB",
  "Luzerner Kantonalbank AG"              = "Lucerne KB",
  "Migros bank AG"                        = "Migros Bank",
  "Raiffeisen Group"                      = "Raiffeisen",
  "Schaffhauser Kantonalbank"             = "Schaffhausen KB",
  "Schwyzer Kantonalbank"                 = "Schwyz KB",
  "St.Galler Kantonalbank AG"             = "St. Gallen KB",
  "Thurgauer Kantonalbank"                = "Thurgau KB",
  "UBS Switzerland AG"                    = "UBS",
  "Valiant Bank AG"                       = "Valiant Bank",
  "Zuercher Kantonalbank"                 = "Zurich KB"
)

exposure_comparison <- treatment_2019 %>%
  select(bank_name, ccyb_exposure_2019) %>%
  left_join(treatment_2021 %>% select(bank_name, ccyb_exposure_2021), by = "bank_name") %>%
  left_join(df %>% select(bank_name, kategorie) %>% distinct(), by = "bank_name") %>%
  mutate(
    kategorie  = as.factor(kategorie),
    bank_label = recode(bank_name, !!!bank_labels)
  )

p3_1 <- ggplot(exposure_comparison,
               aes(x = ccyb_exposure_2019, y = ccyb_exposure_2021, color = kategorie)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "gray50", linewidth = 0.8) +
  geom_point(size = 3.5, alpha = 0.85) +
  geom_text(aes(label = bank_label), hjust = -0.2, vjust = 0.5,
            size = 2.5, check_overlap = TRUE, color = "black") +
  scale_color_manual(values = cat_colors, labels = cat_labels, name = "Bank Category") +
  scale_x_continuous(labels = scales::label_number(accuracy = 0.01)) +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) +
  labs(x = "CCyB Exposure end-2019", y = "CCyB Exposure end-2021") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "bottom",
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.minor = element_blank()
  ) +
  guides(color = guide_legend(nrow = 1, title.position = "top"))

print(p3_1)
ggsave("../figures/treatment_comparison_scatter.png", p3_1, width = 10, height = 8, dpi = 300)

# korrelation zwischen den beiden Exposures
cor.test(exposure_comparison$ccyb_exposure_2019, exposure_comparison$ccyb_exposure_2021)


# dotplot ranking 2019
p4_1 <- treatment_2019 %>%
  arrange(ccyb_exposure_2019) %>%
  mutate(bank_name = factor(bank_name, levels = bank_name)) %>%
  ggplot(aes(x = ccyb_exposure_2019, y = bank_name)) +
  geom_point(size = 4, color = "#2c7bb6", alpha = 0.7) +
  geom_vline(xintercept = median(treatment_2019$ccyb_exposure_2019, na.rm=T),
             linetype = "dashed", color = "#2c7bb6", size = 1) +
  labs(x = "CCyB Exposure", y = NULL) +
  theme_minimal(base_size = 12) +
  theme(axis.text.y = element_text(size = 9))

print(p4_1)
ggsave("../figures/treatment_ranking_2019.png", p4_1, width = 10, height = 12, dpi = 300)

# dotplot ranking 2021
p5_1 <- treatment_2021 %>%
  arrange(ccyb_exposure_2021) %>%
  mutate(bank_name = factor(bank_name, levels = bank_name)) %>%
  ggplot(aes(x = ccyb_exposure_2021, y = bank_name)) +
  geom_point(size = 4, color = "#2c7bb6", alpha = 0.7) +
  geom_vline(xintercept = median(treatment_2021$ccyb_exposure_2021, na.rm=T),
             linetype = "dashed", color = "#2c7bb6", size = 1) +
  labs(x = "CCyB Exposure", y = NULL) +
  theme_minimal(base_size = 12) +
  theme(axis.text.y = element_text(size = 9))

print(p5_1)
ggsave("../figures/treatment_ranking_2021.png", p5_1, width = 10, height = 12, dpi = 300)


# percentiles -> wo mache ich cut?
quantile(treatment_2019$ccyb_exposure_2019, probs = seq(0, 1, 0.1), na.rm = TRUE)
quantile(treatment_2021$ccyb_exposure_2021, probs = seq(0, 1, 0.1), na.rm = TRUE)

# median split (für DiD)
median(treatment_2019$ccyb_exposure_2019, na.rm=T)
median(treatment_2021$ccyb_exposure_2021, na.rm=T)


# save tables
dir.create("../tables", showWarnings = FALSE, recursive = TRUE)

write.csv(exposure_comparison %>% select(bank_name, ccyb_exposure_2019, ccyb_exposure_2021),
          "../tables/treatment_exposure_comparison.csv", row.names = FALSE)
write.csv(treatment_2019 %>% select(bank_name, ccyb_exposure_2019, ccyb_req_2019, excess_cap_2019) %>%
            arrange(desc(ccyb_exposure_2019)),
          "../tables/treatment_exposure_2019.csv", row.names = FALSE)
write.csv(treatment_2021 %>% select(bank_name, ccyb_exposure_2021, ccyb_req_2021, excess_cap_2021) %>%
            arrange(desc(ccyb_exposure_2021)),
          "../tables/treatment_exposure_2021.csv", row.names = FALSE)