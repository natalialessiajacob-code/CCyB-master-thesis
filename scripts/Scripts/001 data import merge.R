# Script 1: Data Import and Merge
# Data files: Fitch, FINMA, Makro und Kategorien

rm(list = ls())
library(tidyverse)
library(openxlsx)

setwd("/Users/natalialessia/Desktop/Masterarbeit/CCYB Analyse/MasterThesis_CCyB_Switzerland/Data/Data Raw/imported_data/")

# Fitch

fitch_raw <- read.xlsx("Bank_paneldata_allyears_raw_newversionfinal.xlsx")

dim(fitch_raw)
# View(fitch_raw)

fitch <- fitch_raw %>%
  mutate(across(where(is.character), ~ ifelse(.x == "-", NA, .x))) %>%
  rename(
    residential_mortgages  = Residential_Mortgage_Loans,
    other_mortgages        = Other_Mortgage_Loans,
    operating_roaa         = Operating_ROAA,
    roaa                   = ROAA,
    cet1_ratio             = CET1_Ratio,
    cet1_capital           = CET1_Capital,
    tier1_ratio            = Tier1_Ratio,
    tier1_capital          = Tier1_Capital,
    npl_ratio              = NPL_Ratio,
    loan_loss_allowances   = Loan_Loss_Allowances,
    loan_growth            = Loan_Growth,
    loans_deposits_ratio   = Loans_Deposits_Ratio,
    liquid_assets_ratio    = Liquid_Assets_Ratio,
    roae                   = ROAE,
    total_equity           = Total_Equity,
    total_assets           = Total_Assets,
    gross_loans            = Gross_Loans,
    customer_deposits      = Customer_Deposits_ST_Funding,
    leverage_ratio         = Basel_III_Leverage_Ratio,
    net_income             = Net_Income,
    net_interest_income    = Net_Interest_Income
  )

# RWA kommt von FINMA!! nicht von Fitch

 
# FINMA

finma_raw <- read.xlsx("finma_keymetrics.xlsx")
# View(finma_raw)

finma <- finma_raw %>%
  mutate(across(where(is.character), ~ ifelse(.x == "-", NA, .x))) %>%
  rename(
    ccyb_rate_pct    = `CCyB_required%`,
    cet1_min_pct     = `CET1_minima%`,
    rwa              = `RWA_CHF`,
    cet1_ratio_finma = `CET1_%`
  )


# Makrodaten

macro_raw <- read.xlsx("macro_control_variables.xlsx")
print(macro_raw) # klein genug um alles zu sehen

macro <- macro_raw %>%
  mutate(across(where(is.character), ~ ifelse(.x == "-", NA, .x))) %>%
  rename(
    ccyb_ch               = CCYB_CH,
    property_price_change = res_property_price_index_CH_change,
    rent_price_change     = rent_price_index_CH_change,
    credit_gdp_change     = credit_gdp_ratio_CH_change,
    gdp_growth            = GDP_CH_change,
    unemployment_change   = Unemployment_rate_CH_change
  )


# Bankenkategorien
bank_category <- read.xlsx("Kategorie_banken.xlsx")


# Merge all

df <- fitch

# FINMA dazu (RWA von hier!)
df <- df %>%
  left_join(
    finma %>% select(bank_name, year, ccyb_rate_pct, cet1_min_pct, rwa, cet1_ratio_finma),
    by = c("bank_name", "year")
  )

df <- df %>% left_join(bank_category, by = "bank_name")
df <- df %>% left_join(macro, by = "year")

#check
nrow(df)
View(df)


# exklude banks bec of data availability  
# Clientis and Glarner: keine FINMA Daten 2019
# Zuger KB: zu viele fehlende Fitch-Werte
banks_to_exclude <- c("Clientis AG", "Glarner Kantonalbank", "Zuger Kantonalbank")
df <- df %>% filter(!bank_name %in% banks_to_exclude)

nrow(df)
n_distinct(df$bank_name) # sollte 23 sein

# Typen bereinigen
df <- df %>%
  mutate(across(where(is.character), ~ ifelse(.x == "-", NA, .x))) %>%
  mutate(across(-c(bank_name, bank_id), as.numeric))


# checks

options(scipen = 6, digits = 2)

# NAs in den wichtigsten Variablen
colSums(is.na(df[, c("residential_mortgages", "cet1_capital", "rwa",
                     "cet1_ratio", "ccyb_rate_pct", "cet1_min_pct")]))

# panel balance
df %>%
  group_by(bank_name) %>%
  summarise(n = n()) %>%
  arrange(n) %>%
  print(n = Inf)

# CCyB plausibel?
summary(df$ccyb_rate_pct)

# check year
df %>% group_by(year) %>% summarise(mean_ccyb = mean(ccyb_rate_pct, na.rm=T))

# TODO: nochmal schauen ob cet1_ratio von Fitch und FINMA stark abweichen
# df %>% mutate(diff = cet1_ratio - cet1_ratio_finma) %>% summary()


# Save

dir.create("../processed_data", showWarnings = FALSE, recursive = TRUE)

saveRDS(df, "../processed_data/01_data_merged.rds")
write.xlsx(df, "../processed_data/01_data_merged.xlsx")