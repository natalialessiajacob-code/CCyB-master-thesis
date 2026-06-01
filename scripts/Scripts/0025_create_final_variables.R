# Script 2.5: Create Final Variables
# Daten für regressionen vorbereiten

library(tidyverse)

setwd("/Users/natalialessia/Desktop/Masterarbeit/CCYB Analyse/MasterThesis_CCyB_Switzerland/Data/Data Raw/imported_data/")

# Load data with raw treatment exposure
df <- readRDS("../processed_data/02_data_with_treatment.rds")

# Part 1: Treatment variables

# Calculate medians
treatment_2019 <- df %>% filter(year == 2019)
treatment_2021 <- df %>% filter(year == 2021)

median_2019 <- median(treatment_2019$ccyb_exposure_2019, na.rm = TRUE)
median_2021 <- median(treatment_2021$ccyb_exposure_2021, na.rm = TRUE)

# Standardization parameters
stats_2019 <- treatment_2019 %>%
  summarise(
    mean_2019 = mean(ccyb_exposure_2019, na.rm = TRUE),
    sd_2019   = sd(ccyb_exposure_2019, na.rm = TRUE)
  )

stats_2021 <- treatment_2021 %>%
  summarise(
    mean_2021 = mean(ccyb_exposure_2021, na.rm = TRUE),
    sd_2021   = sd(ccyb_exposure_2021, na.rm = TRUE)
  )

# Create treatment variables
df <- df %>% 
  mutate(
    # median split
    high_exposure_2019 = if_else(ccyb_exposure_2019 > median_2019, 1L, 0L),
    high_exposure_2021 = if_else(ccyb_exposure_2021 > median_2021, 1L, 0L),
    
    # Sstandardisiert
    ccyb_exposure_2019_std = (ccyb_exposure_2019 - stats_2019$mean_2019) / stats_2019$sd_2019,
    ccyb_exposure_2021_std = (ccyb_exposure_2021 - stats_2021$mean_2021) / stats_2021$sd_2021,
    
    # Time variables
    post_2020 = if_else(year >= 2020, 1L, 0L),
    post_2022 = if_else(year >= 2022, 1L, 0L),
    rel_year_2020 = year - 2020,
    rel_year_2022 = year - 2022
  )

# Part 2: Outcome variables

df <- df %>%
  group_by(bank_id) %>%
  arrange(bank_id, year) %>%
  mutate(
    # Tot mortgage growth (residential + other)
    total_mortgages = residential_mortgages + other_mortgages,
    mortgage_growth_yoy = (total_mortgages / lag(total_mortgages) - 1) * 100,
    
    # Residential mortgage growth 
    residential_mortgage_growth_yoy = (residential_mortgages / lag(residential_mortgages) - 1) * 100,
    
    # Other mortgage growth
    other_mortgage_growth_yoy = (other_mortgages / lag(other_mortgages) - 1) * 100,
    
    # tot loans growth (spillover)
    total_loan_growth_yoy = (gross_loans / lag(gross_loans) - 1) * 100,
    
    # Non-mortgage loan growth (spillover)
    non_mortgage_loans = gross_loans - total_mortgages,
    non_mortgage_growth_yoy = (non_mortgage_loans / lag(non_mortgage_loans) - 1) * 100,
    
    # Non-residential mortgage loan growth (spillover)
    non_residential_mortgage_loans = gross_loans - residential_mortgages,
    non_residential_mortgage_growth_yoy = (non_residential_mortgage_loans / lag(non_residential_mortgage_loans) - 1) * 100,

    # delta_ln_residential = log(residential_mortgages) - log(lag(residential_mortgages)),
    # delta_ln_total_mortgages = log(total_mortgages) - log(lag(total_mortgages)),
    # delta_ln_gross_loans = log(gross_loans) - log(lag(gross_loans)),
    
    # Residential share
    residential_share_of_mortgages = (residential_mortgages / total_mortgages) * 100,
    residential_share_of_loans = (residential_mortgages / gross_loans) * 100,
    residential_share_of_assets = (residential_mortgages / total_assets) * 100,
    
    # Other mortgage share
    other_mortgage_share_of_mortgages = (other_mortgages / total_mortgages) * 100,
    other_mortgage_share_of_loans = (other_mortgages / gross_loans) * 100,
    
    # tot mortgage share
    mortgage_share_of_loans = (total_mortgages / gross_loans) * 100,
    mortgage_share_of_assets = (total_mortgages / total_assets) * 100,
    
    # NPL ratio
    npl_ratio_pct = npl_ratio,  
    
    #  Loan loss provisions
    # loan_loss_ratio = (loan_loss_allowances / gross_loans) * 100,
    
    # Cet1 und tier1
    cet1_ratio_pct = cet1_ratio, 
    tier1_ratio_pct = tier1_ratio,
    
    # Leverage
    leverage_ratio_pct = leverage_ratio,
    
    # Roa
    roaa_pct = roaa, 
    operating_roaa_pct = operating_roaa,
    
    # Roe
    roae_pct = roae,
    
    # Net interest income 
    net_interest_income_growth = (net_interest_income / lag(net_interest_income) - 1) * 100,
    
    # Net income 
    net_income_growth = (net_income / lag(net_income) - 1) * 100,
    
    # Asset growth
    # asset_growth_yoy = (total_assets / lag(total_assets) - 1) * 100,
    
    # Deposit growth
    # deposit_growth_yoy = (customer_deposits / lag(customer_deposits) - 1) * 100,
    
    # Equity growth
    # equity_growth_yoy = (total_equity / lag(total_equity) - 1) * 100,
    
    # Loan-to-deposit ratio 
    # loans_deposits_ratio_pct = loans_deposits_ratio,
    
    # Liquid assets ratio 
    # liquid_assets_ratio_pct = liquid_assets_ratio
    
  ) %>%
  ungroup()

# Save final dataset
saveRDS(df, "../processed_data/04_data_final_for_regression.rds")
