## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Project:           EU-S A2J Codebook
##
## Script:            Codebook in Python (Jupyter Notebook)
##
## Author(s):         A. Santiago Pardo G.        (spardo@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
# 1. Required Packages                                                                  
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Required packages
library(pacman)

p_load(char = c(
  
  # Data Loading
  "haven", "readxl",
  
  # Good 'ol Tidyverse
  "tidyverse"
  
)
)

avg_estimation <- function(df, var_estimate) {
  
  estimation <- df %>%
    group_by(country_name_ltn, nuts_id) %>%
    summarize(
      pop_weight = mean(pop_weight, na.rm = TRUE),
      prevalence = mean(.data[[var_estimate]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      prevalence = prevalence * pop_weight
    ) %>%
    group_by(country_name_ltn) %>%
    summarise(
      prevalence2 = sum(prevalence, na.rm = TRUE),
      .groups = "drop"
    )
  
  return(estimation)
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2.  SharePoint Path                                                                                      ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# SharePoint path
if (Sys.info()["user"] == "santiagopardo") {
  
  path2EU <- paste0("/Users/santiagopardo/OneDrive - World Justice Project/EU Subnational/EU-S Data/")
  
}else {
  
  path2SP <- "PLEASE INSERT YOUR PERSONAL PATH TO THE  WJP - DATA ANALYTICS DIRECTORY"
  
}

# Path to data

path2GPP     <- paste0(path2EU, "/eu-gpp/1. Data/3. Merge/","EU_GPP_2024.dta")
path2weights <- paste0(path2EU, "reports/eu-thematic-reports/data-viz/inputs/region_labels.xlsx")

# Loading data

master_data.df <- read_dta(path2GPP)
regions.df     <- read_excel(path2weights)

# Merge data with the weights

master_data.df <- master_data.df %>%
  left_join(regions.df %>%
              rename(country_name_ltn = country,
                     pop_weight = regionpoppct), 
            by = c("country_name_ltn", "nuts_id"))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3.  Legal Needs Survey                                                                                   ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
### Problem Prevalence                                                                                  ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Define a vector of legal problem categories
legal_problems <- c(
  "A1", "A2", "A3", 
  "B1", "B2", "B3", "B4", 
  "C1", "C2", "C3", "C4", 
  "D1", "D2", "D3", "D4", "D5", "D6", 
  "E1", "E2", "E3", 
  "F1", "F2", 
  "G1", "G2", "G3", 
  "H1", "H2", "H3", 
  "I1", 
  "J1", "J2", "J3", "J4", 
  "K1", "K2", "K3",
  "L1", "L2"
)

# Generate column names for binary indicators of legal problems
legprob_bin <- paste0("AJP_", legal_problems, "_bin")

# Generate column names for severity indicators of legal problems
legprob_sev <- paste0("AJP_", legal_problems, "_sev")

# Process the dataset `master_data.df` to create `A2J.df`
A2J.df <- master_data.df %>%
  # Convert binary indicators to 1 if they are equal to 1, otherwise 0
  mutate(across(all_of(legprob_bin), ~ as.integer(. == 1))) %>%
  
  # Create a new variable `legprob` indicating whether the person has at least one legal problem
  mutate(
    legprob = as.integer(
      rowSums(
        across(
          all_of(legprob_bin)), na.rm = TRUE) > 0)  # If at least one legal problem exists, set to 1
  ) %>%
  
  # Recode severity indicators: 
  #  - If value is between 4 and 97, set to 1 (severe problem)
  #  - If value is below 4, set to 0 (not severe)
  #  - If value is 98 or higher, set to NA (missing or not applicable)
  mutate(
    across(
      all_of(legprob_sev), 
      ~if_else(. > 3 & . < 98, 1, 
               if_else(. < 4, 0, 
                       NA_real_)
      )
    )
  ) %>%
  
  # Create a summary severity indicator `legprob_sev`:
  #  - If at least one severe legal problem exists, set to 1
  #  - Otherwise, set to 0
  mutate(
    legprob_sev = 
      as.integer(
        rowSums(
          across(
            all_of(legprob_sev)), na.rm = TRUE) > 0
      )
  )

# Apply the `avg_estimation` function to estimate the prevalence of severe legal problems
legprob_sev <- avg_estimation(A2J.df, "legprob_sev")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
### Access to Proper Information and Advice                                                                                ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Step 1: Filter for individuals who reported at least one legal problem (`legprob == 1`) 
# and at least one severe legal problem (`legprob_sev == 1`).

access2info <- A2J.df %>%
  filter(legprob == 1) %>%  # Keep only respondents with at least one legal problem
  filter(legprob_sev == 1) %>%  # Keep only respondents with at least one severe legal problem
  mutate(
    # Step 2: Create the `access2info` variable based on the `AJE_infosource` responses
    access2info = case_when(
      AJE_infosource == 1 ~ 1,  # Respondents were able to access information
      AJE_infosource == 2 ~ 1,  # Respondents were able to access information
      AJE_infosource == 3 ~ 0,  # Respondents were NOT able to access information
      AJE_infosource == 4 ~ 0,  # Respondents were NOT able to access information
      AJE_infosource == 98 ~ 0  # Respondents were NOT able to access information
    )
  )

# Step 3: Apply the `avg_estimation` function to estimate the prevalence of `access2info`
# The function likely calculates a weighted average based on population proportions.
access2info <- avg_estimation(access2info, "access2info")


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
### Access to proper representation                                                                               ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

access2rep <- A2J.df %>%
  filter(legprob == 1, legprob_sev == 1) %>%  # More efficient than using two `filter()` calls
  
  # Step 2: Recode `AJD_noadvice_reason` to replace `99` with `NA`
  mutate(
    AJD_noadvice_reason = case_when(
      AJD_noadvice_reason == 99 ~ NA_real_,  # Assign missing value if response was "99"
      TRUE ~ AJD_noadvice_reason  # Keep all other values unchanged
    )
  ) %>%
  
  # Step 3: Create the `access2rep` variable based on `AJD_inst_advice` and `AJD_noadvice_reason`
  mutate(
    access2rep = case_when(
      # 1. Able to access advice from a formal source
      (AJD_adviser_2 == 1 | AJD_adviser_3 == 1 | AJD_adviser_4 == 1 | 
         AJD_adviser_5 == 1 | AJD_adviser_6 == 1 | AJD_adviser_8 == 1) & 
        (AJD_inst_advice == 1) ~ 1, 
      
      # 2. Did not access advice because problem was not important
      (AJD_noadvice_reason %in% c(1, 3)) & (AJD_inst_advice == 2) ~ 1,
      
      # 3. If friend or family has a legal background, people have access to a good representation
      (AJD_inst_advice == 1 & AJD_adviser_1 == 1 & AJD_expert_adviser == 1) ~ 1,
      
      # 4. Accessed advice from a friend, religious org, or other --> in the justice gap
      (AJD_adviser_1 == 1 | AJD_adviser_7 == 1 | AJD_adviser_9 == 1 | 
         AJD_adviser_98 == 1) & (AJD_inst_advice == 1) ~ 0,
      
      # 5. If reason for not seeking advice falls into specific categories --> in the justice gap
      (AJD_noadvice_reason %in% c(2, 4, 5, 6, 7, 8, 9, 10, 98)) & (AJD_inst_advice == 2) ~ 0,
      
      # 6. If `AJD_inst_advice` is 98 --> in the justice gap
      AJD_inst_advice == 98 ~ 0,
      
      # Default case: Assign `NA_real_` if no condition matches
      TRUE ~ NA_real_
    )
  )

# Step 4: Apply the `avg_estimation` function to estimate the prevalence of `access2rep`
access2rep <- avg_estimation(access2rep, "access2rep")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
### Process Barriers: Timeliness                                                                               ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Step 1: Filter for individuals who reported at least one legal problem (`legprob == 1`) 
# and at least one severe legal problem (`legprob_sev == 1`).
rp_time <- A2J.df %>%
  filter(legprob == 1, legprob_sev == 1) %>%  # More efficient than using two `filter()` calls
  
  # Step 2: Create the `rp_time` variable based on `AJR_solvingtime`
  mutate(
    rp_time = case_when(
      # 1. Problem resolved in one year or less --> not in justice gap
      (AJR_solvingtime >= 0 & AJR_solvingtime < 13) & 
        (AJR_state_noresol %in% c(3, 4) | AJR_state_resol %in% c(3, 4)) ~ 1,
      
      # 2. Problem solved in more than one year --> in the justice gap
      (AJR_solvingtime > 12) & 
        (AJR_state_noresol %in% c(3, 4) | AJR_state_resol %in% c(3, 4)) ~ 0,
      
      # 3. Problem remains unresolved with unknown resolution time --> in the justice gap
      AJR_solvingtime == -8888 ~ 0, 
      
      # 4. Missing or invalid resolution time --> NA
      AJR_solvingtime == -9999 ~ NA_real_,
      
      # 5. Problem still ongoing (not resolved) --> NA
      AJR_state_noresol %in% c(1, 2) | AJR_state_resol %in% c(1, 2) ~ NA_real_,
      
      # Default case: Assign `NA_real_` if no condition matches
      TRUE ~ NA_real_
    )
  )

# Step 3: Apply the `avg_estimation` function to estimate the prevalence of `rp_time`
# The function likely calculates a weighted average based on population proportions.
rp_time <- avg_estimation(rp_time, "rp_time")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
### Process Barriers: Costliness                                                                              ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Step 1: Filter for individuals who reported at least one legal problem (`legprob == 1`) 
# and at least one severe legal problem (`legprob_sev == 1`).

rp_cost <- A2J.df %>%
  filter(legprob == 1, legprob_sev == 1) %>%  # More efficient than using two `filter()` calls
  
  # Step 2: Create the `rp_cost` variable based on cost affordability
  mutate(
    rp_cost = case_when(
      # 1. If you incurred costs, but they were easy to pay 
      (AJR_state_noresol %in% c(3, 4) | AJR_state_resol %in% c(3, 4)) & 
        (AJR_costdiff %in% c(1, 2)) & 
        (AJR_solvingcosts == 1) ~ 1,
      
      # 2. If you incurred costs that were difficult to pay 
      (AJR_state_noresol %in% c(3, 4) | AJR_state_resol %in% c(3, 4)) & 
        (AJR_costdiff %in% c(3, 4, 98)) & 
        (AJR_solvingcosts == 1) ~ 0,
      
      # 3. You did not incur costs 
      (AJR_state_noresol %in% c(3, 4) | AJR_state_resol %in% c(3, 4)) & 
        (AJR_solvingcosts == 2) ~ 1,
      
      # 4. Default case: Assign `NA_real_` if no condition matches
      TRUE ~ NA_real_
    )
  )

# Step 3: Apply the `avg_estimation` function to estimate the prevalence of `rp_cost`
# The function likely calculates a weighted average based on population proportions.
rp_cost <- avg_estimation(rp_cost, "rp_cost")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
### Process Barriers: Fairness                                                                               ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Step 1: Filter for individuals who reported at least one legal problem (`legprob == 1`) 
# and at least one severe legal problem (`legprob_sev == 1`).
rp_fair <- A2J.df %>%
  filter(legprob == 1, legprob_sev == 1) %>%  # More efficient than using two `filter()` calls
  
  # Step 2: Create the `rp_fair` variable based on fairness perception
  mutate(
    rp_fair = case_when(
      # 1. Process was perceived as fair → not in the justice gap
      AJR_fair == 1 ~ 1,
      
      # 2. Process was perceived as unfair → in the justice gap
      AJR_fair == 2 ~ 0,
      
      # 3. Respondent indicated "Don’t know" (coded as 98) → in the justice gap
      AJR_fair == 98 ~ 0,
      
      # 4. Default case: Assign `NA_real_` if no condition matches
      TRUE ~ NA_real_
    )
  )

# Step 3: Apply the `avg_estimation` function to estimate the prevalence of `rp_fair`
# The function likely calculates a weighted average based on population proportions.
rp_fair <- avg_estimation(rp_fair, "rp_fair")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
### Status of Legal Problem: Outcomes                                                                               ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Step 1: Filter for individuals who reported at least one legal problem (`legprob == 1`) 
# and at least one severe legal problem (`legprob_sev == 1`).
rp_outcome <- A2J.df %>%
  filter(legprob == 1, legprob_sev == 1) %>% 
  
  # Step 2: Create the `rp_outcome` variable based on problem resolution status
  mutate(
    rp_outcome = case_when(
      # 1. Problem fully resolved
      AJR_state_noresol == 4 | AJR_state_resol == 4 ~ 1,
      
      # 2. Problem persists (not fully resolved)
      AJR_state_noresol == 3 | AJR_state_resol == 3 ~ 0,
      
      # 3. Problem ongoing or uncertain resolution
      AJR_state_noresol %in% c(1, 2) | AJR_state_resol %in% c(1, 2) ~ NA_real_,
      
      # 4. Default case: Assign `NA_real_` if no condition matches
      TRUE ~ NA_real_ 
    )
  )

# Step 3: Apply the `avg_estimation` function to estimate the prevalence of `rp_outcome`
# The function likely calculates a weighted average based on population proportions.
rp_outcome <- avg_estimation(rp_outcome, "rp_outcome")