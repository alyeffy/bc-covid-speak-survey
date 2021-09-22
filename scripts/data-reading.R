library(dplyr)
library(stringr)
library(purrr)
library(openxlsx)
library(here)
library(tidyr)

# Setting some paths for convenience  ----
## raw data and wrangled data folder locations -----
raw_data_loc <- here("data", "00_raw_data")
wrangled_data_loc <- here("data", "01_wrangled_data")
scripts_loc <- here("scripts")

## Data file locations for convenience
data_dictionary_file <- here(raw_data_loc, "Data_dictionary_with_description_20201105_CORRECTED.xlsx")
raw_survey_data_file <- here(raw_data_loc, "SPEAKI_Survey_Data_deidentified.xlsx")


# Read in the data Dictionaries ----
## Read in the CHSA and LHA Sheet ----
chsa_lha_raw_tbl <- read.xlsx(data_dictionary_file, 
                              sheet = "CHSA_LHA_HSDA_HA_lookup") %>%
  as_tibble()


chsa_tbl <- chsa_lha_raw_tbl %>%
  select(CHSA, CHSA_Name) %>%
  unique()

lha_tbl <- chsa_lha_raw_tbl %>%
  select(LHA, LHA_Name) %>%
  unique()

ha_tbl <- chsa_lha_raw_tbl %>%
  select(HA, HA_Name) %>%
  unique()

hsda_tbl <- chsa_lha_raw_tbl %>%
  select(HSDA, HSDA_Name) %>%
  unique()


chsa_ur_class_tbl <- chsa_lha_raw_tbl %>%
  select(CHSA_UR_Class) %>%
  unique() %>%
  mutate(CHSA_UR_Class = as.factor(CHSA_UR_Class))


## Note : it appears that each country, province, municipality has a
## unique code. i.e they do not share codes. Therefore unique() is
## applied anyway to be safe.
## TODO setup testing for uniques? Does not seem critical at the moment. 

## Read in the Municipality ----
municipality_tbl <- read.xlsx(data_dictionary_file, 
                              sheet = "municipality_lookup") %>%
  as_tibble() %>%
  unique()


## Read in the Province look up sheet  ----
province_tbl <- read.xlsx(data_dictionary_file, 
                          sheet = "province_lookup") %>%
  as_tibble() %>%
  unique()


## Read in the country look up sheet ----
country_tbl <- read.xlsx(data_dictionary_file, 
                         sheet = "country_lookup") %>%
  as_tibble() %>%
  unique()

## Read in US State look up ----
us_state_tbl <- read.xlsx(data_dictionary_file, 
                          sheet = "US_state_lookup") %>%
  as_tibble() %>%
  unique()

## Data dictionary read in. ----
## This needs to be processed further
data_dictionary_raw_tbl <- read.xlsx(data_dictionary_file, 
                          sheet = "Data_dictionary") %>%
  as_tibble() %>%
  select(Column.Name, Column.Description, Code.List) %>%
  janitor::clean_names()

  
## Reading in a sample of the survey data for testing
## 100 rows and all the columns 
raw_survey_data <- read.xlsx(raw_survey_data_file,
                             rows = c(1:100)) %>%
  as_tibble()


# Transposing the survey data to enable filtering the features and
# substituting values from the dictionary

raw_survey_data %>%
select(responseid, no_change_behaviour, personal_hygiene) %>%
pivot_longer(cols = c('no_change_behaviour', 'personal_hygiene'),
             names_to = "key") %>% 
pivot_wider(
  names_from = responseid,
  values_from = value)

## TEsting the replacement of values from the dictionaries
## TODO Create a consolidated data dictionary for looking up values?    
## TODO Do we need to remove spurious values from the dictionary ?
