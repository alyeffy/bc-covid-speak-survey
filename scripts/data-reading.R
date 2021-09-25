library(dplyr)
library(stringr)
library(purrr)
library(openxlsx)
library(here)
library(tidyr)

# Setting some paths for convenience  ----
## raw data and wrangled data folder locations -----
raw_data_loc <- here("data")
wrangled_data_loc <- here("data", "01_wrangled_data")
scripts_loc <- here("scripts")


## Data file locations for convenience -----
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

## This needs to be processed further We need to use the filled option
## to unmerge and repeat the content in column.descriptions We also need
## to make all the column_names lower case to match the header columns
## in raw survey data

data_dictionary_raw_tbl <- read.xlsx(data_dictionary_file, 
                                     sheet = "Data_dictionary",
                                     fillMergedCells = T) %>%
  as_tibble() %>%
  select(Column.Name, Column.Description, Code.List) %>%
  janitor::clean_names() %>%
  mutate(column_name = str_to_lower(column_name))

  
## Reading in a sample of the survey data for testing -----
## 100 rows and all the columns 
raw_survey_data <- read.xlsx(raw_survey_data_file,
                             rows = c(1:100)) %>%
  as_tibble()


# Transposing Survey data table -----
## Transposing the survey data to enable filtering the features 
## substituting values from the dictionary
       

transposed_survey_data_tbl <- raw_survey_data %>%
  select(responseid,
         no_change_behaviour,
         hand_wash_regularly,
         practice_phys_distancing,
         stay_home_sick,          
         avoid_gathering,         
         personal_hygiene,        
         working_home2) %>% 
  pivot_longer(cols = no_change_behaviour:working_home2,
               names_to = "key") %>% 
  pivot_wider(
    names_from = responseid,
    values_from = value)

# Left JOin data dictionary to transposed survey table  ----
## Join the data dictionary to obtain column description and code list 
## Mutate the survey data to character to enable subsequent string
## replacement

joined_data_dict_transposed_tbl <-
  transposed_survey_data_tbl %>%
  left_join(data_dictionary_raw_tbl, by = c("key" = "column_name")) %>%
  select(key, column_description, code_list, everything()) %>%
  mutate(code_list = str_c(code_list, ";")) %>% 
  mutate(across(starts_with("R_"), ~ as.character(.x)))


########################################################## NEEED HELP !!

## Replacing survey data table with data dictionary values 
## TODO fix this 


## Is code list not being obtained properly? Do we need purrr::map ?

replaced_transposed_tbl <- joined_data_dict_transposed_tbl %>%
  mutate(across(starts_with("R_"), ~ str_match(code_list,
                                                 paste0("(", cur_column(), "=[a-zA-z]*;)"))[,2]))



######################################### testing with simplified table ------
## This works as a basic example
## Sample string matching 

sample <- joined_data_dict_transposed_tbl %>%
  select(code_list) %>%
  mutate(test0 = as.character(0),
         test1 = as.character(1)) %>%
  mutate(test_parsed_0 = str_match(code_list, paste0(test0, "=([a-zA-Z]*);"))[,2]) %>% 
  mutate(test_parsed_1 = str_match(code_list, paste0(test1, "=([a-zA-Z]*);"))[,2])




