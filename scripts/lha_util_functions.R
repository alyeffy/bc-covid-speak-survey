# Load Local Health Area (LHA) lookup dictonary
library(readxl)
data_dict <- read_excel("C:/Users/Whokin/Downloads/Data_dictionary_with_description_20201105_CORRECTED.xlsx", 
                        sheet = "CHSA_LHA_HSDA_HA_lookup")

geo_lha_filter <- function(survey_data, data_dict, LHA_Name_filter) {
    # create unique key value pairs between string name and ID
    LHA_dict <- data_dict %>% select("LHA", "LHA_Name") %>% unique()
    
    # Rename to join on same column
    LHA_dict <- LHA_dict %>% rename( "lha" = "LHA")
    
    # left join to map ID to the area name
    df_joined <- left_join(survey_data, LHA_dict, by = "lha")
    
    # Filter entries within the area
    df <- df_joined %>% filter(LHA_Name == LHA_Name_filter)
    
    # Return results
    df
}

# Usage
geo_lha_filter(sample_r2, data_dict, "Western Communities")
# Check
geo_lha_filter(sample_r2, data_dict, "Western Communities") %>% select("LHA_Name", "lha")


# Utility function that takes a Local Health Area name and returns the ID
lha_id2str <- function(lha_id){
    # create unique key value pairs between string name and ID
    LHA_dict <- data_dict %>% select("LHA", "LHA_Name") %>% unique()
    LHA_dict %>% filter(LHA == lha_id) %>% select("LHA", "LHA_Name")
}

# Usage
lha_id2str(413)
