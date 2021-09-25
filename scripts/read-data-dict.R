#' Read in data dictionary sheet 
#'
#' @param data_dictionary_file 
#' @param sheet_name 
#'
#' @return tibble 
#' @export
#'
#' @examples
#' data_dict_tbl <- read_data_dictionary_sheet("./data/abcd.xlsx")
read_data_dictionary <- function(data_dictionary_file,
                                      sheet_name = "Data_dictionary") {

  data_dictionary_processed_tbl <- read.xlsx(data_dictionary_file, 
                                             sheet = sheet_name,
                                             fillMergedCells = T) %>%
    as_tibble() %>%
    select(Column.Name, Column.Description, Code.List) %>%
    janitor::clean_names() %>%
    mutate(column_name = str_to_lower(column_name))
          
}


#' Read chsa info from data dictionary
#'
#' @param data_dictionary_file 
#' @param sheet_name 
#'
#' @return
#' @export
#'
#' @examples
read_chsa_data_dict <- function(data_dictionary_file,
                                sheet_name ="CHSA_LHA_HSDA_HA_lookup") {

  chsa_lha_raw_tbl <- read.xlsx(data_dictionary_file, 
                                sheet = sheet_name) %>%
    as_tibble()

  chsa_tbl <- chsa_lha_raw_tbl %>%
    select(CHSA, CHSA_Name) %>%
    unique()

}



#' Read LHA information from data dictionary
#'
#' @param data_dictionary_file 
#' @param sheet_name 
#'
#' @return
#' @export
#'
#' @examples
read_lha_data_dict <- function(data_dictionary_file,
                                sheet_name ="CHSA_LHA_HSDA_HA_lookup") {

  chsa_lha_raw_tbl <- read.xlsx(data_dictionary_file, 
                                sheet = sheet_name) %>%
    as_tibble()
                               

  lha_tbl <- chsa_lha_raw_tbl %>%
    select(LHA, LHA_Name) %>%
    unique()
}


#' Read HA information from data dictionary
#'
#' @param data_dictionary_file 
#' @param sheet_name 
#'
#' @return
#' @export
#'
#' @examples
read_ha_data_dict <- function(data_dictionary_file,
                                sheet_name ="CHSA_LHA_HSDA_HA_lookup") {

  chsa_lha_raw_tbl <- read.xlsx(data_dictionary_file, 
                                sheet = sheet_name) %>%
    as_tibble()
  
  ha_tbl <- chsa_lha_raw_tbl %>%
    select(HA, HA_Name) %>%
    unique()

}


#' Read HSDA information from data dictionary
#'
#' @param data_dictionary_file 
#' @param sheet_name 
#'
#' @return tibble
#' @export
#'
#' @examples
read_hsda_data_dict <- function(data_dictionary_file,
                                sheet_name ="CHSA_LHA_HSDA_HA_lookup") {

  chsa_lha_raw_tbl <- read.xlsx(data_dictionary_file, 
                                sheet = sheet_name) %>%
    as_tibble()
  
  hsda_tbl <- chsa_lha_raw_tbl %>%
    select(HSDA, HSDA_Name) %>%
    unique()
}


#' Read CHSA UR Class information from data dictionary
#'
#' @param data_dictionary_file 
#' @param sheet_name 
#'
#' @return tibble
#' @export
#'
#' @examples
read_chsa_ur_class_data_dict <- function(data_dictionary_file,
                                sheet_name ="CHSA_LHA_HSDA_HA_lookup") {

  chsa_lha_raw_tbl <- read.xlsx(data_dictionary_file, 
                                sheet = sheet_name) %>%
    as_tibble()
  
  chsa_ur_class_tbl <- chsa_lha_raw_tbl %>%
    select(CHSA_UR_Class) %>%
    unique() %>%
    mutate(CHSA_UR_Class = as.factor(CHSA_UR_Class))

  }
