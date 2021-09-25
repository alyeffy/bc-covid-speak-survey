#' Transposing Survey Data
#'
#' @param raw_survey_data 
#'
#' @return
#' @export
#'
#' @examples
#' Transposing Survey data table
#' Transposing the survey data to enable filtering the features 
#' substituting values from the dictionary
transpose_survey_data <- function(raw_survey_data){

  transposed_survey_data_tbl <-
    raw_survey_data %>%
  mutate(across(everything(), as.character)) %>% 
  select(responseid,
         everything()) %>% 
  pivot_longer(cols = 2:ncol(raw_survey_data),
               names_to = "key") %>% 
  pivot_wider(
    names_from = responseid,
    values_from = value)

  }


#' Joining transposed survey data with the data dictionary
#'
#' @param transposed_survey_data 
#'
#' @return
#' @export
#'
#' @examples
join_dictionary_with_survey_data <- function(raw_survey_data,
                                             data_dictionary_file) {
  # Left Join data dictionary to transposed survey table  ----
  ## Join the data dictionary to obtain column description and code list 
  ## Mutate the survey data to character to enable subsequent string
  ## replacement

  transposed_survey_data_tbl <- transpose_survey_data(raw_survey_data)

  data_dictionary_tbl <- read_data_dictionary(data_dictionary_file)

  joined_data_dict_transposed_tbl <-
    transposed_survey_data_tbl %>%
    left_join(data_dictionary_tbl, by = c("key" = "column_name")) %>%
    select(key, column_description, code_list, everything()) %>%
    mutate(code_list = str_c(code_list, ";")) %>% 
    mutate(across(starts_with("R_"), ~ as.character(.x)))

}


#' Parse data dictionary with survey data
#'
#' @param joined_data_dict_transposed_tbl 
#'
#' @return
#' @export
#'
#' @examples
parse_survey_data_dictionary <- function(joined_data_dict_transposed_tbl){
  replaced_transposed_tbl <- joined_data_dict_transposed_tbl %>%
  rowwise() %>%
  ## mutate(test = str_match(code_list, paste0(`R_004WGVdGVpCrWN3`, '=([a=zA-z]*);'))[,2])
  mutate(across(starts_with("R_"), ~ str_match(code_list, paste0(.x, '=([a-zA-Z]*);'))[,2]))
}
