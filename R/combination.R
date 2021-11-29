#' @rdname diagnose_sparese.data.frame
#' @export
diagnose_sparese <- function(.data, ...) {
  UseMethod("diagnose_sparese", .data)
}

#' Diagnosis of level combinations of categorical variables
#'
#' @description The diagnose_sparese() checks for combinations of levels that 
#' do not appear as data among all combinations of levels of categorical variables. 
#'
#' @section Information of sparse levels:
#' The information derived from the sparse levels diagnosis is as follows.
#'
#' \itemize{
#' \item variables : level of categorical variables.
#' \item N : number of observation. (optional)
#' }
#'
#' @param .data a data.frame or a \code{\link{tbl_df}}.
#' @param ... one or more unquoted expressions separated by commas.
#' You can treat variable names like they are positions.
#' Positive values select variables; negative values to drop variables.
#' If the first expression is negative, diagnose_sparese() will automatically
#' start with all variables.
#' These arguments are automatically quoted and evaluated in a context where
#' column names represent column positions.
#' They support unquoting and splicing.
#'
#' @param type a character string specifying how result are extracted.
#' "all" that returns a combination of all possible levels. At this time, 
#' the frequency of each case is also returned..
#' Default is "sparse" returns only sparse level combinations.
#' @param add_character logical. Decide whether to include text variables in the
#' diagnosis of categorical data. The default value is TRUE, 
#' which also includes character variables.
#' @param limit integer. Conditions to check sparse levels. 
#' If the number of all possible combinations exceeds the limit, the calculation ends.
#' @return an object of data.frame.
#' @export
#' @examples
#' \donttest{
#' library(dplyr)
#' 
#' # Examples of too many combinations
#' diagnose_sparese(jobchange)
#' 
#' # Character type is also included in the combination variable
#' diagnose_sparese(jobchange, add_character = TRUE)
#' 
#' # Combination of two variables
#' jobchange %>% 
#'   diagnose_sparese(education_level, major_discipline)
#'
#' # Remove two categorical variables from combination
#' jobchange %>% 
#'   diagnose_sparese(-city, -education_level)
#'
#' diagnose_sparese(heartfailure)
#' 
#' # Adjust the threshold of limt to calculate
#' diagnose_sparese(heartfailure, limit = 50)
#' 
#' # List all combinations, including parese cases
#' diagnose_sparese(heartfailure, type = "all") 
#' 
#' # collaboration with dplyr
#' heartfailure %>% 
#'   diagnose_sparese(type = "all") %>% 
#'   arrange(desc(n_case)) %>% 
#'   mutate(percent = round(n_case / sum(n_case) * 100, 1))
#' }
#' 
#' @method diagnose_sparese data.frame
#' @importFrom tidyselect vars_select
#' @importFrom rlang quos
#' @importFrom purrr map
#' @importFrom tibble is_tibble
#' @export
diagnose_sparese.data.frame <- function(.data, ..., type = c("all", "sparse")[2], 
                                         add_character = FALSE, limit = 500) {
  vars <- tidyselect::vars_select(names(.data), !!! rlang::quos(...))
  diagnose_sparese_impl(.data, vars, type, add_character, limit)
}

diagnose_sparese_impl <- function(df, vars, type, add_character, limit) {
  if (length(vars) == 0) vars <- names(df)

  if (add_character)
    name_factor <- find_class(df[, vars], type = "categorical2", index = FALSE)
  else
    name_factor <- find_class(df[, vars], type = "categorical", index = FALSE)
  
  if (length(name_factor) < 2) {
    stop("There must be more than 2 categorical variables in the data.\n")
    return(NULL)    
  }
  
  if (!type %in% c("all", "sparse")) {
    message("The type argument must be one of \"all\" or \"sparse\".\n")
    return(NULL)    
  }
  
  get_levels <- function(x) {
    if (tibble::is_tibble(x)) x <- pull(x)
    
    if (is.factor(x)) levels(x) else unique(x)
  }
  
  all_levels <- name_factor %>% 
    purrr::map(function(x) get_levels(df[, x])) 
  
  n_all_case <- all_levels %>% 
    purrr::map_int(length) %>% 
    prod()
  
  if (n_all_case > limit) {
    sprintf("All possible combinations of categorical variables exceed %d. (Number of combinations: %s)", 
            limit, format(n_all_case, big.mark = ",")) %>% 
    message()
    return(NULL)   
  }
  
  all_case <- all_levels %>% 
    expand.grid() 
  
  names(all_case) <- name_factor
  
  agg_case <- df[, name_factor] %>% 
    group_by_at(vars(one_of(name_factor))) %>% 
    tally() %>% 
    ungroup() %>% 
    mutate_at(vars(name_factor), function(x) factor(x, ordered = FALSE))
  
  if (type %in% ("sparse")) {
    setdiff(all_case, agg_case %>% select(-n))    
  } else if (type %in% ("all")) {
    suppressMessages(
      all_case %>% 
        left_join(agg_case) %>% 
        rename(n_case = n) %>% 
        mutate(n_case = ifelse(is.na(n_case), 0 , n_case))
    )
  }
}


