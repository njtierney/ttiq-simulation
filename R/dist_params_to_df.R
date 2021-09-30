#' Convert a dataframe of distributions into a dataframe of parameters
#' 
#' @param derived_delay_distributions Dataframe with a scenario column and other
#' columns of distributions
#' 
#' @return A dataframe of 
dist_params_to_df <- function(derived_delay_distributions) {
  derived_delay_distributions %>%
    pivot_longer(-scenario, names_to="delay_name", values_to="distribution") %>%
    mutate(text = as.character(distribution)) %>%
    separate(text, into=c("rate", "w"), sep=",", fill="right") %>%
    mutate(rate = as.numeric(str_remove(rate, "l=")),
           w = as.numeric(str_remove(w, "p="))) %>%
    select(-distribution)
}

