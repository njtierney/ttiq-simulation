#' Create cosmetic parameters
#' 
#' Cosmetic plotting and formatting parameters to be shared between plots.
#' Specify these manually
#' 
#' @export
create_scenario_parameters <- function() {
  dark2 = brewer.pal(8, "Dark2")
  # Define plotting constants in order
  tribble(
    ~name, ~value, ~colour,
    "NSW Optimal", "optimal", dark2[1],
    "VIC Partial", "partial", dark2[2],
    "NSW Current\nwith case-initiated", "current_nsw_case_init", dark2[3],
    "NSW Current\nwithout case-initiated", "current_nsw", dark2[3],
    "VIC Current\nwith case-initiated", "current_vic_case_init", dark2[4],
    "VIC Current\nwithout case-initiated", "current_vic", dark2[4],
    # deprecated - to be replaced later
    "NSW Current", "current", dark2[3]
  ) %>%
    mutate(name = fct_inorder(name))
}