# new_empirical_dist
# make sure we get a prob for each day even if the sample lacked them
x <- rpois(100, 1)
x
bins <- 0:20
breaks <- c(bins, max(bins) + 1)- 0.5

hist_pmf <- function(x, 
                     bins = 0:20,
                     breaks = c(bins, max(bins) + 1)- 0.5){
  hist_info <- hist(x, breaks = breaks, plot = FALSE)
  hist_info$density
}

sample(x[1:21],
       size = 1000,
       replace = TRUE,
       prob = hist_pmf(x))

sample_weights <- cases_scenario %>% 
  filter(scenario == "optimal") %>% 
  pull(test_turnaround_time) %>% 
  hist_pmf()

generate_from_pmf <- function(x, times){
  sample(0:20,
         size = times,
         replace = TRUE,
         prob = hist_pmf(x))
}

sims <- generate_from_pmf(x, times = 1e7)

new_sims <- derive_poisson_mixture(sims)

new_samples <- generate(new_sims, 1e6)[[1]]

new_hist <- function(x) x %>% table() %>% barplot()

table(sims)
table(new_samples)

new_hist(sims)
new_hist(new_samples)

derive_poisson_mixture(x)

# prepare the empirical data part
delay_days <- cases_scenario %>% 
  group_by(scenario) %>%
  summarise(
    pmf_test_turnaround_time = hist_pmf(test_turnaround_time, 
                                        breaks = breaks),
    pmf_time_to_interview = hist_pmf(time_to_interview, 
                                     breaks = breaks),
    pmf_full_contact_delay = hist_pmf(full_contact_delay, 
                                      breaks = breaks),
    pmf_test_to_interview = hist_pmf(test_to_interview,
                                     breaks = breaks)) %>% 
  mutate(
    days = bins,
    fraction_extra_zero = 0) %>% 
  relocate(days, .before = pmf_test_turnaround_time)