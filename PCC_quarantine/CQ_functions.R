# Functions for evaluating testing/isolating strategy on transmission for WP1
#
# Authors: David J Price, Joshua V Ross
# Date: 26 October 2021
#

require(tidyverse)


abbreviate_states <- function(state_names) {
  case_when(
    state_names %in% c("Australian Capital Territory", "ACT") ~ "ACT",
    state_names %in% c("New South Wales", "NSW") ~ "NSW",
    state_names %in% c("Northern Territory", "NT") ~ "NT",
    state_names %in% c("Queensland", "QLD") ~ "QLD",
    state_names %in% c("South Australia", "SA") ~ "SA",
    state_names %in% c("Tasmania", "TAS") ~ "TAS",
    state_names %in% c("Victoria", "VIC") ~ "VIC",
    state_names %in% c("Western Australia", "WA") ~ "WA"
  )
}

unabbreviate_states <- function(state_names) {
  case_when(
    state_names %in% c("Australian Capital Territory", "ACT") ~ "Australian Capital Territory",
    state_names %in% c("New South Wales", "NSW") ~ "New South Wales",
    state_names %in% c("Northern Territory", "NT") ~ "Northern Territory",
    state_names %in% c("Queensland", "QLD") ~ "Queensland",
    state_names %in% c("South Australia", "SA") ~ "South Australia",
    state_names %in% c("Tasmania", "TAS") ~ "Tasmania",
    state_names %in% c("Victoria", "VIC") ~ "Victoria",
    state_names %in% c("Western Australia", "WA") ~ "Western Australia"
  )
}

hhsize.dat <-
  read_csv(here::here("data/hh_byperson_proportions.csv"))

to.named.vector <- function(df) {
  out <- df[, 2] %>% pull()
  names(out) <- as.character(df[, 1] %>% pull())
  return(out)
}

# Get vector of household sizes (1 - 8+) for a random individual
hhsizes <- function(the.state) {
  hhsize.dat %>% select(number, unabbreviate_states(the.state)) %>% to.named.vector() %>% cumsum()
}


samp.hh.size <- function(n, the.state) {
  r <- runif(n)
  hhs <- hhsizes(the.state)
  hh.sizes <-
    sapply(
      X = r,
      FUN = function(x) {
        min(which(x < hhsizes(the.state)))
      }
    )
  return(hh.sizes)
}


# Incubation period
inc.period.samp <- function(n) {
  rlnorm(n, meanlog = 1.63, sdlog = 0.5)
}


distn.dat <-
  read_csv(here::here("data", "ttiq_scenario_run.csv.gz"))

# active.samples <- distn.dat %>% filter(scenario == the.scenario) %>% select(time_to_active) %>% pull() %>% as.numeric()
# passive.samples <- distn.dat %>% filter(scenario == the.scenario) %>% select(time_to_passive) %>% pull() %>% as.numeric()
# all.samples <- distn.dat %>% filter(scenario == the.scenario) %>% select(time_to_isolation_sims) %>% pull() %>% as.numeric()


# Time to isolation from infection
iso.time.samp <- function(n.ind, the.scenario = "optimal") {
  # Need this distribution from others, later make input n.
  # Consistent with JVR for now — assume they are idenfified somewhere between infection and symptom onset
  # pmax(incubation.periods*runif(length(incubation.periods)), incubation.periods - rexp(length(incubation.periods), rate = 1/4))
  
  # The context is PCC's of cases. Their time to isolation should come from the active component (contact-tracing), rather than passive
  distn.dat %>%
    filter(scenario == the.scenario) %>%
    mutate(x = as.numeric(time_to_active)) %>%
    select(x) %>%
    filter(is.finite(x)) %>%
    slice_sample(n = n.ind, replace = TRUE) %>%
    pull() %>% as.numeric()
}


# Sample number of cases generated by an individual from NB distribution
ncases.samp <- function(n, TP, k) {
  # number of samples, transmission potential, dispersion parameter
  rnbinom(n, size = k, mu = TP)
}


tat.dat <- read_csv(here::here("data",
                               "scenario_test_turnaround_time_sims.csv")) %>%
  mutate(
    scenario = case_when(
      scenario == "current_nsw_case_init_0.8" ~ "current_nsw_case_init",
      TRUE ~ scenario
    )
  )
# pull out each delay as a separate vector to speed up the sampling

partial.tat.dat <- tat.dat %>%
  filter(scenario == "partial") %>%
  select(sampled_test_turnaround_time) %>% pull()

optimal.tat.dat <- tat.dat %>%
  filter(scenario == "optimal") %>%
  select(sampled_test_turnaround_time) %>% pull()

nsw.ci.tat.dat <- tat.dat %>%
  filter(scenario == "current_nsw_case_init") %>%
  select(sampled_test_turnaround_time) %>% pull()

test.turnaround.samp <- function(n, the.scenario) {
  # rexp(n, 0.5)
  # tat.dat %>%
  #   filter(scenario == the.scenario) %>%
  #   select(sampled_test_turnaround_time) %>%
  #   slice_sample(n = n, replace = TRUE) %>%
  #   pull()
  if (the.scenario == "partial") {
    sample(x = partial.tat.dat,
           size = n,
           replace = TRUE)
  }
  else {
    if (the.scenario == "optimal") {
      sample(x = optimal.tat.dat,
             size = n,
             replace = TRUE)
    } else{
      sample(x = nsw.ci.tat.dat,
             size = n,
             replace = TRUE)
    }
  }
  
}




#  Delay from notification to interview, and interview to notifying CCs
#  Pull out each delay as a separate vector to speed up sampling

other.delay.dat <- read_csv(here::here("data",
                                       "scenario_turnaround_time_components_probs.csv")) %>%
  mutate(
    scenario = case_when(
      scenario == "current_nsw_case_init_0.8" ~ "current_nsw_case_init",
      TRUE ~ scenario
    )
  )


partial.interview.dat <-
  other.delay.dat %>% filter(scenario == "partial", distribution == "time_to_interview") %>%
  select(days, density) %>% to.named.vector() %>% cumsum()
partial.cc.notify.dat <-
  other.delay.dat %>% filter(scenario == "partial", distribution == "interview_isolation") %>%
  select(days, density) %>% to.named.vector() %>% cumsum()


optimal.interview.dat <-
  other.delay.dat %>% filter(scenario == "optimal", distribution == "time_to_interview") %>%
  select(days, density) %>% to.named.vector() %>% cumsum()
optimal.cc.notify.dat <-
  other.delay.dat %>% filter(scenario == "optimal", distribution == "interview_isolation") %>%
  select(days, density) %>% to.named.vector() %>% cumsum()


nsw.ci.interview.dat <-
  other.delay.dat %>% filter(scenario == "current_nsw_case_init",
                             distribution == "time_to_interview") %>%
  select(days, density) %>% to.named.vector() %>% cumsum()
nsw.ci.cc.notify.dat <-
  other.delay.dat %>% filter(scenario == "current_nsw_case_init",
                             distribution == "interview_isolation") %>%
  select(days, density) %>% to.named.vector() %>% cumsum()

other.delay.samp <- function(n, the.scenario) {
  if (the.scenario == "partial") {
    cumulative.probs.interview <- partial.interview.dat
    cumulative.probs.notify <- partial.cc.notify.dat
  }
  else {
    if (the.scenario == "optimal") {
      cumulative.probs.interview <- optimal.interview.dat
      cumulative.probs.notify <- optimal.cc.notify.dat
    } else{
      cumulative.probs.interview <- nsw.ci.interview.dat
      cumulative.probs.notify <- nsw.ci.cc.notify.dat
    }
  }
  
  r1 <- runif(n)
  r2 <- runif(n)
  
  interview.delay <-
    sapply(
      X = r1,
      FUN = function(x) {
        names(cumulative.probs.interview)[min(which(x < cumulative.probs.interview))]
      }
    )
  notify.delay <-
    sapply(
      X = r2,
      FUN = function(x) {
        names(cumulative.probs.notify)[min(which(x < cumulative.probs.notify))]
      }
    )
  
  return(as.numeric(interview.delay) + as.numeric(notify.delay))
  
}





gi.dist.cdf <- function(q) {
  # Change to Aus baseline GI distribution
  plnorm(q, meanlog = 1.376, sdlog = 0.567)
}


gi.dist.samp <- function(n) {
  # Change to Aus baseline GI distribution
  rlnorm(n, meanlog = 1.376, sdlog = 0.567)
}


# passive.detection <- function(inf.times){
#   it <- unlist(inf.times)
#   n <- length(it[!is.na(it)])
#   # pmin(
#   #   rgamma(n, shape = 7.5/1.6, scale = 1.6), # made up TTIQ dist assumed optimal ttiq <-ACTIVE
#   #   inc.period.samp(n) + test.turnaround.samp(n) # or, incubation period + testTAT <- PASSIVe
#   # ) +
#   #   it
#
#   distn.dat %>%
#     filter(scenario == the.scenario) %>%
#     select(time_to_isolation_sims) %>%
#     slice_sample(n = n) %>%
#     pull() %>% as.numeric() + it
#
# }

passive.detection <- function(inf.times, the.scenario) {
  it <- unlist(inf.times)
  n <- length(it)
  
  # if rand < xx, sample below, else, Inf
  distn.dat %>%
    filter(scenario == the.scenario) %>%
    select(time_to_passive) %>%
    slice_sample(n = n, replace = TRUE) %>%
    pull() %>% as.numeric() + it
  
}

passive.detection.only <- function(n, the.scenario) {
  distn.dat %>%
    filter(scenario == the.scenario) %>%
    select(time_to_passive) %>%
    slice_sample(n = n) %>%
    pull() %>% as.numeric()
  
  
}



active.detection.only <- function(n, the.scenario) {
  distn.dat %>%
    filter(scenario == the.scenario) %>%
    select(time_to_active) %>%
    slice_sample(n = n) %>%
    pull() %>% as.numeric()
}


sec.inf.vac <- function(inf.times, vacc.status, sc.vac.status) {
  n <- length(inf.times)
  if (n > 0) {
    (runif(n) < (1 - VE.trans * vacc.status) * (1 - VE.inf * sc.vac.status)) # returns a vector of those that were infected
  } else{
    (vector("logical"))
  }
}




testing.function2 <-
  function(test.times,
           iso.time,
           inc.period,
           the.scenario) {
    C <- min(inc.period, 3.5) * runif(1)
    
    t <- iso.time + test.times - 1 - inc.period
    s <- t + C
    
    idx <- t >= -inc.period & t <= -C
    
    test.prob <- c(1 / (1 + exp(-(1.5 + 2.2 * s[idx]))),  # pre peak
                   1 / (1 + exp(-(1.5 - 0.22 * s[!idx])))) # post peak
    
    
    test.results <- runif(length(t)) < test.prob
    ttiv <- iso.time + test.times - 1
    # + test.turnaround.samp(n = length(test.times), the.scenario = the.scenario)
    first.pos <- min(ttiv[as.logical(test.results)], Inf)
    # Time results returned relative to isolation
    return(first.pos)
  }


the.test.prob <- function(test.times, iso.time, inc.period) {
  C <- min(inc.period, 3.5) * runif(1)
  
  t <- iso.time + test.times - 1 - inc.period
  s <- t + C
  
  idx <- t >= -inc.period & t <= -C
  
  test.prob <- c(1 / (1 + exp(-(1.5 + 2.2 * s[idx]))),  # pre peak
                 1 / (1 + exp(-(1.5 - 0.2 * s[!idx])))) # post peak
  return(test.prob)
}



cor.binary <- function(n, corr, idx.vac.status) {
  ind <- runif(n) < corr
  as.logical(idx.vac.status * (ind) + (1 - idx.vac.status) * (!ind))
}





hh.infections.pre.or.postiso <-
  function(inf.times, who, hh.size, p = 0.5) {
    pre.postiso.to.hh <- which(who %in% c("preiso", "postiso"))
    n <- length(pre.postiso.to.hh) # number of preiso infections
    n.pre.postiso.hh <-
      min(rbinom(n = 1, size = n, prob = p), unique(hh.size) - 1)
    idx.to.change <-
      sample(x = pre.postiso.to.hh, size = n.pre.postiso.hh)
    who[idx.to.change] <- "hh"
    return(who)
  }



test.designs <- function(times, n, min.space, t1.prior.to = 5) {
  l <- rep(list(times), n)
  all.times <- expand.grid(l)
  
  if (n > 1) {
    all.times <- all.times %>% mutate(id = row_number()) %>%
      pivot_longer(cols = starts_with("Var")) %>%
      group_by(id) %>% filter(min(diff(value)) >= min.space) %>%
      pivot_wider(names_from = "name", values_from = "value") %>%
      ungroup() %>%
      select(-id) %>%
      filter(Var1 <= t1.prior.to)
  }
  return(all.times)
}

# xx <- 21; inf.times <- out$inf.times[out$i == xx]; who <- out$who[out$i == xx]; hh.size <- out$hh.size[out$i == xx]
