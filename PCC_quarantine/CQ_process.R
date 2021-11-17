# Community Quarantine of PCC's simulation model
#
# Authors: David J Price, Joshua V Ross
# Date: 26 October 2021
#

source(here::here("PCC_quarantine", "CQ_functions.R"))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


CQ.sim2 <- function(n.ind,
                    TP,
                    k,
                    p.vac.idx,
                    p.vac.sc,
                    vacc.cor,
                    VE.trans,
                    VE.inf,
                    quarantine.duration,
                    the.scenario,
                    the.state = "NSW") {
  set.seed(1)
  # tic <- Sys.time()
  out <- data.frame("i" = 1:n.ind) %>%
    mutate(
      vacc.status = rbinom(n.ind, size = 1, prob = p.vac.idx),
      #) %>%
      inc.period = inc.period.samp(n.ind),
      #) %>%
      iso.time = iso.time.samp(n.ind, the.scenario),
      #) %>%
      ncases = ncases.samp(n.ind, TP, k),
      # ) %>%
      hh.size = samp.hh.size(n.ind, the.state)
    ) %>% # individual-level details
    group_by(i) %>%
    mutate(inf.times = list(gi.dist.samp(ncases))) %>%
    unnest(inf.times) %>%
    # mutate(preiso = inf.times < iso.time,
    #        hh = inf.times > iso.time & inf.times < iso.time + quarantine.duration,
    #        postiso = inf.times > iso.time + quarantine.duration) %>%
    # group_by(i, hh) %>%
    # mutate(nh = row_number())
    mutate(
      who.original = case_when(
        inf.times < iso.time ~ "preiso",
        inf.times > iso.time &
          inf.times < iso.time + quarantine.duration ~ "hh",
        inf.times > iso.time + quarantine.duration ~ "postiso"
      )
    ) %>%
    # # randomly sample preiso's to be hh instead
    mutate(who.new = hh.infections.pre.or.postiso(
      inf.times = inf.times,
      who = who.original,
      hh.size = hh.size,
      p = 0.5
    )) %>%
    group_by(i, who.new) %>%
    mutate(hh.counter = case_when(who.new == "hh" ~ row_number(),
                                  TRUE ~ 100L)) %>%
    ungroup() %>%
    mutate(keep.row = case_when(
      who.new == "hh" & who.original == "hh" & hh.counter >= hh.size ~ 0,
      TRUE ~ 1
    )) %>%
    filter(keep.row == 1) %>%
    mutate(who = case_when(hh.counter >= hh.size ~ who.original,
                           TRUE ~ who.new)) %>%
    select(-who.original, -who.new, -hh.counter, -keep.row) %>%
    group_by(i) %>%
    mutate(sc.vac.status = case_when(
      who == "hh" ~ cor.binary(
        n = n(),
        corr = vacc.cor,
        idx.vac.status = vacc.status
      ),
      TRUE ~ rbernoulli(n = n(), p = p.vac.sc)
    )) %>%
    mutate(det = passive.detection(inf.times, the.scenario)) %>%
    ungroup() %>%
    mutate(unprotected = runif(n()) < (1 - VE.trans * vacc.status) * (1 - VE.inf *
                                                                        sc.vac.status)) %>%
    filter(unprotected) %>% select(-unprotected)
  # (toc <- Sys.time() - tic)
  # Break here as this is where the test.times comes in
  
  return(out)
}




CQ.sim.test.times <- function(CQ.sim.output,
                              n.ind,
                              test.times,
                              VE.trans,
                              the.scenario) {
  out <- CQ.sim.output %>%
    group_by(i) %>%
    mutate(
      first.pos = testing.function2(
        test.times = test.times,
        iso.time = iso.time,
        inc.period = inc.period,
        the.scenario = the.scenario
      )
    ) %>%
    mutate(
      first.pos = first.pos + test.turnaround.samp(n = 1, the.scenario = the.scenario) +
        other.delay.samp(n = 1, the.scenario = the.scenario)
    ) %>%
    ungroup() %>%
    filter(
      who == "preiso" |
        who == "hh" & inf.times < first.pos |
        who == "postiso" & is.infinite(first.pos)
    ) %>%
    mutate(ipq = gi.dist.cdf(q = pmin(first.pos, det) - inf.times) * TP * (1 - VE.trans *
                                                                             sc.vac.status)) %>%
    summarise(
      IPq = sum(ipq, na.rm = TRUE) / n.ind,
      sdIPq = sd(ipq, na.rm = TRUE),
      mean.cases = n() / n.ind
    )
  return(out)
}





CQ.sim.notest <-
  function(CQ.sim.output,
           n.ind,
           VE.trans,
           the.scenario) {
    out <- CQ.sim.output %>%
      group_by(i) %>%
      # mutate(first.pos = testing.function2(test.times = test.times, iso.time = iso.time,
      #                                      inc.period = inc.period, the.scenario = the.scenario)) %>%
      mutate(first.pos = passive.detection.only(n = n(), the.scenario = the.scenario)) %>%
      mutate(first.pos = first.pos + test.turnaround.samp(n = n(), the.scenario = the.scenario)) %>%
      ungroup() %>%
      filter(
        who == "preiso" |
          who == "hh" & inf.times < first.pos |
          who == "postiso" & is.infinite(first.pos)
      ) %>%
      mutate(ipq = gi.dist.cdf(q = pmin(first.pos, det) - inf.times) * TP * (1 - VE.trans *
                                                                               sc.vac.status)) %>%
      summarise(
        IPq = sum(ipq, na.rm = TRUE) / n.ind,
        sdIPq = sd(ipq, na.rm = TRUE),
        mean.cases = n() / n.ind
      )
    return(out)
  }
