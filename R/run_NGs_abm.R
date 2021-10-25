# run Nick’s ABM

files <- list.files('R/')

full_names <- lapply(files, function(x) paste0('R/', x))
lapply(full_names, source)


# change max_days argument in get_valid_abm_sample.R
# and in compute_abm_metrics.R x2
# change initial infections argument in get_valid_abm_sample.R

# key scripts
# setup_abm.R
# R and vaccination coverage is specified in
# get_valid_ABM_sample
# simulation criteria are specified
future::plan(multisession(workers = 8))
#future::plan(sequential, split=TRUE)
sims <- expand_grid(
  vaccination_coverage = c(0.7),
  #vaccination_test_seeking_multiplier = c(1),
  passive_detection_given_symptoms = c(0.5),
  rel_active_detection_vaccinated_source = c(1, 0.5, 0),
  rel_active_detection_vaccinated_contact = c(1, 0.5, 0),
  do_ttiq = c(TRUE)
) %>%
  mutate(
    # tweak starting R to get optimal reproduction number at about 1
    R = case_when(
      vaccination_coverage == 0.9 ~ 4.6, # fails <2.58
      vaccination_coverage == 0.8 ~ 4.6, # fails < 1.9
      vaccination_coverage == 0.7 ~ 4.6, # fails < 1.55
    )
    
  ) %>%
  rowwise() %>%
  mutate(
    parameters = list(
      setup_abm(
        R = R,
        vaccination_coverage = vaccination_coverage,
        #vaccination_test_seeking_multiplier = vaccination_test_seeking_multiplier,
        passive_detection_given_symptoms = passive_detection_given_symptoms,
        rel_active_detection_vaccinated_source = rel_active_detection_vaccinated_source,
        rel_active_detection_vaccinated_contact = rel_active_detection_vaccinated_contact,
        contact_tracing = do_ttiq,
        screening = do_ttiq
      )
    )
  ) %>%
  mutate(
    simulations = list(
      get_valid_abm_samples(parameters, n_samples = 50)
    )
  )


# format the sims
sims %>%
  mutate(
    metrics = list(
      compute_abm_metrics(simulations)
    )
  ) %>%
  select(
    -parameters,
    -simulations
  ) %>%
  unnest(
    cols = metrics
  ) #%>%
  # # filter(
  # #   statistic == "mean"
  # # ) %>%
  # # select(
  # #   -statistic
  # # ) %>%
  # pivot_wider(
  #   names_from = vaccination_test_seeking_multiplier,
  #   values_from = value,
  #   names_prefix = "test_vacc_sympt_"
  # ) %>%
  # mutate(
  #   across(
  #     starts_with(
  #       "test_vacc_sympt"
  #     ),
  #     .fns = list(ratio = ~.x / test_vacc_sympt_1)
  #   )
  # ) %>%
  # arrange(
  #   statistic, metric, passive_detection_given_symptoms, vaccination_coverage
  # ) %>%
  # print(n = Inf)

plot_df <- sims %>%
  filter(
    vaccination_coverage == 0.7,
    #vaccination_test_seeking_multiplier == 0,
    passive_detection_given_symptoms == 0.5
  ) %>%
  select(-parameters) %>%
  unnest(simulations) #%>%
  #filter(simulation %in%
  #         paste0("sim_", 1:6)) #%>%

# plot different colours with test seeking multiplier
# ggplot(plot_df,
#        aes(
#          x = infection_day, 
#          fill = factor(vaccination_test_seeking_multiplier)
#        )
# ) +
#   geom_histogram(
#     binwidth = 1, 
#     alpha = 0.5, 
#     position = "identity"
#     # colour = "white"
#   ) +
#   facet_wrap(~simulation) +
#   ggtitle(
#     paste0("R = ", plot_df$R,
#            " Vacc. cov. = ",
#            plot_df$vaccination_coverage)
#   ) +
#   labs(fill = "Test seeking\nmultiplier") +
#   theme_minimal()

# spaghetti plots of sims
plot_df_line <- plot_df %>% 
  group_by(simulation, 
           infection_day,
           rel_active_detection_vaccinated_source
           #vaccination_test_seeking_multiplier
           ) %>% 
  summarise(infections=n()
            ) 

# ggplot(
#    plot_df_line, 
#    aes(
#      x=infection_day, 
#      y=log(infections),
#      group=simulation)
#  ) + 
#    geom_line(aes(color=simulation)) 

df_line_0 <- plot_df %>% 
  group_by(simulation, 
           infection_day,
           rel_active_detection_vaccinated_source
           #vaccination_test_seeking_multiplier
  ) %>% 
  summarise(infections=n()
  ) %>% 
  #filter(vaccination_test_seeking_multiplier==0)
  filter(rel_active_detection_vaccinated_source==1)

ggplot(
    df_line_0, 
    aes(
      x=infection_day, 
      y=log(infections),
      group=simulation)
  ) + 
    geom_line(aes(color=simulation)) 

df_line_1 <- plot_df %>% 
  group_by(simulation, 
           infection_day,
           rel_active_detection_vaccinated
           #vaccination_test_seeking_multiplier
  ) %>% 
  summarise(infections=n()
  ) %>% 
  #filter(vaccination_test_seeking_multiplier==1)
  filter(rel_active_detection_vaccinated==1)  

ggplot(
  df_line_1, 
  aes(
    x=infection_day, 
    y=log(infections),
    group=simulation)
) + 
  geom_line(aes(color=simulation)) 

dat_1 <- plot_df_line %>% 
  filter(infection_day > 20, infection_day < 50) %>% 
  #filter(vaccination_test_seeking_multiplier==1)
  filter(rel_active_detection_vaccinated==1)  
  
dat_0 <- plot_df_line %>% 
  filter(infection_day > 20, infection_day < 50) %>% 
  #filter(vaccination_test_seeking_multiplier==0)
  filter(rel_active_detection_vaccinated==0)  
 
lm_1 <- lm(formula = log(infections) ~ infection_day,
           data = dat_1)  
 
lm_0 <- lm(formula = log(infections) ~ infection_day,
   data = dat_0)  

x_vals <- c(-6:200)
y_vals_0 <- lm_0$coefficients[1] + x_vals*lm_0$coefficients[2]
y_vals_1 <- lm_1$coefficients[1] + x_vals*lm_1$coefficients[2]

ggplot() +
  geom_line(data=df_line_1,
            aes(x=infection_day, 
                y=log(infections),
                group=simulation,
                color='grey20')) +
  geom_line(data=df_line_0,
            aes(x=infection_day, 
                y=log(infections),
                group=simulation,
                color='black')) +
  geom_line(aes(x_vals,
                y_vals_0)) +
  geom_line(aes(x_vals,
                y_vals_1),
            linetype='dashed') 

# plot baseline
ggplot() +
  geom_line(data=df_line_1,
            aes(x=infection_day, 
                y=log(infections),
                group=simulation,
                color=simulation)) +
  geom_line(aes(x_vals,
                y_vals_1)) 

ggplot() +
  geom_line(data=df_line_0,
            aes(x=infection_day, 
                y=log(infections),
                group=simulation,
                color=simulation)) +
  geom_line(aes(x_vals,
                y_vals_0)) 

# make residual plots
# create df of fitted values
fitted_vals <- data.frame(
  infection_day = x_vals,
  fitted_value_0 = y_vals_0,
  fitted_value_1 = y_vals_1
)

df_0_r <- df_line_0 %>% 
  left_join(fitted_vals) %>% 
  mutate(residual=log(infections)-fitted_value_0)

df_1_r <- df_line_1 %>% 
  left_join(fitted_vals) %>% 
  mutate(residual=log(infections)-fitted_value_1)

plot(df_0_r$infection_day, df_0_r$residual, cex=0.5, pch=21, bg='black')
plot(df_1_r$infection_day, df_1_r$residual, cex=0.5, pch=21, bg='black')  
  
df_0_r_sry <- df_0_r %>% 
  group_by(infection_day) %>% 
  summarise(mean=mean(residual), sd=sd(residual))

plot(df_0_r_sry$infection_day, df_0_r_sry$sd, cex=0.5, pch=21, bg='black')

df_1_r_sry <- df_1_r %>% 
  group_by(infection_day) %>% 
  summarise(mean=mean(residual), sd=sd(residual))

plot(df_1_r_sry$infection_day, df_1_r_sry$sd, cex=0.5, pch=21, bg='black')

# calculate TPs
sry_tps <- sims %>%
  mutate(
    metrics = list(
      compute_abm_metrics(simulations)
    )
  ) %>%
  select(
    -parameters,
    -simulations
  ) %>%
  unnest(
    cols = metrics
  )

# calculate TPs
full_tps <- sims %>%
  mutate(
    metrics = list(
      compute_abm_metrics_full(simulations)
    )
  ) %>%
  select(
    -parameters,
    -simulations
  ) %>%
  unnest(
    cols = metrics
  )

# make scatterplot of tps
plot_tps <- full_tps %>% 
  filter(vaccination_coverage==0.9,
         passive_detection_given_symptoms==0.5)
  
ggplot(plot_tps) +
  geom_boxplot(aes(x=factor(vaccination_test_seeking_multiplier),
             y=TP))


# ratio of little rs
#ratio_r <- lm_0$coefficients[[2]]/lm_1$coefficients[[2]]

write.csv(sry_tps,
          'freyas_outputs/sry_tps_70pc_vax_R4.6_cf40.csv',
          row.names=FALSE)

write.csv(full_tps,
          'freyas_outputs/full_tps_70pc_vax_R4.6_cf40.csv',
          row.names=FALSE)


pc_change <- full_tps %>% 
  filter(passive_detection_given_symptoms==0.5) %>% 
  group_by(simulation, rel_active_detection_vaccinated_contact, rel_active_detection_vaccinated_source) %>% 
  summarise(tp_ratio=get_ratio(value, )) 


sry_ratios <- sry_20 %>% 
  rbind(sry_30, sry_40) %>% 
  filter(passive_detection_given_symptoms==0.5,
         metric=='TP',
         statistic=='mean') %>% 
  group_by(symp_frac) %>% 
  summarise(tp_ratio=get_ratio(value, vaccination_test_seeking_multiplier)) 

# loop over simulations calculating ratio between reference and others
sims <- unique(full_tps$simulation)

sim_final <- data.frame()

for (i in sims){
  
  sim_dat <- full_tps %>% filter(simulation==i)
  
  ref_tp <- as.numeric(sim_dat %>% filter(rel_active_detection_vaccinated_source==1,
                            rel_active_detection_vaccinated_contact==1) %>% 
    select(TP))
    
  sim_sorted <- sim_dat %>% 
    mutate(pc_change=(TP/ref_tp-1)*100) 
  
  sim_final <- rbind(sim_final, sim_sorted)
  
}

sim_plot <- sim_final %>% 
  mutate(strategy=paste0(rel_active_detection_vaccinated_source, 
                          '_',
                          rel_active_detection_vaccinated_contact)) %>% 
  filter(strategy!='1_1') 
  

sim_sry <- sim_plot %>% 
  group_by(rel_active_detection_vaccinated_source,
           rel_active_detection_vaccinated_contact) %>% 
  summarise(mean=mean(pc_change), sd=sd(pc_change))
  

# generate boxplots of TPs by scenarios
ggplot(sim_plot) +
  #geom_boxplot(aes(x=factor(strategy),
  #                  y=pc_change)) +
  geom_point(aes(x=factor(1),
                 y=pc_change),
             position=position_jitter(width=0.1),
             colour='grey70',
             size=1) +
  geom_pointrange(aes(x=factor(1), y=mean, ymin = mean-sd, ymax = mean+sd), data = sim_sry) +
  geom_hline(yintercept=0,
             linetype='dashed') +
  facet_grid(rel_active_detection_vaccinated_source ~ rel_active_detection_vaccinated_contact) +
  theme_bw() + 
  theme(panel.grid=element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(size=0.2, color = 'black'),
        #axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(),
        plot.title = element_text(hjust = 0.5),
        aspect.ratio = 1) +
  labs(x='strategy',
       y='% increase')

ggsave(
  "freyas_outputs/vax70pc_vary_cf.png",
  width = 6,
  height = 4.5,
  dpi = 500
)






