

# standard scenario
# 80% vaccination effect
# clinical fraction 0.307
# testing seeking behaviour 0.5

# load in results files
sry <- read.csv('freyas_outputs/sry_tps_80pc_A_std.csv',
                  stringsAsFactors = FALSE)

full <- read.csv('freyas_outputs/full_tps_80pc_A_std.csv',
                  stringsAsFactors = FALSE)

full_plot <- full %>% 
  mutate(vaccination_test_seeking_multiplier=case_when(
    vaccination_test_seeking_multiplier==1 ~ 'Yes',
    vaccination_test_seeking_multiplier==0 ~ 'No'
  ))

# make scatterplot of tps
sry <- full_plot %>% 
  group_by(
    vaccination_test_seeking_multiplier) %>% 
  summarise(mean=mean(TP), se=se(TP)) 


ggplot(full_plot) +
  geom_point(aes(x=factor(vaccination_test_seeking_multiplier),
               y=TP),
           position=position_jitter(width=0.1),
           colour='grey70',
           size=1) +
  geom_pointrange(data = sry,
                  aes(x=factor(vaccination_test_seeking_multiplier), 
                      y=mean, 
                      ymin = mean-2*se,
                      ymax = mean+2*se
                  ), 
                  size=0.6) +
  theme_bw() + 
  theme(panel.grid=element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(size=0.2, color = 'black'),
        plot.title = element_text(hjust = 0.5),
        aspect.ratio = 1) +
  labs(x='Testing of symptomatic vaccinated individuals',
       y='Transmission potential')


ggsave(
  "freyas_outputs/res_A_tps.png",
  width = 4,
  height = 4,
  dpi = 500
)

# make ratio plot

ratios <- full %>% 
  group_by(simulation) %>% 
  summarise(tp_change=get_pc_change(TP, vaccination_test_seeking_multiplier))
  
ratios_sry <- ratios %>% 
  summarise(mean=mean(tp_change), se=se(tp_change))
  
ggplot(ratios) +
  geom_point(aes(x=factor(1),
                 y=tp_change),
             position=position_jitter(width=0.1),
             #colour='grey70',
             #pch=21,
             colour='pink',
             #colour='grey30',
             size=2) +
  geom_hline(yintercept=0,
             linetype='dashed') +
  geom_pointrange(data=ratios_sry,
                  aes(x=factor(1), 
                       y=mean, 
                       ymin = mean-2*se,
                       ymax = mean+2*se
                   ), 
                   size=0.7) +
  coord_cartesian(
    ylim=c(-20, 20)) +
  theme_bw() + 
  theme(panel.grid=element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(size=0.2, color = 'black'),
        axis.text.x = element_text(color='white'),
        axis.ticks.x = element_blank(),
        plot.title = element_text(hjust = 0.5),
        aspect.ratio = 1) +
  labs(x='',
       y='% increase in TP')

ggsave(
  "freyas_outputs/res_A_pc_change.png",
  width = 4,
  height = 4,
  dpi = 500
)


# special scenario

# load in results files
sry_special <- read.csv('freyas_outputs/sry_tps_A_special.csv',
                stringsAsFactors = FALSE)

full_special <- read.csv('freyas_outputs/full_tps_A_special.csv',
                 stringsAsFactors = FALSE)


full_plot <- full_special %>% 
  mutate(vaccination_test_seeking_multiplier=case_when(
    vaccination_test_seeking_multiplier==1 ~ 'Yes',
    vaccination_test_seeking_multiplier==0 ~ 'No'
  ))

# make scatterplot of tps
sry <- full_plot %>% 
  group_by(
    vaccination_test_seeking_multiplier) %>% 
  summarise(mean=mean(TP), se=se(TP)) 


ggplot(full_plot) +
  geom_point(aes(x=factor(vaccination_test_seeking_multiplier),
                 y=TP),
             position=position_jitter(width=0.1),
             colour='grey70',
             size=1) +
  geom_pointrange(data = sry,
                  aes(x=factor(vaccination_test_seeking_multiplier), 
                      y=mean, 
                      ymin = mean-2*se,
                      ymax = mean+2*se
                  ), 
                  size=0.6) +
  theme_bw() + 
  theme(panel.grid=element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(size=0.2, color = 'black'),
        plot.title = element_text(hjust = 0.5),
        aspect.ratio = 1) +
  labs(x='Testing of symptomatic vaccinated individuals',
       y='Transmission potential')


ggsave(
  "freyas_outputs/res_A2_tps.png",
  width = 4,
  height = 4,
  dpi = 500
)

ratios <- full_special %>% 
  group_by(simulation) %>% 
  summarise(tp_change=get_pc_change(TP, vaccination_test_seeking_multiplier))

ratios_sry <- ratios %>% 
  summarise(mean=mean(tp_change), se=se(tp_change))

ggplot(ratios) +
  geom_point(aes(x=factor(1),
                 y=tp_change),
             position=position_jitter(width=0.1),
             colour='pink',
             size=1.5) +
  geom_hline(yintercept=0,
             linetype='dashed') +
  geom_pointrange(data=ratios_sry,
                  aes(x=factor(1), 
                      y=mean, 
                      ymin = mean-2*se,
                      ymax = mean+2*se
                  ), 
                  size=0.7) +
  coord_cartesian(
    ylim=c(-20, 20)) +
  theme_bw() + 
  theme(panel.grid=element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(size=0.2, color = 'black'),
        axis.text.x = element_text(color='white'),
        axis.ticks.x = element_blank(),
        plot.title = element_text(hjust = 0.5),
        aspect.ratio = 1) +
  labs(x='',
       y='% increase in TP')

ggsave(
  "freyas_outputs/res_A2_pc_change.png",
  width = 4,
  height = 4,
  dpi = 500
)

# plot question G results

# load in results files
sry <- read.csv('freyas_outputs/sry_tps_G.csv',
                stringsAsFactors = FALSE)

full <- read.csv('freyas_outputs/full_tps_G.csv',
                 stringsAsFactors = FALSE)

full_plot <- full %>% 
  mutate(isolation_days_vax=case_when(
    isolation_days_vax==7 ~ '7 days',
    isolation_days_vax==14 ~ '14 days'
  ))

# make scatterplot of tps
sry <- full_plot %>% 
  group_by(
    isolation_days_vax) %>% 
  summarise(mean=mean(TP), se=se(TP)) 

ggplot(full_plot) +
  geom_point(aes(x=factor(isolation_days_vax),
                 y=TP),
             position=position_jitter(width=0.1),
             colour='grey70',
             size=1) +
  geom_pointrange(data = sry,
                  aes(x=factor(isolation_days_vax), 
                      y=mean, 
                      ymin = mean-2*se,
                      ymax = mean+2*se
                  ), 
                  size=0.5) +
  theme_bw() + 
  theme(panel.grid=element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(size=0.2, color = 'black'),
        plot.title = element_text(hjust = 0.5),
        aspect.ratio = 1) +
  labs(x='Duration of isolation for vaccinated cases',
       y='Transmission potential')


ggsave(
  "freyas_outputs/res_G_tps.png",
  width = 4,
  height = 4,
  dpi = 500
)

# make ratio plot

ratios <- full %>% 
  group_by(simulation) %>% 
  summarise(tp_change=get_pc_change(TP, isolation_days_vax))

ratios_sry <- ratios %>% 
  summarise(mean=mean(tp_change), se=se(tp_change))

ggplot(ratios) +
  geom_point(aes(x=factor(1),
                 y=tp_change),
             position=position_jitter(width=0.1),
             colour='pink',
             size=1.5) +
  geom_hline(yintercept=0,
             linetype='dashed') +
  geom_pointrange(data=ratios_sry,
                  aes(x=factor(1), 
                      y=mean, 
                      ymin = mean-2*se,
                      ymax = mean+2*se
                  ), 
                  size=0.6) +
  coord_cartesian(
    ylim=c(-20, 20)) +
  theme_bw() + 
  theme(panel.grid=element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(size=0.2, color = 'black'),
        axis.text.x = element_text(color='white'),
        axis.ticks.x = element_blank(),
        plot.title = element_text(hjust = 0.5),
        aspect.ratio = 1) +
  labs(x='',
       y='% increase in TP')

ggsave(
  "freyas_outputs/res_G_pc_change.png",
  width = 4,
  height = 4,
  dpi = 500
)



