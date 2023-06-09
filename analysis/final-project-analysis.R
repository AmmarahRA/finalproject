pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, fixest, psych)

final.data <- read.delim('data/output/acs_medicaid.txt')
health_data <- read.delim('data/output/health_data.txt')

#Data Summary 
final.data <- final.data %>% mutate(insured = ins_employer + ins_direct +
                                                    ins_medicaid + ins_medicare)
final.data <- final.data %>% mutate(perc_dir = (ins_direct/adult_pop)*100,
                                    perc_empl = (ins_employer/adult_pop)*100,
                                    perc_mcaid = (ins_medicaid/adult_pop)*100,
                                    perc_ins = (insured/adult_pop)*100,
                                    perc_unins = (uninsured/adult_pop)*100)

#share of uninsured population amongst adult population 

fig1 <- final.data %>% group_by(year) %>% 
  summarise(avg_unins = mean(perc_unins)) %>%
  ggplot(aes(x = year, y = avg_unins)) + 
  geom_line() + geom_point() + 
  labs(x = "Year", y = "Percentage Uninsured") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 
fig1

#share of insurance distribution 

fig2 <- final.data %>% group_by(year) %>%
  summarise(avg_dir = mean(perc_dir),
            avg_empl = mean(perc_empl),
            avg_mcaid = mean(perc_mcaid),
            avg_ins = mean(perc_ins)) %>%
  pivot_longer(cols = c(avg_dir, avg_empl, avg_mcaid, avg_ins),
               names_to = "category",
               values_to = "percentage") %>%
  ggplot(aes(x = year, y = percentage, fill = category)) +
  geom_col(position = "dodge", width = 0.8) +
  labs(x = "Year",
       y = "Percentage Insured", fill = "Insurance Type") +
  scale_fill_discrete(name = "Insurance Type",
                      breaks = c('avg_dir', 'avg_empl', 'avg_mcaid', 'avg_ins'),
                      labels = c("Direct Purchase", "Employer Provided", "Medicaid", "Total Insured")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))  
fig2

#health status before and after medicaid

fig3<- health_data %>% filter(GENHLTH != '9') %>%
  ggplot(aes(x = as.factor(GENHLTH))) +
  geom_bar(aes(fill = as.factor(time)), position = "dodge") +
  scale_fill_discrete(name = "Medicaid Expansion (2014)",
                      labels = c("Before", "After")) +
  labs(x = "Health Status Rating",
       y = "Number of People") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 
fig3

#health plan, medical costs and health status

fig4<- health_data %>% filter(GENHLTH != '9' & HLTHPLN1 == '1' | HLTHPLN1 == '2') %>%
  ggplot(aes(x = as.factor(GENHLTH))) +
  geom_bar(aes(fill = as.factor(HLTHPLN1)), position = "dodge") +
  scale_fill_discrete(name = "Health Coverage",
                      labels = c("Yes", "No")) +
  labs(x = "Health Status Rating",
       y = "Number of People") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 
fig4

#summary stats of insurance

sum_stat <- describe(final.data[ , c('ins_employer', 'ins_direct', 'ins_medicaid', 
                                     'ins_medicare', 'uninsured')])
sum_stat <- sum_stat %>% select(n, mean, sd, min, max)

rownames(sum_stat) <- c("Employer-Provided", "Direct Purchase", "Medicaid", "Medicare", "Uninsured")
emp_mean<- as.integer(sum_stat$mean[1])
maid_mean<-as.integer(sum_stat$mean[3])

#health status according to medicaid expansion and health plan

avg_health1 <- health_data %>% filter(GENHLTH != '9' & time == '0') %>% 
  mutate(avg_health = mean(GENHLTH),
         sd_health = sd(GENHLTH),
         ci_low = avg_health - (1.96 * sqrt(sd_health^2/(obs_nm-1))),
         ci_high = avg_health + (1.96 * sqrt(sd_health^2/(obs_nm-1)))) %>% 
  dplyr::select(time, avg_health, sd_health, ci_low, ci_high)

avg_health2 <- health_data %>% filter(GENHLTH != '9' & time == '1') %>% 
  mutate(avg_health = mean(GENHLTH),
         sd_health = sd(GENHLTH),
         ci_low = avg_health - (1.96 * sqrt(sd_health^2/(obs_m-1))),
         ci_high = avg_health + (1.96 * sqrt(sd_health^2/(obs_m-1)))) %>% 
  dplyr::select(time, avg_health, sd_health, ci_low, ci_high)

obs_nm <- as.numeric(count(avg_health1 %>% ungroup()))
obs_m <- as.numeric(count(avg_health2 %>% ungroup()))

health_stats <- data.frame(Medicaid = c("Yes", "No"),
                           obs = c(obs_m, obs_nm),
                           avg_rating = c(avg_health2$avg_health[1], avg_health1$avg_health[1]),
                           sd_rating = c(avg_health2$sd_health[1], avg_health1$sd_health[1]),
                           ci_low = c(avg_health2$ci_low[1], avg_health2$ci_low[1]),
                           ci_high = c(avg_health2$ci_high[1], avg_health2$ci_high[1]))

# difference between medicare expanded and not expanded states
tab_ne <- final.data %>% filter(expand_ever == 'FALSE') %>%
  group_by(State) %>% summarise(avg_ins = mean(perc_ins),
                                avg_unins = mean(perc_unins),
                                avg_dir = mean(perc_dir),
                                avg_empl = mean(perc_empl),
                                avg_mcaid = mean(perc_mcaid)) %>%
  dplyr::select(State, avg_ins, avg_unins, avg_dir, avg_empl, avg_mcaid)
tab_ne <- rbind(tab_ne, data.frame(State='Total Average', t(colMeans(tab_ne[, -1]))))
tab_ne
          
tab_e <- final.data %>% filter(expand_ever == 'TRUE') %>%
  group_by(State) %>% summarise(avg_ins = mean(perc_ins),
                                avg_unins = mean(perc_unins),
                                avg_dir = mean(perc_dir),
                                avg_empl = mean(perc_empl),
                                avg_mcaid = mean(perc_mcaid)) %>%
  dplyr::select(State, avg_ins, avg_unins, avg_dir, avg_empl, avg_mcaid)
tab_e <- rbind(tab_e, data.frame(State='Total Average', t(colMeans(tab_e[, -1]))))
tab_e

save.image("finalproject.Rdata")

