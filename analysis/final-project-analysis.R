pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, fixest, psych)

final.data <- read.delim('data/output/acs_medicaid.txt')

#Data Summary 
final.data <- final.data %>% mutate(insured = ins_employer + ins_direct +
                                                    ins_medicaid + ins_medicare)

#share of uninsured population amongst adult population 
fig1 <- final.data %>% group_by(year) %>% 
  summarise(avgs_unins = mean((uninsured/adult_pop)*100)) %>%
  ggplot(aes(x = year, y = avgs_unins)) + 
  geom_line() +
  labs(title = "Share of Uninsured Population", x = "Year", y = "Percentage Uninsured") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 

fig1

#share of insurance distribution 

fig2 <- final.data2 %>% group_by(year) %>% 
  summarise(avg_dir = mean(perc_dir),
            avg_empl = mean(perc_empl),
            avg_mcaid = mean(perc_mcaid),
            avg_ins = mean((insured/adult_pop)*100)) %>%
  ggplot(aes(x = year)) +
  geom_line(aes(x = year, y = avg_dir), colour = "blue") +
  geom_line(aes(x = year, y = avg_empl), colour = "red") +
  geom_line(aes(x = year, y = avg_mcaid), colour = "black") +
  geom_line(aes(x = year, y = avg_ins), colour = "green") +
  labs(title = "Share of Insurance Distribution ", x = "Year", y = "Percentage Insured") +
  annotate("text", x = 2018, y = 7.2, label = "Direct Purchase", colour = "black", size = 3) +
  annotate("text", x = 2018, y = 70.2, label = "Employer Provided", colour = "black", size = 3) +
  annotate("text", x = 2018, y = 18.2, label = "Medicaid", colour = "black", size = 3) +
  annotate("text", x = 2018, y = 80.2, label = "Total Insured", colour = "black", size = 3) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 

fig2

#summary stats

sum_stat <- describe(final.data[ , c('ins_employer', 'ins_direct', 'ins_medicaid', 'ins_medicare', 'uninsured')])
sum_stat <- sum_stat %>% select(n, mean, sd, min, max)








