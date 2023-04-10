pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, fixest)

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

fig2 <- final.data2 %>% filter(year == '2012' | year == '2019' & !is.na(expand_ever)) %>%
  ggplot(aes(x = insured)) +
  geom_bar(aes(fill = as.factor(perc_dir)), position = "dodge")





fig_2 <- ggplot(tab_2, aes(x = Year)) + 
  geom_line(aes(Year, avg_price), colour = "red") + 
  geom_line(aes(Year, avg_tax), colour = 'blue') +
  labs(title = "Average Price and Tax from 1970-2018", x = "Year", y = "Average Price and Tax") +
  annotate("text", x = 2016, y = 7.2, label = "Average Price", colour = "black", size = 3) +
  annotate("text", x = 2016, y = 3.2, label = "Average Tax", colour = "black", size = 3) +
  theme_bw()
  
  
ratings.fig <- final.data2 %>% filter(year == '2009'| year == '2012'| year == '2015' & !is.na(Star_Rating)) %>%
  ggplot(aes(x = as.factor(Star_Rating))) +
  geom_bar(aes(fill = as.factor(year)), position = "dodge") +
  scale_fill_grey() + 
  labs(title = "Distribution of Star Ratings", x = "Star Ratings", y = "Count of Plans", fill="Year") +
  theme_bw()  
  


















#final.data %>% group_by(expand_year, State) %>% filter(!is.na(expand_ever)) %>%
  


share.fig <- filtered_data2 %>% 
  ggplot(aes(x = as.factor(year), y = avg_share, group=1)) + 
  stat_summary(fun.y = "mean", geom = "line", na.rm = TRUE) 

ggplot(final.data, aes(x = year)) +
  geom_bar(aes(fill = insured)) +
  scale_y_continuous(labels = percent_format())

final.data %>% filter(!is.na(expand_ever)) %>%
  group_by(expand_year) %>% 
  select(expand_year,insured, uninsured, adult_pop) %>% 
  ggplot(aes(x = as.factor(expand_year))) +
  geom_bar(aes(fill = insured), position = "dodge") +
  scale_fill_grey()
  

ggplot(final.data, aes(x = expand_year)) +
  geom_line(aes(expand_year, ins_employer), colour = "red") +
  

fig_2 <- ggplot(tab_2, aes(x = Year)) + 
    geom_line(aes(Year, avg_price), colour = "red") + 
    geom_line(aes(Year, avg_tax), colour = 'blue') +
    labs(title = "Average Price and Tax from 1970-2018", x = "Year", y = "Average Price and Tax") +
    annotate("text", x = 2016, y = 7.2, label = "Average Price", colour = "black", size = 3) +
    annotate("text", x = 2016, y = 3.2, label = "Average Tax", colour = "black", size = 3) +
    theme_bw()
