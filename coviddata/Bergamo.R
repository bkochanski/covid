library(eurostat)
library(Cairo)
library(dplyr)
library(janitor)
library(ggplot2)

dat3 <- get_eurostat("demo_r_mwk3_ts")

dat3_b<-dat3%>%
  janitor::clean_names()%>%
  mutate(year=as.numeric(substr(time,1,4)), week=as.numeric(substr(time,6,8))) %>%
  filter(geo=='ITC46', sex=='T', year>2010) %>%
  rename('deaths'='values')%>%
  select(year, week, deaths)

plot1<-dat3_b%>%  
  mutate(thisyear = (year == 2020)) %>%
  ggplot(aes(x=week, y=deaths, group=year)) + 
  geom_line(aes(col=thisyear)) +
  scale_color_manual(values=c("FALSE"='gray',"TRUE"='blue')) +
  guides(col=FALSE) +
  ggtitle(paste("Bergamo - zgony tygoniowo; niebieski - 2020, szary - poprzednie lata"))+
  ylab("liczba zgonów") +
  xlab("tydzień")+
  geom_blank(aes(y = 0)) +
  scale_y_continuous(labels= scales::comma)

plot1

dat310 <- get_eurostat("demo_r_mwk3_10")

dat310_b<-dat310%>%
  janitor::clean_names()%>%
  mutate(year=as.numeric(substr(time,1,4)), week=as.numeric(substr(time,6,8))) %>%
  filter(geo=='ITC46', sex=='T', age!='UNK', age!='Y_GE80', year>2010) %>%
  rename('deaths'='values')%>%
  mutate(age = recode_factor(age, 
                             'TOTAL'='Wszystkie grupy wiekowe', 
                             'Y_LT10'='0-9 lat',
                             'Y10-19'='10-19 lat', 
                             'Y20-29'='20-29 lat', 
                             'Y30-39'='30-39 lat', 
                             'Y40-49'='40-49 lat', 
                             'Y50-59'='50-59 lat', 
                             'Y60-69'='60-69 lat', 
                             'Y70-79'='70-79 lat', 
                             'Y80-89'='80-89 lat', 
                             'Y_GE90'='90+ lat'
  ))%>%
  select(year, week, age, deaths) %>%
  group_by(age) 

plot_age_groups<-dat310_b%>%  
  mutate(thisyear = (year == 2020)) %>%
  ggplot(aes(x=week, y=deaths, group=year)) + 
  geom_line(aes(col=thisyear)) +
  facet_wrap(~ age, scales='free_y') +
  scale_color_manual(values=c("FALSE"='gray',"TRUE"='blue')) +
  guides(col=FALSE) +
  ggtitle(paste("Bergamo - tygodniowa liczba zgonów według grup wiekowych; niebieski - 2020"))+
  ylab("liczba zgonów") +
  xlab("tydzień")+
  geom_blank(aes(y = 0)) +
  geom_blank(aes(y = 13*mean_deaths)) +
  scale_y_continuous(labels= scales::comma)

print(plot_age_groups)

