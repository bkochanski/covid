library(eurostat)

dat <- get_eurostat("demo_r_mwk_ts")
dat2 <- get_eurostat("demo_r_mwk2_ts")
dat3 <- get_eurostat("demo_r_mwk3_ts")
pop <- get_eurostat("demo_pjangroup")
pop2 <- get_eurostat("demo_r_pjangroup")
pop3 <- get_eurostat("demo_r_pjangrp3")

dat20 <- get_eurostat("demo_r_mwk_20")
dat10 <- get_eurostat("demo_r_mwk_10")
dat5 <- get_eurostat("demo_r_mwk_5")

library(dplyr)

#View(dat3%>%filter(geo=='UKI', sex=='T'))
#View(pop3%>%filter(geo=='UKI', sex=='T', age=='TOTAL', time=='2019-01-01'))

popUKI<-(pop3%>%filter(geo=='UKI', sex=='T', age=='TOTAL', time=='2019-01-01'))$values

test<-(dat3%>%filter(geo=='UKI', sex=='T')%>%select(time, values)%>%arrange(time)%>%mutate(values=values/popUKI*1000*365/7))

plot_age_groups_by_country<-function(country_now) {

dat10_b<-dat10%>%
  janitor::clean_names()%>%
  mutate(year=as.numeric(substr(time,1,4)), week=as.numeric(substr(time,6,8))) %>%
  filter(geo==country_now[1], sex=='T', age!='UNK', age!='Y_GE80') %>%
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
  group_by(age) %>%
  mutate(mean_deaths = mean(deaths, na.rm=TRUE))

ymax<-max(dat10_b$deaths/dat10_b$mean_deaths)  

plot_age_groups<-dat10_b%>%  
  mutate(thisyear = (year == 2020)) %>%
  ggplot(aes(x=week, y=deaths, group=year)) + 
  geom_line(aes(col=thisyear)) +
  facet_wrap(~ age, scales='free_y') +
  scale_color_manual(values=c("FALSE"='gray',"TRUE"='blue')) +
  guides(col=FALSE) +
  ggtitle(paste(country_now[2], "- fala zgonów według grup wiekowych"))+
  ylab("liczba zgonów") +
  xlab("tydzień")+
  geom_blank(aes(y = 0)) +
  geom_blank(aes(y = ymax*mean_deaths)) +
  scale_y_continuous(labels= scales::comma)

print(plot_age_groups)

ggsave(filename=paste("plot_age_groups_", country_now[1], ".pdf", sep=''), plot=plot_age_groups+theme(plot.margin=unit(c(1,1,1,1),"cm")), width = 297, height = 210, units = "mm", device=cairo_pdf)
}


plot_age_groups_by_country(c('ES', 'Hiszpania'))
plot_age_groups_by_country(c('IT', 'Włochy'))
plot_age_groups_by_country(c('UK', 'Wielka Brytania'))

