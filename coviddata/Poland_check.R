library(eurostat)
library(dplyr)
library(ggplot2)

date_in_week <- function(year, week, weekday){
  as.Date(paste(as.character(2020), "-01-01", sep="")) + week * 7 - as.numeric(format(as.Date(paste(as.character(2020), "-01-01", sep="")), "%w")) +weekday-1
}

dat <- get_eurostat("demo_r_mwk_ts", cache=FALSE)


dat %>%
  mutate(year=as.numeric(substr(time,1,4)), week=as.numeric(substr(time,6,8))) %>%
  filter(year==2020 & week<99 & sex=='T') %>%
  group_by(geo) %>% filter(week == max(week)) %>% 
  filter(geo=='PL'|geo=='SE') %>%
  mutate(date=date_in_week(year=year, week=week, weekday=7))

plotpl<-dat%>%
  filter(geo=='PL')%>%
  janitor::clean_names()%>%
  mutate(year=as.numeric(substr(time,1,4)), week=as.numeric(substr(time,6,8))) %>%
  filter(geo=='PL', sex=='T', year>2000) %>%
  rename('deaths'='values')%>%
  select(year, week, deaths)%>%
  mutate(thisyear = (year == 2020)) %>%
  ggplot(aes(x=week, y=deaths, group=year)) + 
  geom_line(aes(col=thisyear)) +
  scale_color_manual(values=c("FALSE"='gray',"TRUE"='blue')) +
  guides(col=FALSE) +
  ggtitle(paste("Polska - zgony tygodniowo; \nniebieski - 2020, szary - poprzednie lata"))+
  ylab("liczba zgonów") +
  xlab("tydzień")+
  geom_blank(aes(y = 0)) +
  scale_y_continuous(labels= scales::comma)

plotpl


plotpl2<-dat%>%
  janitor::clean_names()%>%
  mutate(year=as.numeric(substr(time,1,4)), week=as.numeric(substr(time,6,8))) %>%
  filter(geo=='PL', sex=='T', year>2000) %>%
  rename('deaths'='values')%>%
  select(year, week, deaths)%>%
  mutate(year_group = ifelse(year == 2020,'2','0')) %>%
  mutate(year_group = ifelse(year == 2019,'1',year_group)) %>%
  ggplot(aes(x=week, y=deaths, group=year)) + 
    geom_line(aes(col=year_group)) +
  scale_color_manual(values=c('0'='gray','1'='green', '2'='blue')) +
  guides(col=FALSE) +
  ggtitle(paste("Polska - zgony tygoniowo; \nniebieski - 2020, zielony - 2019 \nszary - poprzednie lata"))+
  ylab("liczba zgonów") +
  xlab("tydzień")+
  geom_blank(aes(y = 0)) +
  scale_y_continuous(labels= scales::comma)

plotpl2

#Poland vs Sweden

dat%>%
  janitor::clean_names()%>%
  mutate(year=as.numeric(substr(time,1,4)), week=as.numeric(substr(time,6,8))) %>%
  filter(geo %in% c('PL', 'SE'), sex=='T', year>2000, week<99, year<2020 | week<41) %>%
  rename('deaths'='values')%>%
  select(year, week, geo, deaths)%>%
  mutate(year_group = ifelse(year == 2020,'2','0')) %>%
  mutate(year_group = ifelse(year == 2019,'1',year_group)) %>%
  ggplot(aes(x=week, y=deaths, group=year)) + 
  facet_wrap(~ geo, scales='free_y') +
  geom_line(aes(col=year_group)) +
  scale_color_manual(values=c('0'='gray','1'='green', '2'='blue')) +
  guides(col=FALSE) +
  ggtitle(paste("zgony tygodniowo; niebieski - 2020, zielony - 2019, szary - poprzednie lata"))+
  ylab("liczba zgonów") +
  xlab("tydzień")+
  geom_blank(aes(y = 0)) +
  scale_y_continuous(labels= scales::comma)
