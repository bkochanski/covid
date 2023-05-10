#Województwa
#Od M. Rogalskiego (bit.ly/covid19-poland)

library(gsheet)
a<-gsheet2tbl('https://docs.google.com/spreadsheets/d/1ierEhD6gcq51HAm433knjnVwey4ZE5DCnu1bW7PRG3E/edit#gid=1841152698')

#View(a)
start<-50 #to się może zmieniać
a[start+17,1]<-'Polska'
daty<-c(paste(t(a[start,2:(dim(a)[2]-1)]),'2020',sep='.'))
daty<-as.Date(daty, format = "%d.%m.%Y")
a2<-data.frame(a[start+1,1], daty, zgony=unname(c(as.numeric(t(a[start+1,2:(dim(a)[2]-1)])))))
for (i in 2:17){
  a2<-rbind(a2, data.frame(a[start+i,1], daty, zgony=unname(c(as.numeric(t(a[start+i,2:(dim(a)[2]-1)]))))))}
names(a2)<-c('location', 'date', 'new_deaths') 
a2$location<-toupper(a2$location)

#Eurostat
library(eurostat)
cache_option<-FALSE
dat2 <- get_eurostat("demo_r_mwk2_ts", cache=cache_option)

wojewodztwa<-c('PL21', 'PL22', 'PL41', 'PL42', 'PL43', 'PL51', 'PL52', 'PL61', 'PL62', 'PL63', 'PL71', 'PL72', 'PL81', 'PL82', 'PL84', 'PL9')

library(dplyr)
datw<-
  dat2%>%
  janitor::clean_names()%>%
  mutate(year=as.numeric(substr(time,1,4)), week=as.numeric(substr(time,6,8))) %>%
  filter(geo %in% wojewodztwa, sex=='T', year>2010) %>%
  rename('deaths'='values')%>%
  select(year, week, geo, deaths) %>%
  mutate(
    location = recode(geo,PL21 = "MAŁOPOLSKIE",
                      PL22 = "ŚLĄSKIE",
                      PL41 = "WIELKOPOLSKIE",
                      PL42 = "ZACHODNIOPOMORSKIE",
                      PL43 = "LUBUSKIE",
                      PL51 = "DOLNOŚLĄSKIE",
                      PL52 = "OPOLSKIE",
                      PL61 = "KUJAWSKO-POMORSKIE",
                      PL62 = "WARMIŃSKO-MAZURSKIE",
                      PL63 = "POMORSKIE",
                      PL71 = "ŁÓDZKIE",
                      PL72 = "ŚWIĘTOKRZYSKIE",
                      PL81 = "LUBELSKIE",
                      PL82 = "PODKARPACKIE",
                      PL84 = "PODLASKIE",
                      PL9 = "MAZOWIECKIE")) %>%
  group_by(geo) %>%
  mutate(mean_deaths = mean(deaths, na.rm=TRUE))

library(ggplot2)

ymaxw<-max(datw$deaths/datw$mean_deaths)  

plotw1<-datw%>%
  mutate(thisyear = (year == 2020)) %>%
  ggplot(aes(x=week, y=deaths, group=year)) + 
  geom_line(aes(col=thisyear)) +
  facet_wrap(~ location, scales='free_y') +
  scale_color_manual(values=c("FALSE"='gray',"TRUE"='blue')) +
  guides(col=FALSE) +
  ggtitle("Liczba zgonów tygodniowo - rok 2020 na niebiesko")+
  ylab("liczba zgonów") +
  xlab("tydzień") + 
  geom_blank(aes(y = 0)) +
  geom_blank(aes(y = ymaxw*mean_deaths)) +
  scale_y_continuous(labels= scales::comma)

print(ymaxw)
print(plotw1)

datw%>%
  group_by(location)%>%
  summarise(m=max(deaths/mean_deaths))%>%
  select(location, m)%>%
  mutate(m=m*100-100)%>%
  arrange(desc(m))

recent_deaths_w <- datw %>%
  filter(year >= 2015 & year <= 2019) %>%
  group_by(location,week) %>%
  summarise(median_deaths = median(deaths)) %>%
  ungroup()
excess_deaths_w <- datw %>%
  filter(year >= 2015) %>%
  left_join(recent_deaths_w) %>%
  mutate(excess = deaths - median_deaths)
plotw4<-excess_deaths_w %>%
  mutate(thisyear = (year == 2020)) %>%
  ggplot(aes(x=week, y=excess, group=year)) +
  geom_hline(yintercept=0, col='gray') +
  geom_line(aes(col=thisyear)) +
  facet_wrap(~ location, scales='free_y') +
  scale_color_manual(values=c("FALSE"='gray',"TRUE"='red')) +
  guides(col=FALSE) +
  ggtitle("Nadmiarowe zgony tygodniowo")+
  ylab("liczba zgonów") +
  xlab("tydzień") + 
  geom_blank(aes(y = 0)) +
  geom_blank(aes(y = (ymaxw-1)*mean_deaths)) +
  scale_y_continuous(labels= scales::comma)

print(plotw4)

library(lubridate)

maxweek<-max(datw$week[datw$year==2020])
#maxweek<-51

owidw<-a2%>%
  filter(location!='POLSKA' & year(date)==2020
         & isoweek(date)<=maxweek)%>%
  select(location, date, new_deaths)%>%
  mutate(year=year(date), week=isoweek(date))%>%
  group_by(location, year, week)%>%
  summarise(deaths=sum(new_deaths))

plot5w<-excess_deaths_w %>%
  mutate(thisyear = (year == 2020)) %>%
  ggplot(aes(x=week, y=excess, group=year)) +
  geom_area(data=owidw, aes(x=week, y=deaths), fill='yellow', col='orange') +
  geom_hline(yintercept=0, col='gray') +
  geom_line(aes(col=thisyear)) +
  facet_wrap(~ location, scales='free_y') +
  scale_color_manual(values=c("FALSE"='gray',"TRUE"='red')) +
  guides(col=FALSE) +
  ggtitle("Nadmiarowe zgony w 2020 (czerwony) \noraz zgony z Covid19 (żółty) w kolejnych tygodniach")+
  ylab("liczba zgonów") +
  xlab("tydzień")+ 
  geom_blank(aes(y = 0)) +
  geom_blank(aes(y = (ymaxw-1)*mean_deaths)) +
  scale_y_continuous(labels= scales::comma)

print(plot5w)

owidw[owidw$year==2020&owidw$week==44,]%>%
  mutate(covid_deaths=deaths)%>%
  select(location, week, covid_deaths)%>%
  left_join(
excess_deaths_w[excess_deaths_w$year==2020&excess_deaths_w$week==44,c('location', 'excess')])%>%
  mutate(rozziew=excess/covid_deaths)%>%
  arrange(rozziew)

