library(dplyr)
library(tidyr)
library(ggplot2)
library(Cairo)
library(lubridate)

#stmf <- readr::read_csv("https://www.mortality.org/Public/STMF/Outputs/stmf.csv", skip=1)
#saveRDS(stmf, "stmfsaved8")

stmf<-readRDS("stmfsaved8")

mingbrweek<-stmf[startsWith(stmf$CountryCode,'GBR') & stmf$Sex=='b'&stmf$Year==2020,]%>% group_by(CountryCode)%>%
  summarize(maxweek=max(Week))%>%
  select(maxweek)%>%
  summarize(min(maxweek))%>%
  as.numeric()

#summing up UK
gbr<-stmf[startsWith(stmf$CountryCode,'GBR') & stmf$Sex=='b' & (stmf$Week<=mingbrweek|stmf$Year<2020),] %>%  
  mutate(year=Year, week=Week)%>%
  group_by(year, week)%>%
  summarise(deaths=sum(DTotal))%>%
  mutate(country_code='GBR')%>%
  filter(year>2009)%>%
  mutate(country='Wielka Brytania')%>%
  select(year, week, country, country_code, deaths)

deaths <- stmf %>%
  janitor::clean_names() %>%
  select(country_code:d_total) %>%
  pivot_longer(5:10,
               names_to = "age", values_to = "deaths",
               names_pattern = "[d_]*([a-z0-9_p]*)"
  ) %>%
  filter(age == "total", sex == "b") %>%
  filter(!country_code %in% c('GBRTENW', 'GBR_SCO', 'GBR_NIR'))%>%
  mutate(
    country = recode(country_code,
                     AUT = "Austria",
                     BEL = "Belgia",
                     DEUTNP = "Niemcy",
                     DNK = "Dania",
                     ESP = "Hiszpania",
                     FIN = "Finlandia",
                     GBRTENW = "Anglia i Walia",
                     ISL = "Islandia",
                     NLD = "Holandia",
                     NOR = "Norwegia",
                     PRT = "Portugalia",
                     SWE = "Szwecja",
                     USA = "Stany Zjednoczone",
                     POL ="Polska",
                     BGR="Bułgaria",
                     GBR_SCO="Szkocja",
                     CZE="Czechy",
                     LVA="Łotwa",
                     RUS="Rosja",
                     CHE="Szwajcaria",
                     ISR="Izrael",
                     LTU="Litwa", 
                     ITA="Włochy",
                     HRV="Chorwacja",
                     HUN="Węgry",
                     EST="Estonia",
                     SVK="Słowacja", 
                     LUX="Luksemburg",
                     SVN="Słowenia",
                     GRC="Grecja",
                     FRATNP="Francja",
                     GBR_NIR = "Irlandia Północna",
                     KOR="Korea",
                     NZL_NP="Nowa Zelandia", 
                     CHL="Chile",
                     CAN="Kanada")
  ) %>%
  select(year, week, country, country_code, deaths) %>%
  rbind(list(2020, 41, "Polska","POL", 9164)) %>%
  rbind(list(2020, 42, "Polska","POL", 10222)) %>%
  rbind(list(2020, 43, "Polska","POL", 12318)) %>%
  rbind(list(2020, 44, "Polska","POL", 14634)) %>%
  bind_rows(gbr) %>%
  group_by(country) %>%
  mutate(mean_deaths = mean(deaths, na.rm=TRUE))

#weeks 41-44 added from money.pl
#https://infogram.com/liczba-zgonow-w-dobie-epidemii-1h7v4pwnw1kq86k
#https://www.money.pl/gospodarka/i-znowu-rekord-liczba-ukrytych-ofiar-epidemii-wciaz-rosnie-6573778690960288a.html


#deaths [deaths$country %in% c("Stany Zjednoczone"),] %>%
plot1<-deaths%>%
  mutate(thisyear = (year == 2020)) %>%
  ggplot(aes(x=week, y=deaths, group=year)) + 
  geom_line(aes(col=thisyear)) +
  facet_wrap(~ country, scales='free_y') +
  scale_color_manual(values=c("FALSE"='gray',"TRUE"='blue')) +
  guides(col=FALSE) +
  ggtitle("Liczba zgonów tygodniowo - rok 2020 na niebiesko")+
  ylab("liczba zgonów") +
  xlab("tydzień")

print(plot1)

ggsave(filename="plot1.pdf", plot=plot1+theme(plot.margin=unit(c(1,1,1,1),"cm")), width = 297, height = 210, units = "mm")

plot2<-deaths%>%
  mutate(thisyear = (year == 2020)) %>%
  ggplot(aes(x=week, y=deaths, group=year)) + 
  geom_line(aes(col=thisyear)) +
  facet_wrap(~ country, scales='free_y') +
  scale_color_manual(values=c("FALSE"='gray',"TRUE"='blue')) +
  guides(col=FALSE) +
  ggtitle("Liczba zgonów tygodniowo - rok 2020 na niebiesko, pozostałe lata na szaro")+
  ylab("liczba zgonów") +
  xlab("tydzień") + 
  geom_blank(aes(y = 0)) +
  geom_blank(aes(y = 2.75*mean_deaths)) +
  scale_y_continuous(labels= scales::comma)

print(plot2)

ggsave(filename="plot2.pdf", plot=plot2+theme(plot.margin=unit(c(1,1,1,1),"cm")), width = 297, height = 210, units = "mm", device=cairo_pdf)


plot3<-deaths[deaths$country_code=='POL',]%>%
  mutate(thisyear = (year == 2020)) %>%
  ggplot(aes(x=week, y=deaths, group=year)) + 
  geom_line(aes(col=thisyear)) +
  scale_color_manual(values=c("FALSE"='gray',"TRUE"='blue')) +
  guides(col=FALSE) +
  ggtitle("Zgony tygodniowo - Polska \n2020 na niebiesko, pozostałe lata na szaro")+
  ylab("liczba zgonów") +
  xlab("tydzień") + 
  geom_blank(aes(y = 0)) +
  geom_blank(aes(y = 2*mean_deaths)) +
  scale_y_continuous(labels= scales::comma)

print(plot3)

ggsave(filename="plot3.pdf", plot=plot3+theme(plot.margin=unit(c(1,1,1,1),"cm")), width = 297, height = 210, units = "mm", device=cairo_pdf)


recent_deaths <- deaths %>%
  filter(year >= 2015 & year <= 2019) %>%
  group_by(country,week) %>%
  summarise(median_deaths = median(deaths)) %>%
  ungroup()
excess_deaths <- deaths %>%
  filter(year >= 2015) %>%
  left_join(recent_deaths) %>%
  mutate(excess = deaths - median_deaths)
plot4<-excess_deaths %>%
  mutate(thisyear = (year == 2020)) %>%
  ggplot(aes(x=week, y=excess, group=year)) +
  geom_hline(yintercept=0, col='gray') +
  geom_line(aes(col=thisyear)) +
  facet_wrap(~ country, scales='free_y') +
  scale_color_manual(values=c("FALSE"='gray',"TRUE"='red')) +
  guides(col=FALSE) +
  ggtitle("Weekly excess deaths")

print(plot4)

ggsave(filename="plot4.pdf", plot=plot4+theme(plot.margin=unit(c(1,1,1,1),"cm")), width = 297, height = 210, units = "mm", device=cairo_pdf)


# owid <- readr::read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv", col_types='fffDnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn')
#saveRDS(owid, "owidsaved2")

owid<-readRDS("owidsaved2")

selected_countries<-c('POL', 'BEL', 'NLD', 'SWE', 'FRA', 'FRATNP', 'ESP', 'ITA', 'CAN', 'USA', 'CHL', 'ISR', 'DEU', 'DEUTNP', 'CHE', 'GBR')

owid2<-owid%>%
  filter(iso_code %in% selected_countries 
         & year(date)==2020)%>%
  select(iso_code, location, date, new_deaths)%>%
  mutate(year=year(date), week=week(date))%>%
  group_by(iso_code, year, week)%>%
  summarise(deaths=sum(new_deaths))%>%
  mutate(
    country = recode(iso_code,
                     AUT = "Austria",
                     BEL = "Belgia",
                     DEU = "Niemcy",
                     DNK = "Dania",
                     ESP = "Hiszpania",
                     FIN = "Finlandia",
                     GBRTENW = "Anglia i Walia",
                     ISL = "Islandia",
                     NLD = "Holandia",
                     NOR = "Norwegia",
                     PRT = "Portugalia",
                     SWE = "Szwecja",
                     USA = "Stany Zjednoczone",
                     POL ="Polska",
                     BGR="Bułgaria",
                     GBR_SCO="Szkocja",
                     CZE="Czechy",
                     LVA="Łotwa",
                     RUS="Rosja",
                     CHE="Szwajcaria",
                     ISR="Izrael",
                     LTU="Litwa", 
                     ITA="Włochy",
                     HRV="Chorwacja",
                     HUN="Węgry",
                     EST="Estonia",
                     SVK="Słowacja", 
                     LUX="Luksemburg",
                     SVN="Słowenia",
                     GRC="Grecja",
                     FRA="Francja",
                     GBR_NIR = "Irlandia Północna",
                     KOR="Korea",
                     NZL_NP="Nowa Zelandia", 
                     CHL="Chile",
                     CAN="Kanada",
                     GBR='Wielka Brytania'))


plot5<-excess_deaths %>%
  filter(country_code %in% selected_countries)%>%
  mutate(thisyear = (year == 2020)) %>%
  ggplot(aes(x=week, y=excess, group=year)) +
  geom_area(data=owid2, aes(x=week, y=deaths), fill='yellow', col='orange') +
  geom_hline(yintercept=0, col='gray') +
  geom_line(aes(col=thisyear)) +
  facet_wrap(~ country, scales='free_y') +
  scale_color_manual(values=c("FALSE"='gray',"TRUE"='red')) +
  guides(col=FALSE) +
  ggtitle("Nadmiarowe zgony w 2020 (czerwony) \noraz zgony z Covid19 (żółty) w kolejnych tygodniach")+
  ylab("liczba zgonów") +
  xlab("tydzień")

print(plot5)

ggsave(filename="plot5.pdf", plot=plot5+theme(plot.margin=unit(c(1,1,1,1),"cm")), width = 297, height = 210, units = "mm", device=cairo_pdf)


#dla Polski
plot6<-excess_deaths %>%
  filter(country_code=='POL')%>%
  mutate(thisyear = (year == 2020)) %>%
  ggplot(aes(x=week, y=excess, group=year)) +
  geom_area(data=owid2[owid2$iso_code=='POL',], aes(x=week, y=deaths), fill='yellow', col='orange') +
  geom_hline(yintercept=0, col='gray') +
  geom_line(aes(col=thisyear)) +
  facet_wrap(~ country, scales='free_y') +
  scale_color_manual(values=c("FALSE"='gray',"TRUE"='red')) +
  guides(col=FALSE) +
  ggtitle("Nadmiarowe zgony w 2020 (czerwony) \noraz zgony z Covid19 (żółty) w kolejnych tygodniach") +
  ylab("liczba zgonów") +
  xlab("tydzień")
  

print(plot6)

ggsave(filename="plot6.pdf", plot=plot3+theme(plot.margin=unit(c(1,1,1,1),"cm")), width = 297, height = 210, units = "mm", device=cairo_pdf)

