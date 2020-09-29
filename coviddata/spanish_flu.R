url <-"https://upload.wikimedia.org/wikipedia/commons/e/e2/Spanish_flu_death_chart.png"
library(ggimage)
library(eurostat)
library(Cairo)
library(dplyr)
library(janitor)
library(ggplot2)

dat2 <- get_eurostat("demo_r_mwk2_ts")
pop2 <- get_eurostat("demo_r_pjangroup")

selection<-c('UKI', 'ES3', 'FR1', 'ITC4')


pop2_b<-pop2%>%
  filter(geo %in% selection, sex=='T', age=='TOTAL', time=='2019-01-01') %>%
  mutate(geo=as.character(geo))%>%
  rename('population'='values')%>%
  select(geo, population)


dat2_b<-dat2%>%
  janitor::clean_names()%>%
  mutate(year=as.numeric(substr(time,1,4)), week=as.numeric(substr(time,6,8)), geo=as.character(geo)) %>%
  filter(geo %in% selection, sex=='T', year==2020) %>%
  rename('deaths'='values')%>%
  select(geo, year, week, deaths)%>%
  full_join(pop2_b, by=c('geo'))%>%
  mutate(mortality=deaths/population/7*365*1000)%>%
  mutate(
    region = recode(geo,
                     UKI = "London",
                     ES3 = "Madrid",
                     FR1 = "Paris",
                     ITC4 = "Lombardy"))


xupperlimit<-40
g<-
  dat2_b%>%ggplot(aes(x=week, y=mortality, col=region))+geom_line(lwd=2)+geom_point(lwd=4)+
  theme_classic()+theme(legend.position = c(0.8, 0.4))+ 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 120)) +
  scale_x_continuous(expand = c(0, 0), breaks=1:xupperlimit, limits=c(0,xupperlimit)) + xlab("weeks of 2020")+
  annotate(geom="text", x=13, y=70, label="Mortality in selected regions in 2020",
           color="red", size=6)
g2<-ggbackground(g, url)
g2
ggsave("covid_vs_spanish_flu.png", g2, width = 8.42, height = 5.51)
