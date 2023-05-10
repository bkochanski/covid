#USA from the economist

a<-read.csv(url('https://raw.githubusercontent.com/TheEconomist/covid-19-excess-deaths-tracker/master/source-data/united-states/united_states_total_source_latest.csv'), stringsAsFactors = FALSE)

a$deaths<-as.numeric(a$TOTAL.DEATHS)
a$year<-ifelse(a$WEEK>=40,substring(a$SEASON,1,4),paste(substring(a$SEASON,1,2),substring(a$SEASON,6,7), sep=""))

#View(a%>%filter(SUB.AREA %in% c("South Dakota", "North Dakota")))
#write.csv(a%>%filter(SUB.AREA %in% c("South Dakota", "North Dakota")), "test2.csv")

library(ggplot2)
library(dplyr)
a%>%
  filter(SUB.AREA %in% c("South Dakota", "North Dakota")) %>%
  filter(year!='2021' | WEEK<0) %>%
  mutate(thisyear = (year == '2020')) %>%
  group_by(SUB.AREA) %>%
  mutate(mean_deaths = mean(deaths, na.rm=TRUE)) %>%
  ungroup()%>%
  ggplot(aes(x=WEEK, y=deaths/mean_deaths*100, group=year)) + 
  geom_line(aes(col=thisyear)) +
  facet_wrap(~ SUB.AREA, scales='free_y') +
  scale_color_manual(values=c("FALSE"='gray',"TRUE"='blue')) +
  guides(col=FALSE) +
  ggtitle("Liczba zgonów tygodniowo - rok 2020 na niebiesko")+
  ylab("liczba zgonów \nw procentach długoterminowej średniej") +
  xlab("tydzień") + 
  geom_blank(aes(y = 0))+
  geom_blank(aes(y = 250))+ 
  scale_y_continuous(labels= scales::comma)


write.csv2(a%>%
            filter(SUB.AREA %in% c("South Dakota", "North Dakota")) %>%
            filter(year!='2021' | WEEK<0) %>%
            mutate(thisyear = (year == '2020')) %>%
            group_by(SUB.AREA) %>%
            mutate(mean_deaths = mean(deaths, na.rm=TRUE)) %>%
            ungroup(), "test1")



