deaths [deaths$country %in% c("Stany Zjednoczone"),] %>%
  #plot1<-deaths%>%
  mutate(thisyear = (year == 2020)) %>%
  filter(year<2020 | week<31) %>%
  ggplot(aes(x=week, y=deaths, group=year)) + 
  geom_line(aes(col=thisyear)) +
  scale_color_manual(values=c("FALSE"='gray',"TRUE"='blue')) +
  guides(col=FALSE) +
  ggtitle("Liczba zgonów tygodniowo - rok 2020 na niebiesko")+
  ylab("liczba zgonów") +
  xlab("tydzień")+
geom_blank(aes(y = 0)) 
