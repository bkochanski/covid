library(dplyr)
library(tidyr)
library(ggplot2)
library(Cairo)

#stmf <- readr::read_csv("https://www.mortality.org/Public/STMF/Outputs/stmf.csv", skip=1)
#saveRDS(stmf, "stmfsaved2")

stmf<-readRDS("stmfsaved2")

deaths <- stmf %>%
  janitor::clean_names() %>%
  select(country_code:d_total) %>%
  pivot_longer(5:10,
               names_to = "age", values_to = "deaths",
               names_pattern = "[d_]*([a-z0-9_p]*)"
  ) %>%
  filter(age == "total", sex == "b") %>%
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
                     FRATNP="Francja")
  ) %>%
  select(year, week, country, deaths) %>%
  group_by(country) %>%
  mutate(mean_deaths = mean(deaths, na.rm=TRUE))



#deaths [deaths$country %in% c("Polska", "Szwecja"),] %>%
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




