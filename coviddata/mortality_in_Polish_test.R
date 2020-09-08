deaths1564 <- stmf %>%
  janitor::clean_names() %>%
  select(country_code:d_total) %>%
  pivot_longer(5:10,
               names_to = "age", values_to = "deaths",
               names_pattern = "[d_]*([a-z0-9_p]*)"
  ) %>%
  filter(age == "15_64", sex == "b") %>%
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

plot3<-deaths1564%>%
  mutate(thisyear = (year == 2020)) %>%
  ggplot(aes(x=week, y=deaths, group=year)) + 
  geom_line(aes(col=thisyear)) +
  facet_wrap(~ country, scales='free_y') +
  scale_color_manual(values=c("FALSE"='gray',"TRUE"='blue')) +
  guides(col=FALSE) +
  ggtitle("Liczba zgonów osób w wieku 15-64 lata tygodniowo - rok 2020 na niebiesko, pozostałe lata na szaro")+
  ylab("liczba zgonów") +
  xlab("tydzień") + 
  geom_blank(aes(y = 0)) +
  geom_blank(aes(y = 2.75*mean_deaths)) +
  scale_y_continuous(labels= scales::comma)

print(plot3)

ggsave(filename="plot3.pdf", plot=plot3+theme(plot.margin=unit(c(1,1,1,1),"cm")), width = 297, height = 210, units = "mm", device=cairo_pdf)

