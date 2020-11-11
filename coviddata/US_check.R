
stmf1<-readRDS("stmfsaved")
stmf2<-readRDS("stmfsaved2")
stmf3<-readRDS("stmfsaved3")
stmf4<-readRDS("stmfsaved4")
stmf5<-readRDS("stmfsaved5")
stmf6<-readRDS("stmfsaved6")
stmf7<-readRDS("stmfsaved7")
stmf8<-readRDS("stmfsaved8")

uscheck<-(
rbind(
dplyr::mutate(stmf1[stmf1$CountryCode=='USA'&stmf1$Year==2020&stmf1$Sex=='b',c('Week', 'DTotal')], wersja=1),
dplyr::mutate(stmf2[stmf2$CountryCode=='USA'&stmf2$Year==2020&stmf2$Sex=='b',c('Week', 'DTotal')], wersja=2),
dplyr::mutate(stmf3[stmf3$CountryCode=='USA'&stmf3$Year==2020&stmf3$Sex=='b',c('Week', 'DTotal')], wersja=3),
dplyr::mutate(stmf4[stmf4$CountryCode=='USA'&stmf4$Year==2020&stmf4$Sex=='b',c('Week', 'DTotal')], wersja=4),
dplyr::mutate(stmf5[stmf5$CountryCode=='USA'&stmf5$Year==2020&stmf5$Sex=='b',c('Week', 'DTotal')], wersja=5),
dplyr::mutate(stmf6[stmf6$CountryCode=='USA'&stmf6$Year==2020&stmf6$Sex=='b',c('Week', 'DTotal')], wersja=6),
dplyr::mutate(stmf7[stmf7$CountryCode=='USA'&stmf7$Year==2020&stmf7$Sex=='b',c('Week', 'DTotal')], wersja=7),
dplyr::mutate(stmf8[stmf8$CountryCode=='USA'&stmf8$Year==2020&stmf8$Sex=='b',c('Week', 'DTotal')], wersja=8)

))

library(ggplot2)
ggplot(data=uscheck, mapping=aes(x=Week, y=DTotal, group=as.factor(wersja), col=wersja))+geom_line()+ylim(c(0,80000))+xlab("tydzień")+ylab("zgony")+labs(fill="wersje")
