
download.file("https://stat.gov.pl/download/gfx/portalinformacyjny/pl/defaultaktualnosci/5468/39/2/1/zgony_wedlug_tygodni_v2.zip","zipFile.zip")
znames<-unzip("zipFile.zip")
a<-readxl::read_excel("./Zgony wed\u0088ug tygodni w Polsce_2020.xlsx")
unlink("zipFile.zip")
