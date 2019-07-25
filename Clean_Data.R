  # ====================================== #
  # =            Clean Data              =
  # =          Daniel Jiménez            =
  # =        Equipo de Producto          =
  # =         Resolve Studio             =
  # =           2019-07-24               =
  # ====================================== #


# Remove past Dataframes
rm(list = ls())

# Libraries ---------------------------------------------

library(tidyverse)
library(lubridate)
library(sqldf)
library(tidyr)

# Import all Datasets -----------------------------------------

getwd()
setwd("Desktop/R5/Input/20190724/")
temp = list.files(pattern="*.csv")
for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i]))

# Modifications Dataset ------------------------------------

participations.csv->participaciones
Demographic.csv->Demografics
Analytics.csv->analytics
parrilla.csv->parrilla
usersForClassification.csv->usersForClassification

rm(list = c("participations.csv","Demographic.csv","Analytics.csv","parrilla.csv","usersForClassification.csv"))

# Cleans, Joins and Machine Learning operations

### Analytics



## Analytics Data Clean Process 





s02 <- sqldf("select adContent ,upper(adContent) as Contenido from analytics")
analytics$adContent<-s02$Contenido ## La variable se convirtió en mayúscula
rm(s02)




prueba<-sqldf("select * ,
              Case
              when adContent like '%COMPRA TU SOAT EN LíNEA%' then 'COMPRA SOAT EN LINEA 24 HORAS'
              when adContent like '%(NOT SET)%' then '(NOT SET)'
              when adContent like '%BANNER-CAMPAÑA-GOOGLE.PNG%' then 'BANNER-CAMPAÑA-GOOGLE.PNG' 
              when adContent like '%BANNER-CAMPAÑA-GOOGLE2.PNG%'then 'BANNER-CAMPAÑA-GOOGLE2.PNG'
              when adContent like '%BANNER-CAMPAÑA-GOOGLE3.PNG%' then 'BANNER-CAMPAÑA-GOOGLE3.PNG'
              when adContent like '%BANNER-CAMPAÑA-GOOGLE4.PNG%' then 'BANNER-CAMPAÑA-GOOGLE4.PNG'
              when adContent like '%BANNER-CAMPAÑA-GOOGLE5.PNG%' then 'BANNER-CAMPAÑA-GOOGLE5.PNG'
              when adContent like '%BANNER-CAMPAÑA-GOOGLE6.PNG%' then 'BANNER-CAMPAÑA-GOOGLE6.PNG'
              when adContent like '%COMPRA SOAT EN LINEA 24 HORAS%'  then 'COMPRA SOAT EN LINEA 24 HORAS'
              when adContent like '%COMPRA SOAT EN LíNEA 24 HORAS%' then 'COMPRA SOAT EN LINEA 24 HORAS'
              when adContent like '%COMPRA SOAT Y LLEVA OBSEQUIO%' then 'COMPRA SOAT Y LLEVA OBSEQUIO'
              when adContent like '%COMPRA TU SOAT 2018 EN LíNEA%' then 'COMPRA TU SOAT 2018 EN LINEA'
              when adContent like '%AQUí%' then 'AQUI'
              when adContent like '%COMPRA TU SOAT CON ESTA OFERTA%' then 'COMPRA TU SOAT CON ESTA OFERTA'
              when adContent like '%COMPRA TU SOAT EN LíNEA%' then 'COMPRA TU SOAT ONLINE'
              when adContent like '%COMPRA TU SOAT ONLINE%' then 'COMPRA TU SOAT ONLINE'
              when adContent like '%COMPRA TU SOAT POR INTERNET%' then 'COMPRA TU SOAT ONLINE'
              when adContent like '%COMPRA TU SOAT Y LLEVA MILLAS%' then 'COMPRA TU SOAT Y LLEVA MILLAS'
              when adContent like '%PAGA TU SOAT AQUí%' then 'PAGA TU SOAT AQUI'
              when adContent like '%PAGA TU SOAT EN LíNEA%' then 'PAGA TU SOAT EN LINEA'
              when adContent like '%PROMOCIóN SOAT %' then 'PROMOCION SOAT'
              when adContent like '%RENUEVA TU SOAT EN SOMOSF1© %' then 'RENUEVA TU SOAT EN SOMOSF1© '
              when adContent like '%SOAT%' then 'SOAT'
              when adContent like '%SOAT F1 © 100% ONLINE%' then 'SOAT F1 © 100% ONLINE'
              when adContent like '%U SOAT CON BONOS DE DESCUENTO%' then 'U SOAT CON BONOS DE DESCUENTO'
              else 'PRUEBA'
              END AS contenido from analytics")

analytics$adContent<-prueba$contenido
rm(prueba)

analytics$adContent%>%
  table()


#### Quedo Limpio el primer item


analytics%>%
  names()%>%
  cbind()


#### Campaign

analytics$campaign%>%
  table()


s02<-sqldf("select campaign ,upper(campaign) as Camapana from analytics ")

analytics$campaign<-s02$Camapana
rm(s02)

analytics$campaign%>%
  table()

a<-sqldf("select * ,
              Case
              when campaign like '%(NOT SET)%' then '(NOT SET)'
              when campaign like '%1-ENE.-2019%' then '1-ENE-2019'
              when campaign like '%10-DIC-2018%' then '10-DIC-2018'
              when campaign like '%19-DIC-2018%' then '19-DIC-2018'
              when campaign like '%21-12-2018%' then '21-DIC-2018'
              when campaign like '%21-DIC-2018%' then '21-DIC-2018'
              when campaign like '%25-DIC.-2018%' then '25-DIC-2018'
              when campaign like '%28-DIC.-2018%' then '28-DIC-2018'
              when campaign like '%7-DIC-2018%' then '7-DIC-2018'
              when campaign like '%BRANDED SEARCH%' then 'BRANDED SEARCH'
              when campaign like '%BRANDED-SEARCH%' then 'BRANDED SEARCH'
              when campaign like '%CART-ABANDONMENT%' then 'CART-ABANDONMENT'
              when campaign like '%COMERCIAL_20%' then 'COMERCIAL_20'
              when campaign like '%COMERCIAL_20_NEW%' then 'COMERCIAL_20_NEW'
              when campaign like '%COMPRA_CARTERA%' then 'COMPRA_CARTERA'
              when campaign like '%DIC-2018%' then 'DIC-2018'
              when campaign like '%LIBRE INVERSIóN: SIN PRENDA - ELEGIBLES%' then 'LIBRE INVERSION'
              when campaign like '%LIBRE_INVERSION%' then 'LIBRE INVERSION'
              when campaign like '%MAILSIGNATURE%' then 'MAILSIGNATURE'
              when campaign like '%RENOVACIóN SOAT%' then 'RENOVACION SOAT'
              when campaign like '%RESCATES%' then 'RESCATES'
              when campaign like '%SAMPLE - MY FIRST INBOUND CAMPAIGN IN HUBSPOT%' then 'SAMPLE - MY FIRST INBOUND CAMPAIGN IN HUBSPOT'
              when campaign like '%SOAT 2019%' then 'SOAT 2019'
              when campaign like '%SOAT ALPHA%' then 'SOAT ALPHA'
              when campaign like '%SOAT ALPHA COPIA%' then 'SOAT ALPHA'
              when campaign like '%SOAT ALPHA COPIA 2%' then 'SOAT ALPHA'
              when campaign like '%SOAT ALPHA TV%' then 'SOAT ALPHA TV'
              when campaign like '%SOAT COMPRAR%' then 'SOAT COMPRAR'
              when campaign like '%SOAT DYNAMIC SEARCH ADS%' then 'SOAT DYNAMIC SEARCH ADS'
              when campaign like '%SOAT-1497%' then 'PRUEBA'
              when campaign like '%SOAT-154%' then 'PRUEBA'
              when campaign like '%SOAT-15%' OR 'SOAT-16' then 'PRUEBA'
              when campaign like '%TARGET CPA EXPERIMENT%' then 'TARGET CPA EXPERIMENT'
              when campaign like '%TARGET CPA EXPERIMENT 2%' then 'TARGET CPA EXPERIMENT'
              when campaign like '%WIDGET-SOAT%' then 'WIDGET-SOAT'
              else 'PRUEBA'
              END AS contenido from analytics")


a$contenido<-as.factor(a$contenido)
a$contenido%>%
  table()

analytics$campaign<-a$contenido

#### conversion
analytics$conversion%>%
  table()

#### date

analytics$date%>%
  table()

analytics$date%>%
  str()


analytics$date%>%
  class()

ymd_hms(analytics$date)->Fecha

analytics$date<-Fecha

analytics$date%>%
  class()




#### medium

analytics$medium%>%
  table()



s02<-sqldf("select medium ,upper(medium) as Medio from analytics")
analytics$medium<-s02$Medio

analytics$medium%>%
  table()


##### session

analytics$session%>%
  table()


#### source
analytics$source%>%
  table()


#### Demographic ####

Demografics->demographic




demographic%>%
  str()


Demografics$date
Demografics$date<-ymd_hms(demographic$date)

Demografics$device%>%
  table()

Demografics$os%>%
  table()

Demografics%>%
  names()%>%
  cbind()


Demografics$session<-as.numeric(Demografics$session)

Demografics$sessionsDuration<-as.numeric(Demografics$sessionsDuration)



Demografics$sessionsDuration%>%
  summary()

### Parrilla ####



parrilla%>%
  names()%>%
  cbind()


parrilla$calculated%>%
  table()

s02<-sqldf("select calculated, upper(calculated) as Calculated from parrilla")
s02$Calculated<-as.factor(s02$Calculated)

parrilla$calculated<-s02$Calculated


#### "channel"


parrilla$channel%>%
  table()%>%
  cbind()


####  coverage


parrilla$coverage%>%
  table()%>%
  cbind()

s02<-sqldf("select coverage, upper(coverage) as Cov from parrilla")
parrilla$coverage<-s02$Cov

#### end_date

parrilla$end_date<-ymd_hms(parrilla$end_date)
parrilla%>%glimpse()

parrilla$inversion<-as.numeric(parrilla$inversion)

#### format

parrilla$format%>%
  table()




#### medium

parrilla$medium%>%
  table()%>%
  cbind()

s02<-sqldf("select medium , upper(medium) from parrilla")
s02
parrilla$medium<-s02$`upper(medium)`

#### rating

parrilla$rating%>%
  summary()

#### reference

parrilla$reference%>%
  table()%>%
  cbind()


#### start_date

parrilla$start_date<-ymd_hms(parrilla$start_date)

#### support

parrilla$support%>%
  table()


##### Participaciones ####
### adContent

participaciones$adContent%>%
  table()%>%
  cbind()

s02<-sqldf("select adContent, upper(adContent) from participaciones")

participaciones$adContent<-s02$`upper(adContent)`

a<-sqldf("select * , 
         Case
         when adContent like '%COMPRA TU SOAT EN LíNEA%' then 'COMPRA TU SOAT EN LINEA'
         when adContent like '%BANNER-CAMPAÑA-GOOGLE.PNG%' then 'BANNER-CAMPAÑA-GOOGLE.PNG'
         when adContent like '%BANNER-CAMPAÑA-GOOGLE3.PNG%' then  'BANNER-CAMPAÑA-GOOGLE3.PNG'
         when adContent like '%BANNER-CAMPAÑA-GOOGLE3.PNG%' then 'BANNER-CAMPAÑA-GOOGLE3.PNG'
         when adContent like '%BANNER-CAMPAÑA-GOOGLE4.PNG%' then 'BANNER-CAMPAÑA-GOOGLE4.PNG'
         when adContent like '%BANNER-CAMPAÑA-GOOGLE6.PNG%' then 'BANNER-CAMPAÑA-GOOGLE6.PNG'
         when adContent like '%COMPRA SOAT EN LINEA 24 HORAS%' then 'COMPRA SOAT EN LINEA 24 HORAS'
         when adContent like '%COMPRA SOAT EN LíNEA 24 HORAS%' then 'COMPRA SOAT EN LINEA 24 HORAS'
         when adContent like '%COMPRA TU SOAT AQUí UN DOMINGO%' then 'COMPRA TU SOAT AQUí UN DOMINGO'
         when adContent like '%COMPRA TU SOAT CON ESTA OFERTA%' then 'COMPRA TU SOAT CON ESTA OFERTA'
         when adContent like '%COMPRA TU SOAT EN LíNEA%' then 'COMPRA TU SOAT ONLINE'
         when adContent like '%COMPRA TU SOAT EN SOMOSF1©%' then 'COMPRA TU SOAT EN SOMOSF1©'
         when adContent like '%COMPRA TU SOAT ONLINE%' then 'COMPRA TU SOAT ONLINE'
         when adContent like '%COMPRA TU SOAT PARA MOTO AQUí %' then 'COMPRA TU SOAT PARA MOTO AQUI'
         when adContent like '%COMPRA TU SOAT POR INTERNET%' then 'COMPRA TU SOAT ONLINE'
         when adContent like '%PAGA TU SOAT AQUí%' then 'PAGA TU SOAT AQUI'
         when adContent like '%PAGA TU SOAT EN LíNEA%' then 'PAGA TU SOAT EN LINEA'
         when adContent like '%PROMOCIóN SOAT%' then 'PROMOCION SOAT'
         when adContent like '%RENUEVA TU SOAT EN SOMOSF1©%' then 'RENUEVA TU SOAT EN SOMOSF1©'
         when adContent like '%SOAT%' then 'SOAT'
         when adContent like '%SOAT F1 © 100% ONLINE%' then 'SOAT F1 © 100% ONLINE'
         when adContent like '%TU SOAT CON BONOS DE DESCUENTO%' then 'TU SOAT CON BONOS DE DESCUENTO'
         else 'PRUEBA'
         END AS contenido from participaciones")

participaciones$adContent<-a$contenido



#### 

participaciones$adGroup%>%
  table()%>%
  cbind()
s02<-sqldf("select adGroup, upper(adGroup) from participaciones")
participaciones$adGroup<-s02$`upper(adGroup)`

participaciones$assistedConversions%>%
  table()%>%
  cbind()

####

participaciones$campaign%>%
  table()%>%
  cbind()

s02<-sqldf('select campaign, upper(campaign) as Cam from participaciones')

participaciones$campaign<-s02$Cam

s02<-sqldf("select *,
           CASE
           when campaign like '%(NOT SET)%' then '(NOT SET)' 
           when campaign like '1-ENE.-2019' then '1-ENE-2019'
           when campaign like '%10-DIC-2018%' then '10-DIC-2018'
           when campaign like '19-DIC-2018' then '19-DIC-2018'
           when campaign like '21-12-2018' then '21-DIC-2018'
           when campaign like '25-DIC.-2018' then '25-DIC-2018'
           when campaign like 'BRANDED SEARCH' then 'BRANDED SEARCH'
           when campaign like 'BRANDED-SEARCH' then 'BRANDED SEARCH'
           when campaign like '%COMERCIAL_20%' then 'COMERCIAL_20'
           when campaign like 'COMPRA_CARTERA' then 'COMPRA_CARTERA'
           when campaign like '%CREDITO-REFINANCIACION-COLPATRIA%' then 'CREDITO-REFINANCIACION-COLPATRIA'
           when campaign like '%DIC-2018%' then 'DIC-2018'
           when campaign like 'LIBRE_INVERSION' then 'LIBRE_INVERSION'
           when campaign like '%SOAT 2019%' then 'SOAT 2019'
           when campaign like '%SOAT ALPHA%' then 'SOAT ALPHA'
           when campaign like '%SOAT ALPHA COPIA%' then 'SOAT ALPHA COPIA'
           when campaign like '%SOAT ALPHA COPIA 2%' then 'SOAT ALPHA COPIA 2'
           when campaign like '%SOAT ALPHA TV%' then 'SOAT ALPHA TV'
           when campaign like '%SOAT CART ABANDONMENT%' then 'SOAT CART ABANDONMENT'
           when campaign like '%SOAT COMPRAR%' then 'SOAT COMPRAR'
           when campaign like '%SOAT DYNAMIC SEARCH ADS%' then 'SOAT DYNAMIC SEARCH ADS'
           when campaign like '%TARGET CPA EXPERIMENT%' then 'TARGET CPA EXPERIMENT'
           when campaign like '%TARGET CPA EXPERIMENT 2%' then 'TARGET CPA EXPERIMENT 2'
           when campaign like '%WIDGET-SOAT%' then 'WIDGET-SOAT'
           else 'OTROS'
           END AS CAPAÑA from participaciones")

s02$CAPAÑA%>%
  table()%>%
  cbind()


participaciones$campaign<-s02$CAPAÑA

#### 

participaciones$date<-as.Date(participaciones$date)


####
participaciones$medium%>%
  table()%>%
  cbind()
s02<-sqldf("select medium, upper(medium) as Medio from participaciones")


participaciones$medium<-s02$Medio


##### usersForClassification####

usersForClassification$date<-as.Date(usersForClassification$date)
usersForClassification$avgSessionDuration<-as.numeric(usersForClassification$avgSessionDuration)
usersForClassification$avgTimeOnPage<-as.numeric(usersForClassification$avgTimeOnPage)
usersForClassification$bounces<-as.numeric(usersForClassification$bounces)
usersForClassification$client_id<-as.factor(usersForClassification$client_id)
usersForClassification$dayOfWeek<-as.numeric(usersForClassification$dayOfWeek)
usersForClassification$daysSinceLastSession<-as.numeric(usersForClassification$daysSinceLastSession)
hours <- as.numeric(usersForClassification$hour, units = "hours")
usersForClassification$hour<-hours

as.numeric(usersForClassification$minute, units="minutes")->minute
usersForClassification$minute<-minute
usersForClassification$newUsers<-as.numeric(usersForClassification$newUsers)
usersForClassification$organicSearches<-as.numeric(usersForClassification$organicSearches)
usersForClassification$pageviews<-as.numeric(usersForClassification$pageviews)
usersForClassification$userType<-as.factor(usersForClassification$userType)
usersForClassification$day<-days(usersForClassification$day)



#### Primer cruce

hour(parrilla$start_date)->H
minute(parrilla$start_date)->M
parrilla$Hora<-H
parrilla$Minuto<-M
parrilla$Fecha<-as.Date(parrilla$start_date)
FC<-with(parrilla, ymd_hm(paste(Fecha,Hora,Minuto, sep= ' ')))
parrilla$Fecha_cruce<-FC

userForClassification.csv->usersForClassification
usersForClassification%>%
  glimpse()

b <- strptime(usersForClassification$hour, format = "%H")
c <- strptime(usersForClassification$minute, format = "%M")
usersForClassification$hour<-hour(b)
usersForClassification$minute<-minute(c)

a<-usersForClassification
usersForClassification$Fecha_completa<-with(a, ymd_hm(paste(date,hour,minute, sep= ' ')))




left_join(usersForClassification,parrilla, by=c("Fecha_completa"="Fecha_cruce"))->B1
left_join(B1,demographic, by=c("client_id"="dimension1"))->Base_Final
Base_Final$date.y<-ymd_hms(Base_Final$date.y)

left_join(Base_Final,analytics, by=c("date.y"="date"))->Base_OUT

dir.create("/Users/danielj/Desktop/R5/Fechas")
write.csv("../../Fechas/Salida.csv")
