require(dplyr); require(lubridate)
require(ggmap); require(ggplot2); require(leaflet)

pvd = readxl::read_excel("PVD 수집데이터.xlsx")
str(pvd) #10000 obs, 56 variables, 35대 차량

#날짜형태 변경
pvd$GTHR_DTTM = ymd_hms(pvd$GTHR_DTTM)
pvd$OCRN_DTTM = as_datetime(pvd$OCRN_DTTM)

min(pvd$GTHR_DTTM); max(pvd$GTHR_DTTM)
min(pvd$OCRN_DTTM); max(pvd$OCRN_DTTM)
summary(pvd$OCRN_DTTM)
summary(pvd$GTHR_DTTM)
names(pvd)
pvd %>% distinct(RSU_ID)
unique(pvd$VHCL_TYPE_CD)
length(unique(pvd$OBU_ID))                                                      

#차량 운행 속도 분석
#1. 평균속도
pvd = pvd %>% mutate(speed = SPED*0.02*3.6)
pvd.obu = pvd %>% group_by(OBU_ID) %>% 
  summarise(avg =mean(speed), max = max(speed),min = min(speed)) %>% 
  arrange(desc(avg))

ggplot(data = pvd.obu, aes(x = OBU_ID, y = avg))+geom_col()+
  theme(axis.text.x = element_text(angle = 35, hjust = 1, vjust = 1, family = "Times",
                                   face = "italic", colour = "blue", size = rel(1.2)))


ggplot(data = pvd.obu, aes(x=avg, y=OBU_ID))+
  geom_segment(aes(yend = OBU_ID), xend =0, colour ="grey50")+
  geom_point(size=3, colour = "red") 

ggplot(data = pvd, aes(x =GTHR_DTTM, y= speed, colour = OBU_ID ))+
  geom_line()+ylim(0, max(pvd$speed))+
  geom_point()

pvd[pvd$OBU_ID =="7003016F",]$speed
pvd[pvd$OBU_ID == "700301B6",]$speed

#차량 경로 표시
#https://mrkevinna.github.io/R-%EC%8B%9C%EA%B0%81%ED%99%94-3/
#http://lumiamitie.github.io/r_tutorial/blog_link/leaflet_in_r.html
#devtools::install_github("dkahle/ggmap")
register_google(key = 'AIzaSyCojyeoChTJCiClls2MZSNgNrUlq1DrLk0') #구글 지도 사용하기 위한 등록 절차 필요


ggmap(get_map(location = c(mean(car$LGTD), mean(car$LTTD)), source = "google", maptype = "roadmap", zoom = 10))+geom_point(data=car,
           aes(x=LGTD,
               y=LTTD),
           shape ="#",
           color='red',size=4)

  leaflet() %>% 
  addTiles() %>% 
  addMarkers(lng = mean(car$LGTD), lat = mean(car$LTTD))

leaflet(pvd) %>% 
  setView(lng = mean(car$LGTD), lat = mean(car$LTTD), zoom =10) %>% 
  addProviderTiles('Stamene.Toner') %>% 
  addMarkers(lng = ~LGTD, lat = ~LTTD, popup = ~OBU_ID, color = ~obu.color(OBU_ID))

leaflet(pvd) %>% 
  setView(lng = mean(car$LGTD), lat = mean(car$LTTD), zoom =10) %>% 
  addSt  () %>% 
  addCircles(lng = ~LGTD, lat = ~LTTD, popup = ~OBU_ID, color = ~obu.color(OBU_ID))

leaflet(pvd) %>% 
  setView(lng = mean(car$LGTD), lat = mean(car$LTTD), zoom =10) %>% 
  addTiles() %>% 
  addCircles(lng = ~LGTD, lat = ~LTTD, popup = ~OBU_ID, color = ~obu.color(OBU_ID))
  
leaflet(pvd) %>% 
  setView(lng = mean(car$LGTD), lat = mean(car$LTTD), zoom =10) %>% 
  addTiles() %>% 
  addCircleMarkers(lng = ~LGTD, lat = ~LTTD, color = ~OBU_ID)

obu.color = colorFactor('Set1', pvd$OBU_ID)


pvd %>% group_by(OBU_ID) %>% summarise(min = min(GTHR_DTTM), max=max(GTHR_DTTM))
car = pvd %>% filter(OBU_ID=="700200FB") 
str(car)
ggplot(data = pvd, aes(x =GTHR_DTTM, y= speed, colour = OBU_ID ))+geom_line()+ylim(0, max(pvd$speed))+geom_point()

#급감가속 분석
car$speed.diff = diff(car$speed)
for(i in 1:length(car)){
  car$speed.diff[i] = car$speed[i+1]-car$speed[i]
  print(car$speed.diff)
}
plot(car$speed.diff)
ggplot(data = car, aes(speed.diff))+geom_histogram()
ggplot(data = car, aes(OCRN_DTTM,speed.diff))+geom_line()
names(car)
pvd$diff = c(0,diff(pvd$speed))

ggplot(data = pvd[pvd$OBU_ID =="700200FB",], aes(OCRN_DTTM,diff, colour = OBU_ID))+geom_line()
ggplot(data = pvd1, aes(OCRN_DTTM,diff, colour = OBU_ID))+geom_line()
pvd1 = pvd %>% filter(OCRN_DTTM>"2018-12-03")



#seasonal https://woosa7.github.io/R-%EC%8B%9C%EA%B3%84%EC%97%B4%EB%B6%84%EC%84%9D-Time-Series-ARIMA/
vsa = car %>% select(OCRN_DTTM,speed,speed.diff)
ts(vsa, frequency = 202, start = c(2018, 1))
decompose(vsa)
stl(vsa, s.window = "periodic")


#급가속
car %>% filter(between(speed.diff, 11,25))

#급감속
car %>% filter(between(speed.diff,-40, -7.5))
names(car)
str(car)

table(car$ABS_ACTIVE_STAT_CD) #-1,0
table(car$ADAS_FCWS_ACTIVE_YN)# 0
table(car$ADAS_FRWRD_VHCL_DSTNE)# 0
table(car$ADAS_LDWS_ACTIVE_YN)#0
table(car$HMI_CNNC_STAT_CD)#none
table(car$ADAS_CRAH_ANTC_RQRD_HOUR)# 0
table(car$EMRGLT_STAT_CD)#-1, 0
table(car$THRTLP)#0  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 26 27 30 31 37
table(car$DRCT_INDT_CHNG_STAT_CD)

na = colSums(is.na(pvd))
require(xlsx)
write.xlsx(na,"column.xlsx")
car$speed.diff <- c(diff(car$speed)[1], diff(car$speed))
speed.diff <- diff(car$speed)
car$speed.diff <- c(speed.diff[1], speed.diff)
