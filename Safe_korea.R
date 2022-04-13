install.packages("ggmap")
install.packages("ggplot2")
install.packages("raster")
install.packages("rgeos")
install.packages("rgdal")
install.packages("knitr")
install.packages("rmarkdown")
install.packages("sp")
install.packages("dplyr")
install.packages("maptools")
install.packages("plyr")
install.packages("stringr")
install.packages("KoNLP")
install.packages("wordcloud")

library(reshape2)
library(ggmap)
library(ggplot2)
.libPaths("C://rstudy")
library(raster)
library(rgeos)
library(rgdal)
library(knitr)
library(rmarkdown)
library(sp)
library(dplyr)
library(maptools)
library(plyr)
library(stringr)
library(wordcloud)

#-Heejoo_Wordcloud---------------------------------------------------------------------------------------------------
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jdk1.8.0_201')
library(rJava)
library(KoNLP)

#데이터 가져와서 project에 저장
crawling <- readLines("CrawlingData.txt") 

#각 라인에서 명사 단어만 추출
crawling <- sapply(crawling, extractNoun, USE.NAMES = F)

#단어만 가져와서 project2에 저장
crawling2 <- unlist(crawling)

#불필요 단어 공백처리
crawling2 <- gsub("www", "", crawling2)
crawling2 <- gsub("kr", "", crawling2)
crawling2 <- gsub("https", "", crawling2)
crawling2 <- gsub("2017.", "", crawling2)
crawling2 <- gsub("11.", "", crawling2)
crawling2 <- gsub("news", "", crawling2)
crawling2 <- gsub("12.", "", crawling2)
crawling2 <- gsub("2016.", "", crawling2)
crawling2 <- gsub("com", "", crawling2)

#두 글자 이상만 추출
crawling2 <- Filter(function(x) {nchar(x) >= 2}, crawling2)

#테이블 형태로 변환하여 저장
wordcount <- table(crawling2)

#가장 많이 나온 숫사 TOP15
head(sort(wordcount, decreasing = TRUE),15) 

#가공된 데이터를 wordcloud로 표현 
wordcloud(names(wordcount) , freq=wordcount, scale=c(5, 0.3), min.freq = 3, 
          random.order = F, random.color=T, rot.per = .1, 
          colors = brewer.pal(8, "Dark2"), family = "headline")

#---DATA------------------------------------------------------------------------------------------------------

korea_people <- read.csv("인구_전처리.csv")
str(korea_people)
head(korea_people)

safe_korea <- read.csv("민방위대피소_전처리.csv")
str(safe_korea)
head(safe_korea)

#id별로 대피가능 인원수 더하기
safe_people <-ddply(safe_korea,.(id),summarize,대피가능인원수 = sum(대피가능인원수))
safe_people

#id를 기준으로 safe_people의 대피가능인원수 , korea_people의 인원수를 merge
merge_people <- merge(korea_people, safe_people, by="id")
merge_people

#---Daeun_ggplot--------------------------------------------------------------------------------------------

#필요한 데이터만 가져오기
merge_people_data <- merge_people[2:4]

#데이터 재구성
sido <- melt(merge_people_data, id = "행정구역별")
sido

#열이름 다시 지정
colnames(sido) <- c("행정구역별", "분류", "인구수")
colnames(sido)

#표 만들기
ggplot(sido, aes(x=행정구역별, y = 인구수/10000, fill = 분류)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() +
  scale_fill_brewer(palette = "Reds") +
  ggtitle("시도별 민방위 대피시설 수용인원") +
  theme(plot.title = element_text(size=20, hjust = 0.5, face = "bold", color = )) +
  theme(legend.text = element_text(size = 12)) +
  theme(legend.title = element_text(size = 16, face = "bold"), legend.position = c(0.9,0.8)) +
  ylab("인원(만명)") +
  geom_text(aes(label=인구수), vjust=-0.5, position=position_dodge(.9),size=3) +
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

#표 고화질로 저장
ggsave(file="C://rstudy/sido_plot.tiff", units="in", width=20, height=10, dpi=300, compression = 'lzw')
dev.off()

#-----JeongMin_ggmap---------------------------------------------------------------------------------------------

#한국 표시를 위한 쉐이프 파일
korea <- shapefile("TL_SCCO_CTPRVN.shp")
korea

#쉐이프 파일에는 위도,경도의 좌표계가 설정되지 않아 좌표계 설정
korea <- spTransform(korea, CRS("+proj=longlat"))
#fortify 함수를 사용해 쉐이프 파일을 R의 데이터 프레임으로 바꿈
korea_map<- fortify(korea)

#대피가능인원수/인원수 = 수용률
capacity <- ddply(merge_people,.(id),summarize, 대피인구수용률 = (대피가능인원수 / 인원수)*0.1)
#한국지도와 수용률을 id로 merge
merge_map <- merge(korea_map,capacity,by="id")

#구글 키 등록
register_google(key = "AIzaSyCNa5a-Tb71gwIfI3ESVFnX87qkyqgX2os")
#south korea로 지도 지정
map <- get_googlemap("south korea", maptype="roadmap", zoom=7,color='bw')
#지도 그리기
capacity_map <- ggmap(map) + 
  geom_polygon(data = merge_map, aes(x=long, y=lat, group=group, fill=대피인구수용률),color='black', alpha=.75) + 
  scale_fill_distiller(palette = 'Spectral')
capacity_map

#---location------------------------------------------------------------------------------------

#가공한 csv파일을 가져와서 station_data 변수에 할당
station_data <- read.csv("지하철_역별_주소_정보.csv")

str(station_data)

#지하철역 위도와 경도를 가져옴
station_code <- as.character(station_data$"지번주소") %>% enc2utf8() %>% geocode()

#지하철역 데이터에 위도, 경도 추가
station_code_final <- cbind(station_data,station_code)

head(station_code_final)

#엑세 파일 가져와서 safe_korea 변수에 할당
safe_location <- read.csv("서울특별시_은평구_민방위대피시설_1.csv")
str(safe_location)

#은평구 지도 가져오기
eunpyeong_map <- get_googlemap("eunpyeonggu", maptype = "roadmap", zoom = 13)

ggmap(eunpyeong_map)       

#산점도를 이용한 지하철역 위치 표시 및 역명 표시
ggmap(eunpyeong_map) + geom_point(data = station_code_final, aes(x=lon, y=lat), colour="Sienna", size=4) +
  geom_text(data=station_code_final, aes(label=역명, vjust=-1))+ 
  geom_point(data = safe_location, aes(x=lon, y=lat), colour="Royal Blue", size=2)
