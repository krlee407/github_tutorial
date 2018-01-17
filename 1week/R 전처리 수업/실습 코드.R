rm(list=ls())
kospi= read.csv("kospi_data.csv", stringsAsFactors = F)

######### 데이터 상태 확인
head(kospi)
str(kospi)
summary(kospi)

######## na 제거
sum(is.na(kospi))
kospi = na.omit(kospi)

######## 데이터 class 알맞게 바꾸기
kospi$Date = as.Date(kospi$Date, format='%Y/%m/%d')

as.Date("18년1월17일")
as.Date("18년1월17일", format = '%y년%m월%d일') ## %Y는 연도가 뒤에 네자리가 써있을때 ex) 2018년1월17일 !

kospi$UpDown = as.factor(kospi$UpDown)

######## 합치기 
kospi2 = read.csv("kospi_data2.csv", stringsAsFactors = F)
str(kospi2)
kospi2$Date = as.Date(kospi2$Date, format='%Y-%m-%d') 

head(kospi)
head(kospi2)

tail(kospi)
tail(kospi2)

# merge
kospi_merged1 = merge(kospi, kospi2, by='Date')
kospi_merged2 = merge(kospi, kospi2, by='Date', all=T)
kospi_merged3 = merge(kospi, kospi2, by='Date', all.x=T)
kospi_merged4 = merge(kospi, kospi2, by='Date', all.y=T)

# join
 #install.packages("dplyr")
library(dplyr)
kospi_joined1 = inner_join(kospi, kospi2, by='Date')
kospi_joined2 = full_join(kospi, kospi2, by='Date')
kospi_joined3 = left_join(kospi, kospi2, by='Date')
kospi_joined4 = right_join(kospi, kospi2, by='Date')

######## 집계
# 변수별로 뽑아오기
select(kospi_merged1, KOSPI, UpDown)
kospi_merged1[,c("KOSPI","UpDown")] ## 대용량에서 select보다 느리다

# 변수별로 조건걸어 뽑아오기
filter(kospi_merged1, UpDown =='1'& Date > as.Date("2011-11-01")) # or은 기호 | 로 조건들을 이으면 된다. 
subset(kospi_merged1, UpDown =='1' & Date > as.Date("2011-11-01")) 
kospi_merged1[kospi_merged1$UpDown =='1'& kospi_merged1$Date > as.Date("2011-11-01"),] # 내장함수, 속도가 느리다. 

# pipe 함수
mean(kospi_merged1$KOSPI)
kospi_merged1$KOSPI %>% mean() 

arrange(kospi_merged1, UpDown, KOSPI)
kospi_merged1 %>% arrange(UpDown, KOSPI)

# 열 추가
kospi_merged1$diff = kospi_merged1$High - kospi_merged1$Low
kospi_merged1$diff

kospi_merged1 = kospi_merged1 %>%
  mutate(diff2 = High - Low)
kospi_merged1$diff2

str(kospi_merged1)

# 그룹별로 값 산출하기 
kospi_merged1 %>%
  group_by(UpDown) %>% # group_by >> 연산속도가 빠른 tibble이라는 데이터형태로 바꿔줌 
  summarise(mean(KOSPI))

lapply(split(x=kospi_merged1$KOSPI, f=kospi_merged1$UpDown), mean)
tapply(kospi_merged1$KOSPI, INDEX = kospi_merged1$UpDown, mean)
aggregate(kospi_merged1$KOSPI, by=list(kospi_merged1$UpDown), mean)
library(plyr)
ddply(kospi_merged1, .(UpDown), summarize, mean(KOSPI))

####### gather, spread 함수
library(tidyr)

blood_df = data.frame(gender = c("M", "W", "M", "W","M", "W","M", "W"), 
                      type = c("A", "A", "B", "B", "O", "O", "AB", "AB"), 
                      num = c(100, 200, 150, 100, 250, 200, 50, 50))
blood_df

aa = spread(blood_df, gender, num)
aa

bb = gather(aa, type, num)
bb
colnames(bb) = c("type", "gender", "num")
bb = bb[c("gender", "type", "num")]
bb

####### plot
plot(kospi[,c("Date","Low")], type="l", col= "blue", 
     axes =F, xlab = "Month", ylab = "Low & High", main = "2011 KOSPI",
     ylim=c(1600,2300))
lines(kospi[,c("Date", "High")], type="l", col="red")
axis(1, 
     at = seq.Date(as.Date("2011-01-01"), as.Date("2012-1-1"), by="month"), 
     las=1,
     labels = c(paste0(1:12, "월"), "1월"))
axis(2, las=1)
legend(x = as.Date("2011-09-01"), y = 2300, 
       legend = c("Low", "High"), 
       col=c("blue", "red"), 
       lwd=1,
       cex=0.8)

############ 문자 데이터 ###########
movie = read.csv("movie_review.csv", header=T, stringsAsFactors = F)
str(movie)

####### 단어 위치 뽑아내기 
grep("국제", movie$movieTitle) # 위치 보기 
grep("국제", movie$movieTitle, value=T) # 전체 단어 보기

unique(movie$movieTitle) # 몇가지의 영화가 있는지 확인
head(sort(table(movie$movieTitle), decreasing=T),20) # 국제시장에 대한 리뷰가 총 10개 맞는지 확인! 

movie$movieTitle %>% 
  table() %>%
  sort(decreasing=T) %>%
  head(20) # 동일한 내용 pipe 함수로

######## 특정 문자 수정하기 
samp = movie$wom[1]
samp

gsub("10.0", "십점 !! ", samp)
library(stringr)
str_replace_all(samp, "10.0", "십점 ~~ ")

######## 특정 문자 기준으로 쪼개기
samp
str_split(samp, "네요")


############################# 과제 ############################################

# 1. 주어진 cow_data에서 is_edible 이라는 새로운 열을 추가하고
# 나이(age)가 50(개월)이상이면서 등급(grade)이 "3" 또는 "등외"이라면 "폐기용", 아니면 "식용"을 기입하는 함수를 작성해주세요.
# 함수의 매개변수는 대상 데이터 1개 ex) my_function(cow_data)

# 2."1++" 등급이 가장 많은 세 지역(변수 address)을 구하고 각 지역별로 "1++"등급이 츙 몇 마리인지 보여주세요 (시/군 단위로 구해주세요)

# 3. 위 세 도시 별로 각 등급마다 소의 평균가격(price)을 구해주세요

# 4. 위 세 도시 별로 총 몇 마리의 소가 도축됐는지 월 단위로 구하고 그래프로 표현해주세요 (변수 slaughter_date가 도축된 날짜를 의미함)

# (기존기수만 해당) 
# 5. 소 가격(price)과 상관 관계가 있는 변수들이 있다면 찾아내고 그 관계를 설명해주세요 
