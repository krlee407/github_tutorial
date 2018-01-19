cow_data <- read.csv("cow_data.csv", stringsAsFactors = FALSE)

# 1. 주어진 cow_data에서 is_edible 이라는 새로운 열을 추가하고
cow_data <- cbind(cow_data, is_edible=0)

# 나이(age)가 50(개월)이상이면서 등급(grade)이 "3" 또는 "등외"이라면 "폐기용", 아니면 "식용"을 기입하는 함수를 작성해주세요.
# 함수의 매개변수는 대상 데이터 1개 ex) my_function(cow_data)
my_function <- function(row){
  return(ifelse(row["age"] >= 50 & row["grade"] %in% c("3", "등외"), "폐기용", "식용"))
}

cow_data$is_edible <- ifelse(cow_data$age >= 50 & cow_data$grade %in% c("3", "등외"), "폐기용", "식용")


# 2."1++" 등급이 가장 많은 세 지역(변수 address)을 구하고 각 지역별로 "1++"등급이 츙 몇 마리인지 보여주세요 (시/군 단위로 구해주세요)
cow1pp <- cow_data[which(cow_data$grade == "1++"),]
parser <- function(str){
  return(paste(unlist(strsplit(str, ' '))[0:2], collapse = " "))
}
cow1pp_address <- sapply(cow1pp$address, parser)
sort(table(cow1pp_address), TRUE)[0:3]


# 3. 위 세 도시 별로 각 등급마다 소의 평균가격(price)을 구해주세요
top3_city <- sort(table(cow1pp_address), TRUE)[0:3]
address_city <- sapply(cow_data$address, parser)
cow_data <- cbind(cow_data, address_city)
cow_data$price <- sapply(gsub(",", "", cow_data$price), as.integer)
cow_top1 <- cow_data[which(cow_data$address_city == names(top3_city)[1]),]
cow_top2 <- cow_data[which(cow_data$address_city == names(top3_city)[2]),]
cow_top3 <- cow_data[which(cow_data$address_city == names(top3_city)[3]),]
aggregate(cow_top1$price, by=list(cow_top1$grade), mean)
aggregate(cow_top2$price, by=list(cow_top2$grade), mean)
aggregate(cow_top3$price, by=list(cow_top3$grade), mean)


# 4. 위 세 도시 별로 총 몇 마리의 소가 도축됐는지 월 단위로 구하고 그래프로 표현해주세요 (변수 slaughter_date가 도축된 날짜를 의미함)
slaughter_month <- as.integer(cow_data$slaughter_date/100)
cow_data <- cbind(cow_data, slaughter_month)
cow_top1 <- cow_data[which(cow_data$address_city == names(top3_city)[1]),]
cow_top2 <- cow_data[which(cow_data$address_city == names(top3_city)[2]),]
cow_top3 <- cow_data[which(cow_data$address_city == names(top3_city)[3]),]
table(cow_top1$slaughter_month); plot(table(cow_top1$slaughter_month))
table(cow_top2$slaughter_month); plot(table(cow_top2$slaughter_month))
table(cow_top3$slaughter_month); plot(table(cow_top3$slaughter_month))


# (기존기수만 해당) 
# 5. 소 가격(price)과 상관 관계가 있는 변수들이 있다면 찾아내고 그 관계를 설명해주세요 
cor(cow_data[,c(1,2,3,4,7,8,10,11,14)])
# 별로 없다?
