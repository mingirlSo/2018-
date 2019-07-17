setwd("C:/Users/Admin/Desktop/끝난플젝/우정국해커톤")

rm(list=ls())
gc(verbose = T)
#코드 왼쪽으로 옮기는 것 : shift+tab 
#install.packages('pacman')
require(pacman)

p_load(xlsx, data.table, caret, reshape2, tidyverse, psych, lubridate, reshape, reshape2)

### 데이터 기본 전처리 및 패딩 ######################
train<-fread("training_new.csv",encoding="UTF-8")
train<-as.data.frame(train)
str(train) #변수명 v1~v4
colnames(train)<-c('date','send_post','arrive_gu','number') #변수명 수정  
#발송국(@->서울 형태 ):246 /도착구:25 / 수량 
summary(train) #수량 통계량
train$date<-ymd(train$date) #to date type
head(train,10)

# send_post,arrive_gu순 정렬  
train<-train %>% arrange(date,send_post,arrive_gu)
unique(train$date) %>% summary() #ex. 성탄전 전후 1일의 데이터는 없음(test셋)

# padding (date별 모든 조합 만들기 , 거래량이 없던 관계를 만들어줌)
date = rep(unique(train$date),each=246*25) # 발송국 246 X 도착구 25
send_post = rep(unique(train$send_post),each=25,length(unique(train$date))) #345 
arrive_gu = rep(unique(train$arrive_gu),84870)

train_padding = data.frame(date,send_post,arrive_gu) #date별 모든 조합 만들기 
head(train_padding)

train_2 = merge(train_padding,train,by=c('date','send_post','arrive_gu'),all.x=T) #모든 조합에 number 넣기(train 기반)
train_2[is.na(train_2)] <- 0 #send - arrive간 거래량이 없는 경우 0 처리 
colSums(is.na(train_2))
head(train_2)

#발송국 라벨링 (to number)
#send_code = rep(1:length(unique(train$send_post)),each=25,length(unique(train$date)))
#train_2$send_code = send_code 
train_2$send_post <-train_2$send_post %>% as.numeric()
head(train_2) 

train<-train_2 #대체 

rm(train_2) 
rm(train_padding)

##예측해야 하는 것 
#2017년 크리스마스	20171224~20171226	
#2018년 광복절	20180814~20180816	
#2018년 추석	20180921~20180927	
#2018년 개천절	20181002~20181004	
#2018년 한글날	20181008~20181010	

### test셋 만들기  #####
test<-data.frame(c(20171224:20171226,20180814:20180816,20180921:20180927,20181002:20181004,20181008:20181010))
colnames(test)<-c("date") #19
date = rep(unique(test$date),each=246*25) # 발송국 246 X 도착구 25
send_post = rep(unique(train$send_post),each=25,length(unique(test$date))) #19
arrive_gu = rep(unique(train$arrive_gu),19*246)
test = data.frame(date,send_post,arrive_gu) 
test$date<-ymd(test$date)
str(train)
head(test,100)
#write.csv(test,"test.csv",row.names = FALSE)

rm(arrive_gu)
rm(date)
rm(send_post)
### EDA #############################################
### 수량시각화 ####
train$date = ymd(train$date)
str(train)
train$we <- weekdays(train$date)
train$we <- as.factor(train$we)

#요일별 수량sum
we_sum<-train %>% group_by(we) %>% summarise(number=sum(number)) %>% arrange(desc(number))
str(we_sum)
levels(we_sum$we)
we_sum$we<-factor(we_sum$we, levels=c('월요일','화요일','수요일','목요일','금요일','토요일','일요일')) #범주 순서 정렬 
ggplot(we_sum, aes(we, number))+geom_bar(color='red', stat='identity') + 
  geom_text(aes(label = number), vjust = -0.2) 

#날짜별 수량sum 
t_sum_all <- train %>% group_by(date) %>% summarise(number=sum(number)) # 전체 날짜별 수량합
ggplot(t_sum_all, aes(x=date %>% ymd(), y=number)) + geom_line(color='black', size=1)

### 구별 ################ 특이: 2,5,6,9,10,12#######
table(train$arrive_gu)
str(train)
train_1<-train[train$arrive_gu==1,] #1구 
str(train_1)

#1구 + 12월  
train_n <- subset(train_1, substr(train_1$date, 6,7)=="12") #12월
t_sum <- aggregate(number ~ date, train_n, sum)
ggplot(data=t_sum, aes(x=date, y=number)) + geom_line(color='black', size=1.5)
#->한번에 찍어낼 필요성을 느낌.

# 12월 수치 차이 (서울 전체 평균(12월) - 구별 전체(12월)) ####
a<-list()
for (i in 0:24){
  train_n<-train[train$arrive_gu==i,]
  train_m <- subset(train_n, substr(train_n$date, 6,7)=="12")
  t_sum <- aggregate(number ~ date, train_m, sum)
  a<-append(a,t_sum) 
}  #25개 구별 12월 date별 수량합 (number)

t_sum_all <- aggregate(number ~ date, train, sum) #서울 전체 물류량 합 
t_sum_all$number<-t_sum_all$number/25 #서울 평균 
t_sum_all$date2<-substr(t_sum_all$date,6,7) #월 
j<- t_sum_all[t_sum_all$date2=="12",]$number - a[[2]] #12월 서울 평균- 0구 

aa<-data.frame()
for(i in (1:25)){
  ab<-t_sum_all[t_sum_all$date2=="12",]$number - a[[i*2]] #날짜별 
  aa<-rbind(aa,ab)
} #12월 서울평균 - 각 구 
a12<-aa #위 과정 반복
a12
dim(a12) #12월의 각 날짜- 구별 차이 
# 위를 함수화 (완성) 함수파라미터 1 ->for적용 ->파라미터 2로  ############
make_plot<-function(k){
  a<-list()
  aa<-data.frame()
  
  t_sum_all <- aggregate(number ~ date, train, sum) #서울 전체 물류량 합 
  t_sum_all$number<-t_sum_all$number/25 #서울 평균 
  t_sum_all$date2<-substr(t_sum_all$date,6,7)
  
  for (i in 0:24){
    train_n<-train[train$arrive_gu==i,]
    train_m <-subset(train_n, substr(train_n$date, 6,7) %>% as.numeric() ==k)
    t_sum <- aggregate(number ~ date, train_m, sum)
    a<-append(a,t_sum)
  #print(a)}
  
  for ( i in 0:24){  
    hm<- a[[(i+1)*2]]
    #print(hm)
    ab<-t_sum_all[t_sum_all$date2 %>% as.numeric() == k,]$number - hm #날짜별 
    aa<-rbind(aa,ab) 
  }
  aaa<-data.frame()
  aaa<-append(aaa,aa)}
}# 각 구별 수량합을 구하고 서울의 수량합을 구해 그 차이를 추출 + 1~12 월 파라미터를 이용 

mp<-sapply(c(12,1:11),make_plot); #각 월마다 서울평균과의 차이 
mp2<-mp %>% as.data.frame() #25(구) X date(345,월순) 
a<-rbind(t_sum_all %>% filter(date2!="12") %>% select(date),t_sum_all %>% filter(date2=="12") %>% select(date))

colnames(mp2)<-a$date
str(mp2)

summary(mp)
str(mp)

### 서울(평균)과 각 구의 수량 차이 변수(percent) #######

difvar<-t(mp2) %>% as.data.frame()
colnames(difvar)<-as.character(0:24) # 날짜 행/ 구열 서울평균-해당구의 수량차이 
difvar<-cbind(date=sort(unique(train$date)),difvar)
str(difvar)
meltvar<-melt(data = difvar,id.vars = c("date"),
              measure.vars = as.character(0:24))
meltvar2<-meltvar %>% arrange(date)
meltvar2$variable<-as.numeric(as.character(meltvar2$variable))
#write.csv(meltvar2,"difvar.csv",row.names = F)
meltvar2$value= -c(meltvar2$value) #음수 

meltvar2 %>% str()
t_sum2 %>% str()
so<-merge(meltvar2,t_sum2[,1:2],by = 'date')
so %>% str()
so$perc<-so$value/so$number
colnames(so)<-c("date","arrive_gu","dif","seoul_amount","percent")

write.csv(so,"percent.csv",row.names =F )
### 서울(평균)과 각 우체국(send) 수량 차이 변수(difsend) ######
u<-as.character(unique(train$send_post))

t_sum2 <- aggregate(number ~ date, train, sum) #서울 전체 물류량 합 
#head(t_sum2)
summary(t_sum2$number)
length(unique(train$send_post))
head(t_sum2$number)
t_sum2$number<-t_sum2$number/246 #각 우체국 평균 
head(t_sum2$number)
t_sum2$date2<-substr(t_sum2$date,6,7)

##
train %>% str()
train_n=train[train$send_post=="가평우체국",]
train_m <- subset(train_n, substr(train_n$date, 6,7)=="12")
t_sum <- aggregate(number ~ date, train_m, sum)
t_sum

train_n=train[train$send_post=="강릉우체국",]
train_m <- subset(train_n, substr(train_n$date, 6,7)=="12")
t_sum2 <- aggregate(number ~ date, train_m, sum)
####
a<-list()
for (i in u){
  train_n<-train[train$send_post==i,]
  train_m <- subset(train_n, substr(train_n$date, 6,7)=="12")
  t_sum <- aggregate(number ~ date, train_m, sum)
  a<-append(a,t_sum)
}

t_sum2 %>% str()


aa<-data.frame()
for(i in 1:246){
  ab<-t_sum2[t_sum2$date2=="12",]$number - a[[i*2]]
  ab<-as.data.frame(ab)
  aa<-rbind(aa,ab)
}
head(aa)
a12<-aa

train1 %>% str()
difsend<-cbind(a12,a01,a02,a03,a04,a05,a06,a07,a08,a09,a10,a11)
difsend %>% str()
colnames(difsend)<-c(1:345)
difsend<-t(difsend) %>% as.data.frame()
colnames(difsend)<-as.character(1:246)

difsend %>% head()

difsend1<-cbind(date=sort(unique(train$date)),difsend)
str(difsend1)
meltvar<-melt(data = difsend1,id.vars = c("date"),
              measure.vars = as.character(1:246))
meltvar2<-meltvar %>% arrange(date)
meltvar2$variable<-as.numeric(as.character(meltvar2$variable))
meltvar2$value= -c(meltvar2$value) #음수 

#write.csv(meltvar2,"difsend.csv",row.names = F)
#12월 for문 시각화 ####
for (i in 0:24){
  train_1<-train1[train1$arrive_gu==i,]
  train_n <- subset(train_1, substr(train_1$date, 6,7)=="12")
  t_sum <- aggregate(number ~ date, train_n, sum)
  #t_sum<-paste0(t_sum,"_",i)
  a=ggplot(data=t_sum, aes(x=date, y=number)) + geom_line(color='black', size=1.5)
  png(width = 500, height = 500, filename = paste0("png",i,".png"))
  print(a)
  dev.off()
}
 
### 변동계수 (월별 구의 요일변동계수 ) ###########
str(train)
train2<-subset(train, substr(train$date,6,7)=='01')
train2$wd<-weekdays(train2$date) #요일 
train2$wd <- factor(train2$wd, levels=c('월요일','화요일','수요일','목요일','금요일','토요일','일요일'))
all<-melt(data=train2, id.vars=c("arrive_gu",'wd'), measure.vars = ('number')) #요일에 따른 수량 
all1<-cast(data=all, arrive_gu+wd ~ variable, fun=sum) #구,요일별 수량sum
longa<-spread(all1, wd, number) #펼치기 
head(longa)
sd_all<-apply(longa[2:8], 1, sd)
mean_all<-apply(longa[2:8], 1, mean)
cv_01<-sd_all/mean_all
cv_01 %>% head() #구별 요일수량 변동계수
length(cv_01)  
#.... 반복 후 
cv_all_month<-rbind(cv_01, cv_02)
cv_all_month<-rbind(cv_all_month, cv_03)
cv_all_month<-rbind(cv_all_month, cv_04)
cv_all_month<-rbind(cv_all_month, cv_05)
cv_all_month<-rbind(cv_all_month, cv_06)
cv_all_month<-rbind(cv_all_month, cv_07)
cv_all_month<-rbind(cv_all_month, cv_08)
cv_all_month<-rbind(cv_all_month, cv_09)
cv_all_month<-rbind(cv_all_month, cv_10)
cv_all_month<-rbind(cv_all_month, cv_11)
cv_all_month<-rbind(cv_all_month, cv_12)
cv_all_t<-t(cv_all_month)
#write.csv(cv_all_t,'')?

### 변동계수 구별 (요일) ################
train_01<-train
str(train_01)
train_01$date<-as.Date(train_01$date)

train_01$we <- weekdays(train_01$date)
train_01$we <- factor(train_01$we, levels=c('월요일','화요일','수요일','목요일','금요일','토요일','일요일'))

head(train_01)
str(train_01)
we1<-melt(data=train_01, id.vars=c('arrive_gu','we'), measure.vars = ('number'))


we2<-cast(data=we1, arrive_gu+we ~ variable, fun=mean)
we3<-cast(data=we1, arrive_gu+we ~ variable, fun=sd)
long_mean<-spread(we2, we, number)
long_sd<-spread(we3, we, number)

cv_day<- long_sd / long_mean
cv_day
cv_day<-cv_day[,-1]
arrive_gu1<-rep(0:24, each=1)
cv_day <- data.frame(arrive_gu1, cv_day)
colnames(cv_day)<-c('arrive_gu','cv_mon','cv_tue','cv_wed','cv_thu','cv_fri','cv_sat','cv_sun')
cv_day
write.csv(cv_day, 'cv_day.csv')

### 변동계수 구별 (월) -내가 해본것 ################
train_01<-train
str(train_01)
train_01$date<-as.Date(train_01$date)

train_01$month <- substr(train_01$date,6,7)
train_01$month <- factor(train_01$month)

head(train_01)
str(train_01)
month1<-melt(data=train_01, id.vars=c('arrive_gu','month'), measure.vars = ('number'))


month2<-cast(data=we1, arrive_gu+month ~ variable, fun=mean)
month3<-cast(data=we1, arrive_gu+month ~ variable, fun=sd)
long_mean<-spread(month2, month, number)
long_sd<-spread(month3, month, number)
head(long_sd)
cv_month<- long_sd / long_mean
cv_month
cv_month<-cv_month[,-1]
arrive_gu1<-rep(0:24, each=1)
cv_month <- data.frame(arrive_gu1, cv_month)
colnames(cv_month)<-c('arrive_gu','cv_1','cv_2','cv_3','cv_4','cv_5','cv_6','cv_7','cv_8','cv_9','cv_10','cv_11','cv_12')
cv_month
#write.csv(cv_day, 'cv_month.csv')
##################################################################################

### 외부 데이터 전처리 및 변수화 #####################
# 공휴일 전처리  #####
holiday = fread('holi1.csv')
holiday<-as.data.frame(holiday)
head(holiday)
unique(holiday$holiday) # 신정, 설날, 기독탄신일, 추석, 석가탄신일, 삼일절날은 2로 묶음. 
# 대체공휴일, 삼일절, 국회의원선거일, 광복절, 개천절, 한글날, 대통령선거일, 임시공휴일, 1월1일,
# 대체휴무일, 전국동시지방선거, 어린이날, 부처님오신날 는 1로 묶음
# NA는 0으로 묶음.
holiday$holiday <- as.character(holiday$holiday)
holiday[is.na(holiday)] = 0
holiday[holiday$holiday=='신정' | holiday$holiday == '설날' | holiday$holiday == '기독탄신일' | 
          holiday$holiday == '추석' | holiday$holiday =='석가탄신일' | holiday$holiday == '삼일절',]$holiday = 2

holiday[holiday$holiday =='대체공휴일' | holiday$holiday =='삼일절' | holiday$holiday =='국회의원선거일' |
          holiday$holiday == '광복절' | holiday$holiday =='개천절' | holiday$holiday =='한글날' | 
          holiday$holiday == '대통령선거일' | holiday$holiday =='임시공휴일' | holiday$holiday =='1월1일' |
          holiday$holiday =='대체휴무일' | holiday$holiday =='전국동시지방선거' | holiday$holiday =='어린이날' |
          holiday$holiday =='부처님오신날' | holiday$holiday =='현충일',]$holiday = 1
holiday[holiday$holiday =='0' | holiday$holiday =='주말',]$holiday = 0

head(holiday)
holiday$holiday <- as.factor(holiday$holiday)

holiday$weekday <- as.character(holiday$weekday)
holiday[holiday$weekday=='월',]$weekday = 1
holiday[holiday$weekday=='화',]$weekday = 2
holiday[holiday$weekday=='수',]$weekday = 3
holiday[holiday$weekday=='목',]$weekday = 4
holiday[holiday$weekday=='금',]$weekday = 5
holiday[holiday$weekday=='토',]$weekday = 6
holiday[holiday$weekday=='일',]$weekday = 7

holiday$weekday <- as.factor(holiday$weekday)
holiday$date <- ymd(holiday$date)

str(holiday)

# 인구 데이터?(인구에 따라 배달량이 증가할 것이다라는 가설) #######
popul <- read.csv('인구.csv') #? where is data? 
unique(popul$행정구역별)

colnames(popul) <- c('arrive_gu','man_in','woman_in','man_out','woman_out')
head(popul)
head(train)

b = aggregate(train$number,by=list(arrive_gu=train$arrive_gu),sum)
b = merge(b,popul,by='arrive_gu')
b$population = apply(b[,3:6],1,sum)
head(b)

a = c(cor(b$x,b$population), # 상관관계 0.33 ,
      cor(b$x,b$man_in), # 국내 남자 0.3086472
      cor(b$x,b$woman_in), # 국내 여자 0.3413209
      cor(b$x,b$man_out), # 외국 남자 -0.01711366
      cor(b$x,b$woman_out)) # 외국 여자 -0.003582893

barplot(a,names=c('총인구','국내남자','국내여자','외국남자','외국여자'),main='구 별 배송량과 인구의 상관관계')

# 연령별 인구? ########

age_peo <- fread('연령별인구.csv') #where ?
age_peo<-age_peo[,-5]
head(age_peo)

arrive_gu1<-rep(0:24, each=18)
age1<-rep(0:8, each=2)
age1<-rep(age1, 25)
age_peo$arrive_gu<-as.factor(arrive_gu1)
age_peo$age<-as.factor(age1)

age_man<-aggregate(man ~ arrive_gu+age, age_peo, sum)
age_man
age_woman<-aggregate(woman ~ arrive_gu+age, age_peo, sum)
age_woman

age_peo<-merge(age_man, age_woman, by=c('arrive_gu','age'))
age_peo<-age_peo[order(age_peo$arrive_gu, decreasing = FALSE),]

head(age_peo)


write.csv(age_peo, '연령별인구_merge.csv')
str(age_peo)
head(age_peo)
# 1인 가구?    ############################

person_1<-fread('person_1.csv')
str(person_1)
colnames(person_1)<-c("arrive_gu", "gender", "20세미만", "20~24세", "25~29세", "30~34세","35~39세",
                      "40~44세", "45~49세", "50~54세", "55~59세", "60~64세", "65~69세", "70~74세",
                      "75~79세", "80~84세", "85세이상")
arrive_gu1<-rep(0:24, each=2)
gender1<-rep(1:0, each=1)
gender1
gender1<-rep(gender1, 25)
person_1$arrive_gu<-as.factor(arrive_gu1)
person_1$gender<-as.factor(gender1)
head(person_1)

write.csv(person_1, 'person_1_merge.csv')

# 네이버 트렌드? ###########
trend = read.csv('search.csv') #where
a <- aggregate(training2$number,by=list(date=training2$date),sum)
a <- merge(a,trend,by='date')
cor(a$x,a$n_search) # 상관관계 0.3633453
head(a)

### 변수 merge (이때 train+test로 진행)  ###################
# 날짜 데이터 
str(train)
train$year<-substr(train$date,1,4) %>% as.factor()
train$month<-substr(train$date,6,7) %>% as.factor()

str(train)
# 공휴일 merge 
str(holiday)
head(holiday,20) #
train1<-merge(train,holiday,by="date",all = FALSE) 
str(train1)

train1$cluster1 <- as.factor(train1$cluster1)
train1 <- as.data.table(train1)
str(train1) # cluster, weekday는 더미로 
train1$arrive_gu <- as.factor(train1$arrive_gu)

# 인구 merge
popul$arrive_gu <- as.factor(popul$arrive_gu)
train2 = merge(train2,popul[,c(1,3)],by='arrive_gu')
head(train2)

# 연령대 붙이기 
age = read.csv('연령별인구_wide.csv')
age$arrive_gu <- as.factor(age$arrive_gu)
train2 = merge(train2,age,by='arrive_gu')

# 트렌드 붙이기
trend = read.csv('trend_var.csv')
trend$date <- ymd(trend$date)
train2 = merge(train2,trend,by='date')

# 가구 붙이기 
person = read.csv('person_1f_wide.csv')
person$arrive_gu <- as.factor(person$arrive_gu)
train2 <- merge(train2,person,by='arrive_gu')

# 취득세 붙이기 
money = read.csv('재산.csv')
money$arrive_gu <- as.factor(money$arrive_gu)
train2 <- merge(train2,money,by='arrive_gu')

##소득세 /강수 / 소비자지수 추가할 것 
# 더미변수화 : cluster, weekday, year, month, day(필요없을듯), send_code는 그냥 숫자로, arrive_gu  
#send_post 제거 
str(train2)
length(unique(train2$send_post)) #246 :범주화 시키기에 부담스러움 (이를 이용한 변수는 많이 만들었다고 판단 )
train2 = train2 %>% select(-send_post) ## send_post 삭제 

colnames(train2)

#쪼개기 +후 모델링은 python
