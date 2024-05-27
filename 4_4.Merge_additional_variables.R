rm(list=ls())

# 원본 데이터 가져오기
# install.packages("openxlsx")
library(openxlsx)
full_data = read.xlsx("C:/Users/User/Desktop/2024 환경데이터 활용 및 분석 공모전/데이터/0.전체데이터.xlsx")
new_feature = read.xlsx("C:/Users/User/Desktop/환경 공모전/6.새로운변수 추가(공선성 추가 확인)/새로추가된요인.xlsx")
data = full_data[1:230, -c(1, 2, 3)]


# 단계적 선택법을 이용한 주요변수 9개
fillter_data <- data[,c(1, 2, 3, 4, 5, 9, 10, 15, 17)]


# 요인분석을 통한 새로운 요인 생성 2개
### 반올림하여 0.4 이상인 변수(요인)를 선택, 새로운 변수로 생성하였다.
## Factor1 : 농업 및 하수 관리 요인 
### 음식물쓰레기발생량(2), 농업지자체수(3), 유입하수량(4), 하수찌꺼기시설의수(7)
factor_1 <-  fillter_data[,2]*0.854 + fillter_data[,3]*0.569 + fillter_data[,4]*0.685 + fillter_data[,7]*0.412
factor_1
## Factor2 : 축산 및 하수 처리 시설 요인
### 축산지자체수(1), 하수처리시설의수(6), 하수찌꺼기시설의수(7)
factor_2 <- fillter_data[,1]*0.473 + fillter_data[,6]*0.765 + fillter_data[,7]*0.381
factor_2


# 추가적으로 중요하게 적용될 날씨데이터와 혐오시설 데이터 
new_feature = read.xlsx("C:/Users/User/Desktop/환경 공모전/6.새로운변수 추가(공선성 추가 확인)/새로추가된요인.xlsx")


# 요인분석을 통한 2개 요인과 추가적인 4개 요인을 합하여 새로운 데이터 셋 생성
new_data_set <- cbind(new_feature[,1], factor_1, factor_2, new_feature[,2:5])


# 클러스터링을 통한 주요 지역으로 한정(230개 -> 40개)
fillter_local_new_feature_name <- new_data_set[c(41,60,77,101,107,120,131,136,139,144,146,151,175,159,162,164,165,170,171,172,173,176,177,178,179,180,181,182,183,184,185,186,187,189,193,207,223,224,225,226),]
colnames(fillter_local_new_feature_name)[1] <- c('지역이름')
fillter_local_new_feature <- fillter_local_new_feature_name[,-c(1)]
fillter_local_biogas <- data.frame(rep(0,40))
colnames(fillter_local_biogas) <- c("biogas_count")
fillter_local_biogas


# 새로운 변수들의 다중공선성 확인
## 상관행렬
# install.packages("ggcorrplot")
library(ggcorrplot)
# install.packages("corrplot")
library(corrplot)
cor(fillter_local_new_feature)
corrplot(cor(fillter_local_new_feature), method='shade',type="lower", shade.col=NA, tl.col='black', tl.srt=15, tl.cex = 0.7)
## 다중회귀
facfor_1 <- as.numeric(fillter_local_new_feature[,1])
facfor_2 <- as.numeric(fillter_local_new_feature[,2])
temp_ratio <- as.numeric(fillter_local_new_feature[,3])
humid_ratio <- as.numeric(fillter_local_new_feature[,4])
mean_sun <- as.numeric(fillter_local_new_feature[,5])
hate_facility <- as.numeric(fillter_local_new_feature[,6])
lm_fillter_local_new_feature = lm(facfor_1 ~ facfor_2 + temp_ratio + humid_ratio 
                                  + mean_sun + hate_facility, data=fillter_local_new_feature)
summary(lm_fillter_local_new_feature) # 다중회귀분석 결과
## vIF -> 결론 : humid_ratio변수와 mean_sun변수가 제거됨
# install.packages("regclass")
library(regclass)
VIF(lm_fillter_local_new_feature) # VIF >= 10인 경우 다중공선성 존재한다고 판단, 제거한다.


# 새로운 변수들의 다중공선성 제거후 확인
## 상관행렬
cor(fillter_local_new_feature[,c(1,2,5,6)])
corrplot(cor(fillter_local_new_feature[,c(1,2,5,6)]), method='shade',type="lower", shade.col=NA, tl.col='black', tl.srt=15, tl.cex = 0.7)
## 다중회귀
lm_2_fillter_local_new_feature = lm(facfor_1 ~ facfor_2 + temp_ratio + hate_facility, data=fillter_local_new_feature)
summary(lm_2_fillter_local_new_feature) # 다중회귀분석 결과
## vIF -> 결론 : 다중상관성 없음
library(regclass)
VIF(lm_2_fillter_local_new_feature) # VIF >= 10인 경우 다중공선성 존재한다고 판단, 제거한다.


# 최종 데이터
finall_data_name <- fillter_local_new_feature_name[,c(1,2,3,6,7)]
finall_data_name
finall_data <- fillter_local_new_feature[,c(1,2,5,6)]
finall_data


# 엑셀파일로 저장
# install.packages("writexl")
library(writexl)
write_xlsx(finall_data_name, path = "C:/Users/User/Desktop/환경 공모전/6.새로운변수 추가(공선성 추가 확인)/최종요인.xlsx")

