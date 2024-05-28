rm(list=ls())

# 원본 데이터 가져오기
# install.packages("openxlsx")
library(openxlsx)
full_data = read.xlsx("C:/Users/User/Desktop/Environmental-Data-Analysis-Competition/4.Feature_Selection/새로운요인을추가한데이터.xlsx")
data_local_name = full_data[, c(1,2,3,9)]
data <- data_local_name[,-c(1)]


# 단계적 선택법과 요인분석을 이용한 주요변수 2개
# 새롭게 추가된 추가변수 1개
# 클러스터링을 통한 주요지역 40개
fillter_data <- data


# 개수 기준 데이터에 대한 다중공선성 확인
## 상관행렬
# install.packages("ggcorrplot")
library(ggcorrplot)
# install.packages("corrplot")
library(corrplot)
cor(fillter_data)
corrplot(cor(fillter_data), method='shade',type="lower", shade.col=NA, tl.col='black', tl.srt=15, tl.cex = 0.7)

## 다중회귀
facfor_1 <- as.numeric(fillter_data[,1])
facfor_2 <- as.numeric(fillter_data[,2])
hate_facility <- as.numeric(fillter_data[,3])
lm_fillter_local_new_feature = lm(facfor_1 ~ facfor_2 + hate_facility, data=fillter_data)
summary(lm_fillter_local_new_feature) # 다중회귀분석 결과

## vIF -> 결론 : 제거되는 변수 없음
# install.packages("regclass")
library(regclass)
VIF(lm_fillter_local_new_feature) # VIF >= 10인 경우 다중공선성 존재한다고 판단, 제거한다.
# CI(상태지수) -> 결론 : 제거되는 변수가 없음
eig = eigen(cor(fillter_data))
sqrt(max(eig$values)/eig$values) # 상태지수 >= 15인 경우 다중공선성 존재한다고 판단, 제거한다.


# 엑셀파일로 저장
# install.packages("writexl")
library(writexl)
write_xlsx(data_local_name, path = "C:/Users/User/Desktop/Environmental-Data-Analysis-Competition/4.Feature_Selection/최종요인.xlsx")

