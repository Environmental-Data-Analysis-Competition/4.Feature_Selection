rm(list=ls())

# 원본 데이터 가져오기
# install.packages("openxlsx")
library(openxlsx)
full_data = read.xlsx("C:/Users/User/Desktop/2024 환경데이터 활용 및 분석 공모전/데이터/0.전체데이터.xlsx")
data = full_data[1:230, -c(1, 2, 3)]
fillter_data <- data[,c(1, 2, 3, 4, 5, 9, 10, 15, 17)]


# KMO 검정
### KMO 검정 목적 : 변수들 간의 상관관계가 요인 분석을 수행할 수 있을 만큼 충분히 강한지를 확인
### 0.9 <= KMO 값 <= 1.0 : 아주 좋음
### 0.8 <= KMO 값 <= 0.9 : 좋음
### 0.7 <= KMO 값 <= 0.8 : 보통
### 0.6 <= KMO 값 <= 0.7 : 보통 이하
### 0.5 <= KMO 값 <= 0.6 : 나쁨
### 0.0 <= KMO 값 <= 0.5 : 요인 분석에 적합하지 않음
library(psych)
KMO(fillter_data)
### 결론 : 0.5이하인 변수가 없으므로 제거되는 변수 없음


# 인자분석(Factor Analysis)
## 상관계수 행렬 
options(digits=3) # 출력되는 소수점 자리수를 3개로 지정한다.
corMat <- cor(fillter_data)
corMat
## 인자의 수 결정 방법
### {nFactors}패키지 parallel()함수는 임의의 무상관인 표준정규변량의 상관(또는 공분산) 행렬의 고윳값 분포를 제공
### {nFactors}패키지 nScree()함수는 Cartell의 scree test를 수행, 고윳값이 작아지는 직전값을 선택
# install.packages("nFactors")
library(nFactors)
ev <- eigen(cor(fillter_data)) # 상관행렬에 대한 고윳값 
ap <- parallel(subject=nrow(fillter_data), var=ncol(fillter_data), rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)
## 인자분석
### fa()함수를 이용하여 여러가지 회전과 인자화 방법을 제공한다. common 인자화에 사용된다.
### r : 상관행렬  /  nfactors : 인자의 수(디폴트 1)
### rotate : 회전방법, 직교회전("none","varimax","quartimax")과 사교회전("promax","oblimin")이 있다.
### fm : 인자화 방법("pa"(principal axis), "ml"(maximum likelihood), "minres"(minimum residual, OLS) 등)
### 회전은 직교회전과 사교회전으로 구분된며, 두 회전의 차이는 인자들 간의 상관을 허용하는냐의 차이를 가진다.
### 직교회전은 인자들 사이의 독립성을 유지하는 회전으로 해석적인 면에서 유리하며,
### 사교회전은 비독립적인 인자를 허용하는 회전으로 단순한 구조를 만드는데 유리하다.
### 인자화의 방법은 common과 com-ponent로 구분되는데,
### common은 데이터를 잘 묘사하는 것이 주목적이며,
### com-ponent는 데이터의 양을 줄이는 데 그 목적이 있다.
# install.packages("GPArotation")
library(GPArotation) # 사교회전(“oblimin”옵션)의 수행에 필요함
EFA_obl <- fa(r = corMat, nfactors = 2, rotate="oblimin", fm = "pa")
EFA_obl
EFA_var <- fa(r = corMat, nfactors = 2, rotate="varimax", fm = "pa")
EFA_var



# 인자 부하의 시각화
##  직교회전 인자 부하 저장(채택)
# install.packages("psych")
library(psych) # 인자분석을 위한 라이브러리 호출
ls(EFA_var) # EFA(탐색적 인자분석)의 결과로 출력할 수 있는 매소드 출력
EFA_var$loadings # 높은 인자부하를 필터링하여 출력
load_var <- EFA_var$loadings[,1:2]
fa.diagram(EFA_var, main="EDA_varimax") # PA1과 PA2와 연관된 인자들을 그래프로 표기
##  사교회전 인자 부하 저장
# install.packages("psych")
library(psych) # 인자분석을 위한 라이브러리 호출
ls(EFA_obl) # EFA(탐색적 인자분석)의 결과로 출력할 수 있는 매소드 출력
EFA_obl$loadings # 높은 인자부하를 필터링하여 출력
load_obl <- EFA_obl$loadings[,1:2]
fa.diagram(EFA_obl, main="EDA_oblimin") # PA1과 PA2와 연관된 인자들을 그래프로 표기



# 최종 요인 선택
### 반올림하여 0.4 이상인 변수(요인)를 선택, 새로운 변수로 생성하였다.
## Factor1 : 농업 및 하수 관리 요인 
### 음식물쓰레기발생량(2), 농업지자체수(3), 유입하수량(4), 하수찌꺼기시설의수(7)
factor_1 <-  fillter_data[,2]*0.854 + fillter_data[,3]*0.569 + fillter_data[,4]*0.685 + fillter_data[,7]*0.412
factor_1
## Factor2 : 축산 및 하수 처리 시설 요인
### 축산지자체수(1), 하수처리시설의수(6), 하수찌꺼기시설의수(7)
factor_2 <- fillter_data[,1]*0.473 + fillter_data[,6]*0.765 + fillter_data[,7]*0.381
factor_2

