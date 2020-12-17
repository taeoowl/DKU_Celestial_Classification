#### R version 3.5.2
#### ggplot version 3.1.0
#### gridExtra 2.3

##### 패키지 및 데이터 불러오기

library(ggplot2)
library(gridExtra)

setwd("C:/Users/taeoo/Desktop/단국소중")

CE <- read.csv("train.csv", header = T) 
CE_test <- read.csv('test.csv', header = T)
CE <- CE[,-1] # id 변수 삭제
CE_test <- CE_test[,-1]
CE$class <- as.factor(CE$class)
sum(is.na(CE)) # 결측값은 존재하지 않음
CE <- as.data.frame(CE)
str(CE)

##### EDA 
### 몇가지 파생변수를 통해 EDA를 실시후 클래스 별로 분포 차이가 있어 유의미하다고 생각해 추가했지만
### 성능에는 그닥 효과가 없어 EDA를 하지 않았습니다

##### 파생 변수 추가

### (https://www.sdss.org/dr16/algorithms/segue_target_selection/#Legacy)
### Mag 별 평균, 표준편차 파생변수 생성

for(i in 1:nrow(CE)){
  CE$basicMag_sum[i] <- sum(CE[i, 1:5])
  CE$basicMag_mean[i] <- mean(as.numeric(CE[i, 1:5]))
  CE$basicMag_std[i] <- sqrt(var(as.numeric(CE[i, 1:5])))
  
  CE$deredMag_sum[i] <- sum(CE[i, 7:11])
  CE$deredMag_mean[i] <- mean(as.numeric(CE[i, 7:11]))
  CE$deredMag_std[i] <- sqrt(var(as.numeric(CE[i, 7:11])))
  
  CE$airmassMag_sum[i] <- sum(CE[i, 14:18])
  CE$airmassMag_mean[i] <- mean(as.numeric(CE[i, 14:18]))
  CE$airmassMag_std[i] <- sqrt(var(as.numeric(CE[i, 14:18])))
}


### SDSS에서 주로 ugriz 차이를 이용하여 클래스를 분류하므로 ugriz간 사칙연산 파생변수 생성 
### g와 i의 차이 또한 지표로 이용하기에 g-i간 사칙연산 파생변수 생성
### 차이를 통한 클래스 분류는 psfMag ugriz를 이용하지만 일단 사용
### ugriz가 어떤 magnitude인지 알 수 없어 전부 반영, modelMag일 확률이 높음

CE$ug_differ <- CE$u - CE$g
CE$gr_differ <- CE$g - CE$r
CE$ri_differ <- CE$r - CE$i
CE$iz_differ <- CE$i - CE$z
CE$gi_differ <- CE$g - CE$i

CE$dered_ug_differ <- CE$dered_u - CE$dered_g
CE$dered_gr_differ <- CE$dered_g - CE$dered_r
CE$dered_ri_differ <- CE$dered_r - CE$dered_i
CE$dered_iz_differ <- CE$dered_i - CE$dered_z
CE$dered_gi_differ <- CE$dered_g - CE$dered_i

CE$airmass_ug_differ <- CE$airmass_u - CE$airmass_g
CE$airmass_gr_differ <- CE$airmass_g - CE$airmass_r
CE$airmass_ri_differ <- CE$airmass_r - CE$airmass_i
CE$airmass_iz_differ <- CE$airmass_i - CE$airmass_z
CE$airmass_gi_differ <- CE$airmass_g - CE$airmass_i

CE$TS1 <- CE$gr_differ + 19.78*CE$ri_differ
CE$TS2 <- CE$gr_differ - 0.95*CE$ri_differ
CE$TS3 <- CE$iz_differ - 0.68*CE$ri_differ
CE$TS4 <- CE$ug_differ + 2*CE$gr_differ
CE$TS5 <- CE$ri_differ - 0.787*CE$gr_differ
CE$TS6 <- CE$ug_differ - 0.84*CE$gr_differ
CE$TS7 <- CE$ug_differ - 2.4*CE$gr_differ
CE$TS8 <- CE$ug_differ - 2.375*CE$gr_differ
CE$TS9 <- CE$ug_differ - 0.84*CE$gr_differ
CE$TS10 <- CE$ri_differ - 3*CE$gr_differ
CE$TS11 <- CE$ug_differ - 0.9*CE$gr_differ
CE$TS12 <- CE$ri_differ - 0.65*CE$gr_differ
CE$TS13 <- CE$ug_differ - 1.314*CE$gr_differ
CE$TS14 <- CE$ug_differ + 2*CE$gr_differ

CE$dered_TS1 <- CE$dered_gr_differ + 19.78*CE$dered_ri_differ
CE$dered_TS2 <- CE$dered_gr_differ - 0.95*CE$dered_ri_differ
CE$dered_TS3 <- CE$dered_iz_differ - 0.68*CE$dered_ri_differ
CE$dered_TS4 <- CE$dered_ug_differ + 2*CE$dered_gr_differ
CE$dered_TS5 <- CE$dered_ri_differ - 0.787*CE$dered_gr_differ
CE$dered_TS6 <- CE$dered_ug_differ - 0.84*CE$dered_gr_differ
CE$dered_TS7 <- CE$dered_ug_differ - 2.4*CE$dered_gr_differ
CE$dered_TS8 <- CE$dered_ug_differ - 2.375*CE$dered_gr_differ
CE$dered_TS9 <- CE$dered_ug_differ - 0.84*CE$dered_gr_differ
CE$dered_TS10 <- CE$dered_ri_differ - 3*CE$dered_gr_differ
CE$dered_TS11 <- CE$dered_ug_differ - 0.9*CE$dered_gr_differ
CE$dered_TS12 <- CE$dered_ri_differ - 0.65*CE$dered_gr_differ
CE$dered_TS13 <- CE$dered_ug_differ - 1.314*CE$dered_gr_differ
CE$dered_TS14 <- CE$dered_ug_differ + 2*CE$dered_gr_differ

CE$airmass_TS1 <- CE$airmass_gr_differ + 19.78*CE$airmass_ri_differ
CE$airmass_TS2 <- CE$airmass_gr_differ - 0.95*CE$airmass_ri_differ
CE$airmass_TS3 <- CE$airmass_iz_differ - 0.68*CE$airmass_ri_differ
CE$airmass_TS4 <- CE$airmass_ug_differ + 2*CE$airmass_gr_differ
CE$airmass_TS5 <- CE$airmass_ri_differ - 0.787*CE$airmass_gr_differ
CE$airmass_TS6 <- CE$airmass_ug_differ - 0.84*CE$airmass_gr_differ
CE$airmass_TS7 <- CE$airmass_ug_differ - 2.4*CE$airmass_gr_differ
CE$airmass_TS8 <- CE$airmass_ug_differ - 2.375*CE$airmass_gr_differ
CE$airmass_TS9 <- CE$airmass_ug_differ - 0.84*CE$airmass_gr_differ
CE$airmass_TS10 <- CE$airmass_ri_differ - 3*CE$airmass_gr_differ
CE$airmass_TS11 <- CE$airmass_ug_differ - 0.9*CE$airmass_gr_differ
CE$airmass_TS12 <- CE$airmass_ri_differ - 0.65*CE$airmass_gr_differ
CE$airmass_TS13 <- CE$airmass_ug_differ - 1.314*CE$airmass_gr_differ
CE$airmass_TS14 <- CE$airmass_ug_differ + 2*CE$airmass_gr_differ

### i-color, s-color, P1 파생변수 추가
### 기타 기준이 되는 파생변수 추가 

addfeature <- function(a,b,c,d,e){
  a*CE$u + b*CE$g + c*CE$r + d*CE$i + e*CE$z
}
addfeature_dered <- function(a,b,c,d,e){
  a*CE$dered_u + b*CE$dered_g + c*CE$dered_r + d*CE$dered_i + e*CE$dered_z
}
addfeature_airmass <- function(a,b,c,d,e){
  a*CE$airmass_u + b*CE$airmass_g + c*CE$airmass_r + d*CE$airmass_i + e*CE$airmass_z
}

CE$icol <- addfeature(-0.436, 1.129, -0.119, -0.574, 0) + 0.1984
CE$scol <- addfeature(-0.249, 0.794, -0.555, 0, 0) + 0.234
CE$p1 <- 0.91*(CE$u - CE$g) + 0.415*(CE$g - CE$r) - 1.28


CE$dered_icol <- addfeature_dered(-0.436, 1.129, -0.119, -0.574, 0) + 0.1984
CE$dered_scol <- addfeature_dered(-0.249, 0.794, -0.555, 0, 0) + 0.234
CE$dered_p1 <- 0.91*(CE$dered_u - CE$dered_g) + 0.415*(CE$dered_g - CE$dered_r) - 1.28


CE$airmass_icol <- addfeature_airmass(-0.436, 1.129, -0.119, -0.574, 0) + 0.1984
CE$airmass_scol <- addfeature_airmass(-0.249, 0.794, -0.555, 0, 0) + 0.234
CE$airmass_p1 <- 0.91*(CE$airmass_u - CE$airmass_g) + 0.415*(CE$airmass_g - CE$airmass_r) - 1.28

### ELG ( https://www.sdss.org/dr16/algorithms/eboss-target-selection/eboss-elg-target-selection/ ) 

CE$ELG_differ1 <- CE$gr_differ + 0.068*(CE$r - CE$z) - 0.112*(CE$r - CE$z)
CE$ELG_differ2 <- (CE$r - CE$z) + 0.555*CE$gr_differ - 0.218*CE$gr_differ
CE$ELG_differ3 <- (CE$r - CE$z) + 0.555*CE$gr_differ - 0.637*CE$gr_differ

CE$ELG_dered_differ1 <- CE$dered_gr_differ + 0.068*(CE$dered_r - CE$dered_z) - 0.112*(CE$dered_r - CE$dered_z)
CE$ELG_dered_differ2 <- (CE$dered_r - CE$dered_z) + 0.555*CE$dered_gr_differ - 0.218*CE$dered_gr_differ
CE$ELG_dered_differ3 <- (CE$dered_r - CE$dered_z) + 0.555*CE$dered_gr_differ - 0.637*CE$dered_gr_differ

CE$ELG_airmass_differ1 <- CE$airmass_gr_differ + 0.068*(CE$airmass_r - CE$airmass_z) - 0.112*(CE$airmass_r - CE$airmass_z)
CE$ELG_airmass_differ2 <- (CE$airmass_r - CE$airmass_z) + 0.555*CE$airmass_gr_differ - 0.218*CE$airmass_gr_differ
CE$ELG_airmass_differ3 <- (CE$airmass_r - CE$airmass_z) + 0.555*CE$airmass_gr_differ - 0.637*CE$airmass_gr_differ

### 회전으로 인한 파생변수 (https://www.sdss.org/dr16/algorithms/legacy_target_selection/)
### parallel의 경우 가우스 표시인지, 소괄호의 중복표기를 막기위해 사용한건지 알 수없어 둘다 생성

CE$c_orth <- CE$ri_differ - CE$gr_differ/4 - 0.177
CE$c_orth_abs <- abs(CE$c_orth)  
CE$c_paral <- 0.7*CE$gr_differ + 1.2*(CE$ri_differ - 0.177)
CE$c_paral_gaus <- 0.7*CE$gr_differ + 1.2*floor((CE$ri_differ - 0.177)) 
CE$criterion1 <- CE$c_orth + CE$gr_differ/6
CE$criterion2 <- CE$gr_differ - 0.25*CE$ri_differ

CE$dered_c_orth <- CE$dered_ri_differ - CE$dered_gr_differ/4 - 0.177
CE$dered_c_orth_abs <- abs(CE$dered_c_orth)
CE$dered_c_paral <- 0.7*CE$dered_gr_differ + 1.2*(CE$dered_ri_differ - 0.177)
CE$dered_c_paral_gaus <- 0.7*CE$dered_gr_differ + 1.2*floor((CE$dered_ri_differ - 0.177))
CE$dered_criteria1 <- CE$dered_c_orth + CE$dered_gr_differ/6
CE$dered_criteria2 <- CE$dered_gr_differ - 0.25*CE$dered_ri_differ

CE$airmass_c_orth <- CE$airmass_ri_differ - CE$airmass_gr_differ/4 - 0.177
CE$airmass_c_orth_abs <- abs(CE$airmass_c_orth)  
CE$airmass_c_paral <- 0.7*CE$airmass_gr_differ + 1.2*(CE$airmass_ri_differ - 0.177)
CE$airmass_c_paral_gaus <- 0.7*CE$airmass_gr_differ + 1.2*floor((CE$airmass_ri_differ - 0.177)) 
CE$airmass_criterion1 <- CE$airmass_c_orth + CE$airmass_gr_differ/6
CE$airmass_criterion2 <- CE$airmass_gr_differ - 0.25*CE$airmass_ri_differ

### 색상 윤곽 계산 파생변수 (https://www.sdss.org/dr16/algorithms/eboss-target-selection/eboss-qso-target-selection/)

CE$c1 <- 0.95*CE$ug_differ + 0.31*CE$gr_differ + 0.11*CE$ri_differ
CE$c3 <- -0.39*CE$ug_differ + 0.79*CE$gr_differ + 0.47*CE$ri_differ
CE$criteria3 <- 0.55*CE$c1 + CE$c3
CE$criteria4 <- 0.1*CE$c1 + CE$c3

CE$dered_c1 <- 0.95*CE$dered_ug_differ + 0.31*CE$dered_gr_differ + 0.11*CE$dered_ri_differ
CE$dered_c3 <- -0.39*CE$dered_ug_differ + 0.79*CE$dered_gr_differ + 0.47*CE$dered_ri_differ
CE$dered_criteria3 <- 0.55*CE$dered_c1 + CE$dered_c3
CE$dered_criteria4 <- 0.1*CE$dered_c1 + CE$dered_c3


CE$airmass_c1 <- 0.95*CE$airmass_ug_differ + 0.31*CE$airmass_gr_differ + 0.11*CE$airmass_ri_differ
CE$airmass_c3 <- -0.39*CE$airmass_ug_differ + 0.79*CE$airmass_gr_differ + 0.47*CE$airmass_ri_differ
CE$airmass_criteria3 <- 0.55*CE$airmass_c1 + CE$airmass_c3
CE$airmass_criteria4 <- 0.1*CE$airmass_c1 + CE$airmass_c3

### 예측, 실제 필드간 차이 

CE$nDiffer <- CE$nObserve - CE$nDetect


##### 테스트 데이터 변수 추가

for(i in 1:nrow(CE_test)){
  CE_test$basicMag_sum[i] <- sum(CE_test[i, 1:5])
  CE_test$basicMag_mean[i] <- mean(as.numeric(CE_test[i, 1:5]))
  CE_test$basicMag_std[i] <- sqrt(var(as.numeric(CE_test[i, 1:5])))
  
  CE_test$deredMag_sum[i] <- sum(CE_test[i, 7:11])
  CE_test$deredMag_mean[i] <- mean(as.numeric(CE_test[i, 7:11]))
  CE_test$deredMag_std[i] <- sqrt(var(as.numeric(CE_test[i, 7:11])))
  
  CE_test$airmassMag_sum[i] <- sum(CE_test[i, 14:18])
  CE_test$airmassMag_mean[i] <- mean(as.numeric(CE_test[i, 14:18]))
  CE_test$airmassMag_std[i] <- sqrt(var(as.numeric(CE_test[i, 14:18])))
}


### SDSS에서 주로 ugriz 차이를 이용하여 클래스를 분류하므로 ugriz간 사칙연산 파생변수 생성 
### g와 i의 차이 또한 지표로 이용하기에 g-i간 사칙연산 파생변수 생성
### 차이를 통한 클래스 분류는 psfMag ugriz를 이용하지만 일단 사용
### ugriz가 어떤 magnitude인지 알 수 없어 전부 반영, modelMag일 확률이 높음

CE_test$ug_differ <- CE_test$u - CE_test$g
CE_test$gr_differ <- CE_test$g - CE_test$r
CE_test$ri_differ <- CE_test$r - CE_test$i
CE_test$iz_differ <- CE_test$i - CE_test$z
CE_test$gi_differ <- CE_test$g - CE_test$i

CE_test$dered_ug_differ <- CE_test$dered_u - CE_test$dered_g
CE_test$dered_gr_differ <- CE_test$dered_g - CE_test$dered_r
CE_test$dered_ri_differ <- CE_test$dered_r - CE_test$dered_i
CE_test$dered_iz_differ <- CE_test$dered_i - CE_test$dered_z
CE_test$dered_gi_differ <- CE_test$dered_g - CE_test$dered_i

CE_test$airmass_ug_differ <- CE_test$airmass_u - CE_test$airmass_g
CE_test$airmass_gr_differ <- CE_test$airmass_g - CE_test$airmass_r
CE_test$airmass_ri_differ <- CE_test$airmass_r - CE_test$airmass_i
CE_test$airmass_iz_differ <- CE_test$airmass_i - CE_test$airmass_z
CE_test$airmass_gi_differ <- CE_test$airmass_g - CE_test$airmass_i

CE_test$TS1 <- CE_test$gr_differ + 19.78*CE_test$ri_differ
CE_test$TS2 <- CE_test$gr_differ - 0.95*CE_test$ri_differ
CE_test$TS3 <- CE_test$iz_differ - 0.68*CE_test$ri_differ
CE_test$TS4 <- CE_test$ug_differ + 2*CE_test$gr_differ
CE_test$TS5 <- CE_test$ri_differ - 0.787*CE_test$gr_differ
CE_test$TS6 <- CE_test$ug_differ - 0.84*CE_test$gr_differ
CE_test$TS7 <- CE_test$ug_differ - 2.4*CE_test$gr_differ
CE_test$TS8 <- CE_test$ug_differ - 2.375*CE_test$gr_differ
CE_test$TS9 <- CE_test$ug_differ - 0.84*CE_test$gr_differ
CE_test$TS10 <- CE_test$ri_differ - 3*CE_test$gr_differ
CE_test$TS11 <- CE_test$ug_differ - 0.9*CE_test$gr_differ
CE_test$TS12 <- CE_test$ri_differ - 0.65*CE_test$gr_differ
CE_test$TS13 <- CE_test$ug_differ - 1.314*CE_test$gr_differ
CE_test$TS14 <- CE_test$ug_differ + 2*CE_test$gr_differ

CE_test$dered_TS1 <- CE_test$dered_gr_differ + 19.78*CE_test$dered_ri_differ
CE_test$dered_TS2 <- CE_test$dered_gr_differ - 0.95*CE_test$dered_ri_differ
CE_test$dered_TS3 <- CE_test$dered_iz_differ - 0.68*CE_test$dered_ri_differ
CE_test$dered_TS4 <- CE_test$dered_ug_differ + 2*CE_test$dered_gr_differ
CE_test$dered_TS5 <- CE_test$dered_ri_differ - 0.787*CE_test$dered_gr_differ
CE_test$dered_TS6 <- CE_test$dered_ug_differ - 0.84*CE_test$dered_gr_differ
CE_test$dered_TS7 <- CE_test$dered_ug_differ - 2.4*CE_test$dered_gr_differ
CE_test$dered_TS8 <- CE_test$dered_ug_differ - 2.375*CE_test$dered_gr_differ
CE_test$dered_TS9 <- CE_test$dered_ug_differ - 0.84*CE_test$dered_gr_differ
CE_test$dered_TS10 <- CE_test$dered_ri_differ - 3*CE_test$dered_gr_differ
CE_test$dered_TS11 <- CE_test$dered_ug_differ - 0.9*CE_test$dered_gr_differ
CE_test$dered_TS12 <- CE_test$dered_ri_differ - 0.65*CE_test$dered_gr_differ
CE_test$dered_TS13 <- CE_test$dered_ug_differ - 1.314*CE_test$dered_gr_differ
CE_test$dered_TS14 <- CE_test$dered_ug_differ + 2*CE_test$dered_gr_differ

CE_test$airmass_TS1 <- CE_test$airmass_gr_differ + 19.78*CE_test$airmass_ri_differ
CE_test$airmass_TS2 <- CE_test$airmass_gr_differ - 0.95*CE_test$airmass_ri_differ
CE_test$airmass_TS3 <- CE_test$airmass_iz_differ - 0.68*CE_test$airmass_ri_differ
CE_test$airmass_TS4 <- CE_test$airmass_ug_differ + 2*CE_test$airmass_gr_differ
CE_test$airmass_TS5 <- CE_test$airmass_ri_differ - 0.787*CE_test$airmass_gr_differ
CE_test$airmass_TS6 <- CE_test$airmass_ug_differ - 0.84*CE_test$airmass_gr_differ
CE_test$airmass_TS7 <- CE_test$airmass_ug_differ - 2.4*CE_test$airmass_gr_differ
CE_test$airmass_TS8 <- CE_test$airmass_ug_differ - 2.375*CE_test$airmass_gr_differ
CE_test$airmass_TS9 <- CE_test$airmass_ug_differ - 0.84*CE_test$airmass_gr_differ
CE_test$airmass_TS10 <- CE_test$airmass_ri_differ - 3*CE_test$airmass_gr_differ
CE_test$airmass_TS11 <- CE_test$airmass_ug_differ - 0.9*CE_test$airmass_gr_differ
CE_test$airmass_TS12 <- CE_test$airmass_ri_differ - 0.65*CE_test$airmass_gr_differ
CE_test$airmass_TS13 <- CE_test$airmass_ug_differ - 1.314*CE_test$airmass_gr_differ
CE_test$airmass_TS14 <- CE_test$airmass_ug_differ + 2*CE_test$airmass_gr_differ

### i-color, s-color, P1 파생변수 추가
### 기타 기준이 되는 파생변수 추가 

addfeature <- function(a,b,c,d,e){
  a*CE_test$u + b*CE_test$g + c*CE_test$r + d*CE_test$i + e*CE_test$z
}
addfeature_dered <- function(a,b,c,d,e){
  a*CE_test$dered_u + b*CE_test$dered_g + c*CE_test$dered_r + d*CE_test$dered_i + e*CE_test$dered_z
}
addfeature_airmass <- function(a,b,c,d,e){
  a*CE_test$airmass_u + b*CE_test$airmass_g + c*CE_test$airmass_r + d*CE_test$airmass_i + e*CE_test$airmass_z
}

CE_test$icol <- addfeature(-0.436, 1.129, -0.119, -0.574, 0) + 0.1984
CE_test$scol <- addfeature(-0.249, 0.794, -0.555, 0, 0) + 0.234
CE_test$p1 <- 0.91*(CE_test$u - CE_test$g) + 0.415*(CE_test$g - CE_test$r) - 1.28


CE_test$dered_icol <- addfeature_dered(-0.436, 1.129, -0.119, -0.574, 0) + 0.1984
CE_test$dered_scol <- addfeature_dered(-0.249, 0.794, -0.555, 0, 0) + 0.234
CE_test$dered_p1 <- 0.91*(CE_test$dered_u - CE_test$dered_g) + 0.415*(CE_test$dered_g - CE_test$dered_r) - 1.28


CE_test$airmass_icol <- addfeature_airmass(-0.436, 1.129, -0.119, -0.574, 0) + 0.1984
CE_test$airmass_scol <- addfeature_airmass(-0.249, 0.794, -0.555, 0, 0) + 0.234
CE_test$airmass_p1 <- 0.91*(CE_test$airmass_u - CE_test$airmass_g) + 0.415*(CE_test$airmass_g - CE_test$airmass_r) - 1.28

### ELG ( https://www.sdss.org/dr16/algorithms/eboss-target-selection/eboss-elg-target-selection/ ) 

CE_test$ELG_differ1 <- CE_test$gr_differ + 0.068*(CE_test$r - CE_test$z) - 0.112*(CE_test$r - CE_test$z)
CE_test$ELG_differ2 <- (CE_test$r - CE_test$z) + 0.555*CE_test$gr_differ - 0.218*CE_test$gr_differ
CE_test$ELG_differ3 <- (CE_test$r - CE_test$z) + 0.555*CE_test$gr_differ - 0.637*CE_test$gr_differ

CE_test$ELG_dered_differ1 <- CE_test$dered_gr_differ + 0.068*(CE_test$dered_r - CE_test$dered_z) - 0.112*(CE_test$dered_r - CE_test$dered_z)
CE_test$ELG_dered_differ2 <- (CE_test$dered_r - CE_test$dered_z) + 0.555*CE_test$dered_gr_differ - 0.218*CE_test$dered_gr_differ
CE_test$ELG_dered_differ3 <- (CE_test$dered_r - CE_test$dered_z) + 0.555*CE_test$dered_gr_differ - 0.637*CE_test$dered_gr_differ

CE_test$ELG_airmass_differ1 <- CE_test$airmass_gr_differ + 0.068*(CE_test$airmass_r - CE_test$airmass_z) - 0.112*(CE_test$airmass_r - CE_test$airmass_z)
CE_test$ELG_airmass_differ2 <- (CE_test$airmass_r - CE_test$airmass_z) + 0.555*CE_test$airmass_gr_differ - 0.218*CE_test$airmass_gr_differ
CE_test$ELG_airmass_differ3 <- (CE_test$airmass_r - CE_test$airmass_z) + 0.555*CE_test$airmass_gr_differ - 0.637*CE_test$airmass_gr_differ

### 회전으로 인한 파생변수 (https://www.sdss.org/dr16/algorithms/legacy_target_selection/)
### parallel의 경우 가우스 표시인지, 소괄호의 중복표기를 막기위해 사용한건지 알 수없어 둘다 생성

CE_test$c_orth <- CE_test$ri_differ - CE_test$gr_differ/4 - 0.177
CE_test$c_orth_abs <- abs(CE_test$c_orth)  
CE_test$c_paral <- 0.7*CE_test$gr_differ + 1.2*(CE_test$ri_differ - 0.177)
CE_test$c_paral_gaus <- 0.7*CE_test$gr_differ + 1.2*floor((CE_test$ri_differ - 0.177)) 
CE_test$criterion1 <- CE_test$c_orth + CE_test$gr_differ/6
CE_test$criterion2 <- CE_test$gr_differ - 0.25*CE_test$ri_differ

CE_test$dered_c_orth <- CE_test$dered_ri_differ - CE_test$dered_gr_differ/4 - 0.177
CE_test$dered_c_orth_abs <- abs(CE_test$dered_c_orth)
CE_test$dered_c_paral <- 0.7*CE_test$dered_gr_differ + 1.2*(CE_test$dered_ri_differ - 0.177)
CE_test$dered_c_paral_gaus <- 0.7*CE_test$dered_gr_differ + 1.2*floor((CE_test$dered_ri_differ - 0.177))
CE_test$dered_criteria1 <- CE_test$dered_c_orth + CE_test$dered_gr_differ/6
CE_test$dered_criteria2 <- CE_test$dered_gr_differ - 0.25*CE_test$dered_ri_differ

CE_test$airmass_c_orth <- CE_test$airmass_ri_differ - CE_test$airmass_gr_differ/4 - 0.177
CE_test$airmass_c_orth_abs <- abs(CE_test$airmass_c_orth)  
CE_test$airmass_c_paral <- 0.7*CE_test$airmass_gr_differ + 1.2*(CE_test$airmass_ri_differ - 0.177)
CE_test$airmass_c_paral_gaus <- 0.7*CE_test$airmass_gr_differ + 1.2*floor((CE_test$airmass_ri_differ - 0.177)) 
CE_test$airmass_criterion1 <- CE_test$airmass_c_orth + CE_test$airmass_gr_differ/6
CE_test$airmass_criterion2 <- CE_test$airmass_gr_differ - 0.25*CE_test$airmass_ri_differ

### 색상 윤곽 계산 파생변수 (https://www.sdss.org/dr16/algorithms/eboss-target-selection/eboss-qso-target-selection/)

CE_test$c1 <- 0.95*CE_test$ug_differ + 0.31*CE_test$gr_differ + 0.11*CE_test$ri_differ
CE_test$c3 <- -0.39*CE_test$ug_differ + 0.79*CE_test$gr_differ + 0.47*CE_test$ri_differ
CE_test$criteria3 <- 0.55*CE_test$c1 + CE_test$c3
CE_test$criteria4 <- 0.1*CE_test$c1 + CE_test$c3

CE_test$dered_c1 <- 0.95*CE_test$dered_ug_differ + 0.31*CE_test$dered_gr_differ + 0.11*CE_test$dered_ri_differ
CE_test$dered_c3 <- -0.39*CE_test$dered_ug_differ + 0.79*CE_test$dered_gr_differ + 0.47*CE_test$dered_ri_differ
CE_test$dered_criteria3 <- 0.55*CE_test$dered_c1 + CE_test$dered_c3
CE_test$dered_criteria4 <- 0.1*CE_test$dered_c1 + CE_test$dered_c3


CE_test$airmass_c1 <- 0.95*CE_test$airmass_ug_differ + 0.31*CE_test$airmass_gr_differ + 0.11*CE_test$airmass_ri_differ
CE_test$airmass_c3 <- -0.39*CE_test$airmass_ug_differ + 0.79*CE_test$airmass_gr_differ + 0.47*CE_test$airmass_ri_differ
CE_test$airmass_criteria3 <- 0.55*CE_test$airmass_c1 + CE_test$airmass_c3
CE_test$airmass_criteria4 <- 0.1*CE_test$airmass_c1 + CE_test$airmass_c3

### 예측, 실제 필드간 차이 

CE_test$nDiffer <- CE_test$nObserve - CE_test$nDetect

##### 파이썬 작업 환경으로 데이터 저장

write.csv(CE, file =  "C:/taeoowl_py/CE_train.csv")
write.csv(CE_test, file =  "C:/taeoowl_py/CE_test.csv")
