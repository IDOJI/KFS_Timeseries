# 🟥 (23년도 + 04~22년도) + 00년도 ===================================================================================================
## 🟩 데이터 로드 ===========================================================================================
path_data_1 = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID/@_조림/(@완료)수종별 조림실적Plantation forest by tree species/@완료/6.전체데이터합치기/1.Combined.xlsx"
path_data_2000 = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID/@_조림/(@완료)수종별 조림실적Plantation forest by tree species/@완료/3.2000년도_수제작.xlsx"

data_1 = read.xlsx(path_data_1)
data_2000 = read.xlsx(path_data_2000)
View(data_2000)


data_1_sub = data_1[1:18]


## 🟩 열 이름 확인 ===========================================================================================
names(data_2000) %in% names(data_1_sub)
names(data_1_sub)[!names(data_1_sub) %in% names(data_2000)]


## 🟩 열 이름 변경 ===========================================================================================
names(data_2000)[names(data_2000) == "침엽수_계_면적"] = "침엽수_합계_면적"
names(data_2000)[names(data_2000) == "활엽수_계_면적"] = "활엽수_합계_면적"
names(data_2000)[names(data_2000) == "느티나무_면적"] = "활엽수_느티나무_면적"
names(data_2000)[names(data_2000) == "물푸레나무_면적"] = "활엽수_물푸레_면적"
names(data_2000)[names(data_2000) == "활엽수_상수리나무_면적"] = "활엽수_상수리_면적"
# View(data_2000)



## 🟩 열 이름 확인 ===========================================================================================
names(data_1_sub)[!names(data_1_sub) %in% names(data_2000)]
all(names(data_1_sub) %in% names(data_2000))

names(data_2000)
names(data_1_sub)






## 🟨 2002 활엽수, 침엽수 기타 만들기 ===========================================================================================
library(dplyr)

# 제외할 열 목록
excluded_cols <- c("침엽수_소나무_면적", "침엽수_잣나무_면적", "침엽수_낙엽송_면적", 
                   "침엽수_리기다_면적", "침엽수_삼나무_면적", "침엽수_해송_면적", 
                   "침엽수_편백_면적", "침엽수_기타_면적", "활엽수_느티나무_면적", 
                   "활엽수_물푸레_면적", "활엽수_벚나무_면적", "활엽수_상수리_면적", 
                   "활엽수_자작나무_면적", "활엽수_고로쇠_면적", "활엽수_기타_면적",
                   "침엽수_합계_면적", "활엽수_합계_면적")

# 합계 계산에 실제로 사용되는 열을 제외하고 나머지 열만 선택
data_other_conifers <- data_2000 %>%
  select(contains("침엽수"), -one_of(excluded_cols)) %>%
  select(-ends_with("_합계_면적")) %>%
  replace(is.na(.), 0)  # NA 값을 0으로 대체

data_other_broadleaf <- data_2000 %>%
  select(contains("활엽수"), -one_of(excluded_cols)) %>%
  select(-ends_with("_합계_면적")) %>%
  replace(is.na(.), 0)  # NA 값을 0으로 대체

# 각 카테고리 별로 합 계산
other_conifers_sum <- rowSums(data_other_conifers)
other_broadleaf_sum <- rowSums(data_other_broadleaf)

# 결과를 새로운 데이터 프레임에 저장하고, 사용된 열들 제거
data_2000_2 <- data_2000 %>%
  mutate(침엽수_기타_면적 = other_conifers_sum, 
         활엽수_기타_면적 = other_broadleaf_sum) %>%
  select(-one_of(names(data_other_conifers)), -one_of(names(data_other_broadleaf)))  # 계산에 사용된 열들 제거

# 결과 출력
print(data_2000_2)
View(data_2000_2)


# 내가 직접 계산 비교
# 1621 : 침엽수 기타
test  = data_2000 %>% select(-all_of(excluded_cols))
names(test)
what_broadleaf = grep("활엽수", names(test), value=T)
what_conifers = grep("침엽수", names(test), value=T)

test %>% 
  select(starts_with("침엽수")) %>% 
  rowSums(na.rm=T)

data_2000_2$침엽수_기타_면적


test %>% 
  select(starts_with("활엽수")) %>% 
  rowSums(na.rm=T)


data_2000_2$활엽수_기타_면적

# 동일함


## 🟨 2002 각 열 별 합치기 ===========================================================================================
data_2000_2 = data_2000_2 %>% relocate(c("year", "구분", "구분2"), .after = c("ID"))
names(data_2000_2)
summed = colSums(data_2000_2[5:ncol(data_2000_2)], na.rm=T) %>% 
  matrix(nrow=1) %>% 
  as.data.frame

data_2000_3 = data_2000_2[1,]
data_2000_3[5:ncol(data_2000_3)] = summed
View(data_2000_3)



## 🟨 2002 합계 비교 ===========================================================================================
names(data_2000_3)
library(dplyr)

# 침엽수 관련 열들의 합 계산
conifer_areas <- data_2000_3 %>%
  select(starts_with("침엽수"), -contains("합계")) %>% # 합계 열 제외
  rowSums(na.rm = TRUE)

# 활엽수 관련 열들의 합 계산
broadleaf_areas <- data_2000_3 %>%
  select(starts_with("활엽수"), -contains("합계")) %>% # 합계 열 제외
  rowSums(na.rm = TRUE)

# 합계 열과 계산된 합의 비교
conifer_match <- conifer_areas == data_2000_3$침엽수_합계_면적
broadleaf_match <- broadleaf_areas == data_2000_3$활엽수_합계_면적

# 결과를 데이터 프레임으로 저장
results <- data.frame(
  Calculated_Conifer_Sum = conifer_areas,
  Original_Conifer_Sum = data_2000_3$침엽수_합계_면적,
  Conifer_Sum_Match = conifer_match,
  
  Calculated_Broadleaf_Sum = broadleaf_areas,
  Original_Broadleaf_Sum = data_2000_3$활엽수_합계_면적,
  Broadleaf_Sum_Match = broadleaf_match
)

# 결과 출력
print(results)
# 결과는 같지 않지만 일단 진행


## 🟧 데이터 합치기 ===========================================================================================
data_2000_3$구분2 = NULL
all(names(data_1_sub) %in% names(data_2000_3))

data_1_sub %>% names

data_1_new = cbind(data_1_sub, data_1[19:ncol(data_1)])
names(data_1_new)
data_1_new$year = data_1_new$침엽수_year
data_1_new$침엽수_year = data_1_new$활엽수_year = NULL

data_1_new$구분 %>% class
data_2000_3$구분 %>% class
data_2000_3$구분 = as.character(data_2000_3$구분)

data_1_new$year %>% class
data_2000_3$year %>% class
data_2000_3$year = as.numeric(data_2000_3$year)

data_combined = bind_rows(list(data_2000_3, data_1_new)) %>% 
  relocate(year)
data_combined %>% names

## 🟧 내보내기 ===================================================================
path_save = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID/@_조림/(@완료)수종별 조림실적Plantation forest by tree species/@완료/6.전체데이터합치기"
file_name = "2.Combined.xlsx"
write.xlsx(data_combined, file.path(path_save, file_name))





