# 🟥 23년도 + 04~22년도 ===================================================================================================
## 🟩 데이터 로드 ===========================================================================================
path_data_2023 = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID/@_조림/(@완료)수종별 조림실적Plantation forest by tree species/@완료/5.2023년도_복붙.xlsx"
path_data_2022 = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID/@_조림/(@완료)수종별 조림실적Plantation forest by tree species/@완료/4.2004~2022 침엽수, 활엽수 데이터합치기.xlsx"


data_2023 = read.xlsx(path_data_2023)
data_2022 = read.xlsx(path_data_2022)
data_2022_2 = data_2022[1:18]


## 🟩 열 이름 확인 ===========================================================================================
all(names(data_2023) %in% names(data_2022_2))
all(names(data_2022_2) %in% names(data_2023))
inter = intersect(names(data_2022_2), names(data_2023))

setdiff(names(data_2022_2), inter)
setdiff(names(data_2023), inter)




## 🟩 열 이름 변경 및 열 추가 ===========================================================================================
data_2022_3 = data_2022_2 %>% 
  rename("침엽수_합계_면적" := "침엽수_계_면적") %>% 
  rename("활엽수_합계_면적"  := "활엽수_계_면적" )

data_2023$침엽수_리기다_면적 = NA



## 🟩 열 이름 확인 ===========================================================================================
all(names(data_2022_3) %in% names(data_2023))
all(names(data_2023) %in% names(data_2022_3))

data_2023$침엽수_year = 2023
data_2023$활엽수_year = 2023


## 🟨 합치기 ===========================================================================================
class(data_2023$구분) 
data_2023$구분 = as.character(data_2023$구분)
data_combined = bind_rows(list(cbind(data_2022_3, data_2022[19:ncol(data_2022)]), data_2023))
# names(data_2022_3)
# names(data_2023)

# "합계"를 포함하는 열 이름 찾기
col_with_sum <- grep("합계", names(data_combined), value = TRUE)
# "합계"를 포함하는 열을 두 번째 열로 옮기기
data_combined <- data_combined %>%
  select(1, all_of(col_with_sum), everything())

View(data_combined)


## 🟨 합계 면적 확인 ===========================================================================================
data_combined %>% names

library(dplyr)

# 침엽수와 활엽수 면적 열 선택 (_면적으로 끝나는 열만)
data_conifer_areas = data_combined %>%
  select(ends_with("_면적")) %>%
  select(starts_with("침엽수")) %>%
  select(-contains("합계"))  # 합계 열 제외

data_broadleaf_areas = data_combined %>%
  select(ends_with("_면적")) %>%
  select(starts_with("활엽수")) %>%
  select(-contains("합계"))  # 합계 열 제외

# 각 그룹의 면적 합계 계산
sum_conifer_areas = rowSums(data_conifer_areas, na.rm = TRUE)
sum_broadleaf_areas = rowSums(data_broadleaf_areas, na.rm = TRUE)

# 새로운 데이터 프레임 생성
data_summary <- tibble(
  Original_Conifer_Sum = data_combined$`침엽수_합계_면적`,
  Calculated_Conifer_Sum = sum_conifer_areas,
  Conifer_Match = data_combined$`침엽수_합계_면적` == sum_conifer_areas,
  
  Original_Broadleaf_Sum = data_combined$`활엽수_합계_면적`,
  Calculated_Broadleaf_Sum = sum_broadleaf_areas,
  Broadleaf_Match = data_combined$`활엽수_합계_면적` == sum_broadleaf_areas
)

# 결과 출력
print(data_summary)
View(data_summary)



## 🟨 Export ===========================================================================================
path_save = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID/@_조림/(@완료)수종별 조림실적Plantation forest by tree species/@완료/6.전체데이터합치기"
write.xlsx(data_combined, file.path(path_save, "1.Combined.xlsx"))










