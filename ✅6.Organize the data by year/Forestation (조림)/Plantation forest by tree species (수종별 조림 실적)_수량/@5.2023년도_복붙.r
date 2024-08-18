# 🟥 데이터로드 ===================================================================================================
path_data = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID/@_조림/(@완료)수종별 조림실적Plantation forest by tree species/@완료/5.2023년도_복붙/2023_YRBK_00530405.xlsx"
data = read.xlsx(path_data)

View(data)


# 🟥 "그루수"열 제외 ===================================================================================================
# "그루수" 문자열을 포함하는 열을 제외
data_filtered <- data %>%
  select(-matches("그루수"))


# 🟥 합계열 옮기기 ===================================================================================================
# "합계"를 포함하는 열을 찾고 그 인덱스를 얻습니다
sum_columns <- grep("합계", names(data_filtered))

# 모든 열의 인덱스를 기본 순서대로 생성
all_columns <- 1:ncol(data_filtered)

# "합계"를 포함하는 열을 제외한 나머지 열
non_sum_columns <- all_columns[!all_columns %in% sum_columns]

# "합계" 열을 2번째 위치로 이동
# 첫 번째 열 다음에 "합계" 열이 오고, 나머지 열이 이어지도록 배열합니다.
new_order <- c(non_sum_columns[1], sum_columns, non_sum_columns[-1])

# 새로운 순서로 데이터프레임을 재구성
data_filtered_reordered <- data_filtered[, new_order]

# 결과를 확인
data_filtered_reordered %>% View



# 🟥 합계 비교  ===================================================================================================
names(data_filtered_reordered)
library(dplyr)

# 침엽수와 활엽수의 합계를 계산하고 그 값을 해당 합계 열과 비교
data_checked <- data_filtered_reordered %>%
  mutate(침엽수_계산된_합계 = rowSums(select(., starts_with("침엽수_"), -침엽수_합계_면적), na.rm = TRUE),
         침엽수_합계_일치 = 침엽수_계산된_합계 == 침엽수_합계_면적,
         활엽수_계산된_합계 = rowSums(select(., starts_with("활엽수_"), -활엽수_합계_면적), na.rm = TRUE),
         활엽수_합계_일치 = 활엽수_계산된_합계 == 활엽수_합계_면적) %>%
  select(구분, 침엽수_합계_면적, 침엽수_계산된_합계, 침엽수_합계_일치,
         활엽수_합계_면적, 활엽수_계산된_합계, 활엽수_합계_일치)

# 결과를 출력
print(data_checked)
View(data_checked)


# 🟥 내보내기  ===================================================================================================
path_save = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID/@_조림/(@완료)수종별 조림실적Plantation forest by tree species/@완료"
file_name = paste0("5.2023년도_복붙.xlsx")
write.xlsx(data_filtered, file.path(path_save, file_name))



