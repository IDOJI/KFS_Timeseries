### 🟩 데이터 로드  ======================================================
path_data = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/조림/수종별 조림실적Plantation forest by tree species/3.00/2000_YRBK_0030030501.xlsx"
data = read.xlsx(path_data)
# View(data)
names(data)




### 🟧 열옮기기  ======================================================
data_colmoved = data %>% 
  relocate("구분", "구분2", .after = "ID") %>% 
  select(-"구분2") %>% 
  mutate(year = 2000) %>% 
  relocate(year, .after = "구분") %>% 
  select(-행)





### 🟪 행합 =====================================================================
# 더할 열이름들 확인
data_colmoved[4:ncol(data_colmoved)] %>% names

# 각 열별로 합계
data_rowsum = colSums(data_colmoved[4:ncol(data_colmoved)], na.rm=T) %>%
  as.data.frame %>% 
  t() %>% 
  as.data.frame

# 결과 확인
View(data_rowsum)
class(data_rowsum)
names(data_rowsum) %>% head

# 데이터 합치기
data_combined = cbind(data_colmoved[1,1:3], data_rowsum) %>% suppressWarnings()
View(data_combined)





### 🟧 열 이름 추가 ========================================================
# 합계 열이름 옮기기, 이름 변경
data_colnames = data_combined %>% 
  relocate(ends_with("계_면적"), .after = year) %>% 
  relocate(ends_with("계_본수"), .after = year) %>% 
  rename_with(~ gsub("계", "합계", .), ends_with("본수")) %>%
  rename_with(~ gsub("계", "합계", .), ends_with("면적"))

# 결과 확인
View(data_colnames)

# 데이터 프레임의 열 이름
column_names <- names(data_colnames)

# "활엽수_" 또는 "침엽수_"로 시작하지 않는 열 이름 필터링
filtered_columns <- column_names[!grepl("^활엽수_|^침엽수_", column_names)]

# 침엽수와 활엽수 목록 정의
coniferous_trees <- c("잣나무", "낙엽송", "리기다", "리기테다", "강송", "해송", "소나무")
deciduous_trees <- c("느티나무", "물푸레나무")

# 함수 정의: 접두사 추가
add_prefix <- function(column_name) {
  for (tree in coniferous_trees) {
    if (startsWith(column_name, tree)) {
      return(paste0("침엽수_", column_name))
    }
  }
  for (tree in deciduous_trees) {
    if (startsWith(column_name, tree)) {
      return(paste0("활엽수_", column_name))
    }
  }
  return(column_name)
}

# 각 열 이름에 접두사 추가
new_column_names <- sapply(filtered_columns, add_prefix) %>% unname
data.frame(filtered_columns, new_column_names) %>% View


# 데이터프레임의 열 이름을 new_column_names로 업데이트
df = data_colnames
colnames(df)[colnames(df) %in% filtered_columns] <- new_column_names
data.frame(colnames(df), colnames(data_colnames)) %>% View





### 🟨 본수, 면적 합계 비교 ================================================================
data_sum = df
names(data_sum)



# 열 이름 가져오기
column_names <- names(data_sum)

# "_계_" 문자열이 포함된 열 제외
filtered_columns <- column_names[!grepl("_합계_", column_names)]

# "활엽수_" 또는 "침엽수_"로 시작하는 "본수"와 "면적" 열 필터링
leafy_count_columns <- filtered_columns[grepl("^활엽수_.*_본수$", filtered_columns)]
leafy_area_columns <- filtered_columns[grepl("^활엽수_.*_면적$", filtered_columns)]
coniferous_count_columns <- filtered_columns[grepl("^침엽수_.*_본수$", filtered_columns)]
coniferous_area_columns <- filtered_columns[grepl("^침엽수_.*_면적$", filtered_columns)]

# "활엽수"와 "침엽수" 열의 "본수"와 "면적"의 합계 계산
data_sums <- data_sum %>%
  summarize(
    활엽수_총_본수 = rowSums(select(., all_of(leafy_count_columns)), na.rm = TRUE),
    활엽수_총_면적 = rowSums(select(., all_of(leafy_area_columns)), na.rm = TRUE),
    침엽수_총_본수 = rowSums(select(., all_of(coniferous_count_columns)), na.rm = TRUE),
    침엽수_총_면적 = rowSums(select(., all_of(coniferous_area_columns)), na.rm = TRUE)
  )

# "계" 열과 비교
comparison <- data_sum %>%
  select(ID, 구분, year, ends_with("계_본수"), ends_with("계_면적")) %>%
  bind_cols(data_sums)
View(comparison)
# "계" 열과 비교 및 차이 열 생성
comparison <- data_sum %>%
  select(ID, 구분, year, ends_with("합계_본수"), ends_with("합계_면적")) %>%
  bind_cols(data_sums) %>%
  mutate(
    활엽수_본수_차이 = 활엽수_총_본수 - 활엽수_합계_본수,
    활엽수_면적_차이 = 활엽수_총_면적 - 활엽수_합계_면적,
    침엽수_본수_차이 = 침엽수_총_본수 - 침엽수_합계_본수,
    침엽수_면적_차이 = 침엽수_총_면적 - 침엽수_합계_면적
  )

# 결과 확인
View(comparison)


library(dplyr)

# "_계_" 문자열이 포함된 열 제외
filtered_columns <- column_names[!grepl("_합계_", column_names)]
# 
# # "활엽수_" 또는 "침엽수_"로 시작하는 "본수"와 "면적" 열 필터링
# leafy_count_columns <- filtered_columns[grepl("^활엽수_.*_본수$", filtered_columns)]
# leafy_area_columns <- filtered_columns[grepl("^활엽수_.*_면적$", filtered_columns)]
# coniferous_count_columns <- filtered_columns[grepl("^침엽수_.*_본수$", filtered_columns)]
# coniferous_area_columns <- filtered_columns[grepl("^침엽수_.*_면적$", filtered_columns)]
# 
# # "활엽수"와 "침엽수" 열의 "본수"와 "면적"의 합계 계산
# data_sums <- data_sum %>%
#   summarize(
#     활엽수_총_본수 = rowSums(select(., all_of(leafy_count_columns)), na.rm = TRUE),
#     활엽수_총_면적 = rowSums(select(., all_of(leafy_area_columns)), na.rm = TRUE),
#     침엽수_총_본수 = rowSums(select(., all_of(coniferous_count_columns)), na.rm = TRUE),
#     침엽수_총_면적 = rowSums(select(., all_of(coniferous_area_columns)), na.rm = TRUE)
#   )
# 
# # data_sum에 새로운 열 추가 및 비교
# data_sum <- data_sum %>%
#   bind_cols(data_sums) %>%
#   mutate(
#     활엽수_본수_차이 = abs(활엽수_총_본수 - 활엽수_합계_본수),
#     활엽수_면적_차이 = abs(활엽수_총_면적 - 활엽수_합계_면적),
#     침엽수_본수_차이 = abs(침엽수_총_본수 - 침엽수_합계_본수),
#     침엽수_면적_차이 = abs(침엽수_총_면적 - 침엽수_합계_면적)
#   ) %>% 
#   relocate(ends_with("총_면적"), .after = year) %>% 
#   relocate(ends_with("합계_면적"), .after = year) %>% 
#   relocate(ends_with("면적_차이"), .after = year) %>% 
#   relocate(ends_with("총_본수"), .after = year) %>% 
#   relocate(ends_with("합계_본수"), .after = year) %>% 
#   relocate(ends_with("본수_차이"), .after = year)
#   

# 결과 확인
View(data_sum)
names(data_sum)



### 🟨 면적 열 제외 ================================================================
data_sum_2 = data_sum %>% select(-all_of(grep("면적", names(data_sum), value=T)))
names(data_sum_2)



### 🟨 =======================================================================
data_sum_2 = data_sum_2 %>% 
  relocate("침엽수_기타_본수", .after = 활엽수_합계_본수) %>% 
  relocate("활엽수_기타_본수", .after = "침엽수_기타_본수")
names(data_sum_2)


### 🟨 Export ================================================================
path_save = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/조림/수종별 조림실적Plantation forest by tree species/Combined"
file_name = "3.Combined_00_직접입력.xlsx"
write.xlsx(data_sum_2, file.path(path_save, file_name))
# data_sum




