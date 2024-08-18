## 🟧 2.국유림 민유림 ===================================================================================
### 🟩 데이터 로드  ======================================================
path_data = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/조림/수종별 조림실적Plantation forest by tree species/2.82~99"
data.list = lapply(list.files(path_data, full.names= T), read.csv) %>% 
  setNames(list.files(path_data))
# data.list [[1]] %>% View
# names(data.list)








### 🟩 연도추가  =============================================================================
# 1999연보
n_length = length(data.list)
# View(data.list[[n_length]])
data.list[[n_length]][1,3] = paste0("1998_", data.list[[n_length]][1,3])
data.list[[n_length]][2,3] = paste0("1998_", data.list[[n_length]][2,3])


# 1998연보
data.list[[n_length - 1]][1,3] = paste0("1997_", data.list[[n_length - 1]][1,3])
data.list[[n_length - 1]][2,3] = paste0("1997_", data.list[[n_length - 1]][2,3])









### 🟩 특정 열들만 추출  =========================================================================
data.list[[1]] %>% names




# 각 데이터프레임에서 필요한 열을 추출하는 함수 정의
select_columns <- function(df) {
  # 1~3번째 열\
  fixed_cols <- df[, 1:3]
  
  # "계" 문자열을 포함하지만 "민유림" 또는 "국유림"을 포함하지 않는 열 (4번째 열부터 "Categorized_L3_New" 열 전까지)
  kei_cols_indices <- grep("계", names(df))
  exclude_indices <- grep("민유림|국유림", names(df))
  kei_cols_indices <- setdiff(kei_cols_indices, exclude_indices)
  kei_cols <- df[, kei_cols_indices]
  
  # "Categorized_L3_New"에서 마지막 열까지
  start_idx <- which(names(df) == "Categorized_L3_New")
  end_cols <- df[, start_idx:ncol(df)]
  
  # 열들을 결합
  result <- cbind(fixed_cols, kei_cols, end_cols)
  return(result)
}

# 각 데이터프레임에 함수 적용
selected_data_list <- lapply(data.list, select_columns)
selected_data_list[[1]] %>% View






### 🟩 열이름 확인 ======================================================
# sapply(selected_data_list, names) %>% View

# 3번째부터 18번째 열의 열 이름을 확인하는 함수 정의
check_column_names <- function(df) {
  return(names(df)[3:18])
}

# 각 데이터프레임에서 3번째부터 18번째 열의 열 이름 추출
column_name_list <- lapply(selected_data_list, check_column_names)

# 첫 번째 데이터프레임의 열 이름을 기준으로 비교
reference_names <- column_name_list[[1]]

# 모든 데이터프레임의 열 이름이 동일한지 확인
all_identical <- all(sapply(column_name_list, function(x) identical(x, reference_names)))

if (all_identical) {
  print("모든 데이터프레임의 3번째부터 18번째 열 이름이 동일합니다.")
} else {
  print("모든 데이터프레임의 3번째부터 18번째 열 이름이 동일하지 않습니다.")
}











# 🟦 각 데이터프레임에서 연도 행만 남기기 =============================================================================================
# 데이터프레임 리스트를 순회하며 작업 수행
sapply(selected_data_list, function(x){
  names(x)[2]
}) %>% unname %>% unique
selected_data_list[[1]] %>% View

selected_data_list_2 <- lapply(selected_data_list, function(df) {
  # 3번째 열의 데이터에서 4자리 연도를 포함하는 행 추출
  df[[3]] %>% unique
}) %>% unlist %>% unique %>% unname %>% unique



# 연도 행만 남기기
# 주어진 리스트 selected_data_list의 각 요소 df에 대해 필터링 수행
filtered_data_list <- lapply(selected_data_list, function(df) {
  # 3번째 열의 값이 숫자로 시작하는지 확인
  is_numeric_start <- grepl("^[0-9]{4}_", df[, 3])
  # 조건을 만족하는 행들만 필터링하여 반환
  df[is_numeric_start, ]
})





# 🟦 실제 연보와 값 비교 및 값 교체 =============================================================================================
data_list = filtered_data_list

# data_list[[1]]

###  ✴️ 1982 ===========================================================================================================
id = "YRBK_00120304"
ind = grep(id, names(data_list))
data_1 = data_list[[ind]]
View(data_1)


data_1$잣나무_계[which(data_1$잣나무_계 == 27615)] = 27612

tree = "강송_계"
data_1[[tree]][which(data_1[[tree]] == 3636)] = 3434

data_list[[ind]] = data_1



###  ✴️ 1983 ===========================================================================================================
id = "YRBK_00130304"
ind = grep(id, names(data_list))
data_1 = data_list[[ind]]
# View(data_1)

data_1$잣나무_계[which(data_1$잣나무_계 == 27615)] = 27612

data_list[[ind]] = data_1





###  ✴️ 1984 ===========================================================================================================
id = "YRBK_00140304"
ind = grep(id, names(data_list))
data_1 = data_list[[ind]]
# View(data_1)

data_1$잣나무_계[which(data_1$잣나무_계 == 27615)]
data_1$잣나무_계[which(data_1$잣나무_계 == 27615)] = 27612


data_list[[ind]] = data_1





###  ✴️ 1985 ===========================================================================================================
id = "YRBK_00150304"
ind = grep(id, names(data_list))
data_1 = data_list[[ind]]
# View(data_1)

data_1$오동_계
new_values = c(624, 383, 100, 
               60, 277, 170, 
               377, 227, 175, 
               102)
data_1$오동_계 = new_values
# data_1$잣나무_계[which(data_1$잣나무_계 == 27615)] = 27612


data_list[[ind]] = data_1




###  ✴️ 1986 ===========================================================================================================
id = "YRBK_00160304"
ind = grep(id, names(data_list))
data_1 = data_list[[ind]]
# View(data_1)
# data_1 $ID %>% unique

data_1$잣나무_계[which(data_1$잣나무_계 == 27615)] = 27612
data_1$오동_계
new_values = c(100, 60, 277, 
               170, 377, 227, 
               175, 102, 40,
               26)
data_1$오동_계
new_values
data_1$오동_계 = new_values

data_list[[ind]] = data_1




###  ✴️ 1987 ===========================================================================================================
id = "YRBK_00170304"
ind = grep(id, names(data_list))
data_1 = data_list[[ind]]
# View(data_1)
# data_1 $ID %>% unique

data_1$현사시_계[which(data_1$현사시_계 == 2055)] = 6055
data_1$현사시_계[which(data_1$현사시_계 == 131)] = 1313
data_1$현사시_계


data_1$오동_계[which(data_1$오동_계 == 185)] = 175

data_list[[ind]] = data_1





###  ✴️ 1988 ===========================================================================================================
id = "YRBK_00180304"
ind = grep(id, names(data_list))
data_1 = data_list[[ind]]
# View(data_1)
# data_1 $ID %>% unique

data_1$현사시_계[which(data_1$현사시_계 == 2055)] = 6055
data_1$현사시_계[which(data_1$현사시_계 == 131)] = 1313
data_1$현사시_계


data_1$오동_계[which(data_1$오동_계 == 185)] = 175
data_list[[ind]] = data_1



###  ✴️ 1989 ===========================================================================================================
# id = "YRBK_00190304"
# ind = grep(id, names(data_list))
# data_1 = data_list[[ind]]
# # View(data_1)
# # data_1 $ID %>% unique
# 
# data_1$현사시_계[which(data_1$현사시_계 == 2055)] = 6055
# data_1$현사시_계[which(data_1$현사시_계 == 131)] = 1313
# data_1$현사시_계
# 
# 
# data_1$오동_계[which(data_1$오동_계 == 185)] = 175
# 
# data_list[[ind]] = data_1






# 🟨 데이터 합치기 ======================================================
selected_data_list = data_list
combined_data = do.call(rbind, selected_data_list) %>% 
  arrange(구분_1)
# View(combined_data)
rownames(combined_data) = NULL
combined_data[[2]] = NULL

# combined_data %>% View







# 🟨 본수행만 추출 ==============================================================================
# combined_data의 두 번째 열에서 숫자 4자리가 포함된 행 추출
second_column <- combined_data[[2]]
has_year <- grepl("\\d{4}", second_column)
rows_with_year <- combined_data[has_year, ]

# "본수"이라는 문자열을 포함하는 행 추출
contains_area <- grepl("본수", rows_with_year[[2]])

# 최종적으로 "면적"을 포함하는 행만 추출
final_rows <- rows_with_year[contains_area, ]

# 결과 출력
print(final_rows)
View(final_rows)






# 🟪 열 이름 바꾸기 =============================================================================================
names(final_rows)[2] = "구분"

library(dplyr)
library(stringr)

# 열 이름을 "_계"에서 "_본수_계"로 변경
final_rows_2 <- final_rows %>%
  rename_with(~ str_replace_all(., "_계", "_본수_계")) %>% 
  rename_with(~ str_replace_all(., "계_", ""))



# 🟪 연도만 추출 =============================================================================================
library(stringr)

# 2번째 열의 연도 부분만 추출하여 해당 열의 값으로 대체
final_rows_2[[2]] <- str_extract(final_rows_2[[2]], "^\\d{4}")






# 🟦 합계 비교 계산 =============================================================================================
library(dplyr)

# "본수_계" 열을 제외한 "_본수_계"로 끝나는 열들의 합계를 구해 새로운 열 Directly_Summed 생성
final_rows_3 <- final_rows_2 %>%
  rowwise() %>%
  mutate(Directly_Summed = sum(c_across(ends_with("_본수_계")), na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(Difference = `본수_계` - Directly_Summed) %>% # "본수_계"와 "Directly_Summed" 열의 차이를 구해 새로운 열 Difference 생성
  relocate(Directly_Summed, .after = "본수_계") %>% 
  relocate(Difference, .after = Directly_Summed) %>% 
  relocate(year, .after = "구분")


# 결과 확인
final_rows_3 %>% filter(구분 == "1981") %>% View
final_rows_3 %>% View




# 🟦 차이가 0인 연도들 =============================================================================================
# 차이가 0인 연도들과 0이 아닌 연도들 추출
zero_df = final_rows_3 %>% filter(Difference == 0)
non_zero_df = final_rows_3 %>% filter(Difference != 0)
View(non_zero_df)
View(zero_df)
non_zero_to_check_df = non_zero_df %>% filter(!구분 %in% zero_df$구분)
View(non_zero_to_check_df)

# -> 차이 별로 나 않고, 확인한 연보이므로 그대로 사용
new_combined = rbind(zero_df, non_zero_to_check_df) %>% 
  arrange(구분, year)
View(new_combined)



# 🟨 데이터 추출 ======================================================
# dplyr 패키지 로드
library(dplyr)

# "구분" 별로 "year" 값이 가장 큰 행만 남기기
filtered_df <- new_combined %>%
  group_by(구분) %>%
  filter(year == max(year)) %>%
  ungroup() %>% 
  arrange(구분, year)


# 결과 확인
print(filtered_df)
filtered_df %>% View
#


# 🟨 연속 연도만 남았는지 확인 ======================================================
years = filtered_df[[2]] %>% as.numeric

# 연도가 연속적인지 확인
is_consecutive <- all(diff(years) == 1)

# 결과 출력
if (is_consecutive) {
  print("연도가 연속적입니다.")
} else {
  print("연도가 연속적이지 않습니다.")
}



# 🟨 활엽수 침엽수 ======================================================
filtered_df %>% names
library(dplyr)

# 나무 이름 목록
coniferous_trees <- c("잣나무_본수_계", "낙엽송_본수_계", "삼나무_본수_계", "편백_본수_계", 
                      "리기다_본수_계", "테다_본수_계", "리기테다_본수_계", "강송_본수_계", 
                      "해송_본수_계")

leafy_trees <- c("밤나무_본수_계", "이태리포플러_본수_계", "현사시_본수_계", "오동_본수_계")

# 열 이름 변경
filtered_df <- filtered_df %>%
  rename_with(~ ifelse(. %in% coniferous_trees, paste0("침엽수_", .), 
                       ifelse(. %in% leafy_trees, paste0("활엽수_", .), .)))

# 결과 확인
print(names(filtered_df))

filtered_df = filtered_df %>% rename(본수_합계 = 본수_계)


# 열 이름에서 "_계"를 제거하는 코드
new_colnames <- gsub("_계$", "", colnames(filtered_df))

# 데이터프레임에 새로운 열 이름을 적용
colnames(filtered_df) <- new_colnames

# 결과 확인
print(colnames(filtered_df))


# 🟨 Export ======================================================
path_save = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/조림/수종별 조림실적Plantation forest by tree species/Combined"
write.xlsx(filtered_df, file.path(path_save, "2.Combined_82~99_국유림민유림.xlsx"))












