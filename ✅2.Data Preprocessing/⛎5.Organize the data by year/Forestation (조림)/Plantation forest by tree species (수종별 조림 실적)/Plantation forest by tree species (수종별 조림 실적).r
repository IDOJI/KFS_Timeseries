# 🟥 2000년 연보에서 수종별조림실적 없는 데이터 ==========================================
## 🟧 @데이터 확인 ===================================================================================
# 2000년도 없는 것 확인
# 이전연도 : "1999_YRBK_00290304"
# id_2000 = "YRBK_00300305" # 내가 임의로 지정한 ID
# # "YRBK_00300305"
# path_data = "/Users/Ido/Documents/DataAnalysis/KFS_Timeseries/0.Raw Data/DB_20240415/TB_NDI_MRV_STTST_YRBK_OF_FRSTR_DATA_20240415.xlsx"
# raw_data = read.xlsx(path_data)
# raw_data %>% filter(연보.ID == id_2000)
# 
# hdr  = read.xlsx("/Users/Ido/Documents/DataAnalysis/KFS_Timeseries/0.Raw Data/DB_20240415/TB_NDI_MRV_STTST_YRBK_OF_FRSTR_HDR_20240415.xlsx")
# hdr %>% filter(연보.ID == id_2000)
# hdr %>% filter(연보.ID == "YRBK_0030030501")
# 
# data = readRDS("/Users/Ido/Documents/DataAnalysis/KFS_Timeseries/3.Data with HDR/8.3.Combined Data.rds")
# data[names(data) == id_2000][[1]] %>% View
# 
# yb = read.csv("/Users/Ido/Documents/DataAnalysis/KFS_Timeseries/2.Rearrange YB names/4.Added YB ID.csv")
# yb_2000 = yb %>% filter(year == 2000)
# yb_2000$Categorized_L3_New %>% unique
# yb_2000 %>% filter(Categorized_L3_New == "수종별 조림실적_Plantation forest by tree species") %>% View
# 
# # 정리된 엑셀 불러오기
# path_seedling = "/Users/Ido/Documents/DataAnalysis/KFS_Timeseries/4.Exported Data_by ID/@_조림/(@부분완료)수종별 조림실적Plantation forest by tree species/수종별 조림실적Plantation forest by tree species___그루수.xlsx"
# seedling = readxl::read_xlsx(path_seedling)
# 
# seedling_2 = extract_unique_years(seedling)
# View(seedling_2)



## 🟧 @임의로 복사한 파일이름 수정  ===================================================================================
# # 필요 패키지 로드
# library(stringr)
# 
# # 디렉토리 경로
# dir_path <- "/Users/Ido/Documents/DataAnalysis/KFS_Timeseries/4.Exported Data_by ID/@_조림/(@부분완료)수종별 조림실적Plantation forest by tree species/@완료/완료"
# 
# # 디렉토리에서 "copy"가 포함된 CSV 파일 목록 가져오기
# file_names <- list.files(dir_path, pattern = "copy.*\\.csv$")
# 
# # 파일 이름 변경
# for (file_name in file_names) {
#   # 새로운 파일 이름 생성
#   new_file_name <- file_name %>%
#     str_replace("2001", "2000") %>%
#     str_remove(" copy")
#   
#   # 파일 이름 변경
#   file.rename(file.path(dir_path, file_name), file.path(dir_path, new_file_name))
# }



## 🟧 손으로 복붙한 데이터 합치기 ==============================================================================================
# path_data_2000 = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID/@_조림/(@부분완료)수종별 조림실적Plantation forest by tree species/2000_YRBK_0030030501.xlsx"
# data_2000 = path_data_2000 %>% 
#   readxl::read_xlsx() %>% 
#   mutate_if(is.character, ~na_if(., "NA")) %>% # 데이터 프레임 data_2000에서 문자열 "NA"를 실제 NA로 변환
#   mutate_at(vars(5:ncol(.)), as.numeric) %>% # 5번째 열부터 numeric
#   slice(-6)
# 
# data_2000_selected = data_2000[,5:ncol(data_2000)]
# data_2000_summed = colSums(data_2000_selected, na.rm=T) 
# data_2000_summed_df = matrix(data_2000_summed, nrow = 1) %>% 
#   as.data.frame %>% 
#   setNames(names(data_2000_summed))
# 
# combined = cbind(Classification = 1999, data_2000_summed_df)
# 
# path_save = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID/@_조림/(@부분완료)수종별 조림실적Plantation forest by tree species"
# 
# # 데이터 프레임을 엑셀 파일로 저장
# library(openxlsx)
# write.xlsx(combined, file = file.path(path_save, "2000_YRBK_0030030501_summed.xlsx"))



# 🟥 1982~ 면적 등 계산 ======================================================================================
path_data = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID/@_조림/(@완료)수종별 조림실적Plantation forest by tree species/수종별 조림실적Plantation forest by tree species___면적.xlsx"
data = read.xlsx(path_data)

# 열이름에 활엽수, 침엽수 추가
data %>% names

# 침엽수 : 



# 🟥 특정 연도별  ======================================================================================
path_area = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID/@_조림/(@완료)수종별 조림실적Plantation forest by tree species/수종별 조림실적Plantation forest by tree species___면적.xlsx"
library(dplyr)
library(openxlsx)

# Load data and select specific columns
area <- read.xlsx(path_area) %>%
  select(1:5) %>%
  rename(year = 구분_Classification) %>%
  mutate(Remarks = NA) %>%
  mutate(Remarks = ifelse(is.na(.[, 4]) & is.na(.[, 5]) & !is.na(.[, 3]), "Only total values present in the original data", Remarks)) %>% 
  mutate(across(2, ~ str_extract(., "^\\d{4}"))) %>%  # 2번째 열에서 연도만 남기기
  mutate(across(3:5, as.numeric)) %>%  # 3,4,5번째 열은 numeric으로 변환
  arrange(year, ID)

latest_values <- area %>%
  group_by(year) %>%
  slice_tail(n = 1) %>%
  ungroup()

View(latest_values)

# 모든 연도가 있는지 확인
check_continuous_years(latest_values$year)


path_save = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID/@_조림/(@완료)수종별 조림실적Plantation forest by tree species"
write.xlsx(latest_values, file.path(path_save, "수종별조림실적_면적(1960~2021).xlsx"))




# 🟥  기타 면적 계산  ======================================================================================
path_area = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID/@_조림/(@완료)수종별 조림실적Plantation forest by tree species/수종별 조림실적Plantation forest by tree species___면적.xlsx"
area = read.xlsx(path_area)
View(area)

# 217번째 행에서 NA가 아닌 열의 이름
non_na_columns <- names(data)[!is.na(data[217, ])]
print(non_na_columns)
names(data)[!is.na(data[218, ])]
# 활엽수 : 
# 침엽수 : 리기다, 낙엽송, 잣나무, 삼나무, 









# 🟥 데이터 합치기  ======================================================================================
## 🟧 1.Total만 존재하는 경우 ===================================================================================
### 🟨 데이터 로드 ======================================================
path_data = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID/@_조림/(@완료)수종별 조림실적Plantation forest by tree species/@완료/1.Total 면적만 존재"
data.list = lapply(list.files(path_data, full.names=T), read.csv) %>% 
  setNames(tools::file_path_sans_ext(list.files(path_data)))

# 1,2,3,4,5 
# year, NAME_L1, NAME_L2, NAME_L3, NAME_L4, unit_L1, unit_L2, unit_L3, unit_L4, unit_L5 
# 특정 열 선택
### 🟨 특정 열들만 추출 ======================================================
# 데이터 프레임 리스트에서 필요한 열을 선택하는 함수 정의
select_columns <- function(df) {
  df %>%
    select(1:5, year, NAME_L1, NAME_L2, NAME_L3, NAME_L4, unit_L2, unit_L3, unit_L4, unit_L5)
}

# 데이터 리스트에 함수 적용하여 각 데이터 프레임에서 필요한 열만 추출
selected_data_list <- lapply(data.list, select_columns)

sapply(selected_data_list, function(x){
  names(x)[1:5]
})
selected_data_list[[1]] %>% names




### 🟨 열이름 통일 ======================================================
# 통일할 열 이름 정의
column_names <- c("ID", "행", "구분_Classification", "계_면적", "계_수량", 
                  "year", "NAME_L1", "NAME_L2", "NAME_L3", "NAME_L4", 
                  "unit_L2", "unit_L3", "unit_L4", "unit_L5")

# 모든 데이터프레임의 열 이름을 통일
standardize_column_names <- function(df) {
  names(df) <- column_names
  return(df)
}

# 각 데이터프레임에 함수 적용
standardized_data_list <- lapply(selected_data_list, standardize_column_names)

# 데이터프레임들 결합
combined_data <- do.call(rbind, standardized_data_list)
# View(combined_data)
rownames(combined_data) = NULL

combined_data$행 = NULL



### 🟨 연도들만 추출  ======================================================
# combined_data의 두 번째 열 추출
second_column <- combined_data[[2]]

# 숫자 4자리로 시작하거나 끝나는 문자열을 찾기 위한 정규 표현식
is_year <- grepl("^\\d{4}|\\d{4}$", second_column)

# 연도에 해당하는 행만 추출
year_rows <- combined_data[is_year, ]

# 결과 출력
print(year_rows)

View(year_rows)

path_save = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID/@_조림/(@완료)수종별 조림실적Plantation forest by tree species/@완료/1.Total 면적만 존재"
write.xlsx(year_rows, file.path(path_save, "1.Combined.xlsx"))
# year_rows$year


## 🟧 2.국유림 민유림 ===================================================================================
### 🟩 데이터 로드  ======================================================
path_data = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID/@_조림/(@완료)수종별 조림실적Plantation forest by tree species/@완료/2.국유림민유림"
data.list = lapply(list.files(path_data, full.names= T), read.csv) %>% 
  setNames(list.files(path_data))



### 🟩 연도추가  ======================================================
# 1999연보
n_length = length(data.list)
data.list[[n_length]][1,3] = paste0("1998_", data.list[[n_length]][1,3])
data.list[[n_length]][2,3] = paste0("1998_", data.list[[n_length]][2,3])


# 1998연보
data.list[[n_length - 1]][1,3] = paste0("1997_", data.list[[n_length - 1]][1,3])
data.list[[n_length - 1]][2,3] = paste0("1997_", data.list[[n_length - 1]][2,3])






### 🟩 특정 열들만 추출  ======================================================
data.list[[1]] %>% names


# 각 데이터프레임에서 필요한 열을 추출하는 함수 정의
select_columns <- function(df) {
  # 1~3번째 열
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


### 🟩 데이터 합치기 ======================================================
combined_data = do.call(rbind, selected_data_list)
# View(combined_data)
rownames(combined_data) = NULL
combined_data[[2]] = NULL

# combined_data %>% View


### 🟩 연도행만 추출 ======================================================
# combined_data의 두 번째 열 추출
second_column <- combined_data[[2]]

# 숫자 4자리가 포함된 문자열을 찾기 위한 정규 표현식
has_year <- grepl("\\d{4}", second_column)

# 연도가 포함된 행만 추출
rows_with_year <- combined_data[has_year, ]

# 결과 출력
print(rows_with_year)
View(rows_with_year)



### 🟩 면적행만 추출 ======================================================
# combined_data의 두 번째 열에서 숫자 4자리가 포함된 행 추출
second_column <- combined_data[[2]]
has_year <- grepl("\\d{4}", second_column)
rows_with_year <- combined_data[has_year, ]

# "면적"이라는 문자열을 포함하는 행 추출
contains_area <- grepl("면적", rows_with_year[[2]])

# 최종적으로 "면적"을 포함하는 행만 추출
final_rows <- rows_with_year[contains_area, ]

# 결과 출력
print(final_rows)
View(final_rows)




### 🟩 Export ======================================================
path_save = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID/@_조림/(@완료)수종별 조림실적Plantation forest by tree species/@완료/2.국유림민유림"
write.xlsx(final_rows, file.path(path_save, "2.Combined.xlsx"))











## 🟧 4.2000 ===================================================================================
### 🟩 데이터 로드  ======================================================
path_data = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID/@_조림/(@완료)수종별 조림실적Plantation forest by tree species/@완료/3.2000년도_수제작/2000_YRBK_0030030501.xlsx"
data_2000 = read.xlsx(path_data)
# View(data_2000)


### 🟩 ID  ======================================================
data_2000 $ID = data_2000 $ID[1]
data_2000[[2]] = NULL
data_2000[[2]] = data_2000[[2]][1]


data_2000 = data_2000 %>% 
  mutate(year = "2000") %>% 
  relocate(year, .after = ID)



### 🟩 본수 제외  ======================================================
# "본수"라는 문자열을 포함하는 열 이름을 찾기
cols_to_exclude <- grep("본수", names(data_2000), value = TRUE)

# "본수"라는 문자열을 포함하지 않는 열 이름 선택
data_2000_filtered <- data_2000[, !names(data_2000) %in% cols_to_exclude]

# 결과 확인
print(names(data_2000_filtered))



### 🟩 침엽수 이름 추가  ======================================================
# 수정할 열 이름 목록
columns_to_modify <- c(
  "잣나무_면적", "낙엽송_면적", "리기다_면적", "리기테다_면적", 
  "강송_면적", "해송_면적", "소나무_면적", "편백_면적", 
  "전나무_면적", "스트로브잣나무_면적", "화백_면적", "메타세콰이어_면적"
)

# 데이터프레임의 열 이름 수정
for (col in columns_to_modify) {
  if (col %in% names(data_2000_filtered)) {
    names(data_2000_filtered)[names(data_2000_filtered) == col] <- paste0("침엽수_", col)
  }
}

# 결과 확인
print(names(data_2000_filtered))



### 🟩 "계" 열들 옮기기 ======================================================
# "계"라는 문자열을 포함하는 열 이름을 찾기
kei_columns <- grep("계", names(data_2000_filtered), value = TRUE)

# "계" 열을 제외한 나머지 열 이름 찾기
remaining_columns <- setdiff(names(data_2000_filtered), kei_columns)

# 새로운 열 순서 지정 ("계" 열을 두 번째 위치로 이동)
new_column_order <- c(remaining_columns[1], kei_columns, remaining_columns[-1])

# 데이터프레임의 열 순서를 재정렬
data_2000_filtered <- data_2000_filtered[, new_column_order]

# 결과 확인
print(names(data_2000_filtered))




### 🟩 데이터 저장  ======================================================
path_save = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID/@_조림/(@완료)수종별 조림실적Plantation forest by tree species/@완료/3.2000년도_수제작"
write.xlsx(data_2000_filtered, file.path(path_save, "2000_Combined.xlsx"))





## 🟧 5.2004~2022 데이터 합치기 ===================================================================================
### 🟩 활엽수 침엽수 별 데이터  =======================================================
# 필요한 패키지 로드
library(readr)
library(dplyr)

# 데이터가 저장된 경로
data_path <- "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID/@_조림/(@완료)수종별 조림실적Plantation forest by tree species/@완료/4.침엽 활엽"

# 파일 목록 가져오기
files <- list.files(data_path, pattern = "\\.csv$", full.names = TRUE)

# 침엽수와 활엽수 데이터를 저장할 리스트 생성
conifer_list <- list()
broadleaf_list <- list()




# 파일을 읽고 데이터 분류 및 전처리
for (m in seq_along(files)){
  # m=25
  file = files[m]
  
  # 파일 이름에서 연도 추출
  year <- substr(basename(file), 1, 4)
  
  # CSV 파일 읽기
  df <- read.csv(file)
  
  # 3번째 열에서 연도가 포함된 행만 남기기
  df <- df %>% filter(grepl("\\d{4}", .[[3]]))
  
  # "그루수" 또는 "본수"가 포함된 열 제외
  df <- df %>% select(-contains("그루수"), -contains("본수"), -contains("sdls"))
  
  # 열 이름에서 "Area_" 문자열 제거
  names(df) <- gsub("Area_", "", names(df))
  names(df) <- gsub("_Area", "", names(df))
  names(df) <- gsub("Total_", "", names(df))
  
  # "면적_" 문자열이 포함된 열 이름을 "소나무_면적" 등으로 변경
  names(df) <- sapply(names(df), function(x) {
    if (grepl("면적_", x)) {
      sub("면적_", "", x) %>% paste0("_면적")
    } else {
      x
    }
  })
  
  # 중복된 열 이름 수정
  names(df) <- make.unique(names(df)) 
  
  # 특정 열 이름 변경
  names(df)[3] <- "구분"
  names(df)[4] <- "계_면적"
  
  # '침엽수'와 '활엽수'로 데이터 분류
  df= remove_na_columns(df)
  df_conifer <- df %>% dplyr::filter(grepl("침엽수", NAME_L4))
  df_broadleaf <- df %>% dplyr::filter(grepl("활엽수", NAME_L4))
  
  # 각각의 리스트에 저장
  if(nrow(df_conifer) > 0){
    conifer_list[[year]] <- df_conifer  
  } else if(nrow(df_broadleaf) > 0){
    broadleaf_list[[year]] <- df_broadleaf  
  }
}




### 🟩 열이름 확인 및 합치기  ======================================================







### 🟩 열이름 확인 및 합치기  ======================================================
#### 🟨 침엽수 ==============================================================
# names(conifer_list[[1]])
compare_columns(conifer_list, 3,11) # 열이름 확인
sapply(conifer_list, ncol)
conifer_df = bind_rows(conifer_list)
names(conifer_df)



# 2010년도 잣나무 확인
conifer_list$`2010` %>% View
conifer_df %>% filter(ID =="YRBK_0040040401") %>% View


# 이동할 열 이름들
columns_to_move <- c("Categorized_L3_New", "Categorized_L3", "Categorized_L2", "year",
                     "NAME_L1", "NAME_L2", "NAME_L3", "NAME_L4", "NAME_L5", 
                     "ID_L1", "ID_L2", "ID_L3", "ID_L4", "ID_L5", 
                     "unit_L2", "unit_L3", "unit_L4", "unit_L5", 
                     "비고_L2", "비고_L3", "비고_L4", "비고_L5")

# 열을 재정렬
conifer_df_reordered <- conifer_df %>% select(-all_of(columns_to_move), all_of(columns_to_move))

conifer_df_reordered %>% filter(ID == "YRBK_0040040401") %>% View

View(conifer_df_reordered)
# conifer_df_reordered %>% filter(ID == "YRBK_0040040401") %>% View


# 열 합치기 
names(conifer_df_reordered)

conifer_df_reordered$잣나무_면적
conifer_df_reordered$면적_짓나무
combine_columns(conifer_df_reordered, "면적_짓나무", "잣나무_면적", "잣나무_면적", "계_면적") %>% View



rename_columns(conifer_data_by_year) %>% compare_column_names(11)



 
# ## 🟧 3.2001 ===================================================================================
# ### 🟩 데이터 로드  ======================================================
# path_data = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID/@_조림/(@완료)수종별 조림실적Plantation forest by tree species/@완료/3.2001"
# data.list = lapply(list.files(path_data, full.names=T), read.csv)
# data.list[[1]] %>% View
# 
# 
# 
# ### 🟩 "본수" 열 제외  ======================================================
# # 각 데이터프레임에서 특정 열을 제외하는 함수 정의
# exclude_columns <- function(df) {
#   # "Categorized_L3_New" 열의 인덱스 찾기
#   end_idx <- which(names(df) == "Categorized_L3_New") - 1
#   
#   # 4번째 열부터 "Categorized_L3_New"의 이전 열까지의 인덱스
#   target_cols <- 4:end_idx
#   
#   # "본수" 문자열을 포함하는 열들의 인덱스 찾기
#   exclude_indices <- grep("본수", names(df)[target_cols])
#   
#   # 제외할 열들의 전체 인덱스 계산
#   exclude_cols <- target_cols[exclude_indices]
#   
#   # 제외할 열을 제외하고 새로운 데이터프레임 생성
#   result_df <- df[, -exclude_cols, drop = FALSE]
#   
#   return(result_df)
# }
# 
# # 각 데이터프레임에 함수 적용
# data.list <- lapply(data.list, exclude_columns)
# # data.list[[1]]
# 
# 
# 
# 
# 
# ### 🟩 열이름 동일한지 체크 ======================================================
# # 각 데이터프레임의 열 이름을 추출하는 함수 정의
# extract_column_names <- function(df) {
#   return(names(df))
# }
# 
# # 모든 데이터프레임의 열 이름을 리스트로 추출
# column_names_list <- lapply(data.list, extract_column_names)
# 
# # 첫 번째 데이터프레임의 열 이름을 기준으로 설정
# reference_names <- column_names_list[[1]]
# 
# # 모든 데이터프레임의 열 이름이 동일한지 확인
# all_identical <- all(sapply(column_names_list, function(x) identical(x, reference_names)))
# 
# if (all_identical) {
#   print("모든 데이터프레임의 열 이름이 동일합니다.")
# } else {
#   print("모든 데이터프레임의 열 이름이 동일하지 않습니다.")
# }
# 
# 
# 
# 
# # 각 데이터프레임의 열 이름을 추출하는 함수 정의
# extract_column_names <- function(df) {
#   return(names(df))
# }
# 
# # 모든 데이터프레임의 열 이름을 리스트로 추출
# column_names_list <- lapply(data.list, extract_column_names)
# 
# # 첫 번째 데이터프레임의 열 이름을 기준으로 설정
# reference_names <- column_names_list[[1]]
# 
# # 동일하지 않은 열 이름을 추출하는 함수 정의
# check_differences <- function(names_list, reference) {
#   if (!identical(names_list, reference)) {
#     differences <- which(names_list != reference | is.na(names_list) | is.na(reference))
#     return(list(differences = differences, names = names_list[differences]))
#   }
#   return(NULL)
# }
# 
# # 모든 데이터프레임에 대해 차이점 확인
# differences_list <- lapply(column_names_list, check_differences, reference = reference_names)
# 
# # 결과 출력
# for (i in seq_along(differences_list)) {
#   if (!is.null(differences_list[[i]])) {
#     cat(sprintf("DataFrame %d의 차이점:\n", i))
#     print(differences_list[[i]])
#   }
# }
# 
# 
# 
# 
# 
# 
# 
# 
# ### 🟩 합치기 =================================================================
# # data.list
# # library(dplyr)
# data.list[[1]] %>% View
# 
# # 모든 데이터프레임을 하나로 결합 (rbind처럼)
# combined_data <- bind_rows(data.list)
# 
# # 결과 확인
# print(combined_data)
# # names(combined_data)
# # View(combined_data)
# 
# grep("계", names(combined_data), value = T)
# 
# 
# 
# ### 🟩 열들 옮기기  =================================================================
# data.list[[1]] %>% names
# library(dplyr)
# 
# # 결합된 데이터프레임 combined_data를 사용
# # 이동할 열의 이름을 벡터로 정의
# columns_to_move <- c("Categorized_L3_New", "Categorized_L3", "Categorized_L2", "year", 
#                      "NAME_L1", "NAME_L2", "NAME_L3", "NAME_L4", "NAME_L5", 
#                      "ID_L1", "ID_L2", "ID_L3", "ID_L4", "ID_L5", 
#                      "unit_L2", "unit_L3", "unit_L4", "unit_L5", 
#                      "비고_L2", "비고_L3", "비고_L4", "비고_L5")
# 
# # 전체 열 이름
# all_columns <- names(combined_data)
# 
# # 이동할 열을 제외한 나머지 열
# remaining_columns <- setdiff(all_columns, columns_to_move)
# 
# # 새로운 순서로 데이터프레임 재구성
# reordered_data <- combined_data %>% select(all_of(remaining_columns), all_of(columns_to_move))
# 
# # 결과 확인
# print(reordered_data)
# 
# 
# # View(reordered_data)
# 
# 
# names(reordered_data)
# # reordered_data$기타_면적
# 
# grep("계", names(reordered_data), value = T)
# 
# 
# 
# ### 🟩 열합치기  =================================================================
# # 구분
# reordered_data_2 = combine_columns(reordered_data, "구분", "구분_구분", "구분", "행")
# names(reordered_data_2)
# 
# names(reordered_data_2)
# 
# # 자작나무
# reordered_data_3 = combine_columns(reordered_data_2, "자작_면적", "자작나무_면적", "자작나무_면적", "계_면적")
# 
# # 벚나무
# reordered_data_4 = combine_columns(reordered_data_3, "벚나무_면적", "벚나무류_면적", "벚나무류_면적", "계_면적")
# names(reordered_data_4 )
# 
# 
# 
# 
# 
# 
# ### 🟩 침엽수 활엽수  =================================================================
# names(reordered_data_4)
# data_new = reordered_data_4
# # View(data_new)
# 
# 
# #### 🟨 침엽수  ============================================================
# # 대상 나무 이름 목록
# tree_names <- c("잣나무", "낙엽송", "리기다", "리기테다", "강송", "해송", 
#                 "삼나무", "편백", "전나무", "스트로브잣나무", "화백", 
#                 "주목", "소나무", "메타세쿼이야", "히말리아시다", "은행나무")
# 
# # 새로운 열 이름을 저장할 벡터 생성
# new_column_names <- names(data_new)
# 
# # 열 이름 변경 작업
# for (i in seq_along(new_column_names)) {
#   for (tree_name in tree_names) {
#     if (grepl(tree_name, new_column_names[i])) {
#       new_column_names[i] <- paste0("침엽수_", new_column_names[i])
#       break
#     }
#   }
# }
# 
# # 데이터프레임의 열 이름을 새로운 이름으로 변경
# names(data_new) <- new_column_names
# 
# 
# 
# # 결과 확인
# print(names(data_new))
# 
# 
# 
# #### 🟨 활엽수  ============================================================
# # 대상 나무 이름 목록 (활엽수)
# broadleaf_tree_names <- c("벚나무류", "자작나무", "느티나무", "물푸레나무", "산초나무", 
#                           "두충나무", "옻나무", "복자기", "단풍나무", "산수유", 
#                           "상수리", "루브라참나무", "참나무류기타", "거제수", 
#                           "두릅나무", "고로쇠", "옴나무", "황칠나무", "후박나무", 
#                           "가중나무", "이팝나무", "동백", "산딸나무", "철쭉류", 
#                           "배롱나무", "영산홍", "자귀나무", "매화나무", "밤나무", 
#                           "감나무", "대추나무", "호도나무")
# 
# # 새로운 열 이름을 저장할 벡터 생성
# new_column_names <- names(data_new)
# 
# # 열 이름 변경 작업
# for (i in seq_along(new_column_names)) {
#   for (tree_name in broadleaf_tree_names) {
#     if (grepl(tree_name, new_column_names[i])) {
#       new_column_names[i] <- paste0("활엽수_", new_column_names[i])
#       break
#     }
#   }
# }
# 
# # 데이터프레임의 열 이름을 새로운 이름으로 변경
# names(data_new) <- new_column_names
# 
# # 결과 확인
# print(names(data_new))
# 
# 
# 
# 
# 
# ### 🟩 열옮기기  =================================================================
# # 열 이름 목록
# column_names <- names(data_new)
# 
# # '계_면적' 열의 위치 찾기
# ke_area_idx <- which(column_names == "계_면적")
# 
# # 'Categorized_L3_New' 열의 위치 찾기
# cat_idx <- which(column_names == "Categorized_L3_New")
# 
# # '계_면적'과 'Categorized_L3_New' 사이의 열들
# intermediate_columns <- column_names[(ke_area_idx + 1):(cat_idx - 1)]
# 
# # '침엽수'가 포함된 열 이름 찾기
# conifer_columns <- intermediate_columns[grep("침엽수", intermediate_columns)]
# 
# # '활엽수'가 포함된 열 이름 찾기
# broadleaf_columns <- intermediate_columns[grep("활엽수", intermediate_columns)]
# 
# # '침엽수'와 '활엽수'가 아닌 나머지 열
# other_columns <- setdiff(intermediate_columns, c(conifer_columns, broadleaf_columns))
# 
# # 새로운 열 순서 지정
# new_column_order <- c(column_names[1:ke_area_idx],
#                       conifer_columns,
#                       broadleaf_columns,
#                       other_columns,
#                       column_names[cat_idx:length(column_names)])
# 
# # 데이터프레임의 열 순서를 재정렬
# data_new <- data_new[, new_column_order]
# 
# # 결과 확인
# print(names(data_new))
# 
# 
# 
# # 열 이름 목록
# column_names <- names(data_new)
# 
# # '계_면적' 열의 위치 찾기
# ke_area_idx <- which(column_names == "계_면적")
# 
# # 'Categorized_L3_New' 열의 위치 찾기
# cat_idx <- which(column_names == "Categorized_L3_New")
# 
# # '계_면적'과 'Categorized_L3_New' 사이의 열들
# intermediate_columns <- column_names[(ke_area_idx + 1):(cat_idx - 1)]
# 
# # '계'가 포함된 열 이름 찾기
# kei_columns <- intermediate_columns[grep("계", intermediate_columns)]
# 
# # '기타'가 포함된 열 이름 찾기 (활엽수_참나무류기타_면적 제외)
# other_columns <- setdiff(
#   intermediate_columns[grep("기타", intermediate_columns)],
#   "활엽수_참나무류기타_면적"
# )
# 
# # '계'와 '기타'가 아닌 나머지 열
# remaining_columns <- setdiff(intermediate_columns, c(kei_columns, other_columns))
# 
# # 새로운 열 순서 지정
# new_column_order <- c(column_names[1:ke_area_idx],
#                       kei_columns,
#                       other_columns,
#                       remaining_columns,
#                       column_names[cat_idx:length(column_names)])
# 
# # 데이터프레임의 열 순서를 재정렬
# data_new <- data_new[, new_column_order]
# 
# # 결과 확인
# print(names(data_new))
# View(data_new)
# 
# 
# ### 🟩 다른 데이터에서 활엽수 침엽수 합계 확인  =================================================================
# # test = read.xlsx("/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID/@_조림/(@완료)수종별 조림실적Plantation forest by tree species/@완료/2.국유림민유림/2.Combined.xlsx")
# # View(test)
# # test = read.csv("/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID/@_조림/(@완료)수종별 조림실적Plantation forest by tree species/@완료/2017_YRBK_0047040602.csv")
# # View(test)
# # test = read.csv("/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID/@_조림/(@완료)수종별 조림실적Plantation forest by tree species/@완료/2022_YRBK_0052040502.csv")
# # View(test)
# 
# 
# 
# ### 🟩 침엽수 활엽수 계열 에 대해 실제 계산 결과와 비슷한지 확인  =================================================================
# data_3 = data_new
# 
# data_3 %>% names
# 
# # 주어진 열 이후의 열들
# start_cols <- c("ID", "행", "구분", "계_면적", "침엽수계_면적", "활엽수계_면적")
# start_idx <- which(names(data_3) %in% start_cols)
# last_idx <- which(names(data_3) == "Categorized_L3_New") - 1
# 
# # 대상 열 추출 (시작 열 이후, "Categorized_L3_New" 이전)
# target_columns <- names(data_3)[(max(start_idx) + 1):last_idx]
# 
# # "침엽수" 문자열이 포함된 열들에서 각 값들의 합 계산
# conifer_cols <- grep("침엽수", target_columns, value = TRUE)
# conifer_sum <- rowSums(data_3[, conifer_cols], na.rm = TRUE)
# 
# # "활엽수" 문자열이 포함된 열들에서 각 값들의 합 계산
# broadleaf_cols <- grep("활엽수", target_columns, value = TRUE)
# broadleaf_sum <- rowSums(data_3[, broadleaf_cols], na.rm = TRUE)
# 
# 
# # data_3$침엽수계_면적
# # 비교 데이터프레임 생성
# comparison_df <- data.frame(
#   "계산된_침엽수_합계" = conifer_sum,
#   "실제_침엽수_면적_계" = if ("침엽수_면적_계" %in% names(data_3)) data_3$침엽수계_면적 else NA,
#   "계산된_활엽수_합계" = broadleaf_sum,
#   "실제_활엽수_면적_계" = if ("활엽수_면적_계" %in% names(data_3)) data_3$활엽수계_면적 else NA
# )
# 
# # 결과 데이터프레임 출력
# print(comparison_df)
# View(comparison_df)
# 
# 
# 
# # data_3 %>% View
# # "침엽수계_면적" 열에서 NA가 아닌 값을 가지는 행 필터링
# non_na_rows <- !is.na(data_3$침엽수계_면적)
# 
# # 해당 행들의 "ID" 열 값 추출
# id_values <- data_3$ID[non_na_rows]
# 
# # 결과 출력
# print(id_values)
# 
# 
# 
# ### 🟩 침엽수, 활엽수 계 만들기  =================================================================
# data_4 = data_3
# names(data_4)
# # View(data_4)
# 
# 
# # "활엽수" 관련 열들의 합계 계산 (활엽수계_면적 제외)
# broadleaf_cols <- grep("활엽수", names(data_4), value = TRUE)
# broadleaf_cols <- setdiff(broadleaf_cols, "활엽수계_면적")
# broadleaf_sum <- rowSums(data_4[, broadleaf_cols], na.rm = TRUE)
# 
# # "활엽수계_면적"과 비교 및 결합
# data_4$활엽수계_면적 <- mapply(function(sum_val, actual_val) {
#   if (is.na(sum_val) && is.na(actual_val)) {
#     return(NA)
#   } else if (is.na(sum_val)) {
#     return(actual_val)
#   } else if (is.na(actual_val)) {
#     return(sum_val)
#   } else if (sum_val == actual_val) {
#     return(sum_val)
#   } else {
#     stop("Error: Calculated sum and '활엽수계_면적' have different non-NA values.")
#   }
# }, broadleaf_sum, data_4$활엽수계_면적)
# 
# # "침엽수" 관련 열들의 합계 계산 (침엽수계_면적 제외)
# conifer_cols <- grep("침엽수", names(data_4), value = TRUE)
# conifer_cols <- setdiff(conifer_cols, "침엽수계_면적")
# conifer_sum <- rowSums(data_4[, conifer_cols], na.rm = TRUE)
# 
# # "침엽수계_면적"과 비교 및 결합
# data_4$침엽수계_면적 <- mapply(function(sum_val, actual_val) {
#   if (is.na(sum_val) && is.na(actual_val)) {
#     return(NA)
#   } else if (is.na(sum_val)) {
#     return(actual_val)
#   } else if (is.na(actual_val)) {
#     return(sum_val)
#   } else if (sum_val == actual_val) {
#     return(sum_val)
#   } else {
#     stop("Error: Calculated sum and '침엽수계_면적' have different non-NA values.")
#   }
# }, conifer_sum, data_4$침엽수계_면적)
# 
# # 결과 데이터프레임 출력
# print(data_4)
# View(data_4)
# 
# # 계 면적
# # 열 이름의 위치(인덱스) 찾기
# index_conifer <- which(names(data_4) == "침엽수계_면적")
# index_broadleaf <- which(names(data_4) == "활엽수계_면적")
# index_other <- which(names(data_4) == "기타_면적")
# 
# # 새로운 열 계산 및 할당
# data_4[[length(data_4) + 1]] <- rowSums(data_4[, c(index_conifer, index_broadleaf, index_other)], na.rm = TRUE)
# 
# # 새로 추가된 열의 이름 지정
# names(data_4)[length(data_4)] <- "Area_Total_my"
# 
# data_4 = data_4 %>% relocate(Area_Total_my, .after = 4)
# 
# 
# 
# # 4번째 열과 5번째 열의 값을 NA를 0으로 대체
# col4 <- ifelse(is.na(data_4[[4]]), 0, data_4[[4]])
# col5 <- ifelse(is.na(data_4[[5]]), 0, data_4[[5]])
# 
# # 두 열의 값이 동일하지 않은 행 인덱스 찾기
# diff_indices <- which(col4 != col5)
# 
# # 동일하지 않은 값들을 포함하는 데이터프레임 생성
# differences <- data.frame(
#   Row = diff_indices,
#   Value_in_Column_4 = col4[diff_indices],
#   Value_in_Column_5 = col5[diff_indices]
# )
# 
# # 결과 출력
# if (nrow(differences) > 0) {
#   cat("4번째 열과 5번째 열의 값이 동일하지 않은 경우:\n")
#   print(differences)
# } else {
#   cat("4번째 열과 5번째 열의 모든 값이 동일합니다.\n")
# }
# 
# 
# 
# 
# 
# 
# 
# ### 🟩 합계 행만 추출  =================================================================
# data_5 = data_4 %>% filter(구분 == "합계")
# # View(data_5)
# 
# data_5$구분 = "2000"
# # View(data_5)
# data_5$행 = NULL
# 
# 
# 
# 
# 
# 
# ### 🟩 행별 합치기  =================================================================
# data_3 = data_5
# names(data_3)
# # View(data_3)
# # data_3$계_면적 %>% sum
# # 가져올 1행 데이터 열 목록
# one_row_columns <- c(
#   "ID", "구분", "Categorized_L3_New", "Categorized_L3", "Categorized_L2",
#   "year", "NAME_L1", "NAME_L2", "NAME_L3", "NAME_L4", "NAME_L5",
#   "ID_L1", "ID_L2", "ID_L3", "ID_L4", "ID_L5", "unit_L2", "unit_L3",
#   "unit_L4", "unit_L5", "비고_L2", "비고_L3", "비고_L4", "비고_L5"
# )
# 
# # 면적 합계를 구할 열 목록
# sum_columns <- setdiff(names(data_3), one_row_columns)
# 
# # 새로운 데이터프레임 생성
# new_df <- data.frame(matrix(ncol = length(names(data_3)), nrow = 1))
# names(new_df) <- names(data_3)
# 
# # 1행 데이터 가져오기
# new_df[one_row_columns] <- data_3[1, one_row_columns]
# 
# # 면적 값 합계 계산하여 할당
# new_df[sum_columns] <- colSums(data_3[sum_columns], na.rm = TRUE)
# 
# # 결과 데이터프레임 출력
# print(new_df)
# View(new_df)
# 
# 
# new_df = new_df %>% rename("침엽수_계_면적")
# 
# grep("기타", names(new_df), value=T)
# new_df = new_df %>% relocate("기타_면적", .after = "활엽수계_면적")
# new_df %>% names()
# 
# 
# 
# ### 🟩 기타의 합계  =================================================================
# new_df = new_df %>% 
#   rename("침엽수기타_면적_tmp" := "침엽수기타_면적") %>% 
#   rename("활엽수기타_면적_tmp" := "활엽수기타_면적")
# 
# 
# 
# ### 🟩 Export  =================================================================
# path_save = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID/@_조림/(@완료)수종별 조림실적Plantation forest by tree species/@완료/3.2001"
# write.xlsx(new_df, file.path(path_save, "2001_YRBK_00310305_Combined.xlsx"))
# 








