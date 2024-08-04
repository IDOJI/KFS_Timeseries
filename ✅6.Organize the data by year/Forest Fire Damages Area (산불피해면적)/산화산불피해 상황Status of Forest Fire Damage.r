# 🟥 전체 데이터 로드 ============================================================================
path_files = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID/@_산림피해/산화산불피해 상황Status of Forest Fire Damage"
data.list = lapply(list.files(path_files, full.names=T), read.csv) %>% 
  setNames(list.files(path_files, full.names=F))



# 🟩 열이름 확인 ============================================================================
# sapply(data.list, names)

# 각 데이터 프레임의 열 이름을 추출하고 조건에 맞게 필터링
filtered_column_names <- lapply(data.list, function(df) {
  # 'Categorized_L3_New' 열의 인덱스 찾기
  index_Categorized <- which(names(df) == "Categorized_L3_New")
  
  # 'Categorized_L3_New' 열 전까지의 열 이름 추출
  relevant_columns <- names(df)[1:(index_Categorized - 1)]
  
  # "건"이 포함된 열 이름 제외
  filtered_columns <- relevant_columns[!grepl("건", relevant_columns)]
  
  return(filtered_columns)
})

# 결과 출력
print(filtered_column_names)




# 🟩 열이름에 "면적"을 포함하지 않은 데이터 옮기기 ============================================================================
# library(dplyr)
# library(purrr)
# library(stringr)
# 
# # 원본 파일 경로와 새로운 파일 저장 경로를 지정합니다.
# path_files <- "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID/@_산림피해/산화산불피해 상황Status of Forest Fire Damage"
# filtered_path <- "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/Filtered"
# 
# # CSV 파일 리스트를 가져옵니다.
# file_list <- list.files(path_files, full.names = TRUE)
# 
# # "면적" 문자열을 포함하는 열이 없는 파일들을 새로운 경로로 이동합니다.
# lapply(file_list, function(file) {
#   # 폴더가 아닌 파일만 처리
#   if (file.info(file)$isdir == FALSE) {
#     # CSV 파일을 읽어옵니다.
#     data <- read.csv(file)
#     
#     # 열 이름을 확인하고 "면적" 문자열이 포함되지 않은 경우 파일을 이동합니다.
#     if (!any(str_detect(colnames(data), "면적"))) {
#       file.copy(file, file.path(filtered_path, basename(file)))
#     }
#   }
# })


# 🟥 전체 데이터 로드 ============================================================================
path_files <- "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID/@_산림피해/산화산불피해 상황Status of Forest Fire Damage"
data.list <- lapply(list.files(path_files, pattern = "\\.csv$", full.names = TRUE), read.csv) %>% 
  setNames(list.files(path_files, pattern = "\\.csv$", full.names = FALSE))

names(data.list)





# 🟩 열이름 확인 ============================================================================
# sapply(data.list, names)

# 각 데이터 프레임의 열 이름을 추출하고 조건에 맞게 필터링
filtered_column_names <- lapply(data.list, function(df) {
  # 'Categorized_L3_New' 열의 인덱스 찾기
  index_Categorized <- which(names(df) == "Categorized_L3_New")
  
  # 'Categorized_L3_New' 열 전까지의 열 이름 추출
  relevant_columns <- names(df)[1:(index_Categorized - 1)]
  
  # "건"이 포함된 열 이름 제외
  filtered_columns <- relevant_columns[!grepl("건", relevant_columns)]
  
  return(filtered_columns)
})

# 결과 출력
print(filtered_column_names)



# 🟩 "구분"열  ============================================================================
data.list <- lapply(data.list, function(df) {
  if (ncol(df) >= 3 && grepl("구분", colnames(df)[3])) {
    colnames(df)[3] <- "구분"
  }
  return(df)
})



# 🟩 열이름 확인 ============================================================================
sapply(data.list, ncol) %>% unique

sapply(data.list, names)


# 🟩 열 추출 ============================================================================
data.list_2 <- lapply(data.list, function(df) {
  # 1~3열 추출
  initial_columns <- df[, 1:3]
  
  # "Categorized_L3_New" 열의 인덱스를 찾기
  categorized_index <- which(colnames(df) == "Categorized_L3_New")
  
  # "Categorized_L3_New" 이전의 "면적"을 포함하는 열들 추출
  if (categorized_index > 1) {
    area_columns <- df[, grepl("면적", colnames(df)[1:(categorized_index - 1)])]
  } else {
    area_columns <- NULL
  }
  
  # "Categorized_L3_New" 열부터 마지막 열까지 추출
  remaining_columns <- df[, categorized_index:ncol(df)]
  
  # 추출한 열들을 하나의 데이터 프레임으로 결합
  result_df <- cbind(initial_columns, area_columns, remaining_columns)
  
  return(result_df)
})



# 🟩 열이름 확인 ============================================================================
sapply(data.list_2, names)
sapply(data.list_2, ncol) %>% unique





# 🟩 각 데이터 확인 ============================================================================
raw_data = read.xlsx("/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/0.Raw Data/DB_20240415/TB_NDI_MRV_STTST_YRBK_OF_FRSTR_DATA_20240415.xlsx")
raw_data %>% filter(연보.ID == "YRBK_00140312") %>% View

