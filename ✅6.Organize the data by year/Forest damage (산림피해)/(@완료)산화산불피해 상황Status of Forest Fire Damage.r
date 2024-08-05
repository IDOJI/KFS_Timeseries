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
# filtered_path <- "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID/@_산림피해/산화산불피해 상황Status of Forest Fire Damage/면적 열 없는 데이터 제외"
# 
# # 필요한 패키지 로드 (stringr 패키지 필요)
# library(stringr)
# 
# # CSV 파일 리스트를 가져옵니다.
# file_list <- list.files(path_files, full.names = TRUE)
# 
# # "면적" 문자열을 포함하는 열이 없는 파일들을 새로운 경로로 이동합니다.
# lapply(file_list, function(file) {
#   # 폴더가 아닌 파일만 처리
#   if (!file.info(file)$isdir) {
#     # CSV 파일을 읽어옵니다.
#     data <- read.csv(file)
#     
#     # 열 이름을 확인하고 "면적" 문자열이 포함되지 않은 경우 파일을 이동합니다.
#     if (!any(str_detect(colnames(data), "면적"))) {
#       file.rename(file, file.path(filtered_path, basename(file)))
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
  
  if (length(index_Categorized) == 0) {
    # 'Categorized_L3_New' 열이 없으면 마지막 열의 인덱스 사용
    index_Categorized <- ncol(df) + 1
  }
  
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
  # df =  get_data_by_name(data.list, "1992_YRBK_00220314.csv")
  # 1~3열 추출
  df_sub_1 <- df[, 1:3]
  # View(df_sub_1)
  
  # "Categorized_L3_New" 열의 인덱스를 찾기
  categorized_index <- which(colnames(df) == "Categorized_L3_New")
  if(length(categorized_index)>0){
    df_sub_3 = df[,categorized_index:ncol(df)]  
  }else{
    df_sub_3 = NULL
  }
  
  # View(df_sub_3)
  
  # middle
  if(length(categorized_index)>0){
    df_sub_2 = df[,4:(categorized_index - 1)] %>% as_tibble  
  }else{
    df_sub_2 = df[,4:ncol(df)] %>% as_tibble  
  }
  
  df_sub_2_2 = df_sub_2 %>% select(grep("면적", names(df_sub_2)))
  # View(df_sub_2_2)
  
  
  # 열의 개수를 확인합니다.
  num_cols <- ncol(df_sub_2_2)
  
  if (num_cols > 1) {
    # 모든 열에 대해 NA가 아닌 값이 있는지 확인
    non_na_presence <- colSums(!is.na(df_sub_2_2)) > 0
    
    if (all(!non_na_presence)) {
      # 모든 열이 NA인 경우, 첫 번째 열만 남김
      df_sub_2_2 <- df_sub_2_2[, 1]
    } else {
      # NA가 아닌 값이 있는 열을 제외하고 나머지 열들 중 모든 원소가 NA인 열을 제거
      df_sub_2_2 <- df_sub_2_2[, non_na_presence]
    }
  }
  
  
  # 추출한 열들을 하나의 데이터 프레임으로 결합
  if(is.null(df_sub_3)){
    
    result_df <- cbind(df_sub_1, df_sub_2_2)
    
  }else{
    
    result_df <- cbind(df_sub_1, df_sub_2_2, df_sub_3)
    
  }
  
  
  return(result_df)
})



# 🟩 열이름 확인 ============================================================================
sapply(data.list_2, names)
n_cols = sapply(data.list_2, ncol)
table(n_cols)


# 🟩 4번째 열이름 변경  ============================================================================
# data.list_2[[1]] %>% names
data.list_3 = lapply(data.list_2, function(x){
  x %>% rename("면적_헥타" = colnames(x)[4])
})



# 🟦Raw 데이터 확인 ============================================================================
## ⭐️"YRBK_00180315" ===========================================================
raw_data %>% filter(연보.ID == "YRBK_00180315") %>% View
data_1988 = data.list_3$`1988_YRBK_00180315.csv`
data_1988[14:16, 4] = c(922, 247, 3255)

View(data_1988)



# 🟪 (@완료) 면적_헥타 열이 NA인 열 찾기 ============================================================================
which_NA = sapply(data.list_3, function(x){
  x %>% pull("면적_헥타") %>% is.na %>% all
}) %>% which
# 
# NA_data = data.list_3[[which_NA]]
# names(data.list_3)[which_NA]
# NA_ID = "YRBK_00200316"
# 
# # Raw data?
# raw_data %>% filter(연보.ID == NA_ID) %>% View
# 
# data_with_hdr = readRDS("/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/3.Data with HDR/8.2.Combined Data.rds")
# get_data_by_name(data_with_hdr, NA_ID) %>% View
# 
# data_raw_as_list = readRDS("/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/3.Data with HDR/1.Raw Data as a list.rds")
# selected_data = get_data_by_name(data_raw_as_list, NA_ID)
# 코드 수정 완료 후 내보내기 다시 함 -> 이전 코드 다시 돌릴 것.






# 🟨 데이터 합치기  ============================================================================
data_combined = bind_rows(data.list_3)
# View(data_combined)



# 🟨 연도행만 추출  ============================================================================
data_combined %>% names
# "구분" 열에서 4자리 연도가 포함된 행만 추출
filtered_data <- data_combined %>%
  filter(str_detect(구분, "\\b\\d{4}\\b"))

# 결과 확인
filtered_data %>% View

# 🟨 unique한 최근 연도만 추출  ============================================================================
filtered_data = filtered_data %>% arrange(구분, year) %>% relocate(year, .after = "구분")

filtered_data$구분 %>% unique

# View(filtered_data)
# "구분"에서 중복된 연도를 제거하고 "year"에서 가장 최근의 값을 갖는 행을 추출
unique_filtered_data <- filtered_data %>%
  group_by(구분) %>%
  filter(year == max(year)) %>%
  ungroup()

# 결과 확인
# View(unique_filtered_data)

unique_filtered_data$구분 %>% unique




# 🟨 export  ============================================================================
path_save = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID/@_산림피해/산화산불피해 상황Status of Forest Fire Damage"
file_name = "산화산불피해 상황Status of Forest Fire Damage.xlsx"
write.xlsx(unique_filtered_data, file.path(path_save, file_name))











