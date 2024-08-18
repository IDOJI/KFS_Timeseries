# (@완료)🟥 전체 데이터 로드 ============================================================================
# path_files = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/산불/산화산불피해 상황Status of Forest Fire Damage"
# data.list = lapply(list.files(path_files, full.names=T), read.csv) %>% 
#   setNames(list.files(path_files, full.names=F))



# (@완료)🟩 열이름 확인 ============================================================================
# sapply(data.list, names)

# # 각 데이터 프레임의 열 이름을 추출하고 조건에 맞게 필터링
# filtered_column_names <- lapply(data.list, function(df) {
#   # 'Categorized_L3_New' 열의 인덱스 찾기
#   index_Categorized <- which(names(df) == "Categorized_L3_New")
#   
#   # 'Categorized_L3_New' 열 전까지의 열 이름 추출
#   relevant_columns <- names(df)[1:(index_Categorized - 1)]
#   
#   # "건"이 포함된 열 이름 제외
#   filtered_columns <- relevant_columns[!grepl("건", relevant_columns)]
#   
#   return(filtered_columns)
# })
# 
# # 결과 출력
# print(filtered_column_names)




# (@완료)🟩 열이름에 "재적"을 포함하지 않은 데이터 옮기기 ============================================================================
# library(dplyr)
# library(purrr)
# library(stringr)
# 
# # 원본 파일 경로와 새로운 파일 저장 경로를 지정합니다.
# filtered_path <- "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/산불/산화산불피해 상황Status of Forest Fire Damage/excluded"
# 
# # 필요한 패키지 로드 (stringr 패키지 필요)
# library(stringr)
# 
# # CSV 파일 리스트를 가져옵니다.
# file_list <- list.files(path_files, full.names = TRUE)
# 
# # "재적" 문자열을 포함하는 열이 없는 파일들을 새로운 경로로 이동합니다.
# lapply(file_list, function(file) {
#   # 폴더가 아닌 파일만 처리
#   if (!file.info(file)$isdir) {
#     # CSV 파일을 읽어옵니다.
#     data <- read.csv(file)
# 
#     # 열 이름을 확인하고 "재적" 문자열이 포함되지 않은 경우 파일을 이동합니다.
#     if (!any(str_detect(colnames(data), "재적"))) {
#       file.rename(file, file.path(filtered_path, basename(file)))
#     }
#   }
# })




# 🟥 전체 데이터 로드 ============================================================================
path_files <- "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/산불/산화산불피해 상황Status of Forest Fire Damage"
data.list <- lapply(list.files(path_files, pattern = "\\.csv$", full.names = TRUE), read.csv) %>% 
  setNames(list.files(path_files, pattern = "\\.csv$", full.names = FALSE))

names(data.list)





# 🌫️ 열이름에 따라 선택 추출 ============================================================================
# sapply(data.list, names)

# 각 데이터 프레임의 열 이름을 추출하고 조건에 맞게 필터링
filtered_data.list <- lapply(data.list, function(df) {
  # df = data.list[[1]]
  # 'Categorized_L3_New' 열의 인덱스 찾기
  index_Categorized <- which(names(df) == "Categorized_L3_New")
  
  ind_col_1 = 1:3
  ind_col_2 = grep("재적", names(df))
  ind_col_3 = index_Categorized:ncol(df)
  
  df[,c(ind_col_1, ind_col_2, ind_col_3)]
})
# 결과 출력
filtered_data.list[[1]] %>% View




# 🌫 "구분"열  ============================================================================
# 3번째 열이름 확인
sapply(filtered_data.list, function(x){
  names(x)[3]
}) %>% unname %>% unique


# 3번쨰 열이름 변경

data.list_3 <- lapply(filtered_data.list, function(df) {
  if (ncol(df) >= 3 && grepl("구분", colnames(df)[3])) {
    colnames(df)[3] <- "구분"
  }
  return(df)
})


sapply(data.list_3, function(x){
  names(x)[3]
}) %>% unique




# 🌫 4번째 열 이름 ====================================================================================
# 열 이름 확인
sapply(data.list_3, function(x){
  names(x)[4]
}) %>% unique


# 열 이름 통일 : 재적_세제곱미터
data.list_4 = lapply(data.list_3, function(x){
  names(x)[4] = "재적_세제곱미터"
  return(x)
})


sapply(data.list_4, function(x){
  names(x)[4]
}) %>% unique


# 재적이 2개 열인 경우 전부 NA인 열은 제외
which(sapply(data.list_4, ncol) == 27)

which_overlap = c("1992_YRBK_00220314.csv", "1993_YRBK_00230314.csv")

# 1
sub_data = data.list_4[[which_overlap[1]]]
names(sub_data)
sub_data$재적_세제곱미터
sub_data$피해상황_재적_2 = NULL
data.list_4[[which_overlap[1]]] = sub_data


# 2
sub_data = data.list_4[[which_overlap[2]]]
names(sub_data)
sub_data$재적_세제곱미터
sub_data$피해상황_재적_2 = NULL
data.list_4[[which_overlap[2]]] = sub_data




# 🟪 데이터 수정  ============================================================================
## 🟩 1984 =======================================================================
id = "YRBK_00140312"
ind = grep(id, names(data.list_4))
if(length(ind) != 1){
  stop("")
}

sub_data = data.list_4[[ind]]
which_col = grep("재적", names(sub_data))
sub_data[[which_col]]

# 970 -> 685
sub_data[[which_col]][sub_data[[which_col]] %in% 970] = 685

# 1218 -> 3601
sub_data[[which_col]][sub_data[[which_col]] %in% 1218] = 3601

data.list_4[[ind]] = sub_data





# 🟨 데이터 합치기  ============================================================================
data_combined = bind_rows(data.list_4)
# View(data_combined)
names(data_combined )


# 🟨 연도행만 추출  ============================================================================
data_combined %>% names
# "구분" 열에서 4자리 연도가 포함된 행만 추출
filtered_data <- data_combined %>%
  filter(str_detect(구분, "\\b\\d{4}\\b"))

filtered_data$재적_세제곱미터 %>% is.na %>% sum

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


# 🟨 연도 확인  ============================================================================
unique_filtered_data$구분 %>% unique
# unique_filtered_data$구분에서 연도를 가져와서 정수형으로 변환
years <- as.numeric(unique_filtered_data$구분 %>% unique())

# 연도의 최소값과 최대값을 확인
min_year <- min(years)
max_year <- max(years)

# 모든 연도를 포함하는지 확인하기 위해 전체 연도 범위 생성
all_years <- seq(min_year, max_year)

# 누락된 연도가 있는지 확인
missing_years <- setdiff(all_years, years)

if(length(missing_years) == 0) {
  print("모든 연도가 포함되어 있습니다.")
} else {
  print("누락된 연도가 있습니다:")
  print(missing_years)
}




# 🟨 check unit  ============================================================================
unique_filtered_data$unit_L3
unique_filtered_data$unit_L4
unique_filtered_data$unit_L5



# 🟨 export  ============================================================================
path_save = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/산불/산화산불피해 상황Status of Forest Fire Damage"
file_name = "산화산불피해 상황Status of Forest Fire Damage_재적.xlsx"
write.xlsx(unique_filtered_data, file.path(path_save, file_name))











