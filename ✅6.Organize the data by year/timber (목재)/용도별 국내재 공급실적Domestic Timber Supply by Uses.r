# 🟥 데이터 로드 =================================================================
path_data = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID/@_목재/용도별 국내재 공급실적Domestic Timber Supply by Uses"
data.list = lapply(list.files(path_data, full.names=T), read.csv) %>% 
  setNames(list.files(path_data))
data.list[[1]] %>% View




# 🟨 열이름 확인 =================================================================
sapply(data.list, names)

# 각 데이터프레임에서 4번째 열부터 'Categorized_L3_New' 열의 이전 열까지의 열 이름 추출
extracted_column_names <- map(data.list, function(df) {
  # 'Categorized_L3_New' 열의 인덱스 찾기
  index_Categorized <- which(names(df) == "Categorized_L3_New")
  
  # 유효한 'Categorized_L3_New' 인덱스가 있는 경우에만 열 이름 추출
  if (length(index_Categorized) == 1 && index_Categorized > 4) {
    colnames(df)[4:(index_Categorized - 1)]
  } else {
    NULL # 조건에 맞는 열이 없을 경우 NULL 반환
  }
})

# NULL 요소 제거
extracted_column_names <- extracted_column_names[!sapply(extracted_column_names, is.null)]

# 결과 확인
print(extracted_column_names)


# 🟨 3번째 열을 구분으로 바꾸기 =================================================================
library(purrr)
# 3번째 열 이름 체크
thrid = sapply(data.list, function(x){
  names(x)[3]
}) %>% unname



library(purrr)

# 각 데이터 프레임의 3번째 열 이름을 "구분"으로 변경
data.list_2 <- map(data.list, function(df) {
  if (ncol(df) >= 3) {
    colnames(df)[3] <- "구분"
  }
  return(df)
})

# 결과 확인: 각 데이터 프레임의 열 이름
print(map(data.list_2, names))



# 🟦 각 데이터 합치기=================================================================
data_combined = bind_rows(data.list_2) %>% arrange(구분, year) %>% relocate(year, .after = 구분)
names(data_combined)



# 🟦 연도 행만 추출=================================================================
data_combined_2 = filter_by_year(data_combined)


data_combined_2 %>% View


# 🟦 unique 연도 =================================================================
data_combined_3 = data_combined_2 %>% filter_unique_by_recent_year
View(data_combined_3)

# 🟦 unit 확인 =================================================================
data_combined_3$unit_L3
View(data_combined_3 )


# 🟦 export =================================================================
path_save = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID/@_목재/용도별 국내재 공급실적Domestic Timber Supply by Uses"
file_name = "용도별 국내재 공급실적.xlsx"
write.xlsx(data_combined_3, file.path(path_save, file_name))








