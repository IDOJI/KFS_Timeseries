# 🟥 데이터 로드 =================================================================
path_data = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID/@_숲가꾸기/숲 가꾸기Forest tending"
data.list = lapply(list.files(path_data, full.names=T, pattern = "\\.csv$"), read.csv) %>% 
  setNames(list.files(path_data, pattern = "\\.csv$"))




# 🟨 3번째 열이름을 구분으로 통일 =================================================================
library(purrr)

# 각 데이터 프레임의 3번째 열 이름 추출
third_column_names <- map(data.list, function(df) {
  if (ncol(df) >= 3) {
    return(names(df)[3])
  } else {
    return(NA) # 3번째 열이 없는 경우 NA 반환
  }
})

# 결과 확인
print(third_column_names)




# 각 데이터 프레임의 3번째 열 이름을 "구분"으로 변경
data.list <- map(data.list, function(df) {
  if (ncol(df) >= 3) {
    colnames(df)[3] <- "구분"
  }
  return(df)
})

# 결과 확인: 각 데이터 프레임의 열 이름
print(map(data.list, names))



# 🟦 데이터 합치기=================================================================
data_combined = bind_rows(data.list)
View(data_combined)




data.list
# 🟦 연도 행만 추출=================================================================
df_with_area.df_2 = filter_by_year(df_with_area.df)
df_without_area.df_2 = filter_by_year(df_without_area.df)

df_with_area.df_2 %>% View
df_without_area.df_2 %>% View



# 🟦 unique 연도 =================================================================
df_with_area.df_3 = df_with_area.df_2 %>% filter_unique_by_recent_year
df_without_area.df_3 = df_without_area.df_2 %>% filter_unique_by_recent_year

View(df_with_area.df_3)
View(df_without_area.df_3)

# 🟦 unit 확인 =================================================================
df_with_area.df_3$unit_L3
df_without_area.df_3$unit_L3


# 🟦 export =================================================================
path_save = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID/@_산림피해/산림병해충 발생 및 방제상황5 Forest Damage Occurrence and Prevention by Forest Pest Insect and Disease"
combined.list = list(Area = df_with_area.df_3, Seedling = df_without_area.df_3)
file_name = "산림병해충 발생 및 방제상황.rds"
saveRDS(combined.list, file.path(path_save, file_name))








