# 🟥 데이터 로드 ====================================================================================
path_data = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/숲가꾸기/숲 가꾸기_학부생들.xlsx"
data = read.xlsx(path_data)
View(data)




# 🟨 데이터 정리 =================================================================================
## 🌫️ 무육 =======================================================================================
sub_data = data[, names(data) == "무육"]
names(sub_data) = c("A", "B")
sapply(1:nrow(sub_data), function(i){
  
  if(is.na(sub_data[i, 1])){
    sub_data[i, 1] = "NA"
  }
  if(is.na(sub_data[i, 2])){
    sub_data[i, 2] = "NA"
  }
  
  if(sub_data[i,1] != sub_data[i, 2]){
    stop(i)
  }
})


# remove
names(data)[8]
data[,8] = NULL


## 🌫️ 덩굴 제거 =======================================================================================
data_2 <- data %>%
  mutate(덩굴제거 = ifelse(is.na(덩굴제거), 만경류제거, 덩굴제거)) %>% 
  select(-만경류제거)




# 🟨 year 추가 =================================================================
## year 추출
path_files = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/숲가꾸기/숲 가꾸기Forest tending"
file_names = list.files(path_files) %>% gsub(".csv", "", .)

# 연도 추출 및 매칭
data_2$year <- sapply(data_2$ID, function(id) {
  # id = data$ID[1]
  # 해당 ID를 포함하는 file_names에서 추출
  matched_file <- file_names[grep(id, file_names)]
  
  # 매칭된 파일이 있으면 연도 추출, 없으면 NA
  if (length(matched_file) > 0) {
    return(sub("_.*", "", matched_file[1]))  # 첫 번째 매칭된 파일의 앞부분 연도 추출
  } else {
    return(NA)
  }
}) %>% as.character

names(data_2)


# 열 이름에서 중복된 이름이 있는지 확인
duplicated_names <- duplicated(names(data_2))

# 중복된 열 이름 출력
names(data_2)[duplicated_names]


#  NA year 체크
data_2 %>% dplyr::filter(is.na(year)) %>% View




# 🟦 export =================================================================
path_save = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID/@_산림피해/산림병해충 발생 및 방제상황5 Forest Damage Occurrence and Prevention by Forest Pest Insect and Disease"
combined.list = list(Area = df_with_area.df_3, Seedling = df_without_area.df_3)
file_name = "산림병해충 발생 및 방제상황.rds"
saveRDS(combined.list, file.path(path_save, file_name))








