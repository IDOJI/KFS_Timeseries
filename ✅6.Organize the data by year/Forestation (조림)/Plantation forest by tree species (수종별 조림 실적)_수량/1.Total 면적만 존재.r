# 🟥 데이터 로드 ===================================================================================
path_data = '/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID/@_조림/_(@완료)수종별 조림실적Plantation forest by tree species/@완료/1.Total 면적만 존재'
data.list = lapply(list.files(path_data, full.names=T), read.csv) %>% 
  setNames(tools::file_path_sans_ext(list.files(path_data)))

data.list[[1]] %>% View



# 🟥 NA 열이름 처리 ===================================================================================
# data.list라는 리스트에 데이터프레임들이 포함되어 있다고 가정
# 리스트의 각 데이터프레임에 대해 작업 수행
for (i in seq_along(data.list)) {
  # 현재 데이터프레임의 열 이름을 가져옴
  col_names <- names(data.list[[i]])
  
  # "_NA"가 포함된 열 이름에 대해 수정 작업 수행
  col_names <- sapply(col_names, function(name) {
    if (grepl("_NA", name)) {
      # "_NA"를 제거하고 앞에 "수량_"을 추가
      paste0("수량_", sub("_NA", "", name))
    } else {
      # "_NA"가 없는 열 이름은 그대로 유지
      name
    }
  })
  
  # 수정된 열 이름을 현재 데이터프레임에 적용
  names(data.list[[i]]) <- col_names
}



# 결과를 확인
# print(data.list)


# test = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID/@_조림/_(@완료)수종별 조림실적Plantation forest by tree species/@완료/1.Total 면적만 존재.xlsx"
# test = read.xlsx(test)
# View(test)

# 🟥 열이름 체크 ===================================================================================
sapply(data.list, names)






# 🟥 합계가 수량인지 체크 ===================================================================================
test = data.list[[1]]
# View(test)
values <- c(58808, 22486, 3336, 3358, 34351, 22896, 47386, 2098, 20043, 5596, 
            4201, NA, 7646, 286, 538, 273, NA, 62, 94, 23, NA, NA, NA, NA, 
            NA, 32, 6, 11307)
sum(values, na.rm = T)



library(dplyr)

# 각 데이터프레임에 대해 계산을 수행하고 결과를 저장할 리스트 초기화
comparison_results <- list()

# data.list의 각 데이터프레임에 대해 반복
for (name in names(data.list)) {
  # name = names(data.list)[1]
  df <- data.list[[name]]
  names(df)
  # "수량_"으로 시작하는 열들의 합계 계산
  quantity_sum <- df %>%
    select(starts_with("수량_")) %>%
    rowSums(na.rm = TRUE)
  
  # 각 데이터프레임의 "계_면적"과 "계_수량" 가져오기
  total_area <- df$계_면적
  total_quantity <- df$계_수량
  
  # 결과를 리스트에 저장
  comparison_results[[name]] <- data.frame(
    DataFrame = name,
    Total_Area = total_area,
    Sum_Quantity = total_quantity,
    Calculated_Quantity_Sum = quantity_sum
  )
}

# 모든 결과를 하나의 데이터 프레임으로 결합
final_results <- do.call(rbind, comparison_results)
print(final_results)
View(final_results)




# 🟥 1972년도 기타.1 기타.2 처리 ===================================================================================
names(data.list)
data_1971 = data.list[names(data.list) == "1971_YRBK_00040309"][[1]]
data_1972 = data.list[names(data.list) == "1972_YRBK_00050309"][[1]]
View(data_1972 )



# 기타 비교
data_1971 %>% names
data_1972$수량_기타_1
data_1972$수량_기타_2



# 실제 연보에도 존재하면 그냥 기타 로 퉁치기
data_1972_2 = data_1972 %>% 
  mutate(수량_기타 = sum(수량_기타_1, 수량_기타_2, rm.na=TRUE)) %>% 
  select(-"수량_기타_1", -"수량_기타_2") %>%
  relocate(starts_with("수량_"), .after=3) %>% 
  relocate(starts_with("계_"), .after=3)
  
data.list[names(data.list) == "1972_YRBK_00050309"][[1]] = data_1972_2





# 🟪 열이름 체크 ===================================================================================
sapply(data.list, names)
sapply(data.list, ncol)


# 🟪 3번째 열이름 "구분" ===================================================================================
data.list_2 = lapply(data.list, function(x){
  x %>% rename(구분 = names(x)[3])
})



# 🟪 "면적" 열이름 통일 ===================================================================================
data.list_3 = lapply(data.list_2, function(x){
  x %>% rename("계_면적" = grep("면적", names(x), value = T))
})





# 🟥 데이터 합치기 ===================================================================================
data_df = bind_rows(data.list_3) %>% 
  relocate(starts_with("수량_"), .after=3) %>% 
  relocate(starts_with("계_"), .after=3)
names(data_df)
View(data_df)
data_df %>% filter(!is.na(수량_기타_1)) %>% View




# 🟥 연도 행만 추출  ======================================================
# data_df의 두 번째 열 추출
second_column <- data_df[[3]]

# 숫자 4자리로 시작하거나 끝나는 문자열을 찾기 위한 정규 표현식
is_year <- grepl("^\\d{4}|\\d{4}$", second_column)

# 연도에 해당하는 행만 추출
year_rows <- data_df[is_year, ]

# 결과 출력
print(year_rows)

View(year_rows)




# 🟨 수량 합계  =======================================================================
names(year_rows)

library(dplyr)

# "수량_"으로 시작하는 열들의 합계를 구하고, 차이가 1만 이상인 경우만 선택
result_df <- year_rows %>%
  rowwise() %>%
  mutate(합계_수량 = sum(c_across(starts_with("수량_")), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(차이 = abs(합계_수량 - 계_수량)) %>%
  # filter(차이 >= 10000) %>%
  select(합계_수량, 계_수량, 차이)

# 새로운 데이터프레임 출력
View(result_df)

result_df_2 = result_df[result_df$차이 > 100,]
View(result_df_2)


# 합계 차이가 많이 나는 데이터 직접 확인
data_to_check = year_rows %>% 
  filter(계_수량 %in%  result_df_2$계_수량) %>% 
  relocate(year, .after = "구분")

data_to_check %>% View



# 🟨 ID 추출해서 실제 연보와 확인 비교 후 데이터 변형 =============================================================================
IDs_to_check = data_to_check$ID %>% unique











# 🟥 연도 행만 추출  ======================================================
path_save = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID/@_조림/(@완료)수종별 조림실적Plantation forest by tree species/@완료/1.Total 면적만 존재"
write.xlsx(year_rows, file.path(path_save, "1.Combined.xlsx"))
# year_rows$year

