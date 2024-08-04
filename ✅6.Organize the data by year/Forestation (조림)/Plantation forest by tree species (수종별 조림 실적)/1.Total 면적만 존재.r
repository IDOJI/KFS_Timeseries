# 🟥 데이터 로드 ===================================================================================
path_data = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID/@_조림/(@완료)수종별 조림실적Plantation forest by tree species/@완료/1.Total 면적만 존재"
data.list = lapply(list.files(path_data, full.names=T), read.csv) %>% 
  setNames(tools::file_path_sans_ext(list.files(path_data)))





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
# names(data.list)
# data_1971 = data.list[names(data.list) == "1971_YRBK_00040309"][[1]]
# data_1972 = data.list[names(data.list) == "1972_YRBK_00050309"][[1]]
# View(data_1972 )
# 
# 
# # 기타 비교
# data_1971$수량_기타
# data_1972$수량_기타_1
# data_1972$수량_기타_2



# 🟥 데이터 합치기 ===================================================================================
data_df = bind_rows(data.list) 
names(data_df)



# 🟥 합계 체크  ===================================================================================
# test = data.list$`1973_YRBK_00060309`
# test$계_수량
# test %>% names
# View(test)


# 필요한 열들을 벡터로 지정
columns_to_sum <- c("리기다송_NA", "산오리_NA", "사방오리_NA", "물갬나무_NA", "아까시아_NA", "상수리_NA", 
                    "낙엽송_NA", "잣나무_NA", "해송_NA", "삼나무_NA", "편백_NA", "개량포푸라_NA", 
                    "밤나무_NA", "감나무_NA", "고염나무_NA", "호도나무_NA", "대추나무_NA", "옻나무_NA", 
                    "은행_NA", "오동_NA", "유동_NA", "삼지목_NA", "소나무_NA", "리기테다_NA", "테다_NA", 
                    "대나무_NA", "굴참나무_NA", "가래나무_NA", "기타_NA")

# 'test' 데이터프레임에서 위에서 지정한 열들만 선택
selected_columns <- test[, columns_to_sum]

# 선택된 열들의 합 계산
column_sums <- colSums(selected_columns, na.rm = TRUE)

# 결과 출력
data.frame(test$계_수량, column_sums)



# 🟥 열이름 체크 ===================================================================================
sapply(data.list, names)
sapply(data.list, ncol)


## 🟨 침엽수, 활엽수 ======================================================








## 🟨 특정 열들만 추출 ======================================================
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




## 🟨 열이름 통일 ======================================================
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



## 🟨 연도들만 추출  ======================================================
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
