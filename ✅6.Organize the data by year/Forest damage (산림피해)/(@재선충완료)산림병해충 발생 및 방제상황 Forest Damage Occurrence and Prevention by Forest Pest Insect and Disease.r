# 🟥 데이터 로드 =================================================================
path_data = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID/@_산림피해/산림병해충 발생 및 방제상황5 Forest Damage Occurrence and Prevention by Forest Pest Insect and Disease"
data.list = lapply(list.files(path_data, full.names=T), read.csv) %>% 
  setNames(list.files(path_data))
data.list[[1]] %>% View


# 🟪 "2011_YRBK_00410302" ===============================================================
# 대상 데이터 프레임의 이름
target_name <- "2011_YRBK_00410302.csv"

# data.list에서 해당 데이터 프레임의 위치 찾기
target_index <- which(names(data.list) == target_name)

# 해당 데이터 프레임의 열 이름 수정
if (length(target_index) == 1) {
  # 데이터 프레임 추출
  df <- data.list[[target_index]]
  
  # 열 이름 수정
  new_colnames <- names(df)
  new_colnames <- gsub("_1", "_발생면적", new_colnames)
  new_colnames <- gsub("_2", "_방제면적", new_colnames)
  
  # 수정된 열 이름 적용
  colnames(df) <- new_colnames
  
  # 수정된 데이터 프레임을 다시 리스트에 저장
  data.list[[target_index]] <- df
}

# 결과 확인
print(names(data.list[[target_index]]))


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




# 🟨 재선충 열 있는 데이터들만 추출=================================================================
library(purrr)
library(stringr)

# "재선충"이라는 문자열을 포함하는 열 이름이 있는 데이터 프레임만 남김
filtered_data_list <- keep(data.list, function(df) {
  any(str_detect(names(df), "재선충"))
})

# 결과 확인
length(filtered_data_list)
names(filtered_data_list)

# filtered_data_list$`2011_YRBK_00410302.csv` %>% View


# 🟨 재선충 열만 남기기 =================================================================
library(purrr)
library(dplyr)
library(stringr)

filtered_data_list_2 <- map(filtered_data_list, function(df) {
  # 'Categorized_L3_New' 또는 'L3_New' 열의 인덱스 찾기
  index_Categorized <- which(names(df) %in% c("Categorized_L3_New", "L3_New"))
  
  # 기본적으로 1~3열 유지
  final_columns <- names(df)[1:3]
  
  # 'Categorized_L3_New' 열이 존재하는 경우 추가 열 선택
  if (length(index_Categorized) == 1 && index_Categorized > 4) {
    # 4번째 열부터 'Categorized_L3_New' 전까지의 열 중 "재선충" 포함 열 선택
    start_index <- 4
    end_index <- index_Categorized - 1
    selected_columns <- names(df)[start_index:end_index]
    relevant_columns <- selected_columns[str_detect(selected_columns, "재선충")]
    
    # 'Categorized_L3_New'부터 마지막 열까지 포함
    categorized_and_after <- names(df)[index_Categorized:ncol(df)]
    
    # 최종적으로 유지할 열들: 1~3열, relevant_columns, 그리고 categorized_and_after
    final_columns <- c(final_columns, relevant_columns, categorized_and_after)
  } else {
    # 'Categorized_L3_New' 또는 'L3_New' 열이 없으면 4번째 열부터 끝까지 고려
    selected_columns <- names(df)[4:ncol(df)]
    
    # "재선충"을 포함하는 열들만 선택
    relevant_columns <- selected_columns[str_detect(selected_columns, "재선충")]
    
    # 최종적으로 유지할 열들: 1~3열과 relevant_columns
    final_columns <- c(final_columns, relevant_columns)
  }
  
  # 해당 열들만 남기기
  df <- df %>%
    select(all_of(final_columns))
  
  return(df)
})


# 결과 확인
filtered_data_list_2 %>% length
filtered_data_list_2 [[1]]
filtered_data_list_2$`2011_YRBK_00410302.csv` %>% View


# 🟩 (@완료)에러 데이터 확인 -> 처음부터 코드 다시 실행  =================================================================
# library(purrr)
# library(dplyr)
# 
# # 4번째 열이 모두 NA값인 데이터 프레임의 위치 찾기
# na_columns_indices <- map_lgl(filtered_data_list_2, function(df) {
#   if (ncol(df) >= 4) {
#     all(is.na(df[[4]]))
#   } else {
#     FALSE
#   }
# })
# 
# # NA 열 위치 인덱스 추출
# na_indices <- which(na_columns_indices)
# 
# # 결과 확인
# print(na_indices)
# 
# 
# 
# # YRBK_00410302
# ID = "YRBK_00410302"








# 🟪 열 이름 변경 =================================================================
sapply(filtered_data_list_2, names)

## 🟧 면적 ================================================================================
library(purrr)
library(stringr)

# "면적" 문자열을 포함하는 데이터 프레임 추출
df_with_area <- keep(filtered_data_list_2, function(df) {
  ncol(df) >= 5 && 
    (str_detect(names(df)[4], "면적") || str_detect(names(df)[5], "면적"))
})

# 각 데이터 프레임의 5번째 열 이름에 "방제"가 포함되었는지 확인
has_control_in_fifth_column <- map_lgl(df_with_area, function(df) {
  if (ncol(df) >= 5) {
    str_detect(names(df)[5], "방제")
  } else {
    FALSE
  }
})
print(has_control_in_fifth_column)


# 열의 수
sapply(df_with_area, ncol) %>% unique


# 
names(df_with_area[[1]])


library(purrr)

# 각 데이터 프레임의 4번째 및 5번째 열 이름 변경
df_with_area <- map(df_with_area, function(df) {
  if (ncol(df) >= 5) {
    colnames(df)[4] <- "소나무재선충_발생면적"
    colnames(df)[5] <- "소나무재선충_방제면적"
  } else if (ncol(df) == 4) {
    colnames(df)[4] <- "소나무재선충_발생면적"
  }
  return(df)
})

# 결과 확인
print(df_with_area)
sapply(df_with_area, names)





## 🟧 본수 ================================================================================
# "면적" 문자열을 포함하지 않는 데이터 프레임 추출
df_without_area <- keep(filtered_data_list_2, function(df) {
  ncol(df) < 5 ||
    (!str_detect(names(df)[4], "면적") && !str_detect(names(df)[5], "면적"))
})



# 각 데이터 프레임의 5번째 열 이름에 "방제"가 포함되었는지 확인
has_control_in_fifth_column <- map_lgl(df_without_area, function(df) {
  if (ncol(df) >= 5) {
    str_detect(names(df)[5], "방제")
  } else {
    FALSE
  }
})
print(has_control_in_fifth_column)


# 열이름 확인
sapply(df_without_area, names)



library(purrr)

# 각 데이터 프레임의 4번째 및 5번째 열 이름 변경
df_without_area <- map(df_without_area, function(df) {
  if (ncol(df) >= 5) {
    colnames(df)[4] <- "소나무재선충_발생본수"
    colnames(df)[5] <- "소나무재선충_방제본수"
  } else if (ncol(df) == 4) {
    colnames(df)[4] <- "소나무재선충_발생본수"
  }
  return(df)
})

# 결과 확인
print(df_without_area)
df_without_area[[1]] %>% View




# 🟦 각 데이터 합치기=================================================================
df_with_area.df = bind_rows(df_with_area) %>% arrange(구분, year) %>% relocate(year, .after = 구분)
names(df_with_area.df)

df_without_area.df = bind_rows(df_without_area) %>% arrange(구분, year) %>% relocate(year, .after = 구분)
names(df_without_area.df)




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


path_file = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID/@_산림피해/(@재선충완료)산림병해충 발생 및 방제상황5 Forest Damage Occurrence and Prevention by Forest Pest Insect and Disease/산림병해충 발생 및 방제상황_소나무재선충.rds"
combined.list = readRDS(path_file)

path_save = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID/@_산림피해/(@재선충완료)산림병해충 발생 및 방제상황5 Forest Damage Occurrence and Prevention by Forest Pest Insect and Disease"
file_name = "산림병해충 발생 및 방제상황_소나무재선충_면적.xlsx"
write.xlsx(combined.list$Area, file.path(path_save, file_name))


path_save = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID/@_산림피해/(@재선충완료)산림병해충 발생 및 방제상황5 Forest Damage Occurrence and Prevention by Forest Pest Insect and Disease"
file_name = "산림병해충 발생 및 방제상황_소나무재선충_그루수.xlsx"
write.xlsx(combined.list$Seedling, file.path(path_save, file_name))






