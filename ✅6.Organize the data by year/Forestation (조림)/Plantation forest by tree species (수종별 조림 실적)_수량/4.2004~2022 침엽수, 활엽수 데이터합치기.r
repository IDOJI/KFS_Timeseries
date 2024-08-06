# 🟥 데이터 로드  ======================================================================================
library(readr)
library(dplyr)

# 데이터가 저장된 경로
data_path <- "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID/@_조림/(@완료)수종별 조림실적Plantation forest by tree species/@완료/4.2004~2022 침엽수, 활엽수 데이터합치기"

# 파일 목록 가져오기
files <- list.files(data_path, pattern = "\\.csv$", full.names = TRUE)
print(files)

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





# 🟥 열 체크 ======================================================================================
## 🟩 열 개수 체크 ==============================================================
sapply(conifer_list, ncol) %>% unique
sapply(broadleaf_list, ncol) %>% unique






## 🟩 열이름 체크 ==============================================================
### 🟨 침엽수 ===================================================================
sapply(conifer_list, names) %>% View

conifer_list_2 = lapply(conifer_list,rearrange_columns)

sapply(conifer_list_2, names) %>% View

# 열 이름 확인
for(k in 5:15){
  get_unique_column_names(conifer_list_2, k) %>% print
}


# 짓나무 -> 잣나무
conifer_list_2 = rename_column_in_list_korean_pine(conifer_list_2)
for(k in 5:11){
  get_unique_column_names(conifer_list_2, k) %>% print
}





### 🟨 활엽수 ===================================================================
sapply(broadleaf_list, names) %>% View


# 이름변경
names(broadleaf_list$`2016`) = names(broadleaf_list$`2017`)

sapply(broadleaf_list, names) %>% View





# 🟥 각 데이터 합치기 =========================================================================
conifer_df = bind_rows(conifer_list_2) %>% 
  relocate("해송_면적", .after = "삼나무_면적")
broadleaf_df = bind_rows(broadleaf_list)
# View(conifer_df)
broadleaf_df %>% View


# 🟥 2012_1, 2012_2 =========================================================================
library(dplyr)

broadleaf_df <- broadleaf_df %>%
  mutate(구분 = if_else(구분 == "2012_1", "2012",
                      if_else(구분 == "2012_2", "2013", 구분)))





# 🟥 침엽수 활엽수 합치기 =========================================================================
## 🟩 열이름에 침엽수, 활엽수 문자열 추가 ==============================================================
conifer_df_2 = add_prefix_to_columns(conifer_df, "_면적", "침엽수_")
broadleaf_df_2 = add_prefix_to_columns(broadleaf_df, "_면적", "활엽수_")
View(conifer_df_2)
View(broadleaf_df_2)


## 🟩 데이터합치기 ==============================================================
names(conifer_df_2)[3:12]
names(broadleaf_df_2)[4:11]


if(all(conifer_df_2$구분 == broadleaf_df_2$구분) && nrow(conifer_df_2) == nrow(broadleaf_df_2)){
  
  combined.df = cbind(conifer_df_2[3:12], broadleaf_df_2[4:11])

}else{
  stop("Check the columns!!")
}




View(combined.df)


# 🟥 각 합계 비교 ======================================================================================
combined.df %>% names

library(dplyr)

# 침엽수 관련 열 합 계산 및 비교, NA 값 제외하고 소수점 첫째자리에서 반올림
combined.df %>%
  mutate(침엽수_합 = round(rowSums(select(., starts_with("침엽수_"), -침엽수_계_면적), na.rm = TRUE), 1)) %>%
  mutate(침엽수_비교 = 침엽수_합 == round(침엽수_계_면적, 1)) %>%
  select(침엽수_계_면적, 침엽수_합, 침엽수_비교)

# 활엽수 관련 열 합 계산 및 비교, NA 값 제외하고 소수점 첫째자리에서 반올림
combined.df %>%
  mutate(활엽수_합 = round(rowSums(select(., starts_with("활엽수_"), -활엽수_계_면적), na.rm = TRUE), 1)) %>%
  mutate(활엽수_비교 = 활엽수_합 == round(활엽수_계_면적, 1)) %>%
  select(활엽수_계_면적, 활엽수_합, 활엽수_비교)






# 🟥 활엽수 침엽수 정보 추가 ======================================================================================
names(conifer_df_2)[c(1,13:ncol(conifer_df_2))]
names(broadleaf_df_2)[c(1:3, 12:ncol(broadleaf_df_2))]
selected_conifer = conifer_df_2[c(1,13:ncol(conifer_df_2))]
selected_broadleaf = broadleaf_df_2[c(1:3, 12:ncol(broadleaf_df_2))]

names(selected_conifer) = paste0("침엽수_", names(selected_conifer))
names(selected_broadleaf) = paste0("활엽수_", names(selected_broadleaf))

combined.df_2 = cbind(combined.df, selected_conifer, selected_broadleaf)



# 🟥 Export ======================================================================================
path_save = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID/@_조림/(@완료)수종별 조림실적Plantation forest by tree species/@완료"
file_name = "4.2004~2022 침엽수, 활엽수 데이터합치기"
write.xlsx(combined.df_2, file.path(path_save, paste0(file_name, ".xlsx")))





# 🟥 실제 데이터와 비교 ======================================================================================














