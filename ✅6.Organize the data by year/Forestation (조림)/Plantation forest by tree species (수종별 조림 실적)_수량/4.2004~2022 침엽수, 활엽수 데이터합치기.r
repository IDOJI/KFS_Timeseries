# 🟥 데이터 로드  ======================================================================================
library(readr)
library(dplyr)

# 데이터가 저장된 경로
data_path <- "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/조림/수종별 조림실적Plantation forest by tree species/5.04~23_활엽수, 침엽수"

# 파일 목록 가져오기
path_files <- list.files(data_path, pattern = "\\.csv$", full.names = TRUE)

data.list = lapply(path_files, read.csv) %>% setNames(basename(path_files))




# 🟥 연도행만 가져오기 ======================================================================================
# 패턴을 포함하는 행 추출 함수
extract_rows <- function(df) {
  # 세 번째 열에 4자리 숫자가 포함된 행 추출
  df[grep("\\d{4}", df[[3]]), ]
}

# 각 데이터프레임에서 행을 추출하여 새로운 리스트에 저장
filtered_data.list <- lapply(data.list, extract_rows)

# 결과 출력
filtered_data.list[[1]] %>% View



# 🟨 3번째 열의 이름 확인 및 통일 ======================================================================================
sapply(filtered_data.list, function(x){
  names(x)[3]
}) %>% unique
for(k in seq_along(filtered_data.list)){
  
  names(filtered_data.list[[k]])[3] = "구분"
  
}




# 🟨 침엽수 활엽수 나누기 ======================================================================================
conifer.list <- list()
broadleaf.list <- list()

for(kth_data in filtered_data.list){
  
  if(grepl("침엽수", kth_data$NAME_L4[1])){
    conifer.list <- append(conifer.list, list(kth_data))
    
  } else if(grepl("활엽수", kth_data$NAME_L4[1])){
    broadleaf.list <- append(broadleaf.list, list(kth_data))
  }
}

# 결과 출력
conifer.list[[1]] %>% View
broadleaf.list



# 🟨 침엽수, 활엽수 문자열 붙이기 =====================================================================================
# conifer.list에 있는 각 데이터프레임의 열 이름 수정
for (i in seq_along(conifer.list)) {
  # 현재 데이터프레임
  df <- conifer.list[[i]]
  
  # 열 이름 찾기
  col_names <- colnames(df)
  start_index <- 4
  end_index <- which(col_names == "Categorized_L3_New") - 1
  
  # 열 이름 수정
  if (length(start_index:end_index) > 0) {
    colnames(df)[start_index:end_index] <- paste0("침엽수_", col_names[start_index:end_index])
  }
  
  # 수정된 데이터프레임을 다시 리스트에 저장
  conifer.list[[i]] <- df
}

# 결과 출력
conifer.list[[1]] %>% names


# broadleaf.list에 있는 각 데이터프레임의 열 이름 수정
for (i in seq_along(broadleaf.list)) {
  # 현재 데이터프레임
  df <- broadleaf.list[[i]]
  
  # 열 이름 찾기
  col_names <- colnames(df)
  start_index <- 4
  end_index <- which(col_names == "Categorized_L3_New") - 1
  
  # 열 이름 수정
  if (length(start_index:end_index) > 0) {
    colnames(df)[start_index:end_index] <- paste0("활엽수_", col_names[start_index:end_index])
  }
  
  # 수정된 데이터프레임을 다시 리스트에 저장
  broadleaf.list[[i]] <- df
}



# 결과 출력
broadleaf.list[[1]] %>% View






# 🟩 각 열이름 변경 ==============================================================
## 🟨 활엽수 ====================================================================
names_broadleaf = sapply(broadleaf.list, function(x){
  names(x)[3:20]
})

# 열 이름 변경 규칙을 적용하는 함수 정의
rename_columns <- function(df) {
  col_names <- colnames(df)
  
  # 열 이름 변경 규칙 적용
  if (any(grepl("고로쇠", col_names) & grepl("면적", col_names))) {
    col_names[grepl("고로쇠", col_names) & grepl("면적", col_names)] <- "활엽수_고로쇠_면적"
  }
  if (any(grepl("자작나무", col_names) & grepl("본수|그루수", col_names))) {
    col_names[grepl("자작나무", col_names) & grepl("본수|그루수", col_names)] <- "활엽수_자작나무_본수"
  }
  if (any(grepl("계|합계", col_names) & grepl("면적", col_names))) {
    col_names[grepl("계|합계", col_names) & grepl("면적", col_names)] <- "활엽수_계_면적"
  }
  if (any(grepl("계|합계", col_names) & grepl("본수|그루수", col_names))) {
    col_names[grepl("계|합계", col_names) & grepl("본수|그루수", col_names)] <- "활엽수_계_본수"
  }
  if (any(grepl("느티나무", col_names) & grepl("면적", col_names))) {
    col_names[grepl("느티나무", col_names) & grepl("면적", col_names)] <- "활엽수_느티나무_면적"
  }
  if (any(grepl("느티나무", col_names) & grepl("본수|그루수", col_names))) {
    col_names[grepl("느티나무", col_names) & grepl("본수|그루수", col_names)] <- "활엽수_느티나무_본수"
  }
  if (any(grepl("물푸레", col_names) & grepl("면적", col_names))) {
    col_names[grepl("물푸레", col_names) & grepl("면적", col_names)] <- "활엽수_물푸레_면적"
  }
  if (any(grepl("물푸레", col_names) & grepl("본수|그루수", col_names))) {
    col_names[grepl("물푸레", col_names) & grepl("본수|그루수", col_names)] <- "활엽수_물푸레_본수"
  }
  if (any(grepl("벚나무", col_names) & grepl("면적", col_names))) {
    col_names[grepl("벚나무", col_names) & grepl("면적", col_names)] <- "활엽수_벚나무_면적"
  }
  if (any(grepl("자작", col_names) & grepl("면적", col_names))) {
    col_names[grepl("자작", col_names) & grepl("면적", col_names)] <- "활엽수_자작나무_면적"
  }
  if (any(grepl("벚나무", col_names) & grepl("본수|그루수", col_names))) {
    col_names[grepl("벚나무", col_names) & grepl("본수|그루수", col_names)] <- "활엽수_벚나무_본수"
  }
  if (any(grepl("상수리", col_names) & grepl("면적", col_names))) {
    col_names[grepl("상수리", col_names) & grepl("면적", col_names)] <- "활엽수_상수리_면적"
  }
  if (any(grepl("상수리", col_names) & grepl("본수|그루수", col_names))) {
    col_names[grepl("상수리", col_names) & grepl("본수|그루수", col_names)] <- "활엽수_상수리_본수"
  }
  if (any(grepl("기타", col_names) & grepl("면적", col_names))) {
    col_names[grepl("기타", col_names) & grepl("면적", col_names)] <- "활엽수_기타_면적"
  }
  if (any(grepl("기타", col_names) & grepl("본수|그루수", col_names))) {
    col_names[grepl("기타", col_names) & grepl("본수|그루수", col_names)] <- "활엽수_기타_본수"
  }
  
  # 모든 조건을 만족하는 경우
  if (any(grepl("고로쇠", col_names) & grepl("본수", col_names))) {
    col_names[grepl("고로쇠", col_names) & grepl("본수", col_names)] <- "활엽수_고로쇠_본수"
  }
  if (any(grepl("고로쇠", col_names) & grepl("그루수", col_names))) {
    col_names[grepl("고로쇠", col_names) & grepl("그루수", col_names)] <- "활엽수_고로쇠_본수"
  }
  
  # 변경된 열 이름을 데이터프레임에 적용
  colnames(df) <- col_names
  return(df)
}

# broadleaf.list의 각 데이터프레임에 대해 열 이름 변경 함수 적용하여 새로운 리스트 생성
broadleaf.list_2 <- lapply(broadleaf.list, rename_columns)

# 결과 확인
broadleaf.list_2

names_broadleaf = sapply(broadleaf.list_2, function(x){
  names(x)[3:20]
})

View(names_broadleaf)

sapply(broadleaf.list_2, names)

ind = sapply(broadleaf.list_2, function(x){
  "활엽수_갑_본수" %in% names(x)
}) %>% which


broadleaf.list_2 = lapply(broadleaf.list_2, function(x){
  if("활엽수_갑_본수" %in% names(x)){
    
    x = x %>% rename("활엽수_계_본수" = "활엽수_갑_본수")
    
  }
  if("활엽수_구분" %in% names(x)){
    
    if(x[["활엽수_구분"]] %>% is.na %>% all){
      
      x[["활엽수_구분"]] = NULL
      
    }
  }
  x
})
# broadleaf.list_2[[7]] %>% View
# ind


broadleaf.df = bind_rows(broadleaf.list_2)
names(broadleaf.df)





## 🟨 침엽수 ====================================================================
names_conifer = sapply(conifer.list, function(x){
  names(x)[3:20]
})
print(names_conifer)


# 열 이름 변경 규칙을 적용하는 함수 정의
rename_columns_conifer <- function(df) {
  col_names <- colnames(df)
  
  # 열 이름 변경 규칙 적용
  if (any(grepl("소나무", col_names) & grepl("면적", col_names))) {
    col_names[grepl("소나무", col_names) & grepl("면적", col_names)] <- "침엽수_소나무_면적"
  }
  if (any(grepl("소나무", col_names) & grepl("본수|그루수", col_names))) {
    col_names[grepl("소나무", col_names) & grepl("본수|그루수", col_names)] <- "침엽수_소나무_본수"
  }
  if (any(grepl("짓나무", col_names) & grepl("면적", col_names))) {
    col_names[grepl("짓나무", col_names) & grepl("면적", col_names)] <- "침엽수_잣나무_면적"
  }
  if (any(grepl("짓나무", col_names) & grepl("본수|그루수", col_names))) {
    col_names[grepl("짓나무", col_names) & grepl("본수|그루수", col_names)] <- "침엽수_잣나무_본수"
  }
  
  if (any(grepl("잣나무", col_names) & grepl("면적", col_names))) {
    col_names[grepl("잣나무", col_names) & grepl("면적", col_names)] <- "침엽수_잣나무_면적"
  }
  if (any(grepl("잣나무", col_names) & grepl("본수|그루수", col_names))) {
    col_names[grepl("잣나무", col_names) & grepl("본수|그루수", col_names)] <- "침엽수_잣나무_본수"
  }
  
  
  if (any(grepl("낙엽송", col_names) & grepl("면적", col_names))) {
    col_names[grepl("낙엽송", col_names) & grepl("면적", col_names)] <- "침엽수_낙엽송_면적"
  }
  if (any(grepl("낙엽송", col_names) & grepl("본수|그루수", col_names))) {
    col_names[grepl("낙엽송", col_names) & grepl("본수|그루수", col_names)] <- "침엽수_낙엽송_본수"
  }
  if (any(grepl("리기다", col_names) & grepl("면적", col_names))) {
    col_names[grepl("리기다", col_names) & grepl("면적", col_names)] <- "침엽수_리기다_면적"
  }
  if (any(grepl("리기다", col_names) & grepl("본수|그루수", col_names))) {
    col_names[grepl("리기다", col_names) & grepl("본수|그루수", col_names)] <- "침엽수_리기다_본수"
  }
  if (any(grepl("삼나무", col_names) & grepl("면적", col_names))) {
    col_names[grepl("삼나무", col_names) & grepl("면적", col_names)] <- "침엽수_삼나무_면적"
  }
  if (any(grepl("삼나무", col_names) & grepl("본수|그루수", col_names))) {
    col_names[grepl("삼나무", col_names) & grepl("본수|그루수", col_names)] <- "침엽수_삼나무_본수"
  }
  if (any(grepl("편백", col_names) & grepl("면적", col_names))) {
    col_names[grepl("편백", col_names) & grepl("면적", col_names)] <- "침엽수_편백_면적"
  }
  if (any(grepl("편백", col_names) & grepl("본수|그루수", col_names))) {
    col_names[grepl("편백", col_names) & grepl("본수|그루수", col_names)] <- "침엽수_편백_본수"
  }
  if (any(grepl("해송", col_names) & grepl("면적", col_names))) {
    col_names[grepl("해송", col_names) & grepl("면적", col_names)] <- "침엽수_해송_면적"
  }
  if (any(grepl("해송", col_names) & grepl("본수|그루수", col_names))) {
    col_names[grepl("해송", col_names) & grepl("본수|그루수", col_names)] <- "침엽수_해송_본수"
  }
  if (any(grepl("기타", col_names) & grepl("면적", col_names))) {
    col_names[grepl("기타", col_names) & grepl("면적", col_names)] <- "침엽수_기타_면적"
  }
  if (any(grepl("기타", col_names) & grepl("본수|그루수", col_names))) {
    col_names[grepl("기타", col_names) & grepl("본수|그루수", col_names)] <- "침엽수_기타_본수"
  }
  if (any(grepl("계|합계", col_names) & grepl("면적", col_names))) {
    col_names[grepl("계|합계", col_names) & grepl("면적", col_names)] <- "침엽수_계_면적"
  }
  if (any(grepl("계|합계", col_names) & grepl("본수|그루수", col_names))) {
    col_names[grepl("계|합계", col_names) & grepl("본수|그루수", col_names)] <- "침엽수_계_본수"
  }
  
  # 변경된 열 이름을 데이터프레임에 적용
  colnames(df) <- col_names
  return(df)
}

# conifer.list의 각 데이터프레임에 대해 열 이름 변경 함수 적용하여 새로운 리스트 생성
conifer.list_2 <- lapply(conifer.list, rename_columns_conifer)

# 결과 확인
conifer.list_2
names_conifer = sapply(conifer.list_2, function(x){
  names(x)[3:20]
})
print(names_conifer)




conifer.list_3 = lapply(conifer.list_2, function(x){
  if( "침엽수_구분" %in% names(x)){
    
    if(x[[ "침엽수_구분"]] %>% is.na %>% all){
      
      x[[ "침엽수_구분"]] = NULL
      
    }
  }
  
  if( "침엽수_No.ofsdls._구분" %in% names(x)){
    
    if(x[[ "침엽수_No.ofsdls._구분"]] %>% is.na %>% all){
      
      x[[ "침엽수_No.ofsdls._구분"]] = NULL
      
    }
  }
  x
})




conifer.df = bind_rows(conifer.list_3)
names(conifer.df)
conifer.df = conifer.df %>% relocate(contains("해송"), .after = 20) %>% relocate(Categorized_L3_New, .before = Categorized_L3)
names(conifer.df)








# 🟥 2012_1, 2012_2 =========================================================================
library(dplyr)

broadleaf.df <- broadleaf.df %>%
  mutate(구분 = if_else(구분 == "2012_1", "2012",
                      if_else(구분 == "2012_2", "2013", 구분))) %>% 
  arrange(구분, year)




conifer.df  = conifer.df %>% arrange(구분, year)
View(conifer.df)





# 🟥 합계 비교=========================================================================
## 🟨 침엽수 =============================================================================
# "_본수"로 끝나는 열을 합산하고, "침엽수_계_본수"와 비교하는 열을 생성하는 함수
compare_columns <- function(df) {
  # "_본수"로 끝나는 열 이름 추출 (단, "침엽수_계_본수"는 제외)
  bonsoo_columns <- grep("_본수$", names(df), value = TRUE)
  bonsoo_columns <- setdiff(bonsoo_columns, "침엽수_계_본수")
  
  # "_본수" 열 합산
  df$총_본수 <- rowSums(df[bonsoo_columns], na.rm = TRUE)
  
  # "침엽수_계_본수"와 합산한 총 본수의 차이를 비교하는 열 생성
  df$본수_차이 <- df$총_본수 - df$침엽수_계_본수
  
  # 새로운 열을 "침엽수_계_본수" 바로 뒤로 이동
  df <- df[c(names(df)[1:match("침엽수_계_본수", names(df))], "총_본수", "본수_차이", names(df)[(match("침엽수_계_본수", names(df)) + 1):(ncol(df) - 2)])]
  
  return(df)
}

# "_면적"으로 끝나는 열에 대해 동일한 작업을 수행하는 함수
compare_columns_area <- function(df) {
  # "_면적"으로 끝나는 열 이름 추출 (단, "침엽수_계_면적"은 제외)
  area_columns <- grep("_면적$", names(df), value = TRUE)
  area_columns <- setdiff(area_columns, "침엽수_계_면적")
  
  # "_면적" 열 합산
  df$총_면적 <- rowSums(df[area_columns], na.rm = TRUE)
  
  # "침엽수_계_면적"와 합산한 총 면적의 차이를 비교하는 열 생성
  df$면적_차이 <- df$총_면적 - df$침엽수_계_면적
  
  # 새로운 열을 "침엽수_계_면적" 바로 뒤로 이동
  df <- df[c(names(df)[1:match("침엽수_계_면적", names(df))], "총_면적", "면적_차이", names(df)[(match("침엽수_계_면적", names(df)) + 1):(ncol(df) - 2)])]
  
  return(df)
}

# "본수"에 대한 작업 수행
conifer.df <- compare_columns(conifer.df)

# "면적"에 대한 작업 수행
conifer.df <- compare_columns_area(conifer.df)

# 결과 확인
print(conifer.df)
View(conifer.df)
names(conifer.df)

library(dplyr)

# 열 이름 앞에 "침엽수_"를 붙이는 코드
conifer.df <- conifer.df %>%
  rename_with(~ paste0("침엽수_", .), c("총_면적", "면적_차이", "총_본수", "본수_차이"))

# 결과 확인
print(colnames(conifer.df))



## 🟨 활엽수 =============================================================================
names(broadleaf.df)

# "_본수"로 끝나는 열을 합산하고, "활엽수_계_본수"와 비교하는 열을 생성하는 함수
compare_columns_broadleaf <- function(df) {
  # "_본수"로 끝나는 열 이름 추출 (단, "활엽수_계_본수"는 제외)
  bonsoo_columns <- grep("_본수$", names(df), value = TRUE)
  bonsoo_columns <- setdiff(bonsoo_columns, "활엽수_계_본수")
  
  # "_본수" 열 합산
  df$총_본수 <- rowSums(df[bonsoo_columns], na.rm = TRUE)
  
  # "활엽수_계_본수"와 합산한 총 본수의 차이를 비교하는 열 생성
  df$본수_차이 <- df$총_본수 - df$활엽수_계_본수
  
  # 새로운 열을 "활엽수_계_본수" 바로 뒤로 이동
  df <- df[c(names(df)[1:match("활엽수_계_본수", names(df))], "총_본수", "본수_차이", names(df)[(match("활엽수_계_본수", names(df)) + 1):(ncol(df) - 2)])]
  
  return(df)
}

# "_면적"으로 끝나는 열에 대해 동일한 작업을 수행하는 함수
compare_columns_area_broadleaf <- function(df) {
  # "_면적"으로 끝나는 열 이름 추출 (단, "활엽수_계_면적"은 제외)
  area_columns <- grep("_면적$", names(df), value = TRUE)
  area_columns <- setdiff(area_columns, "활엽수_계_면적")
  
  # "_면적" 열 합산
  df$총_면적 <- rowSums(df[area_columns], na.rm = TRUE)
  
  # "활엽수_계_면적"와 합산한 총 면적의 차이를 비교하는 열 생성
  df$면적_차이 <- df$총_면적 - df$활엽수_계_면적
  
  # 새로운 열을 "활엽수_계_면적" 바로 뒤로 이동
  df <- df[c(names(df)[1:match("활엽수_계_면적", names(df))], "총_면적", "면적_차이", names(df)[(match("활엽수_계_면적", names(df)) + 1):(ncol(df) - 2)])]
  
  return(df)
}

# "본수"에 대한 작업 수행
broadleaf.df <- compare_columns_broadleaf(broadleaf.df)

# "면적"에 대한 작업 수행
broadleaf.df <- compare_columns_area_broadleaf(broadleaf.df)

# 결과 확인
View(broadleaf.df)
names(broadleaf.df)

broadleaf.df = broadleaf.df %>% 
  rename_with(~ paste0("활엽수_", .), c("총_면적", "면적_차이", "총_본수", "본수_차이"))

# 🟪 활엽수 침엽수 합치기 ======================================================================================
names(broadleaf.df)
names(conifer.df)
all(conifer.df$구분 == broadleaf.df$구분)
all(conifer.df$year == broadleaf.df$year)
  
combined_data.df = cbind(conifer.df[1:25], 
                         broadleaf.df[4:23], 
                         conifer.df[26:ncol(conifer.df)]) %>% 
  relocate(year, .after = "구분")




# 🟪 하나의 행만 추출 ======================================================================================
conifer.df$구분 
combined_data.df %>% View


# 중복값을 "year"을 기준으로 최신 연도로 선택하는 함수
remove_duplicates <- function(df) {
  # 데이터프레임을 "구분"을 기준으로 그룹화하고, "year"의 최신 값만 남김
  df <- df[order(df$구분, -df$year), ]
  df <- df[!duplicated(df$구분), ]
  
  return(df)
}


# 중복 제거 함수 적용
combined_data.df_2 <- remove_duplicates(combined_data.df)

# 결과 확인
View(combined_data.df_2)


# 🟥 계 열이름 변경 ======================================================================================
names(combined_data.df_2)

# 열 이름 변경
colnames(combined_data.df_2) <- gsub("_계_", "_합계_", colnames(combined_data.df_2))

# 결과 확인
print(colnames(combined_data.df_2))

# 🟥 Export ======================================================================================
path_save = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/조림/수종별 조림실적Plantation forest by tree species/Combined"
file_name = "4.Combined_04~22"
write.xlsx(combined_data.df_2, file.path(path_save, paste0(file_name, ".xlsx")))



















