## 🟧 4.2000 ===================================================================================
### 🟩 데이터 로드  ======================================================
path_data = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID/@_조림/(@완료)수종별 조림실적Plantation forest by tree species/@완료/3.2000년도_수제작/2000_YRBK_0030030501.xlsx"
data_2000 = read.xlsx(path_data)
# View(data_2000)


### 🟩 ID  ======================================================
data_2000 $ID = data_2000 $ID[1]
data_2000[[2]] = NULL
data_2000[[2]] = data_2000[[2]][1]


data_2000 = data_2000 %>% 
  mutate(year = "2000") %>% 
  relocate(year, .after = ID)



### 🟩 본수 제외  ======================================================
# "본수"라는 문자열을 포함하는 열 이름을 찾기
cols_to_exclude <- grep("본수", names(data_2000), value = TRUE)

# "본수"라는 문자열을 포함하지 않는 열 이름 선택
data_2000_filtered <- data_2000[, !names(data_2000) %in% cols_to_exclude]

# 결과 확인
print(names(data_2000_filtered))



### 🟩 침엽수 이름 추가  ======================================================
# 수정할 열 이름 목록
columns_to_modify <- c(
  "잣나무_면적", "낙엽송_면적", "리기다_면적", "리기테다_면적", 
  "강송_면적", "해송_면적", "소나무_면적", "편백_면적", 
  "전나무_면적", "스트로브잣나무_면적", "화백_면적", "메타세콰이어_면적"
)

# 데이터프레임의 열 이름 수정
for (col in columns_to_modify) {
  if (col %in% names(data_2000_filtered)) {
    names(data_2000_filtered)[names(data_2000_filtered) == col] <- paste0("침엽수_", col)
  }
}

# 결과 확인
print(names(data_2000_filtered))



### 🟩 "계" 열들 옮기기 ======================================================
# "계"라는 문자열을 포함하는 열 이름을 찾기
kei_columns <- grep("계", names(data_2000_filtered), value = TRUE)

# "계" 열을 제외한 나머지 열 이름 찾기
remaining_columns <- setdiff(names(data_2000_filtered), kei_columns)

# 새로운 열 순서 지정 ("계" 열을 두 번째 위치로 이동)
new_column_order <- c(remaining_columns[1], kei_columns, remaining_columns[-1])

# 데이터프레임의 열 순서를 재정렬
data_2000_filtered <- data_2000_filtered[, new_column_order]

# 결과 확인
print(names(data_2000_filtered))




### 🟩 데이터 저장  ======================================================
path_save = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID/@_조림/(@완료)수종별 조림실적Plantation forest by tree species/@완료/3.2000년도_수제작"
write.xlsx(data_2000_filtered, file.path(path_save, "2000_Combined.xlsx"))











