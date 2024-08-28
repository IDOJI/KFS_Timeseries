# 🟥 데이터 로드 =============================================================================================
path_data = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/조림/수종별 조림실적Plantation forest by tree species/1.68~80"
data.list = lapply(list.files(path_data, full.names=T), read.csv) %>% 
  setNames(tools::file_path_sans_ext(list.files(path_data)))

data.list[[1]] %>% View
names(data.list)

sapply(data.list, function(x){
  x$year %>% unique
})




# 🟥 NA 열이름 처리 ===========================================================================================
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
names(data.list)

# test = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID/@_조림/_(@완료)수종별 조림실적Plantation forest by tree species/@완료/1.Total 면적만 존재.xlsx"
# test = read.xlsx(test)
# View(test)




# 🟥 열이름 체크 ===================================================================================
sapply(data.list, names)





# 🟫 연도 행만 추출  ======================================================
data.list = lapply(data.list, function(data_df){
  # data_df의 두 번째 열 추출
  second_column <- data_df[[3]]
  
  # 숫자 4자리로 시작하거나 끝나는 문자열을 찾기 위한 정규 표현식
  is_year <- grepl("^\\d{4}|\\d{4}$", second_column)
  
  # 연도에 해당하는 행만 추출
  year_rows <- data_df[is_year, ]

  year_rows  
})

data.list[[1]] %>% View



# 🟦 합계 수량 체크 ===================================================================================
## 🟨 수량에 대한 합계인가 체크 ================================================================
test = data.list[[1]]
# View(test)
values <- c(58808, 22486, 3336, 3358, 34351, 22896, 47386, 2098, 20043, 5596, 
            4201, NA, 7646, 286, 538, 273, NA, 62, 94, 23, NA, NA, NA, NA, 
            NA, 32, 6, 11307)
sum(values, na.rm = T)



library(dplyr)

extract_summed_results(data.list) %>% View





## 🟨 차이 많이 나는 연보 체크 ================================================================
### 🟧 1969_YRBK_00020309 =========================================================================
id = "1969_YRBK_00020309"
ind = which(names(data.list) == id)
selected_data = data.list[[ind]]
View(selected_data)
names(selected_data)

# 계 수량
col_name = "계_수량"
selected_data[[col_name]]
selected_data[[col_name]] = c(236861, 166383, 418555, 91614, 368390, 294389, 288085, 591032, 452442)

# 리기다송
col_name = "수량_리기다송"
selected_data[[col_name]]
selected_data[[col_name]] = c(58723, 66618, 96968, 18617, 44620, 60898, 35247, 623058, 215016)

# 낙엽송
col_name = "수량_낙엽송"
selected_data[[col_name]]
selected_data[[col_name]] = c(41964, 35514, 27668, 18990, 40113, 29717, 44425, 53461, 35362)

# 잣나무
col_name = "수량_잣나무"
selected_data[[col_name]]
selected_data[[col_name]] = c(1167, 3312, 2014, 2247, 2869, 3079, 2988, 6706, 6493)
names(selected_data)
grep("테다", names(selected_data), value=T)
data.list[[ind+4]] %>% names

# 해송
col_name = "수량_해송"
selected_data[[col_name]]
selected_data[[col_name]] = c(19470, 24476, 13881, 14822, 24247, 12195, 4648, 5979, 12489)


# 삼나무
col_name = "수량_삼나무"
selected_data[[col_name]]
selected_data[[col_name]] = c(5395, 2004, 3498, 959, 6242, 8452, 9607, 13148, 4969)


# 편백
col_name = "수량_편백"
selected_data[[col_name]]
selected_data[[col_name]] = c(4121, 152, NA, 1196, 2474, 1800, 10100, 4890, 12180)


# 테다
col_name = "수량_테다"
selected_data[[col_name]]
selected_data[[col_name]] = c(NA, NA, NA, NA, NA, NA, NA, NA, 7744)

# 개량포푸라
names(selected_data)
col_name = "수량_개량포푸라"
selected_data[[col_name]]
selected_data[[col_name]][length(selected_data[[col_name]])] = 4956

# 밤나무
names(selected_data)
col_name = "수량_밤나무"
selected_data[[col_name]]
selected_data[[col_name]][selected_data[[col_name]] == 10] = 1585


# 소나무
names(selected_data)
col_name = "수량_소나무"
selected_data[[col_name]]
selected_data[[col_name]] = c(NA, NA, NA, NA, NA, NA, 140, NA, NA)



# 리기테다
names(selected_data)
col_name = "수량_리기테다"
selected_data[[col_name]]
selected_data[[col_name]] = c(NA, NA, NA, 40, NA, 350, 437, 2574, 2760)


# 굴참나무
names(selected_data)
col_name = "수량_굴참나무"
selected_data[[col_name]] = NULL



# 가래나무
names(selected_data)
col_name = "수량_가래나무"
selected_data[[col_name]] = NULL


# 기타
names(selected_data)
col_name = "수량_기타"
selected_data[[col_name]] 
selected_data[[col_name]] = c(10672, 1387, 724, 705, 66, 397, 740, 11, 644)

# replace
data.list[[ind]] = selected_data
test = extract_summed_results(data.list)
View(test)
selected_data %>% select(starts_with("수량_"))





### 🟧 1972_YRBK_00050309 =========================================================================
id = "1972_YRBK_00050309"
ind = which(names(data.list) == id)
selected_data = data.list[[ind]]

col_name = "수량_리기다송"
selected_data[[col_name]][which(selected_data[[col_name]] == 686902)] = 68690.2
selected_data %>% View
data.list[[ind]] = selected_data
test = extract_summed_results(data.list)



### 🟧 1974_YRBK_00070309 =========================================================================
id = "1974_YRBK_00070309"
ind = which(names(data.list) == id)
selected_data = data.list[[ind]]

col_name = "수량_리기다송"
selected_data[[col_name]][which(selected_data[[col_name]] == 686902)]
selected_data[[col_name]][which(selected_data[[col_name]] == 686902)] = 68690.2
selected_data %>% View
data.list[[ind]] = selected_data
test = extract_summed_results(data.list)
View(test)




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
sapply(data.list_2, function(x){
  names(x)[4]
}) %>% unname

data.list_3 = lapply(data.list_2, function(x){
  names(x)[4] = "합계_면적"
  x
})
names(data.list_3 )
sapply(data.list_2, names)
sapply(data.list_3, names)



# 🟥 데이터 합치기 ===================================================================================
data.list_3[[1]] %>% names
year_rows = bind_rows(data.list_3) %>% 
  relocate(starts_with("수량_"), .after=3) %>% 
  relocate(starts_with("계_"), .after=3) %>%
  relocate(year, .after = "구분") %>% 
  select(-합계_면적) %>% 
  rename(합계_수량 = 계_수량) %>% 
  mutate(., 합계_수량_New = rowSums(select(., starts_with("수량_")), na.rm = T)) %>% 
  relocate("합계_수량_New", .after = 합계_수량) %>% 
  mutate(Diff = abs(합계_수량_New - 합계_수량) %>% round) %>% 
  relocate(Diff, .after = 합계_수량_New)
names(year_rows)
View(year_rows)





# 🟨 최신연도만 남기기 =============================================================================
result <- year_rows %>%
  arrange(year, 구분) %>% 
  group_by(구분) %>%            # "구분" 열을 기준으로 그룹화
  slice_max(order_by = year, n = 1) # 각 그룹 내에서 최신 연도 선택
View(result)

# 결과 체크
result[[3]] %>% table
result %>% filter(구분=="1960") %>% pull(year)
year_rows %>% filter(구분 == "1960") %>% pull(year)

# 모든 연도가 continuous하게 있는가
is_consecutive(result[[3]])



# 🟨 Diff가 0이 아닌 데이터 처리 =============================================================================
names(result)
result %>% filter(Diff != 0) %>% pull(구분)

# 1966년 확인
year_rows %>% filter(구분 == "1966") %>% View
result[result[[3]] == "1966",] = year_rows %>% filter(구분 == "1966") %>% slice(10)
View(result)


# 🟩 열 이름 변경  ======================================================
zero_df = result
names(zero_df)
# 예시 데이터프레임의 열 이름
column_names <- names(zero_df)

# "수량_"로 시작하는 열 이름을 "_본수"로 변경
new_column_names <- sapply(column_names, function(name) {
  if (grepl("^수량_", name)) {
    gsub("^수량_", "", name)
  } else {
    name
  }
})

# "_본수"를 모든 이름의 끝에 추가
new_column_names <- sapply(new_column_names, function(name) {
  if (!grepl("본수$", name) & !name %in% column_names[!grepl("^수량_", column_names)]) {
    paste0(name, "_본수")
  } else {
    name
  }
})

# 변경된 열 이름을 데이터프레임에 적용
names(zero_df) <- new_column_names

# 결과 확인
print(names(zero_df))
View(zero_df)


# 침엽수 및 활엽수 나무 이름 목록
coniferous_trees <- c("리기다송_본수", "낙엽송_본수", "잣나무_본수", "해송_본수","은행_본수",
                      "삼나무_본수", "편백_본수", "소나무_본수", "리기테다_본수", "테다_본수")

leafy_trees <- c("산오리_본수", "사방오리_본수", "물갬나무_본수", "아까시아_본수", 
                 "상수리_본수", "개량포푸라_본수", "밤나무_본수", "감나무_본수", 
                 "고염나무_본수", "호도나무_본수", "대추나무_본수", "옻나무_본수", 
                 "오동_본수", "유동_본수", "삼지목_본수", "대나무_본수", 
                 "굴참나무_본수", "가래나무_본수", "은사시_본수", "개량포플러_본수")

# 열 이름 변경
new_column_names <- sapply(names(zero_df), function(name) {
  if (name %in% coniferous_trees) {
    paste0("침엽수_", name)
  } else if (name %in% leafy_trees) {
    paste0("활엽수_", name)
  } else {
    name
  }
})

# 데이터프레임에 새로운 열 이름 적용
names(zero_df) <- new_column_names

# 결과 확인
print(names(zero_df))

View(zero_df)

# 🟥 연도 행만 추출  ======================================================
path_save = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/조림/수종별 조림실적Plantation forest by tree species/Combined"
write.xlsx(zero_df, file.path(path_save, "1.Combined_68~80.xlsx"))
# year_rows$year





