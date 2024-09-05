# 🟥 데이터로드 ======================================================================================
data_1 = read.xlsx("/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/조림/수종별 조림실적Plantation forest by tree species/Combined/1.Combined_68~80.xlsx")
data_2 = read.xlsx("/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/조림/수종별 조림실적Plantation forest by tree species/Combined/2.Combined_82~99_국유림민유림.xlsx")
data_3 = read.xlsx("/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/조림/수종별 조림실적Plantation forest by tree species/Combined/3.Combined_00_직접입력.xlsx")
data_4 = read.xlsx("/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/조림/수종별 조림실적Plantation forest by tree species/Combined/4.Combined_04~22.xlsx")






# 🟥 열 선택======================================================================================
exclude_vector <- c("Categorized_L3_New", "Categorized_L3", "Categorized_L2", 
                    "NAME_L1", "NAME_L2", "NAME_L3", "NAME_L4", "NAME_L5", 
                    "ID_L1", "ID_L2", "ID_L3", "ID_L4", "ID_L5", 
                    "unit_L2", "unit_L3", "unit_L4", "unit_L5", 
                    "비고_L2", "비고_L3", "비고_L4", "비고_L5")
data_1_sub = data_1 %>% select(-all_of(exclude_vector)) %>% 
  relocate(year, .after = 3) %>% 
  select(-`합계_면적`)
data_2_sub = data_2 %>% 
  select(-all_of(exclude_vector)) %>% relocate(year, .after = 3)
data_3_sub = data_3 %>% 
  relocate(year, .after = 3)
data_4_sub = data_4 %>% 
  select(-all_of(exclude_vector)) %>% relocate(year, .after = 3)






# 🟦 열이름확인 ======================================================================================
names(data_1_sub)
names(data_2_sub)
names(data_3_sub)
names(data_4_sub)

names(data_1_sub)[!names(data_4_sub) %in% names(data_1_sub)]





# 🟨 기타 : 활엽수 침엽수 비율 추정 ======================================================================================
conifer_sum = data_4_sub$침엽수_기타_본수 %>% sum
broad_sum = data_4_sub$활엽수_기타_본수 %>% sum
conifer_prop = (conifer_sum) / sum(conifer_sum, broad_sum)


data_1_sub_2 = data_1_sub %>% 
  mutate(활엽수_기타_본수 = 기타_본수 * conifer_prop) %>% 
  mutate(침엽수_기타_본수 = 기타_본수 * (1 - conifer_prop)) %>% 
  select(-기타_본수) %>% 
  relocate(활엽수_기타_본수, 침엽수_기타_본수, .after = 6)

data_2_sub_2 = data_2_sub %>% 
  mutate(활엽수_기타_본수 = 기타_본수 * conifer_prop) %>% 
  mutate(침엽수_기타_본수 = 기타_본수 * (1 - conifer_prop)) %>% 
  select(-기타_본수) %>% 
  relocate(활엽수_기타_본수, 침엽수_기타_본수, .after = 4)




# 🟪 합계 실제계산 결과랑 비교 ======================================================================================
names(data_1_sub_2)
names(data_2_sub_2)
names(data_3_sub)
names(data_4_sub)



data.list = list()

# 열 이름을 벡터로 생성
selected_columns <- c("classification", "year", "total_seedling", 
                      "total_seedling_new", "abs_diff", 
                      "conifer_total_seedling", "broad_total_seedling",
                      "conifer_total_seedling_new", "broad_total_seedling_new")




## 🟨 data_1 ====================================================================================
data_1_sub_2 %>% names
data_1_sub_2$계_수량
# "침엽수_"로 시작하는 열 이름 선택
coniferous_columns <- grep("^침엽수_", names(data_1_sub_2), value = TRUE)

# "활엽수_"로 시작하는 열 이름 선택
leafy_columns <- grep("^활엽수_", names(data_1_sub_2), value = TRUE)

# 각 행의 "침엽수_" 열들의 합계 계산 (NA 제외)
data_1_sub_2$conifer_total_seedling = NA
data_1_sub_2$conifer_total_seedling_new <- rowSums(data_1_sub_2[ , coniferous_columns], na.rm = TRUE)

# 각 행의 "활엽수_" 열들의 합계 계산 (NA 제외)
data_1_sub_2$broad_total_seedling = NA
data_1_sub_2$broad_total_seedling_new <- rowSums(data_1_sub_2[ , leafy_columns], na.rm = TRUE)

# 결과 확인
head(data_1_sub_2)
data_1_sub_3 = data_1_sub_2 %>% 
  rename(total_seedling = 계_수량) %>% 
  relocate(conifer_total_seedling, broad_total_seedling, .after = total_seedling) %>% 
  rename(classification = 구분) %>% 
  mutate(., total_seedling_new = select(., c("conifer_total_seedling_new", "broad_total_seedling_new")) %>%  rowSums(na.rm = T)) %>% 
  mutate(abs_diff = abs(total_seedling_new - total_seedling)) %>% 
  relocate(total_seedling_new, abs_diff, .after = total_seedling)
names(data_1_sub_3)

data.list[[1]] = data_1_sub_3 %>% 
  select(all_of(selected_columns))


## 🟨 data_2 ====================================================================================
names(data_2_sub_2)
data_2_sub_3 = data_2_sub_2 %>% 
  rename(total_seedling = "본수_총합") %>% 
  rename(classification = "구분") %>% 
  mutate(conifer_total_seedling = NA) %>% 
  mutate(broad_total_seedling = NA) %>% 
  mutate(conifer_total_seedling_new = select(., all_of(grep("^침엽수", names(.), value = TRUE))) %>% rowSums(na.rm = TRUE)) %>% 
  mutate(broad_total_seedling_new = select(., all_of(grep("^활엽수", names(.), value = TRUE))) %>% rowSums(na.rm = TRUE)) %>% 
  relocate(conifer_total_seedling_new, broad_total_seedling_new, .after = 4) %>% 
  mutate(total_seedling_new = (conifer_total_seedling_new + broad_total_seedling_new)) %>% 
  mutate(abs_diff = abs(total_seedling_new - total_seedling)) %>% 
  relocate(total_seedling_new, abs_diff, .after = total_seedling)
View(data_2_sub_3)

data.list[[2]] = data_2_sub_3 %>% 
  select(all_of(selected_columns))



## 🟨 data_3 ====================================================================================
data_3_sub %>% names
data_3_sub_2 = data_3_sub %>% 
  rename(classification = "구분") %>%
  rename(conifer_total_seedling = 침엽수_합계_본수) %>%
  rename(broad_total_seedling = 활엽수_합계_본수) %>% 
  mutate(total_seedling = conifer_total_seedling + broad_total_seedling) %>%
  relocate(total_seedling, .after = year) %>% 
  relocate(conifer_total_seedling, broad_total_seedling, .after = year) %>% 
  mutate(conifer_total_seedling_new = select(., all_of(grep("^침엽수", names(.), value = TRUE))) %>% rowSums(na.rm = TRUE)) %>% 
  mutate(broad_total_seedling_new = select(., all_of(grep("^활엽수", names(.), value = TRUE))) %>% rowSums(na.rm = TRUE)) %>% 
  relocate(conifer_total_seedling_new, broad_total_seedling_new, .after = broad_total_seedling) %>% 
  mutate(total_seedling_new = (conifer_total_seedling_new + broad_total_seedling_new)) %>% 
  mutate(abs_diff = abs(total_seedling_new - total_seedling)) %>% 
  relocate(total_seedling_new, abs_diff, .after = total_seedling)
# View(data_3_sub_2 )

data.list[[3]] = data_3_sub_2 %>% 
  select(all_of(selected_columns))




## 🟨 data_4 ====================================================================================
data_4_sub %>% names
data_4_sub_2 = data_4_sub %>% 
  rename(classification = "구분") %>%
  rename(conifer_total_seedling = 침엽수_합계_본수) %>%
  rename(broad_total_seedling = 활엽수_합계_본수) %>% 
  mutate(total_seedling = conifer_total_seedling + broad_total_seedling) %>%
  relocate(total_seedling, .after = year) %>% 
  relocate(conifer_total_seedling, broad_total_seedling, .after = year) %>% 
  mutate(conifer_total_seedling_new = select(., all_of(grep("^침엽수", names(.), value = TRUE))) %>% rowSums(na.rm = TRUE)) %>% 
  mutate(broad_total_seedling_new = select(., all_of(grep("^활엽수", names(.), value = TRUE))) %>% rowSums(na.rm = TRUE)) %>% 
  relocate(conifer_total_seedling_new, broad_total_seedling_new, .after = broad_total_seedling) %>% 
  mutate(total_seedling_new = (conifer_total_seedling_new + broad_total_seedling_new)) %>% 
  mutate(abs_diff = abs(total_seedling_new - total_seedling)) %>% 
  relocate(total_seedling_new, abs_diff, .after = total_seedling)

# View(data_4_sub_2)

data.list[[4]] = data_4_sub_2 %>% 
  select(all_of(selected_columns))





# 🟥 데이터 합치기 ======================================================================================
data_combined = bind_rows(data.list)
View(data_combined)


# 🟪 열선택  ======================================================================================
data_combined_sub = data_combined %>% select(classification, 
                                             year, 
                                             total_seedling_new, 
                                             conifer_total_seedling_new,
                                             broad_total_seedling_new)
View(data_combined_sub)

# 🟪 내보내기 ======================================================================================
path_save = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/조림/수종별 조림실적Plantation forest by tree species/Combined"
file_name = "5.Combined_final.xlsx"
write.xlsx(data_combined_sub , file.path(path_save, file_name))








# 🍀 연보랑 비교new ======================================================================================
data_combined_2 = data_combined %>% 
  rename(class = classification)


## 🟥 2016 ===================================================
class_year = "2016"
sub = data_combined_2 %>% filter(class == class_year)
sub$year
keywords = list()
keywords[[1]] = c("소나무", "_잣나무", "낙엽송")
extract_columns_by_keywords(sub, keywords[[1]]) %>% View
keywords[[2]] = c("삼나무", "편백", "해송", "침엽수_기타")
extract_columns_by_keywords(sub, keywords[[2]]) %>% View
keywords[[3]] = c("고로쇠", "느티", "물푸레")
extract_columns_by_keywords(sub, keywords[[3]]) %>% View
keywords[[4]] = c("벚", "상수리", "자작", "활엽수_기타")
extract_columns_by_keywords(sub, keywords[[4]]) %>% View
is_na = extract_columns_by_include_exclude_keywords(sub, c(unlist(keywords), "합계"), "본수") %>% unlist
is.na(is_na) %>% sum == length(is_na)






# 🟪 연보랑 비교 ======================================================================================
data_combined_final_new_2 = data_combined_final_new %>% rename(class = 구분)

## 🟨 1968 ===================================================
class_year = "1968"
sub = data_combined_final_new_2 %>% filter(class == class_year)
sub$year
keywords = list()
keywords[[1]] = c("리기다송", "산오리", "사방오리", "물갬")
extract_columns_by_keywords(sub, keywords[[1]]) %>% View
keywords[[2]] = c("아까시아", "상수리", "낙엽송", "_잣나무", "해송", "삼나무", "편백", "개량", "밤나무")
extract_columns_by_keywords(sub, keywords[[2]]) %>% View
keywords[[3]] = c("감나무", "고염나무", "호도나무", "대추", "옻나무", "은행")
extract_columns_by_keywords(sub, keywords[[3]]) %>% View
keywords[[4]] = c("오동", "유동", "삼지목", "소나무", "리기테다", "침엽수_테다", "대나무", "굴참나무", "가래", "기타_본수")
extract_columns_by_keywords(sub, keywords[[4]]) %>% View
is_na = extract_columns_by_include_exclude_keywords(sub, c(unlist(keywords), "합계"), "본수") %>% unlist
is.na(is_na) %>% sum == length(is_na)





## 🟨 1969 ===================================================
class_year = "1969"
sub = data_combined_final_new_2 %>% filter(class == class_year)
sub$year
keywords = list()
keywords[[1]] = c("리기다송", "산오리", "사방오리", "물갬")
extract_columns_by_keywords(sub, keywords[[1]]) %>% View
keywords[[2]] = c("아까시아", "상수리", "낙엽송", "_잣나무", "해송", "삼나무", "편백", "개량", "밤나무")
extract_columns_by_keywords(sub, keywords[[2]]) %>% View
keywords[[3]] = c("감나무", "고염나무", "호도나무", "대추", "옻나무", "은행")
extract_columns_by_keywords(sub, keywords[[3]]) %>% View
keywords[[4]] = c("오동", "유동", "삼지목", "소나무", "리기테다", "침엽수_테다", "대나무", "굴참나무", "가래", "기타_본수")
extract_columns_by_keywords(sub, keywords[[4]]) %>% View
is_na = extract_columns_by_include_exclude_keywords(sub, c(unlist(keywords), "합계"), "본수") %>% unlist
is.na(is_na) %>% sum == length(is_na)




## 🟨 1970 ===================================================
class_year = "1970"
sub = data_combined_final_new_2 %>% filter(class == class_year)
sub$year
keywords = list()
keywords[[1]] = c("리기다송", "산오리", "사방오리", "물갬")
extract_columns_by_keywords(sub, keywords[[1]]) %>% View
keywords[[2]] = c("아까시아", "상수리", "낙엽송", "_잣나무", "해송", "삼나무", "편백", "개량", "밤나무")
extract_columns_by_keywords(sub, keywords[[2]]) %>% View
keywords[[3]] = c("감나무", "고염나무", "호도나무", "대추", "옻나무", "은행")
extract_columns_by_keywords(sub, keywords[[3]]) %>% View
keywords[[4]] = c("오동", "유동", "삼지목", "소나무", "리기테다", "침엽수_테다", "대나무", "굴참나무", "가래", "기타_본수")
extract_columns_by_keywords(sub, keywords[[4]]) %>% View
is_na = extract_columns_by_include_exclude_keywords(sub, c(unlist(keywords), "합계"), "본수") %>% unlist
is.na(is_na) %>% sum == length(is_na)



## 🟨 1971 ===================================================
class_year = "1971"
sub = data_combined_final_new_2 %>% filter(class == class_year)
sub$year
keywords = list()
keywords[[1]] = c("리기다송", "산오리", "사방오리", "물갬")
extract_columns_by_keywords(sub, keywords[[1]]) %>% View
keywords[[2]] = c("아까시아", "상수리", "낙엽송", "_잣나무", "해송", "삼나무", "편백", "개량", "밤나무")
extract_columns_by_keywords(sub, keywords[[2]]) %>% View
keywords[[3]] = c("감나무", "고염나무", "호도나무", "대추", "옻나무", "은행")
extract_columns_by_keywords(sub, keywords[[3]]) %>% View
keywords[[4]] = c("오동", "유동", "삼지목", "소나무", "리기테다", "침엽수_테다", "대나무", "굴참나무", "가래", "기타_본수")
extract_columns_by_keywords(sub, keywords[[4]]) %>% View
is_na = extract_columns_by_include_exclude_keywords(sub, c(unlist(keywords), "합계"), "본수") %>% unlist
is.na(is_na) %>% sum == length(is_na)




## 🟨 1972 ===================================================
class_year = "1972"
sub = data_combined_final_new_2 %>% filter(class == class_year)
sub$year
keywords = list()
keywords[[1]] = c("리기다송", "산오리", "사방오리", "물갬")
extract_columns_by_keywords(sub, keywords[[1]]) %>% View
keywords[[2]] = c("아까시아", "상수리", "낙엽송", "_잣나무", "해송", "삼나무", "편백", "개량", "밤나무")
extract_columns_by_keywords(sub, keywords[[2]]) %>% View
keywords[[3]] = c("감나무", "고염나무", "호도나무", "대추", "옻나무", "은행")
extract_columns_by_keywords(sub, keywords[[3]]) %>% View
keywords[[4]] = c("오동", "유동", "삼지목", "소나무", "리기테다", "침엽수_테다", "대나무", "굴참나무", "가래", "기타_본수")
extract_columns_by_keywords(sub, keywords[[4]]) %>% View
is_na = extract_columns_by_include_exclude_keywords(sub, c(unlist(keywords), "합계"), "본수") %>% unlist
is.na(is_na) %>% sum == length(is_na)




## 🟨 1973 ===================================================
class_year = "1973"
sub = data_combined_final_new_2 %>% filter(class == class_year)
sub$year
keywords = list()
keywords[[1]] = c("리기다송", "산오리", "사방오리", "물갬")
extract_columns_by_keywords(sub, keywords[[1]]) %>% View
keywords[[2]] = c("아까시아", "상수리", "낙엽송", "_잣나무", "해송", "삼나무", "편백", "개량", "밤나무")
extract_columns_by_keywords(sub, keywords[[2]]) %>% View
keywords[[3]] = c("감나무", "고염나무", "호도나무", "대추", "옻나무", "은행")
extract_columns_by_keywords(sub, keywords[[3]]) %>% View
keywords[[4]] = c("오동", "유동", "삼지목", "소나무", "리기테다", "침엽수_테다", "대나무", "굴참나무", "가래", "기타_본수")
extract_columns_by_keywords(sub, keywords[[4]]) %>% View
is_na = extract_columns_by_include_exclude_keywords(sub, c(unlist(keywords), "합계"), "본수") %>% unlist
is.na(is_na) %>% sum == length(is_na)





## 🟨 1974 ===================================================
class_year = "1974"
sub = data_combined_final_new_2 %>% filter(class == class_year)
sub$year
keywords = list()
keywords[[1]] = c("리기다송", "산오리", "사방오리", "물갬")
extract_columns_by_keywords(sub, keywords[[1]]) %>% View
keywords[[2]] = c("아까시아", "상수리", "낙엽송", "_잣나무", "해송", "삼나무", "편백", "개량", "밤나무")
extract_columns_by_keywords(sub, keywords[[2]]) %>% View
keywords[[3]] = c("감나무", "고염나무", "호도나무", "대추", "옻나무", "은행")
extract_columns_by_keywords(sub, keywords[[3]]) %>% View
keywords[[4]] = c("오동", "유동", "삼지목", "소나무", "리기테다", "침엽수_테다", "대나무", "굴참나무", "가래", "기타_본수")
extract_columns_by_keywords(sub, keywords[[4]]) %>% View
is_na = extract_columns_by_include_exclude_keywords(sub, c(unlist(keywords), "합계"), "본수") %>% unlist
is.na(is_na) %>% sum == length(is_na)





## 🟨 1975 ===================================================
class_year = "1975"
sub = data_combined_final_new_2 %>% filter(class == class_year)
sub$year
keywords = list()
keywords[[1]] = c("리기다송", "산오리", "사방오리", "물갬")
extract_columns_by_keywords(sub, keywords[[1]]) %>% View
keywords[[2]] = c("아까시아", "상수리", "낙엽송", "_잣나무", "해송", "삼나무", "편백", "개량", "밤나무")
extract_columns_by_keywords(sub, keywords[[2]]) %>% View
keywords[[3]] = c("감나무", "고염나무", "호도나무", "대추", "옻나무", "은행")
extract_columns_by_keywords(sub, keywords[[3]]) %>% View
keywords[[4]] = c("오동", "유동", "삼지목", "소나무", "리기테다", "침엽수_테다", "대나무", "굴참나무", "가래", "기타_본수")
extract_columns_by_keywords(sub, keywords[[4]]) %>% View
is_na = extract_columns_by_include_exclude_keywords(sub, c(unlist(keywords), "합계"), "본수") %>% unlist
is.na(is_na) %>% sum == length(is_na)




## 🟨 1976 ===================================================
class_year = "1976"
sub = data_combined_final_new_2 %>% filter(class == class_year)
sub$year
keywords = list()
keywords[[1]] = c("리기다송", "산오리", "사방오리", "물갬")
extract_columns_by_keywords(sub, keywords[[1]]) %>% View
keywords[[2]] = c("아까시아", "상수리", "낙엽송", "_잣나무", "해송", "삼나무", "편백", "개량", "밤나무")
extract_columns_by_keywords(sub, keywords[[2]]) %>% View
keywords[[3]] = c("감나무", "고염나무", "호도나무", "대추", "옻나무", "은행")
extract_columns_by_keywords(sub, keywords[[3]]) %>% View
keywords[[4]] = c("오동", "유동", "삼지목", "소나무", "리기테다", "침엽수_테다", "대나무", "굴참나무", "가래", "기타_본수")
extract_columns_by_keywords(sub, keywords[[4]]) %>% View
is_na = extract_columns_by_include_exclude_keywords(sub, c(unlist(keywords), "합계"), "본수") %>% unlist
is.na(is_na) %>% sum == length(is_na)





## 🟨 1977 ===================================================
class_year = "1977"
sub = data_combined_final_new_2 %>% filter(class == class_year)
sub$year
keywords = list()
keywords[[1]] = c("리기다송", "산오리", "사방오리", "물갬")
extract_columns_by_keywords(sub, keywords[[1]]) %>% View
keywords[[2]] = c("아까시아", "상수리", "낙엽송", "_잣나무", "해송", "삼나무", "편백", "개량", "밤나무")
extract_columns_by_keywords(sub, keywords[[2]]) %>% View
keywords[[3]] = c("감나무", "고염나무", "호도나무", "대추", "옻나무", "은행")
extract_columns_by_keywords(sub, keywords[[3]]) %>% View
keywords[[4]] = c("오동", "유동", "삼지목", "소나무", "리기테다", "침엽수_테다", "대나무", "굴참나무", "가래", "기타_본수")
extract_columns_by_keywords(sub, keywords[[4]]) %>% View
is_na = extract_columns_by_include_exclude_keywords(sub, c(unlist(keywords), "합계"), "본수") %>% unlist
is.na(is_na) %>% sum == length(is_na)





## 🟨 1978 ===================================================
class_year = "1978"
sub = data_combined_final_new_2 %>% filter(class == class_year)
sub$year
keywords = list()
keywords[[1]] = c("리기다송", "산오리", "사방오리", "물갬")
extract_columns_by_keywords(sub, keywords[[1]]) %>% View
keywords[[2]] = c("아까시아", "상수리", "낙엽송", "_잣나무", "해송", "삼나무", "편백", "개량", "밤나무")
extract_columns_by_keywords(sub, keywords[[2]]) %>% View
keywords[[3]] = c("감나무", "고염나무", "호도나무", "대추", "옻나무", "은행")
extract_columns_by_keywords(sub, keywords[[3]]) %>% View
keywords[[4]] = c("오동", "유동", "삼지목", "소나무", "리기테다", "침엽수_테다", "대나무", "굴참나무", "가래", "기타_본수")
extract_columns_by_keywords(sub, keywords[[4]]) %>% View
is_na = extract_columns_by_include_exclude_keywords(sub, c(unlist(keywords), "합계"), "본수") %>% unlist
is.na(is_na) %>% sum == length(is_na)



## 🟥 1979 ===================================================
class_year = "1979"
sub = data_combined_final_new_2 %>% filter(class == class_year)
sub$year
keywords = list()
keywords[[1]] = c("잣나무")
extract_columns_by_keywords(sub, keywords[[1]]) %>% View
keywords[[2]] = c("낙엽송", "삼나무", "편백")
extract_columns_by_keywords(sub, keywords[[2]]) %>% View
keywords[[3]] = c("리기다_본수", "침엽수_테다")
extract_columns_by_keywords(sub, keywords[[3]]) %>% View
keywords[[4]] = c("리기테다", "강송", "해송")
extract_columns_by_keywords(sub, keywords[[4]]) %>% View
keywords[[5]] = c("밤나무", "이태리")
extract_columns_by_keywords(sub, keywords[[5]]) %>% View
keywords[[6]] = c("현사시", "오동", "기타_본수")
extract_columns_by_keywords(sub, keywords[[6]]) %>% View
is_na = extract_columns_by_include_exclude_keywords(sub, c(unlist(keywords), "합계"), "본수") %>% unlist
is.na(is_na) %>% sum == length(is_na)




## 🟥 1980 ===================================================
class_year = "1980"
sub = data_combined_final_new_2 %>% filter(class == class_year)
sub$year
keywords = list()
keywords[[1]] = c("잣나무")
extract_columns_by_keywords(sub, keywords[[1]]) %>% View
keywords[[2]] = c("낙엽송", "삼나무", "편백")
extract_columns_by_keywords(sub, keywords[[2]]) %>% View
keywords[[3]] = c("리기다_본수", "침엽수_테다")
extract_columns_by_keywords(sub, keywords[[3]]) %>% View
keywords[[4]] = c("리기테다", "강송", "해송")
extract_columns_by_keywords(sub, keywords[[4]]) %>% View
keywords[[5]] = c("밤나무", "이태리")
extract_columns_by_keywords(sub, keywords[[5]]) %>% View
keywords[[6]] = c("현사시", "오동", "기타_본수")
extract_columns_by_keywords(sub, keywords[[6]]) %>% View
is_na = extract_columns_by_include_exclude_keywords(sub, c(unlist(keywords), "합계"), "본수") %>% unlist
is.na(is_na) %>% sum == length(is_na)




## 🟩 1981 ===================================================
class_year = "1981"
sub = data_combined_final_new_2 %>% filter(class == class_year)
sub$year
keywords = list()
keywords[[1]] = c("잣나무")
extract_columns_by_keywords(sub, keywords[[1]]) %>% View
keywords[[2]] = c("낙엽송", "삼나무", "편백")
extract_columns_by_keywords(sub, keywords[[2]]) %>% View
keywords[[3]] = c("리기다_본수", "침엽수_테다")
extract_columns_by_keywords(sub, keywords[[3]]) %>% View
keywords[[4]] = c("리기테다", "강송", "해송")
extract_columns_by_keywords(sub, keywords[[4]]) %>% View
keywords[[5]] = c("밤나무", "이태리")
extract_columns_by_keywords(sub, keywords[[5]]) %>% View
keywords[[6]] = c("현사시", "오동", "기타_본수")
extract_columns_by_keywords(sub, keywords[[6]]) %>% View
is_na = extract_columns_by_include_exclude_keywords(sub, c(unlist(keywords), "합계"), "본수") %>% unlist
is.na(is_na) %>% sum == length(is_na)


## 🟥 1982 ===================================================
class_year = "1982"
sub = data_combined_final_new_2 %>% filter(class == class_year)
sub$year
keywords = list()
keywords[[1]] = c("잣나무")
extract_columns_by_keywords(sub, keywords[[1]]) %>% View
keywords[[2]] = c("낙엽송", "삼나무", "편백")
extract_columns_by_keywords(sub, keywords[[2]]) %>% View
keywords[[3]] = c("리기다_본수", "침엽수_테다")
extract_columns_by_keywords(sub, keywords[[3]]) %>% View
keywords[[4]] = c("리기테다", "강송", "해송")
extract_columns_by_keywords(sub, keywords[[4]]) %>% View
keywords[[5]] = c("밤나무", "이태리")
extract_columns_by_keywords(sub, keywords[[5]]) %>% View
keywords[[6]] = c("현사시", "오동", "기타_본수")
extract_columns_by_keywords(sub, keywords[[6]]) %>% View
is_na = extract_columns_by_include_exclude_keywords(sub, c(unlist(keywords), "합계"), "본수") %>% unlist
is.na(is_na) %>% sum == length(is_na)





## 🟥 1983 ===================================================
class_year = "1983"
sub = data_combined_final_new_2 %>% filter(class == class_year)
sub$year
keywords = list()
keywords[[1]] = c("잣나무")
extract_columns_by_keywords(sub, keywords[[1]]) %>% View
keywords[[2]] = c("낙엽송", "삼나무", "편백")
extract_columns_by_keywords(sub, keywords[[2]]) %>% View
keywords[[3]] = c("리기다_본수", "침엽수_테다")
extract_columns_by_keywords(sub, keywords[[3]]) %>% View
keywords[[4]] = c("리기테다", "강송", "해송")
extract_columns_by_keywords(sub, keywords[[4]]) %>% View
keywords[[5]] = c("밤나무", "이태리")
extract_columns_by_keywords(sub, keywords[[5]]) %>% View
keywords[[6]] = c("현사시", "오동", "기타_본수")
extract_columns_by_keywords(sub, keywords[[6]]) %>% View
is_na = extract_columns_by_include_exclude_keywords(sub, c(unlist(keywords), "합계"), "본수") %>% unlist
is.na(is_na) %>% sum == length(is_na)








## 🟥 1984 ===================================================
class_year = "1984"
sub = data_combined_final_new_2 %>% filter(class == class_year)
sub$year
keywords = list()
keywords[[1]] = c("잣나무")
extract_columns_by_keywords(sub, keywords[[1]]) %>% View
keywords[[2]] = c("낙엽송", "삼나무", "편백")
extract_columns_by_keywords(sub, keywords[[2]]) %>% View
keywords[[3]] = c("리기다_본수", "침엽수_테다")
extract_columns_by_keywords(sub, keywords[[3]]) %>% View
keywords[[4]] = c("리기테다", "강송", "해송")
extract_columns_by_keywords(sub, keywords[[4]]) %>% View
keywords[[5]] = c("밤나무", "이태리")
extract_columns_by_keywords(sub, keywords[[5]]) %>% View
keywords[[6]] = c("현사시", "오동", "기타_본수")
extract_columns_by_keywords(sub, keywords[[6]]) %>% View
is_na = extract_columns_by_include_exclude_keywords(sub, c(unlist(keywords), "합계"), "본수") %>% unlist
is.na(is_na) %>% sum == length(is_na)






## 🟥 1985 ===================================================
class_year = "1985"
sub = data_combined_final_new_2 %>% filter(class == class_year)
sub$year
keywords = list()
keywords[[1]] = c("잣나무")
extract_columns_by_keywords(sub, keywords[[1]]) %>% View
keywords[[2]] = c("낙엽송", "삼나무", "편백")
extract_columns_by_keywords(sub, keywords[[2]]) %>% View
keywords[[3]] = c("리기다_본수", "침엽수_테다")
extract_columns_by_keywords(sub, keywords[[3]]) %>% View
keywords[[4]] = c("리기테다", "강송", "해송")
extract_columns_by_keywords(sub, keywords[[4]]) %>% View
keywords[[5]] = c("밤나무", "이태리")
extract_columns_by_keywords(sub, keywords[[5]]) %>% View
keywords[[6]] = c("현사시", "오동", "기타_본수")
extract_columns_by_keywords(sub, keywords[[6]]) %>% View
is_na = extract_columns_by_include_exclude_keywords(sub, c(unlist(keywords), "합계"), "본수") %>% unlist
is.na(is_na) %>% sum == length(is_na)






## 🟥 1986 ===================================================
class_year = "1986"
sub = data_combined_final_new_2 %>% filter(class == class_year)
sub$year
keywords = list()
keywords[[1]] = c("잣나무")
extract_columns_by_keywords(sub, keywords[[1]]) %>% View
keywords[[2]] = c("낙엽송", "삼나무", "편백")
extract_columns_by_keywords(sub, keywords[[2]]) %>% View
keywords[[3]] = c("리기다_본수", "침엽수_테다")
extract_columns_by_keywords(sub, keywords[[3]]) %>% View
keywords[[4]] = c("리기테다", "강송", "해송")
extract_columns_by_keywords(sub, keywords[[4]]) %>% View
keywords[[5]] = c("밤나무", "이태리")
extract_columns_by_keywords(sub, keywords[[5]]) %>% View
keywords[[6]] = c("현사시", "오동", "기타_본수")
extract_columns_by_keywords(sub, keywords[[6]]) %>% View
is_na = extract_columns_by_include_exclude_keywords(sub, c(unlist(keywords), "합계"), "본수") %>% unlist
is.na(is_na) %>% sum == length(is_na)





## 🟥 1987 ===================================================
class_year = "1987"
sub = data_combined_final_new_2 %>% filter(class == class_year)
sub$year
keywords = list()
keywords[[1]] = c("잣나무")
extract_columns_by_keywords(sub, keywords[[1]]) %>% View
keywords[[2]] = c("낙엽송", "삼나무", "편백")
extract_columns_by_keywords(sub, keywords[[2]]) %>% View
keywords[[3]] = c("리기다_본수", "침엽수_테다")
extract_columns_by_keywords(sub, keywords[[3]]) %>% View
keywords[[4]] = c("리기테다", "강송", "해송")
extract_columns_by_keywords(sub, keywords[[4]]) %>% View
keywords[[5]] = c("밤나무", "이태리")
extract_columns_by_keywords(sub, keywords[[5]]) %>% View
keywords[[6]] = c("현사시", "오동", "기타_본수")
extract_columns_by_keywords(sub, keywords[[6]]) %>% View
is_na = extract_columns_by_include_exclude_keywords(sub, c(unlist(keywords), "합계"), "본수") %>% unlist
is.na(is_na) %>% sum == length(is_na)



## 🟥 1988 ===================================================
class_year = "1988"
sub = data_combined_final_new_2 %>% filter(class == class_year)
sub$year
keywords = list()
keywords[[1]] = c("잣나무")
extract_columns_by_keywords(sub, keywords[[1]]) %>% View
keywords[[2]] = c("낙엽송", "삼나무", "편백")
extract_columns_by_keywords(sub, keywords[[2]]) %>% View
keywords[[3]] = c("리기다_본수", "침엽수_테다")
extract_columns_by_keywords(sub, keywords[[3]]) %>% View
keywords[[4]] = c("리기테다", "강송", "해송")
extract_columns_by_keywords(sub, keywords[[4]]) %>% View
keywords[[5]] = c("밤나무", "이태리")
extract_columns_by_keywords(sub, keywords[[5]]) %>% View
keywords[[6]] = c("현사시", "오동", "기타_본수")
extract_columns_by_keywords(sub, keywords[[6]]) %>% View
is_na = extract_columns_by_include_exclude_keywords(sub, c(unlist(keywords), "합계"), "본수") %>% unlist
is.na(is_na) %>% sum == length(is_na)





## 🟥 1989 ===================================================
class_year = "1989"
sub = data_combined_final_new_2 %>% filter(class == class_year)
sub$year
keywords = list()
keywords[[1]] = c("잣나무")
extract_columns_by_keywords(sub, keywords[[1]]) %>% View
keywords[[2]] = c("낙엽송", "삼나무", "편백")
extract_columns_by_keywords(sub, keywords[[2]]) %>% View
keywords[[3]] = c("리기다_본수", "침엽수_테다")
extract_columns_by_keywords(sub, keywords[[3]]) %>% View
keywords[[4]] = c("리기테다", "강송", "해송")
extract_columns_by_keywords(sub, keywords[[4]]) %>% View
keywords[[5]] = c("밤나무", "이태리")
extract_columns_by_keywords(sub, keywords[[5]]) %>% View
keywords[[6]] = c("현사시", "오동", "기타_본수")
extract_columns_by_keywords(sub, keywords[[6]]) %>% View
is_na = extract_columns_by_include_exclude_keywords(sub, c(unlist(keywords), "합계"), "본수") %>% unlist
is.na(is_na) %>% sum == length(is_na)






## 🟥 1990 ===================================================
class_year = "1990"
sub = data_combined_final_new_2 %>% filter(class == class_year)
sub$year
keywords = c("잣나무")
extract_columns_by_keywords(sub, keywords) %>% View
keywords = c("낙엽송", "삼나무", "편백")
extract_columns_by_keywords(sub, keywords) %>% View
keywords = c("리기다_본수", "침엽수_테다")
extract_columns_by_keywords(sub, keywords) %>% View
keywords = c("리기테다", "강송", "해송")
extract_columns_by_keywords(sub, keywords) %>% View
keywords = c("밤나무", "이태리")
extract_columns_by_keywords(sub, keywords) %>% View
keywords = c("현사시", "오동", "기타_본수")
extract_columns_by_keywords(sub, keywords) %>% View




## 🟥 1991 ===================================================
class_year = "1991"
sub = data_combined_final_new_2 %>% filter(class == class_year)
sub$year
keywords = c("잣나무")
extract_columns_by_keywords(sub, keywords) %>% View
keywords = c("낙엽송", "삼나무", "편백")
extract_columns_by_keywords(sub, keywords) %>% View
keywords = c("리기다_본수", "침엽수_테다")
extract_columns_by_keywords(sub, keywords) %>% View
keywords = c("리기테다", "강송", "해송")
extract_columns_by_keywords(sub, keywords) %>% View
keywords = c("밤나무", "이태리")
extract_columns_by_keywords(sub, keywords) %>% View
keywords = c("현사시", "오동", "기타_본수")
extract_columns_by_keywords(sub, keywords) %>% View





## 🟥 1992 ===================================================
class_year = "1992"
sub = data_combined_final_new_2 %>% filter(class == class_year)
sub$year
keywords = c("잣나무")
extract_columns_by_keywords(sub, keywords) %>% View
keywords = c("낙엽송", "삼나무", "편백")
extract_columns_by_keywords(sub, keywords) %>% View
keywords = c("리기다_본수", "침엽수_테다")
extract_columns_by_keywords(sub, keywords) %>% View
keywords = c("리기테다", "강송", "해송")
extract_columns_by_keywords(sub, keywords) %>% View
keywords = c("밤나무", "이태리")
extract_columns_by_keywords(sub, keywords) %>% View
keywords = c("현사시", "오동", "기타_본수")
extract_columns_by_keywords(sub, keywords) %>% View






## 🟥 1993 ===================================================
class_year = "1993"
sub = data_combined_final_new_2 %>% filter(class == class_year)
sub$year
keywords = c("잣나무")
extract_columns_by_keywords(sub, keywords) %>% View
keywords = c("낙엽송", "삼나무", "편백")
extract_columns_by_keywords(sub, keywords) %>% View
keywords = c("리기다_본수", "침엽수_테다")
extract_columns_by_keywords(sub, keywords) %>% View
keywords = c("리기테다", "강송", "해송")
extract_columns_by_keywords(sub, keywords) %>% View
keywords = c("밤나무", "이태리")
extract_columns_by_keywords(sub, keywords) %>% View
keywords = c("현사시", "오동", "기타_본수")
extract_columns_by_keywords(sub, keywords) %>% View





## 🟥 1994 ===================================================
class_year = "1994"
sub = data_combined_final_new_2 %>% filter(class == class_year)
sub$year
keywords = c("잣나무")
extract_columns_by_keywords(sub, keywords) %>% View
keywords = c("낙엽송", "삼나무", "편백")
extract_columns_by_keywords(sub, keywords) %>% View
keywords = c("리기다_본수", "침엽수_테다")
extract_columns_by_keywords(sub, keywords) %>% View
keywords = c("리기테다", "강송", "해송")
extract_columns_by_keywords(sub, keywords) %>% View
keywords = c("밤나무", "이태리")
extract_columns_by_keywords(sub, keywords) %>% View
keywords = c("현사시", "오동", "기타_본수")
extract_columns_by_keywords(sub, keywords) %>% View




## 🟥 1995 ===================================================
class_year = "1995"
sub = data_combined_final_new_2 %>% filter(class == class_year)
sub$year
keywords = c("잣나무")
extract_columns_by_keywords(sub, keywords) %>% View
keywords = c("낙엽송", "삼나무", "편백")
extract_columns_by_keywords(sub, keywords) %>% View
keywords = c("리기다_본수", "침엽수_테다")
extract_columns_by_keywords(sub, keywords) %>% View
keywords = c("리기테다", "강송", "해송")
extract_columns_by_keywords(sub, keywords) %>% View
keywords = c("밤나무", "이태리")
extract_columns_by_keywords(sub, keywords) %>% View
keywords = c("현사시", "오동", "기타_본수")
extract_columns_by_keywords(sub, keywords) %>% View



## 🟥 1996 ===================================================
class_year = "1996"
sub = data_combined_final_new_2 %>% filter(class == class_year)
sub$year
keywords = c("잣나무")
extract_columns_by_keywords(sub, keywords) %>% View
keywords = c("낙엽송", "삼나무", "편백")
extract_columns_by_keywords(sub, keywords) %>% View
keywords = c("리기다_본수", "침엽수_테다")
extract_columns_by_keywords(sub, keywords) %>% View
keywords = c("리기테다", "강송", "해송")
extract_columns_by_keywords(sub, keywords) %>% View
keywords = c("밤나무", "이태리")
extract_columns_by_keywords(sub, keywords) %>% View
keywords = c("현사시", "오동", "기타_본수")
extract_columns_by_keywords(sub, keywords) %>% View






## 🟥 1997 ===================================================
class_year = "1997"
sub = data_combined_final_new_2 %>% filter(class == class_year)
sub$year
keywords = c("잣나무")
extract_columns_by_keywords(sub, keywords) %>% View
keywords = c("낙엽송", "삼나무", "편백")
extract_columns_by_keywords(sub, keywords) %>% View
keywords = c("리기다_본수", "침엽수_테다")
extract_columns_by_keywords(sub, keywords) %>% View
keywords = c("리기테다", "강송", "해송")
extract_columns_by_keywords(sub, keywords) %>% View
keywords = c("밤나무", "이태리")
extract_columns_by_keywords(sub, keywords) %>% View
keywords = c("현사시", "오동", "기타_본수")
extract_columns_by_keywords(sub, keywords) %>% View






## 🟥 1998 ===================================================
class_year = "1998"
sub = data_combined_final_new_2 %>% filter(class == class_year)
sub$year
keywords = c("잣나무")
extract_columns_by_keywords(sub, keywords) %>% View
keywords = c("낙엽송", "삼나무", "편백")
extract_columns_by_keywords(sub, keywords) %>% View
keywords = c("리기다_본수", "침엽수_테다")
extract_columns_by_keywords(sub, keywords) %>% View
keywords = c("리기테다", "강송", "해송")
extract_columns_by_keywords(sub, keywords) %>% View
keywords = c("밤나무", "이태리")
extract_columns_by_keywords(sub, keywords) %>% View
keywords = c("현사시", "오동", "기타_본수")
extract_columns_by_keywords(sub, keywords) %>% View






## 🟥 1999 ===================================================
class_year = "1999"
sub = data_combined_final_new_2 %>% filter(class == class_year)
sub$year
sub$합계_수량_New
keywords = c("잣나무", "낙엽송")
extract_columns_by_keywords(sub, keywords) %>% View
keywords = c("낙엽송", "삼나무", "편백")
extract_columns_by_keywords(sub, keywords) %>% View
keywords = c("리기다_본수", "침엽수_테다")
extract_columns_by_keywords(sub, keywords) %>% View
keywords = c("리기테다", "강송", "해송")
extract_columns_by_keywords(sub, keywords) %>% View
keywords = c("밤나무", "이태리")
extract_columns_by_keywords(sub, keywords) %>% View
keywords = c("현사시", "오동", "기타_본수")
extract_columns_by_keywords(sub, keywords) %>% View



## 🟥 2000 ===================================================
class_year = "2000"
sub = data_combined_final_new_2 %>% filter(class == class_year)
sub$year
sub$합계_수량_New
keywords = list()
keywords[[1]] = c("소나무", "_잣나무", "낙엽송")
extract_columns_by_keywords(sub, keywords[[1]]) %>% View
keywords[[2]] = c("리기다_", "삼나무", "편백", "침엽수_기타")
extract_columns_by_keywords(sub, keywords[[2]]) %>% View
keywords[[3]] = c("느티", "물푸레", "벚나무")
extract_columns_by_keywords(sub, keywords[[3]]) %>% View
keywords[[4]] = c("상수리", "자작", "고로쇠", "활엽수_기타")
extract_columns_by_keywords(sub, keywords[[4]]) %>% View
extract_columns_by_include_exclude_keywords(sub, c(unlist(keywords), "합계"), "본수") %>% View



## 🟥 2001 ===================================================
class_year = "2001"
sub = data_combined_final_new_2 %>% filter(class == class_year)
sub$year
sub$합계_수량_New
keywords = list()
keywords[[1]] = c("소나무", "_잣나무", "낙엽송")
extract_columns_by_keywords(sub, keywords[[1]]) %>% View
keywords[[2]] = c("리기다_", "삼나무", "편백", "침엽수_기타")
extract_columns_by_keywords(sub, keywords[[2]]) %>% View
keywords[[3]] = c("느티", "물푸레", "벚나무")
extract_columns_by_keywords(sub, keywords[[3]]) %>% View
keywords[[4]] = c("상수리", "자작", "고로쇠", "활엽수_기타")
extract_columns_by_keywords(sub, keywords[[4]]) %>% View
is_na = extract_columns_by_include_exclude_keywords(sub, c(unlist(keywords), "합계"), "본수") %>% unlist
is.na(is_na) %>% sum == length(is_na)




## 🟥 2002 ===================================================
class_year = "2002"
sub = data_combined_final_new_2 %>% filter(class == class_year)
sub$year
sub$합계_수량_New
keywords = list()
keywords[[1]] = c("소나무", "_잣나무", "낙엽송")
extract_columns_by_keywords(sub, keywords[[1]]) %>% View
keywords[[2]] = c("리기다_", "삼나무", "편백", "침엽수_기타")
extract_columns_by_keywords(sub, keywords[[2]]) %>% View
keywords[[3]] = c("느티", "물푸레", "벚나무")
extract_columns_by_keywords(sub, keywords[[3]]) %>% View
keywords[[4]] = c("상수리", "자작", "고로쇠", "활엽수_기타")
extract_columns_by_keywords(sub, keywords[[4]]) %>% View
is_na = extract_columns_by_include_exclude_keywords(sub, c(unlist(keywords), "합계"), "본수") %>% unlist
is.na(is_na) %>% sum == length(is_na)



## 🟥 2003 ===================================================
class_year = "2003"
sub = data_combined_final_new_2 %>% filter(class == class_year)
sub$year
sub$합계_수량_New
keywords = list()
keywords[[1]] = c("소나무", "_잣나무", "낙엽송")
extract_columns_by_keywords(sub, keywords[[1]]) %>% View
keywords[[2]] = c("리기다_", "삼나무", "편백", "침엽수_기타")
extract_columns_by_keywords(sub, keywords[[2]]) %>% View
keywords[[3]] = c("느티", "물푸레", "벚나무")
extract_columns_by_keywords(sub, keywords[[3]]) %>% View
keywords[[4]] = c("상수리", "자작", "고로쇠", "활엽수_기타")
extract_columns_by_keywords(sub, keywords[[4]]) %>% View
is_na = extract_columns_by_include_exclude_keywords(sub, c(unlist(keywords), "합계"), "본수") %>% unlist
is.na(is_na) %>% sum == length(is_na)




## 🟥 2004 ===================================================
class_year = "2004"
sub = data_combined_final_new_2 %>% filter(class == class_year)
sub$year
sub$합계_수량_New
keywords = list()
keywords[[1]] = c("소나무", "_잣나무", "낙엽송")
extract_columns_by_keywords(sub, keywords[[1]]) %>% View
keywords[[2]] = c("리기다_", "삼나무", "편백", "침엽수_기타")
extract_columns_by_keywords(sub, keywords[[2]]) %>% View
keywords[[3]] = c("느티", "물푸레", "벚나무")
extract_columns_by_keywords(sub, keywords[[3]]) %>% View
keywords[[4]] = c("상수리", "자작", "고로쇠", "활엽수_기타")
extract_columns_by_keywords(sub, keywords[[4]]) %>% View
is_na = extract_columns_by_include_exclude_keywords(sub, c(unlist(keywords), "합계"), "본수") %>% unlist
is.na(is_na) %>% sum == length(is_na)




## 🟥 2005 ===================================================
class_year = "2005"
sub = data_combined_final_new_2 %>% filter(class == class_year)
sub$year
sub$합계_수량_New
keywords = list()
keywords[[1]] = c("소나무", "_잣나무", "낙엽송")
extract_columns_by_keywords(sub, keywords[[1]]) %>% View
keywords[[2]] = c("리기다_", "삼나무", "편백", "침엽수_기타")
extract_columns_by_keywords(sub, keywords[[2]]) %>% View
keywords[[3]] = c("느티", "물푸레", "벚나무")
extract_columns_by_keywords(sub, keywords[[3]]) %>% View
keywords[[4]] = c("상수리", "자작", "고로쇠", "활엽수_기타")
extract_columns_by_keywords(sub, keywords[[4]]) %>% View
is_na = extract_columns_by_include_exclude_keywords(sub, c(unlist(keywords), "합계"), "본수") %>% unlist
is.na(is_na) %>% sum == length(is_na)









## 🟥 2006 ===================================================
class_year = "2006"
sub = data_combined_final_new_2 %>% filter(class == class_year)
sub$year
sub$합계_수량_New
keywords = list()
keywords[[1]] = c("소나무", "_잣나무", "낙엽송")
extract_columns_by_keywords(sub, keywords[[1]]) %>% View
keywords[[2]] = c("삼나무", "편백", "해송", "침엽수_기타")
extract_columns_by_keywords(sub, keywords[[2]]) %>% View
keywords[[3]] = c("고로쇠", "느티", "물푸레")
extract_columns_by_keywords(sub, keywords[[3]]) %>% View
keywords[[4]] = c("벚", "상수리", "자작", "활엽수_기타")
extract_columns_by_keywords(sub, keywords[[4]]) %>% View
is_na = extract_columns_by_include_exclude_keywords(sub, c(unlist(keywords), "합계"), "본수") %>% unlist
is.na(is_na) %>% sum == length(is_na)



## 🟥 2007 ===================================================
class_year = "2007"
sub = data_combined_final_new_2 %>% filter(class == class_year)
sub$year
sub$합계_수량_New
keywords = list()
keywords[[1]] = c("소나무", "_잣나무", "낙엽송")
extract_columns_by_keywords(sub, keywords[[1]]) %>% View
keywords[[2]] = c("삼나무", "편백", "해송", "침엽수_기타")
extract_columns_by_keywords(sub, keywords[[2]]) %>% View
keywords[[3]] = c("고로쇠", "느티", "물푸레")
extract_columns_by_keywords(sub, keywords[[3]]) %>% View
keywords[[4]] = c("벚", "상수리", "자작", "활엽수_기타")
extract_columns_by_keywords(sub, keywords[[4]]) %>% View
is_na = extract_columns_by_include_exclude_keywords(sub, c(unlist(keywords), "합계"), "본수") %>% unlist
is.na(is_na) %>% sum == length(is_na)




## 🟥 2008 ===================================================
class_year = "2008"
sub = data_combined_final_new_2 %>% filter(class == class_year)
sub$year
sub$합계_수량_New
keywords = list()
keywords[[1]] = c("소나무", "_잣나무", "낙엽송")
extract_columns_by_keywords(sub, keywords[[1]]) %>% View
keywords[[2]] = c("삼나무", "편백", "해송", "침엽수_기타")
extract_columns_by_keywords(sub, keywords[[2]]) %>% View
keywords[[3]] = c("고로쇠", "느티", "물푸레")
extract_columns_by_keywords(sub, keywords[[3]]) %>% View
keywords[[4]] = c("벚", "상수리", "자작", "활엽수_기타")
extract_columns_by_keywords(sub, keywords[[4]]) %>% View
is_na = extract_columns_by_include_exclude_keywords(sub, c(unlist(keywords), "합계"), "본수") %>% unlist
is.na(is_na) %>% sum == length(is_na)






## 🟥 2009 ===================================================
class_year = "2009"
sub = data_combined_final_new_2 %>% filter(class == class_year)
sub$year
sub$합계_수량_New
keywords = list()
keywords[[1]] = c("소나무", "_잣나무", "낙엽송")
extract_columns_by_keywords(sub, keywords[[1]]) %>% View
keywords[[2]] = c("삼나무", "편백", "해송", "침엽수_기타")
extract_columns_by_keywords(sub, keywords[[2]]) %>% View
keywords[[3]] = c("고로쇠", "느티", "물푸레")
extract_columns_by_keywords(sub, keywords[[3]]) %>% View
keywords[[4]] = c("벚", "상수리", "자작", "활엽수_기타")
extract_columns_by_keywords(sub, keywords[[4]]) %>% View
is_na = extract_columns_by_include_exclude_keywords(sub, c(unlist(keywords), "합계"), "본수") %>% unlist
is.na(is_na) %>% sum == length(is_na)



      
# 🟪 내보내기 ======================================================================================
path_save = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/조림/수종별 조림실적Plantation forest by tree species/Combined"
file_name = "5.Combined_final.xlsx"
write.xlsx(data_combined_final_2 , file.path(path_save, file_name))




