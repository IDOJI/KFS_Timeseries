# 🟥 (23년도 + 04~22년도 + 00년도) + 국유림민유림 ===================================================================================================
## 🟩 데이터 로드 ===========================================================================================
path_data_1999 = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID/@_조림/(@완료)수종별 조림실적Plantation forest by tree species/@완료/(@완료)2.국유림민유림.xlsx"
path_combined = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID/@_조림/(@완료)수종별 조림실적Plantation forest by tree species/@완료/6.전체데이터합치기/2.Combined.xlsx"

data_1999 = read.xlsx(path_data_1999)
data_1999_sub = data_1999[1:17]
data_combined = read.xlsx(path_combined)
data_combined_sub = data_combined[1:20]


# test = read.xlsx("/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID/@_조림/(@완료)수종별 조림실적Plantation forest by tree species/@완료/6.전체데이터합치기/1.Combined.xlsx")
# names(test)
# path_test = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID/@_조림/(@완료)수종별 조림실적Plantation forest by tree species/@완료/(@완료)4.2004~2022 침엽수, 활엽수 데이터합치기.xlsx"
# test = read.xlsx(path_test)
# names(test)


## 🟩 열 이름 확인 ===========================================================================================
data_1999_sub %>% names
data_combined_sub %>% names



## 🟩 열 이름 변경 ===========================================================================================
library(dplyr)
library(stringr)

# 열 이름 변경
data_1999_sub <- data_1999_sub %>%
  rename_with(~ str_replace(., "_계", "_면적"))

# 변경된 열 이름 확인
print(names(data_1999_sub))
data_1999_sub_2 = data_1999_sub %>% rename("구분" := "구분_1", "합계_면적" := "계_면적")
names(data_1999_sub_2)

# grep("오동", names(data_combined_sub), value=T)
# "잣나무_면적"      
# "낙엽송_면적"       "삼나무_면적"       "편백_면적"         "리기다_면적"      
# "테다_면적"         "리기테다_면적"     "강송_면적"         "해송_면적"        
# 
# 
# "밤나무_면적"       "이태리포플러_면적" "현사시_면적"       "오동_면적"

# 열 이름 변경
data_1999_sub_3 <- data_1999_sub_2 %>%
  rename_with(~ if_else(str_detect(., "잣나무_면적|낙엽송_면적|삼나무_면적|편백_면적|리기다_면적|테다_면적|리기테다_면적|강송_면적|해송_면적"),
                        str_c("침엽수_", .), .),
              .cols = c("잣나무_면적", "낙엽송_면적", "삼나무_면적", "편백_면적", "리기다_면적", "테다_면적", "리기테다_면적", "강송_면적", "해송_면적")) %>%
  rename_with(~ if_else(str_detect(., "밤나무_면적|이태리포플러_면적|현사시_면적|오동_면적"),
                        str_c("활엽수_", .), .),
              .cols = c("밤나무_면적", "이태리포플러_면적", "현사시_면적", "오동_면적"))

# 변경된 열 이름 확인
print(names(data_1999_sub_3))




## 🟩 열 이름 확인 ===========================================================================================
names(data_1999_sub_3)[!names(data_1999_sub_3) %in% names(data_combined_sub)]


data_combined_sub %>% names



## 🟨 기타에서의 활엽수 침엽수 비율 추정 ===========================================================================================
View(data_combined_sub)
# '구분' 열의 unique한 행들을 구하되, 'year' 열을 기준으로 가장 큰 값의 행을 선택
unique_rows <- data_combined_sub %>%
  mutate(year = as.numeric(year)) %>%  # 'year' 열을 수치형으로 변환
  group_by(구분) %>%  # '구분' 열로 그룹화
  filter(year == max(year)) %>%  # 각 그룹 내에서 'year' 값이 최대인 행만 필터링
  ungroup()  # 그룹화 해제

# 비율 추정
conifer_sum = sum(unique_rows$침엽수_기타_면적)
broadleaf_sum = sum(unique_rows$활엽수_기타_면적)

prop_conifer = conifer_sum/(broadleaf_sum + conifer_sum)


# 추정된 비율로 기타 면적 나누기
data_1999_sub_3$침엽수_기타_면적_추정 = data_1999_sub_3$기타_면적 * prop_conifer
data_1999_sub_3$활엽수_기타_면적_추정 = data_1999_sub_3$기타_면적 * (1-prop_conifer)
# data_1999_sub_3$침엽수_기타_면적_추정 + data_1999_sub_3$활엽수_기타_면적_추정 == data_1999_sub_3$기타_면적




## 🟨 활엽수 침엽수 합계 면적 추정 ===========================================================================================
# 침엽수 열들의 합계 계산
data_1999_sub_4 <- data_1999_sub_3 %>%
  mutate(침엽수_합계_면적 = rowSums(select(., starts_with("침엽수_")), na.rm = TRUE))
data_1999_sub_3 %>% select(starts_with("침엽수_")) %>% names
data_1999_sub_3 %>% select(starts_with("침엽수_")) %>% slice(1)
test = c(16746, 32622, 4088, 7517, 459, 1573, 156, 348.6174)
sum(test)

# data_1999_sub_4$침엽수_합계_면적[1]


data_1999_sub_4 <- data_1999_sub_3 %>%
  mutate(침엽수_합계_면적 = rowSums(select(., starts_with("침엽수_")), na.rm = TRUE))
# data_1999_sub_3 %>% names

# 활엽수 열들의 합계 계산
data_1999_sub_5 <- data_1999_sub_4 %>%
  mutate(활엽수_합계_면적 = rowSums(select(., starts_with("활엽수_")), na.rm = TRUE))

# 결과 출력을 위해 일부 열만 보기
View(data_1999_sub_5 %>% select(ID, 침엽수_합계_면적, 활엽수_합계_면적))
data_1999_sub_3[5,] %>% View


names(data_1999_sub_5)
data_1999_sub_5$비고 = "침엽, 활엽 각 합계는 추정치"
data_1999_sub_5 = data_1999_sub_5 %>% relocate("비고")
# data_1999_sub_3 = data_1999_sub_3 %>% rename()
names(data_1999_sub_5)




## 🟧 합치기 =====================================================================================
data_1999_new = cbind(data_1999_sub_5, data_1999[18:ncol(data_1999)])
names(data_1999_new)
names(data_combined)

combined = bind_rows(list(data_1999_new, data_combined)) %>% 
  relocate("활엽수_합계_면적", "침엽수_합계_면적")

names(combined)
View(combined)


## 🟧 구분에서 "면적"제거  ====================================================================================
combined$year
combined$구분
# combined 데이터프레임의 '구분' 열에서 연도만 추출
combined$구분 <- gsub("[^0-9]", "", combined$구분)

# 결과 확인
print(combined$구분)



## 🟧 내보내기 ====================================================================================
path_save = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID/@_조림/(@완료)수종별 조림실적Plantation forest by tree species/@완료/6.전체데이터합치기"
file_name = "3.Combined.xlsx"
# combined = read.xlsx(file.path(path_save, file_name))
write.xlsx(combined, file.path(path_save, file_name))





