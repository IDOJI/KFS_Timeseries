# 🟥 04~22 + 00 ======================================================================================
## 🟨 데이터 로드 ====================================================================================
path_data_3 = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/조림/수종별 조림실적Plantation forest by tree species/Combined/3.Combined_00_직접입력.xlsx"
path_data_4 = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/조림/수종별 조림실적Plantation forest by tree species/Combined/4.Combined_04~22.xlsx"

data_3 = read.xlsx(path_data_3)
data_4 = read.xlsx(path_data_4)



## 🟩열이름 확인 =====================================================================================
names(data_3)
names(data_4)
data_3$구분 = data_3$구분 %>% as.character
data_4$구분 = data_4$구분 %>% as.character




## 🟧 데이터 합치기 ================================================================================
combined_34 = bind_rows(list(data_3, data_4))
grep("기타", names(combined_34), value=T)




# 🟥 (04~22 + 00) + (82~99) ======================================================================================
path_data_2 = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/조림/수종별 조림실적Plantation forest by tree species/Combined/2.Combined_82~99_국유림민유림.xlsx"
data_2 = read.xlsx(path_data_2)
dim(data_2)
names(data_2 )
names(data_2 )[2]  = "구분"
names(data_2 )


# change col class
combined_34$NAME_L4 = combined_34$NAME_L4 %>% as.character
data_2$NAME_L4 = data_2$NAME_L4 %>% as.character
data_2$ID_L4 = data_2$ID_L4 %>% as.character()
combined_34$unit_L3 = combined_34$unit_L3 %>% as.character
data_2$unit_L4 = data_2$unit_L4 %>% as.character
combined_34$비고_L3 = combined_34$비고_L3 %>% as.character
data_2$비고_L4 = data_2$비고_L3 %>% as.character

# combine
combined_234 = bind_rows(list(data_2, combined_34))
names(combined_234)
grep("기타", names(combined_234), value=T)




# 🟥 (04~22 + 00 + 82~99) + (68~80) ======================================================================================
path_data_1 = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/조림/수종별 조림실적Plantation forest by tree species/Combined/1.Combined_68~80.xlsx"
data_1 = read.xlsx(path_data_1)
names(data_1)

names(combined_234)


data_1$NAME_L4 = data_1$NAME_L4 %>% as.character
data_1$ID_L4  = data_1$ID_L4 %>% as.character
data_1$unit_L4  = data_1$unit_L4 %>% as.character
data_1$비고_L3  = data_1$비고_L3 %>% as.character
data_1$비고_L4  = data_1$비고_L4 %>% as.character

# combine
combined_1234 = bind_rows(list(data_1, combined_234))
names(combined_1234)

grep("기타", names(combined_1234), value=T)




# 🟥 데이터 별로 추출 해서 비교 ======================================================================================
## 🟨 활엽수 ======================================================================================
broadleaf.df = combined_1234 %>% 
  select(starts_with("활엽수_")) %>% 
  # rename("#___활엽수_합계_면적" = "활엽수_합계_면적") %>% 
  rename("#___활엽수_합계_본수" = "활엽수_합계_본수") %>% 
  select(., sort(names(.))) %>% 
  select(contains("본수")) %>% 
  relocate("활엽수_기타_본수", .after = "#___활엽수_합계_본수")
  
names(broadleaf.df)

summed = rowSums(broadleaf.df[,2:ncol(broadleaf.df)], na.rm = T)

data.frame(broadleaf.df[[1]], summed, diff = abs(broadleaf.df[[1]] - summed) %>% round) %>% View

broadleaf.df_2 = broadleaf.df
names(broadleaf.df_2)




## 🟨 침엽수 ======================================================================================
conifer.df = combined_1234 %>% 
  select(starts_with("침엽수_")) %>% 
  # select(-"침엽수_총_면적", -"침엽수_본수_차이") %>% 
  # rename("#___침엽수_합계_면적" = "침엽수_합계_면적") %>% 
  rename("#___침엽수_합계_본수" = "침엽수_합계_본수") %>% 
  select(., sort(names(.))) %>% 
  select(contains("본수"))


summed = rowSums(conifer.df[,2:ncol(conifer.df)], na.rm = T)
data.frame(conifer.df[[1]], summed, diff = abs(conifer.df[[1]] - summed) %>% round) %>% View
conifer.df_2 = conifer.df %>% relocate("침엽수_기타_본수", .after = "#___침엽수_합계_본수")

names(conifer.df_2)



## 🟨 기타열 ======================================================================================
grep("기타", names(combined_1234), value = T)

# "활엽수" 또는 "침엽수"를 포함하지 않는 열 이름 선택
others_1 = combined_1234 %>% 
  select(., -grep("L1|L2|L3|L4|L5|활엽수|침엽수|본수|합계", names(.)))
names(others_1)
others_2 = combined_1234 %>% 
  select(., grep("L1|L2|L3|L4|L5", names(.)))
names(others_2)
others_3 = combined_1234 %>% select("기타_본수")
names(others_3)

# 전체 합계 열
data.frame(combined_1234[["본수_합계"]], combined_1234[["합계_수량"]], combined_1234[["year"]])
total = combined_1234 %>% 
  merge_columns("본수_합계", "합계_수량", new_col_name = "합계_수량_merged") %>% 
  select("합계_수량_merged")

# 합계 본수
# total_sub = combined_1234 %>% select(c("침엽수_합계_본수", "활엽수_합계_본수"))



# 🟪 합치기 ======================================================================================
data_combined_1234 = cbind(others_1,
                           others_3,
                           total,
                           # total_sub,
                           conifer.df_2,
                           broadleaf.df_2,
                           others_2) %>% 
  relocate(c("#___활엽수_합계_본수", "활엽수_기타_본수"), .after = "침엽수_기타_본수") %>% 
  relocate("기타_본수", .after = "#___활엽수_합계_본수") %>% 
  relocate("침엽수_기타_본수", .after = "기타_본수") %>% 
  select(-구분_1)

which(names(data_combined_1234)=="기타_본수")


# 모든 열 이름 추출?>
names(combined_1234)[which(! names(combined_1234) %in% names(data_combined_1234))]
View(data_combined_1234)



# 🟪 각 구분에 대해 최신 연도 값만 추출 ======================================================================================
names(data_combined_1234)

library(dplyr)

# 데이터프레임에서 각 "구분"별로 가장 최근의 "year" 값을 갖는 행만 남기기
filtered_data <- data_combined_1234 %>%
  group_by(구분) %>%
  filter(year == max(year)) %>%
  ungroup()

# 결과 확인
print(filtered_data)
View(filtered_data)





# 🟪 연속적인 연도인가 ======================================================================================
library(dplyr)

# 연도 추출 및 정렬
years <- filtered_data  %>%
  select(구분) %>%
  distinct() %>%
  arrange(구분) %>%
  pull(구분)

# 연도가 연속적인지 확인하는 함수
check_consecutive_years <- function(years) {
  all(diff(years) == 1)
}

# 연도들이 연속적인지 확인
are_years_consecutive <- check_consecutive_years(as.numeric(years))

# 결과 출력
if (are_years_consecutive) {
  print("모든 연도가 연속적으로 존재합니다.")
} else {
  print("연도가 연속적으로 존재하지 않습니다.")
}

# 결과 확인
are_years_consecutive




# 🟪 기타에 대한 비율 추정 ======================================================================================
View(filtered_data )
what_cols = c("침엽수_기타_본수", "활엽수_기타_본수")

data_1 = filtered_data %>% filter(is.na(활엽수_기타_본수) & is.na(침엽수_기타_본수))
data_1 %>% select(all_of(what_cols)) %>% View
data_1 %>% pull(기타_본수) 
names(data_1)
# View(data_1)
data_2 = filtered_data %>% filter(!is.na(활엽수_기타_본수) & !is.na(침엽수_기타_본수))
data_2 %>% select(all_of(what_cols)) %>% View
data_2 %>% pull(기타_본수) 
data_2 = data_2 %>% 
  mutate(., 기타_본수 = rowSums(select(., 활엽수_기타_본수, 침엽수_기타_본수)))
data_2 %>% pull(기타_본수)
# View(data_2)



# 비율 구하기
conifer_sum = data_2 %>% pull(what_cols[1]) %>% sum
broadleaf_sum = data_2 %>% pull(what_cols[2]) %>% sum
conifer_prop = conifer_sum/sum(conifer_sum, broadleaf_sum)


data_1[["침엽수_기타_본수"]] = data_1[["기타_본수"]] * conifer_prop
data_1[["활엽수_기타_본수"]] = data_1[["기타_본수"]] * (1 - conifer_prop)
data_1[["침엽수_기타_본수"]] + data_1[["활엽수_기타_본수"]] == data_1[["기타_본수"]]
# View(data_1)




# 🟪 데이터 합치기 ======================================================================================
# names(data_combined_final)
library(dplyr)

data_combined_final <- rbind(data_1, data_2) %>% 
  remove_duplicate_columns
  




# 🟪 합계 비교 ======================================================================================
names(data_combined_final)
# 중복된 열 이름 확인
duplicated_names <- names(data_combined_final_2)[duplicated(names(data_combined_final_2))]
duplicated_names

names(data_combined_final)

data_combined_final_2 <- data_combined_final %>%
  # "활엽수_"로 시작하는 열들의 합계를 계산하여 새로운 열 생성
  mutate(
    활엽수_합계_계산 = rowSums(select(., starts_with("활엽수_")), na.rm = TRUE),
    # "침엽수_"로 시작하는 열들의 합계를 계산하여 새로운 열 생성
    침엽수_합계_계산 = rowSums(select(., starts_with("침엽수_")), na.rm = TRUE)
  ) %>%
  # 각 차이의 절댓값을 계산하여 새로운 열 생성
  mutate(
    활엽수_차이_절댓값 = abs(활엽수_합계_계산 - `#___활엽수_합계_본수`),
    침엽수_차이_절댓값 = abs(침엽수_합계_계산 - `#___침엽수_합계_본수`)
  ) %>%
  # 열 위치 이동
  relocate(활엽수_합계_계산, .after = `#___활엽수_합계_본수`) %>%
  relocate(활엽수_차이_절댓값, .after = 활엽수_합계_계산) %>%
  relocate(침엽수_합계_계산, .after = `#___침엽수_합계_본수`) %>%
  relocate(침엽수_차이_절댓값, .after = 침엽수_합계_계산) %>% 
  mutate(
    합계_수량_New = 침엽수_합계_계산 + 활엽수_합계_계산,
    차이_절댓값 = abs(합계_수량_New - 합계_수량_merged)
  ) %>%
  relocate(합계_수량_New, .after = 합계_수량_merged) %>%
  relocate(차이_절댓값, .after = 합계_수량_New)


# names(data_combined_final_2)

# %>% 
  # mutate(합계_수량_New = rowSums(select(., c("침엽수_합계_계산", "활엽수_합계_계산")))) %>% 
  # relocate(합계_수량_New, .after = "합계_수량_merged")
# View(data_combined_final_2)
# grep("합계", names(data_combined_final_2), value=T)



# 🟪 내보내기 ======================================================================================
path_save = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/조림/수종별 조림실적Plantation forest by tree species/Combined"
file_name = "5.Combined_final.xlsx"
write.xlsx(data_combined_final_2 , file.path(path_save, file_name))




