### 🟩 데이터 로드  ======================================================
path_data = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/조림/수종별 조림실적Plantation forest by tree species/3.00/2000_YRBK_0030030501.xlsx"
data = read.xlsx(path_data)
# View(data)
names(data)



### 🟧 침엽수, 활엽수 문자열 추가 ======================================================================
# "침엽수" 또는 "활엽수"가 포함되지 않은 열 이름만 추출
filtered_colnames <- names(data)[!grepl("침엽수|활엽수", names(data))]

# 결과 출력
print(filtered_colnames)

conifer_trees = c("잣나무", "낙엽송", "리기다", "리기테다", "강송", "해송", "소나무")
broad_trees = c("물푸레", "느티나무")

  

### 🟧 데이터 정리 ======================================================================
View(data_2)
data_2 <- data %>% 
  relocate("구분", "구분2", .after = "ID") %>%  # 구분 열 옮기기
  select(-"구분2") %>%  # 구분2 열 삭제
  mutate(year = 2000) %>%  # 2000년 연도 추가
  relocate(year, .after = "구분") %>%  # year 연도 옮기기
  select(-행) %>%  # 행 열 삭제
  mutate(구분 = "1999") %>%
  rename(classification = 구분) %>% 
  select(-contains("_면적")) %>%  # "_면적"이 포함된 열 제외
  relocate(ends_with("_계_본수"), .after = 4) %>%  # "_계_본수"로 끝나는 열을 4번째 열 뒤로 이동
  combine_data_by_col %>%  # 3열 제외 열 별 합치기
  rename(conifer_total_seedling = 침엽수_계_본수) %>% 
  rename(broad_total_seedling = 활엽수_계_본수) %>% 
  # "침엽수_" 및 "활엽수_" 접두사 추가
  rename_with(~ ifelse(grepl(paste(conifer_trees, collapse = "|"), .) & !grepl("^침엽수_", .), 
                       paste0("침엽수_", .), .), 
              everything()) %>% 
  rename_with(~ ifelse(grepl(paste(broad_trees, collapse = "|"), .) & !grepl("^활엽수_", .), 
                       paste0("활엽수_", .), .), 
              everything())  %>% 
  mutate(
    total_seedling = conifer_total_seedling + broad_total_seedling,  # 합계 계산
    # "침엽수_"로 시작하는 열들의 합을 계산하여 새로운 열에 저장
    conifer_total_seedling_direct = rowSums(select(., starts_with("침엽수_")), na.rm = TRUE),
    # "활엽수_"로 시작하는 열들의 합을 계산하여 새로운 열에 저장
    broad_total_seedling_direct = rowSums(select(., starts_with("활엽수_")), na.rm = TRUE)
  ) %>% 
  relocate(total_seedling, .after = year) %>%   # 합계 열 이동
  mutate(total_seedling_direct = conifer_total_seedling_direct + broad_total_seedling_direct) %>% 
  relocate(total_seedling_direct, conifer_total_seedling_direct, broad_total_seedling_direct, .after = year) %>% 
  mutate(diff_abs = abs(total_seedling_direct - total_seedling)) %>% 
  relocate(diff_abs, .after = year) %>% 
  select(-all_of(starts_with("침엽수_"))) %>% 
  select(-all_of(starts_with("활엽수_")))




### 🟨 Export ================================================================
View(data_2)
path_save = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/조림/수종별 조림실적Plantation forest by tree species/Combined"
file_name = "3.Combined_00_직접입력.xlsx"
write.xlsx(data_2, file.path(path_save, file_name))
# data_sum




