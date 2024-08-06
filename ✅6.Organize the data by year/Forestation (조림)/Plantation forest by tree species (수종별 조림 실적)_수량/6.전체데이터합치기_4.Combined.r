# 🟥 (23년도 + 04~22년도 + 00년도 + 국유림민유림) + Total 면적만 존재 ===================================================================================================
## 🟩 데이터 로드 ===========================================================================================
path_data_only_total = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID/@_조림/_(@완료)수종별 조림실적Plantation forest by tree species/@완료/1.Total 면적만 존재.xlsx"
path_data_combined = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID/@_조림/_(@완료)수종별 조림실적Plantation forest by tree species/@완료/6.전체데이터합치기/3.Combined.xlsx"

data_only_total = read.xlsx(path_data_only_total)
data_only_total_sub = data_only_total[1:5]
data_combined = read.xlsx(path_data_combined)
# View(data_combined)



## 🟪 열이름 확인 및 수정 ===========================================================================================
names(data_only_total_sub)[2] = "구분"
names(data_only_total_sub)[3] = "합계_면적"

names(data_combined)
data_combined$year


## 🟦 79 ~ 85년도 데이터만 추출해서 추정 면적으로 추정 비율 구하기 ===========================================================================================
library(dplyr)

# "구분" 열에서 지정된 연도에 해당하고, 중복된 "구분"이 있을 경우 "year" 열의 최대값을 갖는 행을 추출
result <- data_combined %>%
  mutate(year = as.numeric(year)) %>%  # "year" 열을 수치형으로 변환
  filter(구분 %in% c("1979", "1980", "1981", "1982", "1983", "1984", "1985")) %>%  # 지정된 연도 필터링
  arrange(구분, desc(year)) %>%  # "구분"으로 그룹화하고 "year" 내림차순으로 정렬
  group_by(구분) %>%  # "구분"으로 그룹화
  slice(1)  # 각 그룹에서 첫 번째 행 선택 (가장 큰 "year")

View(result)

# 비율 추정
conifer_sum = result$침엽수_합계_면적 %>% sum
broadleaf_sum = result$활엽수_합계_면적 %>% sum
conifer_prop = conifer_sum/(broadleaf_sum + conifer_sum)

# 추정된 비율로 추정 면적 구하기
data_only_total_sub$침엽수_합계_면적 = data_only_total_sub$합계_면적 * conifer_prop
data_only_total_sub$활엽수_합계_면적 = data_only_total_sub$합계_면적 * (1-conifer_prop)
data_only_total_sub$비고 = "침엽수, 활엽수 합계 면적은 추정치(79~85 각 추정 합계 면적 합계에서의 비율)"
# View(data_only_total_sub)


## 🟧 합치기 ====================================================================================
data_only_total_new = cbind(data_only_total_sub, data_only_total[6:ncol(data_only_total)])
# View(data_only_total_new)
names(data_only_total_new)
names(data_combined)

data_combined_new = bind_rows(list(data_only_total_new, data_combined))
View(data_combined_new)


## 🟧 가장 최근 연도로 추출 ====================================================================================
View(data_combined_new)


# '구분' 별로 가장 최근 'year'를 가진 행만을 추출
data_combined_new$year %>% class
latest_year_rows <- data_combined_new %>%
  group_by(구분) %>%  # '구분' 열로 그룹화
  filter(year == max(year)) %>%  # 각 그룹 내에서 'year' 값이 최대인 행만 필터링
  ungroup() %>%   # 그룹화 해제 
  arrange(구분, year)

# 결과 확인
# print(latest_year_rows)
View(latest_year_rows)
latest_year_rows$year %>% unique
latest_year_rows$구분 %>% unique
names(latest_year_rows)
cbind(latest_year_rows$합계_면적, latest_year_rows$침엽수_합계_면적 + latest_year_rows$활엽수_합계_면적) %>% View
  


## 🟧 시계열 플롯 확인 ====================================================================================
plot_time_series_ggplot(latest_year_rows$침엽수_합계_면적)
plot_time_series_ggplot(latest_year_rows$활엽수_합계_면적)





## 🟧 내보내기 ====================================================================================
path_save = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID/@_조림/_(@완료)수종별 조림실적Plantation forest by tree species/@완료/6.전체데이터합치기"
file_name = "수종별 조림실적Plantation forest by tree species.xlsx"
write.xlsx(latest_year_rows, file.path(path_save, file_name))





