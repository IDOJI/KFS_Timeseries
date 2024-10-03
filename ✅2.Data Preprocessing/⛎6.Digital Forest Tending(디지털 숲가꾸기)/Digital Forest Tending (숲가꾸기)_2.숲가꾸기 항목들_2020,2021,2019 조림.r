# 🟥 데이터 로드 =================================================================
path_data_1 = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/5.디지털숲가꾸기/디지털숲가꾸기db(2010~2019) (1)/조림(2010~2019).xlsx"
path_data_2 = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/5.디지털숲가꾸기/조림(2020_2024).xlsx"

data_1 = read.xlsx(path_data_1)
data_2 = read.xlsx(path_data_2)


# 🟪 check  =======================================================================================
names(data_1)
names(data_2)


# 🟪 year col  =======================================================================================
data_1[[1]]
data_2[[1]]
colnames(data_2)[1] = colnames(data_1)[1] = "year"
data_1$year %>% unique
data_2$year %>% unique




## 🟪 subset by year  =======================================================================================
years = c("2019", "2020", "2021")
data_1_sub = data_1 %>% filter(year %in% years)
data_2_sub = data_2 %>% filter(year %in% years)

dim(data_1_sub)
dim(data_2_sub)



# 🟩 check the regions  =======================================================================================
data_1_sub$`GROUP_FIELD(필지)` %>% unique
data_2_sub$`GROUP_FIELD(필지)` %>% unique



## 🟩 Group regions  =======================================================================================
data_1_sub = data_1_sub %>% new_regions
data_2_sub = data_2_sub %>% new_regions
View(data_2_sub)

# 결과 확인
data_1_sub$regions %>% table %>% as.data.frame()
data_2_sub$regions %>% table %>% as.data.frame()


# 기타?
data_1_sub %>% filter(regions == "기타") %>% View
data_2_sub %>% filter(regions == "기타") %>% View

names(data_1_sub)






## 🟨 필요열들 이름 변경  =======================================================================================
data_1_new = data_1_sub %>% 
  rename("work_area" = `PMS3A011_WORK_AREA(산림자원조성사업정보.작업면적)`) %>% 
  # rename("forest_tending" = `PMS3A011_FRCMB_NM1(숲가꾸기내역.작업종1)`) %>% 
  relocate(work_area, .after = 2)


data_2_new = data_2_sub %>% 
  rename("work_area" = `PMS3A011_WORK_AREA(산림자원조성사업정보.작업면적)`) %>% 
  # rename("forest_tending" = `PMS3A011_FRCMB_NM1(숲가꾸기내역.작업종1)`) %>% 
  relocate(work_area, .after = 2)

data_combined = rbind(data_1_new, data_2_new)





## 🟨 각 지역별 연도별 숲가꾸기 데이터 합산  =======================================================================================
# 각 지역별로 forest_tending에 따라 work_area를 합산한 새로운 데이터프레임 생성
data_aggregated <- data_combined %>%
  group_by(regions, year) %>%
  summarise(total_work_area = sum(work_area, na.rm = TRUE)) %>%
  ungroup()

# data_combined %>% filter(regions == "강원도" & year == "2019") %>% pull(3) %>% sum

# 결과 확인
data_aggregated %>% View



## 🟦 연보데이터 추출  =======================================================================================
### 🟧 데이터 로드 ===================================================================================
# 2019 데이터 -> 2020 연보
path_2019_1 = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/조림/수종별 조림실적Plantation forest by tree species/5.04~23_활엽수, 침엽수/2020_YRBK_0050040601.csv"
path_2019_2 = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/조림/수종별 조림실적Plantation forest by tree species/5.04~23_활엽수, 침엽수/2020_YRBK_0050040602.csv"
data_2019_1 = read.csv(path_2019_1) %>% select(1:4)
data_2019_2 = read.csv(path_2019_2) %>% select(1:4)


# 2020 데이터 -> 2021 연보
path_2020_1 = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/조림/수종별 조림실적Plantation forest by tree species/5.04~23_활엽수, 침엽수/2021_YRBK_0051040601.csv"
path_2020_2 = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/조림/수종별 조림실적Plantation forest by tree species/5.04~23_활엽수, 침엽수/2021_YRBK_0051040602.csv"
data_2020_1 = read.csv(path_2020_1) %>% select(1:4)
data_2020_2 = read.csv(path_2020_2) %>% select(1:4)

# 2021 데이터 -> 2022 연보
path_2021_1 = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/조림/수종별 조림실적Plantation forest by tree species/5.04~23_활엽수, 침엽수/2022_YRBK_0052040501.csv"
path_2021_2 = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/조림/수종별 조림실적Plantation forest by tree species/5.04~23_활엽수, 침엽수/2022_YRBK_0052040502.csv"
data_2021_1 = read.csv(path_2021_1) %>% select(1:4)
data_2021_2 = read.csv(path_2021_2) %>% select(1:4)




### 🟧 합계 면적 ===================================================================================
names(data_2019_1)
names(data_2019_2)
names(data_2020_1)
names(data_2020_2)
names(data_2021_1)
names(data_2021_2)

data_2019 = data_2019_1 %>% 
  mutate(year = "2019")
data_2019[[4]] = data_2019_1[[4]] + data_2019_2[[4]]
names(data_2019)[3] = "regions"
names(data_2019)[4] = "yb_area"

data_2020 = data_2020_1 %>% 
  mutate(year = "2020")
data_2020[[4]] = data_2020_1[[4]] + data_2020_2[[4]]
names(data_2020)[3] = "regions"
names(data_2020)[4] = "yb_area"


data_2021 = data_2021_1 %>% 
  mutate(year = "2021")
data_2021[[4]] = data_2021_1[[4]] + data_2021_2[[4]]
names(data_2021)[3] = "regions"
names(data_2021)[4] = "yb_area"


### 🟧 데이터 합치기 ===================================================================================
combined_data = list(data_2019, data_2020, data_2021) %>% bind_rows
View(combined_data )




### 🟧 Extract data ===================================================================================
combined_data_2 = combined_data %>% 
  filter(regions %in% data_aggregated$regions) %>% 
  select(-2) %>% 
  select(-1) %>% 
  relocate(year)




# 🟥 데이터 합치기  ===================================================================================
# 데이터 체크
data_aggregated
combined_data_2

names(data_aggregated)
names(combined_data_2)

# 두 데이터프레임 병합 (regions와 forest_tending을 기준으로)
combined_data_new <- left_join(data_aggregated, combined_data_2,
                                by = c("regions", "year"), 
                                suffix = c("_digital", "_yb"))
View(combined_data_new )

names(combined_data_2)


# 🟥 ha로  unit 바꾸기  ===================================================================================
combined_data_new_2 = combined_data_new %>% 
  rename(total_work_area_digital = total_work_area) %>% 
  rename(total_work_area_yb = yb_area) %>% 
  mutate(total_work_area_digital_ha = total_work_area_digital / 10000) %>% 
  mutate(diff_abs = abs(total_work_area_digital_ha - total_work_area_yb))





# 🟥 export  ===================================================================================
path_save = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/5.디지털숲가꾸기/Exported"
write.csv(combined_data_new_2, paste0(file.path(path_save, "forestation_area"), ".csv"), row.names = F)






