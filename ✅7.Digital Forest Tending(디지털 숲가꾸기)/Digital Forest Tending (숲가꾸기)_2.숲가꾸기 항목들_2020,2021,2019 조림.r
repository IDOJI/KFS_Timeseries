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



# 결과 확인
data_aggregated %>% View



## 🟦 연보데이터 추출  =======================================================================================
### 🟧 데이터 로드 ===================================================================================
# 2019년도 데이터 -> 2020 연보를 의미
path_yb_2019 = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/숲가꾸기/숲 가꾸기Forest tending/2020_YRBK_00500409.csv"
yb_2019 = read.csv(path_yb_2019) %>% relocate(year, .after = 3)
View(yb_2019)





### 🟧 Check names ===================================================================================
# names(yb_2019)
names(yb_2019)[3] = "classification"
names(yb_2019)[6] = "조림지가꾸기"
names(yb_2019)[9] = "어린나무가꾸기"
names(yb_2019)[10] = "큰나무가꾸기"


### 🟧 Extract data ===================================================================================
yb_2019_sub = yb_2019 %>% 
  select(classification, 조림지가꾸기, 어린나무가꾸기, 큰나무가꾸기) %>% 
  filter(classification %in% data_aggregated$regions)
View(yb_2019_sub)


### 🟧 데이터재구성 ===================================================================================
yb_2019_sub %>% head

# 데이터 변환 함수
transform_data <- function(data, year_value) {
  data %>%
    pivot_longer(cols = c("조림지가꾸기", "어린나무가꾸기", "큰나무가꾸기"),
                 names_to = "forest_tending",
                 values_to = "total_work_area") %>%
    rename(regions = classification) %>%
    select(regions, forest_tending, total_work_area)
}

yb_2019_sub_2 = transform_data(yb_2019_sub)




# 🟥 지역이름 비교  ===================================================================================
sum(yb_2019_sub_2$regions %in% data_aggregated$regions) == nrow(yb_2019_sub_2)
sum(data_aggregated$regions %in% yb_2019_sub_2$regions) == nrow(data_aggregated)




# 🟥 데이터 합치기  ===================================================================================
# 데이터 체크
data_aggregated
yb_2019_sub_2

names(data_aggregated)
names(yb_2019_sub_2)

# 두 데이터프레임 병합 (regions와 forest_tending을 기준으로)
combined_data_2019 <- left_join(data_aggregated, yb_2019_sub_2,
                                by = c("regions", "forest_tending"), 
                                suffix = c("_digital", "_yb")) %>% 
  filter(forest_tending %in% c("큰나무가꾸기", "어린나무가꾸기"))



# 🟥 ha로  unit 바꾸기  ===================================================================================
combined_data_2019 = combined_data_2019 %>% 
  mutate(total_work_area_digital_ha = total_work_area_digital / 10000) %>% 
  mutate(diff_abs = abs(total_work_area_digital_ha - total_work_area_yb))

View(combined_data_2019)
data_aggregated %>% filter(regions == "강원도")
yb_2019_sub_2 %>% filter(regions == "강원도")
yb_2019 %>% filter(classification == "강원도") %>% View

combined_data_2019$regions %>% table




# 🟥 항목별  ===================================================================================
results = list()
results$tending_2019_young = combined_data_2019 %>% 
  filter(forest_tending == "어린나무가꾸기")
results$tending_2019_big = combined_data_2019 %>% 
  filter(forest_tending == "큰나무가꾸기")
# results$tending_2019_young %>% filter(regions == "인천광역시")


# 🟥 export  ===================================================================================
path_save = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/5.디지털숲가꾸기/Exported"
r = sapply(seq_along(results), function(i){
  
  write.csv(results[[i]], paste0(file.path(path_save, names(results)[i]), ".csv"), row.names = F)  
  
})







