# 🟥 데이터 로드 =================================================================
path_data_1 = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/5.디지털숲가꾸기/숲가꾸기(2020_2024)_1.xlsx"
path_data_2 = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/5.디지털숲가꾸기/조림(2020_2024).xlsx"

# 숲가꾸기 1 (2020 ~ 2022)
data_1 = read.xlsx(path_data_1)
View(data_1)


# 조림
data_2 = read.xlsx(path_data_2)

# names(data_1)
# 
# View(data_1)

# 🌫️ data1   ==============================================================================================
## 🟪 check  =======================================================================================
names(data_1)


## 🟪 year col  =======================================================================================
colnames(data_1)[1] = "year"
data_1$year %>% unique


## 🟪 subset 2020, 2021  =======================================================================================
data_1$year %>% class
data_1_sub = data_1 %>% filter(year  %in% c("2020", "2021"))
dim(data_1_sub)



## 🟪 check the data  =======================================================================================
View(data_1_sub)





## 🟩 check the regions  =======================================================================================
data_1_sub$`GROUP_FIELD(필지)` %>% unique




## 🟩 Group regions  =======================================================================================
data_1_sub <- data_1_sub %>%
  mutate(regions = case_when(
    grepl("경기도", `GROUP_FIELD(필지)`) ~ "경기도",
    grepl("경상남도", `GROUP_FIELD(필지)`) ~ "경상남도",
    grepl("충청남도", `GROUP_FIELD(필지)`) ~ "충청남도",
    grepl("전라남도", `GROUP_FIELD(필지)`) ~ "전라남도",
    grepl("충청북도", `GROUP_FIELD(필지)`) ~ "충청북도",
    grepl("경상북도", `GROUP_FIELD(필지)`) ~ "경상북도",
    grepl("대전광역시", `GROUP_FIELD(필지)`) ~ "대전광역시",
    grepl("전라북도", `GROUP_FIELD(필지)`) ~ "전라북도",
    grepl("강원도", `GROUP_FIELD(필지)`) ~ "강원도",
    grepl("강원특별자치도", `GROUP_FIELD(필지)`) ~ "강원도",
    grepl("울산광역시", `GROUP_FIELD(필지)`) ~ "울산광역시",
    grepl("서울특별시", `GROUP_FIELD(필지)`) ~ "서울특별시",
    grepl("부산광역시", `GROUP_FIELD(필지)`) ~ "부산광역시",
    grepl("인천광역시", `GROUP_FIELD(필지)`) ~ "인천광역시",
    grepl("제주특별자치도", `GROUP_FIELD(필지)`) ~ "제주특별자치도",
    grepl("대구광역시", `GROUP_FIELD(필지)`) ~ "대구광역시",
    grepl("광주광역시", `GROUP_FIELD(필지)`) ~ "광주광역시",
    grepl("세종특별자치시", `GROUP_FIELD(필지)`) ~ "세종특별자치시",
    TRUE ~ "기타"
  )) %>% 
  relocate(regions, .after = year)

# data_1_sub %>% filter(regi ons == "서울특별시") %>% View

# 결과 확인
head(data_1_sub)
data_1_sub$regions %>% table %>% as.data.frame()


# 기타?
data_1_sub %>% filter(regions == "기타") %>% View



## 🟨 필요열들 이름 변경  =======================================================================================
data_1_new = data_1_sub %>% 
  rename("work_area" = `PMS3A011_WORK_AREA(산림자원조성사업정보.작업면적)`) %>% 
  rename("forest_tending" = `PMS3A011_FRCMB_NM1(숲가꾸기내역.작업종1)`) %>% 
  relocate(work_area, forest_tending, .after = 2)

data_1_new$forest_tending %>% table





## 🟨 각 지역별 연도별 숲가꾸기 데이터 합산  =======================================================================================
# 각 지역별로 forest_tending에 따라 work_area를 합산한 새로운 데이터프레임 생성
data_aggregated <- data_1_new %>%
  group_by(regions, forest_tending, year) %>%
  summarise(total_work_area = sum(work_area, na.rm = TRUE)) %>%
  ungroup()



# 결과 확인
data_aggregated %>% View
unique(data_1_new$regions) %in%  data_aggregated$regions
data_aggregated$regions %>% unique

# 검토
data_1_new %>% 
  filter(regions == "강원도" & 
           forest_tending == "어린나무가꾸기" &
              year == "2020") %>% 
  pull(work_area) %>% 
  sum(na.rm = T)

data_aggregated %>% 
  filter(regions == "강원도" &
           forest_tending == "어린나무가꾸기" & 
             year == "2020") %>% 
  pull(total_work_area)



## 🟨 연도별 재구성  =======================================================================================
data_aggregated_2021 = data_aggregated %>% filter(year == "2021")
data_aggregated_2020 = data_aggregated %>% filter(year == "2020")
data_aggregated$year




## 🟦 연보데이터 추출  =======================================================================================
### 🟧 데이터 로드 ===================================================================================
# 2021년도 데이터 -> 2022 연보를 의미
path_yb_2021 = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/숲가꾸기/숲 가꾸기Forest tending/2022_YRBK_00520408.csv"
# 2020년도 데이터 -> 2021 연보를 의미
path_yb_2020 = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/숲가꾸기/숲 가꾸기Forest tending/2021_YRBK_00510409.csv"

yb_2021 = read.csv(path_yb_2021) %>% relocate(year, .after = 3)
yb_2020 = read.csv(path_yb_2020) %>% relocate(year, .after = 3)
View(yb_2021)
View(yb_2020)

### 🟧 Check names ===================================================================================
# names(yb_2021)
names(yb_2020)[3] = names(yb_2021)[3] = "classification"
names(yb_2020)[6] = names(yb_2021)[6] = "조림지가꾸기"
names(yb_2020)[9] = names(yb_2021)[9] = "어린나무가꾸기"
names(yb_2020)[10] = names(yb_2021)[10] = "큰나무가꾸기"


### 🟧 Extract data ===================================================================================
View(yb_2021)
View(yb_2021)
yb_2021_sub = yb_2021 %>% 
  select(classification, 조림지가꾸기, 어린나무가꾸기, 큰나무가꾸기) %>% 
  filter(classification %in% data_aggregated$regions)
yb_2020_sub = yb_2020 %>% 
  select(classification, 조림지가꾸기, 어린나무가꾸기, 큰나무가꾸기) %>% 
  filter(classification %in% data_aggregated$regions)



### 🟧 데이터재구성 ===================================================================================
yb_2021_sub %>% head
names(yb_2021_sub)
# 데이터 변환 함수
transform_data <- function(data, year_value) {
  data %>%
    pivot_longer(cols = c("조림지가꾸기", "어린나무가꾸기", "큰나무가꾸기"),
                 names_to = "forest_tending",
                 values_to = "total_work_area") %>%
    rename(regions = classification) %>%
    select(regions, forest_tending, total_work_area)
}

yb_2021_sub_2 = transform_data(yb_2021_sub)
yb_2020_sub_2 = transform_data(yb_2020_sub)

View(yb_2021_sub_2)
yb_2021_sub_2 %>% filter(regions == "서울특별시")




# 🟥 데이터 합치기  ===================================================================================
# 데이터 체크
data_aggregated_2021
data_aggregated_2020

yb_2021_sub_2
yb_2020_sub_2
View(yb_2021)

names(data_aggregated_2021)
names(yb_2021_sub_2)
yb_2021_sub_2$forest_tending %>% table

data_aggregated_2021 %>% filter(regions == "서울특별시")
yb_2021 %>% View

# 두 데이터프레임 병합 (regions와 forest_tending을 기준으로)
combined_data_2021 <- left_join(data_aggregated_2021, yb_2021_sub_2,                                 
                                by = c("regions", "forest_tending"), 
                                suffix = c("_digital", "_yb"))



combined_data_2020 <- left_join(data_aggregated_2020, yb_2020_sub_2,                                 
                                by = c("regions", "forest_tending"), 
                                suffix = c("_digital", "_yb"))



# 1000곱하기
combined_data_2021 = combined_data_2021 %>% 
  mutate(total_work_area_yb_1000ha = total_work_area_yb * 1000) %>% 
  mutate(diff_abs = abs(total_work_area_yb_1000ha - total_work_area_digital))
combined_data_2020 = combined_data_2020 %>% 
  mutate(total_work_area_yb_1000ha = total_work_area_yb * 1000) %>% 
  mutate(diff_abs = abs(total_work_area_yb_1000ha - total_work_area_digital))



# 🟥 export  ===================================================================================
path_save = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/5.디지털숲가꾸기/Exported_2"
write.csv(combined_data_2020, file.path(path_save, "comparison 2020.csv"), row.names = F)
write.csv(combined_data_2021, file.path(path_save, "comparison 2021.csv"), row.names = F)







