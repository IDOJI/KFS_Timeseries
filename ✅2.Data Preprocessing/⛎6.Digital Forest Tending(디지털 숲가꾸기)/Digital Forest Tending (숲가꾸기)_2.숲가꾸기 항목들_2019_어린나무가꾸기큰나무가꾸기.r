# 🟥 데이터 로드 =================================================================
# path_data_1 = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/5.디지털숲가꾸기/디지털숲가꾸기db(2010~2019) (1)/숲가꾸기(2010~2019)_1.xlsx"
# path_data_2 = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/5.디지털숲가꾸기/디지털숲가꾸기db(2010~2019) (1)/숲가꾸기(2010~2019)_2.xlsx"
path_data_3 = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/5.디지털숲가꾸기/디지털숲가꾸기db(2010~2019) (1)/숲가꾸기(2010~2019)_3.xlsx"


# data_1 = read.xlsx(path_data_1)
# data_2 = read.xlsx(path_data_2)
data_3 = read.xlsx(path_data_3)





# 🌫️data2   ==============================================================================================
## 🟪 check  =======================================================================================
# names(data_2)



## 🟪 year col  =======================================================================================
# colnames(data_2)[1] = "year"
# data_2$year %>% unique



# 🌫️data3   ==============================================================================================
## 🟪 check  =======================================================================================
names(data_3)



## 🟪 year col  =======================================================================================
colnames(data_3)[1] = "year"
data_3$year %>% unique


## 🟪 subset 2019  =======================================================================================
data_3_sub = data_3 %>% filter(year  %in% c("2019"))
dim(data_3_sub)



## 🟪 check the data  =======================================================================================
View(data_3_sub)


## 🟩 check the regions  =======================================================================================
data_3_sub$`GROUP_FIELD(필지)` %>% unique




## 🟩 Group regions  =======================================================================================
data_3_sub <- data_3_sub %>%
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

# data_3_sub %>% filter(regi ons == "서울특별시") %>% View

# 결과 확인
head(data_3_sub)
data_3_sub$regions %>% table %>% as.data.frame()


# 기타?
data_3_sub %>% filter(regions == "기타") %>% View


## 🟨 필요열들 이름 변경  =======================================================================================
data_3_new = data_3_sub %>% 
  rename("work_area" = `PMS3A011_WORK_AREA(산림자원조성사업정보.작업면적)`) %>% 
  rename("forest_tending" = `PMS3A011_FRCMB_NM1(숲가꾸기내역.작업종1)`) %>% 
  relocate(work_area, forest_tending, .after = 2)

data_3_new$forest_tending %>% table
# data_3_new %>% filter(regions == "인천광역시") %>% pull(forest_tending) %>% unique




## 🟨 각 지역별 연도별 숲가꾸기 데이터 합산  =======================================================================================
# 각 지역별로 forest_tending에 따라 work_area를 합산한 새로운 데이터프레임 생성
data_aggregated <- data_3_new %>%
  group_by(regions, forest_tending, year) %>%
  summarise(total_work_area = sum(work_area, na.rm = TRUE)) %>%
  ungroup()



# 결과 확인
data_aggregated %>% View
unique(data_3_new$regions) %in%  data_aggregated$regions
data_aggregated$regions %>% unique

# 검토
data_3_new %>% 
  filter(regions == "강원도" & 
           forest_tending == "어린나무가꾸기" &
              year == "2019") %>% 
  pull(work_area) %>% 
  sum(na.rm = T)

data_aggregated %>% 
  filter(regions == "강원도" &
           forest_tending == "어린나무가꾸기" & 
             year == "2019") %>% 
  pull(total_work_area)


data_aggregated %>% filter(regions == "인천광역시")



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







