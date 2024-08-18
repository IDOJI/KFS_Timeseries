# 🟥 데이터 로드 =================================================================
path_data_1 = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/5.디지털숲가꾸기/숲가꾸기(2020_2024)_1.xlsx"
path_data_2 = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/5.디지털숲가꾸기/조림(2020_2024).xlsx"

# 숲가꾸기 1 (2020 ~ 2022)
data_1 = read.xlsx(path_data_1)

# 조림
data_2 = read.xlsx(path_data_2)



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



## 🟨 각 지역별 숲가꾸기 데이터 합산  =======================================================================================
# 각 지역별로 forest_tending에 따라 work_area를 합산한 새로운 데이터프레임 생성
data_aggregated <- data_1_new %>%
  group_by(regions, forest_tending) %>%
  summarise(total_work_area = sum(work_area, na.rm = TRUE)) %>%
  ungroup()

# 결과 확인
data_aggregated %>% View
unique(data_1_new$regions) %in%  data_aggregated$regions
data_aggregated$regions %>% unique









# 🌫 data3 ===============================================================================================
## 🟪 Check the data ===============================================================================
# View(data_3)



## 🟪 change colname ===============================================================================
data_3[,1] %>% unique
colnames(data_3)[1] = "year"


## 🟪 subset 2020, 2021 ===============================================================================
data_3_sub = data_3 %>% filter(year %in% c("2020", "2021"))
View(data_3_sub)


## 🟪 Group Field ===============================================================================
### 🟧 Check 3 col  ==================================================================================================
#  GROUP FIELD : 필지
data_3_sub[,3]
colnames(data_3_sub)[3] = "GROUP_FIELD"

data_3_sub[,3] %>% unique %>% length





### 🟧 Extract each region  ==================================================================================================
#### 🟩 save ====================================================================================
data.list = list()
data_sub.list = list()



#### 🟩 1.경기도 ====================================================================================
k = 1
key = "경기도 "

target = data_3_sub
data.list[[k]] = exclude_by_string(target, 3, key)
data_sub.list[[k]] = filter_by_string(target, 3, key)
names(data_sub.list)[k] = names(data.list)[k] = key




#### 🟩 2.경상남도 ====================================================================================
k = 2
key = "경상남도 "

target = data.list[[k-1]]
data.list[[k]] = exclude_by_string(target, 3, key)
data_sub.list[[k]] = filter_by_string(target, 3, key)
names(data_sub.list)[k] = names(data.list)[k] = key
data.list[[k]][,3] %>% unique




#### 🟩 3.충청남도 ====================================================================================
k = 3
key = "충청남도 "

target = data.list[[k-1]]
data.list[[k]] = exclude_by_string(target, 3, key)
data_sub.list[[k]] = filter_by_string(target, 3, key)
names(data_sub.list)[k] = names(data.list)[k] = key
data.list[[k]][,3] %>% unique




#### 🟩 4.전라남도 ====================================================================================
k = 4
key = "전라남도 "

target = data.list[[k-1]]
data.list[[k]] = exclude_by_string(target, 3, key)
data_sub.list[[k]] = filter_by_string(target, 3, key)
names(data_sub.list)[k] = names(data.list)[k] = key
data.list[[k]][,3] %>% unique



#### 🟩 5.충청북도 ====================================================================================
k = 5
key = "충청북도 "

target = data.list[[k-1]]
data.list[[k]] = exclude_by_string(target, 3, key)
data_sub.list[[k]] = filter_by_string(target, 3, key)
names(data_sub.list)[k] = names(data.list)[k] = key
data.list[[k]][,3] %>% unique




#### 🟩 6.경상북도 ====================================================================================
k = 6
key = "경상북도 "

target = data.list[[k-1]]
data.list[[k]] = exclude_by_string(target, 3, key)
data_sub.list[[k]] = filter_by_string(target, 3, key)
names(data_sub.list)[k] = names(data.list)[k] = key
data.list[[k]][,3] %>% unique



#### 🟩 7.대전광역시 ====================================================================================
k = 7
key = "대전광역시 "

target = data.list[[k-1]]
data.list[[k]] = exclude_by_string(target, 3, key)
data_sub.list[[k]] = filter_by_string(target, 3, key)
names(data_sub.list)[k] = names(data.list)[k] = key
data.list[[k]][,3] %>% unique



#### 🟩 8.전라북도 ====================================================================================
k = 8
key = "전라북도 "

target = data.list[[k-1]]
data.list[[k]] = exclude_by_string(target, 3, key)
data_sub.list[[k]] = filter_by_string(target, 3, key)
names(data_sub.list)[k] = names(data.list)[k] = key
data.list[[k]][,3] %>% unique




#### 🟩 9.강원도 ====================================================================================
k = 9
key = "강원도 "

target = data.list[[k-1]]
data.list[[k]] = exclude_by_string(target, 3, key)
data_sub.list[[k]] = filter_by_string(target, 3, key)
names(data_sub.list)[k] = names(data.list)[k] = key
data.list[[k]][,3] %>% unique



#### 🟩 10.울산광역시 ====================================================================================
k = 10
key = "울산광역시 "

target = data.list[[k-1]]
data.list[[k]] = exclude_by_string(target, 3, key)
data_sub.list[[k]] = filter_by_string(target, 3, key)
names(data_sub.list)[k] = names(data.list)[k] = key
data.list[[k]][,3] %>% unique




#### 🟩 11.서울특별시 ====================================================================================
k = 11
key = "서울특별시 "

target = data.list[[k-1]]
data.list[[k]] = exclude_by_string(target, 3, key)
data_sub.list[[k]] = filter_by_string(target, 3, key)
names(data_sub.list)[k] = names(data.list)[k] = key
data.list[[k]][,3] %>% unique




#### 🟩 12.부산광역시 ====================================================================================
k = 12
key = "부산광역시 "

target = data.list[[k-1]]
data.list[[k]] = exclude_by_string(target, 3, key)
data_sub.list[[k]] = filter_by_string(target, 3, key)
names(data_sub.list)[k] = names(data.list)[k] = key
data.list[[k]][,3] %>% unique





#### 🟩 13.인천광역시 ====================================================================================
k = 13
key = "인천광역시 "

target = data.list[[k-1]]
data.list[[k]] = exclude_by_string(target, 3, key)
data_sub.list[[k]] = filter_by_string(target, 3, key)
names(data_sub.list)[k] = names(data.list)[k] = key
data.list[[k]][,3] %>% unique




#### 🟩 14.제주특별 ====================================================================================
k = 14
key = "제주특별자치도 "

target = data.list[[k-1]]
data.list[[k]] = exclude_by_string(target, 3, key)
data_sub.list[[k]] = filter_by_string(target, 3, key)
names(data_sub.list)[k] = names(data.list)[k] = key
data.list[[k]][,3] %>% unique




#### 🟩 15.대구광역시 ====================================================================================
k = 15
key = "대구광역시 "

target = data.list[[k-1]]
data.list[[k]] = exclude_by_string(target, 3, key)
data_sub.list[[k]] = filter_by_string(target, 3, key)
names(data_sub.list)[k] = names(data.list)[k] = key
data.list[[k]][,3] %>% unique




#### 🟩 16.광주광역시 ====================================================================================
k = 16
key = "광주광역시 "

target = data.list[[k-1]]
data.list[[k]] = exclude_by_string(target, 3, key)
data_sub.list[[k]] = filter_by_string(target, 3, key)
names(data_sub.list)[k] = names(data.list)[k] = key
data.list[[k]][,3] %>% unique




#### 🟩 17.세종특별자치시 ====================================================================================
k = 17
key = "세종특별자치시 "

target = data.list[[k-1]]
data.list[[k]] = exclude_by_string(target, 3, key)
data_sub.list[[k]] = filter_by_string(target, 3, key)
names(data_sub.list)[k] = names(data.list)[k] = key
data.list[[k]][,3] %>% unique
dim(data.list[[k]])
View(data.list[[k]])



#### 🟩 remove space ====================================================================================
names(data_sub.list)
names(data_sub.list) = gsub(" ", "", names(data_sub.list))


#### 🟩 Export ====================================================================================
path_save = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/5.디지털숲가꾸기/Exported"

data_2020.list = list()
data_2021.list = list()

for(i in seq_along(data_sub.list)){
  
  ith_region = names(data_sub.list)[i]
  
  ith_name_2020 = ith_region %>% paste0("디지털숲가꾸기_2020_", ., ".csv")
  ith_name_2021 = ith_region %>% paste0("디지털숲가꾸기_2021_", ., ".csv")
  
  ith_data = data_sub.list[[i]] 
  ith_data_2020 = ith_data %>% filter(year == "2020")
  ith_data_2021 = ith_data %>% filter(year == "2021")
  
  
  ith_data_2020_new = list()
  ith_data_2020_new[["Group_Field"]] = ith_region
  ith_data_2020_new[["Seeling"]] = sum(ith_data_2020$`PMS3A013_BONSU(산림자원조성사업수종정보.본수)`, na.rm = T)
  
  
  ith_data_2021_new = list()
  ith_data_2021_new[["Group_Field"]] = ith_region
  ith_data_2021_new[["Seeling"]] = sum(ith_data_2021$`PMS3A013_BONSU(산림자원조성사업수종정보.본수)`, na.rm = T)
  
  data_2020.list[[i]] = bind_cols(ith_data_2020_new)
  data_2021.list[[i]] = bind_cols(ith_data_2021_new)
  
  write.csv(bind_cols(ith_data_2020_new), file.path(path_save, paste0("Summed_", ith_name_2020)), row.names = F)
  write.csv(bind_cols(ith_data_2021_new), file.path(path_save, paste0("Summed_", ith_name_2021)), row.names = F)
  
  write.csv(ith_data_2020, file.path(path_save, ith_name_2020), row.names = F)
  write.csv(ith_data_2021, file.path(path_save, ith_name_2021), row.names = F)
  
}

names(data_2020.list) = names(data_sub.list)
names(data_2021.list) = names(data_sub.list)


test= data_sub.list$서울특별시
test[[3]]
test$`PMS3A013_BONSU(산림자원조성사업수종정보.본수)` %>% sum




# 🌫️ 임업통계연보 ================================================================================
## 🟧 Load data ================================================================================
path_2020_1 = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/조림/수종별 조림실적Plantation forest by tree species/5.04~23_활엽수, 침엽수/2020_YRBK_0050040601.csv"
path_2020_2 = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/조림/수종별 조림실적Plantation forest by tree species/5.04~23_활엽수, 침엽수/2020_YRBK_0050040602.csv"

path_2021_1 = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/조림/수종별 조림실적Plantation forest by tree species/5.04~23_활엽수, 침엽수/2021_YRBK_0051040601.csv"
path_2021_2 = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/조림/수종별 조림실적Plantation forest by tree species/5.04~23_활엽수, 침엽수/2021_YRBK_0051040602.csv"


data_2020_1 = read.csv(path_2020_1)
data_2020_2 = read.csv(path_2020_2)
data_2021_1 = read.csv(path_2021_1)
data_2021_2 = read.csv(path_2021_2)



## 🟧 지역별 합치기 ============================================================================================
### 🟪 2020 ===========================================================================================
region_1 = data_2020_1[[3]][-c(1:5)]
region_2 = data_2020_2[[3]][-c(1:5)]
all(region_1 == region_2)

data_2020_1_sub = data_2020_1[-c(1:5), c(3,5)]
data_2020_2_sub = data_2020_2[-c(1:5), c(3,5)]

if(all(data_2020_1_sub$구분 ==  data_2020_2_sub$구분)){
  
  data_2020_combined = data_2020_1_sub
  data_2020_combined$합계_본수 = data_2020_1_sub[[2]] + data_2020_2_sub[[2]]
  
}



### 🟪 2021 ===========================================================================================
region_1 = data_2021_1[[3]][-c(1:5)]
region_2 = data_2021_2[[3]][-c(1:5)]
all(region_1 == region_2)

data_2021_1_sub = data_2021_1[-c(1:5), c(3,5)]
data_2021_2_sub = data_2021_2[-c(1:5), c(3,5)]

if(all(data_2021_1_sub$구분 ==  data_2021_2_sub$구분)){
  
  data_2021_combined = data_2021_1_sub
  data_2021_combined$합계_본수 = data_2021_1_sub[[2]] + data_2021_2_sub[[2]]
  
}





# 🌫️ 데이터 합계 비교 ==============================================================================
## 🟧 이름 체크  ===========================================================================================
data_2020_combined_sub = data_2020_combined %>% filter(data_2020_combined$구분 %in% names(data_2020.list))
data_2021_combined_sub = data_2021_combined %>% filter(data_2021_combined$구분 %in% names(data_2021.list))

data_2020_combined_sub[[1]]
data_2021_combined_sub[[1]]



## 🟧 리스트 데이터프레임으로  ===========================================================================================
data_2020.df = bind_rows(data_2020.list)
data_2021.df = bind_rows(data_2021.list)



## 🟧 데이터 합치기  ===========================================================================================
names(data_2021_combined_sub)[1] = names(data_2020_combined_sub)[1] = "Group_Field"
combined_data_2020.df = merge(data_2020.df, data_2020_combined_sub, by = "Group_Field") %>% 
  arrange(Group_Field)

combined_data_2021.df = merge(data_2021.df, data_2021_combined_sub, by = "Group_Field") %>% 
  arrange(Group_Field)



## 🟧 천그루 단위 보정  ===========================================================================================
combined_data_2020.df$임통_본수_New = combined_data_2020.df$합계_본수 * 1000 
combined_data_2021.df$임통_본수_New = combined_data_2021.df$합계_본수 * 1000


## 🟧 Export  ===========================================================================================
write.csv(combined_data_2020.df, file.path(path_save, "2020.csv"), row.names=F)
write.csv(combined_data_2021.df, file.path(path_save, "2021.csv"), row.names=F)









































