# 🟥 데이터 로드 =================================================================
path_final = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/7.최종데이터/data_final_new.xlsx"
path_forestation = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/조림/수종별 조림실적Plantation forest by tree species/Combined/5.Combined_final.xlsx"

final = read.xlsx(path_final)
forestation = read.xlsx(path_forestation)



# 🌫️check data   ==============================================================================================
names(final)



# 🟪 subset by year  =======================================================================================
forestation_sub = forestation %>% filter(구분 %in% final$year)
forestation_sub$구분 == final$year
dim(forestation_sub)
dim(final)


# 🟪 check total  =======================================================================================
names(forestation_sub)
names(final)
final_2 = final %>% 
  cbind(., 
        plant_total_new = forestation_sub$합계_수량_New,
        plant_ch_new = forestation_sub[["침엽수_합계_계산"]],
        plant_hw_new = forestation_sub[["활엽수_합계_계산"]]) %>% 
  relocate(plant_total_new, .after = plant_total) %>% 
  relocate(plant_ch_new, .after = plant_ch) %>% 
  relocate(plant_hw_new, .after = plant_hw) 




data_1$year %>% class
data_1_sub = data_1 %>% filter(year  %in% c("2020", "2021"))
dim(data_1_sub )



## 🟪 check the data  =======================================================================================
View(data_1_sub)








# 🌫️ data2  ==============================================================================================
names(data_2)
View(data_2)



## 🟪 check year ======================================================================================
data_2[,1] %>% unique




## 🟪 Extract only 2020~2021 ======================================================================================
# change name
colnames(data_2)[1] = "year"

# class
class(data_2[,1])

# check
unique(data_2[,1])

# extract
data_2_2022 = data_2 %>% filter(year %in% "2022")
dim(data_2_2022)
data_2_2022[,1] %>% unique
View(data_2_2022 )


# check objectives
data_2_2022$`PMS3A011_COMPO_OBJ_NM(산림자원조성사업정보.조성목적명)` %>% unique
View(data_2_2022)

# the total number of seedling?







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









































