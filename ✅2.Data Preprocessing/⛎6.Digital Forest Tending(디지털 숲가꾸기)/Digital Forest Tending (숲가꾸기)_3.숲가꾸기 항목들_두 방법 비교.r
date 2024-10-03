# 🟥 데이터 로드 ==================================================================================
path_big = c("/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/5.디지털숲가꾸기/Exported/tending_2021_big.csv",
             "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/5.디지털숲가꾸기/Exported/tending_2020_big.csv",
             "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/5.디지털숲가꾸기/Exported/tending_2019_big.csv")

path_you <- c("/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/5.디지털숲가꾸기/Exported/tending_2021_young.csv",
              "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/5.디지털숲가꾸기/Exported/tending_2020_young.csv",
              "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/5.디지털숲가꾸기/Exported/tending_2019_young.csv")

data_big = lapply(path_big, read.csv) %>% bind_rows() %>% arrange(year) %>% filter(regions != "기타")
data_you = lapply(path_you, read.csv) %>% bind_rows() %>% arrange(year) %>% filter(regions != "기타")
data_you$regions %>% table

View(data_you)

test = read.csv("/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/숲가꾸기/숲 가꾸기Forest tending/2020_YRBK_00500409.csv")
test$NA_구분
data_you %>% filter(regions == "인천광역시")

data_you$regions %>% table
data_big$regions %>% table




# 🟩 Bland-Altman test ==================================================================================
## 🌫️ young =======================================================================================
path_save = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/5.디지털숲가꾸기/Exported_plots"

names(data_you)
# 지역별로 표준화 수행
data_you <- data_you %>%
  group_by(regions) %>%
  mutate(across(all_of(c("total_work_area_yb", "total_work_area_digital_ha")), 
                ~ (.-mean(.))/sd(.), 
                .names = "{.col}_stand")) %>%
  ungroup
BA_test(data_you, path_save, "BA-test_young.png", "어린나무가꾸기",
        width = 10, 
        height = 7, 
        label_size = 15,
        title_size = 50, 
        xlab_size = 35, 
        ylab_size = 35,
        dig_col = "total_work_area_digital_ha_stand",
        yb_col = "total_work_area_yb_stand")



## 🌫 big =======================================================================================
path_save = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/5.디지털숲가꾸기/Exported_plots"
names(data_big)
data_big = data_big %>% group_by(regions) %>%
  mutate(across(all_of(c("total_work_area_yb", "total_work_area_digital_ha")), 
                ~ (.-mean(.))/sd(.), 
                .names = "{.col}_stand")) %>%
  ungroup
BA_test(data_big, path_save, "BA-test_big.png",  "큰나무가꾸기",
        width = 10, 
        height = 7, 
        label_size = 15,
        title_size = 50, 
        xlab_size = 35, 
        ylab_size = 35,
        dig_col = "total_work_area_digital_ha_stand",
        yb_col = "total_work_area_yb_stand")



## 🌫 forestation =======================================================================================
path_data_forest = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/5.디지털숲가꾸기/Exported/forestation_area.csv"
path_save = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/5.디지털숲가꾸기/Exported_plots"
data_forest = read.csv(path_data_forest) %>% filter(regions != "기타")
names(data_forest)
data_forest = data_forest %>% group_by(regions) %>%
  mutate(across(all_of(c("total_work_area_yb", "total_work_area_digital_ha")), 
                ~ (.-mean(.))/sd(.), 
                .names = "{.col}_stand")) %>%
  ungroup
BA_test(data_forest, path_save, "BA-test_forestation.png",  "조림면적(활엽수+침엽수)",
        width = 10, 
        height = 7, 
        label_size = 15,
        title_size = 50, 
        xlab_size = 35, 
        ylab_size = 35,
        dig_col = "total_work_area_digital_ha_stand",
        yb_col = "total_work_area_yb_stand")






