# 🟥 데이터 로드 ==================================================================================
path_big = c("/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/5.디지털숲가꾸기/Exported/tending_2021_big.csv",
             "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/5.디지털숲가꾸기/Exported/tending_2020_big.csv",
             "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/5.디지털숲가꾸기/Exported/tending_2019_big.csv")

path_you <- c("/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/5.디지털숲가꾸기/Exported/tending_2021_young.csv",
              "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/5.디지털숲가꾸기/Exported/tending_2020_young.csv",
              "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/5.디지털숲가꾸기/Exported/tending_2019_young.csv")

data_big = lapply(path_big, read.csv) %>% bind_rows() %>% arrange(year)
data_you = lapply(path_you, read.csv) %>% bind_rows() %>% arrange(year)
data_you$regions %>% table

View(data_you)

test = read.csv("/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/숲가꾸기/숲 가꾸기Forest tending/2020_YRBK_00500409.csv")
test$NA_구분
data_you %>% filter(regions == "인천광역시")


# 🟩 Bland-Altman test ==================================================================================
## 🌫️ young =======================================================================================
path_save = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/5.디지털숲가꾸기/Exported_plots"
BA_test(data_you, path_save, "BA-test_young.png", "어린나무가꾸기")



## 🌫 big =======================================================================================
path_save = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/5.디지털숲가꾸기/Exported_plots"
BA_test(data_big, path_save, "BA-test_big.png",  "큰나무가꾸기")






