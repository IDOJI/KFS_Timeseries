# 🟥 데이터 로드 ===========================================================================================
path_yb_data = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/0.Raw Data/DB_20240415/TB_NDI_MRV_STTST_YRBK_OF_FRSTR_DATA_20240415.xlsx"
path_yb_hdr = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/0.Raw Data/DB_20240415/TB_NDI_MRV_STTST_YRBK_OF_FRSTR_HDR_20240415.xlsx"

yb_data = read.xlsx(path_yb_data) %>% rename(yb_id = "연보.ID")
yb_hdr = read.xlsx(path_yb_hdr) %>% rename(yb_id = "연보.ID")



# 🟨 data selection ===========================================================================================
id = "YRBK_00520202"
yb_data_sub = yb_data %>% filter(yb_id == id)
View(yb_data_sub)




# 🟦 loading data ===========================================================================================
path_data = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/연도별 산림면적 및 임목축적Forest Land Area and Growing Stock by Year/2022_YRBK_00520202.csv"
data = read.csv(path_data)
View(data)

data_sub_1 = data[3:7]
# data_sub_1$연도_year %>% as.numeric %>% max
# View(data_sub_1)
data_sub_2 = data[8:11] %>% 
  cbind(year = (1983:2023)[1:(nrow(.))] %>% as.character, .) %>% 
  slice(-nrow(.))
# View(data_sub_2)




# 🟨 change colnames ===========================================================================================
col_names = c("year", "forest_area___ha", "growing_stock___m3", "forest_area___%", "growing_stock_per_ha")
names(data_sub_1) = names(data_sub_2) = col_names
data_combined = rbind(data_sub_1, data_sub_2)
View(data_combined )



# 🟨 Check data ===========================================================================================
data_combined[1:3,] %>% View


# 🟨 new data ===========================================================================================
path_data = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/연도별 산림면적 및 임목축적Forest Land Area and Growing Stock by Year/directly selected data.xlsx"
data = read.xlsx(path_data)
names(data) = col_names
View(data)

# 🟨 export ===========================================================================================
path_save = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/연도별 산림면적 및 임목축적Forest Land Area and Growing Stock by Year"
file_name = "growing stock.xlsx"
file_path = file.path(path_save, file_name)
write.xlsx(data, file_path)


















