# 🟥 data load ##########################################################################
path_data = "/Users/Ido/Documents/DataAnalysis/KFS_Timeseries/3.Data with HDR/8.3.Combined Data.rds"
data = readRDS(path_data)

raw_data_path = "/Users/Ido/Documents/DataAnalysis/KFS_Timeseries/0.Raw Data/DB_20240415/TB_NDI_MRV_STTST_YRBK_OF_FRSTR_DATA_20240415.xlsx"
raw = read.xlsx(raw_data_path)



# 🟥 데이터 체크 ###########################################################################
## 🟧 숲가꾸기 ==================================================================================
### 🟨 "YRBK_0034040602"  ======================================================================
id = "YRBK_0034040602"
# Raw -> 존재
raw_selected = raw %>% filter(연보.ID == id) %>% remove_na_columns
raw_selected$열000

# refined
data_selected = data[names(data) == id][[1]]
data_selected$구분


# 