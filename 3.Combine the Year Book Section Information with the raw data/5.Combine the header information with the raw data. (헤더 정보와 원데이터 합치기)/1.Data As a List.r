# 🟥 raw 데이터 정리(리스트화) #####################################################################################################
## 🟧 data load #####################################################################################################
path_data = "/Users/Ido/Documents/DataAnalysis/KFS_Timeseries/0.Raw Data/DB_20240415/TB_NDI_MRV_STTST_YRBK_OF_FRSTR_DATA_20240415.xlsx"
data = read.xlsx(path_data)

path_yb = "/Users/Ido/Documents/DataAnalysis/KFS_Timeseries/2.Rearrange YB names/4.Added YB ID.csv"
yb = read.csv(path_yb)




## 🟧 subset by ID #####################################################################################################
id = yb$ID
data = data %>% filter(연보.ID %in% id)
dim(data)
length(id)

yb$Categorized_L3_New %>% unique





## 🟧 데이터 체크 #####################################################################################################
# id %>% filter(연보.ID =="YRBK_0049040802")
# header %>% filter(연보.ID =="YRBK_0049040802")
# data %>% filter(연보.ID =="YRBK_0049040802") %>% View
# test %>% filter(연보.ID == "YRBK_0049040802")








## 🟧 연보ID에 따라 데이터 리스트화 #####################################################################################################
# split 함수를 사용하여 "연보.ID"로 그룹화
grouped_data <- split(data, data$연보.ID)

# 각 "연보.ID"를 리스트의 이름으로 설정
result_list <- lapply(names(grouped_data), function(id) {
  return(grouped_data[[id]])
})
names(result_list) <- names(grouped_data)

length(result_list) == data$연보.ID %>% unique %>% length




## 🟧 임시 save #####################################################################################################
path_save = "/Users/Ido/Documents/DataAnalysis/KFS_Timeseries/3.Data with HDR"
saveRDS(result_list, paste0(path_save, "/1.Raw Data as a list.rds"))
# data.list = readRDS(list.files(path_save, pattern = "Raw Data as", full.names=T)) 
# names(result_list)
# result_list[[1]] %>% View

# result_list[["YRBK_0031032803"]] %>% View

