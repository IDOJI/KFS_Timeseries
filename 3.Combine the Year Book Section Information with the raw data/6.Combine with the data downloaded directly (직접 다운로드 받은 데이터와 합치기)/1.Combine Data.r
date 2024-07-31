# 🟥 Load data ###########################################################################################
path_save = "/Users/Ido/Documents/DataAnalysis/KFS_Timeseries/3.Data with HDR"
combined_data = readRDS("/Users/Ido/Documents/DataAnalysis/KFS_Timeseries/3.Data with HDR/8.2.Combined Data.rds")


yb = read.csv("/Users/Ido/Documents/DataAnalysis/KFS_Timeseries/2.Rearrange YB names/4.Added YB ID.csv")
"YRBK_00520609" %in% names(combined_data)




# 🟥 (완료)학부연구생들 데이터 수정 ###########################################################################################
# path_save = "/Users/Ido/Documents/DataAnalysis/KFS_Timeseries/3.Data with HDR/Directly Downloaded Data/wrong_modified"
# path_wrong = "/Users/Ido/Documents/DataAnalysis/KFS_Timeseries/3.Data with HDR/Directly Downloaded Data/wrong"
# 
# 
# ## 🟧 data1 =============================================================================================================
# file_name = "YRBK_00480603_after.csv"
# data_1 = read.csv(file.path(path_wrong, file_name))
# # View(data_1)
# data_1_new = data_1[,-c(1:2)]
# # View(data_1_new)
# write.csv(data_1_new, file.path(path_save, file_name), row.names = F)
# 
# 
# 
# 
# ## 🟧 data2 =============================================================================================================
# file_name = "YRBK_00520609_after.csv"
# data_1 = read.csv(file.path(path_wrong, file_name))
# # View(data_1)
# data_1_new = data_1[,-c(1:2)]
# # View(data_1_new)
# write.csv(data_1_new, file.path(path_save, file_name), row.names = F)
# 
# 
# 
# ## 🟧 data3 =============================================================================================================
# file_name = "YRBK_0047060403_after.csv"
# data_1 = read.csv(file.path(path_wrong, file_name))
# # View(data_1)
# data_1_new = data_1[,-c(1:2)]
# # View(data_1_new)
# write.csv(data_1_new, file.path(path_save, file_name), row.names = F)
# 





# 🟥 임목벌채 데이터 ####################################################################################################
## 🟧 데이터 확인 ======================================================================
yb %>% filter(ID=="YRBK_0047060403")

# 다른 항목들 제대로 전처리 되었는지 확인
id_selected = yb %>% filter(Categorized_L3_New %in% "임목벌채 허가실적_Permit of Annual Tree Cutting") %>% pull(ID)
selected_data = combined_data[names(combined_data) %in% id_selected]
# selected_data[[1]] %>% View
# selected_data[[10]] %>% View
# selected_data[[20]] %>% View
# selected_data[[30]] %>% View
# selected_data[[31]] %>% View
# selected_data[[56]] %>% View
years = sapply( selected_data, function(x){
  x$year %>% unique
}) %>% unname %>% sort
print(years)

## 🟧 데이터 확인2 ======================================================================
selected = sapply(combined_data, function(x){
  if(x$Categorized_L3_New %>% unique  == "임목벌채 허가실적_Permit of Annual Tree Cutting"){
    return(x)
  }
})
selected_2 = selected[!sapply(selected, is.null)]
selected_2 $YRBK_00340604 %>% View




## 🟧 추가적인 데이터 로드  ======================================================================
id = "YRBK_0047060403"
id %in% names(combined_data)

# 데이터 수정
downloaded_data = read.csv("/Users/Ido/Documents/DataAnalysis/KFS_Timeseries/3.Data with HDR/Directly Downloaded Data/completed_cutting/YRBK_0047060403_after.csv")
downloaded_data$ID = id
downloaded_data_2 = downloaded_data[-nrow(downloaded_data), ]
downloaded_data_2 %>% dim
downloaded_data_2$행 = seq(0, nrow(downloaded_data_2)-1, by = 1)



## 🟧 "데이터없음"인 셀 추가  ======================================================================
part_1 = downloaded_data_2[1:4,] %>% process_data_na_columns
part_2 = downloaded_data_2[5:nrow(downloaded_data_2),] %>% process_data_na_columns
downloaded_data_2 = rbind(part_1, part_2)
# View(downloaded_data_2)




## 🟧 데이터에 yb 정보 추가 ======================================================================
# merge
filtered_yb = filter(yb, ID==id)
merged_data <- left_join(downloaded_data_2, filtered_yb, by = "ID")
names(merged_data)
# ncol(downloaded_data_2)
# ncol(filtered_yb)
# ncol(merged_data)

merged_data.list = list(merged_data)
names(merged_data.list) = id





## 🟧 데이터 추가 ======================================================================
combined_data_new = c(combined_data, merged_data.list)
# names(combined_data_new) %>% tail
merged_data.list[[1]] %>% View

 
 
 
# 🟥 데이터 내보내기 ####################################################################################################
path_save = "/Users/Ido/Documents/DataAnalysis/KFS_Timeseries/3.Data with HDR"
saveRDS(combined_data_new, paste0(path_save, "/8.3.Combined Data.rds"))

combined_data_new[names(combined_data_new) == "YRBK_00010301"][[1]] %>% View
combined_data_new[names(combined_data_new) == "YRBK_00030301"][[1]] %>% View













