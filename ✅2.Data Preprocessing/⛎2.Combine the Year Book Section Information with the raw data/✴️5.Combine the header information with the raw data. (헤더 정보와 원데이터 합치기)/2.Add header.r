# 🟥 데이터 로드 ==================================================================================================
### 🟧 Data load  ===========================================================================================
path_save = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/3.Data with HDR"
# raw data
path_raw_data = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/3.Data with HDR/1.Raw Data as a list.rds"
data = readRDS(path_raw_data) 

# yb id 
path_yb = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/2.Rearrange YB names/4.Added YB ID.csv"
yb = read.csv(path_yb)
  
# yb$Categorized_L3_New %>% unique %>% sort

# header
path_header = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/0.Raw Data/DB_20240415/TB_NDI_MRV_STTST_YRBK_OF_FRSTR_HDR_20240415.xlsx"
header = read_xlsx(path_header) %>% rename("ID" = `연보 ID`) %>% suppressWarnings()

# data[names(data) == ith_id][[1]] %>% View

# 🟥 ID ================================================================================================
### 🟧 ID 추출 ===========================================================================================
yb_id = yb$ID
length(yb_id)


### 🟧 제외 ID  ===========================================================================================
yb = yb %>% filter(ID != "YRBK_0047060403")



### 🟧 이전에 선택된 연보 ID로 데이터 필터링  ===========================================================================================
data = data[names(data) %in% yb_id]

# data$YRBK_00010405 %>% View


header = header %>% filter(ID %in% yb_id)

header$`언어 코드`= NULL




### 🟧 데이터 없는 id  ===========================================================================================
common_id = intersect(names(data), header$ID) %>% intersect(yb_id)
data = data[names(data) %in% common_id]
header = header %>% dplyr::filter(ID %in% common_id)
yb = yb %>% filter(ID %in% common_id)
yb_id = yb$ID
# yb %>% filter(ID == "YRBK_001202030101") %>% View

yb$Categorized_L3 %>% unique

# yb %>% filter(ID == "YRBK_0031032803") %>% View





# 🟥 데이터 합치기 ==================================================================================================
### 🟧 각 ID에 대해 합치기 ===========================================================================================
# Add header
combined.list = list()
no_data_id = c()
more_than_1_id = c()
error_id = c()


yb %>% filter(ID == "YRBK_00520503")
yb %>% filter(ID == "YRBK_00520609")
# yb %>% filter(ID == ith_id)
yb %>% filter(ID == 	"YRBK_0047060403")


# for(i in seq_along(yb_id)){
for(i in 1:length(yb_id)){
  # i=4972
  # i= 2989
  ### 🟩 extract ID =================================================================
  ith_id = yb_id[i]
  # ith_id = "YRBK_00520408"
  # ith_id = error_id
  # ith_id = ID
  # ith_id = "YRBK_00200316"
  # ith_id = error_id
  # ith_id = "YRBK_00060209"
  # ith_id = "YRBK_00510714"
  # ith_id <- "YRBK_00340212"
  # ith_id <- "YRBK_0045020802"
  # ith_id <- "YRBK_00490208"
  # ith_id <- "YRBK_00500208"
  # ith_id <- "YRBK_00510208"
  # ith_id <- "YRBK_00070213"
  # ith_id <- "YRBK_0047060403"
  # ith_id <- "YRBK_0045020701"
  # ith_id <- "YRBK_0045020702"
  # ith_id <- "YRBK_0045020703"
  # ith_id <- "YRBK_0045020704"
  # ith_id <- "YRBK_0045020705"
  # yb %>% filter(ID==ith_id)
  
  ### 🟩 hdr =================================================================
  ith_hdr = tryCatch({
    treat_header(header, ith_id)
  }, warning = function(w) {
    cat("\n", crayon::red("Warning occurred, stopping the loop:\n"), w, "\n")
    stop("Warning encountered")
  }, error = function(e) {
    stop(e)
  })
  # print(ith_hdr)
  # header %>% filter(ID == ith_id) %>% View
  
  
  
  
  
  ### 🟩 data =================================================================
  ith_selected_data = data[names(data) == ith_id]
  
  if(length(ith_selected_data) != 0){
    
    ith_data = ith_selected_data[[1]] %>% as_tibble  
    
    if(is.data.frame(ith_data)){
      
      # Combine data with header
      # ith_data_combined %>% View
      # View(ith_data_combined)
      ith_data_combined <- tryCatch({
        treat_data(ith_data, ith_hdr, yb)
      }, warning = function(w) {
        cat("\n", crayon::red("Warning occurred:\n"), conditionMessage(w), "\n")
        NULL  # 경고가 발생한 경우 NULL 반환
      }, error = function(e) {
        error_id <<- c(error_id, ith_id)  # 에러가 발생한 ith_id를 error_id 벡터에 추가
        cat("\n", crayon::red("Error occurred, continuing the loop:\n"), conditionMessage(e), "\n")
        NULL  # 에러가 발생한 경우 NULL 반환
      })
      
      # View(ith_data_combined)
      # names(ith_data_combined)
      # yb %>% filter(ID == ith_id) %>% View
      
          
      
      # Check the last col
      if(!is.null(ith_data_combined)){
        ith_data_combined_2 = tryCatch({
          check_last_col(ith_data_combined)
        }, warning = function(w) {
          cat("\n", crayon::red("Warning occurred, stopping the loop:\n"), w, "\n")
          stop("Warning encountered")
        }, error = function(e) {
          stop(e)
        })
        
        
        # View(ith_data_combined_2 )
        # 결과 저장
        combined.list[[i]] = tryCatch({
          ith_data_combined_2 %>% remove_empty_na_columns
        }, warning = function(w) {
          cat("\n", crayon::red("Warning occurred, stopping the loop:\n"), w, "\n")
          stop("Warning encountered")
        }, error = function(e) {
          stop(e)
        })
        # View(combined.list[[i]])
        # View(combined.list[[i]])  
      }
      
      
      
      cat("\n", crayon::green(i), crayon::red(ith_id), crayon::green("is done!"),"\n")
      
    } else if(length(ith_data) > 1){
      
      more_than_1_id = c(more_than_1_id, ith_id)
      
      
    } else {
      
      no_data_id = c(no_data_id, ith_id)
      
    }
  } else {
    
    no_data_id = c(no_data_id, ith_id)
    
  }
  
  
  if(length(no_data_id) > 0 && length(yb_id) == i){
    
    cat("\n",crayon::bgMagenta("Check check_id vector"),"\n")
    
  }
  
}



## 🟧 Check Error ===========================================================================================
print(error_id)

# 각 에러 아이디의 Category L3 확인
error_L3 = sapply(error_id, function(x){
  yb %>% filter(ID == x) %>% pull(Categorized_L3_New)
}) %>% unname %>% unique
print(error_L3)



## 🟧 # remove NULL ===========================================================================================
combined.list = remove_null_elements(combined.list)




## 🟧 id가 단 하나만 존재하는지 확인 ===========================================================================================
id_length = sapply(seq_along(combined.list), function(i){
  
  combined.list[[i]][,1] %>% unique %>% length
  
})
# 전부 1개 씩만 존재
unique(id_length)



## 🟧 Add id ===========================================================================================
names(combined.list) = sapply(combined.list, function(x){
  x$ID[1]
})






## 🟧 데이터가 없는 ID ===========================================================================================
print(no_data_id)
write.csv(no_data_id, paste0(path_save, "/8.1.no_data_id.csv"), row.names = F)
print(more_than_1_id)
if(is.null(more_than_1_id)){
  write.csv(no_data_id, paste0(path_save, "/8.1.more_than_1_id.csv"), row.names = F)  
}



## 🟧 열이름이 존재하지 않는 데이터 확인 ===========================================================================================
no_colnames_data_id = sapply(combined.list, function(x){
  if(x %>% names %>% is.na %>% sum > 0 ){
    x[1,1] %>% unlist %>% unname
  }
}) %>% unname %>% unlist


new_combined.list = combined.list[!names(combined.list) %in% no_colnames_data_id]
no_data_combined.list = combined.list[names(combined.list) %in% no_colnames_data_id]
length(no_data_combined.list)





## 🟧 data + ybid ===========================================================================================
id = names(new_combined.list)
combined_data.list = lapply(seq_along(id), function(k){
 # k=3450
  print(k)

  kth_id = id[k]
  
  
  kth_data = new_combined.list[[k]] %>% 
    remove_duplicate_na_columns %>% 
    rename_duplicate_columns
  
  kth_ybid = yb %>% filter(ID == kth_id)
  
  kth_combined = left_join(kth_data, kth_ybid, by = "ID")
  
  return(kth_combined)
  
}) %>% setNames(id)




## 🟧 열이름이 바뀌지 않은 부분 찾기  ===========================================================================================
save = c()
for(k in seq_along(combined_data.list)){
  
  kth_data = combined_data.list[[k]]
  
  if(grepl("열", names(kth_data)[3])){
   
    save = c(k, save) 
    
  }
  
}




## 🟧 "활착"포함 데이터들 확인 ===========================================================================================
# selected_data = lapply(seq_along(combined_data.list), function(i){
#   
#   ith_data  = combined_data.list[[i]]
#   if(grepl("활착", ith_data$Categorized_L3_New[1])){
#    return(ith_data) 
#   }
#     
# }) %>% remove_null_elements
# 
# # year 추출
# years = sapply(selected_data, function(x){
#   
#   x[["year"]] %>% unique
#   
# }) %>% sort


## 🟧 (완료)Check error ===========================================================================================
# "YRBK_00340603" %in% error_id

### 🟨 체크 =========================================================================================
# 체크 완료한 항목
# checking_completed = c("YRBK_0047060403")
# error_id_to_check = error_id[!error_id %in% checking_completed]
# error_id_to_check
# 
# ### 🟨 "YRBK_00460603" =========================================================================================
# ith_id = "YRBK_00460603"
# yb %>% filter(ID == ith_id) %>% unlist %>% unname
# data[names(data) == ith_id][[1]] %>% View
# 
# checking_completed = c(checking_completed, ith_id)
# error_id_to_check = error_id[!error_id %in% checking_completed]
# error_id_to_check
# 
# 
# ### 🟨 "YRBK_00470603" =========================================================================================
# ith_id = "YRBK_00470603"
# yb %>% filter(ID == ith_id) %>% unlist %>% unname
# data[names(data) == ith_id][[1]] %>% View
# combined_data.list[names(combined_data.list) == ith_id][[1]] %>% View
# checking_completed = c(checking_completed, ith_id)
# error_id_to_check = error_id[!error_id %in% checking_completed]
# error_id_to_check
# 
# 
# 
# 
# ### 🟨 "YRBK_00480603" =========================================================================================
# ith_id = "YRBK_00480603"
# yb %>% filter(ID == ith_id) %>% unlist %>% unname
# data[names(data) == ith_id][[1]] %>% View
# 
# checking_completed = c(checking_completed, ith_id)
# error_id_to_check = error_id[!error_id %in% checking_completed]
# error_id_to_check




# ### 🟨 "YRBK_00520609" =========================================================================================
# ith_id = "YRBK_00520609"
# yb %>% filter(ID == ith_id) %>% unlist %>% unname
# data[names(data) == ith_id][[1]] %>% View
# 
# checking_completed = c(checking_completed, ith_id)
# error_id_to_check = error_id[!error_id %in% checking_completed]
# error_id_to_check



### 🟨 "YRBK_0045060301" =========================================================================================
# ith_id = "YRBK_0045060301"
# yb %>% filter(ID == ith_id) %>% unlist %>% unname
# data[names(data) == ith_id][[1]] %>% View
# 
# checking_completed = c(checking_completed, ith_id)
# error_id_to_check = error_id[!error_id %in% checking_completed]
# error_id_to_check


### 🟨 "YRBK_00160317" =========================================================================================
# ith_id = "YRBK_00160317"
# yb %>% filter(ID == ith_id) %>% unlist %>% unname
# data[names(data) == ith_id][[1]] %>% View
# 
# checking_completed = c(checking_completed, ith_id)
# error_id_to_check = error_id[!error_id %in% checking_completed]
# error_id_to_check



### 🟨 예시 저장 =========================================================================================
# combined_data.list[[540]] %>% View
# 
# years = sapply(combined_data.list,function(x){
#   x$year[1]
# })
# 
# # combined_data.list[names(combined_data.list) %in%  "YRBK_00510301"][[1]] %>% View
# path_save
# example = combined_data.list[names(combined_data.list) %in%  "YRBK_00510301"][[1]]
# write.csv(example, paste0(path_save, "/example.csv"))


## 🟧 Export ===========================================================================================
combined_data.list$YRBK_001202030101 %>% View
path_save = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/3.Data with HDR"
saveRDS(combined_data.list, paste0(path_save, "/8.2.Combined Data.rds"))
# combined_data.list = readRDS(paste0(path_save, "/8.2.Combined Data.rds"))



## 🟧 Check ===========================================================================================
combined_data.list[[100]] %>% View

category = sapply(combined_data.list, function(y){
  
  y %>% pull(Categorized_New_2) %>% unique
  
}) %>% unname %>% unique

# category

