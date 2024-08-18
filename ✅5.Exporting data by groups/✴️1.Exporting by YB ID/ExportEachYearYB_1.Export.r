# 🟥 data load ====================================================================================
path_data = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/3.Data with HDR/8.3.Combined Data.rds"
data = readRDS(path_data)



# 🟥 Export data ====================================================================================
path_save = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2"

# i = next_id(data, "YRBK_0047060402")


# 체크할 i
i_to_check = c()

# check_L2 = sapply(data, function(x){
#   x$Categorized_L2 %>% unique
# }) %>% unname %>% unique




tictoc::tic()
for(i in seq_along(data)){
  # i=3712
  if(!i %in% i_to_check){
    ith_data = data[[i]]
    # View(data[[i]])
    # 필요 정보 추출
    L2 = ith_data$Categorized_L2 %>% unique %>% replace_slash_with_dash %>% remove_na %>% remove_special_characters
    L3 = ith_data$Categorized_L3_New %>% unique %>% replace_slash_with_dash %>% remove_na %>% remove_special_characters
    # L3 = ith_data$Categorized_New %>% unique %>% replace_slash_with_dash %>% remove_na %>% remove_special_characters
    year  = ith_data$year %>% unique %>% as.character %>% remove_na
    id = ith_data$ID %>% unique %>% remove_na
    
    # id가 더 많으면 체크에 추가
    if(length(id) > 1){
      
      i_to_check = c(i_to_check, i)
      # i_to_check = NULL
       
    }else{
      
      # 경로 생성
      path_new = file.path(path_save, L3)
      dir.create(path_new, recursive = T, showWarnings = F)
      
      # 파일 저장
      write.csv(ith_data, file.path(path_new, paste0(year, "_", id, ".csv")), row.names = F)  
      cat("\n", crayon::green("Exporting is done! : "), crayon::red(id), "\n")  
      
    }
  }
  print(i)
}
tictoc::toc()





# 🟥 Erorr ID 내보내기 ====================================================================================
i_to_check
write.csv(names(data)[i_to_check], paste0(path_save, "/error id.csv"), row.names = F)


# 🟥 Erorr ID 확인 ====================================================================================
test = read.xlsx("/Users/Ido/Documents/DataAnalysis/KFS_Timeseries/Data/0.Raw Data/DB_20240415/TB_NDI_MRV_STTST_YRBK_OF_FRSTR_20240415.xlsx")

# "YRBK_004806","YRBK_00480627" "YRBK_00480628"
test %>% filter(연보.ID == "YRBK_004806") # 임산물 시장
test %>% filter(연보.ID == "YRBK_00480628") # 없음
test %>% filter(상위.연보.ID == "YRBK_004806")


