modify_data = function(df){
  # df = combined_data[[16]]
  # names(combined_data)
  
  id = unique(df[["ID"]])
  
  if(id == "YRBK_00130304"){
    
    df[["잣나무_계"]][6] = 27612
    
  }else if(id == "YRBK_00150304"){
    
    new_values = c(624, 383, 100, 
                   60, 277, 170, 
                   377, 227, 175, 
                   102)
    df[["오동_계"]][1:length(new_values)] =  new_values
    
  }else if(id == "YRBK_00160304"){
    
    df[["잣나무_계"]][2] = 27612 
    new_values = c(100, 60, 277, 
                   170, 377, 227, 
                   175, 102, 40,
                   26)
    df[["오동_계"]][1:length(new_values)] = new_values 
    
  }else if(id == "YRBK_00170304"){
    
    df[["현사시_계"]][4] = 6055
    df[["현사시_계"]][10] = 175
    df[["오동_계"]][5] = 175
    
  }else if(id == "YRBK_00180304"){
    
    df[["현사시_계"]][4] = 6055
    df[["현사시_계"]][10] = 1313
    df[["오동_계"]][5] = 175 # 186
    
  }else if(id == "YRBK_00270304"){
    
    df[["오동_계"]][2] = 300 # 200
    
  }
  
  
  if(id %in% c("YRBK_00290304", "YRBK_00280304")){
    df[[3]] = gsub("합계", unique(df$year-1), df[[3]])
  }
    
  
  return(df)
  
}
# df = combined_data[[18]]
# combined_data[[18]] %>% View
# combined_data[[17]] %>% View





# 🟨 필요 벡터들 ============================================================================
# 나무 이름 목록
coniferous_trees <- c("잣나무_본수", "낙엽송_본수", "삼나무_본수", "편백_본수", 
                      "리기다_본수", "테다_본수", "리기테다_본수", "강송_본수", 
                      "해송_본수")

leafy_trees <- c("밤나무_본수", "이태리포플러_본수", "현사시_본수", "오동_본수")



# 🟨 데이터 처리 ============================================================================
path_data = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/조림/수종별 조림실적Plantation forest by tree species/2.82~99"
# length(combined_data)
# View(combined_data )
combined_data <- path_data %>% 
  list.files(., full.names= T) %>% 
  lapply(read.csv) %>% 
  setNames(list.files(path_data, full.names=F)) %>% 
  lapply(function(x){
    x %>% modify_data 
  }) %>% 
  setNames(list.files(path_data)) %>% 
  do.call(rbind, .) %>% 
  select(-contains("국유림"), -contains("민유림")) %>% 
  rename(classification = 구분_1) %>%  # 이름 변경
  arrange(classification) %>% 
  slice(grep("본수", classification)) %>%  # "본수" 문자열을 포함하는 행만 추출
  select(-행) %>%  # 행 열 삭제
  mutate(classification = substr(classification, 1, 4)) %>%  # classification에서 연도만 추출
  filter(grepl("\\d{4}", classification)) %>% 
  # filter(grepl("^\\d{4}$", classification)) %>%   # classification 열에 연도만 있는 행만 필터링
  rename(total_seedling = 계_계) %>%  # 열이름 변경
  select(-all_of(c("Categorized_L3_New", "Categorized_L3", "Categorized_L2", "NAME_L1", "NAME_L2", "NAME_L3", "NAME_L4", "NAME_L5", "ID_L1", "ID_L2", "ID_L3", "ID_L4", "ID_L5", "unit_L2", "unit_L3", "unit_L4", "unit_L5", "비고_L2", "비고_L3", "비고_L4", "비고_L5"))) %>%
  mutate(conifer_total_seedling = NA,
         broad_total_seedling = NA) %>% 
  # 열 이름에서 "_계"를 "_본수"로 바꾸기
  rename_with(~ gsub("_계", "_본수", .), everything()) %>% 
  # coniferous_trees에 속하면 "침엽수_" 붙이기, leafy_trees에 속하면 "활엽수_" 붙이기
  rename_with(~ ifelse(. %in% coniferous_trees, paste0("침엽수_", .), 
                       ifelse(. %in% leafy_trees, paste0("활엽수_", .), .)), everything()) %>% 
  # 침엽수, 활엽수 열들 합
  mutate(
    # "침엽수_"로 시작하는 열들의 합을 계산하여 새로운 열에 저장
    conifer_total_seedling_direct = rowSums(select(., starts_with("침엽수_")), na.rm = TRUE),
    # "활엽수_"로 시작하는 열들의 합을 계산하여 새로운 열에 저장
    broad_total_seedling_direct = rowSums(select(., starts_with("활엽수_")), na.rm = TRUE)
  ) %>% 
  mutate(total_seedling_direct = conifer_total_seedling_direct + broad_total_seedling_direct + 기타_본수) %>% 
  mutate(diff_abs = abs(total_seedling_direct - total_seedling)) %>% 
  relocate(starts_with("침엽수_"), starts_with("활엽수_"), .after = last_col()) %>% 
  select(-all_of(starts_with("침엽수_"))) %>% 
  select(-all_of(starts_with("활엽수_"))) %>% 
  { rownames(.) <- NULL; . } %>% 
  relocate(total_seedling_direct, diff_abs, .after = total_seedling) %>% 
  relocate(year, .after = classification) %>% 
  mutate(year = as.character(year)) %>% 
  filter(!(year == "1989" & classification == "1983")) %>%  # 이상한 데이터 제외, 이전 연도로 사용
  filter(!(year == "1991" & classification == "1985")) %>%  # 이상한 데이터 제외, 이전 연도로 사용
  filter(!(year == "1995" & classification == "1989")) %>%  # 1995,1994의 데이터에서 실제연보과 비교했을 때 이상은 없었지만,  토탈값이 이전과 동일한 경우에도 값에 차이가 존재
  filter(!(year == "1988" & classification == "1983")) %>% 
  filter(!(year == "1987" & classification == "1983")) %>% 
  filter(!(year == "1986" & classification == "1983")) %>% 
  filter(!(year == "1985" & classification == "1983")) %>% 
  filter(!(year == "1994" & classification == "1989")) %>% 
  filter(!(year == "1997" & classification == "1991")) %>% 
  group_by(classification) %>% 
  slice_max(order_by = as.numeric(year), n = 1) %>% 
  ungroup()




# 
# View(combined_data)



# 🟨 Export ======================================================================================
# filtered_df_3 = read.xlsx(file_path)
path_save = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/조림/수종별 조림실적Plantation forest by tree species/Combined"
file_name = "2.Combined_82~99_국유림민유림.xlsx"
file_path = file.path(path_save, file_name)
write.xlsx(combined_data, file_path)







# 
# 
# # 🟦 실제 연보와 값 비교 및 값 교체 =============================================================================================
# data_list = filtered_data_list
# 
# # data_list[[1]]
# View( data_list[[1]])
# 
# ###  ✴️ 1982 ===========================================================================================================
# id = "YRBK_00120304"
# ind = grep(id, names(data_list))
# data_1 = data_list[[ind]]
# View(data_1)
# 
# 
# data_1$잣나무_계[which(data_1$잣나무_계 == 27615)] = 27612
# 
# tree = "강송_계"
# data_1[[tree]][which(data_1[[tree]] == 3636)] = 3434
# 
# data_list[[ind]] = data_1











