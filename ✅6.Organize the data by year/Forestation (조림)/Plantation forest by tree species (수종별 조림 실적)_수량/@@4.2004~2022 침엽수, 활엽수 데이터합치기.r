# 🟥 정의할 벡터  ======================================================================================
exclude_vector <- c(
  "Categorized_L3_New", "Categorized_L3", "Categorized_L2",
  "NAME_L1", "NAME_L2", "NAME_L3", "NAME_L4", "NAME_L5",
  "ID_L1", "ID_L2", "ID_L3", "ID_L4", "ID_L5",
  "unit_L2", "unit_L3", "unit_L4", "unit_L5",
  "비고_L2", "비고_L3", "비고_L4", "비고_L5"
)



# 🟥 데이터 정리  ======================================================================================
data_path <- "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/조림/수종별 조림실적Plantation forest by tree species/5.04~23_활엽수, 침엽수"
combined_data <- data_path %>% 
  list.files(full.names = T) %>% 
  lapply(read.csv) %>% 
  setNames(basename(list.files(data_path, pattern = "\\.csv$", full.names = TRUE))) %>% 
  lapply(function(x) {
    x %>% 
      slice(grep("\\d{4}", x[[3]])) %>% 
      rename(classification = names(.)[3]) %>% 
      relocate(year, .after = classification) %>% 
      select(-행) %>% 
      select(-all_of(grep("면적", names(.)))) %>% 
      relocate(contains("계"), .after = year) %>% 
      mutate(classification = if_else(classification == "2012_1", "2012",
                                      if_else(classification == "2012_2", "2013", classification)))
  }) %>% 
  # "침엽수"와 "활엽수" 데이터를 나눠서 저장
  {
    conifer_df <- keep(., ~ grepl("침엽수", .x$NAME_L4[1])) %>% 
      lapply(function(x) {
        x %>% 
          select(-all_of(exclude_vector)) %>% 
          rename(conifer_total_seedling = names(.)[4]) %>% 
          mutate(conifer_total_seedling_direct = rowSums(select(., 5:ncol(.)), na.rm = T)) %>% 
          mutate(conifer_diff_abs = abs(conifer_total_seedling_direct - conifer_total_seedling)) %>% 
          select(all_of(c("classification", "year", "conifer_total_seedling_direct", "conifer_total_seedling", "conifer_diff_abs")))
      }) %>% 
      do.call(rbind, .)  # "침엽수" 데이터 병합
    
    broadleaf_df <- keep(., ~ grepl("활엽수", .x$NAME_L4[1])) %>% 
      lapply(function(x) {
        x %>% 
          select(-all_of(exclude_vector)) %>% 
          rename(broad_total_seedling = names(.)[4]) %>% 
          mutate(broad_total_seedling_direct = rowSums(select(., 5:ncol(.)), na.rm = T)) %>% 
          mutate(broad_diff_abs = abs(broad_total_seedling_direct - broad_total_seedling)) %>% 
          select(all_of(c("classification", "year", "broad_total_seedling_direct", "broad_total_seedling", "broad_diff_abs")))
      }) %>% 
      do.call(rbind, .)  # "활엽수" 데이터 병합
    
    merge(conifer_df, broadleaf_df, by = c("classification", "year"), all=T) # 두 데이터 병합
    
  } %>% 
  arrange(classification, year) %>% 
  mutate(total_seedling = conifer_total_seedling + broad_total_seedling,
         total_seedling_direct = conifer_total_seedling_direct + broad_total_seedling_direct) %>% 
  mutate(diff_abs = abs(total_seedling - total_seedling_direct)) %>% 
  # 최신 연도 선택
  group_by(classification) %>% 
  filter(year == max(year)) %>% 
  ungroup()

View(combined_data)
combined_data$classification %>% table




# 🟥 Export ======================================================================================
path_save = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/조림/수종별 조림실적Plantation forest by tree species/Combined"
file_name = "4.Combined_04~22"

write.xlsx(combined_data , file.path(path_save, paste0(file_name, ".xlsx")))



















