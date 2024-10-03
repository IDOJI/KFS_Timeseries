# 🟥 함수 정의 ======================================================================================
others_prop = function(df){
  # df = data_2
  df %>% mutate(침엽수_기타_본수 = 기타_본수 * prop_conifer,
                활엽수_기타_본수 = 기타_본수 * (1 - prop_conifer)) %>% 
    mutate(broad_total_seedling_direct = broad_total_seedling_direct + 활엽수_기타_본수,
           conifer_total_seedling_direct = conifer_total_seedling_direct + 침엽수_기타_본수) %>% 
    select(-침엽수_기타_본수, -기타_본수, -활엽수_기타_본수) %>% 
    mutate(total_seedling_new_2 = broad_total_seedling_direct + conifer_total_seedling_direct) %>% 
    mutate(diff_abs_2 = abs(total_seedling - total_seedling_new_2)) %>% 
    relocate(total_seedling_new_2, diff_abs_2, .after = diff_abs)
}


# 🟥 데이터로드 ======================================================================================
path_data_1 = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/조림/수종별 조림실적Plantation forest by tree species/Combined/1.Combined_68~80.xlsx"
path_data_2 = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/조림/수종별 조림실적Plantation forest by tree species/Combined/2.Combined_82~99_국유림민유림.xlsx"
path_data_3 = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/조림/수종별 조림실적Plantation forest by tree species/Combined/3.Combined_00_직접입력.xlsx"
path_data_4 = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/조림/수종별 조림실적Plantation forest by tree species/Combined/4.Combined_04~22.xlsx"


data_1 = read.xlsx(path_data_1)
data_2 = read.xlsx(path_data_2)
data_3 = read.xlsx(path_data_3)
data_4 = read.xlsx(path_data_4)

View(data_1)
View(data_2)
View(data_3)
View(data_4)


# 🟨 기타에서의 비율 추정  ======================================================================================
sum_conifer = sum(data_4$conifer_total_seedling_direct)
sum_broad = sum(data_4$broad_total_seedling_direct)
prop_conifer = sum_conifer / sum(sum_conifer, sum_broad)

data_1_2 = data_1 %>% others_prop
data_2_2 = data_2 %>% others_prop
  
names(data_1_2)
View(data_1_2)
View(data_2_2)



# 🟩 Combined data  ======================================================================================
combined_data = list(data_1_2, data_2_2, data_3, data_4) %>% 
  lapply(function(x){
    x %>% 
      select(classification, year, total_seedling_direct, conifer_total_seedling_direct, broad_total_seedling_direct) %>% 
      mutate(year = as.character(year))
  }) %>% 
  bind_rows %>% 
  group_by(classification) %>% 
  filter(year == max(year)) %>% 
  ungroup
View(combined_data)

# 유일한 연도?
sum(table(combined_data)) == nrow(combined_data)

# 연속?
combined_data$classification %>% is_consecutive()





# 🟪 내보내기 ======================================================================================
# data_combined_sub = read.xlsx(file_path)
path_save = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/조림/수종별 조림실적Plantation forest by tree species/Combined"
file_name = "5.Combined_final.xlsx"
file_path = file.path(path_save, file_name)
write.xlsx(combined_data , file_path)
View(combined_data)









