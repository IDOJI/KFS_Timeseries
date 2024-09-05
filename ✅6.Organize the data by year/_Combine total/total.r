# 🟨 load final data ===========================================================================
path_final = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/7.최종데이터/data1.xlsx"
final = read.xlsx(path_final)
names(final)
# View(final)
final$year %>% class
final$year = final$year %>% as.character
names(final)


# 🟥 data load ==========================================================================================
path_tending = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/숲가꾸기/exported_new/foresttending_combined.xlsx"
path_forestation = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/조림/수종별 조림실적Plantation forest by tree species/Combined/5.Combined_final.xlsx"
path_stock = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/연도별 산림면적 및 임목축적Forest Land Area and Growing Stock by Year/growing stock.xlsx"


tending = read.xlsx(path_tending) %>% 
  select(year, 어린나무가꾸기, 큰나무가꾸기_경제림가꾸기_솎아베기, 큰나무가꾸기_경제림가꾸기_천연림보육, 큰나무가꾸기_공익림가꾸기) %>%
  rename(youtending = 어린나무가꾸기) %>%
  rename(thinning = 큰나무가꾸기_경제림가꾸기_솎아베기) %>%
  rename(nattending = 큰나무가꾸기_경제림가꾸기_천연림보육) %>%
  rename(pubint_tending = 큰나무가꾸기_공익림가꾸기) %>%
  mutate(bigtending = thinning + nattending + pubint_tending)

stock = read.xlsx(path_stock) %>% 
  mutate(stock = growing_stock___m3/10000)
  

# View(forestation)
forestation = read.xlsx(path_forestation) %>% 
  rename(plant_total = total_seedling_direct,
         plant_hw = broad_total_seedling_direct,
         plant_ch = conifer_total_seedling_direct) %>% 
  select(-year) %>% 
  rename(year = classification)



# 🟥 check data ========================================================================================================
tending %>% check_unique_column("year")
forestation %>% check_unique_column("year")
stock %>%  check_unique_column("year")



# 🟥 merge data ========================================================================================================
merged_data = tending %>% 
  merge(stock, by = "year", all = T) %>% 
  merge(forestation, by = "year", all = T) %>% 
  filter(year %in% final$year)
names(final)


final_2 = final %>% 
  select(supp, dsupp, bug, cut, fire) %>% 
  cbind(merged_data) %>% 
  relocate(year)

names(final_2 )

path_save = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/7.최종데이터"
file_name = "data_final_240903.xlsx"
write.xlsx(final_2, file.path(path_save, file_name))











