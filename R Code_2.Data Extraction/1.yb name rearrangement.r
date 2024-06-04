# 🟥 Load Functions & Packages ##########################################################################
# rm(list = ls())
## 🟨Install and loading Packages ================================
install_packages = function(packages, load=TRUE) {
  # load : load the packages after installation?
  for(pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg)
    }
    
    if(load){
      library(pkg, character.only = TRUE, quietly = T)
    }
  }
}

List.list = list()
List.list[[1]] = visual = c("ggpubr", "ggplot2", "ggstatsplot", "ggsignif", "rlang", "RColorBrewer")
List.list[[2]] = stat = c("fda", "MASS")
List.list[[3]] = data_handling = c("tidyverse", "dplyr", "clipr", "tidyr", "readr", "caret", "readxl")
List.list[[4]] = qmd = c("janitor", "knitr")
List.list[[5]] = texts = c("stringr")
List.list[[6]] = misc = c("devtools")
List.list[[7]] = db = c("RMySQL", "DBI", "odbc", "RSQL", "RSQLite")
List.list[[8]] = sampling = c("rsample")
List.list[[9]] = excel = c("openxlsx")

packages_to_install_and_load = unlist(List.list)
install_packages(packages_to_install_and_load)



## 🟧dplyr =======================================================
filter = dplyr::filter
select = dplyr::select





## 🟧Loading my functions ======================================================
load_functions = function(path_functions){
  list.files(path_functions, full.names = T) %>%
    purrr::walk(source)
}
path_list = list()
path_list[1] = "/Users/Ido/Library/CloudStorage/Dropbox/1.GitHub/R___refineR/R"
path_list[2] = "/Users/Ido/Library/CloudStorage/Dropbox/1.GitHub/R___StatsR/R"
Load = sapply(path_list, load_functions)








# 🟥 Data Load #####################################################################################################
# 연보 이름
path_year_names = "/Users/Ido/Library/CloudStorage/GoogleDrive-clair.de.lune.404@gmail.com/My Drive/DataAnalysis/KFS_Timeseries/Data/DB_20240415/TB_NDI_MRV_STTST_YRBK_OF_FRSTR_20240415.xlsx"
ynames = read.xlsx(path_year_names)






# 🟥 연보 연도 체크 #####################################################################################################
ynames$연보.년도 %>% table





# 🟥 Combine names #####################################################################################################
ynames <- ynames %>%
  mutate(combined_names = ifelse(is.na(`연보.년도`),
                                 paste(`연보명(한글)`, `연보명(영문)`, sep = "_"),
                                 paste(`연보.년도`, `연보명(한글)`, `연보명(영문)`, sep = "_"))) %>%
  relocate(combined_names)









# 🟥 `최상위 연보` 이름 붙이기 #########################################################################
## 🟧 상위.연보가 `NA`인 경우 추출 ######################################################################################################
# 상위 연보가 NA이므로 무조건 최상위 연보
the_upper_ynames = ynames %>% 
  filter(!상위.연보.ID %in% 연보.ID) %>% 
  select(c("combined_names", "연보.ID", "상위.연보.ID"))





## 🟧 repeat setting ######################################################################################################
# left_join에서 합쳐질 이전 데이터프레임 설정
previous_data = the_upper_ynames

# change data object for convenience
data = ynames %>% select(c("combined_names", "연보.ID", "상위.연보.ID"))


# data, results save
data.list = list()


## 🟧 반복해서 필터링 ######################################################################################################
repeat {
  current_level_data <- data %>% filter(상위.연보.ID %in% previous_data$연보.ID)
  remaining_data <- data %>% filter(!상위.연보.ID %in% previous_data$연보.ID)
  print(dim(current_level_data))
  
  # 상위 연보로 존재하지 않는 데이터 저장
  data.list <- previous_data %>%
    filter(!연보.ID %in% data$상위.연보.ID) %>%
    list() %>%
    c(data.list, .)
  
  # 상위연보의 이름을 추가하여 데이터 결합
  if (nrow(current_level_data) > 0) {
    data_1_joined <- current_level_data %>%
      left_join(previous_data %>% select(combined_names, 연보.ID),
                by = c("상위.연보.ID" = "연보.ID"),
                suffix = c("", ".upper")) %>%
      mutate(combined_names = if_else(!is.na(combined_names.upper),
                                      paste(combined_names.upper, combined_names, sep = "___"),
                                      combined_names)) %>%
      select(-combined_names.upper)
    
    previous_data <- data_1_joined %>% select(combined_names, 연보.ID, 상위.연보.ID)
    data <- remaining_data
  } else {
    data.list <- list(remaining_data) %>% c(., data.list)
    break
  }
}

## 🟧 최종 결과 데이터 리스트를 데이터프레임으로 변환 ######################################################################################################
final_data <- do.call(rbind, data.list)



## 🟧 결과 체크 ######################################################################################################
# View(final_data)




## 🟧 Check non Year ######################################################################################################
# combined_names에 연도가 없는 데이터 찾기
# 데이터프레임 `final_data`에서 `combined_names` 열의 앞부분 4글자가 숫자인지 확인
non_year_data <- final_data %>%
  filter(!grepl("^[0-9]{4}$", substr(combined_names, 1, 4)))

# 결과 확인
print(non_year_data)
non_year_data$상위.연보.ID
# "YRBK_00030102 01" "YRBK_00030102 02" "YRBK_00030102 03"
no_year_yb_ID = c("YRBK_0003010201", "YRBK_0003010202", "YRBK_0003010203")

# 상위연보 ID에 따라 확인
final_data %>% filter(연보.ID %in% "YRBK_0003")
# final_data %>% filter(연보.ID %in% "YRBK_000301") -> 없음
final_data %>% filter(연보.ID %in% "YRBK_00030102")



# Add names
non_year_data = final_data %>% filter(연보.ID %in% no_year_yb_ID)
non_year_data$연보.ID==non_year_data$상위.연보.ID


# add 
# 상위연보 변경
non_year_data$상위.연보.ID = "YRBK_00030102"
non_year_data_2 = non_year_data %>% 
  left_join(final_data, 
            by = c("상위.연보.ID" = "연보.ID"), 
            suffix = c("", ".final")) %>% 
  mutate(combined_names = paste(combined_names.final, combined_names, sep = "___")) %>% 
  select(-ends_with(".final"))


final_data_2 = final_data %>% 
  filter(!연보.ID %in% no_year_yb_ID) %>% 
  rbind(., non_year_data_2)



# 🟥 Add year ######################################################################################################
# `final_data_2`에서 `combined_names` 열의 앞부분 4글자를 추출하여 새로운 열 `year` 생성
final_data_2 <- final_data_2 %>%
  mutate(year = substr(combined_names, 1, 4) %>% as.numeric) %>% 
  arrange(year)




# 🟥 상위 연보 ID가 NA인 행 제외 ######################################################################################################
final_data_3 = final_data_2 %>% 
  filter(!is.na(상위.연보.ID))




# 🟥 각 섹션 레벨 추출 ######################################################################################################
# 새로운 열을 만들기 위해 combined_names에서 연도를 제외하고 분리
# 연도를 제외한 문자열을 임시로 만들고, 이를 사용하여 새로운 열 생성
final_data_4 <- final_data_3 %>%
  mutate(temp_names = sub("^[0-9]{4}_", "", combined_names)) %>%
  separate(temp_names, into = c("L1", "L2", "L3"), sep = "___", extra = "merge", fill = "right") %>%
  mutate(temp_names = NULL) %>%   # 임시 열 제거
  relocate(ends_with(".ID"))

  



# 🟥 L2가 NA인 경우 #######################################################################################
data = final_data_4

### 🟨 L2 == NA_NA 항목 체크 ==============================================================================
L2_NA_data = data %>% filter(L2 == "NA_NA")
L2_non_NA_data = data %>% filter(L2 != "NA_NA")


### 🟨 연보 아이디 체크 ==============================================================================
L2_NA_data$`연보.ID` %>% cat(sep = ", ")



### 🟨 다른 연보에서 L3가 동일한 데이터의 L2 값을 사용 ==============================================================================
L2_NA_data_new <- L2_NA_data %>%
  left_join(L2_non_NA_data[,c("L2", "L3")], by = "L3") %>%
  mutate(L2 = L2.y) %>% 
  select(-L2.y, -L2.x) %>% 
  relocate(L2, .after = L1) %>% 
  mutate(combined_names = paste0(year, "_", L1, "___", L2, "___", L3))



### 🟨 Combine & export the data ==============================================================================
L2_NA_data_new %>% dim
L2_non_NA_data %>% dim
combined_data = rbind(L2_non_NA_data, L2_NA_data_new)
path_save = "/Users/Ido/Library/CloudStorage/GoogleDrive-clair.de.lune.404@gmail.com/My Drive/DataAnalysis/KFS_Timeseries/rearranged data"
write.csv(combined_data, paste0(path_save, "/1.Sorted_YB_Names.csv"), row.names = FALSE)














