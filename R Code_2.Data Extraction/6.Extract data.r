# 🟥 Load Functions & Packages ##########################################################################
# rm(list = ls())

Sys.setlocale("LC_ALL", "en_US.UTF-8")

## 🟩Install and loading Packages ================================
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




# 🟥 Define a clustering function #####################################################################################################
## 🟧 필요한 패키지 로드 ##############################################################################################################
library(tm)
library(proxy)
library(cluster)
library(factoextra)
library(showtext)
font_add_google(name = "Nanum Gothic", family = "nanumgothic")
showtext_auto()
library(tm)
library(cluster)
library(factoextra)








# 🟥 Data Load #####################################################################################################
path_save = "/Users/Ido/Library/CloudStorage/GoogleDrive-clair.de.lune.404@gmail.com/My Drive/DataAnalysis/KFS_Timeseries/rearranged data"
data = readRDS(paste0(path_save, "/5.Combined hdr data.rds"))





# 🟥 데이터 추출 함수 #####################################################################################################
# 패턴을 기반으로 리스트 원소를 추출하는 함수 정의
extract_by_pattern <- function(input_patterns, data_list) {
  # 모든 키에서 공백과 언더스코어를 제거한 버전을 미리 준비
  cleaned_names <- sapply(names(data_list), function(name) gsub("[ _]", "", name))
  names(cleaned_names) <- names(data_list)
  
  # 입력 패턴에 대해
  result <- list()
  for (pattern in input_patterns) {
    cleaned_pattern <- gsub("[ _]", "", pattern)
    matched_keys <- names(cleaned_names)[grepl(cleaned_pattern, cleaned_names, ignore.case = TRUE)]
    
    # 매칭된 키에 해당하는 원소를 결과에 추가
    for (key in matched_keys) {
      result[[key]] <- data_list[[key]]
    }
  }
  
  return(result)
}


# 🟥 데이터 추출 테스트 #####################################################################################################
input_patterns <- c("소유별임야면적구성")
result <- extract_by_pattern(input_patterns, data)

result %>% names

input_patterns <- c("숲가꾸기")
result <- extract_by_pattern(input_patterns, combined.list)
names(result)





# 🟥 Export df #####################################################################################################
# path_save = "/Users/Ido/Library/CloudStorage/GoogleDrive-clair.de.lune.404@gmail.com/My Drive/DataAnalysis/KFS_Timeseries/rearranged data"
# write.csv(L3_Categorized_data, paste0(path_save, "/4.L3 Re-Categorized data.csv"), row.names = F)







