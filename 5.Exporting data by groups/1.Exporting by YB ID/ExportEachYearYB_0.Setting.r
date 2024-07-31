# rm(list=ls())
# 🟥 Load Functions & Packages ##########################################################################
## 🟨Install and loading Packages ================================
# rm(list = ls())
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



# 🟥 함수 정의 ##########################################################################
# 슬래시(/)를 대시(-)로 바꾸는 함수 정의
replace_slash_with_dash <- function(input_string) {
  # 문자열 내의 모든 슬래시(/)를 대시(-)로 바꾸기
  output_string <- gsub("/", "-", input_string)
  return(output_string)
}



# 문자열 벡터에서 특정 특수 문자를 제거하는 함수 정의
remove_special_characters <- function(strings) {
  # 특수 문자를 제거하는 정규 표현식 정의
  pattern <- "\\bcdot\\b|[[:punct:]]"
  
  # gsub 함수를 사용하여 특수 문자 제거
  cleaned_strings <- gsub(pattern, "", strings)
  
  return(cleaned_strings)
}


# 다음 연보 id
next_id = function(data, target_id, ind = T){
  ids = data %>% names()
  ind = which(ids == target_id)
  if(ind){
    ind 
  }else{
    ids[ind+1]  
  }
}


# NA를 제외하는 함수 정의
remove_na <- function(input_vector) {
  # NA 값을 제외한 벡터 생성
  output_vector <- input_vector[!is.na(input_vector)]
  return(output_vector)
}
