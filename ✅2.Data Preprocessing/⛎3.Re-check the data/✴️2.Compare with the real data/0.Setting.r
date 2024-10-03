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
List.list[[5]] = texts = c("stringr", "stringi")
List.list[[6]] = misc = c("devtools")
List.list[[7]] = db = c("RMySQL", "DBI", "odbc", "RSQL", "RSQLite")
List.list[[8]] = sampling = c("rsample")
List.list[[9]] = excel = c("openxlsx")
List.list[[10]] = others = c("beepr")

packages_to_install_and_load = unlist(List.list)
install_packages(packages_to_install_and_load)

## 🟧dplyr =======================================================
filter = dplyr::filter
select = dplyr::select




# 🟥 Define Functions ##########################################################################
# 데이터프레임에서 모든 값이 NA인 열을 제거하는 함수 정의
remove_na_columns <- function(df) {
  # 반복문을 사용하여 데이터프레임의 맨 오른쪽 열부터 체크
  for (i in seq(ncol(df), 1)) {
    # 현재 열의 모든 값이 NA인지 확인
    if (all(is.na(df[[i]]))) {
      # 모든 값이 NA인 경우 해당 열을 데이터프레임에서 제거
      df[[i]] <- NULL
    }
  }
  return(df)
}
