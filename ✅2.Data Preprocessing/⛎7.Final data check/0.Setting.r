
# 🟥 Load Functions & Packages ###############################################################################################
# rm(list = ls())
# 문자열 벡터에서 특정 문자열 패턴을 포함하는 값을 추출하는 함수 정의

# 문자열 벡터에서 특정 문자열 패턴을 포함하고 특정 문자열 패턴을 제외하는 값을 추출하는 함수 정의
# 필요한 라이브러리 로드
library(stringr)

# 문자열 벡터에서 특정 문자열 패턴을 포함하고 특정 문자열 패턴을 제외하는 값을 추출하는 함수 정의
# 문자열 벡터에서 특정 문자열 패턴을 포함하고 특정 문자열 패턴을 제외하는 값을 추출하는 함수 정의
# 필요한 라이브러리 로드
library(stringr)

# 문자열 벡터에서 특정 문자열 패턴을 포함하고 특정 문자열 패턴을 제외하는 값을 추출하는 함수 정의
filter_strings <- function(strings, include = NULL, exclude = NULL, or = FALSE) {
  # include에 있는 패턴을 포함하는 값들만 필터링
  if (!is.null(include)) {
    if (or) {
      strings <- strings[sapply(strings, function(x) any(sapply(include, function(pat) grepl(pat, x))))]
    } else {
      strings <- strings[sapply(strings, function(x) all(sapply(include, function(pat) grepl(pat, x))))]
    }
  }
  
  # exclude에 있는 패턴을 제외
  if (!is.null(exclude)) {
    strings <- strings[!sapply(strings, function(x) any(sapply(exclude, function(pat) grepl(pat, x))))]
  }
  
  return(strings)
}
filter = dplyr::filter

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
List.list[[5]] = texts = c("stringr", "stringi")
List.list[[6]] = misc = c("devtools")
List.list[[7]] = db = c("RMySQL", "DBI", "odbc", "RSQL", "RSQLite")
List.list[[8]] = sampling = c("rsample")
List.list[[9]] = excel = c("openxlsx")
List.list[[10]] = others = c("beepr", "pander")

packages_to_install_and_load = unlist(List.list)
install_packages(packages_to_install_and_load)



## 🟧dplyr =======================================================
filter = dplyr::filter
select = dplyr::select






