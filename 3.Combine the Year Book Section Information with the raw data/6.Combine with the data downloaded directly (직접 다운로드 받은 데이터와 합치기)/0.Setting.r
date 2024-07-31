# 🟥 "NA"파트를 데이터없음으로 표기 ##########################################################################
filter = dplyr::filter
select = dplyr::select

process_data_na_columns <- function(df) {
  # 열의 인덱스를 저장할 벡터
  columns_to_modify <- c()
  
  # 각 열에 대해 모든 값이 NA인지 확인
  for (col in 1:ncol(df)) {
    if (all(is.na(df[, col]))) {
      # 왼쪽 또는 오른쪽 열이 모두 NA인지 확인
      if ((col > 1 && all(is.na(df[, col - 1]))) || (col < ncol(df) && all(is.na(df[, col + 1])))) {
        columns_to_modify <- c(columns_to_modify, col)
      }
    }
  }
  
  # 저장된 열의 NA 값을 "데이터없음"으로 수정
  for (col in columns_to_modify) {
    df[, col] <- "데이터없음"
  }
  
  return(df)
}


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


# 🟥 Define functions #################################################################################################################
## 🟧 NA 인 중복열 제거 ##############################################################################################################
remove_duplicate_na_columns <- function(df) {
  # 열 이름들
  col_names <- names(df)
  
  # 중복 열 이름 확인
  duplicated_col_names <- unique(col_names[duplicated(col_names)])
  
  # 중복 열 이름에 대해 처리
  for (col_name in duplicated_col_names) {
    # 해당 이름을 가진 열들의 인덱스
    col_indices <- which(col_names == col_name)
    
    # 모든 원소가 NA인 열의 인덱스 찾기
    na_col_indices <- col_indices[sapply(col_indices, function(i) all(is.na(df[[i]])))]
    
    # 해당 열들 제거
    if (length(na_col_indices) > 0) {
      df <- df[, -na_col_indices, drop = FALSE]
      col_names <- names(df)  # 이름 리스트 업데이트
    }
  }
  
  return(df)
}





## 🟧 중복열 이름 넘버링 ##############################################################################################################
rename_duplicate_columns <- function(df) {
  col_names <- names(df)
  col_count <- table(col_names)
  new_names <- col_names
  
  for (name in names(col_count)) {
    if (col_count[name] > 1) {
      dup_indices <- which(col_names == name)
      for (i in seq_along(dup_indices)) {
        new_names[dup_indices[i]] <- paste0(name, "_", i)
      }
    }
  }
  
  names(df) <- new_names
  return(df)
}


