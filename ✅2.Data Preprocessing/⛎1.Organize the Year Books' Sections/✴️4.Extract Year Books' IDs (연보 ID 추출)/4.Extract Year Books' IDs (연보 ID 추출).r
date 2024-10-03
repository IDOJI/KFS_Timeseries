# 🟥 Load Functions & Packages ##########################################################################
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

packages_to_install_and_load = unlist(List.list)
install_packages(packages_to_install_and_load)

## 🟧dplyr =======================================================
filter = dplyr::filter
select = dplyr::select


# 마지막으로 나타나는 NA가 아닌 값을 반환하는 함수
get_last_non_na <- function(x) {
  # NA가 아닌 값들의 인덱스를 찾기
  non_na_indices <- which(!is.na(x))
  
  # NA가 아닌 값이 하나라도 있으면 마지막 값을 반환하고, 그렇지 않으면 NA를 반환
  if(length(non_na_indices) > 0) {
    return(x[non_na_indices[length(non_na_indices)]])
  } else {
    return(NA)
  }
}

# 데이터 프레임을 입력으로 받아 새로운 열 Last_Value를 추가하는 함수
add_last_value_column <- function(df) {
  # 지정된 열 목록
  columns_to_check <- c("ID_L1", "ID_L2", "ID_L3", "ID_L4", "ID_L5")
  
  # 지정된 열만 선택하여 NA가 나오기 직전의 값을 추출
  df$ID <- apply(df[columns_to_check], 1, get_last_non_na)
  
  df = df %>% relocate(ID, .after = "ID_L5")
  
  return(df)
}





# 🟥 Data Load ##########################################################################
path_yb = "/Users/Ido/Documents/DataAnalysis/KFS_Timeseries/2.Rearrange YB names/3.4.Categorized L3 New.csv"
yb = read.csv(path_yb)


# 🟥 ID 추출 ##########################################################################
yb_2 = yb %>% add_last_value_column
# View(yb_2)


# 🟥 save ##########################################################################
path_save = "/Users/Ido/Documents/DataAnalysis/KFS_Timeseries/2.Rearrange YB names"
write.csv(yb_2, paste0(path_save, "/4.Added YB ID.csv"), row.names = F)









