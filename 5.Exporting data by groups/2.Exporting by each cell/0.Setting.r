# rm(list=ls())

filter_includes <- function(input_vector, include) {
  # input_vector가 벡터인지 확인
  if (!is.vector(input_vector)) {
    stop("input_vector must be a vector")
  }
  
  # include가 문자열 벡터인지 확인
  if (!is.character(include)) {
    stop("include must be a character vector")
  }
  
  # 필터링
  filtered_vector <- Filter(function(x) all(sapply(include, function(pattern) grepl(pattern, x))), input_vector)
  
  return(filtered_vector)
}

# NULL인 원소를 제외하는 함수 정의
remove_null_elements <- function(input_list) {
  # NULL이 아닌 원소만 추출하여 새로운 리스트 생성
  cleaned_list <- input_list[!sapply(input_list, is.null)]
  return(cleaned_list)
}

# 로마 문자를 아라비아 숫자로 변환하는 함수 정의
convert_roman_to_arabic <- function(df) {
  # 로마 문자에서 아라비아 숫자로의 매핑 테이블 정의
  roman_to_arabic <- c("Ⅵ" = 6, "Ⅴ" = 5, "Ⅳ" = 4, "Ⅲ" = 3, "Ⅱ" = 2, "Ⅰ" = 1)
  
  # 열 이름을 순회하며 변환
  new_colnames <- colnames(df)
  for (roman in names(roman_to_arabic)) {
    arabic <- roman_to_arabic[[roman]]
    new_colnames <- gsub(roman, as.character(arabic), new_colnames, fixed = TRUE)
  }
  colnames(df) <- new_colnames
  
  return(df)
}

# 🟥 수치 데이터를 추출하고 파일로 저장하는 함수 =======================================================================
library(dplyr)
library(readr)
library(tictoc)

process_and_export <- function(df, path = ".", exclude = NULL, include = NULL) {
  # 공통 열 이름
  common_cols <- c("Categorized_L3_New", "year", "ID", "unit_L2", "unit_L3", "unit_L4", "unit_L5")
  
  # 열이름에서 로마자 변환
  df = convert_roman_to_arabic(df)
  
  # 공통 열에서 유일한 값을 추출
  categorized_L3 <- unique(df$Categorized_L3_New) %>% remove_non_korean_characters 
  year <- unique(df$year)
  id <- unique(df$ID)
  
  # 제외 목록에 해당하는 경우 루프를 건너뜁니다
  if (!is.null(exclude) && categorized_L3 %in% exclude) {
    message(paste("Skipping due to exclusion: ", categorized_L3))
    return(list())
  }
  
  # 포함 목록에 해당하지 않는 경우 루프를 건너뜁니다
  if (!is.null(include) && !(categorized_L3 %in% include)) {
    message(paste("Skipping due to not being in include list: ", categorized_L3))
    return(list())
  }
  
  # unit 값 설정
  unit_L2 <- unique(na.omit(df$unit_L2))
  unit_L3 <- unique(na.omit(df$unit_L3))
  unit_L4 <- unique(na.omit(df$unit_L4))
  unit_L5 <- unique(na.omit(df$unit_L5))
  
  unit <- ifelse(length(unit_L2) > 0, unit_L2,
                 ifelse(length(unit_L3) > 0, unit_L3,
                        ifelse(length(unit_L4) > 0, unit_L4, unit_L5))) %>% 
    remove_whitespace %>% 
    replace_slash_with_dash %>% 
    replace_colon_with_underscore %>% 
    remove_unit_prefix %>% 
    as.character
  
  # 수치 데이터 열의 인덱스 계산
  start_col <- which(names(df) == "행") + 2
  end_col <- which(names(df) == "Categorized_L3_New") - 1
  
  # numeric 변환
  df <- convert_columns_to_numeric(df, start_col_index = 4, end_col_index = end_col)
  
  # 마지막 열 확인
  if (is.character(df[, end_col])) {
    end_col <- end_col - 1
  }
  
  # 새로운 경로
  path_new <- paste0(path, "/", categorized_L3, "/", year)
  dir.create(path_new, showWarnings = FALSE, recursive = TRUE)
  
  # 수치 데이터를 반복하여 파일로 저장
  error_log <- list()
  tictoc::tic()
  
  # 외부 tryCatch 블록으로 전체 루프를 감쌉니다.
  tryCatch({
    for (i in 1:nrow(df)) {
      for (j in start_col:end_col) {
        value <- df[i, j] %>% unlist
        
        # NA는 0으로
        if (is.na(value)) {
          value <- 0
        }
        
        # "데이터없음"이면 다음 루프로 넘어가기
        if (value == "데이터없음") {
          next
        }
        
        # 파일 이름 생성
        file_name <- sprintf("%s_%s___%s___%s___%s.csv",
                             year,
                             names(df)[j] %>% remove_na_suffix %>% replace_slash_with_dash %>% remove_non_korean_characters,
                             df[i, start_col - 1] %>% remove_non_korean_characters,
                             unit,
                             id) %>% 
          remove_whitespace %>% replace_colon_with_underscore %>% replace_slash_with_dash
        
        # 파일 저장 경로
        file_path <- file.path(path_new, file_name)
        
        # 파일로 저장
        if (file.exists(file_path)) {
          file_path = paste0(file_path, "_중복")
          # stop(paste("Error: File already exists -", file_path))
        }
        write_csv(data.frame(Value = value), file_path)
      }
    }
  }, error = function(e) {
    error_log[[length(error_log) + 1]] <- list(
      error = e$message,
      ID = df$ID[i],
      Categorized_L3_New = df$Categorized_L3_New[i],
      file_name = file_name
    )
    message(e$message) # 에러 메시지를 출력
  })
  
  # 에러 로그를 파일로 저장
  if (length(error_log) > 0) {
    error_log_df <- do.call(rbind, lapply(error_log, as.data.frame))
    write_csv(error_log_df, file.path(path, "error_log.csv"))
  }
  
  cat("\n", crayon::green("Exported : "), crayon::bgMagenta(id), "\n")
  tictoc::toc()
  
  return(error_log)
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
###  🟨 영어 알파벳을 제거하는 함수 =======================================================================
remove_english_letters <- function(strings) {
  # 정규 표현식을 사용하여 영어 알파벳을 제거
  return(gsub("[a-zA-Z]", "", strings))
}

###  🟨 한글 이외의 문자를 제거하는 함수 =======================================================================
remove_non_korean_characters <- function(strings) {
  # 정규 표현식을 사용하여 한글과 숫자 이외의 모든 문자 제거
  return(gsub("[^가-힣0-9]", "", strings))
}


###  🟨 공백을 제거하는 함수 =======================================================================
remove_whitespace <- function(strings) {
  # 정규 표현식을 사용하여 공백 제거
  return(gsub("\\s", "", strings))
}


###  🟨 슬래시를 대시로 바꾸는 함수 =======================================================================
replace_slash_with_dash <- function(strings) {
  # 정규 표현식을 사용하여 슬래시를 대시로 대체
  return(gsub("/", "-", strings))
}


###  🟨 "_NA" 문자열을 제거하는 함수 =======================================================================
remove_na_suffix <- function(strings) {
  # 정규 표현식을 사용하여 "_NA" 제거
  return(gsub("_NA", "", strings))
}



###  🟨 콜론을 =로 바꾸는 함수 =======================================================================
replace_colon_with_underscore <- function(strings) {
  # 정규 표현식을 사용하여 콜론을 언더스코어로 대체
  return(gsub(":", "=", strings))
}


###  🟨 "단위-"를 제거하는 함수 =======================================================================
remove_unit_prefix <- function(strings) {
  # 정규 표현식을 사용하여 "단위-" 제거
  return(gsub("단위-", "", strings))
}




## 🟧 함수 정의 ================================================================================
viewer = function(data.list, element){
  elements_full = sapply(data.list, function(x){
    x[1,3] %>% unlist
  }) %>% unname
  
  
  
  selected = data.list[which(elements_full %in% element)]
  if(length(selected) == 1){
    return(selected[[1]])
  }else{
    return(selected)
  }
}

extract_id = function(data){
  
  extract_id_df = function(df){
    df[,1] %>% unlist %>% unname %>% unique  
  }
  
  if(is.data.frame(data)){
    data %>% extract_id_df %>% return
  }else if(is.list(data)){
    sapply(data, extract_id_df) %>% unlist %>% unname %>% return
  }
}


exclude_element = function(vec, ex){
  vec[!vec %in% ex]
}


check_yb = function(data, ybid){
  selected = data[names(data)%in%ybid]
  if(length(selected)==0){
    cat("no data")
  }else if(length(selected)==1){
    selected[[1]]
  }else{
    selected
  }
}


add_to_check_id <- function(element, selected) {
  if (is.null(check_id[[element]])) {
    check_id[[element]] <<- extract_id(selected)
  } else {
    check_id[[element]] <<- c(check_id[[element]], extract_id(selected))
  }
}




### 🟨 특정 열을 numeric으로 변환하는 함수 =============================================
convert_to_numeric_if_possible <- function(column) {
  numeric_column <- suppressWarnings(as.numeric(column))
  if (all(is.na(numeric_column) == is.na(column))) {
    return(numeric_column)
  } else {
    return(column)
  }
}

### 🟨 지정된 인덱스부터 특정 이전 인덱스까지의 열들을 numeric으로 변환하는 함수 =============================================
convert_columns_to_numeric <- function(df, start_col_index, end_col_index) {
  # 유효한 인덱스인지 확인합니다.
  if (start_col_index <= end_col_index) {
    # 지정된 범위의 열들을 numeric으로 변환합니다.
    for (i in start_col_index:end_col_index) {
      df[[i]] <- convert_to_numeric_if_possible(df[[i]])
    }
  } else {
    stop("시작 인덱스가 종료 인덱스보다 작거나 같아야 합니다.")
  }
  
  return(df)
}

### 🟨 에러 다음 연보 이름=============================================
get_next_name <- function(name, data_list) {
  # 입력한 이름의 인덱스 찾기
  index <- which(names(data_list) %in% name)
  
    
  # 인덱스 유효성 검사
  return(names(data_list)[1 + index])
}
