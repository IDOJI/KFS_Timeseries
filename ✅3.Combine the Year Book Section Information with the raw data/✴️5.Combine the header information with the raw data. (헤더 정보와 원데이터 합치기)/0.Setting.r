list.files("/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/숲가꾸기/숲 가꾸기Forest tending")


# rm(list = ls())
filter = dplyr::filter
select = dplyr::select

# 🟥 check last col #####################################################################################################
check_last_col = function(ith_data_combined){
  
  ith_last_col = ith_data_combined[,ncol(ith_data_combined)] %>% unlist %>% unname
  ith_last_col_name = names(ith_data_combined)[ncol(ith_data_combined)]
  
  if((ith_last_col %>% unlist %>% is.character) | grepl("classification", ith_last_col_name, T) | is_continuous_year_vector(ith_last_col)){
    
    ind = grep("구분", names(ith_data_combined))
    
    if(length(ind)!=1){
      
      ind = 4
      
    }
    
    ith_col = ith_data_combined[,ind] %>% unlist %>% unname  
    
    
    if(ith_col %>% is.character){
      
      last_col_elements = ith_last_col %>% unique
      
      if(length(last_col_elements) == 1){
        
        if(! last_col_elements  %>% is.na){
          
          ith_data_combined[,ind] = paste0(ith_col, "_", ith_last_col)    
          
        }else{
          
          ith_data_combined[,ncol(ith_data_combined)] = NULL
          
        }  
      }else if(last_col_elements %>% last_col_elements_year){
        
        ith_data_combined[,ncol(ith_data_combined)] = NULL
        
      }
    }  
  }
  
  return(ith_data_combined)
}

## 🟧 마지막 행 지우기 ===========================================================================================
remove_last_row_if_total_or_na <- function(df) {
  # 데이터프레임의 3번째 열의 마지막 행의 값을 체크
  last_value <- df[nrow(df), 3]
  
  # 마지막 행의 값이 NA인지 먼저 확인
  if (is.na(last_value)) {
    df <- df[-nrow(df), ]
  } 
  # 마지막 행의 값이 "계"이면 그 행을 삭제
  else if (last_value == "계") {
    df <- df[-nrow(df), ]
  }
  
  return(df)
}
## 🟧 마지막 행이 전부 NA이면 삭제 ===========================================================================================
remove_last_row_if_all_na <- function(df) {
  # 마지막 행의 3번째 열부터 마지막 열까지의 값을 추출
  last_row_values <- df[nrow(df), 3:ncol(df)]
  
  # 모든 값이 NA인지 확인
  if (all(is.na(last_row_values))) {
    df <- df[-nrow(df), ]
  }
  
  return(df)
}
## 🟧 3번째 열에서 공백 제거 ===========================================================================================
remove_spaces_from_third_column <- function(df) {
  # 3번째 열의 값에서 공백 제거
  df[, 3] <- gsub(" ", "", df[, 3])
  return(df)
}
## 🟧 마지막 열이 문자열이면 합치고 제거 ===========================================================================================
combine_third_and_last_columns <- function(df) {
  # 마지막 열의 데이터 유형이 character인지 확인
  if (is.character(df[[ncol(df)]])) {
    # 3번째 열과 마지막 열의 값을 "_"로 결합
    df[, 3] <- paste(df[, 3], df[[ncol(df)]], sep = "_")
    # 마지막 열 삭제
    df <- df[, -ncol(df)]
  }
  return(df)
}

## 🟧 ~연도를 연도까지로 바꾸기 ===========================================================================================
replace_year_format_in_third_column <- function(df) {
  # 3번째 열의 값에서 "~연도" 형식을 "연도까지"로 바꿈
  df[, 3] <- gsub("~([0-9]+)", "\\1까지", df[, 3])
  return(df)
}

## 🟧 소관 표시열 수정 ===========================================================================================
process_dataframe <- function(df) {
  # 마지막 열을 확인하여 "소관임"이 포함된 행이 있는지 확인
  last_col <- df[[ncol(df)]]
  if (any(grepl("소관임", na.omit(last_col)))) {
    # 괄호 표기와 소관을 추출
    patterns <- unique(na.omit(last_col))
    
    # 패턴과 대응되는 소관을 저장할 리스트 생성
    mapping <- list()
    for (pattern in patterns) {
      if (grepl("<>", pattern)) {
        mapping[["<>"]] <- sub(".*<>내는(.*)소관임", "\\1소관", pattern)
      } else if (grepl("\\(\\)", pattern)) {
        mapping[["()"]] <- sub(".*\\(\\)내는(.*)소관임", "\\1소관", pattern)
      } else if (grepl("\\[\\]", pattern)) {
        mapping[["[]"]] <- sub(".*\\[\\]내는(.*)소관임", "\\1소관", pattern)
      }
    }
    
    # 3번째 열과 4번째 열 사이에 새로운 열 "구분" 추가
    df$구분 <- NA
    df <- df[, c(1:3, ncol(df), 4:(ncol(df)-1))]
    
    # 4번째 열의 각 원소를 검사하여 "구분" 열과 원소를 수정
    for (i in 1:nrow(df)) {
      value <- df[i, 5]
      if (!is.na(value)) {
        if (grepl("<.*>", value)) {
          df$구분[i] <- ifelse(!is.null(mapping[["<>"]]), mapping[["<>"]], NA)
          df[i, 5] <- gsub("[<>]", "", value)
        } else if (grepl("\\(.*\\)", value)) {
          df$구분[i] <- ifelse(!is.null(mapping[["()"]]), mapping[["()"]], NA)
          df[i, 5] <- gsub("[()]", "", value)
        } else if (grepl("\\[.*\\]", value)) {
          df$구분[i] <- ifelse(!is.null(mapping[["[]"]]), mapping[["[]"]], NA)
          df[i, 5] <- gsub("[\\[\\]]", "", value)
        }
      }
    }
    
    # 마지막 열 삭제
    df <- df[, -ncol(df)]
  }
  
  return(df)
}




## 🟧 괄호들 제거 ===========================================================================================
remove_brackets <- function(df) {
  # 세 번째 열에 대해 괄호 제거
  df[, 3] <- str_replace_all(df[, 3], "\\(|\\)|<|>|\\[|\\]", "")
  return(df)
}

## 🟧 열에서 괄호들 제거 ===========================================================================================
remove_brackets_from_columns <- function(df) {
  # 네 번째 열부터 마지막 열까지 반복
  for (col in 4:ncol(df)) {
    # 모든 괄호 문자열 제거
    df[, col] <- str_replace_all(df[, col], "\\(|\\)|<|>|\\[|\\]", "")
  }
  return(df)
}


## 🟧 NA값 대체  ===========================================================================================
# 함수 정의
replace_with_na_NF <- function(df) {
  # 네 번째 열의 이름이 "구분"인지 확인
  if (names(df)[4] == "구분") {
    # 세 번째 열과 네 번째 열의 값들을 반복
    for (i in 1:nrow(df)) {
      # 특정 조건에 맞는지 확인 및 NA 값 체크
      if (!is.na(df[i, 3]) && !is.na(df[i, 4]) && startsWith(as.character(df[i, 4]), as.character(df[i, 3]))) {
        df[i, 4] <- NA  # 네 번째 열의 값을 NA로 변경
      }
    }
  }
  return(df)
}

## 🟧 4번째 열부터 숫자로 바꾸는 함수===========================================================================================
convert_columns_to_numeric_from_four <- function(df) {
  # 네 번째 열부터 마지막 열까지 반복
  for (col in 4:ncol(df)) {
    # 가능한 경우 수치형으로 변환
    df[, col] <- suppressWarnings(as.numeric(as.character(df[, col])))
  }
  return(df)
}

# 🟧 열이름에서 cdot 제거 ===========================================================================================
remove_cdot <- function(df) {
  names(df) <- gsub("ㆍ", "", names(df))
  return(df)
}

# 🟧 total 열 제거 ===========================================================================================
remove_total_columns <- function(df) {
  # 마지막 열부터 시작하여 검사
  while (ncol(df) > 0) {
    last_col_index <- ncol(df)
    last_col_values <- df[, last_col_index]
    # 마지막 열의 값들에 "total"이 포함되어 있는지 확인
    if (any(grepl("total", last_col_values, ignore.case = TRUE))) {
      df <- df[, -last_col_index]
    } else {
      break
    }
  }
  return(df)
}


# 🟧 classification 이란 원소 제거 ===========================================================================================
remove_classification <- function(vec) {
  while (length(vec) > 0) {
    last_element <- tail(vec, n = 1)
    if (grepl("classification", last_element, ignore.case = TRUE)) {
      vec <- vec[-length(vec)]
    } else {
      break
    }
  }
  return(vec)
}

# 🟧 구분1, 구분2 NA 값들 채우기 ===========================================================================================
library(dplyr)

# 함수 정의
# 함수 정의
fill_na_with_previous <- function(df) {
  # 마지막 문자열 열 찾기
  last_string_col <- max(which(sapply(df, function(col) {
    is.character(col) || is.factor(col)
  })))
  
  # 조건에 맞는지 확인 및 NA 값을 이전 값으로 채우기
  for (col_index in 3:(last_string_col - 1)) {
    column <- df[[col_index]]
    if (all(is.na(column) | !is.na(column))) {
      # NA 값을 이전 값으로 채우기
      for (i in 2:length(column)) {
        if (is.na(column[i])) {
          column[i] <- column[i - 1]
        }
      }
    }
    df[[col_index]] <- column
  }
  
  return(df)
}

# 🟧 NA인 마지막 원소 삭제 ===========================================================================================
remove_na_string <- function(str_vector) {
  # 문자열 벡터의 길이를 가져옵니다.
  len <- length(str_vector)
  
  # 마지막 원소가 NA인 경우 "NA" 문자열로 바꿉니다.
  if (is.na(str_vector[len])) {
    str_vector[len] <- "NA"
  }
  
  # 문자열 벡터의 마지막 원소를 "_" 기준으로 split 합니다.
  last_element <- strsplit(str_vector[len], "_")[[1]]
  
  # split 결과가 모두 "NA" 인 경우 마지막 원소를 삭제합니다.
  if (all(last_element == "NA")) {
    str_vector <- str_vector[-len]
  }
  
  return(str_vector)
}


# 🟧 마지막 열이 문자열이면 제거 ===========================================================================================
remove_last_column_if_not_numeric_or_year_sequence <- function(df) {
  # 데이터프레임의 마지막 열을 가져옵니다.
  last_col <- df[[ncol(df)]]
  
  # 마지막 열이 전부 NA인지 확인합니다.
  if (all(is.na(last_col))) {
    return(df)
  }
  
  # 문자열로 변환
  last_col_str <- as.character(last_col)
  
  # 수치형으로 변환할 수 없는 문자열이 있는지 확인합니다.
  is_not_numeric <- any(is.na(suppressWarnings(as.numeric(last_col_str))))
  
  # 연도인지 확인하기 위해 수치형으로 변환할 수 있는 부분을 필터링합니다.
  numeric_part <- suppressWarnings(as.numeric(last_col_str))
  
  # 연속된 1년 단위 차이인지 확인합니다.
  is_year_sequence <- length(numeric_part) > 1 && all(diff(na.omit(numeric_part)) == 1)
  
  # 조건을 만족하면 마지막 열을 삭제합니다.
  if (is_not_numeric || is_year_sequence) {
    df <- df[, -ncol(df)]
  }
  
  return(df)
}



# 🟧 맨마지막행이  NA인 행들 제거 ===========================================================================================
remove_rows_with_na <- function(df) {
  # 데이터 프레임의 행 수를 가져옵니다.
  num_rows <- nrow(df)
  num_cols <- ncol(df)
  
  # 마지막 행부터 차례대로 3번째 열부터 마지막 열까지의 값이 모두 NA인 행을 삭제합니다.
  for (row in num_rows:1) {
    if (all(is.na(df[row, 3:num_cols]))) {
      df <- df[-row, ]
    }
  }
  
  return(df)
}


# 🟧 맨마지막열의 NA인 열들 제거 ===========================================================================================
remove_last_col_nas <- function(df, ith_hdr) {
  # 데이터 프레임의 열 수를 가져옵니다.
  num_cols <- ncol(df)
  # ith_data_2 %>% View
  
  # 맨 마지막 열부터 왼쪽으로 차례로 열들을 점검해서 전부 NA인 값을 가진 열을 지웁니다.
  for (col in num_cols:1) {
    if (all(is.na(df[[col]]))) {
      df <- df[, -col]
    } else {
      break
    }
  }
  
  # 현재 데이터프레임의 열 수를 가져옵니다.
  current_num_cols <- ncol(df)
  ith_hdr_length <- length(ith_hdr)
  
  # ith_hdr의 길이보다 df의 열의 개수가 작으면, 부족한 열의 개수만큼 열을 추가합니다.
  if (current_num_cols < ith_hdr_length) {
    num_new_cols <- ith_hdr_length - current_num_cols
    new_cols <- matrix(NA, nrow = nrow(df), ncol = num_new_cols)
    colnames(new_cols) <- paste0("new_col_", seq(current_num_cols + 1, ith_hdr_length))
    df <- cbind(df, new_cols)
  }
  
  return(df)
}

# 🟧 맨마지막열이 문자열이면 삭제 ===========================================================================================
remove_non_numeric_columns <- function(df) {
  # 마지막 열이 수치형으로 변환될 수 없는 경우 열을 삭제하는 작업을 반복합니다.
  while (ncol(df) > 0) {
    last_col <- df[[ncol(df)]]
    
    # 마지막 열이 모두 NA인 경우 중지
    if (all(is.na(last_col))) {
      break
    }
    
    # 괄호 안의 숫자 또는 숫자로 변환 가능한 문자열인지 확인
    is_numeric_or_bracketed_numeric <- function(x) {
      is_numeric <- suppressWarnings(!is.na(as.numeric(x)))
      is_bracketed_numeric <- grepl("^\\(\\d+\\)$", x)
      return(is_numeric | is_bracketed_numeric)
    }
    
    # 열의 모든 값이 위의 조건을 만족하는지 확인
    is_numeric_col <- all(sapply(last_col, function(x) {
      is.na(x) || is_numeric_or_bracketed_numeric(x)
    }))
    
    if (!is_numeric_col) {
      # 마지막 열을 삭제
      df <- df[, -ncol(df)]
    } else {
      # 수치형으로 변환 가능한 열이 나오면 중지
      break
    }
  }
  
  return(df)
}
# 🟧 수치형으로 바꾸기 ===========================================================================================
convert_to_numeric_2 <- function(df) {
  # 4번째 열부터 마지막 열까지 순회
  for (col in 4:ncol(df)) {
    # 현재 열을 시도하여 수치형으로 변환 가능한지 확인
    is_convertible <- grepl("^(-?\\d+\\.?\\d*|\\.\\d+)$", df[[col]]) | is.na(df[[col]])
    
    # 수치형으로 변환 가능한 값만 변환
    if (all(is_convertible)) {
      df[[col]] <- as.numeric(df[[col]])
    }
  }
  
  return(df)
}

# 🟧 연속된 연도인 마지막 열 삭제 ===========================================================================================
remove_last_column_if_consecutive_years <- function(df) {
  last_col <- df[[ncol(df)]]
  
  # NA 값을 제외하고 모든 값이 4자리 숫자(연도)인지 확인
  valid_years <- na.omit(last_col)
  is_year <- all(grepl("^\\d{4}$", valid_years))
  
  if (is_year) {
    # 수치형으로 변환
    years <- as.numeric(valid_years)
    
    # 연도가 연속된 것인지 확인
    is_consecutive_years <- all(diff(sort(years)) == 1)
    
    # 연속된 연도이면 마지막 열을 삭제
    if (is_consecutive_years) {
      df <- df[, -ncol(df)]
    }
  }
  
  return(df)
}



# 🟧 연속된 연도인 마지막 열 삭제 ===========================================================================================
process_NFS <- function(df, yb) {
  # ID 열의 unique한 값 확인
  unique_id <- unique(df$ID)
  
  # yb 데이터프레임에서 조건에 맞는 행 확인
  if (nrow(yb %>% filter(ID == unique_id & Categorized_L3_New == "행정구역별 산림면적 및 임목축적_Forest Land Area and Growing Stock by Administrative Districts")) > 0) {
    
    # 3번째 열에서 중복값을 갖는 행들 찾기
    dup_rows <- df %>% filter(duplicated(df[[3]]) | duplicated(df[[3]], fromLast = TRUE))
    
    # 중복된 3번째 열의 값에 대해 4번째 및 5번째 열의 값이 (숫자) 형태인지 확인
    for (i in seq_len(nrow(df))) {
      if (df[i, 3] %in% dup_rows[[3]]) {
        if (grepl("^\\(\\d+\\)$", df[i, 4])) {
          # 3번째 열의 값 뒤에 "_영림서소관" 추가
          df[i, 3] <- paste0(df[i, 3], "_영림서소관")
          
          # 4번째 열의 괄호 제거
          df[i, 4] <- gsub("[()]", "", df[i, 4])
        }
        if (grepl("^\\(\\d+\\)$", df[i, 5])) {
          # 3번째 열의 값 뒤에 "_영림서소관" 추가
          df[i, 3] <- paste0(df[i, 3], "_영림서소관")
          
          # 5번째 열의 괄호 제거
          df[i, 5] <- gsub("[()]", "", df[i, 5])
        }
      }
    }
  }
  
  return(df)
}

# 🟧 NA  행들 삭제  ===========================================================================================
remove_na_rows <- function(df) {
  # 특정 행을 찾기 위해 3번째 열의 값을 검사
  non_na_index <- max(which(!is.na(df[[3]])))
  
  # 특정 행 이후부터 4번째 열에서 마지막 열까지 모든 값이 NA인 행을 찾기
  rows_to_check <- df[(non_na_index + 1):nrow(df), ]
  rows_to_remove <- apply(rows_to_check[, 4:ncol(df)], 1, function(row) all(is.na(row)))
  
  # 해당 행 삭제
  df <- df[!(1:nrow(df) %in% (non_na_index + which(rows_to_remove))), ]
  
  return(df)
}


library(dplyr)

remove_first_row_if_conditions_met <- function(df) {
  if ("행" %in% colnames(df)) {
    # "행" 열을 numeric으로 변환
    df <- df %>%
      mutate(행 = as.numeric(행))
    
    # 첫 번째 행의 "행" 열 값이 0인지 확인
    if (df$행[1] == 0) {
      # 첫 번째 행의 3번째 열부터 마지막 열까지의 값 확인
      first_row_values <- df[1, 3:ncol(df)]
      
      # 모든 값이 숫자가 포함되지 않은 문자열인지 확인
      is_non_numeric <- sapply(first_row_values, function(x) !grepl("\\d", x))
      
      if (all(is_non_numeric)) {
        # 조건이 충족되면 첫 번째 행을 제거
        df <- df[-1, ]
      }
    }
  }
  
  return(df)
}


# 🟥 ith_data 함수 #####################################################################################################
treat_data = function(ith_data, ith_hdr, yb){
  ## 🟧 데이터 1차 정제 ============================================================================
  # View(ith_data)
  # View(ith_data_2)
  # ith_data_2$열000
  ith_data_2 = ith_data %>% 
    rename(행 := 데이터행) %>% 
    arrange(행) %>% 
    rename(ID = 연보.ID) %>% 
    remove_na_columns() %>% 
    remove_columns_until_clmn_not_found() %>%  # 마지막 열 	CLMN029 확인
    remove_na_columns() %>% 
    remove_na_columns_from_last %>% # 마지막 열부터 모든 원소가 NA인 열들 제거
    remove_commas_from_columns(start_col = 3) %>% # 숫자에서 "," 지우기 
    replace_dash_with_na(3) %>% # 특정 열 이후에 "-"를 NA로 바꾸기
    replace_backtick_with_na(., 3) %>%  # 특정 열 이후 "`"을 NA로 바꾸기
    convert_to_numeric(., 3) %>% 
    arrange(행) %>%     # "행" 열 이후의 열에서 numeric으로 바꿀 수 있으면 바꾸기
    fill_na_with_previous_year() %>%  # 행 다음 열이 연도열일 때, NA인 부분을 연도로 채우기
    # check_for_ref() %>%  # 4번째 열부터 확인 : 모든 데이터 확인 완료 (오직 "YRBK_0045060301")대해서만 존재 
    convert_error_dataframe() %>%    # REF 와 같은 에러 셀 NA 처리 (오직 "YRBK_0045060301")대해서만 존재
    update_and_remove_rows %>%  # NA와 (숫자)데이터 행 지우고 이전 행의 NA 부분을 (숫자로 채우기) : YRBK_00280404 
    update_and_remove_rows_v3 %>%  # 마지막 열에 "한글 영어 번갈아 나오는 경우 "한글_영어"로 합치기 "YRBK_00160317"
    remove_consecutive_rows %>%   # 연속된 연도열에서, 중복값이 존재하고, 그 중복된 행의 데이터가 (숫자) + NA만 존재하는 경우 삭제
    remove_newline_from_colnames %>%  # 열이름 문자열에서 "new line" 제거
    remove_columns_with_same_values_as_third %>%   # 3번째 열과 동일한 열이 중간에 존재하는 경우 제외
    # add_region_prefix_based_on_pattern %>% # 거래형 비거래형 같은 반복 문자열 처리
    remove_na_prefix_from_third_column %>%  # 3번째 열의 원소에서 NA_ 문자열 제거
    add_chk_suffix %>%   # 행정구역 이름이 여러 개 있는 경우
    # remove_na_columns() %>%  # 마지막 열의 NA 열 지우기
    remove_first_row_if_conditions_met %>%  # 첫번째 열이 문자열이면 삭제
    remove_commas_from_columns(start_col = 3) %>%  # 숫자에서 "," 지우기 
    remove_last_row_if_total_or_na %>%   # 마지막 행이 "계"이면 지우기
    convert_to_numeric_2 %>%  # 수치형으로 열들 바꾸기
    remove_non_numeric_columns %>%  # 마지막열이 문자열이면 삭제
    remove_last_column_if_consecutive_years # 마지막 열이 연도이면 삭제

  
  # View(ith_data_2 )
  # ith_data_2[1:50,1:4] %>% as_tibble
  # ith_data_2[,1:4] %>% tail
  
  
  
  ## 🟧 헤더 열이름 추가 ============================================================================  
  # colnames(ith_data_2) %>% length
  # length(ith_hdr)
  # View(ith_data_2)
  # yb %>% filter(ID == ith_id) %>% View
  ### 🟨 마지막열에 total이 있으면 지움 =============================================================
  ith_data_2 = ith_data_2 %>% 
    remove_total_columns
    # remove_last_column_if_not_numeric_or_year_sequence() # 연도이거나 문자열인 마지막 열 삭제
  ith_hdr = ith_hdr %>% 
    remove_classification %>%  # classification 원소이면 삭제
    remove_na_string  # NA인 마지막 원소 삭제
  # dim(ith_data_2)
  # length(ith_hdr)
  # last_col = unlist(ith_data_2[,ncol(ith_data_2)]) %>% unname
  # 
  # ith_data_2
  # 
  #   # if (is.character(last_col)) {
  # #   is_last_col_character = is_mostly_non_numeric(last_col)
  # #   is_last_col_year = FALSE  # 문자형이면 연도가 될 수 없으므로 FALSE로 설정
  # # } else {
  # #   is_last_col_character = FALSE  # 숫자형이면 문자형 판별을 하지 않으므로 FALSE로 설정
  # #   is_last_col_year = is_consecutive_years(last_col)
  # # }
  # 
  # 
  # if(is_last_col_character){
  #   ith_data_2 = remove_character_columns_last(ith_data_2) 
  #   ith_hdr = ith_hdr %>% 
  #     remove_classification %>% 
  #     
  #   
  # } 
  # 
  
  
  ### 🟨 벡터 길이 == 열길이 =============================================================
  if(length(ith_hdr) == length(colnames(ith_data_2))){
    # length(ith_hdr)
    # length(colnames(ith_data_2))
    colnames(ith_data_2) = ith_hdr
    # View(ith_data_2)
    # dim(ith_data_2)
    ### 🟨 벡터길이 > 열길이  =============================================================  
  }else if(length(ith_hdr) > length(colnames(ith_data_2))){
      
      # 필요한 추가 열의 수
      extra_cols <- length(ith_hdr) - length(colnames(ith_data_2))
      
      # NA로 채워진 새로운 열 생성
      for (i in 1:extra_cols) {
        ith_data_2[[paste0("extra_col", i)]] <- NA
      }
      
      colnames(ith_data_2) = ith_hdr
      
      ### 🟨 벡터길이 < 열길이  =============================================================  
  }else{
    
    ith_data_2 = ith_data_2 %>% 
      remove_last_col_nas(., ith_hdr) %>% 
      remove_rows_with_na
    
    
    # 임산물 생산 실적?
    forest_production_id = yb %>% 
      filter(Categorized_L3_New == "임산물 생산실적_Production of Forest Products") %>% 
      pull(ID)
    
    if(unique(ith_data_2$ID) %in% forest_production_id){
      
      # 열 이름 벡터의 길이와 데이터프레임 열 수 비교
      if (length(ith_hdr) < ncol(ith_data_2)) {
        
         ith_data_2 = ith_data_2 %>% remove_na_rows
        
        # 부족한 열 이름을 "열이름없음"으로 채움
        ith_hdr <- c(ith_hdr, rep("열이름없음", ncol(ith_data_2) - length(ith_hdr)))
        
        # 데이터프레임의 열 이름 설정
        colnames(ith_data_2) <- ith_hdr
        
      }else{
        stop("Check colnames!")   
      }
      
    }else{
      
      if(ncol(ith_data_2) == length(ith_hdr)){
       
        colnames(ith_data_2) = ith_hdr
         
      }else{
        stop("Check colnames!")      
      }
    }
  }
    
  # View(ith_data_2)
  
  
  ## 🟧 1행, 2행의 문자열 열이름에 추가 ============================================================================  
  # 만약 data의 첫 번째 행이 문자열만 있는 경우
  the_first_row = ith_data_2[1,-c(1,2)] %>% unlist %>% unname
  the_second_row = ith_data_2[2,-c(1,2)] %>% unlist %>% unname
  the_third_row = ith_data_2[3,-c(1,2)] %>% unlist %>% unname
  
  if(is_strictly_character(the_first_row)){
    
    colnames(ith_data_2)[-c(1,2)] = combine_columns_with_na(colnames(ith_data_2)[-c(1,2)], the_first_row) %>% unname
    ith_data_2 = ith_data_2[-1, ] %>%
      convert_to_numeric(., 3)  # "행" 열 이후의 열에서 numeric으로 바꿀 수 있으면 바꾸기
    
    if(is_strictly_character(the_second_row)){
      
      colnames(ith_data_2)[-c(1,2)] = combine_columns_with_na(colnames(ith_data_2)[-c(1,2)], the_second_row) %>% unname
      ith_data_2 = ith_data_2[-1, ] %>%
        convert_to_numeric(., 3)  # "행" 열 이후의 열에서 numeric으로 바꿀 수 있으면 바꾸기
      
      if(is_strictly_character(the_third_row)){
        
        colnames(ith_data_2)[-c(1,2)] = combine_columns_with_na(colnames(ith_data_2)[-c(1,2)], the_third_row) %>% unname
        ith_data_2 = ith_data_2[-1, ] %>%
          convert_to_numeric(., 3)  # "행" 열 이후의 열에서 numeric으로 바꿀 수 있으면 바꾸기
        
      } 
    }
  }
  ith_data_2 = ith_data_2 %>% remove_commas_from_columns(start_col = 3) # 숫자에서 "," 지우기 
  # View(ith_data_2)
  

  
  
  ## 🟧 열 제거/통합 ============================================================================  
  # ith_data_2[,4]
  # ith_data_2$총계_Grandtotal_NA_NA_NA_NA %>% unname
  # View(ith_data_3)
  # View( ith_data_2)
  # ith_data_3$구분
  # yb %>% filter(ID == "YRBK_00060209")
  ith_data_3 = ith_data_2 %>% 
    process_NFS(., yb) %>% # 영림서 소관 행정구역별 임목축적 처리
    process_dataframe %>%  # 소관 표시열 제거 및 소관 열 생성
    remove_brackets %>%
    remove_brackets_from_columns %>%
    replace_with_na_NF %>%   # 4번째 "구분"열 영림서 제거
    # check_numeric_column %>%  # 4번째 열에 문자형 데이터가 있는 경우
    # remove_character_columns(., 2) %>%  # numeric인 열들 사이에 있고,  괄호 안의 숫자인 데이터 삭제 (YRBK_001202030101 예시로 확인)
    remove_commas_from_columns(start_col = 3) %>% # 숫자에서 "," 지우기 
    replace_dash_with_na(3) %>% # 특정 열 이후에 "-"를 NA로 바꾸기
    convert_to_numeric(., 4) %>%   # "구분" 열 이후의 열에서 numeric으로 바꿀 수 있으면 바꾸기
    fill_na_with_previous %>%  #  구분 열에서 NA들을 이전 값으로 채우기
    remove_last_row_if_all_na %>%   # 3번째 열의 마지막 행의 NA인지 확인하고 없애기
    replace_na_colnames %>% # 열이름의 NA를 NA_1로 변경
    # remove_and_convert_numeric_count_data %>%  # 개소라는 문자열 제거
    combine_character_columns(., key_col_name = "행") %>%   # 열이름 문자열 합치기 : 국유림_합계_계
    check_and_convert_last_column() %>% # 마지막열이 연도이면 character로 바꿈
    # combine_last_column() %>%  # 마지막 열 "구분" 열에 이름 문자열 추가로 합치기
    replace_slash_in_colnames %>%  # 열이름에서 슬래시 삭제
    remove_last_row_if_na %>%  # 마지막 행이 NA이면 제거
    convert_to_numeric(., 3) %>%  # 데이터를 수치로 바꾸기
    remove_spaces_from_colnames %>%  # 열이름 공백 제거
    replace_question_mark_in_colnames %>%  # 열이름 ? 제거
    remove_na_prefix_from_colnames %>%  # 열이름에서 NA_NA_NA 문자열 제거
    remove_commas_from_columns(start_col = 3) %>%  # 숫자에서 "," 지우기 
    add_numbering_to_duplicates %>%  # 중복원소에 넘버링
    remove_spaces_from_third_column %>%  # 3번째 열의 공백 제거
    replace_year_format_in_third_column %>%  # 3번째 열의 "~연도"를 "연도까지"로 바꿈
    convert_columns_to_numeric_from_four %>%  # 4번째 열부터 수치형으로 바꾸기
    # combine_third_and_last_columns %>%  # 마지막 열이 문자열이면 합치고 제거
    remove_cdot # 열이름에서 cdot 제거
  # View(ith_data_3)
  
  
  return(ith_data_3)
}

# 🟥 벡터에서 중복 제거 함수 #####################################################################################################
remove_duplicates <- function(vec) {
  # 4번째 원소부터 검사
  if (length(vec) < 4) {
    return(vec)  # 벡터 길이가 4 미만인 경우, 그대로 반환
  }
  
  # 4번째 원소부터 중복 원소 확인 및 처리
  tail_elements <- vec[4:length(vec)]
  unique_elements <- unique(tail_elements)
  
  # 각 원소별로 중복 횟수 확인
  for (element in unique_elements) {
    occurrences <- which(tail_elements == element)
    if (length(occurrences) > 1) {
      # 중복이 있으면 "_숫자" 형태로 변경
      for (i in 1:length(occurrences)) {
        tail_elements[occurrences[i]] <- paste(element, i, sep="_")
      }
    }
  }
  
  # 수정된 뒷부분을 원본 벡터에 병합
  vec[4:length(vec)] <- tail_elements
  return(vec)
}


## 🟧 "구분"할당  함수 #####################################################################################################
assign_classification <- function(df) {
  # 3번째 열의 1번째 원소가 NA가 아니고 "구분"인지 확인
  if (!is.na(df[1, 3]) && df[1, 3] == "구분") {
    counter <- 1
    for (i in 4:ncol(df)) {
      # 각 열이 전부 NA인지 확인
      if (all(is.na(df[, i]))) {
        # 해당 열의 첫 번째 원소를 "구분_n"으로 할당
        df[1, i] <- paste0("구분_", counter)
        counter <- counter + 1
      } else {
        # "모든 원소가 NA"라는 조건을 만족하지 않는 열이 나타나면 중단
        break
      }
    }
  }
  return(df)
}
# View(ith_data_2)

## 🟧 헤더 파일에서 한글 영어 합치고 행 삭제 ===========================================================================================
merge_korean_english <- function(df) {
  # 데이터프레임의 행을 반복
  row_index <- 1
  while (row_index < nrow(df)) {
    # 각 열의 3번째 열부터 마지막 열까지 검사
    for (col_index in 3:ncol(df)) {
      korean_value <- df[row_index, col_index]
      if (row_index < nrow(df)) {
        english_value <- df[row_index + 1, col_index]
        
        # 윗 행의 값이 "한글"이고 아래 행의 값이 그 "한글" 단어의 "영어" 표기인 경우
        if (!is.na(korean_value) && !is.na(english_value) && grepl("[가-힣]", korean_value) && !grepl("[가-힣]", english_value)) {
          # 한글과 영어를 결합하여 "한글_영어" 형식으로 바꿈
          df[row_index, col_index] <- paste0(korean_value, "_", english_value)
          
          # 아래 행을 삭제
          df <- df[-(row_index + 1), ]
        }
      }
    }
    row_index <- row_index + 1
  }
  return(df)
}


## 🟧 헤더 파일에서 NA값  채우기  ===========================================================================================
fill_na_custom_all_rows <- function(df) {
  # 데이터 프레임의 행 수를 가져옵니다.
  num_rows <- nrow(df)
  
  # 행이 1개인 경우 함수를 적용하지 않습니다.
  if (num_rows <= 1) {
    return(df)
  }
  
  # 모든 행에 대해 순차적으로 규칙을 적용합니다.
  for (K in (num_rows - 1):1) {
    for (col in 3:(ncol(df) - 1)) {  # 1번 대신 3번 열부터 시작
      if (!is.na(df[K, col]) && !is.na(df[K + 1, col]) && is.na(df[K, col + 1])) {
        # 현재 열의 값을 저장합니다.
        fill_value <- as.character(df[K, col])
        
        # 다음 열에서부터 NA가 아닌 열들을 찾습니다.
        for (next_col in (col + 1):ncol(df)) {
          if (!is.na(df[K, next_col])) {
            break
          }
          if (!is.na(df[K + 1, next_col])) {
            df[K, next_col] <- fill_value
          }
        }
      }
    }
  }
  
  return(df)
}




## 🟧 NA 열 이름 채우기  ===========================================================================================
fill_na_custom_alternate <- function(df) {
  # 데이터 프레임의 행 수를 가져옵니다.
  num_rows <- nrow(df)
  num_cols <- ncol(df)
  
  # 행이 2개 이하인 경우 함수를 적용하지 않습니다.
  if (num_rows <= 2) {
    return(df)
  }
  
  # 맨 마지막 행의 위에 위에 행부터 시작합니다.
  for (K in (num_rows - 2):1) {
    for (col in 3:num_cols) {  # 3번 열부터 시작
      # K행의 열이 NA가 아니고, 마지막 행과 K행 사이에 NA들만 존재하는 열을 찾습니다.
      if (!is.na(df[K, col]) && all(is.na(df[(K + 1):(num_rows - 1), col]), na.rm = TRUE) && !is.na(df[num_rows, col])) {
        fill_value <- as.character(df[K, col])
        
        # K행의 C열 바로 다음 열부터 NA를 찾고 채웁니다.
        for (next_col in (col + 1):num_cols) {
          if (next_col > num_cols) {
            break
          }
          if (!is.na(df[K, next_col])) {
            break
          }
          if (!is.na(df[num_rows, next_col]) && all(is.na(df[(K + 1):(num_rows - 1), next_col]), na.rm = TRUE)) {
            df[K, next_col] <- fill_value
          }
        }
      }
    }
  }
  
  return(df)
}

## 🟧  임목축적에서 "_NA_" 문자열 제거  ===========================================================================================
replace_na_in_vector_stock <- function(str_vector) {
  # "임목축적"이 포함된 문자열에서만 "_NA_"를 "_"로 대체
  str_vector <- sapply(str_vector, function(x) {
    if (grepl("임목축적", x)) {
      gsub("_NA_", "_", x)
    } else {
      x
    }
  }, USE.NAMES = FALSE)
  return(str_vector)
}

## 🟧 "구분"에서 "_NA_" 문자열 제거  ===========================================================================================
replace_na_for_gubun <- function(str_vector) {
  str_vector <- sapply(str_vector, function(x) {
    if (grepl("구분", x)) {
      x <- gsub("_NA", "", x)
    }
    return(x)
  }, USE.NAMES = FALSE)
  return(str_vector)
}



# 🟥 ith_hdr 함수 #####################################################################################################
treat_header = function(header,  ith_id){
  # yb %>% filter(ID == "YRBK_0045020801") %>% View
  # dim(ith_hdr)
  # View(ith_hdr)
  # as.data.frame(ith_hdr)
  # ith_id = "YRBK_00450409"
  ith_hdr <- header %>% 
    filter(ID == ith_id) %>% 
    remove_na_columns() %>%
    remove_column_if_exists("언어 코드") %>% 
    remove_spaces() %>%  # 공백 지우기
    remove_last_na_or_zero() %>% 
    rename(행 = 헤더행) %>% 
    assign_classification %>%  # "구분"값 할당
    merge_korean_english %>% # NA 열 이름 채우기 1
    fill_na_custom_all_rows %>%  # NA 열 이름 채우기 2
    fill_na_custom_alternate %>% 
    combine_header_colnames() %>% 
    add_numbering %>% 
    unname %>% 
    insert_year_if_conditions_met %>%  # "연도"원소 추가
    remove_newline_from_vector %>%  # newline 문자열 제거
    remove_duplicates %>%   # 중복 문자열 제거
    replace_na_in_vector_stock %>% 
    replace_na_for_gubun # "구분" 문자열 있는 곳에서 ""로 변경
  
  return(ith_hdr)
}



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
# R 함수 파일들을 로드하는 함수
load_functions <- function(path_functions) {
  list.files(path_functions, pattern = "\\.R$", full.names = TRUE) %>%
    walk(~try(source(.x), silent = TRUE))
}

# 주어진 경로에서 자동으로 R 폴더를 찾고 함수를 읽는 함수
load_r_functions_from_path <- function(paths) {
  walk(paths, ~{
    # 주어진 경로가 디렉토리인지 확인
    if (dir.exists(.x)) {
      # R 폴더 경로 생성
      r_folder_path <- file.path(.x, "R")
      # R 폴더가 존재하는지 확인
      if (dir.exists(r_folder_path)) {
        load_functions(r_folder_path)
        message("R 폴더의 함수들을 로드했습니다: ", r_folder_path)
      } else {
        message("R 폴더가 존재하지 않습니다: ", r_folder_path)
      }
    } else {
      message("유효한 디렉토리가 아닙니다: ", .x)
    }
  })
}

path_packages = c("/Users/Ido/Library/CloudStorage/GoogleDrive-clair.de.lune.404@gmail.com/My Drive/GitHub/refineR",
                  "/Users/Ido/Library/CloudStorage/GoogleDrive-clair.de.lune.404@gmail.com/My Drive/GitHub/StatsR")

# 함수 호출
load_r_functions_from_path(path_packages)





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


## 🟧  NA가 나오기 직전의 값을 추출하는 함수 #############################################################################################################
get_last_non_na <- function(row) {
  last_value <- NA
  for (value in row) {
    if (is.na(value)) {
      break
    }
    last_value <- value
  }
  return(last_value)
}



## 🟧 텍스트 클러스터링 함수 정의 ##############################################################################################################
text_clustering <- function(text_data, special_cases = list(), k_min = 2, k_max = length(text_data)-1) {
  # 전처리된 텍스트 데이터를 저장할 벡터 생성
  cleaned_vector <- text_data
  
  # 특별 케이스를 적용
  for (case in names(special_cases)) {
    cleaned_vector <- gsub(case, special_cases[[case]], cleaned_vector)
  }
  
  # 숫자, 밑줄, 마침표 제거
  cleaned_vector <- gsub("[0-9_.]", "", cleaned_vector)
  
  # 텍스트 데이터를 코퍼스로 변환 (cleaned_vector 사용)
  corpus <- Corpus(VectorSource(cleaned_vector))
  
  # 용어 문서 행렬 생성
  tdm <- TermDocumentMatrix(corpus, control = list(wordLengths = c(1, Inf)))
  tdm_matrix <- as.matrix(tdm)
  tdm_matrix <- t(tdm_matrix)
  
  # TF-IDF 가중치 부여
  tfidf_transform <- weightTfIdf(tdm)
  tdm_matrix <- as.matrix(tfidf_transform)
  tdm_matrix <- t(tdm_matrix)
  
  # 사용되지 않는 열 제거
  non_zero_columns <- apply(tdm_matrix, 2, function(col) sum(col != 0)) > 1
  tdm_matrix <- tdm_matrix[, non_zero_columns]
  
  # 데이터 포인트의 수가 클러스터 수보다 많은지 확인
  num_data_points <- nrow(tdm_matrix)
  if (k_max > num_data_points) {
    stop("k_max is greater than the number of distinct data points.")
  }
  
  # 실루엣 점수를 계산하여 최적의 클러스터 수 선택
  silhouette_score <- function(k) {
    km <- kmeans(tdm_matrix, centers = k, nstart = 25)
    ss <- silhouette(km$cluster, dist(tdm_matrix))
    mean(ss[, 3])
  }
  
  k_values <- k_min:k_max
  avg_sil <- sapply(k_values, silhouette_score)
  
  best_k <- k_values[which.max(avg_sil)]
  print(paste("Best number of clusters:", best_k))
  
  # K-means 클러스터링 수행
  km <- kmeans(tdm_matrix, centers = best_k, nstart = 25)
  
  # 원래의 text_data와 클러스터 할당을 데이터 프레임으로 저장
  data_clusters <- data.frame(text = text_data, cluster = km$cluster)
  data_clusters <- data_clusters[order(data_clusters$cluster), ]
  
  # 클러스터별 텍스트 목록 생성 (원래의 text_data 사용)
  clusters_list <- lapply(unique(data_clusters$cluster), function(cluster) {
    data_clusters$text[data_clusters$cluster == cluster]
  })
  
  # 클러스터링 결과 시각화
  plot_cluster <- fviz_cluster(km, data = tdm_matrix, geom = "point", labelsize = 5, ggtheme = theme_minimal())
  
  tdm_matrix_pca <- prcomp(tdm_matrix, scale. = TRUE)
  tdm_matrix_pca_data <- as.data.frame(tdm_matrix_pca$x)
  tdm_matrix_pca_data$cluster <- as.factor(km$cluster)
  
  plot_pca <- fviz_pca_ind(tdm_matrix_pca, geom = "point", habillage = tdm_matrix_pca_data$cluster, 
                           addEllipses = TRUE, ellipse.level = 0.95, ggtheme = theme_minimal())
  
  # 결과 반환
  return(list(clusters = clusters_list, plot_cluster = plot_cluster, plot_pca = plot_pca, tdm_matrix = tdm_matrix, data_clusters = data_clusters))
}






## 🟧 클러스터링 결과 합치는 함수 정의 ##############################################################################################################
merge_clusters <- function(clustering_result, combined.list) {
  # 기존 클러스터링 결과 가져오기
  data_clusters <- clustering_result$data_clusters
  tdm_matrix <- clustering_result$tdm_matrix
  
  # 새로운 클러스터링 그룹 할당
  data_clusters$new_cluster <- NA
  for (i in 1:length(combined.list)) {
    data_clusters$new_cluster[data_clusters$cluster %in% combined.list[[i]]] <- i
  }
  
  # 새로운 클러스터 리스트 생성
  new_clusters_list <- lapply(seq_along(combined.list), function(kth_cluster) {
    data_clusters %>% filter(new_cluster == kth_cluster) %>% pull(text)
  }) %>% setNames(names(combined.list))
  
  
  # PCA 시각화 준비
  tdm_matrix_pca <- prcomp(tdm_matrix, scale. = TRUE)
  tdm_matrix_pca_data <- as.data.frame(tdm_matrix_pca$x)
  tdm_matrix_pca_data$cluster <- as.factor(data_clusters$new_cluster)
  
  plot_pca <- ggplot(tdm_matrix_pca_data, aes(x = PC1, y = PC2, color = cluster)) +
    geom_point(size = 2) +
    stat_ellipse(level = 0.95) +
    theme_minimal() +
    labs(title = "PCA of Merged Clusters")
  
  # 클러스터 시각화 준비
  plot_cluster <- ggplot(tdm_matrix_pca_data, aes(x = PC1, y = PC2, color = cluster)) +
    geom_point(size = 2) +
    theme_minimal() +
    labs(title = "Merged Clusters Visualization")
  
  return(list(clusters = new_clusters_list, plot_cluster = plot_cluster, plot_pca = plot_pca, data_clusters = data_clusters))
}








## 🟧 사용 예시 ##############################################################################################################
# 예시 데이터
# text_data <- c("예시 문장1", "예시 문장2", "예시 문장3", "예시 문장4", "예시 문장5", 
#                "예시 문장6", "예시 문장7", "예시 문장8", "예시 문장9", "예시 문장10",
#                "예시 문장11", "예시 문장12", "예시 문장13", "예시 문장14", "예시 문장15",
#                "예시 문장16", "예시 문장17", "예시 문장18", "예시 문장19", "예시 문장20")
# 
# result_1 <- text_clustering(text_data, k_min = 8, k_max = 8)
# result_1$clusters
# result_1$data_clusters
# # 결과 출력 및 사용자 정의 클러스터 합치기
# combined.list <- list(c(1, 2, 3, 4, 7), c(5, 6), 8)
# merged_result <- merge_clusters(result_1, combined.list)
# 
# # 병합된 클러스터 결과 출력
# print(merged_result$clusters)

# 시각화 출력
# print(merged_result$plot_cluster)
# print(merged_result$plot_pca)





## 🟧 여러 개의 원소에 union을 적용하는 함수 정의 ##############################################################################################################
# union_multiple 함수 정의
union_multiple <- function(...) {
  lists <- list(...)
  result <- Reduce(union, lists)
  return(result)
}



## 🟧 텍스트 필터링 함수 ##############################################################################################################
# 대소문자 구별 없이 필터링하는 함수 정의
# 대소문자 구별 없이 필터링하는 함수 정의
filter_text_data <- function(text_data, include = NULL, exclude = NULL) {
  if (!is.null(include)) {
    for (inc in include) {
      text_data <- text_data[grep(inc, text_data, ignore.case = TRUE)]
    }
  }
  
  if (!is.null(exclude)) {
    for (exc in exclude) {
      text_data <- text_data[!grepl(exc, text_data, ignore.case = TRUE)]
    }
  }
  
  return(text_data)
}


## 🟧 중복항목확인 함수 ##############################################################################################################
find_duplicates <- function(list) {
  any_duplicates_found <- FALSE
  for (category in names(list)) {
    duplicated_items <- duplicated(list[[category]]) | duplicated(list[[category]], fromLast = TRUE)
    if (any(duplicated_items)) {
      any_duplicates_found <- TRUE
      cat("Category:", category, "\n")
      cat("Duplicates:", list[[category]][duplicated_items], "\n\n")
    }
  }
  
  if (!any_duplicates_found) {
    cat("전체 리스트에서 중복 항목 없음\n")
  }
}




## 🟧 함수 정의 ===========================================================================================
# 모든 행의 원소가 NA인 열을 제거하는 함수 정의
remove_na_columns <- function(df) {
  # 마지막 열부터 첫 번째 열까지 역순으로 열 검사
  for (i in seq(ncol(df), 1)) {
    if (all(is.na(df[[i]]))) {
      df <- df[, -i, drop = FALSE]  # 모든 원소가 NA인 열 제거
    } else {
      break  # NA가 아닌 원소를 포함한 열을 만나면 루프 종료
    }
  }
  return(df)
}
# 헤더 행 합치기 함수
combine_columns <- function(df) {
  # 필요한 패키지 로드
  if(!require(dplyr)) install.packages("dplyr", dependencies=TRUE)
  library(dplyr)
  
  # 첫 번째, 두 번째 열 제외한 나머지 열을 결합
  df %>%
    group_by(ID, 행) %>%
    summarise(across(starts_with("열"), ~ paste(.[1], .[2], sep = "_")), .groups = 'drop')
}
# 열이 존재하는지 확인하는 함수
remove_column_if_exists <- function(data, column_name) {
  if (column_name %in% colnames(data)) {
    data <- data %>% dplyr::select(-all_of(column_name))
  }
  return(data)
}



## 🟧 ID 추출 ===========================================================================================
# NA가 나오기 직전의 값을 추출하는 함수
get_last_non_na <- function(row) {
  last_value <- NA
  for (value in row) {
    if (is.na(value)) {
      break
    }
    last_value <- value
  }
  return(last_value)
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


## 🟧 문자형이지만 숫자로만 이루어진 열을 숫자형으로 변환하는 함수 ##############################################################################################################
convert_to_numeric_if_possible <- function(df) {
  for (col in names(df)) {
    if (is.character(df[[col]])) {
      # 열의 모든 값이 숫자로 변환될 수 있는지 확인
      if (all(!is.na(as.numeric(df[[col]])))) {
        df[[col]] <- as.numeric(df[[col]])
      }
    }
  }
  return(df)
}


## 🟧 특정 열 숫자 이상의 열들에 대해 "-"와 ""을 NA로 바꾸고 ","를 지우는 함수 ##############################################################################################################
# 숫자로 변환될 수 있는 열은 numeric으로 변환하는 함수

clean_and_convert_columns <- function(df, start_col) {
  for (col in start_col:ncol(df)) {
    # "-"와 ""(공백)을 NA로 변경
    df[[col]] <- gsub("-", NA, df[[col]])
    df[[col]] <- gsub("^$", NA, df[[col]])
    # "," 제거
    df[[col]] <- gsub(",", "", df[[col]])
    # 숫자로 변환될 수 있는지 확인
    if (all(!is.na(suppressWarnings(as.numeric(df[[col]]))))) {
      df[[col]] <- as.numeric(df[[col]])
    }
  }
  return(df)
}


## 🟧 # 특정 열 이후의 열들에 대해 모든 원소가 숫자로 변환될 수 있으면 numeric으로 변환하는 함수 ##############################################################################################################
# 데이터프레임의 특정 열 번호 이후의 열들을 numeric으로 변환하는 함수
convert_to_numeric_after_index <- function(df, column_index) {
  # 특정 열 이후의 열들을 numeric으로 변환합니다
  df[ , (column_index+1):ncol(df)] <- lapply(df[ , (column_index+1):ncol(df)], as.numeric)
  
  return(df)
}


## 🟧 데이터프레임의 특정 열 번호 이후의 열들에서 공백을 제거하는 함수 ##############################################################################################################
remove_spaces_after_index <- function(df, column_index) {
  # 특정 열 이후의 열들에 대해 공백을 제거합니다
  df[ , (column_index+1):ncol(df)] <- lapply(df[ , (column_index+1):ncol(df)], function(x) gsub(" ", "", x))
  
  return(df)
}


## 🟧데이터프레임의 여러 열 번호에 대해 열 이름에서 공백을 제거하는 함수 ##############################################################################################################
remove_spaces_from_column_names <- function(df, column_indices) {
  # 주어진 열 번호들의 열 이름에서 공백을 제거합니다
  colnames(df)[column_indices] <- sapply(colnames(df)[column_indices], function(name) gsub(" ", "", name))
  
  return(df)
}


## 🟧열 이름이 없고, 해당 열의 모든 원소가 NA인 열을 제거하는 함수 ##############################################################################################################
# 열 이름이 없거나 NA이고, 해당 열의 모든 원소가 NA인 열을 제거하는 함수
remove_na_columns_no_names <- function(df) {
  # 열 이름이 없거나 NA인 열의 인덱스를 찾습니다
  no_name_cols <- which(is.na(names(df)) | names(df) == "")
  
  # 열 이름이 없거나 NA이고, 해당 열의 모든 원소가 NA인 열을 제거합니다
  to_remove <- vapply(no_name_cols, function(col) all(is.na(df[[col]])), logical(1))
  df <- df[, -no_name_cols[to_remove]]
  
  return(df)
}


## 🟧"CLMN"이라는 문자열을 포함하면 그 열을 지우는 작업##############################################################################################################
# ith_data 데이터프레임의 마지막 열을 확인하고,
# 그 열의 원소들에 unique를 적용하고, 그 길이가 1이고,
# "CLMN"이라는 문자열을 포함하면 그 열을 지우는 작업을
# "CLMN"이 나오지 않을 때까지 반복하는 함수
remove_columns_until_clmn_not_found <- function(df) {
  while(TRUE) {
    # 데이터프레임이 비어있지 않은지 확인
    if (ncol(df) == 0) {
      break
    }
    
    # 마지막 열의 이름과 원소를 가져옴
    last_col_name <- names(df)[ncol(df)]
    last_col_values <- df[[last_col_name]]
    
    # 마지막 열의 원소들에 unique를 적용하고, 길이가 1이고, "CLMN"을 포함하는지 확인
    if (length(unique(last_col_values)) == 1 && grepl("CLMN", unique(last_col_values))) {
      # 조건을 만족하면 마지막 열을 제거
      df <- df[ , -ncol(df)]
    } else {
      # 조건을 만족하지 않으면 반복 종료
      break
    }
  }
  
  return(df)
}


## 🟧모든 원소가 NA이면 그 열을 지우는 작업##############################################################################################################
# ith_data 데이터프레임의 마지막 열을 확인하고,
# 모든 원소가 NA이면 그 열을 지우는 작업을 마지막 열이 모두 NA가 아닐 때까지 반복하는 함수
remove_na_columns_until_last_not_na <- function(df) {
  while(ncol(df) > 0 && all(is.na(df[[ncol(df)]]))) {
    df <- df[ , -ncol(df)]
  }
  return(df)
}



## 🟧NA가 아닌 부분이 모두 연도로 되어 있고 연속적인지 확인하는 함수##############################################################################################################
# ith_last_col 벡터가 주어졌을 때, NA가 아닌 부분이 모두 연도로 되어 있고 연속적인지 확인하는 함수
is_continuous_year_vector <- function(vec) {
  # NA가 아닌 요소들만 추출
  non_na_elements <- vec[!is.na(vec)]
  
  # 모든 요소가 numeric이고 4자리 숫자인지 확인
  are_all_numeric_years <- all(sapply(non_na_elements, function(x) is.numeric(x) && nchar(as.character(x)) == 4))
  
  if (!are_all_numeric_years) {
    return(FALSE)
  }
  
  # 연도가 연속적인지 확인
  are_years_continuous <- all(diff(non_na_elements) == 1)
  
  return(are_years_continuous)
}






## 🟧 패턴 찾는 함수##############################################################################################################
# 그룹을 추출하여 리스트로 저장
# extract_groups <- function(classification) {
#   result <- list()
#   current_group <- NULL
#   
#   sequence()
#   sequence = seq(1, length(classification), 3)
#   classification[-]
#   
#   
#   table(classification) %>% sort(decreasing=T)
#   for (item in classification) {
#     if (!is.null(current_group) && !(item %in% result[[current_group]])) {
#       if (any(item == unlist(result))) {
#         current_group <- item
#       }
#     }
#     
#     if (is.null(current_group) || item == current_group || item %in% result[[current_group]]) {
#       if (!item %in% names(result)) {
#         current_group <- item
#         result[[current_group]] <- list()
#       }
#     } else {
#       result[[current_group]] <- append(result[[current_group]], item)
#     }
#   }
#   
#   return(result)
# }









## 🟧 마지막 열 연도?##############################################################################################################
last_col_elements_year <- function(vec) {
  # NA 제외
  non_na_vec <- na.omit(vec)
  
  # 벡터의 길이가 1 이하인 경우 연속성 여부를 확인할 수 없음
  if (length(non_na_vec) <= 1) {
    return(FALSE)
  }
  
  # 벡터가 숫자인지 확인
  if (!all(is.numeric(non_na_vec))) {
    return(FALSE)
  }
  
  # 벡터가 연속적인지 확인
  return(all(diff(non_na_vec) == 1))
}

## 🟧 데이터 프레임에서 ##############################################################################################################
remove_spaces <- function(df) {
  # 모든 열에 대해 공백을 제거합니다.
  df[] <- lapply(df, function(col) {
    if (is.character(col)) {
      return(gsub(" ", "", col))
    } else {
      return(col)
    }
  })
  return(df)
}

## 🟧 빈 열, 이름 없는 열 삭제 ##############################################################################################################
remove_empty_na_columns <- function(df) {
  # 열 이름이 없는 열 인덱스 찾기
  unnamed_cols <- which(names(df) == "")
  
  # 열 이름이 없는 열 중 모든 값이 NA인 열 찾기
  if(length(unnamed_cols)>0){
    cols_to_remove <- unnamed_cols[sapply(unnamed_cols, function(i) all(is.na(df[[i]])))]
    
    # 해당 열들 제거
    if (length(cols_to_remove) > 0) {
      df <- df[, -cols_to_remove, drop = FALSE]
    }
    
  }
    
  return(df)
}





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


## 🟧 NULL원소 제거 ##############################################################################################################
remove_null_elements <- function(lst) {
  # NULL이 아닌 원소들만 남기기
  return(Filter(Negate(is.null), lst))
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
## 🟧 슬래시 없 애 기   ##############################################################################################################
replace_slash <- function(vec) {
  # vec의 각 요소에 대해 gsub 함수를 적용하여 "/"를 "-"로 대체
  sapply(vec, function(x) gsub("/", "-", x))
}

## 🟧 영어 없애기   ##############################################################################################################
remove_english <- function(vec) {
  # vec의 각 요소에 대해 gsub 함수를 적용하여 영어 단어를 제거
  sapply(vec, function(x) gsub("\\b[A-Za-z]+\\b", "", x))
}


replace_hyphen <- function(vec) {
  # vec의 각 요소에 대해 gsub 함수를 적용하여 "-"를 ""로 대체
  sapply(vec, function(x) gsub("-", "", x))
}


remove_multiple_spaces <- function(vec) {
  # vec의 각 요소에 대해 gsub 함수를 적용하여 한 칸 이상의 공백을 빈 문자열로 대체
  sapply(vec, function(x) gsub("\\s+", "", x))
}
## 🟧 경로생성함수   ##############################################################################################################
create_path <- function(path_save, ith_L2, ith_L3 = NA, ith_L4 = NA, ith_L5 = NA, ith_year = NA, extension = "csv") {
  # 초기 경로는 path_save와 ith_L2로 설정
  path <- file.path(path_save, ith_L2)
  
  # 각 레벨을 확인하고, NA가 아니면 경로에 추가
  if (!is.na(ith_L3)) {
    path <- file.path(path, ith_L3)
    if (!is.na(ith_L4)) {
      path <- file.path(path, ith_L4)
      if (is.na(ith_L5)) {
        # ith_L5가 NA인 경우, ith_L4가 파일명
        file_name <- ith_L4
      } else {
        # ith_L5가 파일명
        file_name <- ith_L5
      }
    } else {
      # ith_L4가 NA인 경우, ith_L3가 파일명
      file_name <- ith_L3
    }
  } else {
    # ith_L3가 NA인 경우, ith_L2가 파일명
    file_name <- ith_L2
  }
  
  # 최종 파일명에 ith_year 추가
  if (!is.na(ith_year)) {
    file_name <- paste0(ith_year, "_", file_name)
  }
  
  # 최종 경로와 파일명 반환
  if (exists("file_name")) {
    dir.create(path, showWarnings = FALSE, recursive = TRUE)
    return(file.path(path, paste0(file_name, ".", extension))) # 파일명에 확장자 추가 (예: .csv)
  } else {
    dir.create(path, showWarnings = FALSE, recursive = TRUE)
    return(path)
  }
}


## 🟧 열들 numeric으로 바꾸기 ##############################################################################################################
# 변환 함수 정의
convert_to_numeric <- function(df, start_col) {
  for (col in start_col:ncol(df)) {
    # 숫자로 변환 시도
    numeric_col <- suppressWarnings(as.numeric(df[[col]]))
    
    # NA가 아닌 값의 수를 확인
    non_na_count <- sum(!is.na(numeric_col))
    original_non_na_count <- sum(!is.na(df[[col]]))
    
    # 숫자로 변환 가능한 경우에만 변환
    if (non_na_count == original_non_na_count) {
      df[[col]] <- numeric_col
    }
  }
  return(df)
  
}


## 🟧 열 삭제 함수 정의 ##############################################################################################################
remove_character_columns <- function(df, start_col) {
  
  cols_to_remove <- c() # 삭제할 열 목록 초기화
  
  # 시작 열 이후부터 데이터 프레임의 열들을 순회
  for (col in (start_col + 1):(ncol(df))) {
    # 현재 열이 character 형식인지 확인
    if (is.character(df[[col]])) {
      # 현재 열이 (숫자) 형식의 값 또는 NA만 포함하는지 확인
      if (all(is.na(df[[col]]) | grepl("^\\(\\d+\\)$", df[[col]]))) {
        cols_to_remove <- c(cols_to_remove, col)
      }
    }
  }
  
  # 삭제할 열을 제거한 새로운 데이터프레임 반환
  if (length(cols_to_remove) == 0) {
    return(df)
  } else {
    df <- df[ , -cols_to_remove, drop = FALSE]
    return(df)
  }
}




## 🟧 "행"열  다음 열들 합치기##############################################################################################################
combine_character_columns <- function(df, key_col_name) {
  # df = ith_data_2
  # 키 열의 인덱스 찾기
  key_index <- which(names(df) == key_col_name)
  
  # A 열의 이름과 원소 설정
  a_col_name <- names(df)[key_index + 1]
  a_col <- df[[key_index + 1]]
  
  # A 열의 다음 열부터 검사 시작
  i <- key_index + 2
  while (i <= ncol(df)) {
    current_col <- df[[i]]
    
    # 열이 numeric으로 변환 가능한지 검사
    is_numeric_col <- all(sapply(current_col, function(x) {
      x <- gsub("^\\(|\\)$", "", x)  # 괄호 제거
      is.na(x) || !is.na(suppressWarnings(as.numeric(x)))  # 숫자로 변환 가능한지 확인
    }))
    
    if (is_numeric_col) {
      # 숫자로 변환 가능한 경우, 중단
      break
    } else {
      # NA 값을 "NA" 문자열로 변경
      current_col[is.na(current_col)] <- "NA"
      
      # A 열과 현재 열을 합치기
      a_col <- paste(a_col, current_col, sep="_")
      
      # 데이터프레임에서 합쳐진 열 제거
      df <- df[ , -i]
    }
  }
  
  # 합쳐진 A 열을 데이터프레임에 다시 할당
  df[[a_col_name]] <- a_col
  
  # 결과 데이터 프레임 반환
  return(df)
}





## 🟧 콤마 지우는 함수##############################################################################################################
remove_commas_from_columns <- function(df, start_col) {
  # 숫자 이후의 모든 열들에 대해 순회
  for (col in start_col:ncol(df)) {
    # 해당 열이 NA 또는 숫자, 특수문자로만 구성된 원소들만 갖는지 확인
    if (all(is.na(df[[col]]) | grepl("^[0-9[:punct:]]*$", df[[col]]))) {
      # ","를 제거
      df[[col]] <- gsub(",", "", df[[col]])
    }
  }
  return(df)
}

## 🟧 -를 NA로 바꾸는 함수 ##############################################################################################################
replace_dash_with_na <- function(df, start_col) {
  # 특정 숫자 이후의 모든 열들에 대해 순회
  for (col in start_col:ncol(df)) {
    # 해당 열의 원소 중 "-"가 있는 경우 NA로 변경
    df[[col]] <- sapply(df[[col]], function(x) ifelse(grepl("-", x), NA, x))
  }
  return(df)
}


## 🟧 벡터의 모든 원소가 character인지 확인 ##############################################################################################################
is_strictly_character <- function(vec) {
  # 벡터의 각 원소에서 "(숫자)" 형태를 숫자로 변환
  vec <- gsub("^\\((\\d+)\\)$", "\\1", vec)
  
  # 벡터의 각 원소를 numeric으로 변환
  numeric_conversion <- suppressWarnings(as.numeric(vec))
  
  # 원래 값이 NA이거나, 숫자로 변환할 수 없는 원소 확인
  is_strictly_char <- is.na(numeric_conversion) & !is.na(vec)
  
  # NA이거나 숫자로 변환할 수 없는 모든 원소가 TRUE인지 확인
  all_strictly_char_or_na <- all(is_strictly_char | is.na(vec))
  
  return(all_strictly_char_or_na)
}


## 🟧 열이름 합치는 함수 ##############################################################################################################
combine_columns_with_na <- function(colnames, row_values) {
  # 열 이름과 행 값을 결합
  combined <- mapply(function(col, val) {
    if (is.na(val)) {
      return(col)
    } else {
      return(paste0(col, "_", val))
    }
  }, colnames, row_values)
  
  return(combined)
}


## 🟧 마지막열 합치기 ##############################################################################################################
combine_last_column <- function(df) {
  # 마지막 열이 모두 NA인지 확인
  if (all(is.na(df[[ncol(df)]]))) {
    return(df)
  }
  
  # 마지막 열이 character인지 확인
  if (is.character(df[[ncol(df)]])) {
    
    vec_1 = df[,ncol(df)-1] %>% unlist %>% unname
    vec_2 = df[,ncol(df)-2] %>% unlist %>% unname
    
    last_colname = names(df)[ncol(df)]
    
    if(sum(vec_1 == vec_2) != length(vec_1) && colnames(df)[ncol(df)-1] == colnames(df)[ncol(df)-2] && !grepl("classification", last_colname, ignore.case = TRUE)){
      names(df)[ncol(df)-1] = last_colname
      names(df)[ncol(df)] = "classification"
    }
    
    # "행" 열의 인덱스 찾기
    row_index <- which(names(df) == "행")
    
    # "행" 열의 바로 다음 열과 마지막 열 이름과 값 합침
    combined_col_name <- paste0(names(df)[row_index + 1], "_", names(df)[ncol(df)])
    combined_col <- paste0(df[[row_index + 1]], "_", df[[ncol(df)]])
    
    # 새로운 데이터프레임 생성
    new_df <- df[, -c(row_index + 1, ncol(df))] # "행" 다음 열과 마지막 열 제거
    
    # 새로운 열 추가
    new_df <- new_df %>%
      add_column(!!combined_col_name := combined_col, .after = row_index, .name_repair = "minimal")
    
    return(new_df)
  } else {
    return(df)
  }
}




## 🟧 header 이름 합치기 ##############################################################################################################
combine_header_colnames = function(ith_hdr){
  
  ind = which(colnames(ith_hdr) == "행")
  
  
  rows.list = lapply(1:nrow(ith_hdr), function(k){
    kth_row = ith_hdr[k, -c(1:ind)] %>% unlist %>% unname
    return(kth_row)
  })
  
  
  for(n in seq_along(rows.list)){
    
    if(n==1){
      
      combined_colname = rows.list[[n]]
      
    }else{
      
      combined_colname = paste0(combined_colname, "_", rows.list[[n]])
      
    }
    
  }
  
  c(names(ith_hdr)[c(1:ind)] %>% unlist, combined_colname) %>% unname %>% return
  
}



## 🟧 첫 행에서 NA인 열 제외 ##############################################################################################################
# 함수 정의
remove_na_rows_columns <- function(df) {
  # 첫번째 행에서 NA가 아닌 열의 인덱스를 찾음
  non_na_cols <- which(!is.na(df[1, ]))
  # 세번째 열까지는 항상 포함하도록 설정
  cols_to_keep <- c(1, 2, 3, non_na_cols[non_na_cols > 3])
  # 해당 열만 선택하여 새로운 데이터프레임 생성
  df_clean <- df[, cols_to_keep]
  return(df_clean)
}


## 🟧 마지막 열이 NA 또는 0만 있으면 제외 ##############################################################################################################
# 함수 정의
remove_last_na_or_zero <- function(df) {
  # 마지막 열의 이름을 가져옴
  last_col <- ncol(df)
  # 마지막 열의 모든 값이 NA 또는 0인지 확인
  if (all(is.na(df[[last_col]]) | df[[last_col]] == 0)) {
    df <- df[, -last_col]
  }
  return(df)
}

## 🟧 중복원소에 넘버링 추가 ##############################################################################################################
# 중복 원소에 넘버링을 추가하는 함수
add_numbering <- function(vec) {
  counts <- table(vec) # 각 원소의 빈도수를 셈
  counts <- counts[counts > 1] # 중복된 원소만 선택
  
  new_vec <- vec
  index_list <- lapply(names(counts), function(x) which(vec == x)) # 중복된 원소의 인덱스 리스트
  
  for (indices in index_list) {
    for (i in seq_along(indices)) {
      new_vec[indices[i]] <- paste0(vec[indices[i]], "_", i)
    }
  }
  
  return(new_vec)
}


## 🟧 마지막 원소가 %이면 열삭제 ##############################################################################################################
remove_na_columns_with_percentage <- function(df) {
  cols_to_keep <- sapply(df[-(1:2)], function(col) {
    all(is.na(col[-nrow(df)])) && col[nrow(df)] == "%"
  })
  
  df <- df[, c(TRUE, TRUE, !cols_to_keep)]
  return(df)
}



## 🟧 열 원소들 합치기 ##############################################################################################################

combine_columns_after_row <- function(df) {
  # "행" 열의 인덱스 찾기
  row_index <- which(names(df) == "행")
  
  # "행" 열 다음의 첫 번째 numeric 열의 인덱스 찾기
  next_numeric_index <- which(sapply(df[(row_index + 1):ncol(df)], is.numeric))[1] + row_index
  
  # 합칠 열들의 인덱스 초기화
  cols_to_combine <- c()
  
  # 합칠 열들을 찾아봄
  for (i in (row_index + 1):(next_numeric_index - 1)) {
    # 해당 열의 모든 원소가 숫자로 변환될 수 있는지 확인
    all_non_numeric <- all(sapply(df[[i]], function(x) {
      suppressWarnings({
        !grepl("[^0-9.]", x) && is.na(as.numeric(as.character(x))) || is.na(x)
      })
    }))
    if (all_non_numeric) {
      cols_to_combine <- c(cols_to_combine, i)
    } else {
      break
    }
  }
  
  # 만약 합칠 열이 없다면 원래 데이터프레임 반환
  if (length(cols_to_combine) == 0) {
    return(df)
  }
  
  # 새 열 이름 생성
  new_col_name <- paste(names(df)[cols_to_combine], collapse = "_")
  
  # 새 열 생성: 각 행에 대해 NA가 아닌 값을 "_"로 합침
  new_col <- apply(df[, cols_to_combine], 1, function(row) {
    paste(na.omit(row), collapse = "_")
  })
  
  # 새로운 데이터프레임 생성
  new_df <- df %>% 
    select(-all_of(names(df)[cols_to_combine])) %>% 
    add_column(!!new_col_name := new_col, .after = row_index)
  
  return(new_df)
}

## 🟧 마지막 열부터 전부 NA이면  지움 ##############################################################################################################
remove_na_columns_from_last <- function(df) {
  df = df %>% as.data.frame
  repeat {
    # Check if the last column is all NA
    if (all(is.na(df[, ncol(df)]))) {
      # Drop the last column
      df <- df[, -ncol(df)]
    } else {
      # If the last column is not all NA, break the loop
      break
    }
  }
  return(df)
}

## 🟧  대부분의 원소가 numeric으로 절대 변경될 수 없는 character인지 여부를 판별하는 함수 ##############################################################################################################
is_mostly_non_numeric <- function(vec, threshold = 0.5) {
  # vec가 character 벡터인지 확인
  if (!is.character(vec)) {
    stop("Input vector must be of type character")
  }
  
  # NA를 제외한 원소들의 수
  non_na_elements <- vec[!is.na(vec)]
  
  # NA를 제외한 원소들 중 numeric으로 변환될 수 없는 원소의 수
  non_numeric_count <- sum(sapply(non_na_elements, function(x) {
    is.na(suppressWarnings(as.numeric(x)))
  }))
  
  # 전체 NA가 아닌 원소의 수 대비 numeric으로 변환될 수 없는 원소의 비율 계산
  non_numeric_ratio <- non_numeric_count / length(non_na_elements)
  
  # 비율이 threshold 이상인 경우 TRUE 반환
  return(non_numeric_ratio > threshold)
}
## 🟧 연속된 연도 벡터 ##############################################################################################################
is_consecutive_years <- function(years) {
  # 벡터가 숫자인지 확인
  if (!all(sapply(years, is.numeric))) {
    stop("Input vector must contain only numeric values")
  }
  
  # 정렬된 상태에서 차이가 모두 1인지 확인
  sorted_years <- sort(years, na.last = TRUE)
  consecutive_check <- all(diff(sorted_years) == 1)
  
  return(consecutive_check)
}


## 🟧 연도열의 NA 채우기 ##############################################################################################################
fill_na_with_previous_year <- function(df) {
  # "행" 열의 인덱스 찾기
  row_index <- which(names(df) == "행")
  
  # 연도 열과 문자 데이터를 포함하는 열 찾기
  year_col_index <- row_index + 1
  char_col_index <- row_index + 2
  
  # NA를 포함한 연도 열을 채우기
  for (i in 2:nrow(df)) {
    if (is.na(df[i, year_col_index]) && !is.na(df[i, char_col_index])) {
      df[i, year_col_index] <- df[i - 1, year_col_index]
    }
  }
  
  return(df)
}

## 🟧엑셀 에러 원소 바꾸기 ##############################################################################################################
convert_error_dataframe <- function(df) {
  # 4번째 열에 "#REF!"가 있는 경우에만 수행
  if (any(grepl("#REF!", df[[4]], fixed = TRUE))) {
    # 4번째 열에서 "#REF!"를 NA로 변경
    df[[4]] <- gsub("#REF!", NA, df[[4]], fixed = TRUE)
    
    # 4번째 열이 numeric으로 변환 가능한지 확인 후 변환
    if (all(is.na(as.numeric(df[[4]])) | !is.na(df[[4]]))) {
      df[[4]] <- as.numeric(df[[4]])
    }
  }
  
  return(df)
}

## 🟧괄호 문자열 제거 ##############################################################################################################
remove_parentheses <- function(df) {
  # 4번째 열부터 마지막 열까지 괄호 () 문자열 제거
  df[, 4:ncol(df)] <- lapply(df[, 4:ncol(df)], function(x) gsub("\\(.*?\\)", "", x))
  return(df)
}


## 🟧 4번째 열부터 엑셀 에러 존재 확인 ##############################################################################################################
check_for_ref <- function(df) {
  if (any(df[, 4:ncol(df)] == "#REF!", na.rm = TRUE)) {
    stop("Data frame contains #REF! in columns from the fourth onward.")
  }else{
    return(df)
  }
}

## 🟧 (숫자)와 NA 데이터 행 지우기 ##############################################################################################################
update_and_remove_rows <- function(df) {
  pattern <- "^\\(\\d+\\)$"
  rows_to_remove <- c()
  
  for (i in seq_len(nrow(df))) {
    
    row_values <- df[i, 4:ncol(df)]
    
    if (all(sapply(row_values, function(x) (!is.na(x) && grepl(pattern, x)) || is.na(x)))) {
      
      ind <- which(grepl(pattern, row_values))
      
      if(length(ind) > 0){
        
        previous_row <- df[i - 1, 4:ncol(df)]
        ind_2 <- which(is.na(previous_row))
        
        ind_2_intersect <- intersect(ind_2, ind)
        
        if(length(ind_2_intersect) > 0){
          
          df[i - 1, 4:ncol(df)][ind_2_intersect] <- row_values[ind_2_intersect]
          rows_to_remove <- c(rows_to_remove, i)
        }
      }
    }
  }
  
  if (length(rows_to_remove) > 0) {
    df <- df[-rows_to_remove, ]
  }
  
  return(df)
}

## 🟧 맨 마지막 열이 "한글" "영어"가 번갈아 나오는 경우 ##############################################################################################################
update_and_remove_rows_v3 <- function(df) {
  rows_to_remove <- c()
  
  for (i in seq_len(nrow(df) - 1)) {
    current_row <- df[i, ]
    next_row <- df[i + 1, ]
    
    # Check if next row has NA in all columns from 4th to last-1 column
    if (all(is.na(next_row[4:(ncol(df) - 1)]))) {
      # Find indices of non-NA values in the last column of the current and next rows
      non_na_current <- !is.na(current_row[ncol(df)])
      non_na_next <- !is.na(next_row[ncol(df)])
      
      if (non_na_current && non_na_next) {
        # Update the current row's last column with "한글_영어"
        df[i, ncol(df)] <- paste(current_row[ncol(df)], next_row[ncol(df)], sep = "_")
        rows_to_remove <- c(rows_to_remove, i + 1)
      }
    }
  }
  
  # Remove the rows identified
  if (length(rows_to_remove) > 0) {
    df <- df[-rows_to_remove, ]
  }
  # View(df)
  return(df)
}


## 🟧 연속 연도 행에서 (숫자),NA 형태면 행 삭제 ##############################################################################################################
remove_consecutive_rows <- function(df) {
  # df = ith_data_2
  # View(df)
  rows_to_remove <- c()
  
  for (i in seq_len(nrow(df) - 1)) {
    if (!is.na(df[i, 3]) && !is.na(df[i + 1, 3]) && df[i, 3] == df[i + 1, 3]) {
      next_row <- df[i + 1, 4:(ncol(df) - 1)]
      pattern <- "^\\(\\d+\\.?\\d*\\)$"
      
      if (all(sapply(next_row, function(x) grepl(pattern, as.character(x)) || is.na(x)))) {
        rows_to_remove <- c(rows_to_remove, i + 1)
      }
    }
  }
  
  if (length(rows_to_remove) > 0) {
    df <- df[-rows_to_remove, ]
  }
  
  return(df)
}



## 🟧 마지막 열이 연도면 character로 바꿈 ##############################################################################################################
check_and_convert_last_column <- function(df) {
  last_col <- df[[ncol(df)]]
  
  suppressWarnings({
    numeric_conversion <- as.numeric(last_col)
    if (all(diff(numeric_conversion) >= 0, na.rm = TRUE)) {
      df[[ncol(df)]] <- as.character(last_col)
    }
  })
  
  return(df)
}


## 🟧 4번째 열이 수치형이 아닌 경우##############################################################################################################
check_numeric_column <- function(df) {
  # df = ith_data_2
  suppressWarnings({
    if (any(is.na(as.numeric(as.character(df[[4]]))))) {
      stop("4번째 열에 numeric 형태로 변환할 수 없는 문자열 데이터가 있습니다.")
    }
  })
}

## 🟧 열이름 문자열에서 슬래시 제거##############################################################################################################
replace_slash_in_colnames <- function(df) {
  colnames(df) <- gsub("/", "⋅", colnames(df))
  return(df)
}

## 🟧 마지막 행이 NA이면 제거 ##############################################################################################################
remove_last_row_if_na <- function(df) {
  if (all(is.na(df[nrow(df), 3:ncol(df)]))) {
    df <- df[-nrow(df), ]
  }
  return(df)
}

## 🟧 열이름에서 newline 문자열 제거 ##############################################################################################################
remove_newline_from_colnames <- function(df) {
  colnames(df) <- gsub("\r\n", "", colnames(df))
  return(df)
}


## 🟧 연도 원소 생성 ##############################################################################################################
insert_year_if_conditions_met <- function(vec) {
  if (length(vec) >= 3 &&
      tolower(vec[2]) == "행" &&
      (grepl("total", vec[3], ignore.case = TRUE) || 
       grepl("합계", vec[3]) || 
       grepl("총합", vec[3])) &&
      grepl("year", vec[length(vec)], ignore.case = TRUE)) {
    vec <- append(vec, "연도", after = 2)
  }
  return(vec)
}

## 🟧 newline 문자열 제거 ##############################################################################################################
remove_newline_from_vector <- function(vec) {
  vec <- gsub("\r\n", "", vec)
  return(vec)
}

## 🟧 3번째 열과 동일한 열 제거 ##############################################################################################################
remove_columns_with_same_values_as_third <- function(df) {
  # df = ith_data_2
  
  if(ncol(df) > 6){
    third_col <- as.character(df[[3]])
    
    cols_to_remove <- sapply(5:(ncol(df) - 1), function(i) {
      col <- as.character(df[[i]])
      all(col == third_col, na.rm = TRUE) && !all(is.na(col))
    })
    
    keep_columns <- c(TRUE, TRUE, TRUE, TRUE, !cols_to_remove, TRUE)
    
    df <- df[, keep_columns, drop = FALSE]  
  }
  
  # View( df[, keep_columns, drop = FALSE])
  return(df)
}


remove_spaces_from_colnames <- function(df) {
  colnames(df) <- gsub(" ", "", colnames(df))
  return(df)
}

## 🟧 거래형 비거래형 등과 같이 반복값이 3번쨰 열에 있는 경우 ##############################################################################################################
add_region_prefix_based_on_pattern <- function(df) {
  last_region <- NA
  repeating_pattern <- unique(df[[3]][duplicated(df[[3]])])
  
  for (i in 1:nrow(df)) {
    current_value <- df[i, 3]
    if (current_value %in% repeating_pattern) {
      df[i, 3] <- paste(last_region, current_value, sep = "_")
    } else {
      last_region <- current_value
    }
  }
  
  return(df)
}

## 🟧 열이름 ? cdot으로 바꾸기  ##############################################################################################################
replace_question_mark_in_colnames <- function(df) {
  colnames(df) <- gsub("\\?", "⋅", colnames(df))
  return(df)
}

## 🟧 3번째 열 원소에서 NA문자열 제거  ##############################################################################################################
remove_na_prefix_from_third_column <- function(df) {
  df[[3]] <- gsub("^NA_", "", df[[3]])
  return(df)
}
## 🟧 열이름에서 NA_NA_NA 문자열 제거  ##############################################################################################################
remove_na_prefix_from_colnames <- function(df) {
  colnames(df) <- gsub("NA_NA_NA_", "", colnames(df))
  return(df)
}

## 🟧 header에 classification 추가 ##############################################################################################################
adjust_ith_hdr <- function(df, start_col, ith_hdr) {
  # (1) 마지막 열부터 왼쪽으로 열을 검사하여 numeric인 열이 나오기 직전까지의 character 열들의 개수 확인
  num_cols <- sapply(df, is.numeric)
  n <- ncol(df)
  char_count <- 0
  
  for (i in seq(n, start_col, by = -1)) {
    if (num_cols[i]) {
      break
    } else {
      char_count <- char_count + 1
    }
  }
  
  # (2) ith_hdr의 마지막 부분에 있는 문자열 원소들 가운데 classification 포함 확인
  ith_hdr_last_part <- tail(ith_hdr, char_count)
  classification_count <- sum(grepl("classification", ith_hdr_last_part, ignore.case = TRUE))
  
  # (3) classfication 포함 문자가 부족하면 ith_hdr에 classification 추가
  if (classification_count < char_count) {
    to_add <- char_count - classification_count
    ith_hdr <- c(ith_hdr, rep("classification", to_add))
  }
  
  return(ith_hdr)
}

## 🟧  문자열 열을 제거하는 함수 정의 ##############################################################################################################
remove_character_columns_last <- function(data) {
  cols_to_check <- min(5, ncol(data))
  for (i in ncol(data):(ncol(data) - cols_to_check + 1)) {
    col <- unlist(data[, i]) %>% unname
    if (is.character(col) && is_mostly_non_numeric(col)) {
      data <- data[, -i]
    } else {
      break
    }
  }
  return(data)
}



## 🟧 벡터의 마지막 원소가 "classification"을 포함하는지 확인하고, 포함하고 있다면 이를 제거하는 작업을 반복하는 함수 ##############################################################################################################
remove_classification <- function(vec) {
  # 마지막 원소가 "classification"을 포함하는지 확인하는 반복문
  while (length(vec) > 0 && grepl("classification", vec[length(vec)], ignore.case = TRUE)) {
    # 마지막 원소 제거
    vec <- vec[-length(vec)]
  }
  return(vec)
}


## 🟧 상위행정구역이름 뒤에 chk 추가 ##############################################################################################################
add_chk_suffix <- function(df) {
  column <- df$열000
  # 상위 행정구역 이름을 정의합니다.
  regions <- c("서울특별시", "부산광역시", "대구광역시", "인천광역시", 
               "광주광역시", "대전광역시", "울산광역시", "경기도", 
               "강원도", "충청북도", "충청남도", "전라북도", 
               "전라남도", "경상북도", "경상남도", "제주특별자치도")
  
  # 열에 지역 이름이 포함되어 있는지 확인합니다.
  if (!any(column %in% regions)) {
    return(df)
  }
  
  # 각 행정구역의 빈도를 계산합니다.
  region_counts <- table(column)
  
  # 행정구역이 두 번 이상 나타나는 경우 "_chk"를 붙입니다.
  for (region in names(region_counts)) {
    if (region_counts[region] > 1 && region %in% regions) {
      column[column == region] <- paste0(region, "_chk")
    }
  }
  
  df$열000 <- column
  return(df)
}


## 🟧 행정구역 합계 ##############################################################################################################
# ith_data_2$열000
# update_administrative_districts <- function(df) {
#   # 연도와 상위 행정구역을 포함한 행을 식별하는 패턴
#   year_pattern <- "^[0-9]{4}$"
#   upper_pattern <- "시$|도$|특별자치도$"
#   
#   # 연도와 상위 행정구역이 연속적으로 있는 부분의 인덱스를 찾기
#   initial_part <- which(grepl(year_pattern, df$열000) | grepl(upper_pattern, df$열000))
#   first_non_upper <- which(!grepl(year_pattern, df$열000) & !grepl(upper_pattern, df$열000))[1]
#   
#   # 연속적으로 상위 행정구역들만 있는 행의 인덱스
#   upper_part <- initial_part[initial_part < first_non_upper]
#   
#   # 상위 행정구역이 중복된 부분의 인덱스를 저장할 벡터
#   upper_indices <- which(grepl(upper_pattern, df$열000) & !(1:nrow(df) %in% upper_part))
#   
#   for (i in seq_along(upper_indices)) {
#     # 현재 상위 행정구역의 인덱스
#     upper_index <- upper_indices[i]
#     
#     # 다음 상위 행정구역의 인덱스 (마지막 상위 행정구역의 경우 nrow + 1)
#     next_upper_index <- ifelse(i < length(upper_indices), upper_indices[i + 1], nrow(df) + 1)
#     
#     # 현재 상위 행정구역 이름 저장
#     upper_name <- df$열000[upper_index]
#     
#     # 하위 행정구역들의 값 합계 계산
#     sub_regions_sum <- sum(as.numeric(df$열001[(upper_index + 1):(next_upper_index - 1)]), na.rm = TRUE)
#     
#     # 합계가 일치하는지 확인
#     if (sub_regions_sum == as.numeric(df$열001[upper_index])) {
#       # 하위 행정구역 이름 앞에 상위 행정구역 이름을 붙임
#       df$열000[(upper_index + 1):(next_upper_index - 1)] <- paste(upper_name, df$열000[(upper_index + 1):(next_upper_index - 1)], sep = "_")
#     } else {
#       warning(paste("합계가 일치하지 않습니다:", upper_name))
#     }
#   }
#   
#   # 연속적으로 상위 행정구역들만 있는 행의 이름에 "_chk" 추가
#   df$열000[upper_part] <- paste(df$열000[upper_part], "chk", sep = "_")
#   
#   return(df)
# }


## 🟧 NA 마지막 열 제거##############################################################################################################
remove_na_last_columns <- function(df, vec) {
  # 벡터의 길이를 가져옵니다.
  vec_length <- length(vec)
  
  # 반복 작업을 수행합니다.
  repeat {
    # 데이터프레임의 열의 개수를 가져옵니다.
    df_cols <- ncol(df)
    
    # 열의 개수와 벡터의 길이가 같으면 반복을 종료합니다.
    if (df_cols == vec_length) {
      break
    }
    
    # 마지막 열이 전부 NA인지 검사합니다.
    if (all(is.na(df[[df_cols]]))) {
      # 마지막 열을 제거합니다.
      df <- df[, -df_cols]
    } else {
      # 마지막 열이 전부 NA가 아니면 반복을 종료합니다.
      break
    }
  }
  
  # 열의 개수가 벡터의 길이와 같으면 열 이름을 벡터의 원소로 설정합니다.
  if (ncol(df) == vec_length) {
    colnames(df) <- vec
  }
  
  return(df)
}

## 🟧 백틱 제거 ##############################################################################################################
replace_backtick_with_na <- function(df, specific_column) {
  # 특정 열의 인덱스를 숫자로 받습니다.
  col_index <- specific_column
  
  # 특정 열이 존재하는지 확인합니다.
  if (col_index > ncol(df) || col_index < 1) {
    stop("특정 열이 데이터 프레임에 존재하지 않습니다.")
  }
  
  # 특정 열 다음의 모든 열들에 대해 처리합니다.
  if (col_index < ncol(df)) {
    for (i in (col_index + 1):ncol(df)) {
      df[, i][df[, i] == "`"] <- NA
    }
  }
  
  return(df)
}

## 🟧 개소 문자열 제거##############################################################################################################
remove_and_convert_numeric_count_data <- function(df) {
  # 5번째 열부터 마지막 열까지 반복합니다.
  for (i in 5:ncol(df)) {
    # 열의 각 원소에서 "개소"를 제거합니다.
    df[, i] <- gsub("개소", "", df[, i])
    
    # 열을 숫자로 변환합니다.
    df[, i] <- suppressWarnings(as.numeric(df[, i]))
  }
  
  return(df)
}



## 🟧 열이름 NA 제거 ##############################################################################################################
replace_na_colnames <- function(df) {
  # 현재 열 이름을 가져옵니다.
  col_names <- colnames(df)
  
  # NA 열 이름의 인덱스를 찾습니다.
  na_indices <- which(is.na(col_names))
  
  # NA 열 이름을 "NA_1", "NA_2" 등으로 변경합니다.
  if (length(na_indices) > 0) {
    for (i in seq_along(na_indices)) {
      col_names[na_indices[i]] <- paste0("NA_", i)
    }
    # 수정된 열 이름을 데이터프레임에 반영합니다.
    colnames(df) <- col_names
  }
  
  return(df)
}


## 🟧 구분의 중북 원소에 넘버링 ##############################################################################################################
add_numbering_to_duplicates <- function(df) {
  column <- df[, 3]  # 세 번째 열을 선택합니다.
  counts <- table(column)  # 각 원소의 빈도를 셉니다.
  counter <- list()  # 각 원소별로 번호를 매기기 위한 리스트입니다.
  
  # 빈도가 1 이상인 원소들에 대해 초기화합니다.
  for (name in names(counts)) {
    if (counts[name] > 1) {
      counter[[name]] <- 0
    }
  }
  
  # 원소들을 순회하며 중복된 원소에 번호를 붙입니다.
  for (i in seq_along(column)) {
    if (column[i] %in% names(counter)) {
      counter[[column[i]]] <- counter[[column[i]]] + 1
      column[i] <- paste0(column[i], "_", counter[[column[i]]])
    }
  }
  
  df[, 3] <- column  # 수정된 열을 데이터 프레임에 다시 할당합니다.
  return(df)
}

