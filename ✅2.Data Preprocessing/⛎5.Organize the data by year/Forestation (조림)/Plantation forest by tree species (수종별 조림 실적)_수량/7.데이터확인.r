# 🟨 04 ~ 22 =========================================================================
path_data = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/조림/수종별 조림실적Plantation forest by tree species/Combined/4.Combined_04~22.xlsx"
data = read.xlsx(path_data) %>% 
  rename(class = 구분)

## 🟥 2016 ============================================================================
class_year = "2016"
sub = data %>% filter(class == class_year)
sub$year
keywords = list()
keywords[[1]] = c("소나무", "_잣나무", "낙엽송")
extract_columns_by_keywords(sub, keywords[[1]]) %>% View
keywords[[2]] = c("삼나무", "편백", "해송", "침엽수_기타")
extract_columns_by_keywords(sub, keywords[[2]]) %>% View
keywords[[3]] = c("고로쇠", "느티", "물푸레")
extract_columns_by_keywords(sub, keywords[[3]]) %>% View
keywords[[4]] = c("벚", "상수리", "자작", "활엽수_기타")
extract_columns_by_keywords(sub, keywords[[4]]) %>% View
is_na = extract_columns_by_include_exclude_keywords(sub, c(unlist(keywords), "합계"), "본수") %>% unlist
is.na(is_na) %>% sum == length(is_na)




## 🟥 2015 ============================================================================
class_year = "2015"
sub = data %>% filter(class == class_year)
sub$year
keywords = list()
keywords[[1]] = c("소나무", "_잣나무", "낙엽송")
extract_columns_by_keywords(sub, keywords[[1]]) %>% View
keywords[[2]] = c("삼나무", "편백", "해송", "침엽수_기타")
extract_columns_by_keywords(sub, keywords[[2]]) %>% View
keywords[[3]] = c("고로쇠", "느티", "물푸레")
extract_columns_by_keywords(sub, keywords[[3]]) %>% View
keywords[[4]] = c("벚", "상수리", "자작", "활엽수_기타")
extract_columns_by_keywords(sub, keywords[[4]]) %>% View
is_na = extract_columns_by_include_exclude_keywords(sub, c(unlist(keywords), "합계"), "본수") %>% unlist
is.na(is_na) %>% sum == length(is_na)




## 🟥 2014 ============================================================================
class_year = "2014"
sub = data %>% filter(class == class_year)
sub$year
keywords = list()
keywords[[1]] = c("소나무", "_잣나무", "낙엽송")
extract_columns_by_keywords(sub, keywords[[1]]) %>% View
keywords[[2]] = c("삼나무", "편백", "해송", "침엽수_기타")
extract_columns_by_keywords(sub, keywords[[2]]) %>% View
keywords[[3]] = c("고로쇠", "느티", "물푸레")
extract_columns_by_keywords(sub, keywords[[3]]) %>% View
keywords[[4]] = c("벚", "상수리", "자작", "활엽수_기타")
extract_columns_by_keywords(sub, keywords[[4]]) %>% View
is_na = extract_columns_by_include_exclude_keywords(sub, c(unlist(keywords), "합계"), "본수") %>% unlist
is.na(is_na) %>% sum == length(is_na)


## 🟥 2013 ============================================================================
class_year = "2013"
sub = data %>% filter(class == class_year)
sub$year
keywords = list()
keywords[[1]] = c("소나무", "_잣나무", "낙엽송")
extract_columns_by_keywords(sub, keywords[[1]]) %>% View
keywords[[2]] = c("삼나무", "편백", "해송", "침엽수_기타")
extract_columns_by_keywords(sub, keywords[[2]]) %>% View
keywords[[3]] = c("고로쇠", "느티", "물푸레")
extract_columns_by_keywords(sub, keywords[[3]]) %>% View
keywords[[4]] = c("벚", "상수리", "자작", "활엽수_기타")
extract_columns_by_keywords(sub, keywords[[4]]) %>% View
is_na = extract_columns_by_include_exclude_keywords(sub, c(unlist(keywords), "합계"), "본수") %>% unlist
is.na(is_na) %>% sum == length(is_na)



## 🟥 2012 ============================================================================
class_year = "2012"
sub = data %>% filter(class == class_year)
sub$year
keywords = list()
keywords[[1]] = c("소나무", "_잣나무", "낙엽송")
extract_columns_by_keywords(sub, keywords[[1]]) %>% View
keywords[[2]] = c("삼나무", "편백", "해송", "침엽수_기타")
extract_columns_by_keywords(sub, keywords[[2]]) %>% View
keywords[[3]] = c("고로쇠", "느티", "물푸레")
extract_columns_by_keywords(sub, keywords[[3]]) %>% View
keywords[[4]] = c("벚", "상수리", "자작", "활엽수_기타")
extract_columns_by_keywords(sub, keywords[[4]]) %>% View
is_na = extract_columns_by_include_exclude_keywords(sub, c(unlist(keywords), "합계"), "본수") %>% unlist
is.na(is_na) %>% sum == length(is_na)



## 🟥 2011 ============================================================================
class_year = "2011"
sub = data %>% filter(class == class_year)
sub$year
keywords = list()
keywords[[1]] = c("소나무", "_잣나무", "낙엽송")
extract_columns_by_keywords(sub, keywords[[1]]) %>% View
keywords[[2]] = c("삼나무", "편백", "해송", "침엽수_기타")
extract_columns_by_keywords(sub, keywords[[2]]) %>% View
keywords[[3]] = c("고로쇠", "느티", "물푸레")
extract_columns_by_keywords(sub, keywords[[3]]) %>% View
keywords[[4]] = c("벚", "상수리", "자작", "활엽수_기타")
extract_columns_by_keywords(sub, keywords[[4]]) %>% View
is_na = extract_columns_by_include_exclude_keywords(sub, c(unlist(keywords), "합계"), "본수") %>% unlist
is.na(is_na) %>% sum == length(is_na)


## 🟥 2011 ============================================================================
class_year = "2011"
sub = data %>% filter(class == class_year)
sub$year
keywords = list()
keywords[[1]] = c("소나무", "_잣나무", "낙엽송")
extract_columns_by_keywords(sub, keywords[[1]]) %>% View
keywords[[2]] = c("삼나무", "편백", "해송", "침엽수_기타")
extract_columns_by_keywords(sub, keywords[[2]]) %>% View
keywords[[3]] = c("고로쇠", "느티", "물푸레")
extract_columns_by_keywords(sub, keywords[[3]]) %>% View
keywords[[4]] = c("벚", "상수리", "자작", "활엽수_기타")
extract_columns_by_keywords(sub, keywords[[4]]) %>% View
is_na = extract_columns_by_include_exclude_keywords(sub, c(unlist(keywords), "합계"), "본수") %>% unlist
is.na(is_na) %>% sum == length(is_na)



## 🟥 2010 ============================================================================
class_year = "2010"
sub = data %>% filter(class == class_year)
sub$year
keywords = list()
keywords[[1]] = c("소나무", "_잣나무", "낙엽송")
extract_columns_by_keywords(sub, keywords[[1]]) %>% View
keywords[[2]] = c("삼나무", "편백", "해송", "침엽수_기타")
extract_columns_by_keywords(sub, keywords[[2]]) %>% View
keywords[[3]] = c("고로쇠", "느티", "물푸레")
extract_columns_by_keywords(sub, keywords[[3]]) %>% View
keywords[[4]] = c("벚", "상수리", "자작", "활엽수_기타")
extract_columns_by_keywords(sub, keywords[[4]]) %>% View
is_na = extract_columns_by_include_exclude_keywords(sub, c(unlist(keywords), "합계"), "본수") %>% unlist
is.na(is_na) %>% sum == length(is_na)




## 🟥 2008 ============================================================================
class_year = "2008"
sub = data %>% filter(class == class_year)
sub$year
keywords = list()
keywords[[1]] = c("소나무", "_잣나무", "낙엽송")
extract_columns_by_keywords(sub, keywords[[1]]) %>% View
keywords[[2]] = c("삼나무", "편백", "해송", "침엽수_기타")
extract_columns_by_keywords(sub, keywords[[2]]) %>% View
keywords[[3]] = c("고로쇠", "느티", "물푸레")
extract_columns_by_keywords(sub, keywords[[3]]) %>% View
keywords[[4]] = c("벚", "상수리", "자작", "활엽수_기타")
extract_columns_by_keywords(sub, keywords[[4]]) %>% View
is_na = extract_columns_by_include_exclude_keywords(sub, c(unlist(keywords), "합계"), "본수") %>% unlist
is.na(is_na) %>% sum == length(is_na)



## 🟥 2007 ============================================================================
class_year = "2007"
sub = data %>% filter(class == class_year)
sub$year
keywords = list()
keywords[[1]] = c("소나무", "_잣나무", "낙엽송")
extract_columns_by_keywords(sub, keywords[[1]]) %>% View
keywords[[2]] = c("삼나무", "편백", "해송", "침엽수_기타")
extract_columns_by_keywords(sub, keywords[[2]]) %>% View
keywords[[3]] = c("고로쇠", "느티", "물푸레")
extract_columns_by_keywords(sub, keywords[[3]]) %>% View
keywords[[4]] = c("벚", "상수리", "자작", "활엽수_기타")
extract_columns_by_keywords(sub, keywords[[4]]) %>% View
is_na = extract_columns_by_include_exclude_keywords(sub, c(unlist(keywords), "합계"), "본수") %>% unlist
is.na(is_na) %>% sum == length(is_na)


## 🟥 2006 ============================================================================
class_year = "2006"
sub = data %>% filter(class == class_year)
sub$year
keywords = list()
keywords[[1]] = c("소나무", "_잣나무", "낙엽송")
extract_columns_by_keywords(sub, keywords[[1]]) %>% View
keywords[[2]] = c("삼나무", "편백", "해송", "침엽수_기타")
extract_columns_by_keywords(sub, keywords[[2]]) %>% View
keywords[[3]] = c("고로쇠", "느티", "물푸레")
extract_columns_by_keywords(sub, keywords[[3]]) %>% View
keywords[[4]] = c("벚", "상수리", "자작", "활엽수_기타")
extract_columns_by_keywords(sub, keywords[[4]]) %>% View
is_na = extract_columns_by_include_exclude_keywords(sub, c(unlist(keywords), "합계"), "본수") %>% unlist
is.na(is_na) %>% sum == length(is_na)









## 🟨 2005 ============================================================================
class_year = "2005"
sub = data %>% filter(class == class_year)
sub$year
keywords = list()
keywords[[1]] = c("소나무", "_잣나무", "낙엽송")
extract_columns_by_keywords(sub, keywords[[1]]) %>% View

keywords[[2]] = c("리기다", "삼나무", "편백", "침엽수_기타")
extract_columns_by_keywords(sub, keywords[[2]]) %>% View
keywords[[3]] = c("느티", "물푸레", "벚")
extract_columns_by_keywords(sub, keywords[[3]]) %>% View
keywords[[4]] = c("상수리", "자작","고로쇠", "활엽수_기타")
extract_columns_by_keywords(sub, keywords[[4]]) %>% View
is_na = extract_columns_by_include_exclude_keywords(sub, c(unlist(keywords), "합계"), "본수") %>% unlist
is.na(is_na) %>% sum == length(is_na)



## 🟨 2004 ============================================================================
class_year = "2004"
sub = data %>% filter(class == class_year)
sub$year
keywords = list()
keywords[[1]] = c("소나무", "_잣나무", "낙엽송")
extract_columns_by_keywords(sub, keywords[[1]]) %>% View

keywords[[2]] = c("리기다", "삼나무", "편백", "침엽수_기타")
extract_columns_by_keywords(sub, keywords[[2]]) %>% View
keywords[[3]] = c("느티", "물푸레", "벚")
extract_columns_by_keywords(sub, keywords[[3]]) %>% View
keywords[[4]] = c("상수리", "자작","고로쇠", "활엽수_기타")
extract_columns_by_keywords(sub, keywords[[4]]) %>% View
is_na = extract_columns_by_include_exclude_keywords(sub, c(unlist(keywords), "합계"), "본수") %>% unlist
is.na(is_na) %>% sum == length(is_na)


## 🟨 2003 ============================================================================
class_year = "2003"
sub = data %>% filter(class == class_year)
sub$year
keywords = list()
keywords[[1]] = c("소나무", "_잣나무", "낙엽송")
extract_columns_by_keywords(sub, keywords[[1]]) %>% View

keywords[[2]] = c("리기다", "삼나무", "편백", "침엽수_기타")
extract_columns_by_keywords(sub, keywords[[2]]) %>% View
keywords[[3]] = c("느티", "물푸레", "벚")
extract_columns_by_keywords(sub, keywords[[3]]) %>% View
keywords[[4]] = c("상수리", "자작","고로쇠", "활엽수_기타")
extract_columns_by_keywords(sub, keywords[[4]]) %>% View
is_na = extract_columns_by_include_exclude_keywords(sub, c(unlist(keywords), "합계"), "본수") %>% unlist
is.na(is_na) %>% sum == length(is_na)



## 🟨 2002 ============================================================================
class_year = "2002"
sub = data %>% filter(class == class_year)
sub$year
keywords = list()
keywords[[1]] = c("소나무", "_잣나무", "낙엽송")
extract_columns_by_keywords(sub, keywords[[1]]) %>% View

keywords[[2]] = c("리기다", "삼나무", "편백", "침엽수_기타")
extract_columns_by_keywords(sub, keywords[[2]]) %>% View
keywords[[3]] = c("느티", "물푸레", "벚")
extract_columns_by_keywords(sub, keywords[[3]]) %>% View
keywords[[4]] = c("상수리", "자작","고로쇠", "활엽수_기타")
extract_columns_by_keywords(sub, keywords[[4]]) %>% View
is_na = extract_columns_by_include_exclude_keywords(sub, c(unlist(keywords), "합계"), "본수") %>% unlist
is.na(is_na) %>% sum == length(is_na)



## 🟨 2001 ============================================================================
class_year = "2001"
sub = data %>% filter(class == class_year)
sub$year
keywords = list()
keywords[[1]] = c("소나무", "_잣나무", "낙엽송")
extract_columns_by_keywords(sub, keywords[[1]]) %>% View

keywords[[2]] = c("리기다", "삼나무", "편백", "침엽수_기타")
extract_columns_by_keywords(sub, keywords[[2]]) %>% View
keywords[[3]] = c("느티", "물푸레", "벚")
extract_columns_by_keywords(sub, keywords[[3]]) %>% View
keywords[[4]] = c("상수리", "자작","고로쇠", "활엽수_기타")
extract_columns_by_keywords(sub, keywords[[4]]) %>% View
is_na = extract_columns_by_include_exclude_keywords(sub, c(unlist(keywords), "합계"), "본수") %>% unlist
is.na(is_na) %>% sum == length(is_na)


## 🟨 2000 ============================================================================
class_year = "2000"
sub = data %>% filter(class == class_year)
sub$year
keywords = list()
keywords[[1]] = c("소나무", "_잣나무", "낙엽송")
extract_columns_by_keywords(sub, keywords[[1]]) %>% View

keywords[[2]] = c("리기다", "삼나무", "편백", "침엽수_기타")
extract_columns_by_keywords(sub, keywords[[2]]) %>% View
keywords[[3]] = c("느티", "물푸레", "벚")
extract_columns_by_keywords(sub, keywords[[3]]) %>% View
keywords[[4]] = c("상수리", "자작","고로쇠", "활엽수_기타")
extract_columns_by_keywords(sub, keywords[[4]]) %>% View
is_na = extract_columns_by_include_exclude_keywords(sub, c(unlist(keywords), "합계"), "본수") %>% unlist
is.na(is_na) %>% sum == length(is_na)



# 🟨 data =====================================================================
path_data = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/조림/수종별 조림실적Plantation forest by tree species/Combined/2.Combined_82~99_국유림민유림.xlsx"
data = read.xlsx(path_data) %>% rename(class = 구분)



## 🟥 1998 ============================================================================
class_year = "1998"
sub = data %>% filter(class == class_year)
sub$year
keywords = list()
keywords[[1]] = c("_잣나무")
extract_columns_by_keywords(sub, keywords[[1]]) %>% View

keywords[[2]] = c("낙엽송", "삼나무", "편백")
extract_columns_by_keywords(sub, keywords[[2]]) %>% View

keywords[[3]] = c("리기다", "_테다" )
extract_columns_by_keywords(sub, keywords[[3]]) %>% View

keywords[[4]] = c("리기테다", "강송", "해송")
extract_columns_by_keywords(sub, keywords[[4]]) %>% View

keywords[[5]] = c("밤나무", "이태리")
extract_columns_by_keywords(sub, keywords[[5]]) %>% View

keywords[[6]] = c("현사시", "오동", "기타")
extract_columns_by_keywords(sub, keywords[[6]]) %>% View


is_na = extract_columns_by_include_exclude_keywords(sub, c(unlist(keywords), "합계"), "본수") %>% unlist
is.na(is_na) %>% sum == length(is_na)

## 🟥 1997 ============================================================================
class_year = "1997"
sub = data %>% filter(class == class_year)
sub$year
keywords = list()
keywords[[1]] = c("_잣나무")
extract_columns_by_keywords(sub, keywords[[1]]) %>% View

keywords[[2]] = c("낙엽송", "삼나무", "편백")
extract_columns_by_keywords(sub, keywords[[2]]) %>% View

keywords[[3]] = c("리기다", "_테다" )
extract_columns_by_keywords(sub, keywords[[3]]) %>% View

keywords[[4]] = c("리기테다", "강송", "해송")
extract_columns_by_keywords(sub, keywords[[4]]) %>% View

keywords[[5]] = c("밤나무", "이태리")
extract_columns_by_keywords(sub, keywords[[5]]) %>% View

keywords[[6]] = c("현사시", "오동", "기타")
extract_columns_by_keywords(sub, keywords[[6]]) %>% View


is_na = extract_columns_by_include_exclude_keywords(sub, c(unlist(keywords), "합계"), "본수") %>% unlist
is.na(is_na) %>% sum == length(is_na)



## 🟥 1996 ============================================================================
class_year = "1996"
sub = data %>% filter(class == class_year)
sub$year
keywords = list()
keywords[[1]] = c("_잣나무")
extract_columns_by_keywords(sub, keywords[[1]]) %>% View

keywords[[2]] = c("낙엽송", "삼나무", "편백")
extract_columns_by_keywords(sub, keywords[[2]]) %>% View

keywords[[3]] = c("리기다", "_테다" )
extract_columns_by_keywords(sub, keywords[[3]]) %>% View

keywords[[4]] = c("리기테다", "강송", "해송")
extract_columns_by_keywords(sub, keywords[[4]]) %>% View

keywords[[5]] = c("밤나무", "이태리")
extract_columns_by_keywords(sub, keywords[[5]]) %>% View

keywords[[6]] = c("현사시", "오동", "기타")
extract_columns_by_keywords(sub, keywords[[6]]) %>% View


is_na = extract_columns_by_include_exclude_keywords(sub, c(unlist(keywords), "합계"), "본수") %>% unlist
is.na(is_na) %>% sum == length(is_na)




## 🟥 1995=========================================================================
class_year = "1995"
sub = data %>% filter(class == class_year)
sub$year
keywords = list()
keywords[[1]] = c("_잣나무")
extract_columns_by_keywords(sub, keywords[[1]]) %>% View

keywords[[2]] = c("낙엽송", "삼나무", "편백")
extract_columns_by_keywords(sub, keywords[[2]]) %>% View

keywords[[3]] = c("리기다", "_테다" )
extract_columns_by_keywords(sub, keywords[[3]]) %>% View

keywords[[4]] = c("리기테다", "강송", "해송")
extract_columns_by_keywords(sub, keywords[[4]]) %>% View

keywords[[5]] = c("밤나무", "이태리")
extract_columns_by_keywords(sub, keywords[[5]]) %>% View

keywords[[6]] = c("현사시", "오동", "기타")
extract_columns_by_keywords(sub, keywords[[6]]) %>% View


is_na = extract_columns_by_include_exclude_keywords(sub, c(unlist(keywords), "합계"), "본수") %>% unlist
is.na(is_na) %>% sum == length(is_na)








## 🟥 1990 =========================================================================
class_year = "1990"
sub = data %>% filter(class == class_year)
sub$year
keywords = list()
keywords[[1]] = c("_잣나무")
extract_columns_by_keywords(sub, keywords[[1]]) %>% View

keywords[[2]] = c("낙엽송", "삼나무", "편백")
extract_columns_by_keywords(sub, keywords[[2]]) %>% View

keywords[[3]] = c("리기다", "_테다" )
extract_columns_by_keywords(sub, keywords[[3]]) %>% View

keywords[[4]] = c("리기테다", "강송", "해송")
extract_columns_by_keywords(sub, keywords[[4]]) %>% View

keywords[[5]] = c("밤나무", "이태리")
extract_columns_by_keywords(sub, keywords[[5]]) %>% View

keywords[[6]] = c("현사시", "오동", "기타")
extract_columns_by_keywords(sub, keywords[[6]]) %>% View


is_na = extract_columns_by_include_exclude_keywords(sub, c(unlist(keywords), "합계"), "본수") %>% unlist
is.na(is_na) %>% sum == length(is_na)


## 🟥 1988 =========================================================================
class_year = "1988"
sub = data %>% filter(class == class_year)
sub$year
keywords = list()
keywords[[1]] = c("_잣나무")
extract_columns_by_keywords(sub, keywords[[1]]) %>% View

keywords[[2]] = c("낙엽송", "삼나무", "편백")
extract_columns_by_keywords(sub, keywords[[2]]) %>% View

keywords[[3]] = c("리기다", "_테다" )
extract_columns_by_keywords(sub, keywords[[3]]) %>% View

keywords[[4]] = c("리기테다", "강송", "해송")
extract_columns_by_keywords(sub, keywords[[4]]) %>% View

keywords[[5]] = c("밤나무", "이태리")
extract_columns_by_keywords(sub, keywords[[5]]) %>% View

keywords[[6]] = c("현사시", "오동", "기타")
extract_columns_by_keywords(sub, keywords[[6]]) %>% View


is_na = extract_columns_by_include_exclude_keywords(sub, c(unlist(keywords), "합계"), "본수") %>% unlist
is.na(is_na) %>% sum == length(is_na)