# 🟥 임산물 가격 및 기타 가격 =====================================================================================================
## 🟧 가계용품 농가구입가격 ==============================================================================
name <- "가계용품 농가구입가격_Price of Household Goods Paid by Farmers"
combined.list[[name]] <-
  union_multiple(
    filter_text_data(text_data, c("price", "household", "goods", "paid", "farmers")),
    filter_text_data(text_data, c("가계용품", "농가", "구입", "가격"))
  )
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[!text_data %in% unlist(combined.list)]

## 🟧 주요상품 도매가격 ==============================================================================
name <- "주요상품 도매가격_Wholesale Prices of Major Commodities"
combined.list[[name]] <-
  union_multiple(
    filter_text_data(text_data, c("wholesale", "prices", "major", "commodities")),
    filter_text_data(text_data, c("주요상품", "도매", "가격"))
  )
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[!text_data %in% unlist(combined.list)]

## 🟧 묘목가격표 ==============================================================================
name <- "묘목가격표_Price List of Seedlings"
combined.list[[name]] <-
  union_multiple(
    filter_text_data(text_data, c("price", "list", "seedlings")),
    filter_text_data(text_data, c("묘목", "가격표"))
  )
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[!text_data %in% unlist(combined.list)]

## 🟧 제조업 생산종업원의 월당 급여액 및 출근 일수 ==============================================================================
name <- "제조업 생산종업원의 월당 급여액 및 출근 일수_Monthly Earnings and Man-Days of Production Workers in Manufacturing"
combined.list[[name]] <-
  union_multiple(
    filter_text_data(text_data, c("monthly", "earnings", "man-days", "production", "workers", "manufacturing")),
    filter_text_data(text_data, c("제조업", "생산종업원", "월당", "급여액", "출근", "일수"))
  )
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[!text_data %in% unlist(combined.list)]

## 🟧 종자가격표 ==============================================================================
name <- "종자가격표_Price List of Seed"
combined.list[[name]] <-
  union_multiple(
    filter_text_data(text_data, c("price", "list", "seed"), "seedling"),
    filter_text_data(text_data, c("종자", "가격표"))
  )
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[!text_data %in% unlist(combined.list)]

