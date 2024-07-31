# 🟥 임산무역 및 가공유통 =====================================================================================================
#### 🟦 해외 산림 개발 진출 현황 ==================================================================================================================
name = "해외 산림개발 진출현황_Oversea Forest Development"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("oversea", "forest", "development"), "개발목"),
                 filter_text_data(text_data, c("해외", "산림개발", "진출", "현황"), "개발목"))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 10, k_max = 10)[[1]]



#### 🟦 주요 임산물 수출실적 ==================================================================================================================
name = "주요 임산물 수출실적_Exports of Major Forest Products"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("exports", "major", "forest", "products")),
                 filter_text_data(text_data, c("주요", "임산물", "수출", "실적")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 10, k_max = 10)[[1]]


#### 🟦 주요 임산물 수입실적 ==================================================================================================================
name = "주요 임산물 수입실적_Imports of Major Forest Products"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("imports", "major", "forest", "products"), "국가"),
                 filter_text_data(text_data, c("주요", "임산물", "수입", "실적"), "국가"))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]


#### 🟦 "주요 국가별 임산물 수입실적_Imports of Forest Products by Major Countries" ==================================================================================================================
name = "주요 국가별 임산물 수입실적_Imports of Forest Products by Major Countries"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("imports", "major", "forest", "products")),
                 filter_text_data(text_data, c("주요", "국가", "임산물", "수입", "실적")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]




#### 🟦 제재목 생산 수급 실적 ==================================================================================================================
name = "제재목 생산 및 수급실적_Production and Supply of Sawnwood"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("production", "supply", "sandwood")),
                 filter_text_data(text_data, c("제재목", "생산", "수급", "실적")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]




#### 🟦 해외 산림 개발목 도입 실적 ==================================================================================================================
name = "해외 산림개발목 도입실적_Timber Imports by Oversea Forest Development"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("timber", "import", "ovresea")),
                 filter_text_data(text_data, c("해외", "산림", "개발목")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]




#### 🟦 주요 화물 철도 수송 ==================================================================================================================
name = "주요화물별 철도수송량_Railway Freight Transportation by Commodity"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("railway", "freight", "transportation", "commodity")),
                 filter_text_data(text_data, c("주요", "화물", "철도", "수송", "량")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]


name = "주요화물별 선박수송량_Marine Transportation by Commodity"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("marine", "transportation", "commodity")),
                 filter_text_data(text_data, c("주요", "화물", "선박", "수송", "량")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]


### 🟦 외재 도입액 ============================================================================================
#### 🟪 지역별 ============================================================================================
name = "지역별 외재도입액_Value of Imported Timber by Countries"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("value", "imported", "timber", "countries")),
                 filter_text_data(text_data, c("지역별", "외재", "도입", "액")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]

#### 🟪 용도별 ============================================================================================
name = "용도별 외재도입액_Value of Imported Timber by Use"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("value", "imported", "timber", "use")),
                 filter_text_data(text_data, c("용도별", "외재", "도입", "액")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]


### 🟦 외재 실적 ============================================================================================
#### 🟪 지역별 ============================================================================================
name = "지역별 외재도입실적_Timber Import by Source"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, 
                                  c("timber", "import", "source"),
                                  "용도별"),
                 filter_text_data(text_data, 
                                  c("지역별", "외재", "도입", "실적"), 
                                  "용도별"))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]


#### 🟪 용도별 ============================================================================================
name = "용도별 외재도입실적_Timber Import by Use"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("timber", "import", "use"), c("value")),
                 filter_text_data(text_data, c("용도별", "외재", "도입", "실적"), c("value")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]




#### 🟪 산지별 ============================================================================================
name = "산지별 외재도입실적_Timber Imports by Origin"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("timber", "imports", "origin")),
                 filter_text_data(text_data, c("산지별", "외재", "도입", "실적")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]



### 🟦 제재 공장 ============================================================================================
name = "제재공장 실태_Status of Sawmills"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("status", "sawmills")),
                 filter_text_data(text_data, c("제재", "공장", "실태")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]



### 🟦 포플러 제품 생산 ============================================================================================
name = "포플러 제품생산 및 공급실적_Production and Supply of Popular Products"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("production", "supply", "popular", "products")),
                 filter_text_data(text_data, c("production", "suppy", "popular", "products")),
                 filter_text_data(text_data, c("포플러", "제품", "생산", "공급", "실적")),
                 filter_text_data(text_data, c("포플라", "제품", "생산", "공급", "실적")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]



### 🟦 목재 가공품 ============================================================================================
name = "나무 및 나무제품 제조업_Manufacture of Wood and Wood Products"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("manufacture", "wood", "wood", "products")),
                 filter_text_data(text_data, c("나무", "나무제품", "제조업")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]



name = "목재가공품 생산 및 공급_Production and Supply of Processed Wood"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("production", "supply", "processed", "wood"), "목질"),
                 filter_text_data(text_data, c("목재", "가공품", "생산", "공급"), "목질"))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]




### 🟦 생산 실적 ============================================================================================
#### 🟪 지류 ============================================================================================
name = "지류생산실적_Paper Production"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("paper", "production")),
                 filter_text_data(text_data, c("지류", "생산", "실적")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]


#### 🟪 칩 ============================================================================================
name = "칩 생산실적_Chip Production"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("chip", "production")),
                 filter_text_data(text_data, c("칩", "생산", "실적")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]

#### 🟪 펄프 ============================================================================================
name = "펄프 생산실적_Pulp Production"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("pulp", "production")),
                 filter_text_data(text_data, c("펄프", "생산", "실적")),
                 filter_text_data(text_data, c("팔프", "생산", "실적")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]




#### 🟪 종이 ============================================================================================
name = "종이 및 종이제품 제조업_Manufacture of Paper and Paper Products"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("manufacture", "paper", "paper", "products")),
                 filter_text_data(text_data, c("종이", "종이제품", "제조업")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]


#
name = "임산물 남북교역 현황_Trade of Forest Products between South and North Korea"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("trade", "forest", "products", "South", "North", "Korea")),
                 filter_text_data(text_data, c("임산물", "남북교역", "현황")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]


# 가격
name = "연도별 목재가격_Lumber & Wood Price by Year"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("lumber", "wood", "price", "year")),
                 filter_text_data(text_data, c("연도별", "목재", "가격")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]


name = "주요 임산물 가격_Prices of Major Forest Products"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("prices", "major", "forest", "products")),
                 filter_text_data(text_data, c("주요", "임산물", "가격")),
                 filter_text_data(text_data, c("임산물", "가격", "동향")) )
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]





#
name = "임산물 유통시설 및 저온저장고 지원현황_Forest Products Marketing Facilities and Low Temperature Storage Houses Financially Supported"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("forest", "products", "marketing", "facilities", "low", "temperature", "storage", "houses", "financially", "supported")),
                 filter_text_data(text_data, c("임산물", "유통시설", "저온저장고", "지원", "현황")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]









