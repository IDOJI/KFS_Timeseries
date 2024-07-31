# 🟥 산림자원 =====================================================================================================
## 🟧 산지이용 구분 현황 =============================================================
name = "산지이용 구분 현황_Status of Forest Land Use Classification"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("forest", "land", "use", "classification"), "용도별"),
                 filter_text_data(text_data, c("산지", "이용", "구분", "현황"), "용도별"))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 10, k_max = 10)[[1]]



## 🟧 산지 구분 현황_Status of Forest Land Classification =============================================================
name = "산지 구분 현황_Status of Forest Land Classification"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("status", "forest", "land", "classification")),
                 filter_text_data(text_data, c("status", "산지", "구분", "현황")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 10, k_max = 10)[[1]]



## 🟧 산림의 타용도별 감소현황 =============================================================
name = "산림의 타용도별 감소현황_Status of Forest Land Use Change"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("forest", "land", "use", "change")),
                 filter_text_data(text_data, c("산림", "타용도", "감소", "현황")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 10, k_max = 10)[[1]]





## 🟧 산지일시사용허가 · 신고 현황 ==============================================================================
name = "산지일시사용허가 · 신고 현황_Status of Temporary Forest Land Conversion Permit · Report"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("temporary", "forest", "land", "conversion", "permit", "report")),
                 filter_text_data(text_data, c("산지", "일시", "사용", "허가", "신고", "현황")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 10, k_max = 10)[[1]]







## 🟧 산림부문 온실가스 흡수량 ==============================================================================
name = "산림부문 온실가스 흡수량_GHG Removals in the Forestry Sector"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("GHG", "removals", "forestry", "sector")),
                 filter_text_data(text_data, c("산림", "온실가스", "흡수량")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 10, k_max = 10)[[1]]








# 🟥 텍스트 필터링해서 그룹화 =====================================================================================================
## 🟧 산림면적 및 임목축적 ==============================================================================
### 🟨 임상별 =========================================================================================================
#### 🟩 임상별, 지종별, 영급별 ==================================================================
name = "임상별ㆍ지종별ㆍ영급별 산림면적 및 임목축적_Forest Land Area and Growing Stock by Forest Types, Land Classes and Age Classes"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("forest type", "land class", "age class", "land area", "지종별")),
                 filter_text_data(text_data, c("임상별", "지종별", "영급별", "임목", "축적")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 10, k_max = 10)[[1]]



#### 🟩 임상별 영급별 (-지종별, -산지구분별, -행정구역별) ==================================================================
name = "임상별 · 영급별 산림면적 및 임목축적_Forest Area and Growing Stock by Forest Types and Age Classes"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data,
                                  c("forest type", "age classe", "forest area", "growing stock"),
                                  c("land class", "land classification")),
                 filter_text_data(text_data,
                                  c("임상별", "영급별", "산림", "면적", "임목", "축적"),
                                  c("지종별", "산지구분별", "행정")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 10, k_max = 10)[[1]]



### 🟨 산림청소관 =========================================================================================================
#### 🟩 관리기관별, 임상별 ==================================================================
name = "산림청소관 국유림 관리기관별, 임상별 산림면적 및 임목축적_Forest Land Area and Growing Stock by Forest Type and Forest Management offices under Korea Forest Service"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data,
                                  c("Forest Management offices", "forest type", "land area", "forest type"),
                                  c("land classification")),
                 filter_text_data(text_data,
                                  c("관리기관별", "임상별", "국유림", "면적", "산림"),
                                  c("산지구분별")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 10, k_max = 10)[[1]]




#### 🟩 관리기관별, 지종별 ==========================================================================
name = "산림청소관 국유림 관리기관별, 지종별 산림면적 및 임목축적_Forest Land Area and Growing Stock by Land Class and Forest Management Offices under Korea Forest Service"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("forest land area", "growing stock", "land class", "forest management office", "Korea Forest Service")),
                 filter_text_data(text_data, c("산림청", "국유림", "관리기관별", "지종별", "산림면적", "임목축적")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 10, k_max = 10)[[1]]




#### 🟩 관리기관별, 산지구분별 ==========================================================================
name = "산림청소관 국유림 관리기관별, 산지구분별 산림면적 및 임목축적_Forest Land Area and Growing Stock by Forest Type and Forest Management offices under Korea Forest Service"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data,
                                  c("forest land area", "growing stock", "Forest land classification", "Korea Forest Service", "Forest Management office"),
                                  c("Age Classes")),
                 filter_text_data(text_data,
                                  c("산림청", "국유림", "산지구분별", "산림면적", "임목축적"),
                                  c("영급별")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 10, k_max = 10)[[1]]




#### 🟩 지종별 (-산지구분별, -영급별) ==========================================================================
name = "산림청 소관 국유림, 지종별 산림면적 및 임목축적_Forest Land Area and Growing Stock of National Forests under Korea Forest Service by Land Classes"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data,
                                  c("forest", "land", "area", "growing", "stock", "national", "forests", "Korea", "Forest", "Service", "land", "classes"),
                                  c("Forest Type","Forest Land Classification", "Age class")),
                 filter_text_data(text_data,
                                  c("산림청", "소관", "국유림", "지종별", "산림면적", "임목축적"),
                                  c("산지구분별", "영급별", "임상별")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 10, k_max = 10)[[1]]




#### 🟩 지종별, 영급별 ==========================================================================
name = "산림청 국유림, 지종별ㆍ영급별 산림면적 및 임목축적_National Forest Land Area and Growing Stock by Land Classes and Age Classes"
combined.list[[name]] = 
  union_multiple(filter_text_data(text_data,
                                  c("National forest","land class", "age class", "land area"),
                                  c("forest type", "Land Classifications")),
                 filter_text_data(text_data,
                                  c("지종별", "영급별", "임목", "축적", "산림청"),
                                  c("임상별", "산지구분별", "산림청")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 10, k_max = 10)[[1]]



#### 🟩 산지구분별, 영급별 ==========================================================================
name = "산림청 소관 국유림, 산지구분별 · 영급별 산림면적 및 임목축적_Forest Area and Growing Stock of National Forests under the Jurisdiction of Korea Forest Service by Forest Land Classifications and Age Classes"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data,
                                  c("Land Classifications", "age class", "land area", "korea forest"),
                                  c("forest type")),
                 filter_text_data(text_data,
                                  c("산지구분별", "영급별", "임목", "축적", "산림청"),
                                  c("임상별", "지종별")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 10, k_max = 10)[[1]]




#### 🟩 임상별 (-산지구분별, -영급별, -관리기관별) ==========================================================================
name = "산림청 소관 국유림, 임상별 산림면적 및 임목축적_Forest Area and Growing Stock of National Forests under the Jurisdiction of Korea Forest Service by Forest Types"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data,
                                  c("forest", "area", "growing", "stock", "national", "forests", "jurisdiction", "Korea", "Forest", "Service", "forest type"),
                                  c("Forest Management Office")),
                 filter_text_data(text_data,
                                  c("산림청", "소관", "국유림", "임상별", "산림면적", "임목축적"),
                                  c("관리기관별")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 10, k_max = 10)[[1]]




### 🟨 소유별 =========================================================================================================
#### 🟩 소유별 산림면적 및 산림률 ==============================================================================
# name = "소유별 산림면적 및 산림률_Forest Land Area and % of Land per Hectare by Ownership"
# combined.list[[name]] =
#   union_multiple(filter_text_data(text_data, c("forest", "land", "area", "ownership", "percentage")),
#                  filter_text_data(text_data, c("소유", "산림", "면적", "산림률")),
#                  filter_text_data(text_data, c("Forest Land Area", "Ownership")),
#                  filter_text_data(text_data, c("Forest Area", "Ownership", "%")),
#                  filter_text_data(text_data, c("소유별", "산림", "면적", "산림율")))
# print(combined.list[[name]])
# #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# text_data = text_data[! text_data %in% unlist(combined.list)]
# # text_clustering(text_data, k_min = 10, k_max = 10)[[1]]
# 



#### 🟩 소유별 임목축적 및 ha당 축적 ==================================================================
name = "소유별 임목축적 및 ha당 임목축적_Forest Growing Stock and Growing Stock per Hectare by Ownership"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("forest growing stock", "ownership", "ha")),
                 filter_text_data(text_data, c("소유별", "임목", "축적", "ha당", "축적")),
                 filter_text_data(text_data, c("소유별", "임목", "축적", "ha당", "축적")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 10, k_max = 10)[[1]]






#### 🟩 관리기관별,영급별 ==================================================================
name = "관리기관별 영급별 산림면적ㆍ임목축적_Forest Land Area And Growing Stock By Management Agencies And Age Class"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data,
                                  c("age class", "Forest Management offices", "forest land area", "growing stock", "management agencies")),
                 filter_text_data(text_data,
                                  c("영급별", "관리기관별", "산림", "면적", "임목", "축적")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 10, k_max = 10)[[1]]




#### 🟩 수종별 산림면적 ==================================================================
name = "수종별 산림면적 및 임목축적 현황_Forest Land Area and Growing Stock by Tree Species"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("tree", "species", "forest land area", "growing stock")),
                 filter_text_data(text_data, c("수종별", "면적", "임목", "축적")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 10, k_max = 10)[[1]]




#### 🟩 임상별, 산지구분별, 영급별 ==================================================================
name = "임상별 · 산지구분별 · 영급별 산림면적 및 임목축적_Forest Area and Growing Stock by Forest Types, Forest Land Classifications and Age Classes"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data,
                                  c("land classification", "forest type", "age classe", "forest area", "growing stock"),
                                  c("land class")),
                 filter_text_data(text_data,
                                  c("산지구분별", "임상별", "영급별", "산림", "면적", "임목", "축적"),
                                  c("지종별")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 10, k_max = 10)[[1]]




### 🟨 연도별 =========================================================================================================
#### 🟩 연도별, 임상별 ==================================================================
name = "연도별·임상별 산림면적 및 임목축적_Forest Land Area, Growing Stock by Year and Forest Types"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("year", "forest type","forest land area", "growing stock"), c("ha당", "㏊당", "㏊ 당")),
                 filter_text_data(text_data, c("연도별", "임상별", "산림", "면적", "임목", "축적"), c("ha당", "㏊당", "㏊ 당")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 10, k_max = 10)[[1]]




#### 🟩 연도별, 임상별 ==================================================================
name = "연도별·임상별 산림면적 및 임목축적_Forest Land Area, Growing Stock by Year and Forest Types"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("year", "forest type","forest land area", "growing stock", "ha")),
                 filter_text_data(text_data, c("연도별", "임상별", "산림", "면적", "임목", "축적", "ha")),
                 filter_text_data(text_data, c("연도별", "임상별", "산림", "면적", "임목", "축적", "㏊")))

print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 10, k_max = 10)[[1]]





#### 🟩 연도별 (-임상별, -기관별) ==================================================================
name = "연도별 산림면적 및 임목축적_Forest Land Area and Growing Stock by Year"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data,
                                  c("year", "forest land area", "growing stock"),
                                  c("forest type", "Management Agencies")),
                 filter_text_data(text_data,
                                  c("연도별", "산림", "면적", "임목", "축적"),
                                  c("임상별", "기관별")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 10, k_max = 10)[[1]]



#### 🟩 연도별,기관별 (-임상별) ==================================================================
name = "연도별 기관별 산림면적 및 임목축적_Forest Land Area and Growing Stock by Year and Management Agencies"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data,
                                  c("year", "Management Agencies", "forest land area", "growing stock"),
                                  c("forest type")),
                 filter_text_data(text_data,
                                  c("연도별", "기관별", "산림", "면적", "임목", "축적"),
                                  c("임상별")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 10, k_max = 10)[[1]]



### 🟨 행정구역별 =========================================================================================================
#### 🟩 행적 구역별 (-임상별) ==========================================================================
name = "행정구역별 산림면적 및 임목축적_Forest Land Area And Growing Stock By Administrative Districts"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data,
                                  c("forest land area", "growing stock", "administrative districts"),
                                  c("forest type")),
                 filter_text_data(text_data,
                                  c("행정구역별", "산림", "면적", "임목", "축적"),
                                  c("임상별")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 10, k_max = 10)[[1]]




#### 🟩 행적 구역별 + 임상별 ==========================================================================
name = "행정구역별 임상별 영급별 산림면적 및 임목축적_Forest Land Area and Growing Stock by Administrative Districts, Forest Type and Age Class"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data,
                                  c("forest type","forest land area", "growing stock", "administrative districts")),
                 filter_text_data(text_data,
                                  c("임상별", "행정구역별", "산림", "면적", "임목", "축적")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 10, k_max = 10)[[1]]






#### 🟩 지역별 ==========================================================================
name = "지역별 산림면적 및 축적_Forest Area and Growing Stock by Province"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data,
                                  c("forest area", "growing stock", "province"),
                                  c("age class", "land classification")),
                 filter_text_data(text_data,
                                  c("지역별", "산림", "면적", "축적"),
                                  c("영급별", "산지구분별")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 10, k_max = 10)[[1]]



### 🟨 지종별 =========================================================================================================
#### 🟩 시·도별 · 산지구분별 · 영급별 산림면적 및 임목축적==========================================================================
name = "시·도별 · 산지구분별 · 영급별 산림면적 및 임목축적_Forest Area and Growing Stock by Provinces, Forest Land Classifications and Age Classes"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data,
                                  c("forest", "area", "growing", "stock", "province", "age", "class", "도별"),
                                  c("Forest Management Office", "관리")),
                 filter_text_data(text_data,
                                  c("영급별", "산림면적", "임목축적", "산지구분", "도별"),
                                  c("관리기관별", "지종별")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# # text_clustering(text_data, k_min = 10, k_max = 10)[[1]]



#### 🟩 지종별 · 영급별 산림면적 및 임목축적==========================================================================
name = "지종별-영급별 산림면적 및 임목축적_Forest Land Area and Growing Stock by Provinces, Land Classes and Age Classes"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data,
                                  c("forest", "area", "growing", "stock", "province", "age", "class"),
                                  c("Forest Management Office", "관리")),
                 filter_text_data(text_data,
                                  c("영급별", "산림면적", "임목축적", "산지구분", "지종별"),
                                  c("관리기관별")))

print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# # text_clustering(text_data, k_min = 10, k_max = 10)[[1]]




## 🟧 지종별 영급별 산림 면적 및 임목축적 ==============================================================================
name = "지종별·영급별 산림면적 및 임목축적_Forest land area and growing stock by land classes and age classes"
combined.list[[name]] =
  union_multiple(
    filter_text_data(text_data, c("forest", "land", "area", "growing", "stock", "land", "classes", "age", "classes")),
    filter_text_data(text_data, c("지종별", "영급별", "산림면적", "임목축적"))
  )
print(combined.list[[name]])
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 10, k_max = 10)[[1]]




# 🟥 텍스트 필터링해서 그룹화 =====================================================================================================
## 🟧 인공 조림지 현황 ==============================================================================
name = "인공 조림지 현황_Area of plantation forests"
combined.list[[name]] =
  union_multiple(
    filter_text_data(text_data, c("area", "plantation", "forests")),
    filter_text_data(text_data, c("인공", "조림지", "현황"))
  )
print(combined.list[[name]])
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 10, k_max = 10)[[1]]


## 🟧 소유별 조림면적 ==============================================================================
name = "소유별 조림면적_Plantation forest area by ownership"
combined.list[[name]] =
  union_multiple(
    filter_text_data(text_data, c("plantation", "forest", "area", "ownership"), c("민유림")),
    filter_text_data(text_data, c("소유별", "조림", "면적"), c("민유림")),
    filter_text_data(text_data, c("plantation", "area", "ownership"), c("민유림")),
    filter_text_data(text_data, c("소유별", "조림", "실적"), c("민유림")),
    filter_text_data(text_data, c("소유별", "조림"), c("민유림"))
  )
print(combined.list[[name]])
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 10, k_max = 10)[[1]]


## 🟧 소유별 민유림 조림면적 ==============================================================================
name = "소유별 민유림조림실적_Reforestation in non-national forest by ownership"
combined.list[[name]] =
  union_multiple(
    filter_text_data(text_data, c("plantation", "forest", "area", "ownership")),
    filter_text_data(text_data, c("소유별", "조림", "면적")),
    filter_text_data(text_data, c("plantation", "area", "ownership")),
    filter_text_data(text_data, c("소유별", "조림", "실적")),
    filter_text_data(text_data, c("소유별", "조림"))
  )
print(combined.list[[name]])
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 10, k_max = 10)[[1]]



## 🟧 숲 가꾸기 ==============================================================================
name = "숲 가꾸기_Forest tending"
combined.list[[name]] =
  union_multiple(
    filter_text_data(text_data, c("forest", "tending"), "벌채"),
    filter_text_data(text_data, c("숲", "가꾸기"), "벌채")
  )
print(combined.list[[name]])
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 10, k_max = 10)[[1]]





## 🟧 조림 실적 ==============================================================================
#### 🟩 재원별 ==============================================================================
name = "재원별 조림실적_Planted area by financial sources"
combined.list[[name]] =
  union_multiple(
    filter_text_data(text_data, c("planted", "area", "financial", "sources")),
    filter_text_data(text_data, c("재원별", "조림", "실적"))
  )
print(combined.list[[name]])
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 10, k_max = 10)[[1]]



#### 🟩 수종별 ==============================================================================
name = "수종별 조림실적_Plantation forest by tree species"
combined.list[[name]] =
  union_multiple(
    filter_text_data(text_data, c("plantation", "forest", "tree", "species"), "민유림"),
    filter_text_data(text_data, c("수종별", "조림", "실적"), "민유림")
  )
print(combined.list[[name]])
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 10, k_max = 10)[[1]]




## 🟧 양묘사업 ==============================================================================
name = "양묘사업 및 생산현황_Status of seedling plantation and production"
combined.list[[name]] =
  union_multiple(
    filter_text_data(text_data, c("status", "seedling", "plantation", "production")),
    filter_text_data(text_data, c("양묘", "사업", "생산", "현황")),
    filter_text_data(text_data, c("양묘"))
  )
print(combined.list[[name]])
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 10, k_max = 10)[[1]]




## 🟧 조림 활착 상황 ==============================================================================
name = "조림 활착상황_Survival rate of reforestation"
combined.list[[name]] =
  union_multiple(
    filter_text_data(text_data, c("survival", "rate", "reforestation")),
    filter_text_data(text_data, c("조림", "활착"))
  )
print(combined.list[[name]])
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 10, k_max = 10)[[1]]




## 🟧 사회공헌형 산림탄소상쇄사업 현황 ==============================================================================
name = "사회공헌형 산림탄소상쇄사업 현황_Status of Forest Carbon Offset Projects - Social Contribution Type"
combined.list[[name]] =
  union_multiple(
    filter_text_data(text_data, c("forest", "carbon", "offset", "projects", "social", "contribution")),
    filter_text_data(text_data, c("사회공헌형", "산림", "탄소", "상쇄", "사업", "현황"))
  )
print(combined.list[[name]])
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 10, k_max = 10)[[1]]




## 🟧 국유재산 취득 및 처분 현황 ==============================================================================
name = "국유재산 취득 및 처분 현황_Acquisition and Disposal of National Forest"
combined.list[[name]] =
  union_multiple(
    filter_text_data(text_data, c("acquisition", "disposal", "national", "forest")),
    filter_text_data(text_data, c("국유재산", "취득", "처분", "현황"))
  )
print(combined.list[[name]])
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 10, k_max = 10)[[1]]





## 🟧 대부/사용허가 ==============================================================================
#### 🟩 indisposable ==============================================================================
name = "indisposable 국유림 사용허가 현황_Status of Lease Permission on Indispensable National Forest"
combined.list[[name]] =
  union_multiple(
    filter_text_data(text_data, c("lease", "permission", "indispensable", "national", "forest")),
    filter_text_data(text_data, c("국유림", "사용", "허가"), c("dispensable")),
    filter_text_data(text_data, c("국유림", "사용", "허가"), c("disposable"))
  )
print(combined.list[[name]])
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 10, k_max = 10)[[1]]


#### 🟩 disposable ==============================================================================
name = "disposable 국유림 대부현황_Status of Lease Permission on Disposable National Forest"
combined.list[[name]] =
  union_multiple(
    filter_text_data(text_data, c("lease", "permission", "disposable", "national", "forest"), c("indispensable")),
    filter_text_data(text_data, c("lease", "permission", "disposable", "national", "forest"), c("indisposable")),
    filter_text_data(text_data, c("국유림", "대부", "현황"), c("indispensable", "indisposable")),
    filter_text_data(text_data, c("국유림", "사용", "허가"), c("indispensable", "indisposable"))
  )
print(combined.list[[name]])
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 10, k_max = 10)[[1]]



## 🟧 산림 전용 ==============================================================================
name = "산림의 타용도 전용현황_Status of Forest Land Conversion"
combined.list[[name]] =
  union_multiple(
    filter_text_data(text_data, c("forest", "land", "conversion")),
    filter_text_data(text_data, c("산림", "타용도", "전용", "현황"))
  )
print(combined.list[[name]])
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 10, k_max = 10)[[1]]









