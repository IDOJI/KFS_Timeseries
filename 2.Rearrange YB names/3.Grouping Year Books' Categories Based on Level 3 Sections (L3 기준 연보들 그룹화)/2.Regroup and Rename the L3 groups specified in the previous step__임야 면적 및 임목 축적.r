# 🟥 임야 면적 및 임목 축적 ============================================================================
#### 🟦 보전 임지 ===================================================================================================
name = "보전ㆍ준보전임지 지정현황_Area of Reserve and Semi-Reserve Forest"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("area", "reserve", "semi-reserve", "forest")),
                 filter_text_data(text_data, c("보전", "준보전임지", "지정", "현황")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]





#### 🟦 관리기관별 ===================================================================================================
##### 🟪 관리기관별, 임상별 산림면적 임목축적 ===========================================================================================================
name = "관리기관별, 임상별 산림면적 임목축적_Forest Land Area and Growing Stock by Management Authorities and Forest Type"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("forest", "land area", "management", "authorities", "Growing Stock", "forest type"), "산림청"),
                 filter_text_data(text_data, c("관리기관별", "임상별", "산림", "면적", "임목", "축적"), "산림청"),
                 filter_text_data(text_data, c("관리기관별", "임상별", "산림", "면적"), "산림청"),
                 filter_text_data(text_data, c("관리기관별", "임상별", "임목", "축적"), "산림청"))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]




#### 🟦 소관별 지종별 임야면적 및 임목축적 ===================================================================================================
##### 🟪 -행정구역별 =================================================================================================================================
name = "소관별ㆍ지종별 임야면적 및 임목축적_Forest Land Area and Growing Stock by Authorities Concerned and Land Classification"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("forest", "land", "area", "growing stock", "authorities", "concerned", "land classification"), "행정구역"),
                 filter_text_data(text_data, c("소관별", "지종별", "임야면적", "임목축적"), "행정구역"))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]




#### 🟦 용도별 산지 이용구분 조사 실적 ===================================================================================================
name = "산지 이용 구분_Classification of Forest Land Utilization"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("forest", "land", "use", "classification")),
                 filter_text_data(text_data, c("classification", "forest", "land", "utilization")),
                 filter_text_data(text_data, c("산지", "이용", "구분")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]





#### 🟦 행정구역별 임야/산림면적 및 임목축적 ===================================================================================================
##### 🟪 임야면적 및 임목축적 ===================================================================================================
name = "행정구역별 산림면적 및 임목축적_Forest Land Area and Growing Stock by Administrative Districts"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, 
                                  c("forest", "land", "area", "growing", "stock", "administrative", "districts"),
                                  c("영림서편", "소유별", "임상별")),
                 filter_text_data(text_data, 
                                  c("행정구역별", "면적", "임목축적"),
                                  c("영림서편", "소유별", "임상별")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]





##### 🟪 임상별 임목축적 ===================================================================================================
name = "행정구역별, 임상별, 임목축적_Forest Growing Stock by Forest Type and Administrative District"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, 
                                  c("forest", "growing", "stock", "forest", "type", "administrative", "district"),
                                  c("영림서편", "소유별", "영급별")),
                 filter_text_data(text_data, 
                                  c("행정구역별", "임상별", "축적"),
                                  c("영림서편", "소유별", "영급별")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]








##### 🟪 소관별 임야면적 ===================================================================================================
name = "행정구역별 소관별 임야면적_Forest Land Area by Ownership and Administrative District"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, 
                                  c("forest", "land", "area", "ownership", "administrative", "district"),
                                  c("영림서편", "소유별")),
                 filter_text_data(text_data, 
                                  c("행정구역별", "소관별", "임야면적"),
                                  c("영림서편", "소유별")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]





##### 🟪 행정구역별 소관별/소유별 임목축적 ===================================================================================================
name = "행정구역별, 소관별/소유별, 임목축적_Growing Stock by Ownership and Administrative District"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, 
                                  c("growing stock", "ownership", "administrative", "district"),
                                  c("영림서편", "임상별")),
                 filter_text_data(text_data, 
                                  c("행정구역별", "소관별", "임목", "축적"),
                                  c("영림서편", "소유별", "임상별")),
                 filter_text_data(text_data, 
                                  c("행정구역별", "소유별", "임목", "축적"),
                                  c("영림서편", "임상별")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]





##### 🟪 행정구역별 임상별 임야면적 ==============================================================================
name = "행정구역별 임상별 임야면적_Forest Land Area by Forest Type and Administrative District"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, 
                                  c("land area", "forest type", "administrative district"),
                                  c("소유별", "영급별")),
                 filter_text_data(text_data, 
                                  c("행정구역별", "임상별", "면적"), 
                                  c("소유별", "영급별")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]




#### 🟦 소유별 ======================================================================================================================
##### 🟪 소유 현황 ==================================================================================================================
name = "산림소유 현황_Status of Forest Ownership"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("status", "forest", "ownership"), c("사유림", "규모별")),
                 filter_text_data(text_data, c("산림", "소유", "현황"), c("사유림", "규모별")))
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
print(combined.list[[name]])
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]



##### 🟪 소유별 지종별 임야/산림면적 및 임목축적 ===================================================================================================
name = "소유별, 지종별 임야/산림면적 및 임목축적_Forest area and growing stock by ownership and land classification"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("forest", "area", "growing", "stock", "ownership", "land", "classification")),
                 filter_text_data(text_data, c("소유별", "지종별", "임야", "면적", "임목", "축적")))
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
print(combined.list[[name]])
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]



##### 🟪 소유별 임목축적 (-소관별, -영급별) ===================================================================================================
name = "소유별 임목축적_Forest growing stock by ownership"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("forest", "growing stock", "ownership"),c("area", "proportion", "구성", "administrative", "age class")),
                 filter_text_data(text_data, c("소유별", "임목", "축적"), c("면적", "구성", "소관별", "영급별")))
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
print(combined.list[[name]])
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]



##### 🟪 소유별 임목축적 구성  ===================================================================================================
name = "소유별 임목축적구성_Proportion of Forest Growing Stock by Ownership"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("proportion", "forest", "growing", "stock", "ownership")),
                 filter_text_data(text_data, c("소유별", "임목", "축적", "구성")))
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
print(combined.list[[name]])
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]




##### 🟪 소유별 임야면적_Area of forest land by ownership ===================================================================================================
name = "소유별 임야면적_Area of forest land by ownership"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, 
                                  c("forest land area", "ownership"), 
                                  c("행정구역", "영급별", "임상별", "임목", "사유림", "산림율", "산림률", "구성")),
                 filter_text_data(text_data, 
                                  c("소유별", "임야", "면적"), 
                                  c("행정구역", "영급별", "임상별", "임목", "사유림", "산림율", "산림률", "구성")))
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
print(combined.list[[name]])
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]





##### 🟪 "소유별 임야면적 구성_Proportion of Forest Land Area by Ownership" ===================================================================================================
name ="소유별 임야면적 구성_Proportion of Forest Land Area by Ownership"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, 
                                  c("forest land area", "ownership"), 
                                  c("행정구역", "영급별", "임상별", "임목", "사유림", "산림율", "산림률")),
                 filter_text_data(text_data, 
                                  c("소유별", "임야", "면적"), 
                                  c("행정구역", "영급별", "임상별", "임목", "사유림", "산림율", "산림률")))
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
print(combined.list[[name]])
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]





##### 🟪 "소유별 산림면적 및 산림율_Forest Land Area and % Land Area by Ownership" ===================================================================================================
name ="소유별 산림면적 및 산림율_Forest Land Area and % Land Area by Ownership"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, 
                                  c("forest land area", "ownership"), 
                                  c("행정구역", "영급별", "임상별", "임목", "사유림")),
                 filter_text_data(text_data, 
                                  c("소유별", "임야", "면적"), 
                                  c("행정구역", "영급별", "임상별", "임목", "사유림")))
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
print(combined.list[[name]])
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]





##### 🟪 소유별 임상별 임야면적 ===================================================================================================
name = "소유별ㆍ임상별 임야면적_Forest Land Area Ownership and Forest Type"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("forest land area", "ownership", "forest type"), "administrative"),
                 filter_text_data(text_data, c("소유별", "임상별", "임야", "면적")))
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
print(combined.list[[name]])
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]





##### 🟪 소유별 영급별 임야면적 ===================================================================================================
name = "소유별 영급별 임야면적_Area of Forest Land by Age Class and Ownership"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, 
                                  c("area", "forest", "land", "age", "class", "ownership"), 
                                  c("축적", "임상별")),
                 filter_text_data(text_data, 
                                  c("소관별", "영급별", "면적"), 
                                  c("축적", "임상별")),
                 filter_text_data(text_data, 
                                  c("소유별", "영급별", "면적"), 
                                  c("축적", "임상별")))
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
print(combined.list[[name]])
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]




#### 🟦 사유림 ===================================================================================================
##### 🟪 사유림 소유 규모 ===================================================================================================
name = "사유림 소유 규모_Ownership Scale of Private Forest"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("ownership", "scale", "private", "forest"), "산주"),
                 filter_text_data(text_data, c("사유림", "소유", "규모"), "산주"))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]




##### 🟪 사유림 소유형태별 임야/산림면적표 ===================================================================================================
name = "다. 기타_Others___사유임야 소유형태별 임야/산림면적표_Table of Private Forest Area by Ownership Form"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("table", "private", "forest", "area", "ownership", "form")),
                 filter_text_data(text_data, c("사유임야", "소유형태별", "임야", "면적표")),
                 filter_text_data(text_data, c("사유임야", "소유형태별", "산림", "면적표")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]



##### 🟪 소유별 산림/영림경영계획 작성현황 ===================================================================================================
name = "소유별 산림/영림경영계획 작성현황_Preparation of Forest Management Plan"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("preparation", "forest", "management", "plan")),
                 filter_text_data(text_data, c("소유별", "산림", "영림경영계획", "작성현황")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]





#### 🟦 영림서 ===================================================================================================
##### 🟪 영림서등관리 국유림 기관별, 임상별 산림면적 ===================================================================================================
name = "영림서등관리 국유림 기관별， 임상별 산림면적_Forest Area by National Forest management Authorities and Forest type"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("forest", "area", "national", "forest", "management", "authorities", "forest type"), "관리청"),
                 filter_text_data(text_data, c("영림서등관리", "국유림", "기관별", "임상별", "산림", "면적"), "관리청"))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]



##### 🟪 영림서등관리 국유림 기관별, 지종별 산림면적 및 임목축적 ===================================================================================================
name = "영림서등관리 국유림 기관별, 지종별, 산림면적 및 임목축적_Forest Land Area and Growing Stock by National Forest management Authorities and Land Classification"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("forest", "land", "area", "growing", "stock", "national", "forest", "management", "authorities", "land", "classification"), "관리청"),
                 filter_text_data(text_data, c("영림서등관리", "국유림", "기관별", "지종별", "산림면적", "임목축적"), "관리청"))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]




#### 🟦 산림청소관 국유림 관리 ===================================================================================================
##### 🟪 산림청소관 국유림 관리 기관별, 임상별 산림면적 ===================================================================================================
name = "산림청소관 국유림 관리 기관별, 임상별 산림면적_Forest Land Area by Management Agencies and Forest Type of National Forest under Forestry Administration"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("forest", "land area", "management agencies", "forest type", "national", "under", "forestry", "administration")),
                 filter_text_data(text_data, c("산림청소관", "국유림", "관리", "기관별", "임상별", "면적")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]


#### 🟦 평균임목축적 ===================================================================================================
name = "시ㆍ도ㆍ서별 1ha당 평균임목축적_Average Growing Stock per 1ha by City Province and National Forest Station"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("average", "growing", "stock", "1ha", "city", "province", "national", "forest", "station")),
                 filter_text_data(text_data, c("시", "도", "서별", "1ha당", "평균", "임목", "축적")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]





name = "연도별 ha당 평균임목축적_Mean Growing Stock per ha by Year"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("mean", "growing", "stock", "ha", "year")),
                 filter_text_data(text_data, c("연도별", "ha당", "평균", "임목", "축적")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]




#### 🟦 연도별 ===================================================================================================
##### 🟪 연도별 평균 임목축적 ===================================================================================================
name = "연도별 ㏊당 평균임목축적_Mean Growing Stock Per ㏊ by Year"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("mean", "growing", "stock", "per", "ha", "year")),
                 filter_text_data(text_data, c("연도별", "㏊당", "평균", "임목축적")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]



##### 🟪 연도별 임상별 임야면적 ===================================================================================================
name = "연도별 임상별 임야면적_Forest Land Area by Forest Type and Year"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("forest", "land", "area", "forest", "type", "year", "연도별"), "축적"),
                 filter_text_data(text_data, c("연도별", "임상별", "임야면적"), "축적"))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]




#### 🟦 영급별 ===================================================================================================
##### 🟪 임야면적 ===================================================================================================
name = "영급별 임야면적_Area of Forest Land by Age Class"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, 
                                  c("area", "forest", "land", "age", "class"),
                                  c("임상별", "축적", "소관별", "소유별")),
                 filter_text_data(text_data, 
                                  c("영급별", "임야면적"),
                                  c("임상별", "축적", "소관별", "소유별")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]



##### 🟪 임목 축적 ===================================================================================================
name = "영급별 임목축적_Forest growing stock by age class"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, 
                                  c("Forest growing stock", "age", "class"),
                                  c("임급별", "area", "소관별", "소유별", "임상별")),
                 filter_text_data(text_data, 
                                  c("영급별", "임목", "축적"),
                                  c("임급별", "면적", "소관별", "소유별", "임상별")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]



#### 🟦 임상별 ===================================================================================================
##### 🟪 임상별 임야면적_Ⅱ-4 Area of forest land by forest type ===================================================================================================
name = "임상별 임야면적_Area of forest land by forest type"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("forest", "land", "area", "forest", "type"), c("propotion", "proportion", "영급", "소유별", "년도별", "연도별", "기관별", "행정구역", "축적")),
                 filter_text_data(text_data, c("임상별", "임야", "면적"),  c("구성", "영급별", "소유별", "연도별", "기관별", "행정구역", "년도별", "연도별", "축적")),
                 filter_text_data(text_data, 
                                  c("forest", "growing", "stock", "forest", "type"),
                                  c("관리청등", "행정구역별", "구성", "영림서", "관리기관별", "영급별", "소유별", "관리 기관별", "년도별", "연도별", "축적")),
                 filter_text_data(text_data, 
                                  c("임상별", "임목축적"), 
                                  c("관리청등", "행정구역별", "구성", "영림서", "관리기관별", "영급별", "소유별", "관리 기관별", "년도별", "연도별", "축적")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]




##### 🟪 임상별 임목축적구성 ===================================================================================================
name = "임상별 임목축적구성_Proportion of forest growing stock by forest type"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("proportion", "forest growing stock", "forest type"), c("age class")),
                 filter_text_data(text_data, c("임상별", "임목", "축적", "구성"), c("영급별")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]





##### 🟪 임상별 영급별 임야면적 및 임목축적 ===================================================================================================
name = "임상별, 영급별 임야면적 및 임목축적_Forest Area and Growing Stock by Forest Type and Age-Classes"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, 
                                  c("forest area", "growing stock", "forest", "type", "age", "class"),
                                  c("영림서편", "소유별", "행정")),
                 filter_text_data(text_data, 
                                  c("임상별", "영급별", "면적", "임목축적"),
                                  c("영림서편", "소유별", "행정")),
                 filter_text_data(text_data, 
                                  c("Forest area and growing stok by forest type and age-classes"),
                                  c("영림서편", "소유별", "행정")),
                 filter_text_data(text_data, 
                                  c("임상별ㆍ영급별 임야면적 및 임목축적"),
                                  c("영림서편", "소유별", "행정")),
                 filter_text_data(text_data, 
                                  c("Forest Area by Forest Type and Age-Classes"),
                                  c("영림서편", "소유별", "행정")),
                 filter_text_data(text_data, 
                                  c("임상별, 영급별 임야면적 및 임목축적"),
                                  c("영림서편", "소유별", "행정")),
                 filter_text_data(text_data, 
                                  c("Forest Area and Growing Stock by Forest Type and Age-Classes"),
                                  c("영림서편", "소유별", "행정")),
                 filter_text_data(text_data, 
                                  c("12. 임상별, 영급별 임야면적 및 임목축적_12. Forest Area and Growing Stock by Forest Type and Age-Classes___Ｏ 동부영림서_Ｏ Eastern N.F.S"),
                                  c("영림서편", "소유별", "행정")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]




#### 🟦 전국 시 도 서편 =============================================================================================================
name = "가. 전국 및 시ㆍ도ㆍ서편_Details by country province city and N.F.S."
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("details", "by", "country", "province", "city", "N.F.S")),
                 filter_text_data(text_data, c("전국", "및", "서편")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]





#### 🟦 영급별 소유별 임목축적_Forest growing stock by age class and ownership ======================================================================================
name = "영급별 소유별 임목축적_Forest growing stock by age class and ownership"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("forest", "growing", "stock", "age", "class", "ownership")),
                 filter_text_data(text_data, c("영급별", "소유별", "임목")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]



#### 🟦 영림서편_Details by National Forest Station ======================================================================================
name = "영림서편_Details by National Forest Station"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("detail", "national", "forest", "station")),
                 filter_text_data(text_data, c("영림", "서편")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]




#### 🟦 기타_Others ======================================================================================
name = "기타_Others"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("others"), "해충"),
                 filter_text_data(text_data, c("기타"), "해충"))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]




#### 🟦 관리청등관리 국유림 기관별, 임상별 산림면적_Forest Area by National Forest management Authorities and Forest type ======================================================================================
name = "관리청등관리 국유림 기관별, 임상별 산림면적_Forest Area by National Forest management Authorities and Forest type"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("forest", "area", "national", "management", "authorities", "type")),
                 filter_text_data(text_data, c("관리청", "국유림", "기관별", "임상별", "면적")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]




#### 🟦 임상별, 영급별, 임목축적 구성_Proportion of Growing Stock by Forest Type and Age Class ======================================================================================
name = "임상별, 영급별, 임목축적 구성_Proportion of Growing Stock by Forest Type and Age Class"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("proportion", "growing", "stock", "forest", "type", "age", "class")),
                 filter_text_data(text_data, c("임상별", "영급별", "임목", "축적", "구성")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]



#### 🟦 관리청등관리 국유림 기관별, 지종별, 산림면적 및 임목축적_Forest Land Area and Growing Stock by National Forest management office Authorities and Land Classification ======================================================================================
name = "관리청등관리 국유림 기관별, 지종별, 산림면적 및 임목축적_Forest Land Area and Growing Stock by National Forest management office Authorities and Land Classification"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("growing", "stock", "national", "area", "land", "classification")),
                 filter_text_data(text_data, c("관리청", "국유림", "기관별", "지종별", "면적")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]



#### 🟦 구ㆍ시ㆍ군편_Details by GuㆍSiㆍGun ======================================================================================================================================================
name = "구ㆍ시ㆍ군편_Details by GuㆍSiㆍGun"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("detail", "Gu", "Si", "Gun")),
                 filter_text_data(text_data, c("구", "군편")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]

