### 🟨 국토와 자연환경 ================================================================================
#### 🟦 대한민국 위치==================================================================================
name = "대한민국의 위치_Location of Repubic of Korea"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("location", "korea")),
                 filter_text_data(text_data, c("대한민국", "위치")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]


#### 🟦 국토 이용 상황 ==================================================================================
name = "국토이용 상황_Status of national land utilization"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("national", "land", "utilization")),
                 filter_text_data(text_data, c("국토", "이용")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]


#### 🟦 측후소 일람표 ==================================================================================
name = "측후소일람표_List of meteorological stations"
combined.list[[name]] = 
  union_multiple(filter_text_data(text_data, c("meteorological", "station")),
                 filter_text_data(text_data, c("측후소")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]


#### 🟦 시도별 면적 및 행정 단위 ==================================================================================
name = "시도별 면적 및 행정단위_Area and administrative unit by province and cities"
combined.list[[name]] = 
  union_multiple(filter_text_data(text_data, c("area", "administrative", "unit"), c("축적", "Gu")),
                 filter_text_data(text_data, c("면적", "행정", "단위"), c("면적", "축적", "Gu")),
                 filter_text_data(text_data, c("면적", "행정구역"), c("축적", "면적", "Gu")),
                 filter_text_data(text_data, c("Area", "and", "administrative", "unit", "by", "province")),
                 filter_text_data(text_data, c("시ㆍ도별", "면적", "및", "행정단위")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]




#### 🟦 강수량표 ==================================================================================
name = "강수량표_Precipitation"
combined.list[[name]] = 
  union_multiple(filter_text_data(text_data, c("precipitation")),
                 filter_text_data(text_data, c("강수량")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# combined.list[[name]] %in% text_data



#### 🟦 일조시간 ==================================================================================
name = "일조 시간_Hours of Sunshine"
combined.list[[name]]=
  union_multiple(filter_text_data(text_data, c("sunshine")),
                 filter_text_data(text_data, c("일조", "시간")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]


#### 🟦 인구변동 추이 ==================================================================================
name = "인구변동 추이_Population Trend"
combined.list[[name]]=
  union_multiple(filter_text_data(text_data, c("population", "trend")),
                 filter_text_data(text_data, c("인구", "추이")),
                 filter_text_data(text_data, c("인구", "추세")),
                 filter_text_data(text_data, c("인구", "추계")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]


#### 🟦 상대습도 ==================================================================================
name = "상대습도_Relative humidity"
combined.list[[name]]=
  union_multiple(filter_text_data(text_data, c("relative", "humidity")),
                 filter_text_data(text_data, c("상대", "습도")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]


#### 🟦 평균기온  ==================================================================================
name = "평균 기온_Average Temperature"
combined.list[[name]]=
  union_multiple(filter_text_data(text_data, c("average", "temper")),
                 filter_text_data(text_data, c("평균", "기온")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]


#### 🟦 지상기상 관측지검 ==================================================================================
name = "지상기상관측지점_List of the Surface Synoptic Stations"
combined.list[[name]]=
  union_multiple(filter_text_data(text_data, c("surface", "synoptic", "station")),
                 filter_text_data(text_data, c("지상", "기상", "관측")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]




## 🟧 영림계획 =====================================================================================================
#### 🟦 산림/영림계획 작성 현황 ===================================================================================================
name = "산림/영림경영계획 작성현황_Preparation of Forest Management Plan"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("preparation", "forest", "management", "plan"), c("소유별", "ownership")),
                 filter_text_data(text_data, c("산림", "경영", "계획", "작성"), c("소유별", "ownership")),
                 filter_text_data(text_data, c("영림", "계획", "작성"), c("소유별", "ownership")))
print(combined.list[[name]])
text_data = text_data[! text_data %in% unlist(combined.list)]


#### 🟦 소유별 영림계획 작성현황_8. Preparation of Forest Management Plan by Ownership ===================================================================================================
name = "소유별 영림계획 작성현황_Preparation of Forest Management Plan by Ownership"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("preparation", "forest", "management", "plan", "ownership")),
                 filter_text_data(text_data, c("산림", "경영", "계획", "작성", "소유별")),
                 filter_text_data(text_data, c("영림", "계획", "작성", "소유별")))
print(combined.list[[name]])
text_data = text_data[! text_data %in% unlist(combined.list)]






## 🟧 국유림 관리 =====================================================================================================
#### 🟦 국유림 직영 벌채 사업 생산  ==================================================================================
name = "국유림직영벌채사업생산 및 매각실적_Production and Sales of Timber by Government Felling"
combined.list[[name]] = 
  union_multiple(filter_text_data(text_data, include = c("production", "timber", "government", "felling")),
                 filter_text_data(text_data, include = c("직영", "벌채", "felling")),
                 filter_text_data(text_data, include = c("관행", "작벌", "매각실적")),
                 filter_text_data(text_data, include = c("국유림", "벌채", "매각")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]





#### 🟦 임야 매각 및 매수/ 국유재산 취득 처분  ==================================================================================
name = "임야매각 및 매수/국유재산취득 및 처분_Disposal & purchase of Forest Land"
combined.list[[name]] = 
  union_multiple(filter_text_data(text_data, include = c("disposal", "purchase", "land", "forest"), "national"),
                 filter_text_data(text_data, include = c("임야", "매각", "매수"), "national"),
                 filter_text_data(text_data, include = c("국유", "재산", "취득"), "national"))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]




#### 🟦 국유림 대부(사용허가)) 현황  ==================================================================================
##### 🟪 불요존 (disposable) =============================================================================================
name = "불요존 국유림 대부(사용허가)현황_Status of land lease out of disposable national forest"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, include = c("lease out", " disposable")),
                 filter_text_data(text_data, include = c("lease out", " dispensable")),
                 filter_text_data(text_data, c("status", "land", "lease", "disposable", "national", "forest"), "indis"),
                 filter_text_data(text_data, c("불요존", "국유임", "대부", "현황")),
                 filter_text_data(text_data, c("불요존", "국유림", "대부", "현황")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]




##### 🟪 요존 (indisposable) =============================================================================================
name = "요존 국유림 대부(사용허가) 현황_Status of Land Lease out of Indispensable National Forest"
combined.list[[name]] = 
  union_multiple(filter_text_data(text_data, 
                                  c("lease out", " indispensable")),
                 filter_text_data(text_data, 
                                  c("status", "land", "lease", " indisposable", "forest"), 
                                  " disposable"),
                 filter_text_data(text_data, 
                                  c("land", "lease", " indisposable", "national", "forest"), 
                                  " disposable"),
                 filter_text_data(text_data, 
                                  c("요존", "국유임", "대부", "현황"), 
                                  "불요존"),
                 filter_text_data(text_data, 
                                  c("요존", "국유림", "대부", "현황"), 
                                  "불요존"))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]




#### 🟦 산림/영림계획 편성 실적  ==================================================================================
name = "영림계획 편성실적_Accomplishment of Forest Management Plan by Ownership"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("accomplishment", "forest", "management", "plan")),
                 filter_text_data(text_data, c("산림", "경영", "계획", "편성", "실적")),
                 filter_text_data(text_data, c("영림", "계획", "편성", "실적")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]







#### 🟦 국유림 부산물 처분 실적  ==================================================================================
name = "국유임야 부산물 처분실적_Sales of Minor Forest Products from National Forest"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("sales", "minor", "forest", "products", "national", "forest")),
                 filter_text_data(text_data, c("국유임", "부산물", "처분", "실적")),
                 filter_text_data(text_data, c("국유림", "부산물", "처분", "실적")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]








#### 🟦 국유림 분수림  ==================================================================================
##### 🟪 불요존 =============================================================================================
name = "불요존 국유림 분수림 현황_Status of Porfit-sharing Forest of Disposable National Forest"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, include = c("profit", "sharing", " disposable")),
                 filter_text_data(text_data, include = c("불요존", "분수림")),
                 filter_text_data(text_data, include = c("profit", "sharing", " dispensable")),
                 filter_text_data(text_data, include = c("profit-sharing", " dispensable")),
                 filter_text_data(text_data, c("status", "profit-sharing", "forest", " disposable", "national", "forest")),
                 filter_text_data(text_data, c("불요존", "국유임", "분수림", "현황")),
                 filter_text_data(text_data, c("불요존", "국유림", "분수림", "현황")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 4, k_max = 4)[[1]]






#### 🟦 임도시설  ==================================================================================
name = "임도시설 현황_Status of Forest Road"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("status", "forest", "road")),
                 filter_text_data(text_data, c("임도", "시설", "현황")),
                 filter_text_data(text_data, c("construction", "forest", "road")),
                 filter_text_data(text_data, c("임도", "시설")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 4, k_max = 4)[[1]]



# 🟥 사유림 =====================================================================================================
name = "사유림 소유규모별 산주현황_Private Forest Owners by Size"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("private", "forest", "owner", "size")),
                 filter_text_data(text_data, c("사유림", "산주", "규모")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]




name = "사유림 소유형태별 산주현황_Private Forest Owners By Ownership"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("private", "forest", "owner", "ownership", "산주현황"), "도별"),
                 filter_text_data(text_data, c("사유림", "산주", "형태"), "도별"))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]





name = "사유림 소유형태별 산림면적_Private Forest Land Area by Ownership"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("private", "forest", "ownership", "land")),
                 filter_text_data(text_data, c("사유림", "소유", "형태", "산림")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]






name = "사유림 소재ㆍ부재 산주현황_Status of Resident and Absentee Forest Owners by Province"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("resident", "absolute", "forest", "owner", "province"), "도별"),
                 filter_text_data(text_data, c("사유림", "부재", "산주"), "도별"))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]








name = "사유림 소유형태별 필지수 현황_Private Forest Lots By Ownership"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("private", "forest", "ownership", "lots")),
                 filter_text_data(text_data, c("사유림", "형태", "필지수")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]








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
name = "산림청 소관 국유림, 산지구분별 산림면적 및 임목축적_Forest Land Area and Growing Stock of National Forests under the jurisdiction of Korea Forest Service by Forest Land Classifications"
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
  union_multiple(filter_text_data(text_data, 
                                  c("year", "forest type","forest land area", "growing stock"), 
                                  c("ha당", "㏊당", "㏊ 당")),
                 filter_text_data(text_data, 
                                  c("연도별", "임상별", "산림", "면적", "임목", "축적"), 
                                  c("ha당", "㏊당", "㏊ 당")),
                 filter_text_data(text_data, 
                                  c("Forest", "Land", "Area", "growing", "stock","by", "Forest", "type", "year"), 
                                  c("ha당", "㏊당", "㏊ 당")),
                 filter_text_data(text_data, 
                                  c("연도", "임상별", "면적", "축적"), 
                                  c("ha당", "㏊당", "㏊ 당")),
                 filter_text_data(text_data, 
                                  c("연도", "임상별", "면적", "축적"),
                                  c("ha당", "㏊당", "㏊ 당")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 10, k_max = 10)[[1]]





#### 🟩 연도별, 임상별 ==================================================================
name = "연도별ㆍ임상별 산림면적, 임목축적, ha당 임목축적_Forest Land Area, Growing Stock, and Growing Stock per Hectare, by Year and Forest Type"
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
                                  c("Forest Management Office", "관리", "지종별")),
                 filter_text_data(text_data,
                                  c("영급별", "산림면적", "임목축적", "산지구분", "도별"),
                                  c("관리기관별", "지종별")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# # text_clustering(text_data, k_min = 10, k_max = 10)[[1]]







## 🟧 지종별 영급별 산림 면적 및 임목축적 ==============================================================================
name = "지종별·영급별 산림면적 및 임목축적_Forest land area and growing stock by land classes and age classes"
combined.list[[name]] =
  union_multiple(
    filter_text_data(text_data, 
                     c("forest", "land", "area", "growing", "stock", "land", "classes", "age", "classes"),
                     c("산지구분", "임상별", "산림청", "도별")),
    filter_text_data(text_data, 
                     c("지종별", "영급별", "산림면적", "임목축적"), 
                     c("산지구분", "임상별", "산림청", "도별")),
    filter_text_data(text_data,
                     c("forest", "area", "growing", "stock", "province", "age", "class"),
                     c("산지구분", "Forest Management Office", "관리", "임상별", "산림청", "도별")),
    filter_text_data(text_data,
                     c("영급별", "산림면적", "임목축적", "산지구분", "지종별"),
                     c("산지구분", "관리기관별", "임상별", "산림청", "도별"))
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
                 filter_text_data(text_data, c("제재목", "생산", "수급", "실적")),
                 filter_text_data(text_data, c("production", "supply", "sawnwood")),
                 filter_text_data(text_data, c("제재목", "생산", "실적")))
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




# 🟥 임산물 시장 =====================================================================================================
# 국내총생산과 임업
name = "국내총생산과 임업_Gross Domestic Product and Forestry"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("gross", "domestic", "product", "forestry")),
                 filter_text_data(text_data, c("국내", "총생산","임업")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]






# 국내 총 생산과 임산물생산
name = "국내 총 생산과 임산물생산_Gross Domestic Product and Forest Products"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("gross", "domestic", "product", "forest", "products")),
                 filter_text_data(text_data, c("국내", "총", "생산", "임산물", "생산")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]





# 용도별 국내재 공급실적
name = "용도별 국내재 공급실적_Domestic Timber Supply by Uses"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("domestic", "timber", "supply", "uses")),
                 filter_text_data(text_data, c("용도별", "국내재", "공급", "실적")),
                 filter_text_data(text_data, c("domestic", "timber", "supply", "by", "use")),
                 filter_text_data(text_data, c("용도별", "내재", "공급")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]







# 수목굴취 허가(신고) 실적
name = "수목굴취 허가(신고) 실적_Permission(Reporting) of Tree Transplanting"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("permission", "reporting", "tree", "transplanting")),
                 filter_text_data(text_data, c("수목굴취", "허가", "신고", "실적")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]







# 목재펠릿 생산 실적
name = "목재펠릿 생산 실적_Wood Pellet Production"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("wood", "pellet", "production")),
                 filter_text_data(text_data, c("목재펠릿", "생산", "실적")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]




# 목질패널 생산 및 공급
name = "목질패널 생산 및 공급_Production and Supply of Processed Wood-based Panel"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("production", "supply", "processed", "wood-based", "panel")),
                 filter_text_data(text_data, c("목질패널", "생산", "공급")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]











# 관상수 생산실적
name = "관상수 생산실적_Ornamental Tree Production"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("ornamental", "tree", "production")),
                 filter_text_data(text_data, c("관상수", "생산", "실적")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]







# 목재수급실적_Demand and Supply of Timber
name = "목재수급실적_Demand and Supply of Timber"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("timber", "demand", "supply"), c("자원별", "facts")),
                 filter_text_data(text_data, c("원목", "수급", "실적"), c("자원별", "facts")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]







# 목재수급실적_Demand and Supply of Timber
name = "자원별 목재수급실적_Demand and Supply of Timber by resources"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("timber", "demand", "supply", "자원별"), c("facts")),
                 filter_text_data(text_data, c("원목", "수급", "실적", "자원별"), c("facts")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]




# 국유림 목재 매각 실적
name = "국유림 목재 매각 실적_National Forest Wood Sales Results"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("national", "forest", "wood", "sales", "results")),
                 filter_text_data(text_data, c("국유림", "목재", "매각", "실적")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]




# 산림벌채 면적 및 벌채량
name = "산림벌채 면적 및 벌채량_Area and Volume of Annual Cut"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("area", "volume", "annual", "cut")),
                 filter_text_data(text_data, c("산림벌채", "면적", "벌채량")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]




# 임산물 수출실적
name = "임산물 수출실적_Export of Forest Products"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("export", "forest", "products")),
                 filter_text_data(text_data, c("임산물", "수출", "실적")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]




# 임산물 수입실적
name = "임산물 수입실적_Import of Forest Products"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("import", "forest", "products")),
                 filter_text_data(text_data, c("임산물", "수입", "실적")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]





# 임산물 생산실적
name = "임산물 생산실적_Production of Forest Products"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("production", "forest", "products"), "관행"),
                 filter_text_data(text_data, c("임산물", "생산", "실적"), "관행"))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]





# 물가 지수
name = "물가 지수_Price index"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("price", "index"), "생산자"),
                 filter_text_data(text_data, c("물가", "지수"), "생산자"),
                 filter_text_data(text_data, c("물  가  지  수"), "생산자"))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]




# 생산자 물가 지수
name = "생산자 물가 지수_Producer Price index"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("price", "index", "생산자")),
                 filter_text_data(text_data, c("물가", "지수", "생산자")),
                 filter_text_data(text_data, c("물  가  지  수",  "생산자")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]







# 🟥 산림서비스 =====================================================================================================
# 산림 복지
name = "산림복지전문업 등록현황_Status of Job Startups on Forest welfare"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("job", "startup", "forest", "welfare")),
                 filter_text_data(text_data, c("산림", "복지", "전문")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]



# 교육
name = "숲사랑 소년단 육성현황_The Number of Green Rangers"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("number", "green", "ranger")),
                 filter_text_data(text_data, c("숲사랑", "소년")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]




name = "산림교육전문가 양성기관 현황_Status of Forest Guide Training Organizations"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("forest", "guide", "training")),
                 filter_text_data(text_data, c("산림", "교육", "전문가")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]




name = "산림교육 수혜인원 현황_The Number of Forest Education Recipients"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("number", "forest", "education", "recipient")),
                 filter_text_data(text_data, c("산림", "교육", "수혜")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]




name = "숲 해설가 수혜인원 현황_Status of Beneficiaries of Forest Guide"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("beneficiaries", "forest", "guide")),
                 filter_text_data(text_data, c("해설가", "수혜", "인원")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]




# 
name = "수목원, 산림박물관, 자생식물원 등록 현황_Establishment of Arboretums, Forest Museum, and Botanical Garden"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("establishment", "arboretum", "forest", "museum", "botanical")),
                 filter_text_data(text_data, c("수목원", "박물관")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]




# 숲 운영
name = "치유의 숲 운영 현황_Management of Healing Forest"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("healing", "management", "forest")),
                 filter_text_data(text_data, c("치유", "숲", "운영")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]




name = "휴양림 운영 및 이용현황_Number of Visitors to Recreation Forests"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("number", "recreation", "forest"), "조성"),
                 filter_text_data(text_data, c("휴양림", "운영"), "조성"))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]




# 숲 조성
name = "명상숲 조성현황_The Number of Meditation Forests"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("number", "Meditation", "forest")),
                 filter_text_data(text_data, c("명상", "숲", "조성")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]




name = "학교 숲 조성현황_Establishment of Forests within Schools"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("establishment", "school", "forest")),
                 filter_text_data(text_data, c("학교", "숲", "조성")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]




name = "도시숲 조성현황_Establishment of Urban Forests"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("establishment", "urban", "forest")),
                 filter_text_data(text_data, c("도시숲", "조성")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]







name = "휴양림 및 산림욕장 조성현황_Establishment of Recreation Forests"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("establishment", "recreation", "forest")),
                 filter_text_data(text_data, c("휴양림", "조성")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]



name = "산림욕장 조성현황_Establishment of Forest Bathing Facilities"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("establishment", "forest", "bathing", "facilities")),
                 filter_text_data(text_data, c("산림", "욕장", "조성")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]







name = "정원 조성 및 운영현황_Create Garden and Manage Garden"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("create garden", "manage garden")),
                 filter_text_data(text_data, c("정원 조성")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]




# 전통 마을
name = "전통마을 숲 조성현황_Establishment of Traditional Village Forests"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("establishment", "traditional", "forest")),
                 filter_text_data(text_data, c("전통", "마을", "숲")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]





# 가로수
name = "가로수 심기현황_Plantation of Roadside Trees"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("roadside", "tree", "plantation")),
                 filter_text_data(text_data, c("가로수", "심기")),
                 filter_text_data(text_data, c("planting", "roadside", "tree")),
                 filter_text_data(text_data, c("planting", "roadisde", "tree")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]



# 
name = "산림복지서비스 제공자 등록 현황_Enrollment of Forest Welfare Service Voucher-available-facilities"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("enrollment", "forest", "welfare", "service")),
                 filter_text_data(text_data, c("산림", "복지", "서비스", "제공")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]



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
                                  c("영림서편", "소유별", "임상별")),
                 filter_text_data(text_data,
                                  c("forest land area", "growing stock", "administrative districts"),
                                  c("forest type")),
                 filter_text_data(text_data,
                                  c("행정구역별", "산림", "면적", "임목", "축적"),
                                  c("임상별")))
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







#### 🟦 연도별 ===================================================================================================
##### 🟪 연도별 평균 임목축적 ===================================================================================================
name = "연도별 ㏊당 평균임목축적_Mean Growing Stock Per ㏊ by Year"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("mean", "growing", "stock", "per", "ha", "year")),
                 filter_text_data(text_data, c("연도별", "㏊당", "평균", "임목축적")),
                 filter_text_data(text_data, c("mean", "growing", "stock", "ha", "year")),
                 filter_text_data(text_data, c("연도별", "ha당", "평균", "임목", "축적")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]



##### 🟪 연도별 임상별 임야면적 ===================================================================================================
name = "연도별 임상별 임야면적_Forest Land Area by Forest Type and Year"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("forest", "land", "area", "forest", "type", "year", "연도별"), "축적"),
                 filter_text_data(text_data, c("연도별", "임상별", "면적"), "축적"),
                 filter_text_data(text_data, c("년도별", "임상별", "면적"), "축적"))
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


# 🟥 산림경영 =====================================================================================================
#### 🟦 산림계 현황과 위탁림 실태  ==================================================================================
name = "산림계 현황과 위탁림 실태_Status of Village Forestry Association and the Consigned Forests"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("status", "village", "forestry", "association", "consigned", "forests")),
                 filter_text_data(text_data, c("산림계", "현황", "위탁림", "실태")),
                 filter_text_data(text_data, c("산림계", "현황")))
print(combined.list[[name]])

text_data = text_data[! text_data %in% unlist(combined.list)]







#### 🟦 임가 현황 ===================================================================================================
##### 🟪 개인 =====================================================================
name = "개인 임가 현황_Private Forest Households"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("private", "forest", "household")),
                 filter_text_data(text_data, c("개인", "임가")))
print(combined.list[[name]])
text_data = text_data[! text_data %in% unlist(combined.list)]



##### 🟪 전 겸업별 =====================================================================
name = "전ㆍ겸업별 임가 현황_Forest Households by Fall and Part Time"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("forest", "household", "part", "time")),
                 filter_text_data(text_data, c("겸업별", "임가", "현황")))
print(combined.list[[name]])
text_data = text_data[! text_data %in% unlist(combined.list)]




##### 🟪 경영형태별 =====================================================================
name = "경영형태별 임가 현황_Forest Households by management type"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("forest", "household", "management")),
                 filter_text_data(text_data, c("경영", "형태", "임가")))
print(combined.list[[name]])

text_data = text_data[! text_data %in% unlist(combined.list)]


##### 🟪 생산형태별 =====================================================================
name = "생산형태별 임가 현황_Forest Households by Production Type"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("forest", "households", "production", "type")),
                 filter_text_data(text_data, c("생산", "형태", "임가")))
print(combined.list[[name]])

text_data = text_data[! text_data %in% unlist(combined.list)]


#### 🟦 사유림 ===================================================================================================
##### 🟪 지역별 산주 현황 =====================================================================
name = "지역별 사유림 산주현황_Ownership of Private Forests by Regions"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("private", "forest", "region", "ownership")),
                 filter_text_data(text_data, c("지역", "사유림", "산주")))
print(combined.list[[name]])

text_data = text_data[! text_data %in% unlist(combined.list)]


##### 🟪 산주 거주지 =====================================================================
name = "임야소재지별 사유림 개인산주 거주지 현황_Location of Residence of Private Forest Land Owner by Location of Forest Land"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("location", "residence", "private", "forest", "land", "owner")),
                 filter_text_data(text_data, c("임야", "소재", "사유림", "개인", "산주", "거주지")))
print(combined.list[[name]])

text_data = text_data[! text_data %in% unlist(combined.list)]




# 🟥 텍스트 필터링 ==========================================================================================================================
##### 🟪 소재 부재 산주 현황  =====================================================================
name = "사유림 소재ㆍ부재 산주현황_Status of Resident and Non-resident Forest Owners by Province"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("resident", "forest", "owner", "province")),
                 filter_text_data(text_data, c("사유림", "소재", "산주"), c("거주지")))
print(combined.list[[name]])

text_data = text_data[! text_data %in% unlist(combined.list)]


##### 🟪 협업 경영 사업 =====================================================================
name = "사유림 협업경영사업_Activities of Private Forest Cooperatives"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("private", "forest", "cooperation", "activities")),
                 filter_text_data(text_data, c("사유림", "협업", "경영", "사업")))
print(combined.list[[name]])
text_data = text_data[! text_data %in% unlist(combined.list)]



#### 🟦 임업 기계 장비 보유 ===================================================================================================
name = "임업기계 · 장비  보유현황_Forest Machinery and Equipment"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("forest", "machinery", "equipment")),
                 filter_text_data(text_data, c("임업", "기계", "장비")))
print(combined.list[[name]])

text_data = text_data[! text_data %in% unlist(combined.list)]



#### 🟦 임가 및 임가 인구 ===================================================================================================
name = "임가 및 임가 인구_Number of Households and Population engaged in the Forestry Sector"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("number", "household", "engaged", "forestry")),
                 filter_text_data(text_data, c("임가", "인구")))
print(combined.list[[name]])

text_data = text_data[! text_data %in% unlist(combined.list)]




#### 🟦 산림사업 고용 현황 ===================================================================================================
name = "산림사업 고용현황_Status of Employment in Forestry"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("employment", "forestry")),
                 filter_text_data(text_data, c("산림", "고용")))
print(combined.list[[name]])

text_data = text_data[! text_data %in% unlist(combined.list)]




#### 🟦 임업 노동력 ===================================================================================================
name = "임업노동력현황_Number Of Workers Engaged In Forestry"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("number", "worker", "engaged", "forestry")),
                 filter_text_data(text_data, c("임업", "노동력", "사유림", "개인", "산주", "거주지")))
print(combined.list[[name]])

text_data = text_data[! text_data %in% unlist(combined.list)]




#### 🟦 경영계획 ===================================================================================================
##### 🟪 국유림 및 민유림 경영계획 ======================================================================================================
name = "국유림 및 민유림 경영계획 편성실적_Accomplishment of Working Plan Preparation"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("accomplishment", "working", "plan", "preparation")),
                 filter_text_data(text_data, c("국유림", "민유림", "경영계획", "편성", "실적")))
print(combined.list[[name]])

text_data = text_data[! text_data %in% unlist(combined.list)]



#### 🟦 산촌생태마을 조성 ===================================================================================================
name = "산촌생태마을 조성현황_Status of Mountain Village Development"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("mountain", "village", "development")),
                 filter_text_data(text_data, c("산촌", "생태마을", "조성")))
print(combined.list[[name]])

text_data = text_data[! text_data %in% unlist(combined.list)]




#### 🟦 독림가 및 임업 후계자 ===================================================================================================
name = "독림가 및 임업후계자 현황_Outstanding Forest Managers and Forest Successors"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("outstanding", "forest", "managers", "successors")),
                 filter_text_data(text_data, c("독림가", "임업", "후계자")))
print(combined.list[[name]])

text_data = text_data[! text_data %in% unlist(combined.list)]



#### 🟦 화전정리 ===================================================================================================
name = "화전정리실적_Arrangement of Shifting Cultivations"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("arrangement", "shifting", "cultivations")),
                 filter_text_data(text_data, c("화전", "정리", "실적")))
print(combined.list[[name]])

text_data = text_data[! text_data %in% unlist(combined.list)]



#### 🟦 경계 측량 및 표주 설치  ==================================================================================
name = "국유임야 경계측량 및 표주설치 실적_Boundary Survey and Landmark in National Forest"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("boundary", "survey", "landmark", "national", "forest")),
                 filter_text_data(text_data, c("국유임", "측량", "표주", "실적")),
                 filter_text_data(text_data, c("국유림", "측량", "표주", "실적")),
                 filter_text_data(text_data, c("boundary", "survey", "landmark", "national", "forest")),
                 filter_text_data(text_data, c("국유임야", "경제", "측량", "표주", "설치", "실적")))
print(combined.list[[name]])

text_data = text_data[! text_data %in% unlist(combined.list)]




#### 🟦 민유림 관리 ===================================================================================================
name = "민유림 관리_Non-national forest management"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("non", "national", "forest"), c("수종별", "자력", "보조", "벌채", "조림")),
                 filter_text_data(text_data, c("민유림", "관리"), c("수종별", "자력", "보조", "벌채", "조림")))
print(combined.list[[name]])
text_data = text_data[! text_data %in% unlist(combined.list)]




#### 🟦 국유림 관리 ===================================================================================================
name = "국유림 관리_National forest management"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("national", "forest"), c("수종별", "자력", "보조", "벌채", "조림", "민유림")),
                 filter_text_data(text_data, c("국유림", "관리"), c("수종별", "자력", "보조", "벌채", "조림", "민유림")))
print(combined.list[[name]])
text_data = text_data[! text_data %in% unlist(combined.list)]




# 🟥 임업생산 =====================================================================================================
#### 🟦 국내생산과 임업생산 ===================================================================================================
##### 🟪 경제활동별 국내총생산 ===================================================================================================
name = "경제활동별 국내총생산_Gross Domestic Product By Kind Of Economic Activity"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("gross", "domestic", "product", "kind", "economic", "activity")),
                 filter_text_data(text_data, c("경제활동별", "국내총생산")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]





#### 🟦 임목벌채 ===================================================================================================
##### 🟪 임목 벌채 공급 계획 및 실적 ===================================================================================================
name = "임목벌채 공급계획 및 실적_Cutting Plan and Supply of Timber"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("cutting", "plan", "supply", "timber")),
                 filter_text_data(text_data, c("임목", "벌채", "공급", "계획", "실적")),
                 filter_text_data(text_data, c("cutting", "plan", "production", "timber")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]



##### 🟪 임목 벌채 허가 실적 ===================================================================================================
name = "임목벌채 허가실적_Permit of Annual Tree Cutting"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("permit", "annual", "tree", "cutting")),
                 filter_text_data(text_data, c("임목", "벌채", "허가", "실적")),
                 filter_text_data(text_data, c("permission", "annual", "timber", "cutting")),
                 filter_text_data(text_data, c("입목벌채", "허가", "실적")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]






##### 🟪 임목 벌채 실적 ===================================================================================================
name = "임목 벌채실적_Timber Production"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("timber", "production", "벌채"), c("permit", "생산량표", "임산물", "계획", "배정량")),
                 filter_text_data(text_data, c("임목", "벌채", "실적"), c("허가", "생산량표", "임산물", "계획", "배정량")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]




##### 🟪 임목 벌채 배정량 및 생산 실적 ===================================================================================================
name = "임목벌채 배정량 및 생산실적_Allocation of Cutting Amount of Wood and Production"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("allocation", "cutting", "amount", "wood", "production")),
                 filter_text_data(text_data, c("임목", "벌채", "배정량", "생산", "실적")),
                 filter_text_data(text_data, c("임목", "별채", "배정량", "생산", "실적")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]




##### 🟪 간벌 및 각종 지장목 벌채 실적_Thinning and cutting of interfering trees ===================================================================================================
name = "간벌 및 각종 지장목 벌채 실적_Thinning and cutting of interfering trees"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("thinning", "cutting", "interfering", "trees"), c("국유림", "민유", "무육")),
                 filter_text_data(text_data, c("지장목", "벌채", "실적"), c("국유림", "민유", "무육")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]







#### 🟦 목재 ===================================================================================================
##### 🟪 목재 수급계획 및 공급 실적 ===================================================================================================
name = "목재 수급 계획 및 공급 실적_Facts of Demand and Supply for Timber"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("facts", "demand", "supply", "for", "timber")),
                 filter_text_data(text_data, c("목재", "수급", "계획", "공급", "실적")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]




##### 🟪 목재생산 및 공급 실적 ===================================================================================================
name = "목재생산 및 공급실적_Timber Production and Supply"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("timber", "production", "supply")),
                 filter_text_data(text_data, c("목재", "생산", "공급", "실적")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]





##### 🟪 자원별 목재 수급 현황 ===================================================================================================
name = "자원별 목재 수급현황_Wood Demand and Supply by Resources"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("wood", "demand", "supply", "resources")),
                 filter_text_data(text_data, c("자원별", "목재", "수급")),
                 filter_text_data(text_data, c("timber", "demand", "supply", "resources")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]




#### 🟦 공급실적 ===================================================================================================
##### 🟪 용도별 원목 공급 실적 ===================================================================================================
name = "용도별 원목 공급 실적_Timber Supply by Use"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("timber", "supply", "use"), "domestic"),
                 filter_text_data(text_data, c("용도별", "원목", "공급", "실적"), "국내재"))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]




# 🟥 텍스트 필터링 해서 그룹화 =====================================================================================
#### 🟦 생산실적 ===================================================================================================
##### 🟪 원목 생산 실적 ===================================================================================================
name = "원목 생산실적_Pound Wood Production"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("pound", "wood", "production")),
                 filter_text_data(text_data, c("원목", "생산실적")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]








#### 🟦 임산물 ===================================================================================================
##### 🟪 임산물 생산액의 구성 ===================================================================================================
name = "임산물 생산액의 구성_Forest Products Value"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("forest", "products", "value"), c("export", "import")),
                 filter_text_data(text_data, c("임산물", "생산액", "구성")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]












# 🟥 조림과 사방 =======================================================================
#### 🟦 조림 면적 ===================================================================================================
##### 🟪 용도별 조림면적구성 ===================================================================================================
name = "용도별 조림면적구성_Constitution of Reforestation Area by Use"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("constitution", "reforestation", "area", "use")),
                 filter_text_data(text_data, c("용도별", "조림", "면적", "구성")),
                 filter_text_data(text_data, c("reforestation", "objective")),
                 filter_text_data(text_data, c("용도별", "조림", "면적", "구성")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]




#### 🟦 조림 실적 ===================================================================================================
##### 🟪 민유림 조림 실적  ===================================================================================================
###### 🟨 민유림 조림 ==========================================================================================================
name = "민유림 조림 실적_Accomplishment of reforestation in non-national forest"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, 
                                  c("reforestation", "non-national", "forest"), 
                                  c("individual", "ownership", "sidy", "species", "owner", "seed")),
                 filter_text_data(text_data, 
                                  c("민유림", "조림", "실적"), 
                                  c("자력", "소유", "보조", "수종", "종자")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]




###### 🟨 민유림 수종별 조림 ==========================================================================================================
name = "수종별 민유림 조림 실적_Accomplishment of reforestation in non-national forest by species"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, 
                                  c("reforestation", "non-national", "forest", "species"), 
                                  c("individual", "ownership", "sidy", "owner", "seed")),
                 filter_text_data(text_data, 
                                  c("민유림", "조림", "실적", "수종"), 
                                  c("자력", "소유", "보조", "종자")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]



###### 🟨 자력 ==========================================================================================================
name = "민유림자력조림실적_Accomplishment of individual reforestation in non-national forest"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("reforestation", "non-national", "forest", "individual"), "species"),
                 filter_text_data(text_data, c("민유림", "조림", "실적", "자력"), "수종별"))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]





###### 🟨 수종별 자력 ==========================================================================================================
name = "수종별 민유림자력조림실적_Accomplishment of individual reforestation in non-national forest by species"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("reforestation", "non-national", "forest", "species", "individual")),
                 filter_text_data(text_data, c("민유림", "조림", "실적", "수종별", "자력")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]






###### 🟨 수종별 보조 ==========================================================================================================
name = "수종별 민유림 보조조림실적_Accomplishment of reforestation under government subsidy in non-national forest by species"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, 
                                  c("reforestation", "non-national", "forest", "government","sidy", "species")),
                 filter_text_data(text_data, 
                                  c("민유림", "조림", "실적", "보조", "수종")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]





###### 🟨 보조조림 ==========================================================================================================
name = "민유림 보조조림실적_Reforestation in Non-national Forest by Government Subsidy"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, 
                                  c("reforestation", "non-national", "forest", "sidy"),
                                  c("species")),
                 filter_text_data(text_data, 
                                  c("민유림", "조림", "실적", "보조"),
                                  c("수종별")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]






##### 🟪 국유림조림실적 ===================================================================================================
name = "국유림 조림실적_Reforestation in National Forest"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("reforestation", "national", "forest"), "민유림"),
                 filter_text_data(text_data, c("국유림", "조림", "실적"), "민유림"))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]






##### 🟪 사업별 조림 실적 ===================================================================================================
name = "사업별 조림실적_Accomplishment of Reforestation by Project"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("accomplishment", "reforestation", "project")),
                 filter_text_data(text_data, c("사업별", "조림", "실적")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]





##### 🟪 조림과 보호 투자 실적 ===================================================================================================
name = "조림, 사방과 보호 투자실적_Investment Accomplishment of Reforestation, Erosion Control and Protection"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("investment", "accomplishment", "reforestation", "protection")),
                 filter_text_data(text_data, c("조림", "보호", "투자", "실적")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]




##### 🟪 조림용 종자 채취 실적 ===================================================================================================
name = "조림용 종자채취실적_Seed Collection for Plantation"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, 
                                  c("seed", "collection", "plantation"),
                                  c("private")),
                 filter_text_data(text_data, 
                                  c("조림용", "종자", "채취", "실적"), 
                                  "민유림"))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]



name = "민유림조림용 종자채취실적_Seed Collection for Plantation in private forest"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, 
                                  c("private", "seed", "collection", "plantation")),
                 filter_text_data(text_data, 
                                  c("조림용", "종자", "채취", "실적", "민유림")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]



##### 🟪 식재조림실적 ===================================================================================================
name = "식재조림실적_Accomplishment of Plantation"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, 
                                  c("accomplishment", "plantation"),
                                  "project"),
                 filter_text_data(text_data, 
                                  c("식재", "조림", "실적"),
                                  "사업별"))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]




#### 🟦 생산 ===================================================================================================
##### 🟪 묘목 =========================================================================================================
name = "묘목생산실적_Tree Seedling Production"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, 
                                  c("tree", "seedling", "production"),
                                  c("양묘", "수종별", "operation")),
                 filter_text_data(text_data, 
                                  c("묘목", "생산", "실적"), 
                                  c("양묘", "수종별")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]




##### 🟪 묘목 =========================================================================================================
name = "수종별 묘목 생산현황_Seedling Production by Tree Species"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, 
                                  c("tree", "수종별", "seedling", "production"),
                                  c("양묘", "operation")),
                 filter_text_data(text_data, 
                                  c("묘목", "생산", "수종별", "실적"), 
                                  c("양묘")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]






# 🟥 데이터 로드 =======================================================================
#### 🟦 자연 보호 ===================================================================================================
##### 🟪 동식물 보호 구역  ===================================================================================================
name = "야생동ㆍ식물보호구역 지정현황_Status of Designated Protected Zones for Wildlife Animals and Plants"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("designated", "protected", "zones", "wildlife", "animals", "plants")),
                 filter_text_data(text_data, c("designated", "protected", "wildlife", "zone")),
                 filter_text_data(text_data, c("야생", "식물", "보호구역", "지정", "현황")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]




##### 🟪 산림 유전 자원 보호림 및 보호수 지정 현황  ===================================================================================================
name = "산림 유전 자원 보호림 및 보호수 지정 현황_tatus of protected area for genetic forest resources and designated protected trees"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("forest", "protected", "area", "status")),
                 filter_text_data(text_data, c("status", "산림", "보호구역", "지정", "현황")),
                 filter_text_data(text_data, c("status", "natural", "forest", "reserve", "nurse-tree"), c("유전")),
                 filter_text_data(text_data, c("천연", "보호림","보호수", "현황", "유전")),
                 filter_text_data(text_data, c("보호수", "현황", "유전"))
                 )
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]





##### 🟪 보안림 면적  ===================================================================================================
name = "보안림 면적_Reserved Forest Area"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, 
                                  c("reserved", "forest", "area"), 
                                  c("genetic", "generic")),
                 filter_text_data(text_data, 
                                  c("보안림", "면적"), 
                                  "유전"),
                 filter_text_data(text_data, 
                                  c("protection", "forest", "area"),
                                  "유전"))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]




##### 🟪 천연보호림 및 보호수 현황 ===================================================================================================
name = "천연보호림 및 보호수 현황_Status of Natural Forest Reserve and Nurse-Tree"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("status", "natural", "forest", "reserve", "nurse-tree"), c("유전")),
                 filter_text_data(text_data, c("천연", "보호림","보호수", "현황"), c("유전")),
                 filter_text_data(text_data, c("보호수", "현황"), c("유전")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]










##### 🟪 조수 보호구 현황 ===================================================================================================
name = "조수 보호구 현황_The Status of Sanctuary"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("status", "sanctuary")),
                 filter_text_data(text_data, c("조수", "보호구")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]



##### 🟪 천연기념물로 지정된 조수 ===================================================================================================
name = "천연기념물로 지정된 조수_Wildlife Designated as Natural Monument"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("wildlife", "designated", "natural", "monument")),
                 filter_text_data(text_data, c("천연", "기념물", "지정", "조수")),
                 filter_text_data(text_data, c("natural", "monuments", "designated", "birds", "mammals")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]




##### 🟪 천연기념물 번식지 및 도래지 ===================================================================================================
name = "천연기념물의 번식지 및 도래지_Breeding and Wintering Grounds Designated as Natural Monuments"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("breeding", "wintering", "grounds", "designated", "natural", "monuments")),
                 filter_text_data(text_data, c("천연기념물", "번식지", "도래지")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]



##### 🟪 야생조수 인공사육   ===================================================================================================
name = "야생조수 인공사육 현황_Status of Wildlife Rearing"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("status", "wildlife", "rearing")),
                 filter_text_data(text_data, c("야생조수", "인공사육")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]



##### 🟪 맹수류 사육   ===================================================================================================
name = "맹수류 사육 현황_Status of Fierce Animal Rearing"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("status", "fierce", "animal", "rearing")),
                 filter_text_data(text_data, c("맹수류", "사육", "현황")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]



##### 🟪 산림 복원  ===================================================================================================
name = "산림복원 현황_Status of Forest Restoration"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("forest", "restoration")),
                 filter_text_data(text_data, c("산림", "복원")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]



##### 🟪 산림의 타용도 전용 허가  ===================================================================================================
name = "산림의 타용도 전용허가 현황_Status of Forest Land Conversion"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("forest", "land", "conversion"), "temporary"),
                 filter_text_data(text_data, c("산림", "전용", "허가")),
                 filter_text_data(text_data, c("forest", "land", "conversion"), "일시"),
                 filter_text_data(text_data, c("산림", "타용도", "전용", "현황"), "일시")
                 )
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]





##### 🟪 산림 형질 변경 허가   ===================================================================================================
name = "산림 형질변경 허가 상황_Permission for Conversion of Forest to Other Uses"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("permission", "conversion", "forest", "other", "uses")),
                 filter_text_data(text_data, c("산림", "형질", "변경", "허가")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]





#### 🟦 휴양림 ===================================================================================================
##### 🟪 이용 현황   ===================================================================================================
name = "휴양림 이용현황_Visitors to Recreational Forests"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("visitors", "recreational", "forests")),
                 filter_text_data(text_data, c("휴양림", "이용", "현황")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]







#### 🟦 피해 ===================================================================================================
##### 🟪 산림 피해지 벌채  ===================================================================================================
name = "산림피해지 벌채현황_Area and Volume of Damages"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("area", "volume", "damages")),
                 filter_text_data(text_data, c("산림", "피해지", "벌채")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]






##### 🟪 불법 산림 훼손 피해  ===================================================================================================
name = "불법 산림훼손 피해현황_Damages from Illegal Forest Activities"
combined.list[[name]] = union_multiple(filter_text_data(text_data, c("damages", "illegal", "forest", "activities")),
                                       filter_text_data(text_data, c("불법", "산림", "훼손", "피해")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]






# 🟥 데이터 로드 =======================================================================================
##### 🟪 동물 기상 산림 피해 ===================================================================================================
###### 🟨 면적 구성 ===================================================================================================
name = "동물 및 기상적 산림피해발생 면적의 구성_Proportion of Forest Damage by Animal and Meteorological Cause"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("proportion", "forest", "damage", "animal", "meteorological", "cause")),
                 filter_text_data(text_data, c("동물", "기상적", "산림", "피해", "발생", "면적", "구성")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]



###### 🟨 방제 상황 ===================================================================================================
name = "동물 및 기상적 산림피해발생 및 방제상황_Prevention of Forest Damage by Animal and Meteorological Cause"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("prevention", "forest", "damage", "animal", "meteorological", "cause")),
                 filter_text_data(text_data, c("동물", "기상적", "산림", "피해", "발생", "방제", "상황")),
                 filter_text_data(text_data, c("동물", "기상적", "산림", "피해", "발생", "상황")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]




##### 🟪 산화/산불 피해 상황 ===================================================================================================
name = "산화/산불피해 상황_Status of Forest Fire Damage"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("status", "forest", "fire", "damage")),
                 filter_text_data(text_data, c("산화", "피해")),
                 filter_text_data(text_data, c("산불", "피해")),
                 filter_text_data(text_data, c("damage", "forest", "fires")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]



##### 🟪 산림 훼손 허가 상황 ===================================================================================================
name = "산림 훼손 허가 상황_Permission Status of Forest Exploitation"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("permission", "status", "forest", "exploitation")),
                 filter_text_data(text_data, c("산림", "훼손", "허가", "상황")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]




##### 🟪 인위적 산림피해상황 ===================================================================================================
name = "인위적 산림피해상황_Status of Artificial Forest Damage"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("status", "artificial", "forest", "damage")),
                 filter_text_data(text_data, c("인위적", "산림", "피해")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]




##### 🟪 병해충 ===================================================================================================
###### 🟨 자재 소비 상황 ===================================================================================================
name = "산림병해충방제 자재 소비상황_Consumption of Pesticide for Prevention and Control of Forest Disease and Pest"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("consumption", "pesticide", "prevention", "control", "forest", "disease", "pest")),
                 filter_text_data(text_data, c("산림", "해충", "자재", "소비")),
                 filter_text_data(text_data, c("방제", "자재", "소비")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]


###### 🟨 발생 및 방제 상황 ===================================================================================================
name = "산림병해충 발생 및 방제상황_5. Forest Damage Occurrence and Prevention by Forest Pest Insect and Disease"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, 
                                  c("disease", "prevention", "forest", "disease", "pest"), 
                                  c("소비", "자재")),
                 filter_text_data(text_data, 
                                  c("산림", "해충", "방제", "상황"), 
                                  c("소비", "자재")),
                 filter_text_data(text_data, 
                                  c("forest", "damage", "occurrence", "prevention", "insect", "disease"),  
                                  c("소비", "자재")),
                 filter_text_data(text_data, c("산림", "병해충", "발생", "방제"),  
                                  c("소비", "자재")),
                 filter_text_data(text_data, c("산림", "해충", "발생", "방제"),  
                                  c("소비", "자재")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]



#### 🟦 수렵 ===================================================================================================
name = "사냥터운영상황_Operation of Hunting Ground"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("operation", "hunting", "ground")),
                 filter_text_data(text_data, c("사냥터", "운영")),
                 filter_text_data(text_data, c("수렵장", "운영")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]




#### 🟦 독림가 ===================================================================================================
name = "독림가 현황_Details of Sincere Forest Manager"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("details", "sincere", "forest", "manager"), "내역"),
                 filter_text_data(text_data, c("독림가", "현황"), "내역"),
                 filter_text_data(text_data, c("독림가", "내역"), "현황"))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]



#### 🟦 사방사업실적 ===================================================================================================
name = "사방사업실적_Accomplishment of Erosion Control"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("accomplishment", "erosion", "control")),
                 filter_text_data(text_data, c("사방", "사업", "실적")),
                 filter_text_data(text_data, c("erosion", "control", "projects")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]









#### 🟦 수묘표 ===================================================================================================
name = "수묘표_Nursery Practice"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("nursery", "practice")),
                 filter_text_data(text_data, c("수묘", "표")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]



#### 🟦 영림단 조직 ===================================================================================================
name = "영림단 조직 현황_Units of Forest Craft Workers"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("units", "forest", "craft", "workers")),
                 filter_text_data(text_data, c("영림단", "조직")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]



#### 🟦 산림 보호 구역 지정 현황 ===================================================================================================
name = "산림보호구역 지정 현황_Forest Protected Areas"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("forest", "protected", "areas")),
                 filter_text_data(text_data, c("산림", "보호", "지정")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]





# 🟥 작업1 ======================================================================================================================







## 🟦 수종별 종자 생산 현황_Seed Production by Tree Species===========================================================================================
name ="수종별 종자 생산현황_Seed Production by Tree Species"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("Seed", "Production", "by", "Tree", "Species")),
                 filter_text_data(text_data, c("수종별", "종자", "생산")))
print(combined.list[[name]])
text_data = text_data[!text_data %in% unlist(combined.list)]



## 🟦 산림면적 및 산림비율_Forest Area and % of Forest Area by Ownership===========================================================================================
name = "산림면적 및 산림비율_Forest Area and % of Forest Area by Ownership"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("Forest", "Area", "and", "%", "by", "Ownership")),
                 filter_text_data(text_data, c("소유별", "산림면적", "산림비율")))
print(combined.list[[name]])
text_data = text_data[!text_data %in% unlist(combined.list)]




## 🟦 재배작물별 재배면적_Cultivated Area by Growing Crops===========================================================================================
name = "재배작물별 재배면적_Cultivated Area by Growing Crops"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("Cultivated", "Area", "by", "Growing", "Crops")),
                 filter_text_data(text_data, c("재배작물별", "재배면적")))
print(combined.list[[name]])
text_data = text_data[!text_data %in% unlist(combined.list)]




# 🟥 작업2 ======================================================================================================================
## 🟦 임가경제 주요지표_Main Indicators of Forest Household Economy===========================================================================================
name = "임가경제 주요지표_Main Indicators of Forest Household Economy"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("Main", "Indicators", "of", "Forest", "Household", "Economy")),
                 filter_text_data(text_data, c("임가경제", "주요지표")))
print(combined.list[[name]])
text_data = text_data[!text_data %in% unlist(combined.list)]








## 🟦 종자 및 묘목 생산현황_Status of Seed and Seedling Production===========================================================================================
name = "종자 및 묘목 생산현황_Status of Seed and Seedling Production"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("Status", "of", "Seed", "and", "Seedling", "Production")),
                 filter_text_data(text_data, c("종자", "및", "묘목", "생산현황")))
print(combined.list[[name]])
text_data = text_data[!text_data %in% unlist(combined.list)]




## 🟦 산림사업 융자실적_Loans for Forest Activities===========================================================================================
name = "산림사업 융자실적_Loans for Forest Activities"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("Loans", "for", "Forest", "Activities")),
                 filter_text_data(text_data, c("산림사업", "융자실적")))
print(combined.list[[name]])
text_data = text_data[!text_data %in% unlist(combined.list)]





## 🟦 귀산촌인 연령별 현황_People Returning to Mountain Villages by Age===========================================================================================
name = "귀산촌인 연령별 현황_People Returning to Mountain Villages by Age"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("People", "Returning", "to", "Mountain", "Villages", "by", "Age")),
                 filter_text_data(text_data, c("귀산촌인", "연령별", "현황")))
print(combined.list[[name]])
text_data = text_data[!text_data %in% unlist(combined.list)]




## 🟦 산림사업 융자규모_Loans for Forestry Business===========================================================================================
name = "산림사업 융자규모_Loans for Forestry Business"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("Loans", "for", "Forestry", "Business")),
                 filter_text_data(text_data, c("산림사업", "융자규모")))
print(combined.list[[name]])
text_data = text_data[!text_data %in% unlist(combined.list)]




## 🟦 행정구역별 면적 및 행정구역수_Area and number of administrative districts===========================================================================================
name = "행정구역별 면적 및 행정구역수_Area and number of administrative districts"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("Area", "and", "number", "of", "administrative", "districts")),
                 filter_text_data(text_data, c("행정구역", "구역", "면적", "수")))
print(combined.list[[name]])
text_data = text_data[!text_data %in% unlist(combined.list)]





## 🟦 사유림 산주 현황_Ownership of Private Forests by Provinces===========================================================================================
name = "사유림 산주 현황_Ownership of Private Forests by Provinces"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("Ownership", "of", "Private", "Forests", "by", "Provinces")),
                 filter_text_data(text_data, c("사유림", "산주", "현황")))
print(combined.list[[name]])
text_data = text_data[!text_data %in% unlist(combined.list)]








# 🟥 작업3 ======================================================================================================================
## 🟦 산촌지역 연령별 인구 현황_Population Status by Age in Mountain Village===========================================================================================
name = "산촌지역 연령별 인구 현황_Population Status by Age in Mountain Village"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("Population", "Status", "by", "Age", "Mountain", "Village")),
                 filter_text_data(text_data, c("산촌지역", "연령별", "인구", "현황")))
print(combined.list[[name]])
text_data = text_data[!text_data %in% unlist(combined.list)]





## 🟦 산림관계 세입 세출 예산_Forest Budget for Revenue and Expenditure===========================================================================================
name = "산림관계 세입 세출 예산_Forest Budget for Revenue and Expenditure"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("Forest", "Budget", "for", "Revenue", "and", "Expenditure")),
                 filter_text_data(text_data, c("산림관계", "세입", "세출", "예산")))
print(combined.list[[name]])
text_data = text_data[!text_data %in% unlist(combined.list)]




## 🟦 임상별 임야면적 구성_Proportion of forest land area by forest type===========================================================================================
name = "임상별 임야면적 구성_Proportion of forest land area by forest type"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("Proportion", "of", "forest", "land", "area", "by", "forest", "type")),
                 filter_text_data(text_data, c("임상별", "임야면적", "구성")))
print(combined.list[[name]])
text_data = text_data[!text_data %in% unlist(combined.list)]



## 🟦 구ㆍ시ㆍ군별 면적 및 행정단위_Area and administrative unit by Gu, Si and Gun===========================================================================================
name = "구ㆍ시ㆍ군별 면적 및 행정단위_Area and administrative unit by Gu, Si and Gun"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("Area", "and", "administrative", "unit", "by", "Gu", "Si", "Gun")),
                 filter_text_data(text_data, c("구ㆍ시ㆍ군별", "면적", "및", "행정단위")))
print(combined.list[[name]])
text_data = text_data[!text_data %in% unlist(combined.list)]






# 🟥 작업4 ======================================================================================================================
## 🟦 임상별 임목축적_Forest growing stock by forest type===========================================================================================
name = "임상별 임목축적_Forest growing stock by forest type"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("Forest", "growing", "stock", "by", "forest", "type")),
                 filter_text_data(text_data, c("임상별", "임목축적")))
print(combined.list[[name]])
text_data = text_data[!text_data %in% unlist(combined.list)]




## 🟦 국유림 간벌 및 각종지장목 벌채실적_Thinning and cutting of interfering trees in national forest===========================================================================================
name = "국유림 간벌 및 각종지장목 벌채실적_Thinning and cutting of interfering trees in national forest"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("Thinning", "cutting", "interfering", "trees", "national", "forest"), c("민유")),
                 filter_text_data(text_data, c("국유림", "지장목", "벌채"), c("민유")))
print(combined.list[[name]])
text_data = text_data[!text_data %in% unlist(combined.list)]




# 🟥 작업4 ======================================================================================================================
## 🟦 산림관계세출예산_Forest Estimated Expenditures===========================================================================================
name = "산림관계세출예산_Forest Estimated Expenditures"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("Forest", "Estimated", "Expenditures")),
                 filter_text_data(text_data, c("산림관계", "세출예산")))
print(combined.list[[name]])
text_data = text_data[!text_data %in% unlist(combined.list)]




## 🟦 산림관계 세입예산_Forest Estimated Revenue===========================================================================================
name = "산림관계 세입예산_Forest Estimated Revenue"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("Forest", "Estimated", "Revenue")),
                 filter_text_data(text_data, c("산림관계", "세입예산")))
print(combined.list[[name]])
text_data = text_data[!text_data %in% unlist(combined.list)]




## 🟦 민유림야 성림무육 및 각종지장목 벌채 허가실적_Amount of thinning and cutting of interfering trees authorized in non-national forest===========================================================================================
name = "민유림야 성림무육 및 각종지장목 벌채 허가실적_Amount of thinning and cutting of interfering trees authorized in non-national forest"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("Amount", "thinning", "cutting", "interfering", "trees", "authorized", "non-national", "forest")),
                 filter_text_data(text_data, c("민유", "지장목", "벌채")))
print(combined.list[[name]])
text_data = text_data[!text_data %in% unlist(combined.list)]



## 🟦 성림 무육 및 각종 지장목 벌채 실적_Ⅳ-4 Tending operation and cutting of interfering trees===========================================================================================
name = "성림 무육 및 각종 지장목 벌채 실적_Tending operation and cutting of interfering trees"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("tending", "operation", "cutting", "interfering", "tree")),
                 filter_text_data(text_data, c("성림", "지장목", "벌채")))
print(combined.list[[name]])
text_data = text_data[!text_data %in% unlist(combined.list)]



## 🟦 영림단 운영현황_Forest Management Units ==========================================================================================
name = "영림단 운영현황_Forest Management Units"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("forest", "management", "unit")),
                 filter_text_data(text_data, c("영림단", "운영")))
print(combined.list[[name]])
text_data = text_data[!text_data %in% unlist(combined.list)]




## 🟦 "시ㆍ도별ㆍ지종별ㆍ영급별 산림면적 및 임목축적_Forest Land Area and Growing Stock by Provinces, Land Classes and Age Classes" ==========================================================================================
name = "시ㆍ도별ㆍ지종별ㆍ영급별 산림면적 및 임목축적_Forest Land Area and Growing Stock by Provinces, Land Classes and Age Classes"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("forest", "land", "area", "growing", "stock", "province", "land", "class", "age")),
                 filter_text_data(text_data, c("도별", "지종별", "영급별", "면적", "축적")))
print(combined.list[[name]])
text_data = text_data[!text_data %in% unlist(combined.list)]












