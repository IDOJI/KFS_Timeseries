
# 🟥 산림경영 =====================================================================================================
#### 🟦 산림계 현황과 위탁림 실태  ==================================================================================
name = "산림계 현황과 위탁림 실태_Status of Village Forestry Association and the Consigned Forests"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("status", "village", "forestry", "association", "consigned", "forests")),
                 filter_text_data(text_data, c("산림계", "현황", "위탁림", "실태")),
                 filter_text_data(text_data, c("산림계", "현황")))
print(combined.list[[name]])
text_data = L2
text_data = text_data[! text_data %in% unlist(combined.list)]

#### 🟦 귀산촌인 연령별 현황 ===================================================================================================
name = "귀산촌인 연령별 현황_People Returning to Mountain Villages by Age"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("people", "return", "mountain", "age")),
                 filter_text_data(text_data, c("산촌", "연령")))
print(combined.list[[name]])
text_data = L2
text_data = text_data[! text_data %in% unlist(combined.list)]


#### 🟦 산림관계 세입 세출 예산 ===================================================================================================
name = "산림관계 세입·세출 예산_Tax Revenue and Expenditure of Korea Forest Service"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("tax", "revenue", "expenditure", "forest")),
                 filter_text_data(text_data, c("산림", "세출")),
                 filter_text_data(text_data, c("산림", "세입")))
print(combined.list[[name]])
text_data = L2
text_data = text_data[! text_data %in% unlist(combined.list)]


#### 🟦 임가경제 주요지표 ===================================================================================================
name = "임가경제 주요지표_Main indicators of forest household economy"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("main", "indicator", "household", "economy", "forest")),
                 filter_text_data(text_data, c("임가", "경제", "주요", "지표")))
print(combined.list[[name]])
text_data = L2
text_data = text_data[! text_data %in% unlist(combined.list)]


#### 🟦 재배작물별 재배면적 ===================================================================================================
name = "재배작물별 재배면적_Cultivated Area by Growing Crops"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("cultivated", "area", "crop")),
                 filter_text_data(text_data, c("재배", "작물", "재배", "면적")))
print(combined.list[[name]])
text_data = L2
text_data = text_data[! text_data %in% unlist(combined.list)]


#### 🟦 산림사업 융자 ===================================================================================================
##### 🟪 실적 =====================================================================
name = "산림사업 융자실적_Loans for Forest Activities"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("loan", "forest", "activities")),
                 filter_text_data(text_data, c("산림", "융자", "실적")))
print(combined.list[[name]])
text_data = L2
text_data = text_data[! text_data %in% unlist(combined.list)]

##### 🟪 규모 =====================================================================
name = "산림사업 융자규모_Loans for Forestry Business"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("loan", "forest", "business")),
                 filter_text_data(text_data, c("산림", "융자", "규모")))
print(combined.list[[name]])
text_data = L2
text_data = text_data[! text_data %in% unlist(combined.list)]


#### 🟦 영림단 ===================================================================================================
name = "영림단 운영현황_Forest Management Units"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("forest", "management", "unit")),
                 filter_text_data(text_data, c("영림단")))
print(combined.list[[name]])
text_data = L2
text_data = text_data[! text_data %in% unlist(combined.list)]


#### 🟦 임가 현황 ===================================================================================================
##### 🟪 개인 =====================================================================
name = "개인 임가 현황_Private Forest Households"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("private", "forest", "household")),
                 filter_text_data(text_data, c("개인", "임가")))
print(combined.list[[name]])
text_data = L2
text_data = text_data[! text_data %in% unlist(combined.list)]

##### 🟪 전 겸업별 =====================================================================
name = "전ㆍ겸업별 임가 현황_Forest Households by Fall and Part Time"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("forest", "household", "part", "time")),
                 filter_text_data(text_data, c("겸업별", "임가", "현황")))
print(combined.list[[name]])
text_data = L2
text_data = text_data[! text_data %in% unlist(combined.list)]


##### 🟪 경영형태별 =====================================================================
name = "경영형태별 임가 현황_Forest Households by management type"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("forest", "household", "management")),
                 filter_text_data(text_data, c("경영", "형태", "임가")))
print(combined.list[[name]])
text_data = L2
text_data = text_data[! text_data %in% unlist(combined.list)]


##### 🟪 생산형태별 =====================================================================
name = "생산형태별 임가 현황_Forest Households by Production Type"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("forest", "households", "production", "type")),
                 filter_text_data(text_data, c("생산", "형태", "임가")))
print(combined.list[[name]])
text_data = L2
text_data = text_data[! text_data %in% unlist(combined.list)]


#### 🟦 사유림 ===================================================================================================
##### 🟪 소유 규모별 산주 =====================================================================
name = "소유 규모별 사유림 산주현황_Status of Private Forest Land Owners by Land Area Size"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("private", "forest", "land", "owner", "area", "size")),
                 filter_text_data(text_data, c("사유림", "소유", "규모", "산주")))
print(combined.list[[name]])
text_data = L2
text_data = text_data[! text_data %in% unlist(combined.list)]


##### 🟪 지역별 산주 현황 =====================================================================
name = "지역별 사유림 산주현황_Ownership of Private Forests by Regions"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("private", "forest", "region", "ownership")),
                 filter_text_data(text_data, c("지역", "사유림", "산주")))
print(combined.list[[name]])
text_data = L2
text_data = text_data[! text_data %in% unlist(combined.list)]


##### 🟪 산주 거주지 =====================================================================
name = "임야소재지별 사유림 개인산주 거주지 현황_Location of Residence of Private Forest Land Owner by Location of Forest Land"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("location", "residence", "private", "forest", "land", "owner")),
                 filter_text_data(text_data, c("임야", "소재", "사유림", "개인", "산주", "거주지")))
print(combined.list[[name]])
text_data = L2
text_data = text_data[! text_data %in% unlist(combined.list)]




# 🟥 텍스트 필터링 ==========================================================================================================================
##### 🟪 소재 부재 산주 현황  =====================================================================
name = "사유림 소재ㆍ부재 산주현황_Status of Resident and Non-resident Forest Owners by Province"
print(combined.list[[name]])
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("resident", "forest", "owner", "province")),
                 filter_text_data(text_data, c("사유림", "소재", "산주"), c("거주지")))
text_data = L2
text_data = text_data[! text_data %in% unlist(combined.list)]


##### 🟪 협업 경영 사업 =====================================================================
name = "사유림 협업경영사업_Activities of Private Forest Cooperatives"
print(combined.list[[name]])
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("private", "forest", "cooperation", "activities")),
                 filter_text_data(text_data, c("사유림", "협업", "경영", "사업")))
text_data = L2
text_data = text_data[! text_data %in% unlist(combined.list)]



#### 🟦 임업 기계 장비 보유 ===================================================================================================
name = "임업기계 · 장비  보유현황_Forest Machinery and Equipment"
print(combined.list[[name]])
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("forest", "machinery", "equipment")),
                 filter_text_data(text_data, c("임업", "기계", "장비")))
text_data = L2
text_data = text_data[! text_data %in% unlist(combined.list)]



#### 🟦 임가 및 임가 인구 ===================================================================================================
name = "임가 및 임가 인구_Number of Households and Population engaged in the Forestry Sector"
print(combined.list[[name]])
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("number", "household", "engaged", "forestry")),
                 filter_text_data(text_data, c("임가", "인구")))
text_data = L2
text_data = text_data[! text_data %in% unlist(combined.list)]


#### 🟦 산림사업 고용 현황 ===================================================================================================
name = "산림사업 고용현황_Status of Employment in Forestry"
print(combined.list[[name]])
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("employment", "forestry")),
                 filter_text_data(text_data, c("산림", "고용")))
text_data = L2
text_data = text_data[! text_data %in% unlist(combined.list)]




#### 🟦 임업 노동력 ===================================================================================================
name = "임업노동력현황_Number Of Workers Engaged In Forestry"
print(combined.list[[name]])
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("number", "worker", "engaged", "forestry")),
                 filter_text_data(text_data, c("임업", "노동력", "사유림", "개인", "산주", "거주지")))
text_data = L2
text_data = text_data[! text_data %in% unlist(combined.list)]




#### 🟦 경영계획 ===================================================================================================
##### 🟪 산림/영림계획 작성 현황 ======================================================================================================
name = "산림/영림경영계획 작성현황_Preparation of Forest Management Plan"
print(combined.list[[name]])
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("preparation", "forest", "management", "plan")),
                 filter_text_data(text_data, c("산림", "경영", "계획", "작성")),
                 filter_text_data(text_data, c("영림", "계획", "작성")))
text_data = L2
text_data = text_data[! text_data %in% unlist(combined.list)]


##### 🟪 산림/영림계획 편성 실적 ======================================================================================================
# name = "영림계획 편성실적_Accomplishment of Forest Management Plan by Ownership"
# print(combined.list[[name]])
# combined.list[[name]] =
#   union_multiple(filter_text_data(text_data, c("accomplishment", "forest", "management", "plan")),
#                  filter_text_data(text_data, c("산림", "경영", "계획", "편성", "실적")),
#                  filter_text_data(text_data, c("영림", "계획", "편성", "실적")))


##### 🟪 국유림 및 민유림 경영계획 ======================================================================================================
name = "국유림 및 민유림 경영계획 편성실적_Accomplishment of Working Plan Preparation"
print(combined.list[[name]])
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("accomplishment", "working", "plan", "preparation")),
                 filter_text_data(text_data, c("국유림", "민유림", "경영계획", "편성", "실적")))
text_data = L2
text_data = text_data[! text_data %in% unlist(combined.list)]



#### 🟦 산촌생태마을 조성 ===================================================================================================
name = "산촌생태마을 조성현황_Status of Mountain Village Development"
print(combined.list[[name]])
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("mountain", "village", "development")),
                 filter_text_data(text_data, c("산촌", "생태마을", "조성")))
text_data = L2
text_data = text_data[! text_data %in% unlist(combined.list)]



#### 🟦 해외산림개발 진출 ===================================================================================================
name = "해외산림개발진출현황_Overseas Forest Development"
print(combined.list[[name]])
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("overseas", "forest", "development")),
                 filter_text_data(text_data, c("해외", "산림", "개발", "진출")))
text_data = L2
text_data = text_data[! text_data %in% unlist(combined.list)]



#### 🟦 독림가 및 임업 후계자 ===================================================================================================
name = "독림가 및 임업후계자 현황_Outstanding Forest Managers and Forest Successors"
print(combined.list[[name]])
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("outstanding", "forest", "managers", "successors")),
                 filter_text_data(text_data, c("독림가", "임업", "후계자")))
text_data = L2
text_data = text_data[! text_data %in% unlist(combined.list)]



#### 🟦 화전정리 ===================================================================================================
name = "화전정리실적_Arrangement of Shifting Cultivations"
print(combined.list[[name]])
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("arrangement", "shifting", "cultivations")),
                 filter_text_data(text_data, c("화전", "정리", "실적")))
text_data = L2
text_data = text_data[! text_data %in% unlist(combined.list)]



#### 🟦 경계 측량 및 표주 설치  ==================================================================================
name = "국유임야 경계측량 및 표주설치 실적_Boundary Survey and Landmark in National Forest"
print(combined.list[[name]])
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("boundary", "survey", "landmark", "national", "forest")),
                 filter_text_data(text_data, c("국유임", "측량", "표주", "실적")),
                 filter_text_data(text_data, c("국유림", "측량", "표주", "실적")),
                 filter_text_data(text_data, c("boundary", "survey", "landmark", "national", "forest")),
                 filter_text_data(text_data, c("국유임야", "경제", "측량", "표주", "설치", "실적")))
text_data = L2
text_data = text_data[! text_data %in% unlist(combined.list)]





#### 🟦 임도시설 ===================================================================================================
name = "임도시설 현황_Status of Forest Road"
print(combined.list[[name]])
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("status", "forest", "road")),
                 filter_text_data(text_data, c("임도", "시설", "현황")))
text_data = L2
text_data = text_data[! text_data %in% unlist(combined.list)]




#### 🟦 민유림 관리 ===================================================================================================
name = "민유림 관리_Non-national forest management"
print(combined.list[[name]])
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("non", "national", "forest")),
                 filter_text_data(text_data, c("민유림", "관리")))
text_data = L2
text_data = text_data[! text_data %in% unlist(combined.list)]




#### 🟦 국유림 관리 ===================================================================================================
name = "국유림 관리_National forest management"
print(combined.list[[name]])
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("national", "forest")),
                 filter_text_data(text_data, c("국유림", "관리")))
text_data = L2
text_data = text_data[! text_data %in% unlist(combined.list)]



#### 🟦 관행작벌 사업생산 및 매각실적_Production and sale of timber by government felling ===================================================================================================
name = "관행작벌 사업생산 및 매각실적_Production and sale of timber by government felling"
print(combined.list[[name]])
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("production", "sale", "timber")),
                 filter_text_data(text_data, c("관행", "작벌", "사업")))
text_data = L2
text_data = text_data[! text_data %in% unlist(combined.list)]



