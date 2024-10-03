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
                 filter_text_data(text_data, c("status", "산림", "보호구역", "지정", "현황")))
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






##### 🟪 "산림 유전자원 보호림 및 보호수 지정 현황_Status of protected area for genetic forest resources and designated protected trees" ===================================================================================================
name = "산림 유전자원 보호림 및 보호수 지정 현황_Status of protected area for genetic forest resources and designated protected trees"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("status", "natural", "forest", "reserve", "nurse-tree"), c("유전")),
                 filter_text_data(text_data, c("천연", "보호림","보호수", "현황", "유전")),
                 filter_text_data(text_data, c("보호수", "현황", "유전")))
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
                 filter_text_data(text_data, c("산림", "전용", "허가")))
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



#### 🟦 사유림 협업 경영 사업 ===================================================================================================
name = "사유림 협업 경영 사업_Activities of Private Forest Cooperatives"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("activities", "private", "forest", "cooperatives")),
                 filter_text_data(text_data, c("사유림", "협업", "경영", "사업")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]



#### 🟦 독림가 ===================================================================================================
name = "독림가 현황_Details of Sincere Forest Manager"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("details", "sincere", "forest", "manager"), "내역"),
                 filter_text_data(text_data, c("독림가", "현황"), "내역"))
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




#### 🟦 임업 노동력 ===================================================================================================
name = "임업 노동력 현황_Number of Forestry Workers Engaged in Forestry"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("number", "forestry", "workers", "engaged", "forestry")),
                 filter_text_data(text_data, c("임업", "노동력", "현황")))
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









