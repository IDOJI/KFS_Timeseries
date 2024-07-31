# 🟥 data   ==========================================================================================================================
path_save = "/Users/Ido/Documents/DataAnalysis/KFS_Timeseries/2.Rearrange YB names"
data = read.csv("/Users/Ido/Documents/DataAnalysis/KFS_Timeseries/2.Rearrange YB names/3.2.Combined L3 data with L3 Category.csv")



# 🟥 L3가 NA인 항목  ==========================================================================================================================
data_NA_L3 = data %>% filter(Categorized_L3 %>% is.na)
text_data = data_NA_L3$NAME_L3
print(text_data)
  


# 🟥 L3 범주 클러스터링 확인  ==========================================================================================================================
L3_clustering = text_clustering(data$Categorized_L3 %>% unique, k_min = 100, k_max = 100)[[1]]
print(L3_clustering )


# 🟥 몇몇 L3 범주들 제외 ==========================================================================================================================
exclude <- c(
  "조수 보호구 현황_The Status of Sanctuary",
  "물가 지수_Price index",
  "독림가 및 임업후계자 현황_Outstanding Forest Managers and Forest Successors",
  "독림가 현황_Details of Sincere Forest Manager",
  "산림복지서비스 제공자 등록 현황_Enrollment of Forest Welfare Service Voucher-available-facilities",
  "산림복지전문업 등록현황_Status of Job Startups on Forest welfare",
  "사냥터운영상황_Operation of Hunting Ground",
  "임업기계 · 장비 보유현황_Forest Machinery and Equipment",
  "주요상품 도매가격_Wholesale Prices of Major Commodities",
  "임야소재지별 사유림 개인산주 거주지 현황_Location of Residence of Private Forest Land Owner by Location of Forest Land",
  "임업노동력현황_Number Of Workers Engaged In Forestry",
  "영림단 운영현황_Forest Management Units",                                                                                                                                                                     
  "영림단 조직 현황_Units of Forest Craft Workers", 
  "생산자 물가 지수_Producer Price index", 
  "산림사업 융자규모_Loans for Forestry Business",                                                                                                                                                               
  "산림사업 융자실적_Loans for Forest Activities", 
  "산림관계 세입예산_Forest Estimated Revenue",                                                                                                                                                                  
  "산림관계세출예산_Forest Estimated Expenditures",                                                                                                                                                              
  "산림교육 수혜인원 현황_The Number of Forest Education Recipients",                                                                                                                                            
  "산림교육전문가 양성기관 현황_Status of Forest Guide Training Organizations", 
  "맹수류 사육 현황_Status of Fierce Animal Rearing",    
  "가계용품 농가구입가격_Price of Household Goods Paid by Farmers",
  "천연기념물로 지정된 조수_Wildlife Designated as Natural Monument",                                                                                                                                            
  "천연기념물의 번식지 및 도래지_Breeding and Wintering Grounds Designated as Natural Monuments", 
  "지역별 사유림 산주현황_Ownership of Private Forests by Regions", 
  "주요화물별 선박수송량_Marine Transportation by Commodity",                                                                                                                                                    
  "주요화물별 철도수송량_Railway Freight Transportation by Commodity",
  "임산물 남북교역 현황_Trade of Forest Products between South and North Korea", 
  "임산물 생산액의 구성_Forest Products Value",                                                                                                                                                                  
  "임산물 수입실적_Import of Forest Products",                                                                                                                                                                   
  "임산물 수출실적_Export of Forest Products", 
  "임가 및 임가 인구_Number of Households and Population engaged in the Forestry Sector",                                                                                                                        
  "임가경제 주요지표_Main Indicators of Forest Household Economy",                          
  "야생조수 인공사육 현황_Status of Wildlife Rearing",  
  "숲 해설가 수혜인원 현황_Status of Beneficiaries of Forest Guide",                                                                                                                                             
  "숲사랑 소년단 육성현황_The Number of Green Rangers",         
  "산림사업 고용현황_Status of Employment in Forestry", 
  "산림관계 세입 세출 예산_Forest Budget for Revenue and Expenditure", 
  "사유림 소재ㆍ부재 산주현황_Status of Resident and Absentee Forest Owners by Province",                                                                                                                        
  "사유림 소재ㆍ부재 산주현황_Status of Resident and Non-resident Forest Owners by Province",   
  "사유림 소유규모별 산주현황_Private Forest Owners by Size",                                                                                                                                                    
  "사유림 소유형태별 산주현황_Private Forest Owners By Ownership",        
  "사유림 산주 현황_Ownership of Private Forests by Provinces",  
  "귀산촌인 연령별 현황_People Returning to Mountain Villages by Age",
  "측후소일람표_List of meteorological stations", 
  "임산물 생산액의 구성_Forest Products Value" ,
  "임산물 수입실적_Import of Forest Products"    ,
  "임산물 수출실적_Export of Forest Products",
  "인구변동 추이_Population Trend",                                                                                                      
  "임산물 유통시설 및 저온저장고 지원현황_Forest Products Marketing Facilities and Low Temperature Storage Houses Financially Supported",
  "해외 산림개발 진출현황_Oversea Forest Development",
  "경영형태별 임가 현황_Forest Households by management type",
  "생산형태별 임가 현황_Forest Households by Production Type",  
  "전ㆍ겸업별 임가 현황_Forest Households by Fall and Part Time",
  "제재공장 실태_Status of Sawmills",
  "개인 임가 현황_Private Forest Households",
  "휴양림 및 산림욕장 조성현황_Establishment of Recreation Forests",
  "휴양림 운영 및 이용현황_Number of Visitors to Recreation Forests",                                                                                                                                            
  "휴양림 이용현황_Visitors to Recreational Forests",
  "경제활동별 국내총생산_Gross Domestic Product By Kind Of Economic Activity",
  "대한민국의 위치_Location of Repubic of Korea",
  "전통마을 숲 조성현황_Establishment of Traditional Village Forests"
)


# exclude
data_exclude_L3 = data %>% filter(!Categorized_L3 %in% exclude)
data_exclude_L3$Categorized_L3 %>% unique

# test = data %>% filter(Categorized_L3  == "임산물 생산실적_Production of Forest Products")
# View(test)


# reconfirm
L3_clustering = text_clustering(data_exclude_L3$Categorized_L3 %>% unique, k_min = 100, k_max = 100)[[1]]
print(L3_clustering )




# 🟥 exclude ==========================================================================================================================
write.csv(data_exclude_L3, file.path(path_save, "3.3.Exclude L3.csv"), row.names=F)



