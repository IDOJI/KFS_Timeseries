# 🟥 Load data ##########################################################################
path_data = "/Users/Ido/Documents/DataAnalysis/KFS_Timeseries/3.Data with HDR/8.3.Combined Data.rds"
data = readRDS(path_data)

id = data %>% names




# 🟥 (완료)각 데이터의 2번째 열이름 확인 ##########################################################################
## 🟧 추출 ================================================================
second_col_name = sapply(data, function(x){
  names(x)[2]
}) %>% unname %>% unlist %>% as.vector

# 인덱스 추출
ind = which(second_col_name != "행")
if(length(ind )==0){
  print("No error!")
}

# 체크
# data[names(data) == "YRBK_00070702"][[1]] %>% View


# data[[1111]] %>% View




# 🟥 (완료)1행3열 원소 확인 ##########################################################################
## 🟧 Check ================================================================================
elements = sapply(data, function(x){
  x[1,3] %>% unlist
}) %>% unname %>% unique


## 🟧 이상한 항목들 ================================================================================
### 🟨 ID 저장 벡터  ========================================================================
check_id = list()


### 🟨"전   국\r\nWhole country"  ========================================================================
elements
element = "전   국\r\nWhole country"
selected = viewer(data, element)
selected %>% View
add_to_check_id(element, selected)
print(check_id)
elements = exclude_element(elements, element)




### 🟨 "1973_6412"  ========================================================================
elements
element =  "1973_6412"
selected = viewer(data, element)
selected %>% View
add_to_check_id(element, selected)
print(check_id)
elements = exclude_element(elements, element)



### 🟨 "계_18461.18_32개소"   ========================================================================
elements
element =  "계_18461.18_32개소"
selected = viewer(data, element)
selected %>% View
add_to_check_id(element, selected)
print(check_id)
elements = exclude_element(elements, element)



### 🟨 "1973_6412"   ========================================================================
elements
element =  "1973_6412"
selected = viewer(data, element)
selected %>% View
add_to_check_id(element, selected)
print(check_id)
elements = exclude_element(elements, element)




### 🟨 "1968_2810"  ========================================================================
elements
element =  "1968_2810"
selected = viewer(data, element)
selected %>% View
add_to_check_id(element, selected)
print(check_id)
elements = exclude_element(elements, element)



### 🟨 "2016_2015"  ========================================================================
elements
element =  "2016_2015"
selected = viewer(data, element)
selected %>% View
add_to_check_id(element, selected)
print(check_id)
elements = exclude_element(elements, element)



### 🟨 "1985_7321_6792"  ========================================================================
elements
element =  "1985_7321_6792"
selected = viewer(data, element)
selected %>% View
add_to_check_id(element, selected)
print(check_id)
elements = exclude_element(elements, element)



### 🟨  "1974_6356"  ========================================================================
elements
element =  "1974_6356"
selected = viewer(data, element)
selected %>% View
add_to_check_id(element, selected)
print(check_id)
elements = exclude_element(elements, element)



### 🟨  "계_11483.22_32개소"  ========================================================================
elements
element =  "계_11483.22_32개소"
selected = viewer(data, element)
selected %>% View
add_to_check_id(element, selected)
print(check_id)
elements = exclude_element(elements, element)



### 🟨  "계_18082.16_32개소"  ========================================================================
elements
element =  "계_18082.16_32개소"
selected = viewer(data, element)
selected %>% View
add_to_check_id(element, selected)
print(check_id)
elements = exclude_element(elements, element)



### 🟨  "남부영림서_226517"  ========================================================================
elements
element =  "남부영림서_226517"
selected = viewer(data, element)
selected %>% View
add_to_check_id(element, selected)
print(check_id)
elements = exclude_element(elements, element)



### 🟨   "1952_9925096"   ========================================================================
elements
element =   "1952_9925096" 
selected = viewer(data, element)
selected %>% View
add_to_check_id(element, selected)
print(check_id)
elements = exclude_element(elements, element)



### 🟨   "산업_41134.5_Industries"     ========================================================================
elements
element =   "산업_41134.5_Industries"
selected = viewer(data, element)
selected %>% View
add_to_check_id(element, selected)
print(check_id)
elements = exclude_element(elements, element)




### 🟨   "총계_계_1093"       ========================================================================
elements
element =   "총계_계_1093"  
selected = viewer(data, element)
selected %>% View
add_to_check_id(element, selected)
print(check_id)
elements = exclude_element(elements, element)



### 🟨    "산업_48,229.8_Industries"    ========================================================================
elements
element =    "산업_48,229.8_Industries" 
selected = viewer(data, element)
selected %>% View
add_to_check_id(element, selected)
print(check_id)
elements = exclude_element(elements, element)





## 🟧 최종 확인할 ID들 ================================================================================
check_id_vec = check_id %>% unlist %>% unname
print(check_id_vec)



# 🟥 L3에 NA 항목들 확인 ##########################################################################
id = "YRBK_001202050203"
data[names(data) == id][[1]] %>% View


# 🟥 기타 ##########################################################################
check_yb(data, "YRBK_004905040101")
check_yb(data, "YRBK_004905040102")

yb_old = read.xlsx("/Users/Ido/Documents/DataAnalysis/KFS_Timeseries/Data/0.Raw Data/DB_20240415/TB_NDI_MRV_STTST_YRBK_OF_FRSTR_20240415.xlsx")
yb_old %>% filter(연보.ID == "YRBK_004905040102")


yb_2 = read.csv("/Users/Ido/Documents/DataAnalysis/KFS_Timeseries/Data/2.Rearrange YB names/1.rearranged_yb_names.csv")
yb_2 %>% filter(ID_L4 == "YRBK_004905040101")
yb_2 %>% filter(ID_L4 == "YRBK_004905040102")
yb_2$

yb = read.csv("/Users/Ido/Documents/DataAnalysis/KFS_Timeseries/Data/2.Rearrange YB names/6.Added YB ID.csv")
yb %>% filter(ID == "YRBK_004905040101")












