hanwoo_weather <- read.csv("C:/Users/user/Downloads/contest_data_2/hanwoo/hanwoo_weather.csv")
hanwoo_train <- read.csv("C:/Users/user/Downloads/contest_data_2/hanwoo/hanwoo_train.csv")
hanwoo_lineage <- read.csv("C:/Users/user/Downloads/contest_data_2/hanwoo/hanwoo_lineage.csv")
hanwoo_death <- read.csv("C:/Users/user/Downloads/contest_data_2/hanwoo/hanwoo_death.csv")
hanwoo_area <-read.csv("C:/Users/user/Downloads/contest_data_2/hanwoo/hanwoo_area.csv") 

# 컬럼명을 PDF의 변수 설명 기준으로 한국어로 변환하는 함수

colname_ko <- c(
  # 공통 / 축산 데이터
  "SIDO"              = "시도",
  "SIGUNGU"           = "시군구",
  "EUPMYEONDONG"      = "읍면동",
  "STN"               = "관측지점ID",
  "ABATT_DATE"        = "도축일자",
  "JUDGE_DATE"        = "판정일자",
  "JUDGE_SEX"         = "성별",
  "WEIGHT"            = "도체중",
  "BACKFAT"           = "등지방두께",
  "REA"               = "등심단면적",
  "WINDEX"            = "육량지수",
  "WGRADE"            = "육량등급",
  "INSFAT"            = "근내지방도",
  "YUKSAK"            = "육색",
  "FATSAK"            = "지방색",
  "TISSUE"            = "조직감",
  "GROWTH"            = "성숙도",
  "COST_AMT"          = "낙찰가격",
  "AGE"               = "나이",
  "BIRTH_YMD"         = "출생일자",
  "CATTLE_NO"         = "개체식별번호",
  "FARM_UNIQUE_NO"    = "농장식별번호",
  "LAST_GRADE"        = "최종등급",
  
  # 기상 데이터
  "DATE"              = "날짜",
  "TA_MIN"            = "일최저온도",
  "TA_MAX"            = "일최대온도",
  "RN_DAY"            = "일강수량",
  "RHM_AVG"           = "일평균상대습도",
  "WS_DAVG"           = "일평균풍속",
  
  # 농장별 폐사 개체 데이터
  "DEAD_YMD"          = "폐사일자",
  "DEAD_REASON"       = "폐사사유",
  
  # 농장별 사육현황 및 농장 면적 데이터
  "C2023"             = "2023년사육마릿수",
  "C2024"             = "2024년사육마릿수",
  "C2025"             = "2025년사육마릿수",
  "AREA"              = "사육농장면적",
  
  # 개체 혈통 데이터
  "KPN_NO"             = "정액번호",
  "FATHER_CATTLE_NO"   = "부개체식별번호",
  "MOTHER_ANIMAL_NO"   = "모개체식별번호",
  "F_GMOTHER_ANIMAL_NO"= "외조모개체식별번호",
  "F_GFATHER_CATTLE_NO"= "외조부개체식별번호",
  "M_GMOTHER_ANIMAL_NO"= "조모개체식별번호",
  "M_GFATHER_CATTLE_NO"= "조부개체식별번호"
)

rename_cols_ko <- function(df) {
  key <- toupper(names(df))
  new_names <- unname(colname_ko[key])
  
  names(df) <- ifelse(
    is.na(new_names),
    names(df),      # 매핑표에 없는 컬럼은 기존 이름 유지
    new_names
  )
  
  df
}

hanwoo_weather_ko <- rename_cols_ko(hanwoo_weather)
hanwoo_train_ko   <- rename_cols_ko(hanwoo_train)
hanwoo_lineage_ko <- rename_cols_ko(hanwoo_lineage)
hanwoo_death_ko   <- rename_cols_ko(hanwoo_death)
hanwoo_area_ko    <- rename_cols_ko(hanwoo_area)

head(hanwoo_weather_ko)
head(hanwoo_train_ko)
head(hanwoo_lineage_ko)
head(hanwoo_death_ko)

#--------------------------------------------------------------------------------------
# train / lineage의 개체 식별 번호 맞추기

library(dplyr)

# 1. 혹시 모를 공백/자료형 차이 제거
train_tmp <- hanwoo_train_ko %>%
  mutate(`개체식별번호` = trimws(as.character(`개체식별번호`)))

lineage_tmp <- hanwoo_lineage_ko %>%
  mutate(`개체식별번호` = trimws(as.character(`개체식별번호`)))

# 2. 각 데이터의 개체식별번호만 추출
train_ids <- train_tmp %>%
  filter(!is.na(`개체식별번호`),
         `개체식별번호` != "",
         `개체식별번호` != "-99") %>%
  distinct(`개체식별번호`)

lineage_ids <- lineage_tmp %>%
  filter(!is.na(`개체식별번호`),
         `개체식별번호` != "",
         `개체식별번호` != "-99") %>%
  distinct(`개체식별번호`)

# 3. 두 데이터에 공통으로 존재하는 개체식별번호
common_ids <- inner_join(
  train_ids,
  lineage_ids,
  by = "개체식별번호"
)

# 공통 개체식별번호 개수
nrow(common_ids)

# 공통 개체식별번호 일부 확인
head(common_ids, 20)

common_vec <- common_ids$`개체식별번호`

train_tmp %>%
  summarise(
    train_전체행 = n(),
    lineage와_매칭되는_행수 = sum(`개체식별번호` %in% common_vec),
    lineage와_매칭되지_않는_행수 = sum(!`개체식별번호` %in% common_vec),
    매칭비율_퍼센트 = round(mean(`개체식별번호` %in% common_vec) * 100, 2)
  )

nrow(hanwoo_train_ko)-nrow(hanwoo_lineage_ko)


#-------------------------------------------------------------------------
# 기상관측지점 ID 비교

library(dplyr)

# 자료형 차이 방지: 숫자/문자 섞여 있어도 비교되도록 문자형으로 변환
train_tmp <- hanwoo_train_ko %>%
  mutate(`관측지점ID` = trimws(as.character(`관측지점ID`)))

weather_tmp <- hanwoo_weather_ko %>%
  mutate(`관측지점ID` = trimws(as.character(`관측지점ID`)))

# 각 데이터의 고유 관측지점ID
train_stn <- train_tmp %>%
  filter(!is.na(`관측지점ID`),
         `관측지점ID` != "",
         `관측지점ID` != "-99") %>%
  distinct(`관측지점ID`)



weather_stn <- weather_tmp %>%
  filter(!is.na(`관측지점ID`),
         `관측지점ID` != "",
         `관측지점ID` != "-99") %>%
  distinct(`관측지점ID`)

# 공통 관측지점ID
common_stn <- inner_join(
  train_stn,
  weather_stn,
  by = "관측지점ID"
)

# 요약
tibble(
  train_관측지점수 = nrow(train_stn),
  weather_관측지점수 = nrow(weather_stn),
  공통_관측지점수 = nrow(common_stn),
  train_중_weather에_있는_비율 = round(nrow(common_stn) / nrow(train_stn) * 100, 2),
  weather_중_train에_있는_비율 = round(nrow(common_stn) / nrow(weather_stn) * 100, 2)
)


weather_not_in_train <- anti_join(
  weather_stn,
  train_stn,
  by = "관측지점ID"
)

head(weather_not_in_train, 20)
nrow(weather_not_in_train)