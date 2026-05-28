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

#---------------------------------------------------------------------------
# THI 계산

# ============================================================
# 한우 THI 단계별 일수 계산 코드
# 기준:
# - 개체별 기간: 출생일자 ~ 도축일자, 양끝 날짜 포함
# - 매칭 기준: 관측지점ID + 날짜
# - 온도 기본값: (일최저온도 + 일최대온도) / 2
# - THI 단계는 서로 겹치지 않게 분류
#   양호: THI < 72
#   주의: 72 <= THI < 78
#   경고: 78 <= THI < 89
#   위험: 89 <= THI < 98
#   심각: 98 <= THI
# ============================================================

library(data.table)

# ------------------------------------------------------------
# 1. 안전한 날짜 변환 함수
# ------------------------------------------------------------

parse_date_safe <- function(x) {
  x_chr <- trimws(as.character(x))
  x_chr[x_chr %in% c("", "-99", "NA", "NaN", "NULL", "null")] <- NA_character_
  
  out <- as.Date(rep(NA_character_, length(x_chr)))
  
  # 1) 2023-01-05, 2023/01/05, 2023.01.05, 2023년01월05일 처리
  x_clean <- x_chr
  x_clean <- gsub("년|월", "-", x_clean)
  x_clean <- gsub("일", "", x_clean)
  x_clean <- gsub("[./]", "-", x_clean)
  x_clean <- gsub("\\s+", "", x_clean)
  x_clean <- sub("-$", "", x_clean)
  
  idx1 <- !is.na(x_clean) & grepl("^\\d{4}-\\d{1,2}-\\d{1,2}$", x_clean)
  out[idx1] <- suppressWarnings(as.Date(x_clean[idx1], format = "%Y-%m-%d"))
  
  # 2) 20230105 또는 2023-01-05 00:00:00 같은 값 처리
  x_digit <- gsub("\\D", "", x_chr)
  
  idx2 <- is.na(out) & !is.na(x_digit) & nchar(x_digit) >= 8
  out[idx2] <- suppressWarnings(as.Date(substr(x_digit[idx2], 1, 8), format = "%Y%m%d"))
  
  # 3) 엑셀 날짜 일련번호 처리: 예) 45234
  x_num <- suppressWarnings(as.numeric(x_chr))
  
  idx3 <- is.na(out) & !is.na(x_num) & x_num >= 20000 & x_num <= 80000
  out[idx3] <- as.Date(x_num[idx3], origin = "1899-12-30")
  
  data.table::as.IDate(out)
}

# ------------------------------------------------------------
# 2. 숫자 변환 함수
# ------------------------------------------------------------

to_num <- function(x) {
  x_chr <- trimws(as.character(x))
  x_chr[x_chr %in% c("", "-99", "NA", "NaN", "NULL", "null")] <- NA_character_
  suppressWarnings(as.numeric(gsub(",", "", x_chr)))
}

mean_or_na <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  } else {
    return(mean(x, na.rm = TRUE))
  }
}

# ------------------------------------------------------------
# 3. THI 단계별 일수 추가 함수
# ------------------------------------------------------------

add_thi_stage_counts <- function(
    train,
    weather,
    id_col = "개체식별번호",
    stn_col = "관측지점ID",
    birth_col = "출생일자",
    slaughter_col = "도축일자",
    weather_date_col = "날짜",
    rh_col = "일평균상대습도",
    tmin_col = "일최저온도",
    tmax_col = "일최대온도",
    tavg_col = "일평균온도",
    temp_mode = c("mean_minmax", "tmax", "tavg")
) {
  
  temp_mode <- match.arg(temp_mode)
  
  stage_names <- c("양호", "주의", "경고", "위험", "심각")
  cnt_cols <- paste0("THI_", stage_names, "일수")
  cum_cols <- paste0("cum_", cnt_cols)
  
  # ----------------------------------------------------------
  # 필수 컬럼 확인
  # ----------------------------------------------------------
  
  need_train <- c(id_col, stn_col, birth_col, slaughter_col)
  
  if (!all(need_train %in% names(train))) {
    stop(
      "hanwoo_train_ko에 필요한 컬럼이 없습니다: ",
      paste(setdiff(need_train, names(train)), collapse = ", ")
    )
  }
  
  need_weather_base <- c(stn_col, weather_date_col, rh_col)
  
  if (!all(need_weather_base %in% names(weather))) {
    stop(
      "hanwoo_weather_ko에 필요한 컬럼이 없습니다: ",
      paste(setdiff(need_weather_base, names(weather)), collapse = ", ")
    )
  }
  
  if (temp_mode == "mean_minmax") {
    need_temp <- c(tmin_col, tmax_col)
    if (!all(need_temp %in% names(weather))) {
      stop(
        "temp_mode = 'mean_minmax'를 쓰려면 필요한 온도 컬럼이 없습니다: ",
        paste(setdiff(need_temp, names(weather)), collapse = ", ")
      )
    }
  }
  
  if (temp_mode == "tmax") {
    if (!(tmax_col %in% names(weather))) {
      stop("temp_mode = 'tmax'를 쓰려면 일최대온도 컬럼이 필요합니다.")
    }
  }
  
  if (temp_mode == "tavg") {
    if (!(tavg_col %in% names(weather))) {
      stop("temp_mode = 'tavg'를 쓰려면 일평균온도 컬럼이 필요합니다.")
    }
  }
  
  # ----------------------------------------------------------
  # train 데이터 정리
  # ----------------------------------------------------------
  
  train_dt <- as.data.table(copy(train))
  
  # 같은 코드를 반복 실행할 때 기존 결과 컬럼 제거
  old_result_cols <- intersect(
    c(
      "row_id_THI",
      cnt_cols,
      "THI_전체기간일수",
      "THI_계산가능일수",
      "THI_기상누락일수"
    ),
    names(train_dt)
  )
  
  if (length(old_result_cols) > 0) {
    train_dt[, (old_result_cols) := NULL]
  }
  
  train_dt[, row_id_THI := .I]
  
  train_dt[, (stn_col) := trimws(as.character(get(stn_col)))]
  train_dt[get(stn_col) %in% c("", "-99", "NA", "NaN", "NULL", "null"),
           (stn_col) := NA_character_]
  
  train_dt[, (birth_col) := parse_date_safe(get(birth_col))]
  train_dt[, (slaughter_col) := parse_date_safe(get(slaughter_col))]
  
  train_dt[, THI_전체기간일수 := fifelse(
    !is.na(get(birth_col)) &
      !is.na(get(slaughter_col)) &
      get(birth_col) <= get(slaughter_col),
    as.integer(get(slaughter_col) - get(birth_col)) + 1L,
    NA_integer_
  )]
  
  # ----------------------------------------------------------
  # weather 데이터 정리
  # ----------------------------------------------------------
  
  weather_dt <- as.data.table(copy(weather))
  
  weather_dt[, (stn_col) := trimws(as.character(get(stn_col)))]
  weather_dt[get(stn_col) %in% c("", "-99", "NA", "NaN", "NULL", "null"),
             (stn_col) := NA_character_]
  
  weather_dt[, (weather_date_col) := parse_date_safe(get(weather_date_col))]
  weather_dt[, RH_for_THI := to_num(get(rh_col))]
  
  # 온도 선택
  if (temp_mode == "mean_minmax") {
    weather_dt[, TEMP_for_THI := (to_num(get(tmin_col)) + to_num(get(tmax_col))) / 2]
  }
  
  if (temp_mode == "tmax") {
    weather_dt[, TEMP_for_THI := to_num(get(tmax_col))]
  }
  
  if (temp_mode == "tavg") {
    weather_dt[, TEMP_for_THI := to_num(get(tavg_col))]
  }
  
  # ----------------------------------------------------------
  # 관측지점ID + 날짜 단위로 THI 계산
  # 중복 행이 있으면 평균으로 정리
  # ----------------------------------------------------------
  
  weather_daily <- weather_dt[
    !is.na(get(stn_col)) &
      !is.na(get(weather_date_col)),
    .(
      TEMP_for_THI = mean_or_na(TEMP_for_THI),
      RH_for_THI   = mean_or_na(RH_for_THI)
    ),
    by = c(stn_col, weather_date_col)
  ]
  
  weather_daily[, THI :=
                  (1.8 * TEMP_for_THI + 32) -
                  ((0.55 - 0.0055 * RH_for_THI) *
                     (1.8 * TEMP_for_THI - 26.8))
  ]
  
  # ----------------------------------------------------------
  # THI 단계 분류
  # ----------------------------------------------------------
  
  weather_daily[, THI단계 := fcase(
    is.na(THI), NA_character_,
    THI >= 98, "심각",
    THI >= 89, "위험",
    THI >= 78, "경고",
    THI >= 72, "주의",
    default = "양호"
  )]
  
  # 단계별 indicator 생성
  for (i in seq_along(stage_names)) {
    stage <- stage_names[i]
    col <- cnt_cols[i]
    
    weather_daily[, (col) := as.integer(!is.na(THI단계) & THI단계 == stage)]
  }
  
  # ----------------------------------------------------------
  # 관측지점ID별 누적합 생성
  # ----------------------------------------------------------
  
  setorderv(weather_daily, c(stn_col, weather_date_col))
  
  weather_daily[
    ,
    (cum_cols) := lapply(.SD, cumsum),
    by = stn_col,
    .SDcols = cnt_cols
  ]
  
  weather_cum <- weather_daily[
    ,
    c(stn_col, weather_date_col, cum_cols),
    with = FALSE
  ]
  
  setkeyv(weather_cum, c(stn_col, weather_date_col))
  
  # ----------------------------------------------------------
  # 도축일자까지 누적값 조회
  # ----------------------------------------------------------
  
  end_lookup <- train_dt[
    ,
    .(
      row_id_THI,
      key_stn = get(stn_col),
      key_date = get(slaughter_col)
    )
  ]
  
  setnames(end_lookup, c("key_stn", "key_date"), c(stn_col, weather_date_col))
  
  end_cum <- weather_cum[
    end_lookup,
    on = c(stn_col, weather_date_col),
    roll = TRUE
  ]
  
  end_cum <- end_cum[, c("row_id_THI", cum_cols), with = FALSE]
  setnames(end_cum, cum_cols, paste0("end_", cnt_cols))
  
  # ----------------------------------------------------------
  # 출생일자 전날까지 누적값 조회
  # ----------------------------------------------------------
  
  start_lookup <- train_dt[
    ,
    .(
      row_id_THI,
      key_stn = get(stn_col),
      key_date = get(birth_col) - 1L
    )
  ]
  
  setnames(start_lookup, c("key_stn", "key_date"), c(stn_col, weather_date_col))
  
  start_cum <- weather_cum[
    start_lookup,
    on = c(stn_col, weather_date_col),
    roll = TRUE
  ]
  
  start_cum <- start_cum[, c("row_id_THI", cum_cols), with = FALSE]
  setnames(start_cum, cum_cols, paste0("start_", cnt_cols))
  
  # ----------------------------------------------------------
  # 출생일자 ~ 도축일자 구간의 단계별 일수 계산
  # ----------------------------------------------------------
  
  counts_dt <- merge(
    end_cum,
    start_cum,
    by = "row_id_THI",
    all = TRUE,
    sort = FALSE
  )
  
  for (col in cnt_cols) {
    end_col <- paste0("end_", col)
    start_col <- paste0("start_", col)
    
    counts_dt[, (col) :=
                fifelse(is.na(get(end_col)), 0L, get(end_col)) -
                fifelse(is.na(get(start_col)), 0L, get(start_col))
    ]
  }
  
  counts_dt[, THI_계산가능일수 := rowSums(.SD), .SDcols = cnt_cols]
  
  # 유효하지 않은 기간은 NA 처리
  valid_period <- train_dt[
    ,
    .(
      row_id_THI,
      valid_THI_period =
        !is.na(get(stn_col)) &
        get(stn_col) != "" &
        !is.na(get(birth_col)) &
        !is.na(get(slaughter_col)) &
        get(birth_col) <= get(slaughter_col)
    )
  ]
  
  counts_dt <- merge(
    counts_dt,
    valid_period,
    by = "row_id_THI",
    all.x = TRUE,
    sort = FALSE
  )
  
  counts_dt[
    is.na(valid_THI_period) | valid_THI_period == FALSE,
    c(cnt_cols, "THI_계산가능일수") := NA
  ]
  
  # ----------------------------------------------------------
  # train 데이터에 결과 컬럼 붙이기
  # ----------------------------------------------------------
  
  result_dt <- merge(
    train_dt,
    counts_dt[, c("row_id_THI", cnt_cols, "THI_계산가능일수"), with = FALSE],
    by = "row_id_THI",
    all.x = TRUE,
    sort = FALSE
  )
  
  result_dt[, THI_기상누락일수 := THI_전체기간일수 - THI_계산가능일수]
  
  setorder(result_dt, row_id_THI)
  result_dt[, row_id_THI := NULL]
  
  return(result_dt)
}

# ------------------------------------------------------------
# 4. 실제 실행
# ------------------------------------------------------------

hanwoo_train_thi <- add_thi_stage_counts(
  train = hanwoo_train_ko,
  weather = hanwoo_weather_ko,
  temp_mode = "mean_minmax"
)

check_cols <- c(
  "개체식별번호",
  "관측지점ID",
  "출생일자",
  "도축일자",
  "THI_양호일수",
  "THI_주의일수",
  "THI_경고일수",
  "THI_위험일수",
  "THI_심각일수",
  "THI_전체기간일수",
  "THI_계산가능일수",
  "THI_기상누락일수"
)

head(hanwoo_train_thi[, ..check_cols])



library(data.table)

# data.table 형태로 맞추기
setDT(hanwoo_train_thi)

# 기상누락일수가 있는 데이터만 추출
missing_weather_data <- hanwoo_train_thi[
  !is.na(THI_기상누락일수) & THI_기상누락일수 > 0
]

# 행 개수 확인
nrow(missing_weather_data)

# 앞부분 확인
head(missing_weather_data)
