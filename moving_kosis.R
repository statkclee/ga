# 0. 환경설정 ----------------------------------------------------------------------

library(tidyverse)
library(readxl)


# 1. 데이터 ------------------------------------------------------------------
## 1.1. 행정코드 --------------------------
adm_code_2019 <- read_excel("data/국내이동통계/020429.xlsx", sheet = "행정구역코드", skip = 2)
adm_code_2020 <- read_excel("data/국내이동통계/020430.xlsx", sheet = "행정구역코드", skip = 2)
adm_code_2021 <- read_excel("data/국내이동통계/020431.xlsx", sheet = "행정구역코드", skip = 2)


adm_code_lst <- list("2019" = adm_code_2019, "2020" = adm_code_2020, "2021" = adm_code_2021)

adm_code <- adm_code_lst %>% 
  enframe(name = "연도", value = "행정코드")

adm_code %>% 
  write_rds("data/국내이동통계/adm_code.rds")

## 1.2. 시군구 이동 --------------------------
## 출처: 온라인간행물 --> 인구 --> 국내인구이동통계 --> 2. 이동자수
### 시도명 - 전국제외
sido_cd <- read_excel("data/시도2자리코드.xlsx")

adm_sido_cd <- sido_cd %>% pull(시도) %>% setdiff("전국")

moving_2021_raw <- read_excel("data/국내이동통계/03012313.xlsx", sheet = "_2021")

moving_2021 <- moving_2021_raw %>% 
  select(전출지, 전입지, 계) 

### 전국 --> 시도 -------
moving_2021_lvl_01 <- moving_2021 %>% 
  filter(전출지 == "전국",
         전입지 %in% adm_sido_cd)   

### 시도 --> 시도 -------
moving_2021_lvl_02 <- moving_2021 %>% 
  filter(전출지 %in% adm_sido_cd,
         전입지 %in% adm_sido_cd)

### 시도 --> 구시군 -------
moving_2021_lvl_03 <- moving_2021 %>% 
  filter(전출지 != "전국", 전입지 != "전국") %>% 
  filter(전출지 %in% adm_sido_cd,
         ! 전입지 %in% adm_sido_cd) 

### 구시군 --> 구시군 -------
moving_2021_lvl_04 <- moving_2021 %>% 
  filter(전출지 != "전국", 전입지 != "전국") %>% 
  filter(! 전출지 %in% adm_sido_cd,
         ! 전입지 %in% adm_sido_cd)

## 1.3. 연앙인구 (2021) --------------------------

pop_2021_raw <- read_excel("data/국내이동통계/주민등록연앙인구_2021_1035.xls", 
                      sheet = "2021년_5세간격", skip = 2)

pop_2021_sido <- pop_2021_raw %>% 
  filter(LEVEL == "시도",
         성별 == "전체") %>% 
  select(연도, 행정구역명, 인구수 = 합계)


# 3. 기초통계 분석 --------------------------------------------------------------

moving_2021_lvl_01 %>% 
  left_join(sido_cd, by = c("전입지" = "시도")) %>% 
  left_join(pop_2021_sido, by = c("시도명" = "행정구역명")) %>% 
  select(연도, 전출지, 전입지, 인구수, 전입자수=계) %>% 
  mutate(이사비율 = 전입자수 / 인구수) %>% 
  arrange(desc(이사비율)) %>%
  distinct()





