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


# 3. 기초통계 분석 -------------------------------------------------------------
## 3.1. 시도 이사비율 ----------------------------------------------------------

moving_2021_lvl_01_tbl <- moving_2021_lvl_01 %>% 
  left_join(sido_cd, by = c("전입지" = "시도")) %>% 
  left_join(pop_2021_sido, by = c("시도명" = "행정구역명")) %>% 
  select(연도, 전출지, 전입지, 인구수, 전입자수=계) %>% 
  mutate(이사비율 = 전입자수 / 인구수) %>% 
  arrange(desc(이사비율)) %>%
  distinct()

moving_2021_lvl_01_tbl

## 3.2. 시도 이사비율 시각화 ---------------------------------------------------

moving_2021_lvl_01_tbl_viz <- moving_2021_lvl_01_tbl %>% 
  select(전입지, 인구수, 전입자수, 이사비율) %>% 
  pivot_longer(cols = c(인구수, 전입자수)) 

moving_2021_lvl_01_tbl_viz_g <- moving_2021_lvl_01_tbl_viz %>% 
  ggplot(aes(x = fct_reorder(전입지, value), y = value, fill = name, 
             width = ifelse(name == "전입자수", 0.3, 0.5))) +
    geom_col(stat = "identity") +
    theme_bw(base_family = "NanumGothic") +
    theme(legend.position = "top",
          axis.text.y = element_text(size = rel(1.5), colour = "gray35", family = "NanumBarunpen", face="bold"),
          axis.text.x = element_text(size = rel(1.0), colour = "black", family = "NanumBarunpen", face="bold",
                                     angle = 15, vjust = 0.5, hjust=0.5),
          strip.background=element_rect(fill="gray95"),
          plot.title=element_text(size=18, face="bold", family = "NanumBarunpen"),
          plot.subtitle=element_text(face="bold", size=13, colour="grey10", family = "NanumBarunpen")) +
    labs(x        = "",
         y        = "",
         title    = "시도별 이동인구수",
         subtitle = "2021년 인구수/전입자수 [이사비율]",
         caption  = "자료 출처: 국가통계포털(KOSIS), https://kosis.kr/publication/publicationThema.do",
         fill     = "") +
    scale_y_continuous(labels = scales::comma) +
    coord_flip() +
    scale_fill_manual(values = c("gray77", "darkblue", "red")) +
    geom_rect(aes(ymin=14000000, ymax=15500000, xmin=-Inf, xmax=Inf), fill="gray90") +
    geom_text(data = moving_2021_lvl_01_tbl_viz , 
              aes(x = 전입지, y = 14750000, 
                  label=glue::glue("[ {scales::percent(이사비율, accuracy = 0.1)} ]")), 
              fontface="bold", size=5, family="NanumBarunPen") 

moving_2021_lvl_01_tbl_viz_g

ragg::agg_png(glue::glue("module/images/moving_2021_lvl_01_tbl_viz_g.png"), width = 297, height = 210, units = "mm", res = 600)
moving_2021_lvl_01_tbl_viz_g
dev.off()  
  


# 4. 시도 --> 시도  -----------------------------------------------------------

# 4.1. 기술통계 ---------------------------------------------------------------

moving_2021_lvl_02  %>% 
  arrange(desc(계)) %>% 
  mutate(비율 = 계/sum(계),
         누적비율 = cumsum(비율))

# 4.2. 네트워크 데이터 ----------------------------------------------------------------
library(tidygraph)
library(ggraph)

## 4.2.1. 네트워크 데이터 정제: 결점 -----

moving_nodes <- pop_2021_sido %>% 
  left_join(sido_cd, by = c("행정구역명" = "시도명")) %>% 
  select(id = 시도, 인구수, 구분) %>% 
  mutate(인구수 = 인구수/10^4)

## 4.2.2. 네트워크 데이터 정제: 연결선 -----

moving_edge <- moving_2021_lvl_02 %>%
  rename(from = 전출지,
         to   = 전입지,
         이사인구수 = 계) %>% 
  mutate(이사인구수 = 이사인구수/10^4)

## 4.2.3. 네트워크 데이터 변환(tidygraph) -----
moving_nw <- tbl_graph(
  nodes = moving_nodes, edges = moving_edge, directed = TRUE
)

# 4.3. 네트워크 시각화 --------------------------------------------------

moving_sido_nw_g <- ggraph(moving_nw, layout = "linear", circular = TRUE) +
  # geom_edge_link(aes(width = 이사인구수), alpha = 0.8) +
  geom_edge_arc(aes(width = 이사인구수), alpha = 0.3) +
  geom_edge_loop(aes(width = 이사인구수))+ 
  scale_edge_width(range = c(0.1, 5)) +
  geom_node_point(aes(size = 인구수, color = 구분)) +
  geom_label(aes(x = x, y = y,label = id), nudge_y = 0.1) +
  theme_graph(base_family="NanumGothic") 

ragg::agg_png(glue::glue("module/images/moving_sido_nw_g.png"), width = 297, height = 210, units = "mm", res = 600)
moving_sido_nw_g
dev.off()  



