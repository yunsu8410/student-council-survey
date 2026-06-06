# 기존 작업 지우기
rm(list = ls())
# 워킹디렉토리 설정
setwd(getwd())
getwd()
# 기본 패키지 설치
library(tidyverse) #기능 불러오기
#데이터 파일 불러와 표로 만들기-------------------------------------------------
list.files()
data1 = 'INU바로미터 총학생회 여론조사 1주차.csv' %>% 
  read_csv( Encoding('cp949')) %>% 
  mutate(주차 ='1')
data2 = 'INU바로미터 총학생회 여론조사 2주차.csv' %>% 
  read_csv( Encoding('cp949')) %>% 
  mutate(주차 = '2')
names(data1)
data4 = read_csv('학과 명단 유권자수.csv')
data5 = read_csv('단대 유권자수.csv')

# 학번 중복, 두 자리 삭제하기(이거 돌리면 값이 줄어듦)
name.data = data2 %>% # data숫자로 주차 바꾸기
  left_join(data4, by = c("X7" = "학과")) %>% 
  arrange(X6, desc(unknown)) %>%
  distinct(X6, .keep_all = TRUE) %>% 
  filter(as.numeric(X6)>100000000 & 1000000000>as.numeric(X6)) 

# 1. 설문조사 단과대 비율 확인하기
dan.data= name.data %>% 
  group_by(단과대학) %>%
  filter(!is.na(단과대학)) %>% 
  summarise(`설문 추출 수` =n()) %>% 
  mutate(`설문 추출 비율` = round(`설문 추출 수`/sum(`설문 추출 수`)*100, 1))
# 표본 인원 = 설문조사 총 합계를 실제 비율에 대입했을 때 나오는 인원
dan.data= data5 %>% 
  mutate(`표본 인원` = round(nrow(name.data) * `단대 유권자 비율` / 100, 1)) %>% 
  left_join(dan.data, by = c("단과대학"="단과대학")) %>% 
  mutate(`과대표집 확인` = `표본 인원`-`설문 추출 수`)
dan.data1 = dan.data %>% 
  select(단과대학,`설문 추출 수`,`설문 추출 비율`)
write.csv(dan.data, "INU바로미터 총학생회 여론조사 1주차(단과대 비율).csv", row.names = FALSE, fileEncoding = 'cp949')

# 2. 과대표집 된 단과대에서 몇명을 삭제해야 할까?
result2 <- dan.data %>%
  filter(`과대표집 확인` < 0) %>%
  summarise(합계 = -sum(`과대표집 확인`) - (nrow(name.data) - 130)) %>%
  pull(합계)
# ^ 과정: 과대표집 된 수 다 삭제하면 200명 아래가 되니
# 200명 아래가 되지 않기 위해서는 몇 명은 남겨야 할까? = 6.2명
# 6.2명은 과대표집되어도 그냥 남겨야 한다
result <- dan.data %>%
  filter(`과대표집 확인` < 0) %>%
  mutate(
    `과대표집 확인` = abs(`과대표집 확인`),  # 상황 열의 값을 양수로 변환
    음수합계 = sum(`과대표집 확인`), 
    비율 = `과대표집 확인` / 음수합계 * 100,
    조정값 = 비율 * result2 / 100,  # 소수점 절삭 후 최종 삭제 인원 계산
    최종삭제인원 = floor(`과대표집 확인` - 조정값)  # 상황에서 최종 삭제 인원 빼기
  ) %>%
  select(단과대학, `과대표집 확인`, 비율, 조정값, 최종삭제인원)
# ^ 6.2명을 각 단과대가 과대표집된 비율만큼 나눠줘서 과대표집 인원만큼 빼주기
# 그 결과 최종삭제인원 도출

# 3. 최종삭제인원을 단과대에서 어떻게 제거할까?
# 단과대 별로 학과가 과대 대표된 곳을 찾아 그만큼 제거하자
dande = '글로벌정경대학' #삭제 원하는 단과대
how = 15 # 단과대 설문조사 추출 명 - 최종삭제인원
몇명제거 = data4 %>% 
  filter(단과대학 == dande) %>% 
  mutate(`실제 단대별 비율` = round(유권자/sum(유권자)*100,1)) %>% 
  select(학과,`실제 단대별 비율`)
몇명제거 = name.data %>% 
  group_by(X7) %>%
  filter(!is.na(X7)) %>% 
  summarise(`표본 추출 수` =n()) %>% 
  left_join(data4, by = c('X7' = '학과')) %>% 
  select(단과대학, X7, `표본 추출 수`) %>% 
  filter(단과대학 == dande) %>% 
  mutate(`표본 단대별 비율` = round(`표본 추출 수`/sum(`표본 추출 수`)*100,1)) %>% 
  left_join(몇명제거, by = c('X7' = '학과')) %>% 
  mutate(`실제 몇 명` = round(how*`실제 단대별 비율`/100,1),
         `-만큼 제거` = round(how*`실제 단대별 비율`/100-`표본 추출 수`,1))

# 4. 학과별로 수 붙여서 랜덤 추출하기
name.data <- name.data %>%
  rename(학과 = X7)
# slice_sample(n = 4)에는 남겨야 하는 값만큼 입력
sampled_data <- bind_rows(
  name.data %>% filter(학과 == "정치외교학과") %>% slice_sample(n = 2),
  name.data %>% filter(학과 == "행정학과") %>% slice_sample(n = 2),
  name.data %>% filter(학과 == "소비자학과") %>% slice_sample(n = 2),
  name.data %>% filter(학과 == "신소재공학과") %>% slice_sample(n = 3),
  name.data %>% filter(학과 == "에너지화학공학과") %>% slice_sample(n = 3),
  name.data %>% filter(학과 == "생명공학부") %>% slice_sample(n = 4),
  name.data %>% filter(학과 == "생명과학부") %>% slice_sample(n = 4),
  name.data %>% filter(학과 == "문헌정보학과") %>% slice_sample(n = 5),
  name.data %>% filter(!학과 %in% c("정치외교학과", "행정학과", "신소재공학과", "에너지화학공학과", "생명공학부", "생명과학부", "문헌정보학과"))
)
write.csv(sampled_data, "2주차_표본조정후.csv", fileEncoding = 'cp949')
# 전처리 후 단과대 비율 확인
sample_dan.data = sampled_data %>% 
  group_by(단과대학) %>%
  filter(!is.na(단과대학)) %>% 
  summarise(`설문 추출 수` =n()) %>% 
  mutate(`설문 추출 비율` = round(`설문 추출 수`/sum(`설문 추출 수`)*100, 1))

# 5. 지지율
# 과대표집 전처리 전 지지율 분석
turnout.data = name.data %>% 
  group_by(X8) %>%
  filter(!is.na(단과대학)) %>% 
  summarise(n =n()) %>% 
  mutate(rate = round(n/sum(n)*100, 1))
# 학과 과대표집 제거 후 지지율 확인
sample_turnout.data = sampled_data %>% 
  group_by(X8) %>%
  filter(!is.na(단과대학)) %>% 
  summarise(n =n()) %>% 
  mutate(rate = round(n/sum(n)*100, 1)) %>% 
  print()
voting_summary <- sampled_data %>%
  mutate(
    단과대학_그룹 = case_when(
      단과대학 %in% c("경영대학", "글로벌정경대학", "동북아국제통상학부") ~ "경영, 글정경, 동북아",
      단과대학 %in% c("공과대학") ~ "공과",
      단과대학 %in% c("생명과학기술대학", "자연과학대학", "정보기술대학") ~ "생명, 자연, 정보",
      단과대학 == "도시과학대학" ~ "도시대",
      단과대학 %in% c("법학부", "사회과학대학", "인문대학") ~ "법학, 인문, 사과대",
      단과대학 %in% c("사범대학", "예술체육대학") ~ "사범, 예체대",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(단과대학_그룹)) %>%  # 그룹이 없는 경우 제외
  group_by(단과대학_그룹, X8) %>%
  summarise(갯수 = n(), .groups = "drop") %>%
  pivot_wider(names_from = X8, values_from = 갯수, values_fill = 0)
final_voting_summary <- as.data.frame(t(voting_summary))

# 5-1. 왜 그 후보자 지지?
# 기호 1번
sampled_data$X8
기호1번선택이유 = sampled_data %>% 
  filter(X8 == '기호 1번 (학생회장: 장형도 / 부학생회장: 이겨레)') %>% 
  group_by(X9) %>% 
  summarise(`왜 그 후보 선택` = n()) %>% 
  mutate(비율 = round(`왜 그 후보 선택`/sum(`왜 그 후보 선택`)*100,1)) %>% 
  arrange(desc(`왜 그 후보 선택`))
기호2번선택이유 = sampled_data %>% 
  filter(X8 == '기호 2번 (학생회장: 한광덕 / 부학생회장: 이지민)') %>% 
  group_by(X9) %>% 
  summarise(`왜 그 후보 선택` = n()) %>% 
  mutate(비율 = round(`왜 그 후보 선택`/sum(`왜 그 후보 선택`)*100,1)) %>% 
  arrange(desc(`왜 그 후보 선택`))

# 투표율
voting_summary <- sampled_data %>%
  mutate(
    단과대학_그룹 = case_when(
      단과대학 %in% c("경영대학", "글로벌정경대학", "동북아국제통상학부") ~ "경영, 글정경, 동북아",
      단과대학 %in% c("공과대학") ~ "공과",
      단과대학 %in% c("생명과학기술대학", "자연과학대학", "정보기술대학") ~ "생명, 자연, 정보",
      단과대학 == "도시과학대학" ~ "도시대",
      단과대학 %in% c("법학부", "사회과학대학", "인문대학") ~ "법학, 인문, 사과대",
      단과대학 %in% c("사범대학", "예술체육대학") ~ "사범, 예체대",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(단과대학_그룹)) %>%  # 그룹이 없는 경우 제외
  group_by(단과대학_그룹, X10) %>%
  summarise(갯수 = n(), .groups = "drop") %>%
  pivot_wider(names_from = X10, values_from = 갯수, values_fill = 0)
투표율 <- as.data.frame(t(voting_summary))
# 7-1. 누구에게 투표하려고 하십니까?
why_vote = sampled_data %>% 
  filter(X10 == "한다.") %>% 
  group_by(X11) %>% 
  summarise(`왜 총학생회 투표` = n()) %>% 
  mutate(비율 = round(`왜 총학생회 투표`/sum(`왜 총학생회 투표`)*100,1)) %>% 
  arrange(desc(`왜 총학생회 투표`))
# 7-2. 왜 총학생회 선거에 투표하지 않으려고 하십니까?
why_no_vote = sampled_data %>% 
  filter(X10 == "안한다.") %>% 
  group_by(X14) %>% 
  summarise(`왜 투표 안해` = n()) %>% 
  mutate(비율 = round(`왜 투표 안해`/sum(`왜 투표 안해`)*100,1)) %>% 
  arrange(desc(`왜 투표 안해`))
# 8. 응답자님께서는 어떤 투표 방식을 선호하십니까?
what_type = sampled_data %>%
  pivot_longer(cols = c(X12, X15), names_to = "응답", values_to = "투표방식") %>%
  mutate(응답 = ifelse(응답 == "X12", "한다", "안한다")) %>%
  filter(!is.na(투표방식)) %>%
  group_by(응답,투표방식) %>%
  summarise(합산 = n()) %>%
  arrange(desc(투표방식))

