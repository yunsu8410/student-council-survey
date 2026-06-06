# 기존 작업 지우기
rm(list = ls())
# 워킹디렉토리 설정
setwd(getwd())
getwd()
# 기본 패키지 설치
library(tidyverse) #기능 불러오기
#데이터 파일 불러와 표로 만들기-------------------------------------------------
list.files()
names(data1)
data1 = read_csv('INU바로미터 총학생회 여론조사 1주차.csv', Encoding('cp949'))
data2 = read_csv('학과 명단 유권자수.csv')
data3 = read_csv('단대 유권자수.csv')

# 학번 중복, 두 자리 삭제하기(이거 돌리면 값이 줄어듦)
name.data = data1 %>% 
  left_join(data2, by = c("X7" = "학과")) %>% 
  arrange(X6, desc(unknown)) %>%
  distinct(X6, .keep_all = TRUE) %>% 
  filter(as.numeric(X6)>100000000 & 1000000000>as.numeric(X6)) 

# 과대표집 전처리 전 투표율 분석
turnout.data = name.data %>% 
  group_by(X8) %>%
  filter(!is.na(단과대학)) %>% 
  summarise(n =n()) %>% 
  mutate(rate = round(n/sum(n)*100, 1))

# 1. 설문조사 단과대 비율 확인하기
dan.data = name.data %>% 
  group_by(단과대학) %>%
  filter(!is.na(단과대학)) %>% 
  summarise(`설문 추출 수` =n()) %>% 
  mutate(`설문 추출 비율` = round(`설문 추출 수`/sum(`설문 추출 수`)*100, 1))
# 표본 인원 = 설문조사 총 합계를 실제 비율에 대입했을 때 나오는 인원
dan.data = data3 %>% 
  mutate(`표본 인원` = round(nrow(name.data) * `단대 유권자 비율` / 100, 1)) %>% 
  left_join(dan.data, by = c("단과대학"="단과대학")) %>% 
  mutate(`과대표집 확인` = `표본 인원`-`설문 추출 수`)
write.csv(dan.data, "INU바로미터 총학생회 여론조사 1주차(단과대 비율).csv", row.names = FALSE, fileEncoding = 'cp949')

# 2. 과대표집 된 단과대에서 몇명을 삭제해야 할까?
result2 <- dan.data %>%
  filter(`과대표집 확인` < 0) %>%
  summarise(합계 = -sum(`과대표집 확인`) - (nrow(name.data) - 200)) %>%
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
dande = '공과대학'
how = 51 # 단과대 설문조사 추출 명 - 최종삭제인원
hak.data = data2 %>% 
  filter(단과대학 == dande) %>% 
  mutate(`실제 단대별 비율` = round(유권자/sum(유권자)*100,1)) %>% 
  select(학과,`실제 단대별 비율`)
hak.data = name.data %>% 
  group_by(X7) %>%
  filter(!is.na(X7)) %>% 
  summarise(`설문 추출 수` =n()) %>% 
  left_join(data2, by = c('X7' = '학과')) %>% 
  select(단과대학, X7, `설문 추출 수`) %>% 
  filter(단과대학 == dande) %>% 
  mutate(`설문 단대별 비율` = round(`설문 추출 수`/sum(`설문 추출 수`)*100,1)) %>% 
  left_join(hak.data, by = c('X7' = '학과')) %>% 
  mutate(`실제 몇 명` = round(how*`실제 단대별 비율`/100,1),
         `얼마나 차이` = round(how*`실제 단대별 비율`/100-`설문 추출 수`,1))

# 4. 학과별로 수 붙여서 랜덤 추출하기
name.data <- name.data %>%
  rename(학과 = X7)
# slice_sample(n = 4)에는 남겨야 하는 값만큼 입력
sampled_data <- bind_rows(
  name.data %>% filter(학과 == "정치외교학과") %>% slice_sample(n = 4),
  name.data %>% filter(학과 == "무역학부") %>% slice_sample(n = 15),
  name.data %>% filter(학과 == "행정학과") %>% slice_sample(n = 4),
  name.data %>% filter(학과 == "기계공학과") %>% slice_sample(n = 14),
  name.data %>% filter(학과 == "산업경영공학과") %>% slice_sample(n = 10),
  name.data %>% filter(학과 == "불어불문학과") %>% slice_sample(n = 7),
  name.data %>% filter(학과 == "중어중국학과") %>% slice_sample(n = 8),
  name.data %>% filter(!학과 %in% c("정치외교학과", "무역학부", "행정학과", "기계공학과", "산업경영공학과", "불어불문학과", "중어중국학과"))
)
# 5.학과 과대표집 제거 후 투표율 확인
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
      단과대학 %in% c("공과대학", "생명과학기술대학", "자연과학대학", "정보기술대학") ~ "공과, 생명, 자연, 정보",
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

# 첫 번째 행을 열 이름으로 지정하고, 행 번호를 새로 추가
colnames(final_voting_summary) <- final_voting_summary[1, ]
final_voting_summary <- final_voting_summary[-1, ]
rownames(final_voting_summary) <- NULL

final_voting_summary
## 4. 과정과 5. 과정을 반복하면 값이 바뀌는 것을 확인(래덤으로 추출되기 때문)

# 6. 전처리 후 단과대 비율 확인
sample_dan.data = sampled_data %>% 
  group_by(단과대학) %>%
  filter(!is.na(단과대학)) %>% 
  summarise(`설문 추출 수` =n()) %>% 
  mutate(`설문 추출 비율` = round(`설문 추출 수`/sum(`설문 추출 수`)*100, 1))
# 7-1. 왜 총학생회 선거에 투표하려고 하십니까?
why_vote = sampled_data %>% 
  filter(X8 == "한다.") %>% 
  group_by(X9) %>% 
  summarise(`왜 총학생회 투표` = n()) %>% 
  mutate(비율 = round(`왜 총학생회 투표`/sum(`왜 총학생회 투표`)*100,1)) %>% 
  arrange(desc(`왜 총학생회 투표`))
# 7-2. 왜 총학생회 선거에 투표하지 않으려고 하십니까?
why_no_vote = sampled_data %>% 
  filter(X8 == "안한다.") %>% 
  group_by(X12) %>% 
  summarise(`왜 투표 안해` = n()) %>% 
  mutate(비율 = round(`왜 투표 안해`/sum(`왜 투표 안해`)*100,1)) %>% 
  arrange(desc(`왜 투표 안해`))
# 8. 응답자님께서는 어떤 투표 방식을 선호하십니까?
what_type = sampled_data %>%
  pivot_longer(cols = c(X10, X13), names_to = "응답", values_to = "투표방식") %>%
  mutate(응답 = ifelse(응답 == "X10", "한다", "안한다")) %>%
  filter(!is.na(투표방식)) %>%
  group_by(응답,투표방식) %>%
  summarise(합산 = n()) %>%
  arrange(desc(투표방식))
-------------------------------------------------------------------------------
# 주차 별 후보 지지율
sum.1 = join.data1 %>% 
  group_by(q3) %>%
  summarise(n = n()) %>% 
  mutate(rate = round(n/sum(n)*100, 1))
sum.2 = join.data2 %>% 
  group_by(q3) %>% 
  summarise(nn = n()) %>% 
  mutate(rate = round(nn/sum(nn)*100, 1))
sum.3 = join.data3 %>% 
  group_by(q3) %>% 
  summarise(nnn = n()) %>% 
  mutate(rate = round(nnn/sum(nnn)*100, 1))
sum.123 = sum.1 %>% 
  left_join(sum.2, by = c("q3" = "q3")) %>% 
  left_join(sum.3, by = c("q3" = "q3"))
#저장하기
write.csv(sum.123, "10.11 모임 때 후보자 지지율.csv", row.names = FALSE)

