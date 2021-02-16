library(tidyverse)

tuesdata <- tidytuesdayR::tt_load('2021-02-09')

home_owner <- tuesdata$home_owner
income_aggregate <- tuesdata$income_aggregate
income_distribution <- tuesdata$income_distribution
income_limits <- tuesdata$income_limits
income_mean <- tuesdata$income_mean
income_time <- tuesdata$income_time
lifetime_earn <- tuesdata$lifetime_earn
race_wealth <- tuesdata$race_wealth
retirement <- tuesdata$retirement
student_debt <- tuesdata$student_debt




home_owner %>%
  group_by(race) %>%
  ggplot(aes(x=year,y=home_owner_pct))+
  geom_line(aes(col=race))

income_aggregate %>%
  filter(race=="All Races", income_quintile!="Top 5%") %>%
  mutate(income_quintile=fct_relevel(income_quintile, "Highest","Fourth","Third","Second","Lowest")) %>%
  group_by(income_quintile) %>%
  ggplot(aes(x=year,y=income_share))+
  geom_col(aes(fill=income_quintile))

income_distribution %>%
  filter(race=="All Races") %>%
  mutate(income_bracket=fct_relevel(income_bracket, "$200,000 and over","$150,000 to $199,999","$100,000 to $149,999","$75,000 to $99,999","$50,000 to $74,999","$35,000 to $49,999", "$25,000 to $34,999", "$15,000 to $24,999", "Under $15,000")) %>%
  group_by(income_bracket) %>%
  ggplot(aes(x=year,y=income_distribution))+
  geom_col(aes(fill=income_bracket))

#income_limits

#income_mean


income_time %>%
  group_by(percentile) %>%
  ggplot(aes(x=year,y=income_family))+
  geom_line(aes(col=percentile))

lifetime_earn %>%
  group_by(race) %>%
  ggplot(aes(x=race,y=lifetime_earn))+
  geom_col(aes(fill=gender), position = "dodge")+
  coord_flip()

race_wealth %>%
  filter(race!="Non-White", type=="Median",year>=1980) %>%
  group_by(race) %>%
  ggplot(aes(x=year,y=wealth_family))+
  geom_line(aes(col=race))

retirement %>%
  group_by(race) %>%
  ggplot(aes(x=year,y=retirement))+
  geom_line(aes(col=race))

student_debt %>%
  group_by(race) %>%
  ggplot(aes(x=year,y=loan_debt))+
  geom_line(aes(col=race))


student_debt %>%
  group_by(race) %>%
  ggplot(aes(x=year,y=loan_debt_pct))+
  geom_line(aes(col=race))
