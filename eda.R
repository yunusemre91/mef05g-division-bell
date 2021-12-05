library(tidyr)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(scales)
library(gridExtra)

df <- readRDS("natural_gas_data.rds")

str(df)

summary(df)

ggplot(df,
       aes(x=Date,
           y=Gas_Reference_Price)) +
  geom_bar(stat = "identity",
           aes(fill=Gas_Reference_Price)) +
  theme_light() +
  geom_hline(yintercept = mean(df$Gas_Reference_Price),
             size=1,
             color="red") +
  scale_fill_gradient(name="Gas Reference Price") +
  labs(title="Daily Gas Reference Prices",
       x="Date",
       y="Gas Reference Price")

ggplot(df,
       aes(x=Date,
           y=Total_Trade_Volume)) +
  geom_bar(stat = "identity",
           aes(fill=Total_Trade_Volume)) +
  theme_light() +
  geom_hline(yintercept = mean(df$Total_Trade_Volume),
             size=1,
             color="red") +
  scale_fill_gradient(name="Total Trade Volume") +
  labs(title="Daily Total Trade Volume",
       x="Date",
       y="Total Trade Volume")

sd(df$Total_Trade_Volume)
sd(df$Gas_Reference_Price)

view(df)
tv_years_average <- df %>%
  group_by(year_num) %>%
  summarise(mean_yearly_tv = mean(Total_Trade_Volume)) %>%
  ggplot(aes(x=year_num, y=mean_yearly_tv, fill=mean_yearly_tv)) +
  geom_bar(stat="identity") +
  geom_hline(yintercept = mean(df$Total_Trade_Volume),
             size=1,
             color="red") +
  labs(
    x="Years",
    y="Average Yearly Total Trade Volume",
    title="Average Trade Volume over the Years"
  )
tv_years_total <- df %>%
  group_by(year_num) %>%
  summarise(total_yearly_tv = sum(Total_Trade_Volume)) %>%
  ggplot(aes(x=year_num, y=total_yearly_tv, fill=total_yearly_tv)) +
  geom_bar(stat="identity") +
  labs(
    x="Years",
    y="Total Yearly Trade Volume",
    title="Total Trade Volume over the Years"
  )
gp_years_average <- df %>%
  group_by(year_num) %>%
  summarise(mean_yearly_gp = mean(Gas_Reference_Price)) %>%
  ggplot(aes(x=year_num, y=mean_yearly_gp, fill=mean_yearly_gp)) +
  geom_bar(stat="identity") +
  geom_hline(yintercept = mean(df$Gas_Reference_Price),
             size=1,
             color="red") +
  labs(
    x="Years",
    y="Average Yearly Gas Reference Price",
    title="Average Gas Reference Price over the Years"
  )
gp_years_total <- df %>%
  group_by(year_num) %>%
  summarise(total_yearly_gp = sum(Gas_Reference_Price)) %>%
  ggplot(aes(x=year_num, y=total_yearly_gp, fill=total_yearly_gp)) +
  geom_bar(stat="identity") +
  labs(
    x="Years",
    y="Total Yearly Gas Reference Price",
    title="Total Gas Reference Price over the Years"
  )
grid.arrange(gp_years_average, gp_years_total, tv_years_average, tv_years_total, ncol=2)

tv_months_average <- df %>%
  group_by(month_num) %>%
  summarise(mean_monthly_tv = mean(Total_Trade_Volume)) %>%
  ggplot(aes(x=month_num, y=mean_monthly_tv, fill=mean_monthly_tv)) +
  geom_bar(stat="identity") +
  geom_hline(yintercept = mean(df$Total_Trade_Volume),
           size=1,
           color="red") +
  labs(
    x="Months",
    y="Average Monthly Total Trade Volume",
    title="Average Trade Volume over the Months"
  )
tv_months_total <- df %>%
  group_by(month_num) %>%
  summarise(total_monthly_tv = sum(Total_Trade_Volume)) %>%
  ggplot(aes(x=month_num, y=total_monthly_tv, fill=total_monthly_tv)) +
  geom_bar(stat="identity") +
  labs(
    x="Months",
    y="Total Monthly Trade Volume",
    title="Total Trade Volume over the Months"
  )
gp_months_average <- df %>%
  group_by(month_num) %>%
  summarise(mean_monthly_gp = mean(Gas_Reference_Price)) %>%
  ggplot(aes(x=month_num, y=mean_monthly_gp, fill=mean_monthly_gp)) +
  geom_bar(stat="identity") +
  geom_hline(yintercept = mean(df$Gas_Reference_Price),
             size=1,
             color="red") +
  labs(
    x="Months",
    y="Average Monthly Gas Reference Price",
    title="Average Gas Reference Price over the Months"
  )

gp_months_total <- df %>%
  group_by(month_num) %>%
  summarise(total_monthly_gp = sum(Gas_Reference_Price)) %>%
  ggplot(aes(x=month_num, y=total_monthly_gp, fill=total_monthly_gp)) +
  geom_bar(stat="identity") +
  labs(
    x="Months",
    y="Total Monthly Gas Reference Price",
    title="Total Gas Reference Price over the Months"
  )
grid.arrange(gp_months_average, gp_months_total, tv_months_average, tv_months_total, ncol=2)

tv_weeks_average <- df %>%
  group_by(week_num) %>%
  summarise(mean_weekly_tv = mean(Total_Trade_Volume)) %>%
  ggplot(aes(x=week_num, y=mean_weekly_tv, fill=mean_weekly_tv)) +
  geom_bar(stat="identity") +
  geom_hline(yintercept = mean(df$Total_Trade_Volume),
           size=1,
           color="red") +
  labs(
    x="Weeks",
    y="Average Weekly Total TV",
    title="Average TV over the Weeks"
  )
tv_weeks_total <- df %>%
  group_by(week_num) %>%
  summarise(total_weekly_tv = sum(Total_Trade_Volume)) %>%
  ggplot(aes(x=week_num, y=total_weekly_tv, fill=total_weekly_tv)) +
  geom_bar(stat="identity") +
  labs(
    x="Weeks",
    y="Total Weekly TV",
    title="Total TV over the Weeks"
  )
gp_weeks_average <- df %>%
  group_by(week_num) %>%
  summarise(mean_weekly_gp = mean(Gas_Reference_Price)) %>%
  ggplot(aes(x=week_num, y=mean_weekly_gp, fill=mean_weekly_gp)) +
  geom_bar(stat="identity") +
  geom_hline(yintercept = mean(df$Gas_Reference_Price),
             size=1,
             color="red") +
  labs(
    x="Weeks",
    y="Average Weekly GRP",
    title="Average GRP over the Weeks"
  )
gp_weeks_total <- df %>%
  group_by(week_num) %>%
  summarise(total_weekly_gp = sum(Gas_Reference_Price)) %>%
  ggplot(aes(x=week_num, y=total_weekly_gp, fill=total_weekly_gp)) +
  geom_bar(stat="identity") +
  labs(
    x="Weeks",
    y="Total Weekly GRP",
    title="Total GRP over the Weeks"
  )

grid.arrange(gp_weeks_average, gp_weeks_total, tv_weeks_average, tv_weeks_total, ncol=2)

tv_days_average <- df %>%
  group_by(day_of_week) %>%
  summarise(mean_daily_tv = mean(Total_Trade_Volume)) %>%
  ggplot(aes(x=day_of_week, y=mean_daily_tv, fill=mean_daily_tv)) +
  geom_bar(stat="identity") +
  geom_hline(yintercept = mean(df$Total_Trade_Volume),
           size=1,
           color="red") +
  labs(
    x="Days of the Week",
    y="Average Daily Total TV",
    title="Average TV over the Weekdays"
  )
tv_days_total <- df %>%
  group_by(day_of_week) %>%
  summarise(total_daily_tv = sum(Total_Trade_Volume)) %>%
  ggplot(aes(x=day_of_week, y=total_daily_tv, fill=total_daily_tv)) +
  geom_bar(stat="identity") +
  labs(
    x="Days of the Week",
    y="Daily Total TV",
    title="Total TV over the Weekdays"
  )
gp_days_average <- df %>%
  group_by(day_of_week) %>%
  summarise(mean_daily_gp = mean(Gas_Reference_Price)) %>%
  ggplot(aes(x=day_of_week, y=mean_daily_gp, fill=mean_daily_gp)) +
  geom_bar(stat="identity") +
  geom_hline(yintercept = mean(df$Gas_Reference_Price),
             size=1,
             color="red") +
  labs(
    x="Days of the Week",
    y="Average Daily GRP",
    title="Average GRP over the Weekdays"
  )
gp_days_total <- df %>%
  group_by(day_of_week) %>%
  summarise(total_daily_gp = sum(Gas_Reference_Price)) %>%
  ggplot(aes(x=day_of_week, y=total_daily_gp, fill=total_daily_gp)) +
  geom_bar(stat="identity") +
  labs(
    x="Days of the Week",
    y="Total Daily GRP",
    title="Total GRP over the Weekdays"
  )
grid.arrange(gp_days_average, gp_days_total, tv_days_average, tv_days_total, ncol=2)

tv_seasons_average <- df %>%
  group_by(season) %>%
  summarise(mean_seasonal_tv = mean(Total_Trade_Volume)) %>%
  ggplot(aes(x=season, y=mean_seasonal_tv, fill=mean_seasonal_tv)) +
  geom_bar(stat="identity") +
  geom_hline(yintercept = mean(df$Total_Trade_Volume),
             size=1,
             color="red") +
  labs(
    x="Seasons",
    y="Average Seasonal TV",
    title="Average TV over the Seasons",
    fill="Average Seasonal TV"
  )
tv_seasons_total <- df %>%
  group_by(season) %>%
  summarise(total_seasonal_tv = sum(Total_Trade_Volume)) %>%
  ggplot(aes(x=season, y=total_seasonal_tv, fill=total_seasonal_tv)) +
  geom_bar(stat="identity") +
  labs(
    x="Seasons",
    y="Total Seasonal TV",
    title="Total TV over the Seasons",
    fill="Total Seasonal TV"
  )
gp_seasons_average <- df %>%
  group_by(season) %>%
  summarise(mean_seasonal_gp = mean(Gas_Reference_Price)) %>%
  ggplot(aes(x=season, y=mean_seasonal_gp, fill=mean_seasonal_gp)) +
  geom_bar(stat="identity") +
  geom_hline(yintercept = mean(df$Gas_Reference_Price),
             size=1,
             color="red") +
  labs(
    x="Seasons",
    y="Average Seasonal GRP",
    title="Average GRP over the Seasons",
    fill="Average Seasonal GRP"
  )
gp_seasons_total <- df %>%
  group_by(season) %>%
  summarise(total_seasonal_gp = sum(Gas_Reference_Price)) %>%
  ggplot(aes(x=season, y=total_seasonal_gp, fill=total_seasonal_gp)) +
  geom_bar(stat="identity") +
  labs(
    x="Seasons",
    y="Total Seasonal GRP",
    title="Total GRP over the Seasons",
    fill="Total Seasonal GRP"
  )
grid.arrange(gp_seasons_average, gp_seasons_total, tv_seasons_average, tv_seasons_total, ncol=2)
