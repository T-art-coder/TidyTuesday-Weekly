---
title: "TidyTuesday 2022-05-24"
subtitle: "Women's Rugby"
output: html_notebook
---

```{r}
library(tidyverse)

library(scales)
theme_set(theme_minimal())
library(lubridate)
```

```{r}
sevens <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-24/sevens.csv')
fifteens <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-24/fifteens.csv')
```

```{r}
fifteens
```

```{r}
fifteens %>% filter(winner == "England", tournament == "World Cup") %>% select(date) %>% distinct()
```


```{r}
fifteens %>% select(tournament) %>% distinct()
```

```{r}
fifteens <- fifteens %>% 
  mutate(winner_score = if_else(team_1 == winner, score_1, score_2),
                    n = 1)

fifteens
```


```{r}
fifteens_cl <- fifteens %>% 
  select(winner, tournament, n, margin_of_victory, winner_score) %>%   
  filter(tournament == "World Cup") %>%
  group_by(winner, tournament) %>% 
  summarise(my_sum = sum(n), 
            winner_score = round(mean(winner_score),1),
            margin = round(mean(margin_of_victory),1),
            winner_lead = round((winner_score/(winner_score - margin)),1)) %>%
  ungroup() %>%
  arrange(desc(my_sum))




fifteens_cl                                           
```

```{r}
library(gt)
library(gtExtras)
```

```{r}
fifteens_cl %>% select(winner, winner_score, my_sum, winner_lead) %>% 
  head(10) %>%
  gt() %>%
  gt_theme_nytimes()
```

```{r}
tab <- fifteens_cl %>% select(winner, winner_score, my_sum, winner_lead) %>% 
  head(10)

tab
```

```{r fig.height=12, fig.width=8}
tab %>%
  gt() %>%
  gt_theme_nytimes() %>%
  tab_header(title = "Women's Rugby World Cup Win Statistics",
             subtitle = md("**English Speaking Countries Hold Most Of The Cups**")) %>%
  fmt_number(columns = winner_lead,
             decimals = 0,
             pattern = "{x} X"
)%>%
   cols_label(winner_score = "Winner's Score",  
             my_sum = "Victories",
             winner_lead = "Average Winner lead") %>%
  tab_style(locations = 
              cells_title(groups = "title"),
            style = list(
              cell_text(font = google_font(name = "Bebas Neue"),
              size = 'xx-large',
              color = 'indianred'
              )))%>%
  tab_options(
    heading.subtitle.font.size = "medium"
  )


 


```

```{r}
tab <- tab %>%
  mutate(Country = case_when(
    str_detect(winner,'England') ~ 'https://hatscripts.github.io/circle-flags/flags/gb-eng.svg',
    str_detect(winner,'New Zealand') ~ 'https://raw.githubusercontent.com/HatScripts/circle-flags/369a12401e40e9b2ad7e25d78ee4bdf777e6c389/flags/nz.svg',
    str_detect(winner,'United States') ~ 'https://hatscripts.github.io/circle-flags/flags/us.svg',
    str_detect(winner,'France') ~ 'https://raw.githubusercontent.com/BjnNowak/TdF/main/fr.png',
    str_detect(winner,'Canada') ~ 
      'https://hatscripts.github.io/circle-flags/flags/ca.svg',
    str_detect(winner,'Australia') ~ 
      'https://hatscripts.github.io/circle-flags/flags/au.svg',
    str_detect(winner,'Spain') ~ 
    'https://hatscripts.github.io/circle-flags/flags/es.svg',
    str_detect(winner,'Kazakhstan') ~ 'https://hatscripts.github.io/circle-flags/flags/kz.svg',
    str_detect(winner,'Ireland') ~ 
      'https://hatscripts.github.io/circle-flags/flags/ie.svg',
    str_detect(winner,'Wales') ~ 
      'https://hatscripts.github.io/circle-flags/flags/gb-wls.svg',
    
  ))
```

```{r}
tab <- tab %>% relocate(Country, before = winner)
tab <- tab %>% rename(winner = before)

head(tab)
```

```{r}
tab
```


```{r}
tab_end <- tab %>% 
  gt() %>%
  gt_theme_nytimes() %>% 
  tab_header(title = "Women's Rugby World Cup Statistics",
             subtitle = md("**English Speaking Countries Dominate The Scene**")) %>%
  fmt_number(columns = winner_lead,
             decimals = 0,
             pattern = "{x} X"
             ) %>%
   cols_label(winner_score = "Average Score",  
             my_sum = "Wins",
             winner_lead = "Average Game Lead",
             winner = "") %>%
  tab_style(locations = 
              cells_title(groups = "title"),
            style = list(
              cell_text(font = google_font(name = "Bebas Neue"),
              size = 'xx-large',
              color = 'indianred'
              )))%>%
  tab_options(
    heading.subtitle.font.size = "medium"
  ) %>%
  gt_img_rows(columns = Country, height = 20) %>%
  # gt_merge_stack(col1 = winner, col2 = Country) %>%
 tab_source_note(source_note = "#TidyTuesday week 21 | Data from ScrumQueens on Women's Rugby | @TimurZolkin")

tab_end
```

```{r}
tab_end <- tab_end %>%
  cols_width(my_sum ~ px(100),
             winner_lead ~ px(100),
             winner_score ~ px(100))

tab_end
```
```{r}
# install.packages("webshot")
# library(webshot2)
tab_end %>% gtsave(filename =  "mytab2.png")
```

```{r}
fifteens_raw <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-24/fifteens.csv')

head(fifteens_raw)
```

```{r}
fifteen_wl <- fifteens_raw %>% pivot_longer(c("team_1", "team_2")) %>%
  pivot_longer(score_1: score_2, names_to = "team_score", values_to = "score") %>%
  filter(!(name=="team_1"& team_score=="score_2"),!(name=="team_2"& team_score=="score_1")) %>%
  group_by(value) %>%
  mutate(id = row_number(), win_lose = case_when(value == winner ~ 1, TRUE ~ 0)) %>%
  rename(team = value) %>%
  filter(tournament == "World Cup")


```

```{r}
w_outcomes <- fifteen_wl %>% group_by(winner) %>%
  summarise(Wins = length(win_lose[win_lose == 1]),
            Losses = length(win_lose[win_lose == 1]),
            outcomes = list(win_lose), .groups = "drop") %>%
  select(winner, outcomes)

w_outcomes
```

```{r}
tab_gt_with_outcomes <- tab %>%
  left_join(w_outcomes, by = "winner") %>%
  gt() %>%
  gt_theme_nytimes() %>% 
  tab_header(title = "Women's Rugby World Cup Statistics",
             subtitle = md("**English Speaking Countries Dominate The Scene**")) %>%
  fmt_number(columns = winner_lead,
             decimals = 0,
             pattern = "{x} X"
             ) %>%
   cols_label(winner_score = "Average Score",  
             my_sum = "Wins",
             winner_lead = "Average Lead",
             winner = "") %>%
  tab_style(locations = 
              cells_title(groups = "title"),
            style = list(
              cell_text(font = google_font(name = "Bebas Neue"),
              size = 'xx-large',
              color = 'indianred'
              )))%>%
  tab_options(
    heading.subtitle.font.size = "medium"
  ) %>%
  gt_img_rows(columns = Country, height = 20) %>%
  # gt_merge_stack(col1 = winner, col2 = Country) %>%
 tab_source_note(source_note = "#TidyTuesday week 21 | Data from ScrumQueens on Women's Rugby | @TimurZolkin") %>%
  gt_plt_winloss(outcomes, max_wins = 31) %>%
  cols_width(Country ~ px(100),
             winner ~ px(100),
             my_sum ~ px(100),
             winner_lead ~ px(100),
             winner_score ~ px(100), 
             outcomes ~ px(200))

tab_gt_with_outcomes
```
```{r}
tab_gt_with_outcomes %>% gtsave(filename =  "rugby_outcomes.png")
```
