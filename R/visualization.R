

library(tidyverse)
library(readxl)

restores <- read_xlsx("data/RESTORES 1.xlsx")


restores %>%
  select(patient_id, pt_group, visit, weight) %>%
  ggplot(aes(x = visit, y = weight, col = pt_group)) +
  geom_point(alpha = 0.20, shape = 21) +
  geom_smooth(se = FALSE) +
  theme_bw()


ggdata_points <- restores %>%
  select(patient_id, pt_group, visit, weight)

ggdata_smry <- ggdata_points %>%
  group_by(pt_group, visit) %>%
  summarise(
    mean_value = mean(weight, na.rm = TRUE),
    se_value = sd(weight, na.rm = TRUE) / sqrt( n() )
  )

ggplot() +
  geom_point(
    data = ggdata_points,
    aes(x = visit, y = weight, col = pt_group),
    position = position_jitterdodge(
      dodge.width = 1/3,
      jitter.width = 1/24
    )
  ) +
  geom_point(
    data = ggdata_smry,
    size = 3,
    aes(x = visit, y = mean_value, col = pt_group),
    position = position_jitterdodge(
      dodge.width = 1/3,
      jitter.width = 1/24
    )
  ) +
  geom_line(
    data = ggdata_smry,
    aes(x = visit, y = mean_value, col = pt_group),
    position = position_jitterdodge(
      dodge.width = 1/3,
      jitter.width = 1/24
    )
  )
