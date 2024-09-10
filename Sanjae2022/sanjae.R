# Load Library
library(tidyverse)
library(showtext)
library(systemfonts)
library(ggtext)
library(ggsci)
library(ggrepel)

## Import Dataset ---------------------------
raw <- read.csv("./Jaehae_type_sanup.csv")

## Fonts and Colors
font_add_google(name = "Oswald", family = "oswald")
font_add_google(name = "Noto Sans KR", family = "noto")
showtext_auto()
main_font <- "noto"

font <- system_fonts()
font

c25 <- c(
  "dodgerblue2", "#E31A1C", # red
  "green4",
  "#6A3D9A", # purple
  "#FF7F00", # orange
  "black", "gold1",
  "skyblue2", "#FB9A99", # lt pink
  "palegreen2",
  "#CAB2D6", # lt purple
  "#FDBF6F", # lt orange
  "gray70", "khaki2",
  "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
  "darkturquoise", "green1", "yellow4", "yellow3",
  "darkorange4", "brown"
)

## Text ---------------------------
social <- socialcap::socialcap(gitname = "gaba-tope", twitname = "@tope_ezia",
                               textfont = main_font)
caption <- paste0("Data: 고용노동부,「산업재해현황」, 2022, 2024.09.10, 전체 재해 현황 및 분석-발생형태별(산업별 중분류)", "<br>Graphic: ", social)

## Data Wrangling --------------------------- Tidy Data / Name / Data type / NA
df <- raw 
# Name
colnames(df) <- df[1, ]
df <- df[-1, ]
colnames(df)[c(1,2)] <- c("type1", "type2")
# Tidy
df <- df |> 
  pivot_longer(-c(type1, type2), names_to = "injury")
# Data Type 
df <- df |> 
  mutate_at(c("type1", "type2", "injury"), as.factor) |> # Select specific variable to apply function.
  mutate_at("value", as.numeric)



# 산업 중분류별 재해 현황 (midtype)
midtype_df <- df |> 
  dplyr::filter(type1 != "총계" & type2 == "소계" & injury == "계") |> 
  select(type1, value)

midtype_plot <- ggplot(data = midtype_df) + 
  geom_col(aes(x = value, y = fct_reorder(type1, value), fill = value)) +
  guides(fill = "none") +
  labs(x = "재해 발생 (명)", y = "산업중분류") +
  theme_minimal()

midtype_plot  

# 광업 분류별 재해 현황 (mineral)
mineral_df <- df |> 
  dplyr::filter(type1 == "광업", type2 != "소계" & injury == "계") |>
  select(type2, value) |> 
  mutate(hsize = 4, percent = value/sum(value))
  
mineral_plot <- ggplot(data = mineral_df) +
  geom_col(aes(x = hsize, y = value, fill = type2), color = "black") + 
  geom_text(aes(x = hsize, y = value, label = scales::percent(percent, accuracy = .1)),
            hjust = c(-1, 1), vjust = c(10, -1)
            ) +
  scale_fill_brewer(palette = "Pastel1") +
  guides(fill = guide_legend(title = "광업 소분류")) +
  labs(x = "", y = "") +
  coord_polar(theta = "y") + 
  xlim(c(3, 4.5)) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        panel.grid = element_blank()
        )
  
mineral_plot
ggsave(filename = "mineral_plot.svg", plot = mineral_plot, 
       device = "svg", width = 1500, height = 1500, units = "px")


# 산업군별 재해형태의 현황
type_injury_df <- df |> 
  dplyr::filter(type1 != "총계" & type2 == "소계" & injury != "계" & type1 != "기타의 사업") |> 
  select(type1, injury, value) |>
  group_by(type1) |> 
  mutate(percent = value / sum(value), total = sum(value), hsize = 4)

type_injury_plot <- ggplot(data = type_injury_df) +
  geom_col(aes(x = value, y = injury, fill = value)) +
  facet_wrap(~fct_reorder(type1, -total), ncol = 5) +
  scale_fill_gradient(low = "black", high = "red") +
  guides(fill = guide_legend(title = "재해 발생 건수 (명)", reverse = T)) +
  labs(x = "재해 발생 건수 (명)", y = "재해 유형", title = "산업군별 산업재해 발생 건수") +
  theme_minimal()

type_injury_plot

# 산업군별 재해유형 발생 비율
type_injury_pie <- ggplot(data = type_injury_df) +
  geom_col(aes(x = hsize, y = percent, fill = injury), color = "black") +
  facet_wrap(~fct_reorder(type1, -total), ncol = 5) +
  scale_fill_manual(values = c25) +
  guides(fill = guide_legend(title = "재해 발생 비율")) +
  coord_polar(theta = "y") +
  labs(title = "산업군별 산업재해 유형 비율") +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank())

type_injury_pie


# 산업군별 높은 비율 5가지 
type_injury_top <- type_injury_df |> 
  arrange(type1, desc(percent)) |> 
  group_by(type1) |> 
  top_n(5, percent) |> 
  mutate(label = paste0(injury, as.character(formatC(percent * 100, digits = 2)),
                        "%"))

type_injury_top2 <- type_injury_top |> 
  group_by(type1) |> 
  top_n(2, percent)

# others <- type_injury_top |>
#   group_by(type1) |>
#   mutate(others = 1 - sum(percent)) |>
#   select(type1, others) |>
#   distinct() |>
#   mutate(injury = as.factor("기타"))
# 
# add_others <- others
# colnames(add_others) <- c("type1", "percent", "injury")
# add_others

type_injury_top_other <- rbind(type_injury_top, add_others) |>
  arrange(type1, desc(percent)) |>
  mutate(level = ifelse(injury == "기타", 0, percent))

type_injury_top_plot <- ggplot() +
  geom_col(data = type_injury_top,
           aes(x = hsize, y = percent, fill = injury),
           color = "black") +
  geom_label_repel(data = type_injury_top2,
                  min.segment.length = 0,
                  aes(x = hsize, y = percent, label = label),
                size = 3, nudge_x = c(0, .35),
             family = main_font) +
  facet_wrap(~fct_reorder(type1, -total), ncol = 5,
             labeller = labeller(type1 = label_wrap_gen)) +
  scale_fill_brewer(palette = "Paired") +
  guides(fill = guide_legend(title = "유형별 비율 (%)", reverse = T)) +
  labs(x = "재해 발생 비율 (%)", y = "재해 유형",
       title = "재해 유형별 발생 비율 - 상위 5개 유형",
       caption = caption) +
  coord_polar(theta = "y") +
  xlim(c(3, 4.5)) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(family = main_font, face = "bold"),
        legend.title = element_text(family = main_font, face = "bold"),
        legend.text = element_text(family = main_font),
        plot.caption = element_textbox(family = main_font,
                                       hjust = 0),
        strip.text = element_text(family = main_font, size = 10))

type_injury_top_plot

ggsave(filename = "type_injury_top_plot.svg", plot = type_injury_top_plot,
       device = "svg", width = 1200, height = 600, units = "px",
       dpi = 150)

