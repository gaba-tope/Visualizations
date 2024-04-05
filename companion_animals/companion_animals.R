## Import library-----------------------------------------
library(tidyverse)
library(dplyr)
library(showtext)
library(ggtext)
library(ggplot2)
library(waffle)   #remotes::install_github("hrbrmstr/waffle")
library(socialcap) 


## Data Import---------------------------------------------
# 통계청,「인구총조사」, 2020, 2024.04.04, 거처의종류별/반려동물보유유형별가구-시도
# https://kosis.kr/statHtml/statHtml.do?orgId=101&tblId=DT_1PH2011&conn_path=I2
raw <- read.csv("companion_animals.csv", header = T, na = "-")


## Data Wrangling----------------------------------
# Name columns
data <- raw |> as_tibble()
names(data) <- data[1, ]
data <- data[-1, ]
names(data) <- c("District", "HouseType", "HouseTotal", "HouseWOAnimals","HouseWAnimals",
                 "SingleTotal", "SingleDog", "SingleCat", "SingleEtc",
                 "MultiTotal", "MultiDogCat", "MultiDogEtc", "MultiCatEtc", "MultiDogCatEtc")
# Convert chr to num
data[, -c(1,2)] <- data[, -c(1,2)] |> lapply(as.integer)
# Select and Add Columns and Rows
d_f <- data |> 
  dplyr::filter(`HouseType` == "계") |> 
  rowwise() |> 
  mutate(total_dog = SingleDog + MultiDogCat + MultiDogEtc + MultiDogCatEtc,
         total_cat = SingleCat + MultiDogCat + MultiCatEtc + MultiDogCatEtc,
         total_etc = SingleEtc + MultiDogEtc + MultiCatEtc + MultiDogCatEtc) |> 
  mutate(dog_per_100 = (total_dog/HouseTotal)*100,
         cat_per_100 = (total_cat/HouseTotal)*100,
         etc_per_100 = (total_etc/HouseTotal)*100,
         animal_per_100 = (HouseWAnimals/HouseTotal)*100)
# To see if there is a NA
complete.cases(d_f)

# tibble for waffle plot1
tib_waffle <- d_f |> 
  select(District, dog_per_100, cat_per_100) |> 
  filter(District == "전국") |> 
  mutate(house_wo_dog_100 = 100 - dog_per_100,
         house_wo_cat_100 = 100 - cat_per_100) |> 
  pivot_longer(cols = !District)

tib_waffle$animal <- c("dog", "cat", "dog", "cat")

# tibble for waffle plot2
tib_waffle2 <- d_f |> 
  select(District, dog_per_100, cat_per_100, etc_per_100) |> 
  filter(District == "전국") |> 
  mutate(house_wo_ani_per_100 = 100 - dog_per_100 - cat_per_100 - etc_per_100) |> 
  pivot_longer(cols = !District) |> 
  mutate(value_r = round(value, digits = 0))
  

## Fonts & Colors -------------------------------------------------
font_add_google(name = "Oswald", family = "oswald")
font_add_google(name = "IBM Plex Sans KR", 
                regular.wt = 400,
                bold.wt = 700 ,
                family = "ibm")
sysfonts::font_add(family = "font-awesome", regular = "Font Awesome 6 Free-Solid-900.otf")
showtext_auto()
main_font <- "ibm"

bg_col <- "#eeeeee"
text_col <- "grey10"
major_grid_col <- "#bebebe"
minor_grid_col <- "#d6d6d6"
pal <- c("#5992B5", "#937455", "#bebebe","#bebebe" )
pal2 <- c("#5992B5", "#937455", "#7ea57e", "#bebebe")


## Texts --------------------------------------------------
social_caption <- socialcap(gitname = "gaba-tope", twitname = "@tope_ezia", textfont = "ibm")
title <- "멍멍 냐옹 찍찍 반려동물과 함께하는 삶"
subtitle <- "반려동물과 함께 사는 가구 수 (100가구당)"
expl <- paste0("우리 통계청은 **2020년에 실시한 인구총조사**에서 처음으로 **반려동물 양육 여부** 항목을
포함하여 표본조사를 실시하였습니다. ",
"전체 표본 가구 중 **14.95%의 가구에서 반려동물을** 기르고 있었으며,
 그 중 <span style = 'color:#5992B5;'>**3.43%의 가구에서 고양이를**</span>, 
<span style = 'color:#937455;'>**11.6%의 가구에서 개를**</span> 기르고 있었습니다. ")
expl2 <- paste0("우리 통계청은 **2020년에 실시한 인구총조사**에서 처음으로 **반려동물 양육 여부** 항목을
포함하여 표본조사를 실시하였습니다. ",
               "전체 표본 가구 중 **14.95%의 가구에서 반려동물을** 기르고 있었으며,
 그 중 <span style = 'color:#5992B5;'>**3.43%의 가구에서 고양이를**</span>, 
<span style = 'color:#937455;'>**11.6%의 가구에서 개를**</span>, 
<span style = 'color:#7ea57e;'>**0.72%의 가구에서 기타 반려동물을</span>** 기르고 있었습니다. ")
caption <- paste0(expl,"<br><br>**자료**: 통계청,「인구총조사」, 2020. 거처의종류별/반려동물보유유형별가구-시도.
                  <br>**만든이**: ", social_caption)
caption2 <- paste0(expl2,"<br><br>**자료**: 통계청,「인구총조사」, 2020. 거처의종류별/반려동물보유유형별가구-시도.
                  <br>**만든이**: ", social_caption)


## Themes -------------------------------------------------
main_theme <- theme(
  plot.title.position = "plot", # plot title 기본 위치가 plot 바로 위.
  plot.caption.position = "plot",# plot title 기본 위치가 plot 바로 아래.
  plot.background = element_rect(fill = bg_col, colour = bg_col),
  panel.background = element_rect(fill = bg_col, colour = bg_col),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  plot.margin = margin(10, 10, 10, 10),
  plot.title = element_textbox_simple(
    colour= text_col,
    face = "bold",
    family = main_font,
    lineheight = 0.5,
    size = 27,
    margin = margin(b = 5, t = 2) 
  ),
  plot.subtitle = element_textbox_simple(
    colour= text_col,
    family = main_font,
    face = "bold",
    lineheight = 0.5,
    size = 20,
    margin = margin(b = 10, t = 2) 
  ),
  plot.caption = element_textbox_simple(
    colour= text_col,
    lineheight = 1.2,
    family = main_font,
    margin = margin(t = 13, b = 5, l = 5),
    size = 15
  ),
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  strip.background = element_rect(fill = bg_col,
                                  colour = bg_col),
  strip.text = element_textbox_simple(colour = text_col,
                                      face = "bold",
                                      family = main_font,
                                      size = 20,
                                      margin = margin(t = 10, l = 73, b = -1))
)


# Plot1: w vs. w/o dog or cat --------------------
plot_waffle <- ggplot(data = tib_waffle, 
                      aes(fill = name, values = value)) +
  geom_pictogram(n_rows = 10,
                 aes(colour = name,
                     label = name),
                 family = "font-awesome",
                 make_proportional = TRUE,
                 flip = TRUE) +
  scale_label_pictogram(
    name = NULL,
    values = c("cat", "dog", "cat", "dog"),
    labels = c("cat_per_100", "dog_per_100", "house_wo_cat_100", "house_wo_dog_100")
  ) +
  facet_wrap(~animal, ncol = 2,
             labeller = labeller(animal = c("cat" = "<span style = 'color:#5992B5;'>고양이</span>와 사는 가구 수",
                                            "dog" = "<span style = 'color:#937455;'>개</span>와 사는 가구 수"))
             ) +
  scale_color_manual(values = pal,
                     labels = c("cat_per_100", "dog_per_100",
                                "house_wo_cat_100", "house_wo_dog_100")
                     ) +
  guides(colour = "none", label = "none") +
  coord_equal()+
  labs(title = title,
       subtitle = subtitle,
       caption = caption) +
  main_theme

plot_waffle


## Save Plot1 ---------------------------------------
ggsave(
  filename = "waffle.svg",
  plot = plot_waffle,
  device = "svg",
  units = "px",
  width = 2900,
  height = 2300,
  dpi = 300
)

rsvg::rsvg_png(
  "waffle.svg", "waffle.png",
  width = 2900, height = 2300
)


## Plot2: Combine Two Plots into One Waffle
plot_waffle_combine <- ggplot(data = tib_waffle2, 
                      aes(fill = name, values = value_r)) +
  geom_pictogram(n_rows = 10,
                 aes(colour = name,
                     label = name),
                 family = "font-awesome",
                 make_proportional = TRUE,
                 flip = TRUE) +
  scale_label_pictogram(
    name = NULL,
    values = c("cat", "dog", "dove", "home"),
    labels = c("cat_per_100", "dog_per_100", "etc_per_100", "house_wo_ani_per_100")
  ) +
  scale_color_manual(values = pal2,
                     labels = c("cat_per_100", "dog_per_100", "etc_per_100", "house_wo_ani_per_100")
  ) +
  guides(colour = "none", label = "none") +
  coord_equal()+
  labs(title = title,
       subtitle = subtitle,
       caption = caption2) +
  main_theme

plot_waffle_combine


## Save Plot2--------------------------------
ggsave(
  filename = "waffle_c.svg",
  plot = plot_waffle_combine,
  device = "svg",
  units = "px",
  width = 2800,
  height = 3000,
  dpi = 290
)

rsvg::rsvg_png(
  "waffle_c.svg", "waffle_c.png",
  width = 2800, height = 3000
)
