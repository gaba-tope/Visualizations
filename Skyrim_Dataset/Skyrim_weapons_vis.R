## Import library-----------------------------------------
library(tidyverse)
library(ggpmisc)
library(reshape2)
library(forcats)
library(showtext)
library(ggtext)
library(ggrepel)
library(treemapify) # treemap on ggplot2
library(rvest) # HTML 
library(ggpubr) # depends on {cowplot} and {gridExtra}
library(jpeg) # Import .jpeg file
library(png)
library(grid)
library(svglite)
library(scales)
library(socialcap) # My custom-package for adding social media handles to ggplot caption

options(scipen = 999) # Disable scientific notation.


## Fonts---------------------------------------------------
font_add_google(name = "Oswald", family = "oswald")
# Sansserifexbflfcond-jLXR.otf & Sansserifbldflfcond-5OpB.otf
# Designed by 'Casady & Greene', Public Domain
# https://www.fontspace.com/sansserifflf-font-f7810 
sysfonts::font_add(family = "sansserif-exb", regular = "./Sansserifexbflfcond-jLXR.otf")
sysfonts::font_add(family = "sansserif-bld", regular = "./Sansserifbldflfcond-5OpB.otf")
showtext_auto()

title_font <- "sansserif-exb"
main_font <- "oswald"
try_font <- "sansserif-bld"
sysfonts::font_families()

## Colors -------------------------------------------------
#bg_col <- "#eeeeee"
bg_col <- "black"
#text_col <- "grey10"
text_col <- "#d6d6d6"

skyrim_grey <- "#72716F"
major_grid_col <- "#bebebe"
minor_grid_col <- "#d6d6d6"
# Okabe-Ito Palette
orange <- "#E69F00" 
lightblue <- "#56B4E9" 
blue <- "#0272B2"
dark_orange <- "#D55E00"
green <- "#059D73"


display.brewer.all()

## Data Import---------------------------------------------
 # https://www.kaggle.com/datasets/elmartini/skyrim-weapons-dataset/data 
 # CC0 Public Domain
skyrim <- read.csv("Skyrim_Weapons.csv", header = T) |> as_tibble()
skyrim

# From https://en.uesp.net/wiki/Skyrim:Unobtainable_Items 
# url_uesp <- "https://en.uesp.net/wiki/Skyrim:Unobtainable_Items"
# html_uesp <- read_html(url_uesp, encoding = "UTF-8")
# unobtain_weapon <- html_nodes(x = html_uesp, xpath = '//*[@id="mw-content-text"]/div/table[1]') |> 
#   html_table()
# unobtain_weapon <- unobtain_weapon[[1]]
# saveRDS(unobtain_weapon, file = "./unobtain_weapon.rds")

unobtain_weapon <- readRDS("./unobtain_weapon.rds")

# Import background image
# https://unsplash.com/ko/%EC%82%AC%EC%A7%84/%EC%82%B0-%EC%95%8C%ED%94%84%EC%8A%A4%EC%9D%98-%ED%92%8D%EA%B2%BD-%EC%82%AC%EC%A7%84-vddccTqwal8
# Unsplash License
bg_img <- readJPEG("./samsommer_mount_unsplash.jpg")
bg_img_p <- writePNG(bg_img)
bg_img_pr <- readPNG(bg_img_p)



if (!(dim(bg_img_pr)[3] == 4)) {
  # Create an alpha layer with full opacity (1)
  alpha_layer <- array(1, dim = c(dim(bg_img_pr)[1], dim(bg_img_pr)[2]))
  
  # Add the alpha layer to the image
  bg_img_pr <- abind::abind(bg_img_pr, alpha_layer, along = 3)
}

bg_img_pr[,,4] <- bg_img_pr[,,4] * 0.2

bg_img_pr_grob <- rasterGrob(bg_img_pr)


ggplot(mtcars, aes(x=wt, y=mpg)) +
  annotation_custom(bg_img_pr_grob, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  geom_point()

## Data Wrangling -----------------------------------------
# Add corresponding levels for the duplicated item in skyrim$Name:
dup_name_ind <- duplicated(select(skyrim, Name)) | 
  duplicated(select(skyrim, Name), fromLast = TRUE)

dup_name <- skyrim[dup_name_ind, "Name"] |> as.vector() |> unlist() 
level <- c("1-11", "12-18", "19-26", "27-35", "36+", "11-18", "19-26",
           "27-35", "36-45", "46+")
skyrim[dup_name_ind, "Name"] <- glue::glue("{dup_name} (Lvl {level})")

# Remove trailing whitespaces from skyrim$Name:
skyrim$Name <- str_trim(skyrim$Name, side = "right")

# Remove (DR|DG)$ from skyrim$Name
skyrim$Name <- str_replace(skyrim$Name, "\\s(DR|DG)$", "")

# Remove unobtainable weapon from skyrim based on `unobtain_weapon` dataset.
unobtain_weapon_cl <- unobtain_weapon |> select(`Name (ID)`, Type)
unobtain_weapon_cl <- mutate(unobtain_weapon_cl, #Remove `(code)`
                             Name = str_extract(`Name (ID)`, "^[^\\(]+")) 
unobtain_weapon_cl$Name <- str_replace(unobtain_weapon_cl$Name, 
                                       "\\s?(CC|DB|DG)$", "") # Remove (CC|DG|DB)$

matches <- skyrim$Name %in% unobtain_weapon_cl$Name
skyrim[matches, ]

skyrim_use <- skyrim |> filter(!(skyrim$Name %in% unobtain_weapon_cl$Name))

# Exploratory Linear Realationship Plot: Damage, Gold, Weight variables per Type of Weapons
DWG <- skyrim_use |> select(Type, Damage, Weight, Gold)
grouped_DWG <- DWG |> split(DWG$Type)

combi <- tribble(~V1, ~V2,
                 "Weight", "Damage",
                 "Damage", "Gold",
                 "Weight", "Gold")

scatter_fun <-  function(Var1, Var2) {
    ggplot(data = DWG, 
           aes(x = .data[[Var1]], y = .data[[Var2]])
    ) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "grey74")+
      stat_poly_eq(use_label(c("eq", "R2")),
                   size = 2)+
      facet_wrap(~Type)+
      theme_bw()
    }
  
corplots <- pmap(combi, ~scatter_fun(Var1 = .x, Var2 = .y))
corplots[[1]]

pdf("all_scatterplots.pdf") # Save the whole plots as pdf.
corplots
dev.off()

# Exploratory Damage Bar Plot
bar_dam_fun <- function(ty){
  d <- skyrim_use |> 
    filter(Type == ty)
  d <- d |> arrange(desc(Damage)) |> 
    mutate(Name = factor(Name, levels = Name))
  
  ggplot() +
    geom_bar(data = d, aes(x = Name, y = Damage),
             stat = "identity") +
    ggtitle(label = paste("Type : ", ty)) +
    theme(axis.text.x = element_text(angle = 90))
}

Type_name_dam <- skyrim_use$Type |> unique()
dam_plots <- map(Type_name_dam, bar_dam_fun)

pdf("all_dam_plots.pdf")
dam_plots
dev.off()

# Exploratory Cost-efficiency Bar Plot (Damage/Gold)
cost_eff <- skyrim_use |> 
  filter(Weight != 0 & Gold != 0) |>
  mutate(Damage_per_gold = Damage / Gold) |>
  arrange(desc(Damage_per_gold))

bar_cost_fun <- function(ty){
  d <- cost_eff |> 
    filter(Type == ty)
  d <- d |> 
    arrange(desc(Damage_per_gold)) |> 
    mutate(Name = factor(Name, levels = Name))
  
  ggplot() +
  geom_bar(data = d, aes(x = Name, y = Damage_per_gold),
           stat = "identity") +
  ggtitle(label = paste("Type : ", ty)) +
  theme(axis.text.x = element_text(angle = 90))
  }
Type_name_cost <- cost_eff$Type |> unique()
cost_plots <- map(Type_name_cost, bar_cost_fun)

cost_plots[[2]]

pdf("all_costplots.pdf") # Save the whole plots as pdf.
cost_plots
dev.off()

## Texts --------------------------------------------------
social_caption <- socialcap(gitname = "gaba-tope", twitname = "@tope_ezia",
                            textcol = text_col, iconcol = text_col)

title_bar <- "Exploring the Arsenal of Skyrim:"
subtitle_bar <- "The Number of Weapons per Type"
title_tree <- "From Swords to Silverware:"
subtitle_tree <- " The Percentage Panorama of Skyrim's Weapons"
cap_skyrim <- paste0(
  "**Data**: Skyrim Weapons Dataset (ElMartian in Kaggle)<br>**Graphic**: Tope ",
  social_caption)

## Themes -------------------------------------------------
# bar theme -----------------------------------------------
bar_theme <- theme(
  plot.title.position = "plot", # Default position is right above the plot
  plot.caption.position = "plot", # Default position is right below the plot
  plot.background = element_rect(fill = bg_col, colour = bg_col),
  panel.background = element_rect(fill = bg_col, colour = bg_col),
  panel.grid.major = element_line(color = skyrim_grey),
  panel.grid.minor = element_blank(),
  plot.margin = margin(10, 10, 10, 10),
  plot.title = element_textbox_simple(
    colour= text_col,
    face = "bold",
    family = title_font,
    lineheight = 0.5,
    size = 50,
    margin = margin(b = 2, t = 2) # respect to title's default position
  ),
  plot.subtitle = element_textbox_simple(
    colour = text_col,
    family = main_font,
    size = 35,
    margin = margin(b = 10) # respect to subtitle's default position
  ),
  plot.caption = element_textbox_simple(
    size = 20,
    colour = text_col,
    lineheight = 0.5,
    family = main_font,
    margin = margin(t = 13, b = 5, l = 5), # respect to caption's default position
  ),
  axis.line = element_line(colour = skyrim_grey),
  axis.text.x = element_text(size = 20, 
                             family = main_font,
                             margin = margin(t = 5),
                             colour = text_col),
  axis.text.y = element_markdown(size = 20,#,
                             family = main_font,
                             colour = text_col,
                             hjust = 1,
                             margin = margin(r = 5, l = -5)),
  axis.title.x = element_text(size = 30, 
                              family = main_font,
                              colour = text_col,
                              margin = margin(t = 3)),
  axis.title.y = element_text(size = 30,
                              family = main_font,
                              colour = text_col,
                              margin = margin(r = 10))
)

# tree map theme ------------------------------------------------
treemap_theme <- theme(
  plot.title.position = "plot", # Default position is right above the plot
  plot.caption.position = "plot", # Default position is right below the plot
  legend.position = "none",
  plot.background = element_rect(fill = bg_col, colour = bg_col),
  panel.background = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  plot.margin = margin(10, 10, 10, 10),
  plot.title = element_textbox_simple(
    colour= text_col,
    face = "bold",
    family = title_font,
    lineheight = 0.5,
    size = 50,
    margin = margin(b = 2, t = 2) # respect to title's default position
  ),
  plot.subtitle = element_textbox_simple(
    colour = text_col,
    family = main_font,
    size = 40,
    margin = margin(b = 10) # respect to subtitle's default position
  ),
  plot.caption = element_textbox_simple(
    size = 20,
    colour = text_col,
    lineheight = 0.5,
    family = main_font,
    margin = margin(t = 13, b = 5, l = 5), # respect to caption's default position
  ),
  axis.text = element_blank(),
  axis.title = element_blank(),
  axis.ticks = element_blank()
  )
# bar + tree combined plot theme ----------------------------
bt_com_theme <- theme(
  plot.background = element_rect(fill = bg_col, colour = bg_col),
  plot.title = element_textbox_simple(
    colour= text_col,
    face = "bold",
    family = title_font,
    lineheight = 0.5,
    size = 50,
    margin = margin(b = 2, t = 2) # respect to title's default position
  ),
  plot.subtitle = element_textbox_simple(
    colour = text_col,
    family = main_font,
    size = 35,
    margin = margin(b = 10) # respect to subtitle's default position
  ),
  plot.caption = element_textbox_simple(
    size = 20,
    colour = text_col,
    lineheight = 0.5,
    family = main_font,
    margin = margin(t = 13, b = 5, l = 5), # respect to caption's default position
  ),
  plot.margin = margin(10, 10, 10, 10, unit = "pt"),
  axis.text.y = element_markdown(size = 20,
                                 family = main_font,
                                 colour = text_col)
)


## Plots --------------------------------------------------
# Bar Plot of Frequency per Type ---------------------------
y_axis_text <- skyrim_use$Type |> fct_infreq() |> fct_rev() |>unique() |> levels()

y_axis_text <- y_axis_text |> sapply(function(x) {ifelse(x == "Sword",
                                                  #paste("<span style = 'font-weight:bold'>", x,"</span>"),
                                                  glue::glue("**{x}**"),
                                                  glue::glue("*{x}*")
                                                  )                                                 
                                                  #paste("<span style = 'font-weight:plain'>", x,"</span>"))
                                    }
                              )

y_axis_text <- y_axis_text |> as.vector()

type_freq_plot <- ggplot(skyrim_use) +
  annotation_raster(bg_img_pr, xmin = -Inf, xmax = Inf,
                    ymin = -Inf, ymax = Inf) +
  geom_bar(aes(y = fct_rev(fct_infreq(Type))),
           fill = lightblue, alpha = 0.8) +
  scale_y_discrete(labels = y_axis_text) +
  labs(title = title_bar,
       subtitle = subtitle_bar,
       caption = cap_skyrim,
       x = "Number",
       y = "Type of Weapons") 


type_bar_plot <- type_freq_plot + bar_theme

ggsave(file = "Type_freq.png", plot = type_bar_plot, 
       width = 254, height = 229, units = "mm",
       dpi = 100)
# Bar plot w/o title, subtitle and caption ----------------
type_freq_plot_only <- ggplot(skyrim_use) +
  annotation_raster(bg_img_pr, xmin = -Inf, xmax = Inf,
                    ymin = -Inf, ymax = Inf) +
  geom_bar(aes(y = fct_rev(fct_infreq(Type))),
           fill = lightblue) +
  scale_y_discrete(labels = y_axis_text) +
  labs(x = "Number",
       y = "Type of Weapons") +
  bar_theme

ggsave(file = "Type_freq_only.png", plot = type_freq_plot_only, 
       width = 254, height = 229, units = "mm",
       dpi = 100)

# Treemap ---------------------------------------
Type_Freq <- unlist(skyrim_use$Type) |>
  table() |>
  as.data.frame() |>
  arrange(desc(Freq))
names(Type_Freq)[[1]] <- "Type"

Type_Cate <- skyrim_use[ ,c(7,8)] |> distinct() 
Type_Freq_Cate <- merge(Type_Freq, Type_Cate, by = "Type")
Ty_Fr_Ca_Pr <- Type_Freq_Cate |>
  mutate(Prop = paste(Type, "\n(",
                      round(Freq / sum(Type_Freq_Cate$Freq) * 100, digits = 1), "%",
                      ")"))

type_treemap_plot <- ggplot(Ty_fr_ca_pr_dam,
                            aes(area = Freq, subgroup = Category)) + 
  annotation_raster(bg_img, xmin = -Inf, xmax = Inf,
                    ymin = -Inf, ymax = Inf) +
  geom_treemap(start = "topleft",
               colour = skyrim_grey,
               fill = "black",
               size = 1, 
               alpha = 0.8) +
  geom_treemap_text(start = "topleft",
                    label = Type_Freq_Cate_Prop$Prop, 
                    colour = text_col,
                    place = "centre",
                    size = 20,
                    family = main_font) + 
  geom_treemap_subgroup_border(start = "topleft",
                               colour = skyrim_grey,
                               size = 5) + 
  geom_treemap_subgroup_text(start = "topleft",
                             place = "centre", grow = T,
                             alpha = 0.25, colour = text_col,
                             min.size = 10,
                             fontface = "italic",
                             family = title_font) +
  labs(title = title_tree,
       subtitle = subtitle_tree,
       caption = cap_skyrim) + 
  treemap_theme


ggsave(file = "type_treemap_plot.png", plot = type_treemap_plot, 
       width = 260, height = 235, units = "mm",
         dpi = 100)

# Combined Plots -------------------------------
library(patchwork)

combined_plot <- plot_grid(type_freq_plot_only + type_treemap_plot) +
  plot_layout(guides = 'collect') + 
  plot_annotation(
    title = title_bar,
    subtitle = subtitle_bar,
    caption = cap_skyrim,
    theme = bt_com_theme
  )

combined_plot

ggsave(file = "combined.png", plot = combined_plot, 
       width = 1600, height = 1000, units = "px",
       dpi = 100)

ggsave(file = "combined.png", plot = combined_plot, 
       width = 400, height = 229, units = "mm",
       dpi = 100)

ggsave(file="test.svg", plot = combined_plot, 
       width = 400, height = 229, units = "mm")
