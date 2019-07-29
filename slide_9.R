
#' ---
#' title: "Pakistan training course"
#'
#' author: "N Green"
#' date: "`r format(Sys.Date())`"
#' output:
#'   html_document:
#'     keep_md: TRUE
#' ---


library(dplyr)
library(zoo)
library(reshape2)
library(ggplot2)
library(scales)


##############
# preprocess #
##############

# dat <- read_excel(file.choose())
dat <-
  readxl::read_xlsx(
    path = "C:/Users/ngreen1/Google Drive/Pakistan training course/data/MPQA _March SNID.xlsx")
              
x <- dat %>% select(-DESK, -FIELD)

x$prov_district <- paste(x$"Province name", x$"District name")


#########
# plots #
#########

#TODO: change colour scheme

ggplot(x, aes(x = prov_district, y = Overall)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  geom_text(aes(label = sprintf("%d%%", Overall)), vjust = 0, angle = -90, nudge_y = -5) +
  scale_x_discrete(breaks = x$prov_district, labels = x$"District name",
                   expand = c(0.1,0.1)) +
  facet_grid(~ `Province name`, space = "free_x", scales = "free_x", switch = "x") + 
  theme_bw() +
  theme(strip.placement = "outside",
        strip.background = element_rect(fill = NA, linetype = 0),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        panel.spacing = unit(0,"cm")) +
  labs(subtitle =
         "- Micro Plans of Tier-1 Districts from Karachi town, Quetta Block and Peshawar/Khyber are passed\n- Substantial gaps identified primarily in Balochistan and pockets of KP and Sindh") +
  ggtitle("SUMMARY OF MPQA RESULTS", ) +
  xlab("") + ylab("") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  # geom_text(data = x, aes(y = value, label = value), position = position_fill(vjust = 0.5))
