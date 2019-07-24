
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
  geom_bar(stat = "identity", fill = "green") +
  scale_x_discrete(breaks = x$prov_district, labels = x$"District name",
                   expand = c(0.1,0.1)) +
  facet_grid(~ `Province name`, space = "free_x", scales = "free_x", switch = "x") + 
  theme_bw() +
  theme(strip.placement = "outside",
        strip.background = element_rect(fill = NA, colour = "green"),
        panel.spacing = unit(0,"cm")) +
  ggtitle("SUMMARY OF MPQA RESULTS") +
  xlab("") + ylab("") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) #+
  # geom_text(data = x, aes(y = value, label = value), position = position_fill(vjust = 0.5))
