
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
    path = "C:/Users/ngreen1/Google Drive/Pakistan training course/data/List of Env Samples 2015-2019.xlsx",
    range = "A2:F58", 
    sheet = "Sheet1") #, sheet = 1)

dat$YRONSET <- zoo::na.locf(dat$YRONSET)

dat <-
  dat %>% 
  rowwise() %>% 
  mutate(check_total = sum(Positive, Negative, `Under Process`, na.rm = TRUE) == `Grand Total`)

any(!dat$check_total)
              
dat <- dat %>% select(-"Grand Total", -check_total)

x <- melt(dat, id.vars = c("YRONSET", "MONTH"))
x$month_year <- paste(x$MONTH, x$YRONSET)


#########
# plots #
#########

#TODO: change colour scheme

ggplot(x, aes(x = month_year, y = value, fill = variable)) +
  geom_bar(position = "fill", stat = "identity") +
  scale_y_continuous(labels = percent_format()) +
  scale_x_discrete(breaks = x$month_year, labels = x$MONTH,
                   expand = c(0.1,0.1)) +
  facet_grid(~ YRONSET, space = "free_x", scales = "free_x", switch = "x") + 
  theme_bw() +
  theme(strip.placement = "outside",
        strip.background = element_rect(fill = NA, colour = "green"),
        panel.spacing = unit(0,"cm")) +
  ggtitle("Environmental Sampling Results 2015-19*  pakistan") +
  xlab("") +
  geom_text(data = x, aes(y = value, label = value), position = position_fill(vjust = 0.5))
