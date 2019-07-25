
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
dat <- dat[-nrow(dat), ]

dat$month_year <- paste(dat$MONTH, dat$YRONSET)
# dat$month <- factor(dat$month, ordered = TRUE)
dat$month_year <- factor(dat$month_year, levels = dat$month_year, ordered = TRUE)

x <- melt(dat, id.vars = c("YRONSET", "MONTH", "month_year"))
x$variable <- factor(x$variable,
                     levels = c("Positive", "Negative", "Under Process"))

x$variable <- relevel(x$variable, 'Negative')


#########
# plots #
#########

#TODO: change colour scheme

ggplot(x, aes(x = month_year, y = value, fill = variable)) +
  scale_fill_manual("legend", values = c("Negative" = "lightgreen", "Positive" = "red", "Under Process" = "grey")) +
  geom_bar(position = "fill", stat = "identity") +
  scale_y_continuous(labels = percent_format(),
                     breaks = seq(from = 0, to = 1, by = 0.1)) +
  scale_x_discrete(breaks = x$month_year, labels = x$MONTH,
                   expand = c(0.1,0.1)) +
  facet_grid(~ YRONSET, space = "free_x", scales = "free_x", switch = "x") + 
  theme_bw() +
  theme(strip.placement = "outside",
        strip.background = element_rect(fill = NA, colour = "green"),
        panel.spacing = unit(0,"cm")) +
  ggtitle("Environmental Sampling Results 2015-19* \nPakistan") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") + ylab("") +
  geom_text(data = x, aes(y = value, label = value), position = position_fill(vjust = 0.5)) +
  theme(legend.position = "bottom", legend.title = element_blank())
