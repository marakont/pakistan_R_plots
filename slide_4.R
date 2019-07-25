
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
library(ggplot2)


##############
# preprocess #
##############

dat <- readxl::read_xlsx("C:/Users/ngreen1/Google Drive/Pakistan training course/data/List of AFP Cases 2015-2019.xlsx", sheet = "Data")

dat$month <- lubridate::month(dat$DENTER, label = TRUE)

dat$afp_cases <- dat$AFP == "ALL DIAGNOSED"

x <-
  dat %>% 
  group_by(month, YRONSET) %>%
  summarise(cases = sum(afp_cases, na.rm = TRUE)) %>% 
  mutate(month_year = paste(month, YRONSET)) %>% 
  arrange(YRONSET, month)


#########
# plots #
#########

ggplot(x, aes(x = month_year, y = cases)) +
  geom_bar(stat = "identity")

barplot(height = x$cases, col = "darkgreen", names.arg = x$month, las = 2)
# axis(1, at = seq(nrow(x)), labels = x$month)

# plotrix::textbox(c(-10, -10), -1, textlist = "hjkl", col = "white", fill = "green")
mtext("2015", side = 1, line = 3, at = 8)
mtext("2016", side = 1, line = 3, at = 22)
mtext("2017", side = 1, line = 3, at = 36)
mtext("2018", side = 1, line = 3, at = 50)
mtext("2019", side = 1, line = 3, at = 64)

title("Graph 1:Distribution of AFP Cases by Month, Pakistan 2015-2019*
", ylab = "Cases (n)")

abline(v = 15)
abline(v = 30)
abline(v = 45)

x$month <- factor(x$month, ordered = TRUE)
x$month_year <- factor(x$month_year, levels = x$month_year, ordered = TRUE)

ggplot(data = x, aes(x = month_year, y = cases)) +
  geom_bar(stat = "identity") + 
  scale_x_discrete(breaks = x$month_year, labels = x$month,
                   expand = c(0.1,0.1)) +
  facet_grid(~ YRONSET, space = "free_x", scales = "free_x", switch = "x") + 
  theme_bw() +
  theme(strip.placement = "outside",
        strip.background = element_rect(fill = NA, colour = "green"),
        panel.spacing = unit(0,"cm"),
        panel.grid.major.x = element_line(colour = NA, size = NULL, linetype = NULL,
                                          lineend = NULL, color = NULL, arrow = NULL,
                                          inherit.blank = FALSE),
        panel.grid.major.y = element_line(colour = "black", size = 1, linetype = NULL,
                                          lineend = NULL, color = NULL, arrow = NULL,
                                          inherit.blank = FALSE),
        plot.title = element_text(color = "green", size = 14, face = "bold.italic")) +
  ggtitle("Graph 1:Distribution of AFP Cases by Month, Pakistan 2015-2019*") +
  xlab("") +
  ylab("Cases (n)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(breaks = round(seq(min(x$cases), max(x$cases), by = 50), 1)) + 
  labs(caption = "* Afp.rec Data as of  15-07-2019")
