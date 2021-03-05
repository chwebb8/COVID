library("readxl")
covid_0407 <- read_excel("covid_0407.xlsx")


library(ggplot2)
covid_states <- read_excel("covid_prod_states.xlsx")
election_states <- read_excel("election_states.xlsx")

party_colors <- c("#2E74C0", "#CB454A") 



p0 <- ggplot(data = subset(election_states, statename != "District of Columbia") ,
             mapping = aes(x = margin,
                           y = reorder(statename, margin),
                           color = winner))

p1 <- p0 + geom_vline(xintercept = 0, color = "gray30") +
  geom_point(size = 2)

p2 <- p1 + scale_color_manual(values = party_colors)

p3 <- p2 + scale_x_continuous(breaks = c(-40, -30, -20, -10, 0, 10, 20, 30),
                              labels = c("40\n (REP)","30", "20", "10", "0",
                                         "10", "20", "30\n(DEM)"))

p3 + facet_wrap(~ region, ncol=1, scales="free_y") +
  guides(color=FALSE) + labs(x = "Point Margin", y = "") +
  theme(axis.text=element_text(size=9))


library(statebins)

covid_states1 <- subset(covid_states, report_date == '2020-12-31')

statebins(state_data = covid_states1, state_col = "statename", 
          value_col = "deathsper100k", name="Per 100K pop",
          palette = "Reds", 
          direction = 1) +
  labs(title="2020 Mortality Rate") +
  theme_statebins()


statebins(
  font_size=4, dark_label = "white", light_label = "white",
  state_data = election_states, 
  state_col =  "statename",
  ggplot2_scale_function = scale_fill_manual,
  value_col = "winner",
  values = c(dem = "#2166ac", rep = "#b2182b"),
  round = TRUE
) +
  labs(title="2020 Presidential Election") +
  theme_statebins()


install.packages("ggrepel")
library(ggrepel)

covid_states_2 <- read_excel("covid_prod_states_2.xlsx")
cs <- subset(covid_states_2, report_date >= '2020-01-15')

p <- ggplot(data = cs,
            mapping = aes(x = report_date, y = rolling_new_cases_per100k,
                          group = statename))
p + geom_line(color = "gray70") 


p0 <- ggplot(data = cs,
             mapping = aes(x = report_date, y = rolling_new_cases_per100k))

p1 <- p0 + geom_line(color = "gray70", 
                     mapping = aes(group = statename)) 

p2 <- p1 + geom_smooth(mapping = aes(group = division),
                       se = FALSE)

p3 <- p2 + geom_text_repel(data = subset(cs, report_date == max(report_date)),
                           mapping = aes(x = report_date, y = rolling_new_cases_per100k, label = Abbr),
                           size = 3, segment.color = NA, nudge_x = 30
                           ,max.iter = Inf) +
  coord_cartesian(c(min(cs$report_date), 
                    max(cs$report_date)))



p4 <- p3 + labs(x = "", y = "New Cases Per 100k",
          title = "State-Level New Cases per 100k") +
  facet_wrap(~ reorder(division, -rolling_new_cases_per100k, na.rm = TRUE), nrow  = 3)

p4 + theme_light()