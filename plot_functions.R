#1. Custom Themes ----
#ak_plot_theme
ak_plot_theme <- function(..., base_size = 12) {
  theme(
    text = element_text(size = base_size),
    axis.ticks = element_blank(),
    axis.title = element_text(color = "black",
                              face = "bold"),
    axis.text = element_text(color = "black",
                             face = "bold"),
    axis.text.x = element_text(angle = 40, hjust =0.6, vjust = 0.8),
    plot.title.position = "plot",
    plot.title = element_text(size = 16,
                              #face = "bold",
                              color = "black",
                              vjust = 5),
    plot.subtitle = element_text(color = "black",
                                 hjust = 0.5),
    plot.caption = element_text(size = 8,
                                face = "italic",
                                color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major =  element_line(color = "#e0e0e0"),
    panel.grid.major.x = element_blank(), #maybe..
    panel.background = element_rect(fill = "#ffffff"),
    plot.background = element_blank(),
    panel.border = element_blank())
}


#2. Stat Summary Functions ----
#this is a function for the stat_summary pointrange geom
mean_peak <- function(x) {
  data.frame(y = (mean(x) + 0.0001),
             ymax = max(x),
             ymin = mean(x))
}

#95%CI calculation.. calculating t-value because of small sample sizes
#t-value*(sd/sqrt(n))
mean_95CI <- function (x) {
  data.frame(y = (mean(x) + 0.001),
             ymax = mean(x) + qt(p = .05/2, df=length(x)-1, lower.tail = FALSE) *(sd(x)/sqrt(length(x))),
             ymin= mean(x) - qt(p = .05/2, df=length(x)-1, lower.tail = FALSE) *(sd(x)/sqrt(length(x))))
}
###



#3a. stat_summary_plot ----