start_date = "2017-01-22"
subset_duration = 18 # in months, incl 1 extra month for T_out (validation)
step_size = 2 # in months
n_steps = 1
t_0 = as.Date(start_date)
t_1 = as.Date(t_0) + months(subset_duration) #stored end_date, for subset
subset = build_vix9_rv_subset(t_0, t_1)

#subset$vix_9d_out = NA
temp = data_frame()
for (i in 1:9) {
  temp = rbind(temp, f_GARCH[[i]]$returns[,c("date", "price_r_out")])
}
subset = left_join(subset, temp, by = "date")[,c(1,3,6)]
names(subset)[3] = "vix_9d_predicted"
names(subset)[2] = "vix_9d_true"


rv_long <- melt(subset, id="date")
ggplot(data=rv_long, aes(x=date, y=value, colour=variable)) + 
  geom_line(alpha=.9) +
  theme_ipsum() +
  geom_vline(xintercept = as.Date(c("2017-07-22", "2017-09-22", "2017-11-22", "2018-01-22", "2018-03-22", "2018-05-22")), 
             show.legend = F, colour = "steel blue", linetype="dotted", legend.position="bottom") +
  #ylim(0,0.0007) +
  scale_x_date(limit=c(as.Date("2017-07-15"),as.Date("2018-06-20"))) + 
  theme(legend.position="bottom")
remove(rv_long)