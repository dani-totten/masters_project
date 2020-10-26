library(lubridate)
library(dplyr)
library(FAdist)
library(extRemes)
library(goftest)
library(tidyverse)
require(gridExtra)
library(RColorBrewer)

setwd("/Users/danielletotten/Desktop/MS-Stat/project/images")
# distribution I'll be using 

sim500 <- ggplot(yrsam, aes(sim_date, sim_val)) + geom_point() + labs(x = "Simulation Date", y = "Simulation Value", title = "Sample Observations - 500th Simulation")
sim_500den <- ggplot(yrsam, aes(sim_val)) + geom_density() + labs(x = "Simulation Value", y = "density", title = "Distribution - 500th Simulation")
grid.arrange(sim500, sim_500den, ncol = 2)

# show cdf and pdf of tides over one year
llog_cdf<- data.frame(cbind(seq(0, 3, .05), pllog(seq(0, 3, .05), betuh, alphuh)))
colnames(llog_cdf) <- c("quant", "val")
llog_pdf <- data.frame(cbind(seq(0, 3, .05), dllog(seq(0, 3, .05), betuh, alphuh)))
colnames(llog_pdf) <- c("quant", "val")

# show pdf and cdf of log-logistic
a <- ggplot(llog_cdf, aes(quant, val)) + 
  geom_line() + 
  labs(x = "x", y = "Probability", title = "CDF of log-logistic",  subtitle = "alpha = 0.1, beta = 0.15")
b<- ggplot(llog_pdf, aes(quant, val)) + 
  geom_line() + 
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(x = "x", y = "", title = "PDF of log-logistic", subtitle = "alpha = 0.1, beta = 0.15")
grid.arrange(b, a, ncol = 2)
ggsave("dist_llog.png", grid.arrange(a, b, ncol=2))

sim500 <- ggplot(yrsam, aes(sim_date, sim_val)) + geom_point() + labs(x = "Simulation Date", y = "Simulation Value", title = "Sample Observations")
sim_500den <- ggplot(yrsam, aes(sim_val)) + geom_density() + labs(x = "Simulation Value", y = "density", title = "Sample Density")

ggsave("sim_summ.png", grid.arrange(sim500, sim_500den, ncol = 2))

# create fake annual data
yr1 <- head(yrsam, 365)
yr1 <- yr1 %>% 
  mutate(blockmax = as.logical(ifelse(sim_val == max(sim_val), 1, 0)))
yr1 <- yr1 %>% 
  group_by(mnth = month(sim_date)) %>% 
  mutate(monthblock = as.logical(ifelse(sim_val == max(sim_val), 1, 0)))
yr1 <- yr1 %>% 
  group_by(wk = week(sim_date)) %>% 
  mutate(weekblock = as.logical(ifelse(sim_val == max(sim_val), 1, 0)))

d1 <- ggplot(yr1, aes(sim_val)) + 
  geom_density() +
  labs(x = "Height of Tide", y = "", title = "Density of Tide Levels, 1900") +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
ggsave("d1.png")
a1<- ggplot(yr1, aes(sim_date, sim_val)) + 
  geom_point(aes(color = blockmax)) +
  labs(x = "", y = "Tide Level", title = "Daily Tide Level, 1900", color = "Annual Maximum")
  
m1 <- ggplot(yr1, aes(sim_date, sim_val)) + 
  geom_point(aes(color = monthblock)) +
  labs(x = "", y = "Tide Level", title = "Daily Tide Level, 1900", color = "Monthly Maximum")

w1 <- ggplot(yr1, aes(sim_date, sim_val)) + geom_point(aes(color = weekblock)) +
  labs(x = "", y = "Tide Level", title = "Daily Tide Level, 1900", color = "Weekly Maximum")

grid.arrange(a1, m1, w1, nrow= 3)

yr2 <- yrsam[366:730,]
yr2 <- yr2 %>% 
  mutate(blockmax = as.logical(ifelse(sim_val == max(sim_val), 1, 0)))
yr2 <- yr2 %>% 
  group_by(mnth = month(sim_date)) %>% 
  mutate(monthblock = as.logical(ifelse(sim_val == max(sim_val), 1, 0)))
yr2 <- yr2 %>% 
  group_by(wk = week(sim_date)) %>% 
  mutate(weekblock = as.logical(ifelse(sim_val == max(sim_val), 1, 0)))

d2 <- ggplot(yr2, aes(sim_val)) + 
  geom_density() +
  labs(x = "Height of Tide", y = "", title = "Density of Tide Levels, 1901") +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

a2<- ggplot(yr2, aes(sim_date, sim_val)) + 
  geom_point(aes(color = blockmax)) +
  labs(x = "", y = "Tide Level", title = "Daily Tide Level, 1901", color = "Annual Maximum")

m2 <- ggplot(yr2, aes(sim_date, sim_val)) + 
  geom_point(aes(color = monthblock)) +
  labs(x = "", y = "Tide Level", title = "Daily Tide Level, 1901", color = "Monthly Maximum")

w2 <- ggplot(yr2, aes(sim_date, sim_val)) + geom_point(aes(color = weekblock)) +
  labs(x = "", y = "Tide Level", title = "Daily Tide Level, 1901", color = "Weekly Maximum")


yr3 <- yrsam[731:1096,]
yr3 <- yr3 %>% 
  mutate(blockmax = as.logical(ifelse(sim_val == max(sim_val), 1, 0)))
yr3 <- yr3 %>% 
  group_by(mnth = month(sim_date)) %>% 
  mutate(monthblock = as.logical(ifelse(sim_val == max(sim_val), 1, 0)))
yr3 <- yr3 %>% 
  group_by(wk = week(sim_date)) %>% 
  mutate(weekblock = as.logical(ifelse(sim_val == max(sim_val), 1, 0)))

d3 <- ggplot(yr3, aes(sim_val)) + 
  geom_density() +
  labs(x = "Height of Tide", y = "", title = "Distribution of Tide Levels, 1902") +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
a3<- ggplot(yr3, aes(sim_date, sim_val)) + 
  geom_point(aes(color = blockmax)) +
  labs(x = "", y = "Tide Level", title = "Daily Tide Level, 1902", color = "Annual Maximum")

m3 <- ggplot(yr3, aes(sim_date, sim_val)) + 
  geom_point(aes(color = monthblock)) +
  labs(x = "", y = "Tide Level", title = "Daily Tide Level, 1902", color = "Monthly Maximum")

w3 <- ggplot(yr3, aes(sim_date, sim_val)) + geom_point(aes(color = weekblock)) +
  labs(x = "", y = "Tide Level", title = "Daily Tide Level, 1902", color = "Weekly Maximum")

grid.arrange(a1, a2, a3, nrow = 3)

# clean up output
grid.arrange(m1, m2, m3, nrow = 3)
grid.arrange(w1, w2, w3, nrow = 3)




annual_dot <- ggplot(head(year_block,100), aes(yr, block.max)) + geom_point() +
  labs(x="", y = "Annual Maximum Tide Level", title = "Annual Maximum Tide Level, 1900-1999")
da <- ggplot(head(year_block, 100), aes(block.max)) + geom_density() +
  labs(x = "Tide Level", title = "Density of Annual Maxima, 1900-1999") +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + xlim(0, 5)
ggsave("density_annual.png")
dm <- ggplot(head(month_block, 1200), aes(block.max)) + geom_density() +
  labs(x = "Tide Level", title = "Density of Monthly Maxima, 1900-1999") +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + xlim(0, 5)
ggsave("density_month.png")
dw <- ggplot(head(week_block, 5200), aes(block.max)) + geom_density() +
  labs(x = "Tide Level", title = "Density of Weekly Maxima, 1900-1999") +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + xlim(0, 5)
ggsave("density_week.png")

test <- grid.arrange(da, dm, dw, nrow = 3)
ggsave("density_stacked.png", test)
# fit a gev an doverlay it??

ggplot(head(month_block,1200), aes(yr, block.max)) + geom_point() +
  labs(x="", y = "Monthly Maximum Tide Level", title = "Monthly Maxima, 1900-1999")


grid.arrange(w1, w2, w3, nrow = 3)
ggsave("weekly_dots.png")


# EXTREME VALUE DISTRIBUTIONS
fevd(year_block$block.max,  units = "deg C", time.units = "years")
plot(fevd(year_block$block.max,  units = "deg C", time.units = "years"))
fevd(head(month_block$block.max, 1200),  units = "deg C", time.units = "months")
plot(fevd(head(month_block$block.max, 1200),  units = "deg C", time.units = "months"))
fevd(head(week_block$block.max, 5200),  units = "deg C", time.units = "52/year")
plot(fevd(head(week_block$block.max, 5200),  units = "deg C", time.units = "52/year"))






store_annual_df <- data.frame(store_annual)
colnames(store_annual_df) <- c("location", "loc_se", "scale", "scale_se", "shape","shape_se","AIC", "20low", "20est", "20hi", "ks_s", "ks_p")
store_annual_df$block <- "annual"
store_month_df <- data.frame(store_month)
colnames(store_month_df) <- c("location", "loc_se", "scale", "scale_se", "shape","shape_se","AIC", "20low", "20est", "20hi", "ks_s", "ks_p")
store_month_df$block <- "month"
store_week_df <- data.frame(store_week)
colnames(store_week_df) <- c("location", "loc_se", "scale", "scale_se", "shape","shape_se","AIC", "20low", "20est", "20hi", "ks_s", "ks_p")
store_week_df$block <- "week"



sim500 <- ggplot(yrsam, aes(sim_date, sim_val)) + geom_point() + labs(x = "Simulation Date", y = "Simulation Value", title = "Sample Observations - 500th Simulation")
sim_500den <- ggplot(yrsam, aes(sim_val)) + geom_density() + labs(x = "Simulation Value", y = "density", title = "Distribution - 500th Simulation")
grid.arrange(sim500, sim_500den, ncol = 2)



# order stat CDFs
max_ostat_df <- data.frame(cbind(x_seq, cdf_maxostat_annual, cdf_maxostat_month, cdf_maxostat_week))
names(max_ostat_df) <- c("val", "annual_ostat", "month_ostat", "week_ostat")

cdf_a <- ggplot(max_ostat_df, aes(val, annual_ostat)) + 
  geom_line() + 
  labs(x = "x", y = "F[(x)]^n", title = "CDF of Maximum Order Statistic", subtitle = "a - Block Size: N = 365")
cdf_m <- ggplot(max_ostat_df, aes(val, month_ostat)) + 
  geom_line() + 
  labs(x = "x", y = "F[(x)]^n", subtitle = "b - Block Size: N = ~30")
cdf_w <- ggplot(max_ostat_df, aes(val, week_ostat)) + 
  geom_line() + 
  labs(x = "x", y = "F[(x)]^n", subtitle = "c - Block Size: N = 7")
grid.arrange(cdf_a, cdf_m, cdf_w, ncol=1)
ggsave("ostat_cdfs.png", grid.arrange(cdf_a, cdf_m, cdf_w, ncol=1))

ostat_long <- max_ostat_df %>% pivot_longer(-val, names_to = "block", values_to = "quant")

a_avg <- round(FAdist::pgev(x_seq,shape=mean(store_annual_df$shape),scale=mean(store_annual_df$scale),location=mean(store_annual_df$location)), 10)
a_avg[1:4] <- 0.0
m_avg <- round(FAdist::pgev(x_seq,shape=mean(store_month_df$shape), scale=mean(store_month_df$scale),location=mean(store_month_df$location)), 10)
w_avg <- round(FAdist::pgev(x_seq,shape=mean(store_week_df$shape),scale=mean(store_week_df$scale),location=mean(store_week_df$location)), 10)
avg_cdf <- data.frame(cbind(x_seq, a_avg, m_avg, w_avg))
names(avg_cdf) <- c("val", "annual_gev", "month_gev", "week_gev")

# put into ggplot format
avgcdf_long <- avg_cdf %>% pivot_longer(-val, names_to = "block", values_to = "quant")
cdfs <- rbind(ostat_long, avgcdf_long)

cdfs <- cdfs %>% mutate(type = str_detect(block, "ostat"))

ggplot(cdfs, aes(val, quant)) + 
  geom_line(aes(color = block, linetype = type)) + 
  labs(x = "x", y = "Probability", title = "CDF of Maximum Order Statistic and GEV")
ggsave("cds.png")
an1 <- ggplot(filter(cdfs, block == "annual_gev" | block == "annual_ostat"), aes(val, quant)) + 
  geom_line(aes(color = block, linetype = type)) + 
  labs(x = "x", y = "Probability", title = "CDF of Maximum Order Statistic and GEV", subtitle = "a- Annual Blocks") +
  scale_colour_brewer(palette = "Dark2") + scale_fill_brewer(palette = "Dark2")

m1 <- ggplot(filter(cdfs, block == "month_gev" | block == "month_ostat") , aes(val, quant)) + 
  geom_line(aes(color = block, linetype = type)) + 
  labs(x = "x", y = "Probability", subtitle = "b - Month Blocks") +
  scale_colour_brewer(palette = "Dark2") + scale_fill_brewer(palette = "Dark2")

w1 <- ggplot(filter(cdfs, block == "week_gev" | block == "week_ostat") , aes(val, quant)) + 
  geom_line(aes(color = block, linetype = type)) + 
  labs(x = "x", y = "Probability", subtitle = "c - Week Blocks") +
  scale_colour_brewer(palette = "Dark2") + scale_fill_brewer(palette = "Dark2")

ggsave("osgev.png", grid.arrange(an1, m1, w1))



big <- rbind(store_annual_df, store_week_df, store_month_df)

# ggplot, violin plots for parameter estimation
loc_den <- ggplot(big, aes(location)) + 
  geom_density(aes(color = block, fill = block)) + 
  labs(x = "Location Parameter", y = "Density", title = "Density of Location Parameter Estimates") +
  scale_fill_brewer(palette = "Dark2") +
  scale_colour_brewer(palette = "Dark2")
locse_den <- ggplot(big, aes(block, loc_se)) + 
  geom_boxplot(aes(block, fill = block)) + 
  labs(x ="Block Size" , y = "Standard Error", title = "Standard Error of Location Parameter Estimates") +
  scale_fill_brewer(palette = "Dark2")
ggsave("loc.png", grid.arrange(loc_den, locse_den, nrow = 2))

shape_den <- ggplot(big, aes(shape)) + 
     geom_density(aes(color = block, fill = block), alpha = .7) + 
     labs(x = "Shape Parameter", y = "Density", title = "Density of Shape Parameter Estimates") +
     scale_colour_brewer(palette = "Dark2") + scale_fill_brewer(palette = "Dark2")
shapese_den <- ggplot(big, aes(block, shape_se)) + 
  geom_boxplot(aes(fill = block)) + 
  labs(x = "Block Size", y = "Standard Error", title = "Standard Error of Shape Parameter Estimates") +
  scale_fill_brewer(palette = "Dark2") 
ggsave("shape.png", grid.arrange(shape_den, shapese_den, nrow = 2))

scale_den <- ggplot(big, aes(scale)) + 
  geom_density(aes(color = block, fill = block)) + 
  labs(x = "Scale Parameter", y = "Density", title = "Density of Scale Parameter Estimates") +
  scale_colour_brewer(palette = "Dark2") + scale_fill_brewer(palette = "Dark2")
scalese_den <- ggplot(big, aes(block, scale_se)) + 
  geom_boxplot(aes(fill = block)) + 
  labs(x = "Block Size", y = "Standard Error", title = "Standard Error of Scale Parameter Estimates") +
  scale_fill_brewer(palette = "Dark2") 
ggsave("scale.png", grid.arrange(scale_den, scalese_den))

aic_summ <- cbind(summary(store_annual_df$AIC), summary(store_month_df$AIC), summary(store_week_df$AIC))
colnames(aic_summ) <- c("Annual", "Month", "Week")

# p-values
big %>% group_by(block) %>% filter(ks_p > 0.05) %>% summarize(n())
pval_summ <- cbind(summary(store_annual_df$ks_p), summary(store_month_df$ks_p), summary(store_week_df$ks_p))
colnames(pval_summ) <- c("Annual", "Month", "Week")
View(round(pval_summ, 5))

ggplot(big, aes(x = ks_p, color = block, fill = block), binwidth = .05) +
  geom_histogram(position = "dodge") +
  labs(x = "Simulation p-val", y = "Number of Simulations", title = "KS p-value") +
  scale_fill_brewer(palette = "Dark2") + scale_colour_brewer(palette = "Dark2")
ggsave("ks.png")

library(evd)

plot(pgev(seq(1, 10, .1), loc = 1, scale = 1, shape = .1))

plot(density(rgev(100000, loc = 1, scale = 1, shape = .1)))
plot(density(rgev(100000, loc = 1, scale = 1, shape = 0)))
plot(density(rgev(100000, loc = 1, scale = 1, shape = -.1)))

plot(gev_week)
plot(gev_month)
plot(gev_annual)

grid.arrange(a, b, c)
# 
h <- 1
set.seed(h)

llog_out <- matrix(0, nrow = 700, ncol = 3)
for (i in 1:7){
  for (j in 1:100){
    llog_sim <- rllog(10^i, .15, .1)
    llog_out[h,] <- c(10^i, h, max(llog_sim))
    h <- h + 1
    set.seed(h)
  }
}

llog_out <- data.frame(llog_out)
colnames(llog_out) <- c("n_sample", "sim", "max")
#llog_out$n_sample <- as.factor(llog_out$n_sample)
llog_out_summ <- llog_out %>% group_by(n_sample = as.factor(n_sample)) %>% summarize("avg_sam_max" = mean(max))
ggplot(llog_out_summ, aes(n_sample, avg_sam_max)) + 
  geom_point() + 
  labs(x = "Sample Size", y = "Average Sample Maximum", title = "Avg. Max of Log-Logistic")
ggsave("llog_sim.png")
