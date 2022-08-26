library(tidyverse)

# Analysis with actual data ----
#Test for normality and homogeneity of variance
plot.data <- read_csv("solar-irrigation-data.csv")
attach(plot.data)
# Test for normality using Shapiro Wilk test
# If P > 0.05, we cannot reject the NULL hypothesis that the distribution is different than normal
# If P < 0.05, then the distribution is significantly different than normal
for(i in 6:16){
  print(names(plot.data[i]))
  print(shapiro.test(plot.data[[i]]))
}
# Results: only cropping intensity can be assumed to be normal (P = 0.15).
# normality test by district
df <- filter(plot.data, District == "Nawalpur")
for(i in 6:16){print(names(df[i]))  
  print(shapiro.test(df[[i]])) }

message(names(df[i]), print(shapiro.test(df[[i]])) )

# results: Bardiya: y.ci not normal, Mahottari: all measures except y.ci are not normal, Nawalpur: all measures except x.ci are not normal.

# Test for homogeneity of variances
#convert to tidy format
data.tidy <- pivot_longer(plot.data, cols = x.area:y.ci, names_to = "treatment", values_to = "value")

# If p > 0.05, variances are not significantly different between groups
# if p < 0.05, variances are significantly different between groups
df <- data.tidy %>% filter(., treatment %in% c("x.ci", "y.ci"))
bartlett.test(data = df, value ~ treatment)

data.tidy %>% filter(., treatment %in% c("x.ci", "y.ci")) %>% 
  bartlett.test(data = ., value ~ treatment) %>% .[["p.value"]]
data.tidy %>% filter(., treatment %in% c("x.kg", "y.kg")) %>% 
  bartlett.test(data = ., value ~ treatment) %>% .[["p.value"]]
data.tidy %>% filter(., treatment %in% c("x.income", "y.income")) %>% 
  bartlett.test(data = ., value ~ treatment) %>% .[["p.value"]]
data.tidy %>% filter(., treatment %in% c("x.crop", "y.crop")) %>% 
  bartlett.test(data = ., value ~ treatment) %>% .[["p.value"]]
data.tidy %>% filter(., treatment %in% c("x.ci", "y.ci")) %>% 
  bartlett.test(data = ., value ~ treatment) %>% .[["p.value"]]
# Results: all variables have unequal variances  

# Check for unequal variances by district
data.tidy %>% filter(., District == "Bardiya" & treatment %in% c("x.kg", "y.kg")) %>%
  bartlett.test(data = ., value ~ treatment) %>% .[["p.value"]]

# Generate summaries using data.tidy ----
summ.by.all <- data.tidy %>% 
  group_by(treatment) %>% 
  summarize(
    nobs = length(value),
    min = min(value),
    max = max(value),
    average = mean(value),
    sd = sd(value),
    median = median(value),
    var = var(value)
  )

summ.by.dist <- data.tidy %>% 
  group_by(District, treatment) %>% 
  summarize(
    nobs = length(value),
    min = min(value),
    max = max(value),
    average = mean(value),
    sd = sd(value),
    median = median(value),
    var = var(value)
  )

summ.by.site <- data.tidy %>% 
  group_by(District, Site, treatment) %>% 
  summarize(
    nobs = length(value),
    min = min(value),
    max = max(value),
    average = mean(value),
    sd = sd(value),
    median = median(value),
    var = var(value)
  )

write_csv(summ.by.all,  "summbyall.csv")
write_csv(summ.by.dist, "summbydist.csv")
write_csv(summ.by.site, "summbysite.csv")
#combine the files in Excel

# Plots ----
# Crop intensity
scale_max <- max(x.ci, y.ci, na.rm = TRUE) + 1
p.ci <- plot.data %>% 
  ggplot(aes(x = x.ci, y = y.ci, shape = District, color = Site)) +
  geom_abline(intercept = 0, slope = 1) +
  xlim(0, scale_max) + ylim(0, scale_max) +
  geom_point(size = 8, alpha = 1) +
  geom_text(aes(label = SN)) +
  scale_shape_manual(values = c(1, 0, 2)) +
  facet_grid(. ~ District) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  labs(title = "A. Cropping intensity",
       x = "Before",
       y = "After")
p.ci

# Production
scale_max <- max(x.kg, y.kg, na.rm = TRUE)/1000 + 1
p.kg <- plot.data %>% 
  ggplot(aes(x = x.kg/1000, y = y.kg/1000, shape = District, color = Site)) +
  geom_abline(intercept = 0, slope = 1) +
  xlim(0, scale_max) + ylim(0, scale_max) +
  geom_point(size = 8, alpha = 1) +
  geom_text(aes(label = SN)) +
  scale_shape_manual(values = c(1, 0, 2)) +
  facet_grid(. ~ District) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  labs(title = "B. Production in tons",
       x = "Before",
       y = "After")
p.kg

# Crop diversity
scale_max <- max(x.crop, y.crop, na.rm = TRUE) + 1
p.crop <- plot.data %>% 
  ggplot(aes(x = x.crop, y = y.crop, shape = District, color = Site)) +
  geom_abline(intercept = 0, slope = 1) +
  xlim(0, scale_max) + ylim(0, scale_max) +
  geom_point(size = 8, alpha = 1) +
  geom_text(aes(label = SN)) +
  scale_shape_manual(values = c(1, 0, 2)) +
  facet_grid(. ~ District) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  labs(title = "C. Crops per household",
       x = "Before",
       y = "After")
p.crop

# Change in Production Area
scale_max <- max(x.area, y.area, na.rm = TRUE) + 1
p.area <- plot.data %>% 
  ggplot(aes(x = x.area, y = y.area, shape = District, color = Site)) +
  geom_abline(intercept = 0, slope = 1) +
  xlim(0, scale_max) + ylim(0, scale_max) +
  geom_point(size = 8, alpha = 1) +
  geom_text(aes(label = SN)) +
  scale_shape_manual(values = c(1, 0, 2)) +
  facet_grid(. ~ District) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  labs(title = "D. Production area per household",
       x = "Before",
       y = "After")
p.area

# Income
scale_max <- max(x.income, y.income, na.rm = TRUE) + 1
p.inc <- plot.data %>% 
  ggplot(aes(x = x.income, y = y.income, shape = District, color = Site)) +
  geom_abline(intercept = 0, slope = 1) +
  xlim(0, scale_max) + ylim(0, scale_max) +
  geom_point(size = 8, alpha = 1) +
  geom_text(aes(label = SN)) +
  scale_shape_manual(values = c(1, 0, 2)) +
  facet_grid(. ~ District) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  labs(title = "E. Income per household",
       x = "Before",
       y = "After")
p.inc

library(cowplot)
prow <- plot_grid(p.ci + theme(legend.position = "none"),
                  p.kg + theme(legend.position = "none"),
                  p.crop + theme(legend.position = "none"),
                  p.area + theme(legend.position = "none"),
                  p.inc + theme(legend.position = "none"),
          ncol = 2)
# Extract and add a common legend
p.legend <- get_legend(p.ci + theme(legend.box.margin = margin(0, 0, 0, 12)))
prow.legend <- plot_grid(prow, p.legend, rel_widths = c(3, 0.4))
prow.legend
ggdraw(add_sub(prow.legend, 
               "plots created using simulated data for illustration", 
               size = 10,
               hjust = 0.5))
ggsave(paste0("ScatterPlots_faceted", Sys.Date(), ".png"), plot = last_plot(), width = 12, height = 18, units = "in", dpi = 300, device = "png")



# Traditional plots ----
library(ggpubr)
# by commenting out the #facet.by  = "District" line, I can create the overall comparison for supplementary figure 1.

# Cropping Intensity
bpd <- plot.data[c("District", "x.ci", "y.ci")]
names(bpd) <- c("District", "before", "after")
bp.ci <- ggpaired(bpd, cond1 = "before", cond2 = "after",
         facet.by = "District", 
         line.color = "gray",
         title = "A. Change in cropping intensity",
         xlab = "",
         ylab = "Cropping intensity") +
  stat_compare_means(method = "wilcox.test" , 
                     paired = TRUE,
                     label.y.npc = .9,
                     aes(label = paste("P =", ..p.format..) ) )
bp.ci

# Production
bpd <- plot.data[c("District", "x.kg", "y.kg")]
names(bpd) <- c("District", "before", "after")
bp.kg <- ggpaired(bpd, cond1 = "before", cond2 = "after",
         facet.by = "District", 
         line.color = "gray",
         title = "B. Change in production",
         xlab = "",
         ylab = "Kg") +
  stat_compare_means(method = "wilcox.test" , 
                     paired = TRUE,
                     label.y.npc = .9,
                     aes(label = paste("P =", ..p.format..) ) )
bp.kg

# Change in number of crops
bpd <- plot.data[c("District", "x.crop", "y.crop")]
names(bpd) <- c("District", "before", "after")
bp.crop <- ggpaired(bpd, cond1 = "before", cond2 = "after",
         facet.by = "District", 
         line.color = "gray",
         title = "C. Change in crops per household",
         xlab = "",
         ylab = "Crops per HH") +
  stat_compare_means(method = "wilcox.test" , 
                     paired = TRUE,
                     label.y.npc = .9,
                     aes(label = paste("P =", ..p.format..) ) )
bp.crop

# Change in area of production
bpd <- plot.data[c("District", "x.area", "y.area")]
names(bpd) <- c("District", "before", "after")
bp.area <- ggpaired(bpd, cond1 = "before", cond2 = "after",
                    facet.by = "District", 
                    line.color = "gray",
                    title = "D. Change in production area per household",
                    xlab = "",
                    ylab = "Ropani per HH") +
  stat_compare_means(method = "wilcox.test" , 
                     paired = TRUE,
                     label.y.npc = .9,
                     aes(label = paste("P =", ..p.format..) ) )
bp.area

# Change in income
bpd <- plot.data[c("District", "x.income", "y.income")]
names(bpd) <- c("District", "before", "after")
bp.income <- ggpaired(bpd, cond1 = "before", cond2 = "after",
                    facet.by = "District", 
                    line.color = "gray",
                    title = "E. Change in income per household",
                    xlab = "",
                    ylab = "Income from agriculture (NPR)") +
  stat_compare_means(method = "wilcox.test" , 
                     paired = TRUE,
                     label.y.npc = .9,
                     aes(label = paste("P =", ..p.format..) ) )
bp.income

library(cowplot)
bp.row <- plot_grid(bp.ci, bp.kg, bp.crop, bp.area, bp.income,
          ncol = 2,
          nrow = 3)
bp.row
ggsave(paste0("ComparisonPlots", Sys.Date(), ".png"), plot = last_plot(), width = 13, height = 16, units = "in", dpi = 300, device = "png")

ggsave(paste0("ComparisonPlotsAggregate-", Sys.Date(), ".png"), plot = last_plot(), width = 12, height = 18, units = "in", dpi = 300, device = "png")
