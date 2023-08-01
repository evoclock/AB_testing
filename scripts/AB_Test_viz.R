# Data viz AB test

source("../random_bits/tidy_loader.R")
load_required_packages()

conv_rate_A = as.numeric(readRDS("./outputs/conv_rate_A.Rds"))
conv_rate_B = as.numeric(readRDS("./outputs/conv_rate_B.Rds"))
ci_A_lower = as.numeric(readRDS("./outputs/ci_A_lower.Rds"))
ci_A_upper = as.numeric(readRDS("./outputs/ci_A_upper.Rds"))
ci_B_lower = as.numeric(readRDS("./outputs/ci_B_lower.Rds"))
ci_B_upper = as.numeric(readRDS("./outputs/ci_B_upper.Rds"))
SE_pool = readRDS("./outputs/SE_pool.Rds")

# Create the data for ABTestdata
ABTestdata = data.frame(
  variant = factor(rep(c("A", "B"), each = 200)),
  conf_interval = c(rnorm(200, 0.02773925, 0.006116051), 
                    rnorm(200, 0.05068493, 0.008118638))
)

# Create the data for pe
pe = data.frame(
  variant = c("A", "B"),
  conversion_rate = round(c(conv_rate_A, conv_rate_B), 4),
  lower_confidence = round(c(ci_A_lower, ci_B_lower), 4),
  upper_confidence = round(c(ci_A_upper, ci_B_upper), 4)
)

# Plot the distributions with annotations
p <- ggplot(ABTestdata, aes(x = conf_interval, fill = variant)) +
  geom_density(alpha = 0.3) +
  geom_vline(aes(xintercept = lower_confidence[1], color = "Variant A CI lower"), 
             data = pe, linetype = "dashed") +
  geom_vline(aes(xintercept = upper_confidence[1], color = "Variant A CI upper"), 
             data = pe, linetype = "dashed") +
  geom_vline(aes(xintercept = lower_confidence[2], color = "Variant B CI lower"), 
             data = pe, linetype = "dashed") +
  geom_vline(aes(xintercept = upper_confidence[2], color = "Variant B CI upper"), 
             data = pe, linetype = "dashed") +
  geom_text(data = pe, aes(x = conversion_rate, 
                           y = 8, 
                           label = paste("Conversion rate\n", 
                                         conversion_rate*100, 
                                         "%")),
            vjust = -1, hjust = 0.5, color = "black", fontface = "bold", family = "Helvetica", size = 4) +
  labs(
    title = "Expected Distributions of Variants A and B",
    subtitle = "Variant B's observed conversion rate was 82.72% higher than Variant A's conversion rate",
    x = "Confidence Interval",
    y = "",
    caption = "J Gamboa"
  ) +
  scale_color_manual(values = c("Variant A CI lower" = "red", 
                                "Variant A CI upper" = "red", 
                                "Variant B CI lower" = "green", 
                                "Variant B CI upper" = "green")) +
  scale_fill_manual(values = c("#21918c", "#4b779d")) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(family = "Helvetica", 
                              size = 20, 
                              color = "black"),
    plot.subtitle = element_text(family = "Helvetica", 
                                 size = 12, 
                                 face = "bold", 
                                 color = "black"),
    panel.grid.major.y = element_line(color = "#FAFAFA")
  )

# Save the plot to "./figures/" with a white background
ggsave(filename = "./figures/01_CI_dist_plot.png", 
       plot = p, 
       width = 8, 
       height = 6, 
       bg = "white")

p

# Visualizing the pool using a normal distribution
ABTdata_pool = data.frame(Conf_interval = rnorm(n = 10000, 
                                                mean = 0, 
                                                sd = SE_pool))

p_pool = ggplot(ABTdata_pool, aes(x = Conf_interval)) +
  geom_histogram(aes(y = ..density..), 
                 colour = "black", 
                 fill = "white", 
                 binwidth = 0.00029) +
  geom_density(alpha = 0.2, fill = "#21918c") +
  theme_minimal()

# Save the pool plot to "./figures/" with a white background
ggsave(filename = "./figures/02_pool_plot.png", 
       plot = p_pool, 
       width = 8, 
       height = 6, 
       bg = "white")


p_pool
