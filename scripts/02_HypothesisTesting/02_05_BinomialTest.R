### Packages ----------------------------------------------------------------

library(dplyr)
library(ggplot2)

### Simulating Data ---------------------------------------------------------
set.seed(3)

# Data Observed
# p_hat = 1 -> Sample Proportion
# p0 = 0.5 -> Hypothesized Proportion
# n = 8

# Hypothesis Test
# H0: p0 = 0.5
# H1: p0 > 0.5

df <-
        data.frame("success" = rbinom(1e5, 8, prob = .5)) |> mutate("proportions" = success / 8)


# p-value = P(p_sim >= 1 | p = 0.5)
p <-
        with(data = df,
             expr = sum(proportions == 1)/sum(proportions != 1)
             ) 

counts <- df |>
        group_by(success) |>
        summarise(count = n()) |> 
        arrange(desc(success))

# Bar Chart --------------------------------------------------------------------

ggplot(data = counts, mapping = aes(x = success, y = count)) +
        geom_bar(stat = "identity", color = "black",
                 fill = "red", alpha = .7) +
        labs(title = "Frequency of Simulation Successes",
             subtitle = "Xi ~ Binom(n = 8, p = 0.5)",
             x = "# of Successes",
             y = "Frequency") +
        theme_gray() +
        theme(
                plot.title = element_text(face = "bold", hjust = .5),
                plot.subtitle = element_text(face = "bold", hjust = .5,
                                             color = "gray"),
                axis.title.x = element_text(face = "bold",
                                            color = "dimgrey"),
                axis.title.y = element_text(face = "bold",
                                            color = "dimgrey")
        ) 


# Exact Binomial Test -----------------------------------------------------------

test <- 
        binom.test(x = 8, n = 8, p = .5, alternative = "greater",
                   conf.level = .95)

