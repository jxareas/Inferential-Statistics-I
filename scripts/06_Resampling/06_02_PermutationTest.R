# Packages ----------------------------------------------------------------

library(datasets)
library(ggplot2)
library(ggthemes)

# Data --------------------------------------------------------------------

df <-
        InsectSprays |>
        subset(spray %in% c("B", "C"))

count <-
        df$count

group <-
        as.character(df$spray)

# The statistic used for the test is the Difference in Means
mean_diff <-
        function(count, group)
        {
                mean(count[group == "B"]) - mean(count[group == "C"])
        }

# Our observed test statistic is:
test_statistic <-
        mean_diff(count = count, group = group)


# Ordinary Permutation Test --------------------------------------------------------
set.seed(287)

# We permute the labels (spray) while maintaining the values (count)
permutations <-
        sapply(1:1e4, \(k)
               {
                       mean_diff(count = y, sample(group))
        })

p_value <- mean(permutations > test_statistic)


# EDA: Boxplot ------------------------------------------------------------

boxplot <- 
        ggplot(data = df, mapping = aes(y = spray, x = count, fill = spray)) +
        stat_boxplot(geom = "errorbar", width = 1) +  
        geom_boxplot() +
        guides(fill = "none") +
        labs(
                title = "Spray Comparison",
                x = "Spray",
                y = "Count of Insects Killed"
        ) +
        theme(
                plot.title = element_text(face = "bold", hjust = .5),
                axis.title.x = element_text(face = "bold",
                                            color = "dimgrey"),
                axis.title.y = element_text(face = "bold",
                                            color = "dimgrey")
                
        )

boxplot

# Histogram of the Observed Permutations ----------------------------------
hist_plot <-
        ggplot(mapping = aes(permutations)) +
        geom_histogram(fill = "red4", color = "black") +
        geom_vline(xintercept = test_statistic,
                   color = "red",
                   size = 2) +
        annotate(
                geom = "text",
                label = "Test Statistic",
                x = test_statistic - 2.7,
                y = 1e3,
                color = "red",
                fontface = 2
        ) +
        annotate(
                geom = "text",
                label = "= 13. 75",
                x = test_statistic - 2.7,
                y = 1e3 - 100,
                color = "red",
                fontface = 2
        ) +
        labs(title = "Observed Permutations",
             x = "Mean Difference",
             y = "Frequency") +
        theme_minimal() +
        theme(
                plot.title = element_text(face = "bold", hjust = .5),
                axis.title.x = element_text(face = "bold",
                                            color = "dimgrey"),
                axis.title.y = element_text(face = "bold",
                                            color = "dimgrey")
                
        )


# Density Function --------------------------------------------------------
density_plot <-
        ggplot(mapping = aes(x = permutations)) +
        geom_density(color = "yellow",
                     fill = "yellow",
                     alpha = .6) +
        geom_vline(xintercept = test_statistic,
                   size = 2,
                   color = "magenta") +
        annotate(
                geom = "text",
                label = "Sample Diff. In Means",
                x = 11.25,
                y = 0.12,
                fontface = 2,
                color = "magenta"
        ) +
        scale_x_continuous(breaks = c(seq(-10, 10, by = 5), test_statistic)) +
        labs(title = "Density Estimate",
             x = "Difference in Means",
             y = "Probability") +
        theme_solarized(light = 0) +
        theme(
                plot.title = element_text(
                        face = "bold",
                        hjust = .5,
                        color = "white"
                ),
                axis.title.x = element_text(face = "bold",
                                            color = "white"),
                axis.text.x = element_text(color = "white"),
                axis.title.y = element_text(face = "bold",
                                            color = "white"),
                axis.text.y = element_text(color = "white"),
        )
density_plot
