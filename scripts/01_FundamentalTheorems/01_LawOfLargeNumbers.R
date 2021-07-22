# Packages ----------------------------------------------------------------

require(ggplot2)

# Cumulative Means -------------------------------------------------------

set.seed(148)
n <- 1000
cummeans <- cumsum(rnorm(n, mean = 0, sd = 1))/(1:n)
df <- data.frame("n" = 1:n, "means" = cummeans)


# Plot --------------------------------------------------------------------


ggplot(df, aes(x = n, y = cummeans)) +
        geom_line(color = "darkblue", size = 1.4) +
        geom_hline(yintercept = 0, col = "red", size = 1.6) +
        labs(
                title = "Variance of the Cummulative Mean",
                y = "Cumulative Mean",
                x = "n"
        ) +
        theme_light() +
        theme(
                plot.title = element_text(face = "bold", hjust = 0.5, color = "darkblue"),
                axis.title.x = element_text(face = "bold"),
                axis.title.y = element_text(face = "bold")
        ) 