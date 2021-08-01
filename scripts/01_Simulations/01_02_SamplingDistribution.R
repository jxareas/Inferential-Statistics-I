# Packages ----------------------------------------------------------------

require(ggplot2)
require(ggthemes)

# Poisson Distributed Data ------------------------------------------------

set.seed(3)
A <- rpois(n = 60000, lambda = 10)


# Sampling Distribution of the Sample Mean --------------------------------

sample <- vector(mode = "list", 5000L) |> 
        lapply(
                \(x)
                {
                        sample(A, 1000, T)
                }
        )

means <- sample |> 
        sapply(
                mean, simplify = T
        )

df <- data.frame("X" = means)


# Plot --------------------------------------------------------------------

ggplot(df, mapping = aes(x = X)) +
        geom_density(fill = "magenta", col = "black", alpha = 0.2) +
        labs(
                x = "X",
                y = "P(X = x)",
                title = "Sampling Distribution of the Sample Mean"
        ) +
        theme_stata() +
        theme(
                plot.title = element_text(face = "bold", hjust = 0.5, color = "black"),
                axis.title.x = element_text(face = "bold", size = 12),
                axis.title.y = element_text(face = "bold", size = 12)
        )