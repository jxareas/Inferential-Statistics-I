# Packages ----------------------------------------------------------------

library(ggplot2)
library(ggthemes)

# Data --------------------------------------------------------------------

power.t.test(
        n = 1:60,
        delta = 2,
        sd = 4,
        type = "one.sample",
        alt = "one.sided"
) -> p

df <-
        data.frame(with(p, list(n, power)))

names(df) <- c("n", "power")


# Plot --------------------------------------------------------------------

plot <-
        ggplot(data = df,
               mapping = aes(x = n, y = power, col = power)) +
        geom_point() +
        geom_hline(
                yintercept = 1,
                color = "red",
                size = 1.5,
                linetype = "dashed"
        ) +
        labs(title = "Power Convergence by Sample Size",
             x = "n",
             y = "Power") +
        guides(col = "none") +
        theme_minimal() +
        theme_stata(scheme = "s1color") +
        theme(
                plot.title = element_text(face = "bold",
                                          hjust = .5),
                axis.title.x = element_text(face = "bold",
                                            color = "dimgrey"),
                axis.title.y = element_text(face = "bold",
                                            color = "dimgrey")
        )
