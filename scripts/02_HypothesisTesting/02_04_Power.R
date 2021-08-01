### Packages ----------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(reshape2)


### Calculating Power -------------------------------------------------------

# Parameters to calculate Power
alpha = 0.05
mu0 = 30
sigma = 4
z = qnorm(1 - alpha)

sample_size <- local({
        k <- 1:7
        2 ^ k
        
})

alternative_mean <- seq(30, 35, by = .05)

power <- sapply(sample_size,
                FUN = \(n)
                {
                        pnorm(
                                mu0 + z * sigma / sqrt(n),
                                mean = alternative_mean,
                                sd = sigma / sqrt(n),
                                lower.tail = FALSE
                        )
                })

colnames(power) <- as.character(sample_size)

df <-
        as.data.frame(cbind("altMean" = alternative_mean,
                            power)) |>
        melt(id.vars = "altMean",
             value.name = "power",
             variable.name = "sampleSize") |>
        mutate(sampleSize = ordered(sampleSize))


# Power Curve Plot --------------------------------------------------------

ggplot(data = df,
       mapping = aes(x = altMean, y = power, col = sampleSize)) +
        geom_line(size = 2) +
        geom_hline(
                yintercept = 0.05,
                color = "red",
                size = 1.5,
                linetype = "dashed"
        ) +
        geom_hline(
                yintercept = 1,
                color = "red",
                size = 1.5,
                linetype = "dashed"
        ) +
        coord_cartesian(ylim = c(0, 1), expand = T) +
        labs(
                title = "Power Curve",
                subtitle = "Significance Level: 0.95, Null Mean: 30, Sigma: 4",
                x = "Alternative Hypothesis Mean",
                y = "Power",
                col = "Sample Size"
        ) +
        scale_color_viridis_d(direction = -1,
                              option = "inferno",
                              end = .9) +
        theme_minimal() +
        theme(
                plot.title = element_text(face = "bold", hjust = .5),
                plot.subtitle = element_text(
                        face = "bold",
                        hjust = .5,
                        color = "gray"
                ),
                axis.title.x = element_text(face = "bold",
                                            color = "dimgrey"),
                axis.title.y = element_text(face = "bold",
                                            color = "dimgrey"),
                legend.title = element_text(face = "bold",
                                            color = "dimgrey"),
                legend.text = element_text(color = "gray4")
        ) 