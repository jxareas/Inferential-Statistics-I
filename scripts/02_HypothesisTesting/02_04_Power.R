### Packages ----------------------------------------------------------------

library(ggplot2)
library(reshape2)
library(dplyr)


# Calculating Type I and Type II Error Rate -------------------------------------------

# Parameters to calculate Power
alpha = 0.05
mu0 = 30
mua = 32
sigma = 4
z = qnorm(1 - alpha)
n = 16

# Type I Error Rate: Alpha
alpha <- pnorm(
        q = mu0 + z*sigma/sqrt(n),
        mean = mu0,
        sd = sigma/sqrt(n),
        lower.tail = F
)

# Power of a Test
beta_complement <- pnorm(
        q = mu0 + z*sigma/sqrt(n),
        mean = mua,
        sd = sigma/sqrt(n),
        lower.tail = F 
)

# Type II Error Rate: Beta
beta <- 1 - beta_complement



### Calculating Power -------------------------------------------------------

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