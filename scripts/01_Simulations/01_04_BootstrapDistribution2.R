# Packages ----------------------------------------------------------------

library(UsingR)
library(ggplot2)
library(boot)

# Data --------------------------------------------------------------------
set.seed(287)
data(father.son)

sample <- father.son$sheight
m <- length(sample) # Size of each column vector (Sample)

median <- median(sample)


# Bootstrapping -----------------------------------------------------------
n <- 1e5 # N of column vectors. Each one represents a sample.

resamples <- matrix(data = sample(x = sample, m * n, replace = T),
                    nrow = m,
                    ncol = n)

resampledMedians <- apply(resamples, 2, median)


# Plot: Boostrap Distribution of the Sample Median ------------------------

ggplot(mapping = aes(x = resampledMedians)) +
        geom_histogram(color = "black", fill = "skyblue") +
        geom_vline(xintercept = median, color = "red",
                   size = 1.4) +
        annotate(
                "text", label = "Sample Median",
                x = median + .15, y = 25e3, size = 4, colour = "red"
        ) +
        labs(
                title = "Bootstrap Distribution of the Sample Median",
                x = "Bootstrapped Sample Median",
                y = "Frequency"
        ) +
        theme_light() +
        theme(
                plot.title = element_text(face = "bold", hjust = .5),
                axis.title.x = element_text(face = "bold",
                                            color = "dimgrey"),
                axis.title.y = element_text(face = "bold",
                                            color = "dimgrey")
        )


# Calculating the Bootstrap CI using the Boot Package ----------------------------

bootMedian <- function(data, indices) median(data[indices])

b <- boot(sample, bootMedian, R = 5e3)

bci <- 
        boot.ci(b, conf = .95,
                type = c("norm","perc", "bca"))

