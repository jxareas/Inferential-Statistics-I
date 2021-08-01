# Packages ----------------------------------------------------------------

require(datasets)
require(reshape2)
require(dplyr)
require(ggplot2)
require(ggthemes)


# Data Wrangling ----------------------------------------------------------

data(ChickWeight)

# Weight over Time grouped by Chick & Diet
wide <- ChickWeight |>
        dcast(Diet + Chick ~ Time, value.var = "weight")

names(wide)[-c(1:2)] <-
        paste0("time", names(wide)[-c(1:2)])

wide <- wide |>
        mutate(gain = time21 - time0)



# Spaghetti Plot Grouped by Diet --------------------------------------------------

ggplot(ChickWeight, aes(
        x = Time,
        y = weight,
        colour = Diet,
        group = Chick
)) +
        geom_line(key_glyph = draw_key_timeseries) +
        stat_summary(
                aes(group = 1),
                geom = "line",
                fun = mean,
                size = 1.5,
                col = "black"
        ) +
        labs(
                title = "Weight Over Months by Diet",
                x = "Time (in Months)",
                y = "Weight",
                caption = "Dataset: ChickWeight (R)"
                
        ) +
        facet_grid(. ~ Diet) +
        scale_color_brewer(palette = "Set1") +
        theme_stata(scheme = "s1color") +
        theme(
                plot.title = element_text(face = "bold", hjust = 0.5),
                plot.caption = element_text(
                        face = "bold",
                        colour = "darkgray",
                        hjust = 1
                ),
                axis.title.x = element_text(face = "bold.italic"),
                axis.title.y = element_text(face = "bold.italic"),
                legend.position = "right"
        )



# Violin Plot -------------------------------------------------------------

ggplot(data = wide,
       mapping = aes(
               x = Diet,
               y = gain,
               col = Diet,
               fill = Diet
       )) +
        geom_violin(alpha = 0.5) +
        labs(
                title = "Weight Gain by Diet",
                x = "Diet",
                y = "Weight Gain",
                caption = "Dataset: ChickWeight (R)"
        ) +
        theme_economist_white() +
        theme(
                plot.title = element_text(face = "bold", hjust = 0.5),
                plot.caption = element_text(
                        face = "bold",
                        colour = "darkgray",
                        hjust = 1
                ),
                axis.title.x = element_text(face = "bold", color = "dimgray"),
                axis.title.y = element_text(
                        face = "bold",
                        color = "dimgray",
                        vjust = 3
                )
        )


# Difference in Means: Welch's two sample t-test --------------------------------------------------------

# We want to test whether the weight gain in diet 1 is equal to the
# weight gain in diet 4 (or not).
# H0: MU_group1 - MU_group2 = 0
# H1: Mu_group1 - MU_group2 != 0

wide <- subset(wide, Diet %in% c(1, 4))

# Confidence Interval for Unequal Variances
t <-
        t.test(
                gain ~ Diet,
                data = wide,
                paired = F,
                var.equal = F,
                mu = 0,
                alternative = "two.sided"
        )

t$conf |> as.numeric() |> round(2)
# Difference in means C.I. is entirely negative.
# Then Mu_group1 - Mu_group4 < 0, therefore Mu_group4 > Mu_group1
