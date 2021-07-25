# Packages ----------------------------------------------------------------

require(statsr)
require(dplyr)
require(ggplot2)
require(data.table)

# Simulation & Data Wrangling ----------------------------------------------------------
set.seed(5042)

# Population
data(ames)

# Simulation of Random Samples from the Population: Ames
sample_means <- vector(mode = "list", length = 50)

sample_means <- 
        sample_means |> 
        lapply(
                \(v)
                {
                   t <- ames |> 
                                sample_n(size = 100, replace = T) |> 
                                select(area) |> 
                                t.test() 
                        
                        t$conf.int |> as.numeric()
                }
        )

df <- data.frame(
        "lower" = sample_means |> sapply(\(v) {v[1]}),
        "upper" = sample_means |> sapply(\(v) {v[2]}),
        "mean" = with(ames, mean(area))
)

df <- df |> 
        mutate(coverage = ifelse(mean >= lower & mean <= upper, "Yes", "No"),
            n = 1:nrow(df)) |> 
        select(-mean)


# Wide to Long Format
df <-
        melt(setDT(df),
             id.vars = c("n", "coverage"), 
             variable.name = "bound") |> 
        arrange(n)
        


# Plot --------------------------------------------------------------------

ggplot(data = df, mapping = aes(x = value, y = n,
                                group = n, col = coverage)) +
        geom_point(size = 1) + 
        geom_line() +
        geom_vline(xintercept = mean(ames$area), color = "black",
                   size = 0.5, linetype = "dashed") +
        labs(
           title = "Simulation of a hundred 95% Confidence Intervals",
           x = "Area",
           y = "n",
           col = "Coverage"
        ) +
        scale_color_manual(values = c("red", "darkgreen")) +
        theme_test() +
        theme(
                plot.title = element_text(face = "bold", hjust = .5),
                axis.title.x = element_text(face = "bold.italic", color = "dimgray"),
                axis.title.y = element_text(face = "bold.italic", color= "dimgray")
        ) + guides(col = "none") 
