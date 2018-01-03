# R subroutines for reading and visualizing benchmark results.

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(sfsmisc)
})

## R library routines for analyzing benchmark results
bench.read <- function(filename) {
  data <- read.csv(filename)
  ## baseline is the mean performance of the "A" version
  baseline <- data %>%
    filter(letter=="A") %>%
    group_by(benchmark) %>%
    summarize(baseline = mean(cycles))
  ## Add 'relative' performance column: compared to mean from baseline branch
  relative <- data %>%
    left_join(baseline, by="benchmark") %>%
    group_by(benchmark, version) %>%
    mutate(relative = first(baseline) / cycles)
  return(relative)
}

## Jitter plot faceted by benchmark
bench.jitterplot <- function(data) {
  ggplot(aes(y=relative, x=version, color=version), data=data) +
    geom_jitter(shape=1, alpha=0.5) +
    scale_y_continuous(breaks=seq(0, 3, 0.1), labels=scales::percent) +
    theme(aspect.ratio = 1) +
    theme(axis.text.x = element_text(angle=90)) +
    ylab("Performance relative to baseline average") +
    ggtitle("Comparative performance between RaptorJIT versions") +
    facet_wrap(~ benchmark, scales="free_x")
}

## ECDF plot faceted by benchmark
bench.ecdfplot <- function(data) {
  ggplot(aes(x=relative, color=version), data=data) +
  stat_ecdf() +
  scale_x_continuous(labels=scales::percent) +
  scale_y_log10(labels=scales::percent) +
  theme(aspect.ratio = 1) +
  theme(axis.text.x = element_text(angle=90)) +
  ylab("Performance relative to baseline average") +
  xlab("Percentage of results at or above this performance level") +
  ggtitle("Comparative performance between RaptorJIT variants") +
  facet_wrap(~ benchmark)
}

bench.significance <- function(d) {
    f <- function(data) {
        n <- length(data)
        D <- KSd(n)
        ec <- ecdf(data)
        x <- get("x", envir = environment(ec))
        y <- get("y", envir = environment(ec))
        tibble(x = x,
               ymax = pmin(y+D, 1),
               ymin = pmax(y-D, 0))
    }

    data <- tibble()
    for (v in levels(as.factor(d$version))) {
        new <- f(filter(d, version == v)$cycles)
        new$version <- v
        data <- rbind(data, new)
    }
    ggplot(data = data) +
        geom_ribbon(aes(x=x, ymin=ymin, ymax=ymax, color=version, fill=version),
                       alpha=0.25) +
        scale_x_continuous(labels = scales::comma) +
        scale_y_continuous(labels = scales::percent) +
        labs(y = "% benchmarks completed",
             x = "elapsed time (cycles)",
             title = "Branch-wise RaptorJIT benchmark CDF",
             subtitle = "95% confidence band for Cumulative Distribution Function (CDF) based on Kolmogorov-Smirnov statistic")
}
