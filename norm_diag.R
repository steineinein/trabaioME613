library(tidyverse)
library(qqplotr)
library(gridExtra)

normal_diag <- function(fit.model){
  student_residuals <- tibble(residual = rstudent(fit.model),
                              fit = fitted(fit.model),
                              index = names(rstudent(fit.model)) %>% as.numeric())
  residual_index <- student_residuals %>% ggplot(aes(x = index, y = residual)) +
    geom_point() + geom_hline(yintercept = c(-2,0,2),linetype = "dashed") +
    labs(x="Índice", y="Resíduo Studentizado")
  residual_fitted <- student_residuals %>% ggplot(aes(x = fit, y = residual)) +
    geom_point() + geom_hline(yintercept = c(-2,0,2),linetype = "dashed") +
    labs(x="Valores Ajustados", y="Resíduo Studentizado")
  histbase <- hist(student_residuals$residual, plot = FALSE)
  residual_histogram <- student_residuals %>% ggplot(aes(x = residual)) +
    geom_histogram(aes(y = ..density..),
                   bins = histbase$breaks %>% length(),
                   breaks=histbase$breaks,
                   color="black", fill="white",center=0) +
    labs(y="Densidade", y="Resíduo Studentizado")
  residual_qqplot <- student_residuals %>% ggplot(aes(sample = residual)) +
    stat_qq_band(bandType = "boot", alpha = 0.4) +
    stat_qq_line(colour = "#8DA0CB") +
    stat_qq_point() + labs(y ="Resíduo Studentizado", x = "Percentil da N(0,1)")
  grid.arrange(residual_index, residual_fitted, residual_histogram, residual_qqplot)
}
