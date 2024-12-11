library(ggplot2)
library(gridExtra)

brownian_motion  <- function (n_steps, seed = 12345, rnorm_mean = 0,
                              step_sd = 1, drift = 0) {
  
  df <- data.frame(x = numeric(n_steps), y = numeric(n_steps),  
                    time = seq_len(n_steps)) 
  
  set.seed(seed)
  
  df[1,] <- c(0,0,1)
  
  h_x <- rnorm(n_steps - 1, mean = rnorm_mean + drift, sd = step_sd)
  h_y <- rnorm(n_steps - 1, mean = rnorm_mean + drift, sd = step_sd)
  
  angle_change <- rnorm(n_steps - 1, mean = rnorm_mean, sd = step_sd)
  cos_offsets <- cos(angle_change)
  sin_offsets <- sin(angle_change)
  
  x_steps <- cos_offsets + h_x
  y_steps <- sin_offsets + h_y
  
  x_positions <- c(0, cumsum(x_steps))
  y_positions <- c(0, cumsum(y_steps))
  
  df$x <- x_positions
  df$y <- y_positions
  
  return(df)

}
  


data1 <- brownian_motion(500)

plot1 <- ggplot(aes(x = x, y = y), data = data1) +
  
  geom_path(aes(colour = time)) +
  
  theme_bw() +
  
  xlab("x-coordinate") +
  
  ylab("y-coordinate")

data2 <- brownian_motion(500)

plot2 <- ggplot(aes(x = x, y = y), data = data2) +
  
  geom_path(aes(colour = time)) +
  
  theme_bw() +
  
  xlab("x-coordinate") +
  
  ylab("y-coordinate")

grid.arrange(plot1, plot2, ncol=2)

