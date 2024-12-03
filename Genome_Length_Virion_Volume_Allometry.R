library("ggplot2")

data <- read.csv("/cloud/project/Cui_etal2014.csv")

## Row & Column Numbers 
dim(data)

## Subset data 
length_volume_subset <- data[, c(12, 10)]

## Log data and produce new logged data frame
log_genome_length <- log(as.numeric(length_volume_subset[,1]))
log_virion_volume <- log(as.numeric(length_volume_subset[,2]))
logged_data <- data.frame(
  loggenome_length = log_genome_length, 
  log_virion_volume = log_virion_volume) 


## Constructing linear model and finding α and β values
lm_model <- lm(log_virion_volume ~ log_genome_length) 
summary(lm_model)
alpha <- exp(coef(lm_model)[1])
beta <- coef(lm_model)[2]

##Relplicating the plot using ggplot()

ggplot(logged_data, aes(x = log_genome_length, y = log_virion_volume)) + 
  geom_point(color = "black", size = 2) +  
  labs(
    x = "log [Genome length (kb)]", 
    y = "log [Virion volume (nm3)]") +
  theme_bw() +
  theme(
    axis.title.x = element_text(face = "bold", size = 10),
    axis.title.y = element_text(face = "bold", size = 10)) +
  geom_smooth(method = "lm", se = TRUE, color = "royalblue2")


##Predicting y-values

x_predict_input <- data.frame(log_genome_length = 300)
predicted_y <- predict(lm_model, newdata = x_predict_input)
print(predicted_y)

