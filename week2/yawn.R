# a)

yawn <- rep('yawn',14)
not_yawn <- rep('not yawn',36)
yawn_total <- append(yawn,not_yawn)
dist <- rep(0,1e5)

for (i in 1:1e5){
  yawn_total <- sample(yawn_total)
  control <- yawn_total[1:16]
  treatment <- yawn_total[17:50]
  
  control_count <- 0
  for(j in 1:16){
    if(control[j] == 'yawn'){
      control_count <- control_count + 1
    }
  }
  control_ratio <- control_count / length(control)

  treatment_count <- 0
  for(k in 1:34){
    if(treatment[k] == 'yawn'){
      treatment_count <- treatment_count + 1
    }
  }
  treatment_ratio <- treatment_count / length(treatment)
  
  dist[i] <- treatment_ratio - control_ratio

}

ggplot(data.frame(dist), aes(x = dist)) +
  geom_histogram(binwidth = 0.02)

# b)

(10/34) - (4/16) # 0.04411765 or a 4.41% difference

# c)
sum(dist > ((10/34) - (4/16))) / 1e5 # p-value = 0.51424 
1 - pnorm(0.04411765, mean = mean(dist), sd = sd(dist)) 
# p-value = 0.375

# d)
# I do not have sufficient evidence to reject the null 
# distribution because my p-value is greater than 0.05

# e)
library(pwr)
power.prop.test(n = 25, 
                p1 = 10/34, 
                p2 = 4/16,
                sig.level = 0.05,
                power = NULL,
                alternative = c("one.sided")
)
# Power = 0.0974

# f)
power.prop.test(n = NULL, 
                p1 = 10/34, 
                p2 = 4/16,
                sig.level = 0.05,
                power = 0.8,
                alternative = c("one.sided")
)
# Experiment should be rerun with a sample size of 1258 per group
