set.seed(123)


library(MASS)

# Simulation parameters
N <- 4
sigma <- matrix(c(0.05, 0, 0, 0.05), 2)
mu1 <- c(1,1)
mu2 <- c(2,1.5)

bvn1 <- mvrnorm(N, mu = mu1, Sigma = sigma )
bvn2 <- mvrnorm(N, mu = mu2, Sigma = sigma )
bvnU <- mvrnorm(2, mu = mu1, Sigma = sigma )

bvn1 <- as.data.frame(bvn1)
colnames(bvn1) <- c("x1","x2")
bvn1$class <- "class 1"
bvn2 <- as.data.frame(bvn2)
colnames(bvn2) <- c("x1","x2")
bvn2$class <- "class 2"
bvnU <- as.data.frame(bvnU)
bvnU <- bvnU[1,]
colnames(bvnU) <- c("x1","x2")
bvnU$class <- "unknown"

df <- rbind(bvn1,bvn2,bvnU)



# Plotting.
library(ggplot2)
library(viridis)
library(tikzDevice)


# Classification Problem (1-1)

p1 <- ggplot(df, aes(x=x1,y=x2,shape=class,color=class)) +
  geom_point(fill=NA, stroke=1, size=2) +
  coord_cartesian(xlim=c(0.5,2.3), ylim=c(0.8,1.9)) +
  scale_shape_manual(values=c(1, 2, 0)) +
  scale_color_viridis(end = 0.99, discrete=TRUE) +
  labs(color="Class", shape="Class", x="$x_1$", y="$x_2$") +
  theme_bw() + 
  theme(axis.ticks = element_blank(), axis.text = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_text(size = rel(0.8)),
        legend.title = element_text(size=rel(0.9)), legend.text = element_text(size = rel(0.8))
  )

# Save plot as .tex file
tikz(file = "/home/mnl/Statistik/coda/output/classification_problem.tex", 
     width = 4, height = 1.5)
plot(p1)
dev.off()


# How to Classify (1-2)
means <- as.data.frame(lapply(bvn1[,c(1,2)], mean))
means <- rbind(means, as.data.frame(lapply(bvn2[,c(1,2)], mean)))
means$class <- c("class 1", "class 2")
means$text <- c("$\\hat{\\mu}_1$", "$\\hat{\\mu}_2$")

lines <- rbind(df[9,], df[9,])
lines <- rbind(lines, means[,c(1,2,3)])
lines$group <- c(1,2,1,2)

p2 <- p1 +
  geom_line(data=lines, aes(x=x1,y=x2,group=group), color="grey60", linetype=4, size=0.5) +
  geom_point(data=means, aes(x=x1,y=x2,color=class), show.legend = FALSE, shape=4) +
  geom_text(data=means, aes(x=x1,y=x2,color=class,label=text), 
            nudge_x=0.1, nudge_y=0.05, show.legend = FALSE)

p2

# Save plot as .tex file
tikz(file = "/home/mnl/Statistik/coda/output/how_to_classify.tex", 
     width = 4, height = 1.5)
plot(p2)
dev.off()



# How To Classify (Division Line) 1-3
disc <- lines[c(1,3),]
disc$x1 <- c(2.05,0.57)
disc$x2 <- c(0,3)

# make a grid of test points
#library(tidyverse)
# No dont do that.

( -(means[1,2] - means[2,2])*3 + 0.5*(sum(means[1,c(1,2)]**2) - sum(means[2,c(1,2)]**2)) )/((means[1,c(1,2)]-means[2,c(1,2)])$x1)

annotations <- data.frame(
  xpos = c(-Inf,Inf),
  ypos =  c(Inf,-Inf),
  class = c("class 1","class 2"),
  hjustvar = c(-0.5,1.5) ,
  vjustvar = c(2.5,-1.5))


p3 <- p1 +
  geom_point(data=means, aes(x=x1,y=x2,color=class), show.legend = FALSE, shape=4) +
  geom_text(data=means, aes(x=x1,y=x2,color=class,label=text), 
            nudge_x=0.1, nudge_y=0.05, show.legend = FALSE) +
  geom_line(data=disc, aes(x=x1,y=x2,group=group), color="black", show.legend = FALSE) +
  geom_text(data = annotations, aes(x=xpos,y=ypos,hjust=hjustvar,
                                    vjust=vjustvar,label=class), show.legend = FALSE)

p3

# Save plot as .tex file
tikz(file = "/home/mnl/Statistik/coda/output/how_to_classify_division.tex", 
     width = 4, height = 1.5)
plot(p3)
dev.off()


p3 <- p1 +
  geom_point(data=means, aes(x=x1,y=x2,color=class), show.legend = FALSE, shape=4) +
  geom_text(data=means, aes(x=x1,y=x2,color=class,label=text), 
            nudge_x=0.1, nudge_y=0.05, show.legend = FALSE) +
  geom_line(data=disc, aes(x=x1,y=x2,group=group), color="black", show.legend = FALSE) +
  geom_text(data = annotations, aes(x=xpos,y=ypos,hjust=hjustvar,
                                    vjust=vjustvar,label=class), show.legend = FALSE)

p3





# Simulation parameters
N <- 75
sigma <- matrix(c(0.19, 0.19, 0.19, 0.2), 2)
mu1 <- c(0.9,1.05)
mu2 <- c(2.1,1.55)

bvn1 <- mvrnorm(N, mu = mu1, Sigma = sigma )
bvn2 <- mvrnorm(N, mu = mu2, Sigma = sigma )

bvn1 <- as.data.frame(bvn1)
colnames(bvn1) <- c("x1","x2")
bvn1$class <- "class 1"
bvn2 <- as.data.frame(bvn2)
colnames(bvn2) <- c("x1","x2")
bvn2$class <- "class 2"

df <- rbind(bvn1,bvn2)

p1 <- ggplot(df, aes(x=x1,y=x2,shape=class,color=class)) +
  geom_point(fill=NA, stroke=0.5, size=1, alpha=0.35) +
  coord_equal(xlim=c(0,3), ylim=c(0.75,2)) +
  scale_shape_manual(values=c(1, 2, 0)) +
  scale_color_viridis(end = 0.5, discrete=TRUE) +
  labs(color="Class", shape="Class", x="$x_1$", y="$x_2$") +
  theme_bw() + 
  theme(axis.ticks = element_blank(), axis.text = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_text(size = rel(0.8)),
        legend.title = element_text(size=rel(0.9)), legend.text = element_text(size = rel(0.8))
  )

# How to Classify (1-2)
means <- as.data.frame(lapply(bvn1[,c(1,2)], mean))
means <- rbind(means, as.data.frame(lapply(bvn2[,c(1,2)], mean)))
means$class <- c("class 1", "class 2")
means$text <- c("$\\hat{\\mu}_1$", "$\\hat{\\mu}_2$")


p2 <- p1 +
  geom_point(data=means, aes(x=x1,y=x2,color=class), size=4, show.legend = FALSE, shape=4) +
  geom_text(data=means, aes(x=x1,y=x2,color=class,label=text), 
            nudge_x=0.25, nudge_y=0.01, show.legend = FALSE)

# How To Classify (Division Line) 1-3
disc <- lines[c(1,3),]
disc$x1 <- c(2.24,0.7080)
disc$x2 <- c(0,3)
disc$class <- c("class 1", "class 1")

( -(means[1,2] - means[2,2])*0 + 0.5*(sum(means[1,c(1,2)]**2) - sum(means[2,c(1,2)]**2)) )/((means[1,c(1,2)]-means[2,c(1,2)])$x1)

annotations <- data.frame(
  xpos = c(-Inf,Inf),
  ypos =  c(Inf,-Inf),
  class = c("class 1","class 2"),
  hjustvar = c(-0.5,1.5) ,
  vjustvar = c(2.5,-1.5))


p3 <- p2 +
  geom_line(data=disc, aes(x=x1,y=x2,group=group), color="black", show.legend = FALSE) +
  geom_text(data = annotations, 
            aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label=class), 
            show.legend = FALSE) +
  labs(color="Class", shape="Class", x="$x_1$", y="$x_2$")

p3

# Save plot as .tex file
tikz(file = "/home/mnl/Statistik/coda/output/covariance_problem.tex", 
     width = 4, height = 1.5)
plot(p3)
dev.off()




#######################
# Decision Boundaries #
#######################


model <- lda(formula = class ~ ., data = df)


classgrid <- function(model, data, resolution = 100){
  
  data <- data[,1:2]
  
  # Create Grid
  r <- sapply(data, range, na.rm = TRUE)
  xs <- seq(r[1,1], r[2,1], length.out = resolution)
  ys <- seq(r[1,2], r[2,2], length.out = resolution)
  
  # Bind as dataframe
  g <- cbind(rep(xs, each=resolution), rep(ys, time = resolution))
  colnames(g) <- colnames(r)
  g <- as.data.frame(g)
  
  # Predict Class
  p <- predict(model, g, type = predict_type)
  g$class <- p$class
  return(g)
}

grid <- classgrid(model, df, resolution=60)

p3 <- ggplot(df, aes(x=x1,y=x2,shape=class,color=class)) +
  geom_point(fill=NA, stroke=0.5, size=1, alpha=0.7) +
  coord_equal(xlim=c(0.1,2.9), ylim=c(0.75,2)) +
  scale_shape_manual(values=c(1, 2, 0)) +
  scale_color_viridis(end = 0.5, discrete=TRUE) +
  labs(color="Class", shape="Class", x=expression(x[1]), y="$x_2$") +
  theme_bw() + 
  theme(axis.ticks = element_blank(), axis.text = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_text(size = rel(0.8)),
        legend.title = element_text(size=rel(0.9)), legend.text = element_text(size = rel(0.8))
  ) +
  geom_point(data=grid, aes(x=x1,y=x2,color=class), shape=4, alpha = 0.15, show.legend=FALSE) +
  geom_text(data = annotations, 
            aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label=class), 
            show.legend = FALSE) +
  labs(color="Class", shape="Class", x="$x_1$", y="$x_2$")

p3

ggsave(filename = "/home/mnl/Statistik/coda/output/simulation_lda.pdf",
       p3, width=4, height=1.5)
# Save plot as .tex file
#tikz(file = "/home/mnl/Statistik/coda/output/simulation_lda.tex", 
     width = 4, height = 1.5)
#plot(p3)
#dev.off()

