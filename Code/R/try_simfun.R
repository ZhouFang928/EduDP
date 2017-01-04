# From http://stats.stackexchange.com/questions/11233/how-to-simulate-data-based-on-a-linear-mixed-model-fit-object-in-r

library(nlme)

fm2 <- lme(distance ~ age + Sex, data = Orthodont, random = ~ 1) 

simfun <- function(n) {
    # n is the number of subjects, total rows will be 4*n

    # sig.b0 is the st. dev. of the random intercepts
    # it might be easier to just copy from the output
    sig.b0 <- exp(unlist(fm2$modelStruct$reStruct))*fm2$sigma
    b0 <- rnorm(n, 0, sig.b0)

    sex <- rbinom(n, 1, 0.5)  # assign sex at random

    fe <- fixef(fm2)
    my.df <- data.frame( Subject=rep(1:n, each=4), 
        int = fe[1] + rep(b0, each=4), 
        Sex=rep(sex,each=4), age=rep( c(8,10,12,14), n ) )
    my.df$distance <- my.df$int + fe[2] * my.df$age + 
        fe[3]*my.df$Sex + rnorm(n*4, 0, fm2$sigma)

    my.df$int <- NULL
    my.df$Sex <- factor( my.df$Sex, levels=0:1,
        labels=c('Male','Female') )
    my.df
}

Orthodont2 <- simfun(100)

head(Orthodont)

str(Orthodont)

class(Orthodont)
class(Orthodont2)

Orthodont2 <- groupedData(distance ~ age | Subject,
                          data=Orthodont2,
                          FUN=mean,
                          outer=~Sex,
                          labels=list(x="Age",
                                      y="Distance from pituitary to pterygomaxillary fissure"),
                          units=list(x="(yr)", y="(mm)"))

head(Orthodont2)
dim(Orthodont2)
class(Orthodont2)

fm3 <- lme(distance ~ age + Sex, data=Orthodont2, random=~ 1)

# Perhaps Orthodont2 can be the start of a simulated dataset for modelling.
