#Analysis of historical catches reported by hunters
# CJ Brown 2022-04-27
rm(list= ls())
library(ggplot2)
library(mgcv)
library(tidyr)

dat <- readr::read_csv("Shared/Data/turtle-hunter-questions.csv")

dat2 <- dat %>% dplyr::filter(`Hunting method(s) used when started hunting` != "During breeding period of turtles. Used to go ashore to lay eggs and catch them.") %>%
  dplyr::select(Fisherman, year = `Year started catching turtles?`,
                              catch_start, catch_end) %>%
  dplyr::filter(!is.na(catch_end)) %>%
  pivot_longer(-c(1:2), names_to = "Period", values_to = "catch") %>%
  dplyr::mutate(year2 = ifelse(Period == "catch_start", year, 2018),
                catch2 = round(catch))
  


dat2$Fisherman <- factor(dat2$Fisherman)
m1 <- gam(catch2 ~ year2 + s(Fisherman, bs = "re"),
            data = dat2, 
            family = "poisson")
summary(m1)

pdat <- data.frame(year2 = 1974:2018,
                   Fisherman = dat2$Fisherman[1])
Xp <- predict(m1, newdata = pdat,
                 type = "lpmatrix",
                 exclude = "Fisherman")
Xp[,3] <- 0 #set fisherman ID to zero
pdat$catch2 <- exp(Xp %*% coef(m1))
pdat$catch2[pdat$year2==1988]/pdat$catch2[pdat$year2==2018]

exp(-coef(m1)[2]*30)

1-exp(coef(m1)[2])
1-exp(coef(m1)[2]+0.0058*1.96)
1-exp(coef(m1)[2]-0.0058*1.96)

g1 <- ggplot(dat2) + 
  aes(x= year2, y = catch2, group = Fisherman) + 
  geom_line(alpha = 0.3) +
  geom_line(data = pdat, color = "black", size = 1) + 
  xlim(1974, 2018) +
  xlab("Year") + 
  ylab("Harvest") +
  theme_classic() +
  theme(legend.position="none")
  
ggsave(plot = g1, filename = "Shared/Outputs/historic-catch-model.png",
       width = 5.5, height = 3)
