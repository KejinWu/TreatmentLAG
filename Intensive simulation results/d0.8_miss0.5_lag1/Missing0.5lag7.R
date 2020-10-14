# Lag 7, Cohen'sd = 0.2, Missing = 0.5 // I run the setting N = 100, T =300 sepeartely. 
setwd("/Users/wukejin/Desktop/Research/Comprehensive simulations/Comprehensive simulation/d0.2_miss0.5_lag7")
temp = list.files(pattern="*.rda")
for (i in 1:length(temp)) assign(temp[i], load(temp[i]))

# Calculate coverage rate of U and X obtained from additional stage
All_lmerresults_d0.2_Miss0.5_lag7 #This are the results for other setting
U1Power_results_d0.2_Miss0.5_lag7
XPower_results_d0.2_Miss0.5_lag7
d0.2lag7miss0.5CoverU = c()
for (i in 1:(length(All_lmerresults_d0.2_Miss0.5_lag7)-1)) {
  d0.2lag7miss0.5CoverU = c(d0.2lag7miss0.5CoverU,All_lmerresults_d0.2_Miss0.5_lag7[[i]][2,11] )
}
d0.2lag7miss0.5CoverU = as.numeric(c(d0.2lag7miss0.5CoverU,0.942))
d0.2lag7miss0.5CoverU = matrix(d0.2lag7miss0.5CoverU,ncol = 4, nrow = 4)
d0.2lag7miss0.5CoverX = c()
for (i in 1:(length(All_lmerresults_d0.2_Miss0.5_lag7)-1)) {
  d0.2lag7miss0.5CoverX = c(d0.2lag7miss0.5CoverX,All_lmerresults_d0.2_Miss0.5_lag7[[i]][1,11] )
}
d0.2lag7miss0.5CoverX = as.numeric(c(d0.2lag7miss0.5CoverX,0.946))
d0.2lag7miss0.5CoverX = matrix(d0.2lag7miss0.5CoverX,ncol = 4, nrow = 4)
# Record the power of U and X obtained from DTVEM
d0.2lag7miss0.5PowerU = as.numeric(c(U1Power_results_d0.2_Miss0.5_lag7,1))
d0.2lag7miss0.5PowerU = matrix(d0.2lag7miss0.5PowerU,ncol = 4, nrow = 4)
d0.2lag7miss0.5PowerX = as.numeric(c(XPower_results_d0.2_Miss0.5_lag7,1))
d0.2lag7miss0.5PowerX = matrix(d0.2lag7miss0.5PowerX,ncol = 4, nrow = 4)
colnames(d0.2lag7miss0.5PowerU) = c(1,10,50,100) ; rownames(d0.2lag7miss0.5PowerU) = c(14,28,50,300)
colnames(d0.2lag7miss0.5CoverU) = c(1,10,50,100) ; rownames(d0.2lag7miss0.5CoverU) = c(14,28,50,300)
colnames(d0.2lag7miss0.5PowerX) = c(1,10,50,100) ; rownames(d0.2lag7miss0.5PowerX) = c(14,28,50,300)
colnames(d0.2lag7miss0.5CoverX) = c(1,10,50,100) ; rownames(d0.2lag7miss0.5CoverX) = c(14,28,50,300)


# # Lag 7, Cohen'sd = 0.5, Missing = 0.5 // I run the setting N =  --------
# Calculate coverage rate of U and X obtained from additional stage 
setwd("/Users/wukejin/Desktop/Research/Comprehensive simulations/Comprehensive simulation/d0.5_miss0.5_lag7")
temp = list.files(pattern="*.rda")
for (i in 1:length(temp)) assign(temp[i], load(temp[i]))

All_lmerresults_d0.5_Miss0.5_lag7 #This are the results for other setting
U1Power_results_d0.5_Miss0.5_lag7
XPower_results_d0.5_Miss0.5_lag7
d0.5lag7miss0.5CoverU = c()
for (i in 1:(length(All_lmerresults_d0.5_Miss0.5_lag7)-1)) {
  d0.5lag7miss0.5CoverU = c(d0.5lag7miss0.5CoverU,All_lmerresults_d0.5_Miss0.5_lag7[[i]][2,11] )
}
d0.5lag7miss0.5CoverU = as.numeric(c(d0.5lag7miss0.5CoverU,0.946))
d0.5lag7miss0.5CoverU = matrix(d0.5lag7miss0.5CoverU,ncol = 4, nrow = 4)
colnames(d0.5lag7miss0.5CoverU) = c(1,10,50,100) ; rownames(d0.5lag7miss0.5CoverU) = c(14,28,50,300)
d0.5lag7miss0.5CoverX = c()
for (i in 1:(length(All_lmerresults_d0.5_Miss0.5_lag7)-1)) {
  d0.5lag7miss0.5CoverX = c(d0.5lag7miss0.5CoverX,All_lmerresults_d0.5_Miss0.5_lag7[[i]][1,11] )
}
d0.5lag7miss0.5CoverX = as.numeric(c(d0.5lag7miss0.5CoverX,0.938))
d0.5lag7miss0.5CoverX = matrix(d0.5lag7miss0.5CoverX,ncol = 4, nrow = 4)
colnames(d0.5lag7miss0.5CoverX) = c(1,10,50,100) ; rownames(d0.5lag7miss0.5CoverX) = c(14,28,50,300)
# Record the power of U and X obtained from DTVEM
d0.5lag7miss0.5PowerU = as.numeric(c(U1Power_results_d0.5_Miss0.5_lag7,1))
d0.5lag7miss0.5PowerU = matrix(d0.5lag7miss0.5PowerU,ncol = 4, nrow = 4)
colnames(d0.5lag7miss0.5PowerU) = c(1,10,50,100) ; rownames(d0.5lag7miss0.5PowerU) = c(14,28,50,300)
d0.5lag7miss0.5PowerX = as.numeric(c(XPower_results_d0.5_Miss0.5_lag7,1))
d0.5lag7miss0.5PowerX = matrix(d0.5lag7miss0.5PowerX,ncol = 4, nrow = 4)
colnames(d0.5lag7miss0.5PowerX) = c(1,10,50,100) ; rownames(d0.5lag7miss0.5PowerX) = c(14,28,50,300)


# # Lag 7, Cohen'sd = 0.8, Missing = 0.5 // I run the setting N =  --------
setwd("/Users/wukejin/Desktop/Research/Comprehensive simulations/Comprehensive simulation/d0.8_miss0.5_lag7")
temp = list.files(pattern="*.rda")
for (i in 1:length(temp)) assign(temp[i], load(temp[i]))

d0.8lag7miss0.5CoverU = c()
for (i in 1:(length(All_lmerresults_d0.8_Miss0.5_lag7)-1)) {
  d0.8lag7miss0.5CoverU = c(d0.8lag7miss0.5CoverU,All_lmerresults_d0.8_Miss0.5_lag7[[i]][2,11] )
}
d0.8lag7miss0.5CoverU = as.numeric(c(d0.8lag7miss0.5CoverU,0.94))
d0.8lag7miss0.5CoverU = matrix(d0.8lag7miss0.5CoverU,ncol = 4, nrow = 4)
colnames(d0.8lag7miss0.5CoverU) = c(1,10,50,100) ; rownames(d0.8lag7miss0.5CoverU) = c(14,28,50,300)
d0.8lag7miss0.5CoverX = c()
for (i in 1:(length(All_lmerresults_d0.8_Miss0.5_lag7)-1)) {
  d0.8lag7miss0.5CoverX = c(d0.8lag7miss0.5CoverX,All_lmerresults_d0.8_Miss0.5_lag7[[i]][1,11] )
}
d0.8lag7miss0.5CoverX = as.numeric(c(d0.8lag7miss0.5CoverX,0.948))
d0.8lag7miss0.5CoverX = matrix(d0.8lag7miss0.5CoverX,ncol = 4, nrow = 4)
colnames(d0.8lag7miss0.5CoverX) = c(1,10,50,100) ; rownames(d0.8lag7miss0.5CoverX) = c(14,28,50,300)
# Record the power of U and X obtained from DTVEM
d0.8lag7miss0.5PowerU = as.numeric(c(U1Power_results_d0.8_Miss0.5_lag7,1))
d0.8lag7miss0.5PowerU = matrix(d0.8lag7miss0.5PowerU,ncol = 4, nrow = 4)
colnames(d0.8lag7miss0.5PowerU) = c(1,10,50,100) ; rownames(d0.8lag7miss0.5PowerU) = c(14,28,50,300)
d0.8lag7miss0.5PowerX = as.numeric(c(XPower_results_d0.8_Miss0.5_lag7,1))
d0.8lag7miss0.5PowerX = matrix(d0.8lag7miss0.5PowerX,ncol = 4, nrow = 4)
colnames(d0.8lag7miss0.5PowerX) = c(1,10,50,100) ; rownames(d0.8lag7miss0.5PowerX) = c(14,28,50,300)


#Plot 4D figure
xlab <- list(title = "N",titlefont = list(size = 15),tickfont = list(size = 11))
ylab <- list(title = "T",titlefont = list(size = 15),tickfont = list(size = 11))
zlab <- list( title = "Cohen's d value",titlefont = list(size = 15),tickfont = list(size = 11),tickvals = tick.vals,ticktext = tick.text)
#Coverage rate X
MatrixForColor0.2 <- d0.2lag7miss0.5CoverX
MatrixForColor0.5 <- d0.5lag7miss0.5CoverX
MatrixForColor0.8 <- d0.8lag7miss0.5CoverX
fig2 <- plot_ly(colors=rev(c(heat.colors(50))))%>%
  add_surface(z = matrix(0.2, nrow = 4, ncol = 4),
              opacity = 1,
              x = c(1,10,50,100),
              y = c(14,28,50,300),
              surfacecolor = MatrixForColor0.2,
              cauto=F,
              cmax=1,
              cmin=0
  ) %>%
  add_surface(z = matrix(0.5, nrow = 4, ncol = 4),
              opacity = 1,
              x = c(1,10,50,100),
              y = c(14,28,50,300),
              surfacecolor = MatrixForColor0.5,
              cauto=F,
              showscale=FALSE,
              cmax=1,
              cmin=0
  ) %>%
  add_surface(z = matrix(0.8, nrow = 4, ncol = 4),
              opacity = 1,
              x = c(1,10,50,100),
              y = c(14,28,50,300),
              surfacecolor = MatrixForColor0.8,
              cauto=F,
              cmax=1,
              showscale=FALSE,
              cmin=0
  )%>%
  layout(title= list(text = "Coverage rate of Xlag1 with 50% missingness", font = list(size = 16)),scene = list(xaxis=xlab,yaxis=ylab,zaxis=zlab,camera = list(eye=list(x=-2.2, y=-1.2, z=0.85))))
fig2

#Coverage rate U
MatrixForColor0.2 <- d0.2lag7miss0.5CoverU
MatrixForColor0.5 <- d0.5lag7miss0.5CoverU
MatrixForColor0.8 <- d0.8lag7miss0.5CoverU
fig1 <-plot_ly(colors=rev(c(heat.colors(50))))%>%
  add_surface(z = matrix(0.2, nrow = 4, ncol = 4),
              opacity = 1,
              x = c(1,10,50,100),
              y = c(14,28,50,300),
              surfacecolor = MatrixForColor0.2,
              cauto=F,
              cmax=1,
              cmin=0
  ) %>%
  add_surface(z = matrix(0.5, nrow = 4, ncol = 4),
              opacity = 1,
              x = c(1,10,50,100),
              y = c(14,28,50,300),
              surfacecolor = MatrixForColor0.5,
              cauto=F,
              cmax=1,
              showscale=FALSE,
              cmin=0
  ) %>%
  add_surface(z = matrix(0.8, nrow = 4, ncol = 4),
              opacity = 1,
              x = c(1,10,50,100),
              y = c(14,28,50,300),
              surfacecolor = MatrixForColor0.8,
              cauto=F,
              showscale=FALSE,
              cmax=1,
              cmin=0
  )%>%
  layout(title= list(text = "Coverage rate of Ulag7 with 50% missingness", font = list(size = 16)),scene = list(xaxis=xlab,yaxis=ylab,zaxis=zlab,camera = list(eye=list(x=-2.2, y=-1.2, z=0.85))))
fig1



#Power of X and U

#Power of X
MatrixForColor0.2 <- d0.2lag7miss0.5PowerX
MatrixForColor0.5 <- d0.5lag7miss0.5PowerX
MatrixForColor0.8 <- d0.8lag7miss0.5PowerX
fig2 <- plot_ly(colors=rev(c(heat.colors(50))))%>%
  add_surface(z = matrix(0.2, nrow = 4, ncol = 4),
              opacity = 1,
              x = c(1,10,50,100),
              y = c(14,28,50,300),
              surfacecolor = MatrixForColor0.2,
              cauto=F,
              cmax=1,
              cmin=0
  ) %>%
  add_surface(z = matrix(0.5, nrow = 4, ncol = 4),
              opacity = 1,
              x = c(1,10,50,100),
              y = c(14,28,50,300),
              surfacecolor = MatrixForColor0.5,
              cauto=F,
              showscale=FALSE,
              cmax=1,
              cmin=0
  ) %>%
  add_surface(z = matrix(0.8, nrow = 4, ncol = 4),
              opacity = 1,
              x = c(1,10,50,100),
              y = c(14,28,50,300),
              surfacecolor = MatrixForColor0.8,
              cauto=F,
              cmax=1,
              showscale=FALSE,
              cmin=0
  )%>%
  layout(title= list(text = "Power of detecting Xlag1 with 50% missingness", font = list(size = 16)),scene = list(xaxis=xlab,yaxis=ylab,zaxis=zlab,camera = list(eye=list(x=-2.2, y=-1.2, z=0.85))))
fig2

# Power of U
MatrixForColor0.2 <- d0.2lag7miss0.5PowerU
MatrixForColor0.5 <- d0.5lag7miss0.5PowerU
MatrixForColor0.8 <- d0.8lag7miss0.5PowerU
fig1 <-plot_ly(colors=rev(c(heat.colors(50))))%>%
  add_surface(z = matrix(0.2, nrow = 4, ncol = 4),
              opacity = 1,
              x = c(1,10,50,100),
              y = c(14,28,50,300),
              surfacecolor = MatrixForColor0.2,
              cauto=F,
              cmax=1,
              cmin=0
  ) %>%
  add_surface(z = matrix(0.5, nrow = 4, ncol = 4),
              opacity = 1,
              x = c(1,10,50,100),
              y = c(14,28,50,300),
              surfacecolor = MatrixForColor0.5,
              cauto=F,
              cmax=1,
              showscale=FALSE,
              cmin=0
  ) %>%
  add_surface(z = matrix(0.8, nrow = 4, ncol = 4),
              opacity = 1,
              x = c(1,10,50,100),
              y = c(14,28,50,300),
              surfacecolor = MatrixForColor0.8,
              cauto=F,
              showscale=FALSE,
              cmax=1,
              cmin=0
  )%>%
  layout(title= list(text = "Power of detecting Ulag7 with 50% missingness", font = list(size = 16)),scene = list(xaxis=xlab,yaxis=ylab,zaxis=zlab,camera = list(eye=list(x=-2.2, y=-1.2, z=0.85))))
fig1



