library(ggplot2)
library(moments)

# params
# list1 type list
# list2 type list 
get_list_point_difference <- function(list1, list2) {
  len = length(list1)
  new_list = c()
  for(i in 1:len) {
    new_list = c(new_list, list1[[i]] - list2[[i]])
  }
  return(new_list)
}
# params
# length type int
# min type int 
# max type int 
generate_random_list <- function(length,min, max) {
  rnd = floor(runif(length,min=min, max=max))
  return(sort(rnd))
}
# params
# sample type list
# list type list 
get_random_geo_points <- function(sample,list) {
  len = length(sample)
  new_list = c()
  for(i in 1:len) {
    x = sample[[i]]
    new_list = c(new_list, list[[x]])
  }
  return(new_list)
}
# params
# list type list
# returns all statistical measures in a dataframe
get_statistics <- function(list) {
  min = min(list, na.rm=TRUE)
  max = max(list,na.rm=TRUE)
  mean = mean(list,na.rm=TRUE)
  standard_deviation = sd(list,na.rm=TRUE)
  rmse = sqrt(mean((list)^2,na.rm=TRUE))
  median = median(list, na.rm = TRUE)
  kurtosis = kurtosis(list,na.rm=TRUE)
  skewness = skewness(list,na.rm=TRUE)
  range = range(list,na.rm=TRUE)
  df = data.frame(min,max,mean,standard_deviation,median,kurtosis,skewness,range,rmse)
  return (df)
}
# params
# l type list
# title type string
# x_title type string
# y_title type string
create_histogramm <- function(l,title="Title",x_title="X axis", y_title="y axis") {
  values = c()
  len = length(l)
  for(i in 1:len) {
    x = l[i]
    if(is.na(x)) { 
      print("missing")
    } else {
      values = c(values,x)
    }
  }
  # ascending sort 
  values = sort(values)
  freqdata = data.frame(frequency = values)
  # Store histogram info
  freq_hist <- hist(freqdata$frequency,main=title, xlab = x_title, ylab = y_title,col="darkmagenta")
  create_cummulutive_linechart(freq_hist$counts,title)
}

create_cummulutive_linechart <- function(counts,title) {
  elevation_error <- 1:length(counts)
  cummulutive_frequency <- cumsum(counts)
  data <- data.frame(elevation_error = elevation_error,cummulutive = cummulutive_frequency)
  ggplot(data, aes(x=elevation_error, y=cummulutive_frequency)) + geom_line() + ggtitle(title) + xlab("Elevation Error (m)") + ylab("Cummulutive Frequency")
}

get_point_difference <- function(array) {
  new_list = c()
  len = length(array)
  for(i in 1:len) {
    for(j in i:len) {
      if(j != i) {
        dif = array[[i]] - array[[j]]
        new_list <- append(new_list, dif)
      }
    }
  }
  return(new_list)
}
