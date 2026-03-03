#' ---
#' title: "Motivate Bikeshare Data Analysis"
#' author: "Josh Miller"
#' output: html_document
#' ---

#' # EXPLORE BIKESHARE DATA

#' In this project I will be using R to analyze Motivate bikeshare data from 
#' three locations in the United States:
#' 
#'   - Washington, D.C.
#'   
#'   - Chicago
#'   
#'   - New York City
#' 
#' My analysis will answer three questions:
#' 
#'   - What are the busiest stations in each city?
#'   
#'   - Which city has the highest percentage of rides by subscribers?
#'   
#'   - What gender takes the most trips? Longest average trip?

# import plotting library
library(ggplot2)

# read in data sets
bs.wdc <- read.csv('../Datasets/washington.csv')
bs.chi <- read.csv('../Datasets/chicago.csv')
bs.nyc <- read.csv('../Datasets/new-york-city.csv')

#' ## Question 1: What are the busiest stations in each city?

#' The busiest stations in each city will be the one that shows up the most as
#' either a start or end location, so we will be adding the counts of both
#' columns for each station and find the top ten values for each city.

#' I will write a function to calculate this so I don't have to write the same
#' code block three times.

busiest_stations <- function(bs.data){
  
# First I will read in the two station name columns into tables, which will
# produce counts of how many times all the stations have been recorded as
# either start (departure) or end (arrival) stations.
  
  station.start <- table(bs.data$Start.Station)
  station.end <- table(bs.data$End.Station)
  
# Next I want to setup data frames for the start and end station counts,
# because they are easier to work with when we need to add rows or columns
# later on (which we will). The count values in the tables above will be 
# stored as vectors, and the names of each station from each table will be
# stored as well, with both variables forming the two columns in each data
# frame - name of the station, and how many times it was arrived at / 
# departed from.
  
  freq.start <- c(station.start)
  names(freq.start) <- NULL
  station.start.df <- data.frame(
    station = names(station.start),
    departures = freq.start
  )

  freq.end <- c(station.end)
  names(freq.end) <- NULL
  station.end.df <- data.frame(
    station = names(station.end),
    arrivals = freq.end
  )
  
# The main reason I put these into data frames is that if the original tables
# were bound, I found that there is a misaligned section of indices that
# caused some values to be assigned to the wrong stations because some 
# stations are only ever left from or arrived to, so they don't show up in 
# the frequency tables and values get shifted in the merge. My solution
# is to write a couple of loops that will find stations that were either only
# departed from or arrived at, and add those to the other data frame with 
# values of 0 manually assigned, which will allow us to align our values
# correctly and easily calculate total traffic values for each station.
  
  unmatched.start <- list()
  unmatched.end <- list()
  station.start.df.full <- station.start.df
  station.end.df.full <- station.end.df

  for (n in station.start.df$station){
    if (!(n %in% station.end.df$station)){
      cat("Unmatched: ", n, "\n")
      unmatched.start <- append(unmatched.start, 
                                c(which(station.start.df$station == n)))
    }
  }

  for (n in station.end.df$station){
    if (!(n %in% station.start.df$station)){
      cat("Unmatched: ", n, "\n")
      unmatched.end <- append(unmatched.end, 
                              c(which(station.end.df$station == n)))
    }
  }

  for(s in unmatched.start){
    new.row.start <- data.frame(station = station.end.df$station[s],
                                departures = 0)
    station.start.df.full <- rbind(station.start.df.full, new.row.start)
  }

  for (e in unmatched.end){
    new.row.end <- data.frame(station = station.start.df$station[e],
                              arrivals = 0)
    station.end.df.full <- rbind(station.end.df.full, new.row.end) 
  }
  
# The data frames are complete sets now, so we can sort them alphabetically
# and merge so that each row shows a station name, how many times it was
# departed from, and how many times it was arrived at.

  station.start.df.full <- sort_by.data.frame(station.start.df.full, 
                                              station.start.df.full$station)

  station.end.df.full <- sort_by.data.frame(station.end.df.full,
                                            station.end.df.full$station)

  station.busy <- merge(station.start.df.full, station.end.df.full, 
                        by = 'station')
  
# Then we can add a total column to show total traffic values for each station.

  total.col <- data.frame(total = station.busy$departures +
                                  station.busy$arrivals)
  
  station.busy <- cbind.data.frame(station.busy, total.col)
  
# Our busiest stations will be the ones with the highest values in the total
# column, so let's descending sort the data frame by that column.

  station.busiest <- sort_by.data.frame(station.busy, station.busy$total, 
                                        decreasing=TRUE)
  
# In practicing plot production on this data set, I found that the station
# names, which are often cross streets, were quite long and made the bottom
# margin of the plot look weird, or the names would run off the side/bottom
# of the plot entirely. To help remedy this, I inserted newlines after any
# slashes or ampersands in the station names. The other fixes lie in the
# plotting function.

  station.busiest$station <- gsub("/", "/\n", station.busiest$station)
  station.busiest$station <- gsub("&", "&\n", station.busiest$station)

# Now we can return the cleaned and completed data frame for the city named
# in the parameters of this function.
  
  return(station.busiest)
}

#' I will also write a function to plot the top ten busiest stations in each 
#' city. This function will take the data frame returned by the above function,
#' a string to tile the plot with, and outline and fill colors for the bar 
#' graphs it produces. On top of the bar plot, I am adding line plots to show
#' how departures and arrivals contribute to each station's total traffic, which
#' hopefully will show some interesting and potentially insightful gaps in flow 
#' at some stations.

busiest_stations_plot <- function(bs.busiest.data, plot.name, 
                                  outline_color, fill_color){
  ggplot(
    aes(x = reorder(station, total, decreasing=TRUE), # reordering bars to hi-lo
        y = total), 
    data = head(bs.busiest.data, 10) # only want top ten results to show
    ) +
    geom_col(
      color = outline_color, 
      fill = fill_color
    ) +
    xlab(
      "Station Location"
    ) +
    ylab(
      "Total Traffic"
    ) +
    ggtitle(
      plot.name
    ) +
    theme(
      axis.text.x = element_text(
        angle = 50, # angle adjustment required to fit some station names
        hjust = 1, 
        vjust = 1, 
        family = 'mono', # most legible for angled text at this size
        face='bold'),
      plot.title = element_text(
        hjust = .5, 
        family = 'mono',
        face = 'bold')
    ) +
    geom_line( # arrivals line plot
      aes(x = station, y = arrivals, 
          group = 1, color = 'Arrivals'), 
      data = head(bs.busiest.data, 10),
      linewidth = 1.5,
      linetype = 3
    ) +
    geom_line( # departures line plot
      aes(x = station, y = departures,
          group = 1, color = 'Departures'),
      data = head(bs.busiest.data, 10),
      linewidth = 1.5,
      linetype = 3
    ) +
    guides(
      color = guide_legend(
        title = "Type of Traffic"
      )
    )
}

#' Now let's see what these data frames and plots can tell us. I used colors 
#' associated with each city's flag to customize and differentiate the
#' appearance of their plots. I will save the plots to my project folder after
#' generating them as well, in case I want to put them in a different report
#' later on.
#' 
#' ### Washington, D.C.

wdc.busiest <- busiest_stations(bs.wdc)

wdc.plot <- busiest_stations_plot(wdc.busiest, 
            "Busiest Motivate Bikeshare Stations - Washington, D.C.",
            '#E91436', '#FFFFFF')

head(wdc.busiest, 10)
wdc.plot

#' The busiest station in Washington, D.C. is at Columbus Circle / Union Station
#' with almost 12,000 trips beginning or terminating there. 

ggsave('../Visualizations/Motivate-Washington_Busiest-Stations.png', 
       plot=wdc.plot)

#' ### Chicago

chi.busiest <- busiest_stations(bs.chi)

chi.plot <- busiest_stations_plot(chi.busiest,
            "Busiest Motivate Bikeshare Stations - Chicago",
            '#FF0000', '#B3DDF2')

head(chi.busiest, 10)
chi.plot

#' The busiest station in Chicago is by far Streeter Dr & Grand Ave, with over 
#' 14,000 trips beginning or ending there.

ggsave('../Visualizations/Motivate-Chicago_Busiest-Stations.png', 
       plot=chi.plot)

#' ### New York City

nyc.busiest <- busiest_stations(bs.nyc)

nyc.plot <- busiest_stations_plot(nyc.busiest,
            "Busiest Motivate Bikeshare Stations - New York City",
            '#FF6600', '#003585')

head(nyc.busiest, 10)
nyc.plot

#' Finally, New York City's busiest station, Pershing Square North, logs a 
#' little over 6,000 trip starts and ends.

ggsave('../Visualizations/Motivate-NYC_Busiest-Stations.png', 
       plot=nyc.plot)

#' ## Question 2: Which city has the highest percentage of rides by subscribers?

#' In this section we will find the city with the highest proportion of rides
#' completed by customers paying a regular subscription by finding the sub-
#' scriber ratio in each city and comparing the three values.

#' To start, I will put the city names and respective data sets into a list, 
#' so that we can easily iterate through the subscriber ratio calculation later.

cities <- list(name = c('Washington, D.C.', 'Chicago', 'New York City'), 
               bs.data = list(bs.wdc, bs.chi, bs.nyc))

#' Next, I will set up an empty data frame with the column names I wish to use.

user.type.df <- data.frame(
  city = NULL,
  customers = NULL,
  subscribers = NULL,
  pct_customers = NULL,
  pct_subscribers = NULL
)

#' Here is a function I made to calculate the percent of riders. It takes the
#' name of a city, its data set, and the data frame I built above as parameters.
#' It will start by counting the number of each user type in a table, then use
#' the values in that table to calculate the pct of rides by subscribed users.
#' Finally, it will store the new row of data in the provided data frame and
#' return the updated data frame.

pct_rides_subbed <- function(city, bs.data, df){
  city.tbl <- table(bs.data$User.Type)
  pct.sub <- round(100 * (city.tbl[['Subscriber']] / nrow(bs.data)), digits=2)
  pct.cust <- 100 - pct.sub

  add.city <- data.frame(city = city, 
                         customers = city.tbl[['Customer']],
                         subscribers = city.tbl[['Subscriber']],
                         pct_customers = pct.cust,
                         pct_subscribers = pct.sub)

  user.count.df <- rbind(df, add.city)
  
  return(user.count.df)
}

#' I will now use this function in a loop to add the three cities and their 
#' values to the data frame.

for (i in c(1:lengths(cities)[['name']])){
  target.city <- cities$name[i]
  target.data <- cities$bs.data[[i]]
  user.type.df <- pct_rides_subbed(target.city, target.data, user.type.df)
}

#' The city with the most rides by subscribers will be the max value in the 
#' `pct_subscribers` column of the data frame. I will store the value and name
#' of this result in their own variables to make printing the result easier.

top.sub.pct <- max(user.type.df$pct_subscribers)

top.sub.city <- user.type.df$city[which(
                                    user.type.df$pct_subscribers == top.sub.pct
                                    )]

cat("City with highest percent of rides by subscribers: ", top.sub.city,
    " (", top.sub.pct, "%)", sep="")

#' Let's make a plot to drive this result home visually.

pct.sub.plot <- ggplot(
  aes(x = city, y = pct_subscribers + pct_customers, fill = city, color = city),
  data = user.type.df) +
  geom_col(fill = "#FFFFFF") + # first plot layer will be a bar = 100%
  xlab("City") +
  scale_y_continuous(
    name = "Percent of Rides",
    breaks = (seq(0, 100, 20)), 
                     labels = scales::label_percent(scale=1)
  ) +
  
  # manually setting color values to match city flags again
  scale_color_manual(values = c("Chicago" = "#FF0000", 
                               "New York City" = "#003585",
                               "Washington, D.C." = "#E91436")) +
  scale_fill_manual(values = c("Chicago" = "#B3DDF2", 
                               "New York City" = "#FF6600",
                               "Washington, D.C." = "#E91436")) +
  
  # adding plot layer to show percent of rides by subscribed users
  geom_col(aes(x = city, y = pct_subscribers, fill = city, color = city),
           data = user.type.df,
           linewidth = 1) + 
  
  # legend is redundant
  theme(legend.position = "none",
        plot.title = element_text(hjust=.5)) +
  ggtitle("Percent of Total Rides by User Type - Subscriber")

pct.sub.plot

#' We can see that New York City has the highest ratio of rides by subscribers.

ggsave('../Visualizations/Motivate_Percent-Rides-By-Subscriber.png', 
       plot=pct.sub.plot)

#' ## Question 3: What gender takes the most trips? Longest average trip?

#' This two-part question is answered using pretty simple analysis. Note 
#' this section only uses Chicago and New York City data, as they record user
#' gender and Washington, D.C. does not.

trips <- rbind(bs.chi, bs.nyc)

#' These tables include some empty Gender values, presumably some customers
#' prefer not to share their gender in their account. We will filter the data
#' to only include rows with complete gender information.

trips <- subset(trips, (Gender == 'Male' | Gender == 'Female'))

#' The first part of question will just need a simple count of how many trips
#' are recorded by each gender, which we can find by putting the Gender column
#' of the full data into a table.

trips.g <- table(trips$Gender)

#' Similar to before, I am storing the name of the gender and the count in 
#' separate variables to make writing my cat() statement easier.

most.trips.gender <- names(trips.g)[which.max(trips.g)]
most.trips.amt <- trips.g[which.max(trips.g)]

cat("Gender which takes the most trips: ", most.trips.gender, " (", 
    most.trips.amt, ")", sep="")

most.trips.plot <- ggplot(aes(x=Gender, fill = Gender), 
                     data=trips) +
  geom_bar() +
  ggtitle("Total Trips Recorded by Gender") +
  scale_y_continuous(labels = scales::comma) +
  theme(legend.position = 'none',
        plot.title = element_text(hjust = 0.5))

most.trips.plot

#' The bar graph shows that it wasn't very close, male users recorded 
#' significantly more trips than female users.

#' For the second part of this question, since we need both gender and trip
#' duration data, and we are looking for an average, I'll use a by() statement 
#' to filter our data. I am going to use median as my measure of middle here 
#' because, as will be seen in the box plot later, both genders have several
#' outliers dragging their mean values up. Here's what the summary for this data
#' looks to give an idea:

by(trips[,"Trip.Duration"], trips[,"Gender"], summary)

#' Those are some incredibly high max values, and the mean is ~200 seconds 
#' higher than the median in both cases. I trust the median to be a better fit. 

trips.d.med <- by(trips[,"Trip.Duration"], trips[,"Gender"], median)

#' Again, storing name of gender + value separately.

longest.trips.gender <- names(trips.d.med)[which.max(trips.d.med)]
longest.trips.amt <- max(trips.d.med)

cat("Gender which takes the longest average trip: ", longest.trips.gender, " (",
    longest.trips.amt, " seconds)", sep="")

#' Let's look at the boxplots for these results. I brought the y limit of the 
#' plot in to trips lasting 40 minutes, because that's what cuts out enough 
#' outliers that the box plot results are actually legible.

longest.trips.boxplot <- ggplot(aes(x=Gender, y = Trip.Duration), 
                        data=trips) +
  geom_boxplot() +
  scale_y_continuous(breaks = seq(0, 2400, 300)) +
  coord_cartesian(ylim = c(0, 2400)) +
  ggtitle("Quartile Comparisons of Trip Duration by Gender") +
  ylab("Trip Duration (seconds)") + 
  theme(plot.title = element_text(hjust = 0.5))

longest.trips.boxplot

#' We can see all the quartiles for female users are higher than for male, so
#' it looks like our results printed earlier check out.

#' I also want to look at histograms for each gender to get a real sense of 
#' scale for how many of these longer trips female users are taking compared to 
#' male users. I'll be overlaying the medians for each as vertical lines to add 
#' context too.

longest.trips.hist <- ggplot(aes(x=Trip.Duration, color = Gender), 
                             data=trips) +
  geom_histogram(binwidth = 60, position = "dodge") +
  scale_x_continuous(breaks = (seq(0, 2400, 300))) +
  coord_cartesian(xlim = c(60,2400)) +
  xlab("Trip Duration (seconds)") +
  ggtitle("Spread of Trip Durations by Gender w/ Median Line") +
  geom_vline(aes(xintercept = trips.d.med["Male"]), 
             data = subset(trips, Gender=="Male"), 
             color = '#00bfc4',
             linewidth = 1.5) +
  geom_vline(aes(xintercept = trips.d.med["Female"]), 
             data = subset(trips, Gender=="Female"), 
             color = '#f8766d', 
             linewidth = 1.5) +
  facet_wrap(~Gender) +
  theme(legend.position = 'none',
        plot.title = element_text(hjust = 0.5))

longest.trips.hist

#' These right-skewed histograms really help illustrate that the majority of 
#' trips are 10 minutes long or less, and because male users make more trips
#' by a significant amount, it makes sense that their median (and mean) are
#' lower than those of female users.

ggsave('../Visualizations/Motivate_Gender-Trip-Count.png', 
       plot = most.trips.plot)
ggsave('../Visualizations/Motivate_Gender-Trip-Duration_BP.png', 
       plot = longest.trips.boxplot)
ggsave('../Visualizations/Motivate_Gender-Trip-Duration_Hist.png', 
       plot = longest.trips.hist)