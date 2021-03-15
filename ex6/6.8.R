# 6-8 Exploration of Datasets in R ----------------------------------------


# >(a) --------------------------------------------------------------------
# Use the help command to get information on
# the built-in dataset AirPassengers.
help(AirPassengers)


# >>(i) -------------------------------------------------------------------
# Plot the dataset using the plot() command.
# What do you see? Describe the resulting plot.
Air <- AirPassengers
plot(Air)


# >>(ii) ------------------------------------------------------------------
# Create a histogram using the built-in function hist()
# What do you observe?
hist(Air)
print("that Frequency decrease as Air Passengers increase")

# >>(iii) -----------------------------------------------------------------
# Check the class() and mode() of the dataset. Are these as expected?
# If you are not sure what the mode and class functions do use the help() function.
class(Air)
print('Class(Air) returns "TS" which is a Time-Series Objects')
mode(Air)
print('returns "numeric" (integer and double) which is the most frequent type of data I guess?')

# >(b) --------------------------------------------------------------------
# R comes with many historical data sets. One of them is the Titanic dataset.
# Use the help() function to read about the data set.
# Then make a mosaic plot using the mosaicplot() command. What do you observe?
help("Titanic")

mosaicplot(Titanic)
print("It gives us an overview of the people who died, their gender, and ther Class.. as 1st, 2st.. class.")
