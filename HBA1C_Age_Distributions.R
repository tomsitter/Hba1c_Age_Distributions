#Create Reports for Age and Hb A1C distributions 

#Read files
input_file = file.choose();
output_dir = dirname(input_file)
filename = basename(input_file)

#From tools library
#Removes file extensions
file_path_sans_ext <- function (x, compression = FALSE) 
{
  if (compression) 
    x <- sub("[.](gz|bz2|xz)$", "", x)
  sub("([^.]+)\\.[[:alnum:]]+$", "\\1", x)
}
#Remove file extension to reuse filename/path for output
base_filename = file_path_sans_ext(input_file);

#Read in the data as a dataframe
data = read.csv(input_file)

#Helper function to convert HbA1Cs to numeric
as.numeric.factor <- function(x) {suppressWarnings(as.numeric(levels(x))[x])}

#Convert to numeric
hba1c = as.numeric.factor(data$Hb.A1C)

#Plot histogram
#Save as filename + "_hba1c_distribution"
png(filename=paste(base_filename, "_hba1c_distribution.png"));
par(mar=c(5,4,3,2))
hist_info <- hist(hba1c, col="firebrick", labels=TRUE, main="Hb A1C Distribution", xlab="Hb A1C", ylab="# of Patients", axes=FALSE)
#Round max patient count up to nearest 5 for y-axis
nearestFive = ceiling(max(hist_info$counts)/5)*5
axis(side=1, at=seq(0, 0.15, by=0.01))
axis(side=2, at=seq(0, nearestFive, by=5))
#This performs the save
dev.off()

#Convert age to numeric
age = as.numeric(data$Age)

#Plot histogram
#Save as filename + "_age_distribution"
png(filename=paste(base_filename, "_age_distribution.png"));
par(mar=c(5,4,3,2))
hist_info <- hist(age, col="firebrick", labels=TRUE, breaks=seq(10, 100, l=12), main="Diabetic Age Distribution", axes=FALSE, xlab="Age", ylab="# of Patients")
#Round max patient count up to nearest 5 for y-axis
nearestFive = ceiling(max(hist_info$counts)/5)*5
axis(side=1, at=seq(10, 100, by=5))
axis(side=2, at=seq(0, nearestFive, by=5))
#This performs the save
dev.off()

winDialog(type="ok",
          sprintf("Finished! You can find the files in %s", output_dir));

