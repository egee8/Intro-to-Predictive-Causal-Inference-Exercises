
#Use some kind of software (preferably R) to create a fake data set that contains 200 observations.

#Your data set should consist of a single (continuous) dependent variable and at least two independent variables (also known as factors, or predictors).

#Make sure that your independent variables predict the dependent variable, but make sure that they do not PERFECTLY predict the dependent variable --  this means you have to add some random noise to your model. Feel free to create this random noise, and your independent variables using R’s random distribution functions (e.g., rnorm, runif) or any other method you wish.

#E.g., your formula could be: 20*Age + 50*(Education)^2 - 2*Gender + 4*treatment_indicator*gender + 10*treat + treatment_indicator/(log(parents_income)) + N(0, 10)…
#This rather complex formula would work if you had already created a dataframe with age, education, gender, treatment_indicator, and parents_income. Your formula need not be quite so complex. A simple formula is fine, as long as it meets the requirements. The N(0, 10) item at the end is where you would use rnorm(200, 0, 10)...

#Devise a story about the data set — what does it describe? Write down your short story in a paragraph about 3-5 sentences long.

#Do not overthink this. The entire exercise should take you no more than 30 minutes. POST QUESTIONS ON PIAZZA!
  
#Save your data (and the code that created it, if you used code), so that we will be able to utilize your creation whenever necessary.

#An example and additional info re what I would like you to do is available here:

#https://gist.github.com/diamonaj/46ea2e2ac60d950bacdd794b71867f7f


# Number of hours of sleep student the night before
sleep <- rnorm(200, mean = 7.5, sd = 1.5)
sleep # to see the variable you created

# Number of hours of the student studied the night before
study <- runif(200, min = 0, max = 8)
study 

# The average number of minutes the student usually exercises
exercise <- rnorm(200, mean =0.5, sd=0.15)
exercise

# Test score of student
test_score <- sleep + study + exercise
test_score
hist(test_score)

# Story: We all know good self-care habits lead to a healthy brain that is ready to learn and more likely to perform well on a test.
# And studying is, of course, an important factor in getting good test scores. 
# This fake data consists of survey responses of a few habits of students who took a test in a class.

