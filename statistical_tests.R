# Welcome to the final exam of Biostatistics 1BI043 Fall 2020!

###############################################################################
# Instructions:
#
# Read carefully.
# Write your answer below each question. Comment only if requested, otherwise
# code producing the requested output is enough.
# Unless otherwise stated, you can use any R function you like in your answers.
# You have three hours to complete the exam.
###############################################################################

###############################################################################
# Evaluation:
#
# Your teacher will only evaluate this R script.
# For each question you will get a mark between 0 and 2.
# There are 15 questions, so the maximum is 30 points. You pass with 18.
# You pass with distinction with 24.
# Good luck!
###############################################################################

###############################################################################
# Study description:
#
# Every year, several deaths are caused by accidental ingestion of Amanita
# Phalloides, which is by far the most poisonous mushroom in European countries.
# The species is known to contain two main groups of toxins: the amatoxins and
# the phallotoxins. In this study, we investigate the effect of poisoning from
# Amanita Phalloides in Sweden, using data from the national registers of
# hospitalizations.
###############################################################################

###############################################################################
# Variable descriptions:
#
# In 2000-2015, 598 patients were hospitalized in Sweden after eating A.
# Phalloides. We recorded their sex, age, BMI, and estimated the ingested
# quantity ('mushroom_quantity', fresh grams). The data also include the time
# between ingestion and the first treatment at the hospital ('time_to_treat').
# We measured the blood concentration of the toxins ('tox', expressed in mg/L).
# We recorded whether the patient was liver transplanted, and whether he/she
# survived.
###############################################################################


# Q0. Read the data by running exam_dat <- read.csv(...), with the three dots
#     replaced by appropriate arguments. In this csv, the variable separator
#     is a comma and the decimal separator a dot. After you have read the data,
#     delete that code from your script so your identity cannot be inferred
#     from the file path.

# I assigned all the data from the csv file to the variable "examdata"

# Q1. Compute the sample mean and sample standard deviation of 'tox'.
mean(examdata$tox)
sd(examdata$tox)

# Q2. Compute the standard error of the sample mean in Q1 and compute
#     the 0.995th quantile of the t-distribution with n - 1 degrees of freedom,
#     where n is the number of observations of 'tox'.

sd(examdata$tox) / sqrt(nrow(examdata)) # standard error
t.test(examdata$tox, conf.level = 0.995)$conf.int[2] # 0.995th quantile 

# Q3. Compute a 99 % confidence interval for the unknown population mean of
#     'tox', either by using your answers in Q1 and Q2 or by using a suitable
#     R function.

t.test(examdata$tox, conf.level = 0.99)$conf.int

# Q4. Plot a histogram of 'mushroom_quantity'. Draw a vertical line at the
#     sample mean. Does the distribution look symmetric around its mean?
#     Why or why not? Put your answer as a comment below the code producing the
#     histogram and line.

hist(examdata$mushroom_quantity)
abline(v = mean(examdata$mushroom_quantity), col = "blue")
# the data does not look symmetrical around its mean because the mean is closer to the 
# minimum than the maximum value of mushroom quantity


# Q5. Produce a plot showing how the distribution of 'mushroom_quantity'
#     depends on 'sex'. Comment on what you observe.

boxplot(examdata$mushroom_quantity ~ examdata$sex)
# the means between males and females are different for mushroom quantity 
#ingested, males on average ingested more mushroom than females.

# Q6. Test the null hypothesis that the unknown population mean of ingested
#     mushroom quantity is the same for males and females. Test on the 5 %
#     level, and construct a 95 % confidence interval for the difference in
#     means. In a comment, explain what the level (or size) of a test is.

t.test(examdata$mushroom_quantity ~ examdata$sex)
# the p value is less than 2.2e-16, so the null hypothesis that the means are the same is rejected on the 5% level. 

t.test(examdata$mushroom_quantity ~ examdata$sex)$conf.int
# the size of a test is the type 1 error rate

# Q7. Interpret the confidence interval and p-value in Q6. Write your answer as
#     a comment.

# the  p value is less than 2.2e-16, so we reject the null hypothesis that the difference in means of mushroom ingested 
# is zero for males and females. The confidence interval is -67.6 to -45.8, so there is a 95% probability that the
# difference in means is in that interval.  


# Q8. Plot an illustration of how the distribution of 'dead' depends on
#     'liver_transplant'. Label the axes clearly. Write a comment explaining
#     what association between the variables the plot indicates, if any.

barplot(tapply(examdata$dead, examdata$liver_transplant, mean), xlab = "had liver transplant", ylab = "proportion that are dead", names.arg = c("no", "yes"))
# the barplot indicates that a higher proportion of people died that had liver transplant than those that did not have liver transplant

# Q9. Create a contingency table for 'dead' and 'liver_transplant'. Using the
#     numbers in that table, manually, i.e. without using any R functions,
#     calculate the odds ratio for 'dead' and 'liver_transplant'. Interpret the
#     odds ratio in a comment.
 
table(examdata$dead, examdata$liver_transplant, dnn = c("dead", "liver transplant"))
num <- 201 / 4
denom <- 291 / 102
num / denom
# the odds are roughly 17.6 times more likely to live for those that did not have liver transplant than those with liver transplant. 

# Q10. Test the null hypothesis that there is no association between 'dead'
#      and 'liver_transplant' using one parametric and one non-parametric test.
#      Write a comment indicating which tests, if any, reject.

t.test(examdata$dead[examdata$liver_transplant == 1], examdata$dead[examdata$liver_transplant == 0])
fisher.test(examdata$dead, examdata$liver_transplant)
# both of these tests reject their null hypothesis.

# Q11. Create a figure illustrating how the distribution of 'time_to_treat'
#      is different for the two groups defined by the variable 'dead'.
#      Comment on what you observe.

boxplot(examdata$time_to_treat ~ examdata$dead)
# it is observed that the means of these groups differ, so the time to treat variable 
# seems to be generally larger for those that died than for those that survived. 


# Q12. Use two non-parametric tests to test the null hypothesis that the
#      distribution of 'time_to_treat' is the same for patients with
#      'dead' = 0 and 'dead' = 1. Comment on the outcomes of the tests.


wilcox.test(examdata$time_to_treat ~ examdata$dead)
kruskal.test(examdata$time_to_treat, examdata$dead)
# both null hypotheses are rejected, indicating that the time to treat variable distributions 
# are not the same for those that survived and those that died


# Q13. Suppose a researcher is about to replicate our study and they tell you
#      they are planning on randomly sampling hospital records in Denmark of 50
#      patients who died ('dead' = 1) after ingesting Amanita Phalloidesand and
#      50 who did not ('dead' = 0). They are planning on using a t-test to test,
#      on the 1 % level, the null hypothesis that mean 'time_to_treat' for the
#      two groups are the same. Assuming that the standard deviation of their
#      'time_to_treat' measurements are the same in both groups and equal to the
#      sample standard deviation from our study, what is their power against the
#      alternative that the difference in means is 3? In a comment, explain what
#      power is.

# power computed by: 
power.t.test(n = nrow(examdata), delta = c(3), sd = sd(examdata$time_to_treat), sig.level = 0.01)$power
# power is 1 minus the probability of a type two error



# Q14. The researcher in Q13 is worried that since 'time_to_treat' cannot take
#      negative numbers, it cannot be normally distributed, and hence the
#      t-test will be inappropriate. Write a short comment explaining why
#      the test statistic can be approximately t-distributed even if the data
#      are not normally distributed.

# by the central limit theorem (CLT), the data can be approximated to a normal distribution for a large enough dataset (large n), which
# allows for the t test, which assumes that the data has a normal distribution, to be applicable even for non normal data. Also, the 
# t distribution has heavier tails, so it may be easier to approximate to a t distribution for non normal data.

# Q15. Create a vector of length 6, whose i:th element is TRUE if the i:th
#      statement below is true, and FALSE otherwise. Name this vector ans_15.
#      For example, your answer could be ans_15 <- c(FALSE, FALSE, FALSE, FALSE
#      FALSE, FALSE) if you think all of the below statements are false.
#
#      1: In the Kruskal--Wallis test, the null hypothesis is that k >= 2
#         groups have the same mean.
#      2: Non-parametric tests tend to have more statistical power than
#         parametric tests.
#      3: Non-parametric tests tend to make weaker, or fewer, assumptions than
#         parametric tests.
#      4: If a random variable X is greater than or equal to 2 with probability
#         1 / 3, then it is strictly less than 2 with probability 2 / 3.
#      5: A 96 % confidence interval for the mean of a variable in general
#         includes 96% of the observations of that variable.
#      6: For any events A and B, 0 <= P(A and B) <= P(A) <= 1.

ans_15 <- c(FALSE, FALSE, TRUE, TRUE, FALSE, TRUE)




# BONUS (free point): One can see that the proportion of dead is greater
#                     among those who had liver transplants. Does this imply
#                     liver transplants increase the risk of dying? Explain
#                     in a short comment.

# No, the toxicity in the blood from the amount of ingested mushrooms contribute to liver failure and increases the risk of dying.  
# this can be visualized and explored with the following code: 
t.test(examdata$mushroom_quantity ~ examdata$dead)
boxplot(examdata$tox ~ examdata$liver_transplant)
boxplot(examdata$tox ~ examdata$dead)
