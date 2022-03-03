mani_df <- subset(mani_df, select = -c(26))
mani_df$new_hiquamum <- factor(mani_df$new_hiquamum, ordered = FALSE)
mani_df$new_hiquamum <- relevel(mani_df$new_hiquamum, ref="no-degree")
mani_df <- subset(mani_df, select = -c(26))
p13 <-  ggplot(mani_df, aes(x= new_hiquamum, y=ks4score)) + geom_boxplot() 
p14 <-  ggplot(rw_df, aes(x= new_secshort, y=ks4score)) + geom_boxplot()

```{r}
#df manipulation
mani_df <- rw_df
mani_df$new_secshort <- rw_df$SECshort

levels(mani_df$new_secshort)[levels(mani_df$new_secshort) %in% c("Intermediate","Routine,_semi-routine_or_unemployed")] <- "non-professional"
levels(mani_df$new_secshort)[levels(mani_df$new_secshort) %in% c("Managerial_and_professional")] <- "professional"
mani_df$new_secshort <- factor(mani_df$new_secshort, ordered = FALSE)
mani_df$new_secshort <- relevel(mani_df$new_secshort , ref="non-professional")

first_lm <- lm(ks4score ~ SECshort, data = rw_df)
summary(first_lm)

```

```{r}
sec_lm <- lm(ks4score ~ new_secshort, data = mani_df)
display(sec_lm)

```

```{r}
mani_df$new_hiquamum <- mani_df$hiquamum
levels(mani_df$new_hiquamum)[levels(mani_df$new_hiquamum) %in% c("No_qualification","Other_qualifications","GCSE_grades_A-C_or_equiv","GCE_A_Level_or_equivalent","HE_below_degree_level")] <- "no-degree"
levels(mani_df$new_hiquamum)[levels(mani_df$new_hiquamum) %in% c("Degree_or_equivalent")] <- "degree-equivalent"

mani_df$new_hiquamum <- factor(mani_df$new_hiquamum, ordered = FALSE)

does the proportion 
```{r}
chisq.test(mani_df$fiveac, mani_df$fiveem)

#does the expected grade of people achieving 5 and more IGSC grade (fiveem) differ than those achieving 5 and more IGSC grade that includes maths and science. 
# ## Dealing with missing values
SECshort - possibly merge missing and unemployed - mean is almost the same. 
fsm - merge missing and yes - they are unlikely to declare this due to shame and mean is the same.
hiquamum - merge missing, no_qualification, other qualification 
singlepar - check how many missing values there is