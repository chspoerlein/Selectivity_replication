### helper function to recode age

age_recode <- function(agevar){
  age <- NA
  age=replace(age,agevar>=15 & agevar<=19,1)
  age=replace(age,agevar>=20 & agevar<=24,2)
  age=replace(age,agevar>=25 & agevar<=29,3)
  age=replace(age,agevar>=30 & agevar<=34,4)
  age=replace(age,agevar>=35 & agevar<=39,5)
  age=replace(age,agevar>=40 & agevar<=44,6)
  age=replace(age,agevar>=45 & agevar<=49,7)
  age=replace(age,agevar>=50 & agevar<=54,8)
  age=replace(age,agevar>=55 & agevar<=59,9)
  age=replace(age,agevar>=60 & agevar<=64,10)
  return(age)
}



