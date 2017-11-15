using("assertthat")


q1 <- "When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd"
q2 <- "Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his"
q3 <- "I'd give anything to see arctic monkeys this"
q4 <- "Talking to your mom has the same effect as a hug and helps reduce your"
q5 <- "When you were in Holland you were like 1 inch away from me but you hadn't time to take a"
q6 <- "I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the"
q7 <- "I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each"
q8 <- "Every inch of you is perfect from the bottom to the"
q9 <- "I’m thankful my childhood was filled with imagination and bruises from playing"
q10 <- "I like how the same people are in almost all of Adam Sandler's"
#source('c:/dev/r-course/10-capstone/predictor.r')
 


ana = word_analogy(file_name = "vec.bin", search_words = q1, num = 20)