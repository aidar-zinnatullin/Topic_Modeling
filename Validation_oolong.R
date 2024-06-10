# Details:
# Chan, C. H., & Sältzer, M. (2020). oolong: An R package for validating automated content analysis tools. The Journal of Open Source Software: JOSS, 5(55), 2461.
# https://www.theoj.org/joss-papers/joss.02461/10.21105.joss.02461.pdf

library(here)
library(oolong)
library(stm)

load(here("stm_outputs", "saved_objects.RData")) # download objects
load(here("stm_outputs", "modelPrevFit_27.RData")) # download the model

# start the test
oolong_test <- create_oolong(input_model = modelPrevFit_27,
                             input_corpus = meta$text)
oolong_test
oolong_test$do_word_intrusion_test()
oolong_test$do_topic_intrusion_test()


oolong_test$lock()
oolong_test


# for two raters ----------------------------------------------------------


oolong_test_rater1 <- create_oolong(modelPrevFit_27, meta$text)
oolong_test_rater2 <- clone_oolong(oolong_test_rater1)
## Let rater 1 do the test.
oolong_test_rater1$do_word_intrusion_test()
oolong_test_rater1$do_topic_intrusion_test()
oolong_test_rater1$lock()
## Let rater 2 do the test.
oolong_test_rater2$do_word_intrusion_test()
oolong_test_rater2$do_topic_intrusion_test()
oolong_test_rater2$lock()

# Summary
summarize_oolong(oolong_test_rater1, oolong_test_rater2)




