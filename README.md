# Do We Need Multiple Imputation for Prediction?

When analyzing incomplete data, we must address the missing values in some
way. Missing data imputation is one way in which we can do so. The process of
imputing the missing data entails estimating a statistical model and using
predictions from this model to replace the missing values. Broadly speaking, we
can do missing data imputation in two ways: single imputation (SI) or multiple
imputation (MI). When testing hypotheses or doing inference on estimated
parameters, SI will produce attenuated standard errors that will inflate Type I
error rates and produce overly narrow confidence intervals (CIs; Enders,
2010). MI was developed by Rubin (1978, 1987) to address this limitation and
produce accurate statistical inferences from imputed data. MI is much more
computationally intensive than SI, however, and there are certain circumstances
where the additional computational burden may not be necessary.

Well-implemented SI routines can produce unbiased point estimates of parameters,
and there are certain data analytic contexts wherein point estimation is the
only objective. Point prediction problems are one such context. Computational
efficiency and scalability are often paramount concerns in prediction problems,
so SI applications dominate MI in the prediction literature (e.g.,
García-Laencina, Sancho-Gómez, & Figueiras-Vidal, 2010). Indeed, the
unbiasedness of SI suggests that point predictions should also be unbiased when
generated from a model fit to singly imputed data. Point predictions are not the
whole story, though. In practice, we often want some type of interval estimate
around predictions (e.g., a CI or a prediction interval [PI]). As in the case of
CIs for model parameters, we would also expect CIs and PIs for predicted values
to be too narrow.  

In this project, you will use Monte Carlo simulation methods to explore the
relative influence of SI and MI on point predictions, CIs for predicted values,
and PIs. This project can support up to three students. There are many different
ways to parameterize imputation models and many problem characteristics which
may affect the relative performance of imputation methods. Therefore, each
student will work with the supervisor to define their own operationalization of
the problem. Each student will then run an independent simulation study to
explore their conceptualization of the problem. Of course, students are
encouraged to collaborate on any overlapping portions of their respective
codebases to avoid “reinventing the wheel”. Throughout this project, we will
strive to follow best practices in open science and reproducible research
workflows.
