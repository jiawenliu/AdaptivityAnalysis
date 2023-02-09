We thank the reviewers for these constructive
comments. We will use them to improve our paper.

* Responses to Common Concerns

1. Several concerns are about the practical usefulness of
our work in data analysis.

Our research agenda
is to integrate our static analysis into a data analysis framework like the
Guess-and-Check. The static analysis would allow the framework to
automatically switch to the best mechanism.
Part of the evaluation of this tool would be to compare
it with the same framework without static analysis and evaluate how
this helps in practice. However, this goes beyond our vision for the
current work which is formalizing and reasoning about a concept that 
is often used informally: adaptivity of a data analysis.

To have a sense of this practical usefulness, we plan to infer the generalization error and recommend the mechanisms according to our existing result, and enrich the evaluation table.

2. Some doubts are about the comparison evaluation over alternatives.

Since our tool is the first to compute the adaptivity level, we did not report any comparison evaluation. We agree it is important to have comparison and we plan to add the comparison evaluation in our section 7.
The alternatives are the simplified versions of Adaptfun. We will remove the "reachability-bound" analysis, and the "reaching definition" in the two alternatives respectively.

3. Some concerns are the motivation for automatically computing the adaptivity and reducing generalization error.

The use of specific mechanism to reduce the generalization error of a 
data analysis is still in its infancy. A reason for this is that using
these methods imposes extra burdens on the data analysts.
Though the calculation of adaptivity could be
done manually in some situations,
we believe that formal tools would reduce the burden on data analysts and make this calculation more robust to mistakes and more scalable to bigger
programs. 


* List of Changes

1 - Add the comparison evaluations over alternatives.

2 - Add the inferred generalization error and recommended mechanisms into the evaluation table

3 - Refer to the motivating example in Definitions 1 - 7.

* Reviewer A

> 1A. 

Based on the existing evaluation, any regression algorithms equipped with the grade decedent optimization can be expressed and analyzed by our tool.

> 1B. 

1) **Scalability**: Programs with 200+ lines without nested loops or 30+ lines with nested loops are large programs that we cannot handle.
2) **Real programs evaluation**: We haven't evaluated over real programs, but only few algorithms of small size written in our language.
3) **Relaxation**: The relaxation occurs when there are multiple-path loops. In this case, we use the worst-case loop iteration times to over-approximate the iteration times of each loop path.
4) **Soundness**: We proved the soundness in Appendix D in the supplementary material.

> 1C. 

The first five programs are real-world data analyses translated into our language model.
The sixth to eighth programs are self-crafted with complicated loop patterns and at most 11 lines of code. We use them to show that we can compute accurate adaptivity for complicated data analyses.
The rest programs are synthesized from the first five one with lines of code at most 500. We use them to show the scalability of our tool.


* Reviewer C

> The confusion on the symbol k.

We compute symbolic estimates of the adaptivity.
Given "k" as an input variable, its value is unknown during the static program analysis, so we give the symbolic expression as the estimated adaptivity where "k" is a symbol. 

> What happens in the truly interactive setting?

The truly interactive setting is non-deterministic.
Our existing approach only works for the data analysis that is committed to a query protocol ahead of time. We would like to extend the language model with probabilistic computation and explore truly interactive settings.

* Reviewer D

> Third concern.

We evaluate our tool only over a few of the real-world algorithms by translating them into our language model.
As an initial work in this direction, we focus the contribution of our work from the principle
of programming language perspective. Specifically, we capture a new
interesting concept that previous analyses in isolation were not able
to capture. But we believe our tools can be applied to more real-world programs
beyond those presented in Section 7.