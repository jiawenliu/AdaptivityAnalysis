We thank the reviewers for these careful and constructive
comments. We will use them to improve our paper.

* Responses to Common Concerns

1. Several concerns are about the practical usefulness of
our work in data analysis.

Our research agenda
would be to integrate our static analysis into a data analysis framework like the
Guess and Check one. Our static analysis would allow the framework to
automatically choose the best mechanism to use
Part of the evaluation of this tool would be to compare
it with the same framework without static analysis and evaluate how
this helps in practice. However, this goes beyond our vision for the
current work which is studying the principles of programming languages
that can be useful to formalize and for reasoning about a concept that 
is often used informally: adaptivity of a data analysis.

To have a preview of the practical usefulness,
we will make the changes to the evaluation table by inferring the generalization error and recommending the mechanisms according to our static analysis.

2. Some doubts are about the comparison evaluation over alternatives.

Since our tool is the first to compute the adaptivity level, we have not yet made comparison evaluations with alternatives.

To show the evidence that our complicated approach is necessary to compute adaptivity precisely,
we will add the comparison evaluations.
The alternatives are the simplified versions of our Adaptfun. We will remove the loop-bound analysis step in the first comparison, and remove the data dependency analysis step in the second comparison.

3. Some concerns are the motivation for automatically computing the adaptivity and reducing the generalization error.

The use of specific methods to reduce the generalization error of a 
data analysis is still in its infancy. A reason for this is that using
these methods imposes an extra burden on the data analysts: change
their analysis to integrate these methods. 
Though the calculation of adaptivity could be
done manually in some situations,
we believe that formal tools would make this calculation more robust to mistakes and more scalable to bigger
programs. It will also reduce the burden from the data analysts.


* List of Changes

1 - Add the comparison evaluations over alternatives.

2 - Add the estimated generalization error and recommended mechanisms into the evaluation table

3 - Evaluate several regression algorithms that are commonly used in practice.

4 - Add explanations in Definitions 1 - 7 by referring to the motivating example.


* Reviewer A

> 1A. 

Based on the existing evaluation, the regression algorithms equipped with the grade decedent optimization are all able to be expressed and analyzed by our framework.

> 1B. 

1) **Scalability**: Programs with 200+ lines of code without nested loops or 50+ lines of code with nested loops are large programs that we are unable to handle.
2) **Evaluation over real programs**: We haven't evaluated over real programs, but only over few algorithms of small size written in our language.
3) **Relaxation**: The relaxation occurs when there are multiple-path loops. In this case, we use the worst-case loop iteration times to over-approximate the iteration times of each loop path.
4) **Soundness**: In our supplementary material, we have the soundness proof of this approximation. 


> 1C. 

The first five programs are real-world data analyses translated into our language model.
The sixth to eighth programs are self-crafted with complicated loop patterns and at most 11 and lines of code. We use them to show that our tool can estimate accurate adaptivity for complicated data analyses.
The rest are synthesized from the first five programs, with lines of code from 27 to 500. We use them to show the scalability of our tool.


* Reviewer C

> The confusion on the symbol k.

We compute symbolic estimates of the adaptivity.
Given "k" as an input variable, its value is unknown during the static program analysis, so we give the symbolic expression as the estimated adaptivity where "k" is a symbol. 

> What happens in the truly interactive setting?

The truly interactive setting is indeed non-deterministic.
We admit that our existing approach only works for the data analysis that is committed to a query protocol ahead of time. 
However, we would like to extend the language model with probabilistic computation and explore truly interactive settings.

* Reviewer D

> Third concern.

We agree that our evaluation is very limited, it is only based on the programs written in our language.
We evaluate only a few of the real-world algorithms by translating them into our language model.
However, as an initial work in this direction, we focus the contribution of our work from the principle
of programming language perspective. Specifically, we capture a new
interesting concept that previous analyses in isolation were not able
to capture. But we believe our tools can be applied to other real-world programs
beyond those presented in Section 7.