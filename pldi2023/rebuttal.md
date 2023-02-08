 We thank the reviewers for their detailed, careful, and constructive
comments. We will use them to improve our paper.

* Overview of Response

1. Several reviewers have doubts about the practical usefulness of
our work in data analysis.

In the introduction, our research agenda
would be to integrate our static analysis into a framework like the
Guess and Check one. The static analysis would allow the framework to
automatically choose the mechanism to use, accordingly to the theorem
we presented. Part of the evaluation of this tool would be to compare
it with the same framework without static analysis and evaluate how
this helps in practice. However, this goes beyond our vision for the
current work which is studying the principles of programming languages
that can be useful to formalize and for reasoning about a concept that 
is often used only informally: adaptivity of a data analysis.

To have a preview of the practical usefulness,
we will make the changes to the evaluation table by inferring the generalization error and recommending the mechanisms according to the estimated adaptivity rounds.

2. Several reviewers also have doubts about the comparison evaluation over alternatives.

Thanks for bringing up this suggestion. Since our tool is the first to compute the adaptivity level, we have not yet made comparison evaluations with alternatives.
<!-- We admit that our approach is complicated.  -->
To show the evidence that our complicated approach is needed to compute adaptivity precisely,
we will add the comparison evaluations.
The alternatives are the simplified versions of our Adaptfun framework. We will remove the loop-bound analysis step in the first comparison, and remove the data dependency analysis step in the second comparison.

3. Some concerns are the motivation for automatically computing the adaptivity and using it to reduce the generalization error,
specifically the difficulty in manually computing the adaptivity and the generalization error.

The use of specific methods to reduce the generalization error of a 
data analysis is still in its infancy. A reason for this is that using
these methods imposes an extra burden on the data analysts: change
their analysis to integrate these methods. We believe that there would
be benefit in having automated tools helping data analysts with this
task, and it would make it more scalable. Our work is the first step in
this direction.
Though the calculation of adaptivity could be
done manually in some situations,
<!-- , similar to many other properties
for which as a community we build automated formal tools,
e.g. checking buffer overflow, alias analysis, or resource
analysis. However,  -->
we believe that formal tools would make this calculation more robust to mistakes and more scalable to bigger
programs.


* List of Changes

1 - Add the comparison evaluations over alternatives.

2 - Add the estimated generalization error and recommended mechanisms into the evaluation results

3 - Evaluate several regression algorithms that are commonly used in practice.

4 - Add explanations in Definitions 1 - 7 by referring back to the motivating example.


* Reviewer A
> 1A. What common analysis tasks can be expressed in your programming language?

Based on the existing evaluation, we believe that the regression algorithms equipped with the grade decedent optimization are all able to be expressed and analyzed by our framework.

> 1B. In lines 876-877, you mention that your approach does not scale to large programs. It would be good to explain:

1) **Scalability**: Programs with 200+ lines of code without nested loops or 50+ lines of code with nested loops are large programs that we are unable to handle.
2) **Evaluation over real programs**: For now, we only evaluated the linear regression with the grade decedent optimization algorithm, the two-rounds, and the multiple-rounds algorithms.
3) **Relaxation**: The relaxation comes in the multiple-path loops.
In our existing approach, we do not handle the path-sensitivity. When there are loops with multiple paths in the program, we use the worst-case loop iteration times to over-approximate the iteration times of each loop path.
4) **Soundness**: In our supplementary material, we have the soundness proof of this approximation. 


> 1C. The explanation for the 25 programs.

The first five programs are real data analysis, we translate them into our language syntax.
The sixth to eighth programs are self-crafted programs, their lines of code are at most 11 and contains complicated loop patterns. We aim to show that our tool can compute accurate adaptivity for complicated data analysis algorithms over these programs.
The last seven programs are auto-synthesized programs using the first eighteen programs. Their lines of code are from 27 to 500 and we want to show the scalability of our tool in terms of efficiency.


> 1D. 
<!-- it was hard for me to understand the quality and usefulness of your upper bounds (both I and II), compared to the exact adaptivity level. Can these bounds guide the analyzer to switch a mechanism? Can a user rely on these bounds to infer the generalization error? -->

The answer to both of the questions is Yes. We will make the change to the evaluation table, specifically adding the estimated generalization error
and the recommended mechanism for each program on the table.


* Reviewer C

> I was confused by the parameter k appearing in most of the benchmarks in Table 1. 
<!-- Does the analysis compute symbolic estimates of the adaptivity (for e.g., 2 + max(1, 2k)) or does it return the adaptivity for specific values of k? -->

We compute symbolic estimates of the adaptivity.
Given "k" as an input variable, its value is unknown during the static program analysis. In this sense, we give the symbolic expression as the estimated adaptivity where "k" shows up as a symbol. 

<!-- Even though future queries may depend on the results of previous queries, the present paper involves the analyst committing to a query protocol ahead of time.  -->
> What happens in the truly interactive setting, where the analyst chooses the next query based on subjective considerations? 
<!-- I assume that this would manifest in the form of non-determinism in the query program. -->

This is an interesting question. We admit that our existing approach only works for the data analysis that is committed to a query protocol ahead of time. The truly interactive setting is indeed non-deterministic.
We would like to extend the language model with probabilistic programming and explore truly interactive data analysis.

* Reviewer D

> Third, the evaluation is limited. 
<!-- The evaluation uses benchmarks written by the authors in the paper's while-like language. The text states that a few of the benchmarks are based on real data analyses from prior work. It would help to provide more information would the evaluated benchmarks beyond the few sentences in Section 7. Programs written in a real language and not written by the authors would be more likely to contain both spurious dependencies and code patterns that cause the analysis to detect false dependencies, leading to substantial overestimates of adaptivity. -->

Thanks for this suggestion. We agree that our evaluation is very limited, it is only based on the programs written in our language.
We translate a few of the real-world algorithms into our language, the linear regression with grade decedent optimization, the two-round and the multiple-round algorithms. 
But we would like to explore a variety of real work data analysis programs next.
As an initial work in this direction, we focus the contribution of our work from the principle
of programming language perspective. Specifically, we capture a new
interesting concept that previous analyses in isolation were not able
to capture. But we believe our tools can be applied to other real work data analysis programs
beyond those presented in Section 7.