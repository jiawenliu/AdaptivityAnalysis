
We thank the reviewers for their detailed, careful and constructive
comments. We will use them to improve our paper.

* Overview of Response

We tried to address the main concerns that the reviewer had. We
highlight two of them here.
Several reviewers were confused by what we call execution-based graph
and dynamic analysis. We tried to clarify below what is the
situation. In a few words, in Section 4 we define the notion of
adaptivity. We do this by using the operational trace semantics, this
is why we sometime refer to it as a dynamic analysis or to the
execution-based graph as such. However, this is not an analysis, just
the definition. Instead, in Section 5 we give our static analysis. We
need the definition from Section 4 when we want to prove that the
analysis in Section 5 is sound.
Several reviewer have also doubts about the practical usefulness of
our work in data analysis, in particular, in comparison with manual
efforts or with other techniques which work well in low adaptivity
regimes and which don't need the exact adaptivity. We do agree that we
didn't explore this aspect in details, yet. We believe that doing this
would require a considerable effort which is not necessarily related
to how the program analysis work. In particular, as we discussed in
the introduction we do plan to integrate our technique in existing
framework to do these experimental comparison. Nevertheless, we
believe that the contribution of our work has value from the principle
of programming language perspective, since we capture a new
interesting concept that previous analyses in isolation were not able
to capture and we show new potential applications of static analysis
techniques in other fields.

* List of Changes

1 - Clarify the confusion about dynamic analysis vs the use of the
semantics for our definition of adaptivity.
2 - Give a better description of the examples and our formalization.
3 - Fix all the typos in the formalization and examples pointed out by the reviewers.
4 - Add the performance evaluation of the implementation.
5 - Add the formal definition of appearance in the main body of the paper.


* Reviewer A

> I would like to read a clear explanation of the meaning of weight
w_k, how it is computed and of how it becomes k?

Sorry if this was confusing. In our framework we have two distinct
kinds of graphs. The first kind is used to define the notion of
adaptivity and is build using the program semantics (we call this
"execution-based dependency graph"). The second one instead is the one
we use for the program analysis to obtain an upper bound on the
adaptivity (we call this just "dependency graph").  In the running
example, Fig.3 b is the execution-based dependency graph of the
program, while Fig.3 c is the dependency graph.

The weights $w$ in Fig.3(b) are functions from initial states to
natural numbers, which we use to express the exact number of times the
corresponding command is executed starting from the initial state.  We
use some shorthand which may have created confusion. We use $w_1$ for
the function that on every initial state $\tau$ returns 1 (this
corresponds to the node being visited 1 time). We use w_k for the
function that on a state $\tau$ returns the number $n$ that $k$ is
assigned to in $\tau$ (this corresponds to the command being visited
$n$ times). These weight functions $w$ are just a way to summarize the
how many times in a given execution a command is reached. We don't
compute the semantics directly and hence we don't compute these
functions directly either, we just use them in our proof of soundness
to show that the adaptivity our program analysis (the one using the
second kind of dependency graph) provides is a good upper bound for
the semantics one.

In Fig.3(c) we have instead symbolic bounds (arithmetic expressions
over program variables). Here $1$ stands for the integer $1$, while
$k$ stands for the program variable $k$. We realize now we should have
explained this or use different notation. These symbolic bounds are
computed using a reachability-bound technique we present in Section
5.3.2 which is essentially an adaptation of the difference constraint
method by [Sinn et al. 2017a]. In this section we overload the word
"weight" and the symbol $w$ to denote symbolic reachability-bound
expressions. We thought this would be ok, since the two uses of
"weight" and the symbol $w$ are used to express similar quantities,
but we realize now that this may have also created some confusion,
especially in the example after Theorem 5.1 where we use both
notations. We will fix this in future versions.

Theorem 5.1 connect these two quantities (the semantics one and the
estimated one) to express the soundness of the reachability-bound
analysis.

> In sect. 4.1 I think that the notion of appearance of the assignment
  should be provided formally...

Due to space constraints, this is given in Definition 8 in the
Appendix. We will move it to the main body of the paper.

>In the same section what is query(X[1]) in the explanation on the
 example c_1?

It is a typo. It should be  query(X[3]).

* Reviewer B

> Please clarify whether there is an issue with the semantics 
 of sequential composition

Indeed, this is a typo. We moved from big step to small step but 
somehow we copied the wrong rule. We have the two usual rules for 
sequential composition in small step semantics. 

> Please clarify how the edge from $x^3$ to $a^0$ in Fig. 3 is obtained from the formal definition.

The edges from vertex $x^3$ to $a^0$ in Fig.3(b) and Fig.3(c) are
typos. There should be another edge from vertex $a^5$ to $a^0$ in the
two graphs.

> Can you give us an indication of the performance of the prototype implementation?

The implementation is efficient in the sense the maximum running times
over all the examples is 0.0025 second. We will add more info about
the examples in the next version.


* Reviewer C

> how much does being able to have a static analysis of the worst-case
  number of rounds of adaptivity help in practice?

This is a good question and we believe that our work can help
answering it. As we discuss in the introduction, our research agenda
would be to integrate our static analysis in a framework like the
Guess and Check one. The static analysis would allow the framework to
automatically choose the mechanism to use, accordingly to the theorem
we presented. Part of the evaluation of this tool would be to compare
it with the same framework without static analysis and evaluate how
this helps in practice. However, this goes beyond our vision for the
current work which is studying the principles of programming languages
that can be useful to formalize and for reasoning about a concept that 
is often used only informally: adaptivity of a data analysis.


* Reviewer D

> Please comment on why manually analyzing adaptivity is infeasible.
The use of specific methods to reduce the generalization error of a
data analysis is still in its infancy. A reason for this is that using
these methods imposes an extra burden on the data analysts: change
their analysis to integrate these methods. We believe that there would
be benefit in having automated tools helping data analysts with this
task, and it would make it more scalable. Our work is a first step in
this direction.
We agree with the reviewer that the calculation of adaptivity could be
done manually in some situations, similarly to many other properties
for which as a community we build automated formal tools,
e.g. checking buffer overflow, or alias analysis, or resource
analysis. However, we believe that formal tools would make these
calculation more robust to mistakes and more scalable to bigger
programs.

> What is the purpose of employing dynamic analysis?

We see that our choice of terminology was not great. In fact, we do
not perform dynamic analysis in a proper sense. Rather, we define our
notion of adaptivity on the trace semantics of programs, which
summarize the dynamic behavior of the program. We are sorry for the
confusion and we will fix this.


