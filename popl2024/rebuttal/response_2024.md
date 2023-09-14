We would like to sincerely thank all the reviewers for their
constructive and detailed comments.


- Overview

We agree that the presentation can be improved and we thank the
reviewer for pointing out in details where improvements are needed. We
plan to carefully revise the paper by adding explanations and
examples, as suggested, and we will also work on improving the writing
overall, including in the appendix. We will also improve the
experiments and their presentation following the reviewers
suggestions. We are confident that these changes can be done in the
available timeframe and using a few more pages.

- Change List
 
 1. We will add more examples to make the presentation easier to
 follow. In particular, we plan to add one example (taking it from the
 ones we already have in the paper) to show how we calculate the
 estimated weight using the reachability bound algorithm TB(e,c) and one
 example to show how the algorithm AdaptBD_scc works.

 2. We will add the algorithm adpatBD and discuss it more, we will
 explain in more details the "monitor argument" and we will move
 clarifications accordingly to the suggestions of the reviewers.

 3. We will improve our evaluation by adding the actual generation
 error, and by highlighting the mechanism our heuristic chooses. We
 will also improve the comparison between our choices of mechanisms
 with the best mechanism.

 4. We will carefully rewrite the sentences which causes confusions.
  

- Detailed Response

Review A  

First of all, thanks for the detailed comments, section by section and your constructive suggestion on using more examples to clarify some key concepts. We will make the changes to make our paper easier to follow up!  

Question to be address by this response:
------------------------------------------
> Please clarify Figs. 2(a) and 2(c).  The ``last adaptive query'' is the 401st in Fig. 2(a), which for Gaussian noise has RMSE 0.37.
  But in the x=400 position of Fig. 2(c), RMSE for Gaussian noise appears to be 0.028, which is wildly different.  Clearly, I'm not
  understanding what you meant to convey.


  Thanks for pointing out this confusion. Fig. (c) is experimenting under different number of underlying data size, which  we do not explicitly give in overview. We will add the experiments' settings then.


> Shouldn't Theorems 2.1-2.3 all contain the clause ``with high probability'' (as do the informal statements of theorems in [18])?

Yes.

> Please give an example to illustrate lines 749-762

line 749-762 describes how our reachability bound TB(e,c) returns an symbolic upper bound for an edge e in the set of edges absE(c). The local bound of this edge e is computed using difference constraint described in last step between line 738-748. TB(e,c) calculated local bound of e, if e is an symbolic constant(short for SC, either a nature number such as 3, or infinity, or input variable such as k in the running example, or a symbol Q_m representing a query request), TB(e,c) just return this symbolic constant. Let us look at Fig.6(b), the edge e1 (0, top, 1) has local bound 1 so TB(e1,c) returns 1 as shown in the node 0 and 1 in Fig.6(c) with bound 1.  
Otherwise, local bound of e is not SC, it will be a variable. For instance, still Fig.6(b), the edge e4 (4, j' <= j -1 ,5) is in an SCC appearing in the while loop, the local bound is variable j, not a symbolic constant. 
Then, to get TB(e4,c), it calculates two quantities: the first one is the sum of the reachability bound TB(e,c) of all the edges that may increment j, such as of the form (l, j' <= j + v ,l') plus the increment v, v is a SC.  For the edge e4, Fig.6(b) does not have any edge that increment j, so the first quantity is 0. 
The second one is the sum of the reachability bound of edge which reset j, such as of the form (l, j <  v ,l') multiplied with maximal value of this symbolic expression v. For the edge e4, Fig.6(b), the only edge that resets j is e2(1,j' <= k ,2), TB(e2,c) = 1 and maximal value of k is k. So the second quantity is k.
In this case, TB(e4,c) = k. It shows that the edges e4 will be executed at most k times.

   


Response to comments
----------------------------------------------------------
> The explanation is confusing at several key points (749, Alg. 1, explanation of Alg. 1, 899)
> There are several places that would be helped by examples.
> 5. Algorithm, lines 749-762: unclear.  Please add an example.
We will add an example, for instance, the running example in section Overview to show how we can calculate the symbolic weight by using the reachability bound analysis.
 
The explanation of Alg 1 is not clear. We use the algorithm AdaptBD to estimate the adaptivity, and AdaptBD_scc to estimate the Strong connected component of the graph. AdaptBD uses AdaptBD_scc. We will add the algorithm of AdaptBD as All 2, and add an example to show how Alg1(AdaptBD_scc) calculates the result. 

- There are several places where the presentation would be helped by providing the explanatory text/example *before* the definition/theorem.  One example: Definition 5.
Definition 5, the walk can be better understood if we discuss the an example, the one in Fig.3(b), before the definition. We will arrange to move the explaining examples before Definition 5.
Same for Definition 6, the query length, we will move the explanation of Fig.3(b) before the definition 6. 


5. Algorithm

> lines 864-866, breakdown of the steps of Algorithm 1: please refer to specific line numbers (or ranges of line numbers)
It is nice to refer to specific line numbers between lines 864 - 866, thanks.

> line 864: ``first collects all the paths in $SCC_i$''.  First, you probably mean ``all simple cycles.''  Second, I don't see any code in Algorithm 1 that creates a data structure that collects any set of paths.
"first collects all the paths in $SCC_i$",  from line 13 to line 15, every vertex v represents a path because we say "paths collected in step 1 are all simple cycles
with same starting and ending vertex", so at line 13, every v representing a path from v to v. Sorry for the confusion, to be precise, the algorithm handles paths on the fly, one by one. "collect all the paths" is implicit and not precise. We will change the sentence.   
  

 2. Overview
> Shouldn't Theorems 2.1-2.3 all contain the clause ``with high probability'' (as do the informal statements of theorems in [18])?

  Yes, thanks for pointing it out.

> I'm confused by lines 284-288: you use $\Chi$ to ``abstract a possible row'' (where row is used in the singular), but then say that query($\Chi[j], \Chi[k]$) computes an empirical mean.  I think the problem is the abuse of notation on line 193.  There, ``query(P)'' returns an expectation.  However, in your programming language, the similar-looking program statement ``query(P)'' computes an empirical mean (and involves an implicit loop that you never talk about or represent in your IR).
> lines 289-290: ``compute an approximation of the product of the empirical mean of the first $k$ attributes.''  I'm even more mystified: in Fig. 3(a), statement 5 accumulates $a$ as a running sum.  You've added together emperical means of products of the $j$-th attribute with the $k$-th attribute.


The abuse of notation of query does cause some confusion, we will modify it. For the example in Fig. 3(a), statement 5 accumulates $a$ as a running sum, statement 3, x<- query(chi[j].chi[k]), returns the empirical means of products of the $j$-th attribute with the $k$-th attribute. Since j starting from k to 0, a will finally store sum of the means of row k times row k, row (k-1) times row k, until row 0 times row k. The sum a will be used to construct another query at line 6. This is how two rounds algorithm is designed. We realize the sentences may cause confusion, will rewrite it with concrete details.  


6. Examples

> A poor explanation in lines 899-917: What is the goal of the ``monitor argument''?  You haven't said enough, and suddenly introduce a ``hidden database D'' in lin 903.

  More details about "monitor argument" is worth to showing up and then a hidden database D will be accepted in a natural way. We will make the change to make the example easier to follow up.


7. Implementation

> Change the section title to ``Evaluation.'' The section is not much about the implementation per se
> Table 1: In the $A_{est}$ column, it would help to put in bold font the entries that match the ground truth adaptivity in column 2.
> line 1020: Change paragraph title to ``Ablation studies''
Agree, thanks for the advice.

> line 1017-19: How about a much longer time out for the three examples, like 120 minutes?


   jumbo takes around 40 mins. the long time is for weight, AdaptBD is fast. For the other 2, more than 120 mins, still caused by getting the weight.


> Table 3: in line 1105 to the end of the section, you give a lot of text discussing the circumstances in which the heuristic in your tool would choose one mechanism or another.  You should indicate the choice in the table, then then the reader would easily see how the choices (based on your analysis) agreed with the best generalization error, shown in bold.  You've only given us half the story in Table 1, and make the reader hunt for insight in the text from line 1105 to the end of the section!

Thanks for the feedback. In table 3, the bold results are the one chosen by our heuristic. Some of them is not the best, for instance, RQ, nDPair, our choice(in bold) in Table 3 is not the best among the 3 mechanisms but still acceptable. We should clarify that our choice is highlighted in Table 3 to make it clear. 


minor:
Thanks for pointing out the minor, we will fix them.



Review B

Thanks for your detailed suggestion.



Detailed comments for authors
-----------------------------
(1) A high-level question I had is as follows: is it possible to describe the approach in the simple steps I describe above? is there any more nuance than the above description? If so, you could plainly state it that way, and your paper will still be a useful contribution. I think you have a lot of notation to capture control and data dependence etc which are well-known concepts and there is no need to describe these in the level of detail you have done.
Yes, it is. agree that we can save more space by omitting some well-know control and data dependence to add more examples for better presentation. 


(2) In Table 3, can you add an extra column for the actual generalization error? How would you estimate the actual generalization error?
Yes, we will.   @Jiawen, can you describe how we estimate the generation error?


(4) Is there value in combining different mechanisms for different queries. That is, use split data for some queries, gaussian noise for some queries etc.
Not sure if it is possible.  @Marco


(3) In Table 2 it will be good to also add the actual adaptivity (as in Table 1) - - it will make it easier to compare, rather than flip back and forth with Table 1.
Agree. Thanks for the suggestion.


Review C


Questions to be addressed by author response
--------------------------------------------
- How common is it that an adaptive data analysis algorithm does not have its adaptivity set by design?


- What is the complexity of Algorithm 1?


------------------------------

Detailed comments

> The exposition in Section 2.1 presents the problem of coming up with an estimate $a$ of the population mean $query(P)$. Then the text writes "...test depends on the prefix $query_1, a_1,...$", which creates some confusion around these $a_j$, which seem to appear out of nowhere. Are we instead in an online setting where a new estimate is given after each query?
> In Theorems 2.1, 2.2, 2.3, what are the $a_j$? The bounds should depend on the way the $a_j$ are computed, yet this is not specified. I suppose these are empirical means, like $a$ earlier (lines 199-200)?

Sorry for the confusion, $a_j$ stands for the result of the jth query, similar as a = query(P) in the previous sentence of this text, we will make it explicit.

> line 300, "However, capturing this concept formally is surprisingly difficult". I don't see why this is more difficult than standard dependency analysis. You write "the difficulty comes from the fact that a query can depend on the result of another query in multiple ways, by means of data dependency or control flow dependency". These are standard challenges in dependency analyses, why are they labeled as new here?
The challenge also covers the formal definition of adaptivity, not just dependency analysis.


- line 353, "we consider the walk that visits...". Why is such a walk uniquely defined? I suspect it is not.
It is not uniquely defined. To get the definition of adaptivity, we only care about one walk with most number of queries. 

- line 355, "the number of query nodes visited", are these distinct nodes or not? Consider disambiguating here. The paper also mixes the terms "node" and "vertex", consider sticking to one.
The nodes will be distinct. Thanks for the suggestion, we will be consistent.


- line 357, why don't you take the path $l^6 \to x^3$ as the one defining adaptivity? Why go through $a^5$?
Good point! Based on our definition, either going through $a^5$ or not will not affect the definition, we will explain that both will be fine to get the adaptivity.


- line 412, a small discussion on query values would help here.
Indeed, we plan to add a concrete example to make it clear.


- Defintion 1, I could not see a definition of $\mathcal{E}$ (perhaps I missed it). I suppose it denotes the events of the program. Similarly with $\mathcal{C}$ in Definition 2.
At line 431, we define the set of events. Sorry for the confusion, we should clarify the definition near the definition and we will.


> line 543, "whether the change in $\epsilon_1$ affects the appearance in the computation..". Which change? The appearance of what?
It is not clear. To understand it, the change of value in the event $\epsilon_1$ (maybe the assignment assigns a different value to some variable) affects the appearance of the event $\epsilon_2$.

> line 644 (also other places). Perhaps "upper bound" is a better term than "estimate", since you are stating soundness.
Agree.


- line 659, "if the command with label $l'$ can execute right after". Though I likely understand what you mean here (simple control-flow), this statement is technically ambiguous ("can execute" refers to your formal semantics, and cannot be computed).
Good point. We will rewrite this sentence to make it unambiguous!


- line 840, "The algorithm uses another algorithm AdaptBD_{SCC} recursively..". I don't see the recursion.
The algorithm AdaptBD is not shown in the paper. We will add it. @Jiawen, not recursively, right?

- line 864, "first collects all the paths". Which data structure collects all the paths? What do you mean by "all the paths"? There are infinitely many of those. Perhaps all simple paths/simple cycles?
"first collects all the paths in $SCC_i$",  from line 13 to line 15, every vertex v represents a path because we say "paths collected in step 1 are all simple cycles
with same starting and ending vertex", so at line 13, every v representing a path from v to v. Sorry for the confusion, to be precise, the algorithm handles paths on the fly, one by one. "collect all the paths" is implicit and not precise. We will change the sentence.   


- line 867, "by the property of SCC", which property?

- line 872, a comment on how you compute the maximum over symbolic expressions (or what types of symbolic expressions you can handle) would be helpful.



Monor: 
Thanks for pointing typos out, we will fix them.

