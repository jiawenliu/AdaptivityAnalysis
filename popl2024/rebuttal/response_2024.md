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
 the clarifications and examples accordingly to the suggestions 
 of the reviewers.

 3. We will improve our evaluation by adding the actual generation
 error, and by highlighting the mechanism our heuristic chooses. We
 will also improve the comparison between our choices of mechanisms
 with the best mechanism.

 4. We will carefully rewrite the sentences which causes confusions.
  

- Detailed Response

Review A  

First of all, thanks for the detailed comments, section by section 
and your constructive suggestion on using more examples to clarify 
some key concepts. We will make the recommended changes to improve
our paper and make it easier to follow.

Question to be address by this response:
------------------------------------------
> Please clarify Figs. 2(a) and 2(c).  The ``last adaptive query'' is the 
  401st in Fig. 2(a), which for Gaussian noise has RMSE 0.37.
  But in the x=400 position of Fig. 2(c), RMSE for Gaussian noise appears 
  to be 0.028, which is wildly different.  Clearly, I'm not
  understanding what you meant to convey.

  Thanks for pointing out this. With Fig. 2(c) we want to show that the generalization error
  is also affected by the number of queries, and how different mechanism affect the analysis 
  in a different way. To do this we fixed the data analysis to be the tworounds example as in 
  Fig 2(a) and we run it with multiple queries and multiple mechanisms. One essential difference
  that we did not emphasize is that in the two figures we use a different data sizes, mostly 
  to account for the fact that we run many more queries. Using a larger data set in Fig. 2(c), 
  as we know, also has the result of affecting the magnitude of the error.  
  We will make sure to put the two plots on a similar scale and we will discuss the parameters
  of these two experiments in more details.


> Shouldn't Theorems 2.1-2.3 all contain the clause ``with high probability'' 
  (as do the informal statements of theorems in [18])?

  Yes, indeed. We had at first more precise statements at first and when we changed them we 
  erroneously left this out. We will revise them. 

> Please give an example to illustrate lines 749-762

  Thanks for pointing out that this part is unclear. We will use Fig 6 to make it more clear as follow. 
  Lines 749-762 describe how our reachability bound function TB(e,c) returns a 
  symbolic upper bound for an edge e. This is used to compute the weights of each node.
  TB(e,c) calculate the local bound of e, as described in lines 738-748. If the local bound
  is a symbolic constant TB(e,c) just return it. For example, in Fig.6(b), the edge e1=(0, top, 1) 
  has local bound 1 so TB(e1,c) returns 1 as shown in the node 0 in Fig.6(c) with weight 1 (node 0
  has only one edge, so the weight coincide with TB(e1,c) ).  
  Otherwise, if the local bound of e is not SC, it will be a variable. For instance, still in Fig.6(b), 
  the edge e4=(4, j' <= j -1 ,5) is in an SCC appearing in a while loop, so the local bound is the
  variable j. Then, to get TB(e4,c), we calculate two quantities: the first one is the sum of the 
  reachability bound of all the edges that may increment j, such as edges of the form 
  (l, j' <= j + v ,l') plus the increment v, v is a SC. In this example ther is no edge that 
  increments j, so the first quantity is 0. 
  The second one is the sum of the reachability bound of edges which reset j, such as edges 
  of the form (l, j' <  v ,l'), multiplied by the maximal value of the associated symbolic 
  expression v. For the edge e4, the only edge that resets j is e2=(1,j' <= k ,2), for which we 
  have TB(e2,c) = 1 and the maximal value of k is k. So the second quantity is k.
  In this case, TB(e4,c) = k. It shows that the edges e4 will be executed at most k times. This 
  also corresponds to the weight of the node 4, since there is only one edge outgoing from it. 
  Notice that also TB((5,T,2),c) = k since also in this case the local bound is the variable j.
  We will also consider adding more explicit computations of the reachability bound in Figure 6.
   

Response to comments
----------------------------------------------------------
> The explanation is confusing at several key points (749, Alg. 1, explanation of Alg. 1, 899)
> There are several places that would be helped by examples.

  We will improve the explanation of Alg 1 and add mode examples. Thanks for pointing this out.

> There are several places where the presentation would be helped by 
  providing the explanatory text/example *before* the definition/theorem.  
  One example: Definition 5.

  We will move explanatory text and examples before definition 5 and we will do the same in other 
  similar places.


 2. Overview

> I'm confused by lines 284-288: you use $\Chi$ to ``abstract a possible row'' (where 
  row is used in the singular), but then say that query($\Chi[j], \Chi[k]$) computes an 
  empirical mean.  I think the problem is the abuse of notation on line 193.  There, 
  ``query(P)'' returns an expectation.  However, in your programming language, the similar-looking 
  program statement ``query(P)'' computes an empirical mean (and involves an implicit loop that 
  you never talk about or represent in your IR).
  
  Thanks for pointing this out. Indeed we use "query" with two different meanings and two different
  types. We will change the notation to avoid confusion. 
  
> lines 289-290: ``compute an approximation of the product of the empirical mean of the 
  first $k$ attributes.''  I'm even more mystified: in Fig. 3(a), statement 5 accumulates 
  $a$ as a running sum.  You've added together emperical means of products of the 
  $j$-th attribute with the $k$-th attribute.

  We agree, this sentence is wrong. What we do is adding together the empirical means of 
  products of the $j$-th attribute with the $k$-th attribute. We will fix this.

5. Algorithm


> line 864: ``first collects all the paths in $SCC_i$''.  First, you probably mean 
  ``all simple cycles.''  Second, I don't see any code in Algorithm 1 that creates a 
  data structure that collects any set of paths.

  We see that this sentence is confusing and we will rephrase it. In fact, in the loop 
  at lines 13-15 every vertex v represents a path because they are all simple cycles
  with the same starting and ending vertex, so at line 13, every v represents a path 
  from v to v. Also we don't need a data structure because the algorithm handles paths 
  on the fly, one by one. We will clarify this.


6. Examples

> A poor explanation in lines 899-917: What is the goal of the ``monitor argument''?  
  You haven't said enough, and suddenly introduce a ``hidden database D'' in lin 903.

  The monitor argument is a technique to estimate the accuracy of a
  set of potentially adaptive queries over the population by using only the answer of the
  accuracy of a carefully chosen query. We will provide more details in the revised version
  of the paper.

7. Implementation

> line 1017-19: How about a much longer time out for the three examples, like 120 minutes?

   jumbo takes around 40 mins. The long time is for computing the weights, AdaptBD is fast. 
   For the other two examples, the weights cannot be computed in 120mins. We will try to 
   experiment more with them to see if we can have longer time outs for them.


> Table 3: in line 1105 to the end of the section, you give a lot of text discussing 
  the circumstances in which the heuristic in your tool would choose one mechanism or 
  another.  You should indicate the choice in the table, then then the reader would 
  easily see how the choices (based on your analysis) agreed with the best generalization 
  error, shown in bold.  You've only given us half the story in Table 1, and make 
  the reader hunt for insight in the text from line 1105 to the end of the section!

  Thanks for the feedback. In fact, as written in 1097-1098 the bold is used to 
  identify the results chosen by our heuristic. Some of them are not the best, 
  for instance, for RQ and nDPair, our choice (in bold) in Table 3 is not 
  the best among the 3 mechanisms but still acceptable. We will clarify that our 
  choice is highlighted in Table 3 to make it clear. 

Thanks for all the other comments, which we will also address, and for pointing out 
the minor points, we will fix them.



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

