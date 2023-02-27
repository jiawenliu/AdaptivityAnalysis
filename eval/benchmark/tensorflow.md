# Summarization

Below is a short summary on the evaluations so far, on the tensorflow, sklearn and pytorch bechmarks. These programs are widespread in real-world machine learning area.

## Difficulties

The weakness in our practical usefulness in these benchmarks are
1. Reimplementation of the real-world data analysis program to be compatible with different mechanisms  will bring extra burden to the programers.   
2. There isn’t too many complex data analysis program, most of the “adaptivity” in the data analysis programs can be specified/determined directly by parameters of these algorithm, such as the epochs, steps-per-epochs, batch-size, etc. In this sense, the auto-analysis of the adaptivity isn’t so necessary.
3. The auto-adaptivity analysis might reduce the efficiency of the data analysis program.

## Proposed method

1. For the point 1, I think I can reimplement the core training/fitting programs in existing tensorflow/sklearn library and make them accessible to programmers. Then we can solve this issue.
2. For the point 2, I haven’t find some real-world data analysis programs that have complex loop structures. So this could be an issue.
3. For the point 3, I think we might need either auto translator or reimplementation tool for different data analysis programs in order to make connection to the realistic programs.

## Conclusion 
1. adaptivity is very useful and it can help to improve the generalization error in realistic data analysis programs. However, adaptivity is easy to be obtained and controlled by programers.
2. With the adaptivity in hand, reimplementation of the realistic data analysis programs to be compatible with different mechanisms is the real burden on the programmers. 
3. Instead of choosing mechanism to reduce the generalization error, sometimes it is easier and more effective to do it by changing the adaptivity directly.


## Some comparison results

Though the reimplementation is complex, we still get some good evaluation results, below I attached few darfting plots on some deep learning algorithms in comparison with the mechanized version.

the y-axis are the predicated values computed by different algorithms. The more they are close to the true value, the more accurate they are.
To make them useable on the paper, I will need to rescale them.
￼
￼