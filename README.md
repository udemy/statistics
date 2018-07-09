# com.udemy.statistics
JVM Statistics Library

This library was built to fill some of the gaps in currently available libraries. This is not intended to be the only 
library you need for performing statistics in the JVM. In fact, the thought is that this package would be a good 
complement to the Apache Commons math library.


# Naming Conventions

In order to have a consistent naming convention that allows for consumers of the library to more easily navigate it, 
we have put together some naming conventions which are followed herein. This is not meant to be 
exhaustive. We fully expect that as the library needs to be extended these conventions will need to be 
expanded on. The naming conventions, like all parts of this library, are open for improvement via issues and pull 
requests. This is a good example of a case when you need not write code to have a major positive contribution to this 
project.

Some of the naming conventions are influenced by the Apache Commons math library.

Distributions
----

All distributions will fall under the `distribution` package. Distributions have different common uses. For example, 
the first distribution included in this package, the normal inverse gamma, is primarily used as a conjugate, Bayesian 
prior. Thus, it has methods for Bayesian updating. This is different than, for example, the normal distribution which 
is more often used for sampling. Consequently some distributions may be implemented with methods for their common 
uses without implementing all of their methods. Nonetheless, all of the distributions should fall under the 
`distribution` package.

Another example of methods that would fall under the `distribution` package are methods for computing Bayesian, 
credible intervals. For example, if you want the highest density interval for the normal distribution, you would find 
that in the `Normal` class in the `distribution` package.

Algorithms
----

Various computational algorithms will be placed under the `algorithm` package. For example, an algorithm for computing 
the probabilities that each element of a set of distributions is the best will be found in this package. In general, 
methods or algorithms that are not closed form and are not related to any family of or specific distribution will be 
found in the `algorithm` package.
