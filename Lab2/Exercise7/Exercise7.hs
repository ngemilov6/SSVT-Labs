--- Time Spent: 10min

{-
Yes, there is a difference between the symmetric closure of the transitive closure of a relation R 
and the transitive closure or the symmetric closure of a relation R.
The key differenc is reflexivity. If the symmetric closure is computed first, there are back-and-forth pairs on every "edge",
if we look at the relation as a graph of nodes where the nodes represent elements of the domain, and arrows represent relations.
In this case, reflexive arrows are added when the transitive closure is applied, as there are always pairs xRy, yRx 
which creates xRx under the transitive closure.
If the transitive closure is applied first, none of these reflexive arrows are added, unless a cycle exists (x R n1, n1 R n2, ..., nk R x).
An example to illustrate this difference is the simple relation on the domain [1,2] with relation R = {(1,2)}.
Applying the transitive closure to R, we get R' = {(1,2)}, then applying the symmetric closure we get R'' = {(1,2), (2,1)}.
If we apply the symmetric closure first we get R' = {(1,2), (2,1)}, then applying the transitive closure gives R'' = {(1,1), (1,2), (2,1), (2,2)}.
This counter example proves that there is a difference in the result, depending on the order in which the closures are applied.
-}