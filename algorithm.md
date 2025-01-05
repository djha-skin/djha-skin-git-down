Maintain a list of asd to git repositories reverse lookup map.

A repo record has a url and a checkout branch.

Map requiring of `asd`s to repositories requiring each other.

Then if one repo _x_ is requiring another _y_ from commit _c_, identify commit
_k_ in repo _y_ such that _k_ is the first commit in the checkout branch
reachable from HEAD with timestamp strictly less than that of HEAD. Identify
commit _t_ on that branch whose commit is strictly older than _c_ which is
tagged. Which one did they mean? _t_ or _k_?

For now we chose _t_ if it exists, otherwise _k_, unless configured to take
latest. They can also configure a specific ref.



