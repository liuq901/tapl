/* Examples for testing */

true;
if false then true else false; 

0; 
succ (pred 0);
iszero (pred (succ (succ 0))); 

lambda x. x;
(lambda x. x) (lambda x. x x); 