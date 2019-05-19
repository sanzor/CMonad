module Writer where 
    
 newtype W a= W (a,String)

 apl::W a->(a->W a)->W a
 apl (W (a,s1)) f=let W (l,s)= f a in (W (l,s1++s)
