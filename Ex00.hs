module Ex00 where

name, idno, username :: String
name      =  "Yifan Zhu"  -- replace with your name
idno      =  "18300717"    -- replace with your student id
username  =  "zhuyi"   -- replace with your TCD username


declaration -- do NOT modify this
 = unlines
     [ ""
     , "@@@ This exercise is all my own work."
     , "@@@ Signed: " ++ name
     , "@@@ "++idno++" "++username
     ]

{- Modify everything below here to ensure all tests pass -}

hello  =  "Hello World :-)"
