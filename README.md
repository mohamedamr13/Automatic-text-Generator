# Automatic-text-Generator
This is an algorithm written in Haskell that statistically learns from a set of documents to predict future text.

![image](https://user-images.githubusercontent.com/65868639/117166594-d0043a80-adc6-11eb-93e4-b63babca47f3.png)


## generateOneProb
Generates the probability of a word following two other words given a trigram frequency pair and
a list of Bigram frequencies. The probability is calculated as follows:
Prob(w3|w1,w2) = count(w1,w2,w3) / count(w1,w2)


```haskell
-- getFreq
getFreq :: (Eq a, Num b) => a -> [(a,b)] -> b

getFreq para [] = error(" Search Parameter not found !")
getFreq para (h:t)
          | para == fst h = snd h
		  | otherwise = getFreq para t



-- GENERATE PROBABILTY 

generateOneProb :: Fractional a => ((String,String,String),a) ->[((String,String),a)] -> a


generateOneProb ( (a,b,_) , freq ) l 
            | freq == 0 = 0 
 			| otherwise =    freq / ( getFreq (a,b) l )

 ```
 
 
## generateNextWord
Given a list of two words and a probability pairs list, it randomly chooses the next word taking into
consideration that the probability of this word following those two words is greater than 0.03. 


```haskell
third ( ( _ , _ , x ) , _ ) = x 

generateNextWord l1 l2 = third ( find2 l1 l2 )

find2 l1 l2 = find 0  ( randomZeroToX ( length ( filter3 l1  l2 ) - 1 ) ) (  filter3 l1 l2 ) 

```

## generateText
According to the content of the docs list in the DataFile.hs file and given a string of 2 items (words/-
punctuation) and number n, you should generate n words/punctuation taking into consideration
the statistics mentioned above and return the whole text including the two words you started with.

```haskell
 	
generateText :: String -> Int -> String
generateText l n = if ( n == 0 ) then l else  append l ( generateTextH l n 0 " " )

a = wordTokenList docs

probPairs = genProbPairs (trigramsFreq a) (bigramsFreq a)
	
word l = ( wordToken l )

append s1 s2 = s1 ++ " " ++ s2
	
f s = generateNextWord ( wordToken s ) probPairs




generateTextH l n count acc
            |  count == 0 = generateTextH (append (w) x ) n (count+1) x
			| count >  0 && count < n = generateTextH l1 n (count+1) ( append acc x )
			| otherwise = acc
			where w = last ( wordToken l )
			      x = ( f l ) 
			      l1 = append ( w ) x



```




