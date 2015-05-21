The databases swbd.tab, bncw.tab, and bncs.tab are not in the repo because they're too big. They can be found on Judith's computer in this directory. Searches were run on jdegen@slate from ./projects/adjectives for swbd/bncs and from ./projects/adjectives_bncw for bncw. The latter got its own search because it's much larger and there are over 4 million cases of any adjective occurring before any noun. Things crashed, so I restricted the search to be the following disjunction:

```
	@ REFEXP    (@NP=np 
                 [< (/^JJ/=adj < @TARGETADJ $.. (/^NN/ !,, (@TERMINAL ,, =adj !>> (@DISFL >> =np)))) 
                | < (/^JJ/=adji , (/^JJ/ < @TARGETADJ > =np) $.. (/^NN/ !,, (@TERMINAL ,, =adji !>> (@DISFL >> =np))))
                | < (/^JJ/=adjii , (/^JJ/ > =np , (/^JJ/ < @TARGETADJ > =np)) $.. (/^NN/ !,, (@TERMINAL ,, =adjii !>> (@DISFL >> =np))))]);
```
with @TARGETADJ being defined as

```
@ TARGETADJ     /^(red|yellow|green|blue|purple|brown|big|small|huge|tiny|short|long|wooden|plastic|metal|smooth|hard|soft|old|new|rotten|fresh|good|bad|round|square)$/;
```

instead of the more general search used for swbd and bncs (where cases of adjectives we're not interested in are weeded out later in R):

```
@ REFEXP        (@NP=np < (/^JJ/=adj $.. (/^NN/=noun !,, (@TERMINAL ,, =adj !>> (@DISFL >> =np)))));
```
		
For the bncw, because of TDTlite restrictions, I extracted the noun separately depending on whether it occurred 1, 2, or 3 words away from the noun. To put them back together again, did the following from within data/bncw for each NounX.t2o file:

``` bash
	sed -n '/<none>/!p' ./Noun1.t2o > Noun1Modified.t2o
	sed -n '/<none>/!p' ./Noun2.t2o > Noun2Modified.t2o
	sed -n '/<none>/!p' ./Noun3.t2o > Noun3Modified.t2o
```

Then

``` bash
	cat Noun*Modified.t2o > Noun.t2o
```

Then you can just use Noun.t2o in options.

Number of cases in each corpus:

| Corpus | Cases |  Cases with target adjectives | Target cases with one adjective | Target cases with two adjectives | Target cases with at least three adjectives |
| -------|-----:|-----:|-----:|-----:|-----:|
| swbd | 15744 | 2287 | 2128 (93%) | 154 (7%) | 5 (.2%) |
| bncs | 201261 | 22944 | 20955 (91%) | 1878 (8%) | 111 (.5%) |
| bncw | | | | | | |
