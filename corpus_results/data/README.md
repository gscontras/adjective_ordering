# Tgrep2/TDTlite search

The databases swbd.tab, bncw.tab, and bncs.tab are not in the repo because they're too big. They can be found on Judith's computer in this directory. Searches were run on jdegen@slate from ./projects/adjectives for swbd/bncs and from ./projects/adjectives_bncw for bncw. The latter got its own search because it's much larger and there are over 4 million cases of any adjective occurring before any noun. Things crashed, so I ran three separate searches on bncw (adj_1, adj_2, and adj_3 subdirectories):

```
@ REFEXP        (@NP=np < (/^JJ/=adj < @TARGETADJ $.. (/^NN/ !,, (@TERMINAL ,, =adj !>> (@DISFL >> =np)))));
@ REFEXP        (@NP=np < (/^JJ/=adj , (/^JJ/ < @TARGETADJ > =np) $.. (/^NN/ !,, (@TERMINAL ,, =adj !>> (@DISFL >> =np)))));
@ REFEXP        (@NP=np < (/^JJ/=adj , (/^JJ/ > =np , (/^JJ/ < @TARGETADJ > =np)) $.. (/^NN/ !,, (@TERMINAL ,, =adj !>> (@DISFL >> =np)))));
```
with @TARGETADJ being defined as

```
@ TARGETADJ     /^(red|yellow|green|blue|purple|brown|big|small|huge|tiny|short|long|wooden|plastic|metal|smooth|hard|soft|old|new|rotten|fresh|good|bad|round|square)$/;
```

instead of the more general search used for swbd and bncs (where cases of adjectives we're not interested in are weeded out later in R):

```
@ REFEXP	(@NP=np < (/^JJ/=adj $.. (/^NN/=noun !,, (@TERMINAL ,, =adj !>> (@DISFL >> =np)))));
```

Then I threw the three subsearches together in adjectives_bncw

``` bash
cat adj_*/results/bncw.tab > bncw.tab
```

# Results

Total number of cases with target adjectives: 270,694

Number of cases in each corpus:

| Corpus | Cases |  Cases with target adjectives | Target cases with one adjective | Target cases with two adjectives | Target cases with at least three adjectives |
| -------|-----:|-----:|-----:|-----:|-----:|
| swbd | 15744 | 2287 | 2128 (93%) | 154 (7%) | 5 (.2%) |
| bncs | 201261 | 22944 | 20959 (91%) | 1878 (8%) | 111 (.5%) |
| bncw | NA | 270694 | 237107 (88%) | 3195 (12%) | 1662 (.6%) |


For cases with more than one modifying adjective: mean distance from the head noun by adjective class

![Distance by adjective class](/corpus_results/graphs/mean_distance_from_noun_morethanonemodifier.jpg "Distance by adjective class for cases with more than one modifier")