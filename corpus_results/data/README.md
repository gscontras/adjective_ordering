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


All the following plots are of mean distance from the head noun for each of the 26 adjectives that we tested in the faultless disagreement/pairwise preference experiments. 

For cases with at least two modifying adjectives (39,199 cases): mean distance from the head noun by adjective class

![Distance by adjective class](/corpus_results/graphs/mean_distance_from_noun_morethanonemodifier.jpg "Distance by adjective class for cases with at least two modifiers")

It looks like these results pattern quite nicely with the faultless disagreement data, where the order is quality, size, age, texture, color, shape, material. So: quality and size are reversed (though in the corpus counts the difference between the two is teeny-tiny), as are color and shape. This is really quite similar!

Distribution over adjective class for these 39,199 cases:

| | | | | | | | |
| ------| ---:| ---:| ---:| ---:| --:| ---:| ---:|
| **Class** |     age     | size |    color |  quality |  texture | material |    shape |
| **Count** |   15688 |   11576 |     5024 |     3790 |     1720 |     1183 |      218 | 
| **Proportion** | 0.40   |  0.30 |     0.13 |     0.10 |     0.04 |     0.03 |     0.01 |

Split up by corpus (swbd: 180 cases, bncs: 2,232 cases, bncw: 36,787 cases), it looks like the pattern we observe globally also holds in each corpus, though size and quality are reversed in the bncs, which is more in line with the faultless disagreement data:

![Distance by adjective class and corpus](/corpus_results/graphs/mean_distance_from_noun_morethanonemodifier_bycorpus.jpg "Distance by adjective class and corpus for cases with at least two modifiers")

Split up by adjective:

![Distance by adjective and corpus](/corpus_results/graphs/mean_distance_from_noun_morethanonemodifier_byadj.jpg "Distance by adjective and corpus for cases with at least two modifiers")

At the by-adjective level, the only pattern that's mirrored in the faultless disagreement data is the difference in the quality adjectives. For most of the others there's no effect in the fd data anyway, with the exception of the age adjectives: there seems to be more fd for "fresh" than for "rotten". This is not reflected in the corpus; here, instead, the only difference is that "old" is closer to the noun than the rest of the adjectives.

For cases with three modifying adjectives (2,091 cases): mean distance from the head noun by adjective class

![Distance by adjective class](/corpus_results/graphs/mean_distance_from_noun_morethantwomodifiers.jpg "Distance by adjective class for cases with  three modifiers")

Again, we see a similar pattern as in the fd data (and on this subset the quality/size order comes out the same). Here, the outlier is shape, though the rest is ordered the same. There are only six shape cases, though. For your amusement:

*them little square welsh terriers*    
*those round white dutch cabbages*     
*small square coloured inserts*        
*the square transparent boiled sweets*
*the dismal gray square outside*      
*small square coloured inserts*

Distribution over adjective class for these 2,091 cases:

| | | | | | | | |
| ------| ---:| ---:| ---:| ---:| --:| ---:| ---:|
| **Class** |     age     | size |    color |  quality |  texture | material |    shape |
| **Count** |   797 |     627 |      338 |      162 |       87 |       74 |        6 |
| **Proportion** | 0.38 |    0.30 |     0.16 |     0.08 |     0.04 |     0.04 |     0.003 |



It would be nice to compare the corpus counts to the faultless disagreement/ordered preference experiment data in a more fine-grained way, e.g. by noun class (furniture vs. food), which seems to play a role. Because there were only 92 cases in total of AAN/AAAN where N was one of the nouns we tested, I instead put together a list of 125 food (mostly fruit and vegetables) and 54 furniture nouns in both their singular and plural forms. If a given case did not contain one of these nouns, it was excluded. The remaining 668 cases entered the graph below, showing the mean distance from the head noun by adjective class, split up by the class of the noun it occurs with. 

Distribution over adjective classes:

| | | | | | | | |
| ------| ---:| ---:| ---:| ---:| --:| ---:| ---:|
| **Class** |    size |  material | age | color | texture |  quality |   shape |
| **Count** |    202  |    144 |      135 |      111 |       56 |       18 |        2 |
| **Proportion** | 0.30  |   0.22 |     0.20 |     0.17 |     0.08 |     0.03 |     0.003 |

Distribution over noun classes:

| | |
| ------| ---:| ---:|
| **Class** |     food | furniture |
| **Count** |      233 |      435 |

![Distance by adjective class and noun class](/corpus_results/graphs/mean_distance_from_noun_morethanonemodifier_bynounclass.jpg "Distance by adjective class and noun class for cases with at least two modifiers")

