# Noun-specific effects in adjective ordering

## Goal

Targeted test for noun-specific effects in adjective ordering preferences.

## General approach

Put together a set of adjectives and nouns that vary in the probability of occurrence of the adjective-noun bigrams given the individual adjective and noun probabilities. In particular, we want some cases that are less frequent than expected from the individual frequencies, some that are more frequent, and some that are smack in the middle. Then test those cases for adjective ordering preferences.

## Method

1. Extract all A N cases from BNC. 

2. For each A, extract probability of occurrence. 

3. For each N, extract probability of occurrence. 

4. For each A N, extract probability of occurrence.

5. For each A N, compute probability of occurrence from 2. and 3.

6. Restrict dataset to include just the 26 As we've tested so far and check for high, low, and mid probability cases. Discuss with Greg.