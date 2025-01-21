semantic generator using Minimalist grammar an predicate logical expressions
----------------------------------------------
use the generate(LogicExp,Sentance,Lambda,Tree) to generate 
expressions given a fitting input expression

LogicExp = any predicate logical expression, constructable by a given lexicon
Sentace = the generated sentance/word
Lambda = the logical expression generated; should be identical to LogicExp
Tree = the derivation tree construction during generation
----------------------------------------------
the lexions are in the folder 'grammars' 
the lexicons can be changed in the load.pl file
----------------------------------------------
ltree.tex is a latex file that generates a pdf showing
the last succesfully generated derivation tree
