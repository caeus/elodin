
Purpose of the grammar

1. Easy to parse (easy to implement for me)
2. Easy to read (easy to adopt by others)

How to make it easy to read?
Copy grammar of a well established grammar. Not exactly, not compatibly, but that it looks similar.
In this case JS

How to make it easy to parse?

In order to make the language easy to comprehend and construct. We're gonna talk about two kinds of
expressions. Greedy and delimited (maybe we add reluctant ones later, but so far, not the case).

Delimited expressions are those which have no ambiguity regarding its starting point and end.
In this kind inhabit the literal expressions such as numbers, text, boolean, etc.
Also, in this category we find references, and enclosed expressions (in between parentheses or 
curly braces)

Greedy expressions are those which have no clear termination rules. Among them there are
application, lambda expressions, let expressions and import expressions.

```bnf

delimitedExpr := literalExpr |
  arrayExpr |
  dictExpr |
  enclosedExpr

bindingsExpr := ref "=" expr (";" ref "=" expr)*

letExpr := "let" bindingsExpr ";" expr

lambdaExpr := "fun" "(" ref ("," ref)*")" "=" expr

importExpr := importHeader expr

argsExpr := (groupedExpr |
    "(" expr ("," expr)+ ")"
  )+
//+/ sum(1,2,3,4)
//-/ sum(1,2){ 3 }4
//+/ if(condition){let x=4; x}{}
applyExpr := delimitedExpr argsExpr? (operator applyExpr)?

expr := delimitedExpr |
  greedyExpr
greedyExpr := letExpr |
  lambdaExpr |
  importExpr |
  applyExpr
```

  
import "math" hiding {a,b,c} asd.qwe.rwe {depD=d,depE=e,depF=f} dfs.wer.fhgn;