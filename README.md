# Viv
"We are many. We are one"

## Status
Viv is currently in an experimental stage. The code may be interesting to
read, but it is essentially non-funcitonal.

## The Idea
Viv is based on a single (dumb) idea: What if we tried to cram basically
everything into one langauge? Currently, I'm working on implementing:
+ Functional Programming
+ Imperative Programming
+ Concatenative Programming
+ Logic Programming

Currently in planning are:
+ Actor Model Concurrency (process calculus)
+ Hardware Programming (maybe) (BlueSpec vs SystemVerilog)
+ Type system (Dependent)


## What do I mean by Paradigm (Execution Model)?

If you notice your favourite paradigm missing, it's likely because I'm using a
different definition of paradigm than you. I find 'Programming Paradigm' to be a
very vague term, and here am using a more specific definition, which I call the
'Execution Model' definition. 

The idea is that we identify programming paradigms based on the structure of
their abstract (operational) semantics. For example:
+ Functional programming languages are identified with reduction steps, i.e. a
  'computation' is a sequence of terms which are related by a reduction
  relation. So, the program `(fn x -> x + 2) 5` first reduces (via β-reduction)
  to `5 + 2`, which then reduces to 7. 

+ Imperative programming languages compute by transitioning between states. So,
  we might have the program `x := 5; x := x + 3; x := x * 2`. If we consider the value of
  `x` to be the program state, then the sequence of states this program
  transitions through are `x = 5`, then `x = 8`, then `x = 16`.
  
+ Logic programming languages compute via unification and search (either breath or
  depth-first). 

+ Concatenative programming langauges have a very similar characteristic to
  imperative languages. The primary difference is that imperative langauges
  speak of named locations, which may be created upon request (new/malloc),
  while concatenative programming languages manipulate an explicit stack.
  
+ Actor-based programming langauges have their semantics based on a process
  calculus, which makes concurrency much more explicit. 
  
  
