# Alonzo — Functional Language Interpreter in OCaml

**Alonzo** is a small, experimental **functional programming language** inspired by the **λ-calculus**, designed and implemented as a project for the course **IFT2035 — Concepts of Programming Languages** at **Université de Montréal (UdeM), Fall 2025**. The goal is to implement an interpreter for Alonzo in **OCaml**, supporting basic functional constructs, variable binding, and application.

## Language Overview

Alonzo’s syntax is simple and purely functional:

```bash
<expr>  ::= <let> | <fun> | <apply> | <var>  
<let>   ::= "(let (" <ident> " " <expr> ") " <expr> ")"  
<fun>   ::= "(fun " <ident> " " <expr> ")"  
<apply> ::= "(" <expr> " " <expr> ")"  
<var>   ::= <ident>  
<ident> ::= <letter> <ident> | <letter>  
<letter>::= "a" | ... | "z" | "A" | ... | "Z" | "0" | ... | "9" | "-"
```

### Examples

- Variable: `x`  
- Identity function: `(fun x x)`  
- Function application: `((fun x (fun y x)) y)`  
- Nested let binding: `(let (s (fun f (fun x (f x)))) (s s))`

---

## Project Structure

- `interpreter.ml` — Parser, evaluator, and substitution logic  
- `toplevel.ml` — Interactive OCaml REPL to test Alonzo expressions  

---

## Requirements

- OCaml >= 4.14  
- `utop` (optional, for interactive testing)  

---

## Running Alonzo

1. Clone the repository:

```bash
git clone <repo-url>
cd alonzo

```
2.	Compile the interpreter and toplevel:
   ```bash
ocamlc -o alonzo interpreter.ml toplevel.ml

```
3.	Run the interactive REPL:
   ```bash
./alonzo
