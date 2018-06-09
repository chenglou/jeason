# Jeason

Jeason stands for "JavaScript Reason" or "Janky Reason". It takes your JavaScript and converts it into really bad [Reason](http://facebook.github.io/reason/) code.

This isn't meant to be a transpiler. It's a codemod that gets 80% of the way there. The converted Reason file will throw you type errors and what not. **You learn Reason while converting real JS files over and fixing the type errors.** Two birds one stone!

Because if this relaxation of constraints, everyone can contribute their favorite translation of JS idioms to Reason! For example, **we support converting React.js components over to the working Reason bindings**. The actual bindings will be released soon. Join us on [Discord](discord.gg/reasonml)!

## ~~How to Use It~~

**Warning**: might not be working right now. Please see the contributing section to use this

```sh
yarn add global https://github.com/chenglou/jeason.git
jeason myFileName.js
```

This will read `myFileName.js` and spit out the converted Reason code in the terminal =).

## What Is This Black Magic?

If you're not familiar with the Reason toolchain, here's how this repo works under the hood: it forked [Flow](https://github.com/facebook/flow) in order to use its JavaScript parser, takes the parsed abstract syntax tree and uses Reason's pretty-printer to print out the code.

## Contributing

- Clone this repo
- Make sure you're on OPAM switch `4.02.3` (might work with other switches; haven't tried)
  - You need `reason` (`refmt`) and `ocamllex` installed through OPAM (latter comes by default)
- `yarn && yarn build`
- `yarn test`

This repo is actually a fork of Flow's `src/parser/` directory, pulled out. The only files that we added are:

- `main.re` (core logic)
- `jeason.sh` (convenience script)
- `test.re`
- `getReasonAst.re` (for development iteration)
- `bsconfig.json` (for the build)

We tweaked:

- `package.json`
- `.gitignore`

We also removed:

- `flow_parser_dot_js.ml`
- `flow_parser_js.ml`

Just because.

`main.re` is a big file (don't be misled by its size! It's mostly pattern matching on all the possible JS AST nodes).

Say you want to convert a variable declaration, e.g. `var a = 1`,  to a Reason let binding, e.g. `let a = 1`.

Note that the first snippet is JS code and the second one is Reason. I wish I could find more distinct examples between the two languages, but unfortunately Reason's syntax is just [too good and familiar](https://reasonml.github.io/docs/en/syntax-cheatsheet.html). Heck, when your boss comes over and asks "Woah there what's this new language you're writing in?" you can just go "oh that's ES2030 syntax with proper pattern matching and function currying, we're using a Babel transform".

Back on topic, you'd first enter the a JS code example `var a = 1` into [ASTExplorer](https://astexplorer.net/#/A8QiKiG0pm), switch to the `flow` parser and check the JS AST on the right. These are the tokens present in `main.re`. Find that case and use OCaml/Reason's built-in [Reason nodes construction mechanisms](http://caml.inria.fr/pub/docs/manual-ocaml/libref/Ast_helper.html) to output the right Reason AST node.

How to find the "right Reason AST node"? You can paste `let a = 1;` into `./getReasonAst.re` and run `yarn getReasonAst` to print the AST.

Try your transform by pasting the the JS code snippet into `./test.js`, then run `yarn test`.

Send some PRs! It's a crappy converter; as long as your heuristics output nice Reason code, we can accept it =).
