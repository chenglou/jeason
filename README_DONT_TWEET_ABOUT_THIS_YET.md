# Jeason

Jeason stands for "JavaScript Reason" or "Janky Reason". It takes your JavaScript and converts it into really bad [Reason](http://facebook.github.io/reason/) code.

This isn't meant to be a transpiler. It's a codemod that gets 80% of the way there. The converted Reason file will throw you type errors and what not. **You learn Reason while converting real JS files over and fixing the type errors.** Two birds one stone!

Because if this relaxation of constraints, everyone can contribute their favorite translation of JS idioms to Reason! For example, **we support converting React.js components over to the working Reason bindings**. The actual bindings will be released soon. Join us on [Discord](discord.gg/reasonml)!

## How to Use It

```sh
git clone https://github.com/chenglou/jeason.git
cd jeason
npm install
npm start
```

This will take in  the `./test.js` file in ths repo and spit out Reason code in the terminal =). Or you can directly invoke `./_build/src/main.native myFileToConvert.js`

## What Is This Black Magic?

If you're not familiar with the Reason toolchain, here's how this repo works under the hood: it forked [Flow](https://github.com/facebook/flow) in order to use its JavaScript parser, takes the parsed abstract syntax tree and uses Reason's pretty-printer to print out the code.

## Contributing

Most files in this codebase are Flow files. We haven't stripped out all the unnecessary parts yet. The only file that matters to us is `src/main.re`, the converter itself.

Check out the `npm start` part: it compiles the converter and calls `_build/src/main.native ./test.js`. `main.re` is a big file (don't be misled by its size! It's mostly pattern matching on all the possible JS AST nodes).

Say you want to convert a variable declaration, e.g. `var a = 1`,  to a Reason let binding, e.g. `let a = 1`.

Note that the first snippet is JS code and the second one is Reason. I wish I could find more distinct examples between the two languages, but unfortunately Reason's syntax is just [too good and familiar](http://facebook.github.io/reason/#syntax-basics). Heck, when your boss comes over and asks "Woah there what's this new language you're writing in?" you can just go "oh that's ES2030 syntax with proper pattern matching and function currying, we're using a Babel transform".

Back on topic, I would first enter the a JS code example `var a = 1` into [ASTExplorer](https://astexplorer.net/#/A8QiKiG0pm), switch to the `flow` parser and check the JS AST on the right. These are the tokens present in `main.re`. Find that case and use OCaml/Reason's built-in [Reason nodes construction mechanisms](http://caml.inria.fr/pub/docs/manual-ocaml/libref/Ast_helper.html) to output the right Reason AST node.

How to find the "right Reason AST node"? You can paste `let a = 1;` into `./getReasonAst.re` and run `npm run getReasonAst` to print the AST.

Send some PRs! It's a crappy converter; as long as your heuristics output nice Reason code, we can accept it =).
