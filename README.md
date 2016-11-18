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

Check out the `npm start` part: it compiles the converter and calls `_build/src/main.native ./test.js`. `main.re` is a big file (don't be misled by its size; it's mostly pattern matching on all the possible JS AST nodes).
