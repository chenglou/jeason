/* refmt -print ast dparse.re | pbcopy */
let aTop = Js.Null.empty;

let bTop = 6.5;

6;

"asd";

if a {b};

let myObj = {a: 1, b: fun () => 2, c: fun a b => 3};

let f a => 1;

{
  val asd = "asd";
  val propInit = fun () => 1;
  method haha foo => {
    oneCall1 ();
    oneCall2 ();
    let aInner () => 5;
    oneCall3 ();
    let bInner a => 6;
    let cInner = 7;
    if cond {b};
    this##leThis##leThat this##leObj##laObj;
    (foo.bar ()).oneCall4 ()
  };
  method haha2 foo bar => 1;
  method haha3 () => {
    let okA = true && true || false;
    let okB = 2;
    3
  }
};
