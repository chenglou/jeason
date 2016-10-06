/* refmt -print ast dparse.re | pbcopy */
let aTop = Js.Null.empty;

let bTop = 6.5;

{
  val asd = "asd";
  val propInit = fun () => 1;
  method haha foo => {
    oneCall1 ();
    oneCall2 ();
    let aInner = 5;
    oneCall3 ();
    let bInner = 6;
    let cInner = 7;
    (foo.bar ()).oneCall4 ()
  };
  method haha2 foo bar => 1;
  method haha3 () => 1
};
