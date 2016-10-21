/* refmt -print ast dparse.re | pbcopy */
type props =
  Js.t <
    inner : int,
    something : Js.null_undefined string,
    children : Js.null_undefined React.reactElement
  >;

external props : inner::int => something::Js.null_undefined string? => unit => 'reactJsProps = "" [@@bs.obj];

let aTop = Js.Null.empty;

let bTop = 6.5;

6;

"asd";

a;

japplique ();

japplique2 a b;

let foo = {a: 1};

if a {b};

let myObj = {a: 1, b: fun () => 2, c: fun a b => 3};

let f a => 1;

let asd = createClass (
  {
    val propTypes = {
      "name": React.PropTypes.isRequired React.PropTypes.string,
      "name2": React.PropTypes.string,
      "className": React.PropTypes.oneOfType [|React.PropTypes.isRequired React.PropTypes.number|],
      "onClick": React.PropTypes.oneOf [|"foo"|],
      "foo": ReactRe.PropTypes.shape {
        "foo": ReactRe.PropTypes.isRequired (
          ReactRe.PropTypes.arrayOf (ReactRe.PropTypes.isRequired ReactRe.PropTypes.number)
        )
      }
    };
    val asd = "asd";
    val propInit = fun () => 1;
    method getInitialState () :state => {
      let a = 5;
      5();
      {"count": 0, "name": 1}
    };
    method haha foo => {
      let check = React.PropTypes.string.isRequired;
      oneCall1 ();
      oneCall2 ();
      let aInner () => 5;
      oneCall3 ();
      let bInner a => 6;
      let cInner = 7 + 1;
      if cond {b};
      this##leThis##leThat this##leObj##laObj;
      (foo.bar ()).oneCall4 ()
    };
    method haha2 foo bar => 1;
    method haha3 () :state => {
      let state: state = ReactRe.getState this;
      let okA = true && true || false;
      let okB = FooModule.Bar.baz.baaz;
      let a = Some foo;
      <div onClick=this##handleClick> 1 2 3 </div>
    }
  }
  [@bs]
);
