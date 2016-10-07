let asd = 1;
let asd2;

let foo = {
  a: 1,
  b: () => 2,
  c: (a, b) => 3,
};

function asd() {
  return 1;
}

let arrow = (b) => 2;
let arrow = () => 2;

if (a) {
  b
}

bla();
bla(a);

let a1 = React.createClass({});

let a2 = React.createClass({
  foo: 1.5,
  getInitialState() {
    return {a: 1};
  },
  getInitialState2: function() {
    return {a: 1};
  },
  getInitialState3() {},
  getInitialState4 = (a, b) => 1,
  getInitialState5 = () => 2,
  shouldComponentUpdate: function(nextProps, nextState) {
    let nothing;
    let a = "hello world";
    let b = 6;
    foo();
    foo(a);
    (a.baz ()).bar(a, b);
    var c = 7;
    const d = 8;
    return b;
  },
  handleClick: (e) => {
    return 1;
    this.props.handleClick();
  },
  handleClick2: (e) => {
    this.props.handleClick(this.foo.bar);
    this.props.handleClick2();
  },
  render: function() {
    if (a && b() || c) {
      b;
      c;
      d;
    }
    return 1
  }
});

// ==================== future test:
// source:
// var comp = React.createClass({
//   propTypes: {
//     name: React.PropTypes.string.isRequired,
//     asd: React.PropTypes.oneOfType([React.PropTypes.string]),
//     asd2: React.PropTypes.oneOf(["foo"])
//   },
//   id: null,
//   getInitialState: function() {
//     return {
//       count: 0,
//       name: 1
//     };
//   },
//   componentDidMount: function() {
//     console.log("mounted!");
//     this.id && setInterval(() => {
//       console.log('asd');
//     }, 1000);
//   },
//   componentWillUnmount: function() {
//     this.id && clearInterval(this.id);
//   },
//   handleClick: function() {
//     this.setState(function(state) {
//       return {
//         count: state.count + 1
//       };
//     });
//   },
//   handleClick2: function() {
//     return this.setState({
//       count: this.state.count + 22
//     });
//   },
//   render: function() {
//     return <div onClick={this.handleClick}>
//              <div>
//                {this.state.count}
//              </div>
//              <Comp onClick={this.handleClick} style={{ border: "1px solid black" }}>
//                {this.state.count}
//              </Comp>
//              <ReFile2.comp inner={1} something="duckyou">
//                {this.state.name}
//                {this.state.count}
//                {this.props.children}
//              </ReFile2.comp>
//              <ReFile2.comp inner={1} something="asd">
//                {this.state.name}
//                {this.state.count}
//                {this.props.children}
//              </ReFile2.comp>
//              <ReFile2.comp inner={1}>
//                {this.state.name}
//                {this.state.count}
//              </ReFile2.comp>
//              <ReFile2.comp inner={1} something={foo} />
//              <div>
//                {this.props.name}
//              </div>
//              {/* array */[<div key={1}> {this.props.name + "hello"} </div>, <div key={2}> {this.props.name} </div>]}
//            </div>;
//   }
// });


// expected reason code for the component part:

// let comp = React.createClass (
//   {
//     val displayName = "Something";
//     val propTypes = {
//       "name": PropTypes.isRequired PropTypes.string,
//       "asd": PropTypes.oneOfType [|PropTypes.string|],
//       "asd2": PropTypes.oneOf [|"foo"|]
//     };
//     val mutable id = None;
//     method getInitialState () :state => {"count": 0, "name": 1.};
//     method componentDidMount () => {
//       Console.log "mounted!";
//       this##id#=(Some (setInterval (fun () => print_endline "asd") 1000))
//     };
//     method componentWillUnmount () =>
//       switch this##id {
//       | None => ()
//       | Some id => clearInterval id
//       };
//     method handleClick _ =>
//       React.setState this (fun (state: state) => {"count": state##count + 1});
//     method handleClick2 _ => {
//       let state: state = React.getState this;
//       React.setState this {"count": state##count + 22}
//     };
//     method render () => {
//       let state: state = React.getState this;
//       let props: props = React.getProps this;
//       <div
//         onClick=this##handleClick>
//         <div> state##count </div>
//         <JsFile2Re
//           onClick=this##handleClick
//           style={"border": "1px solid black"}>
//           state##count
//         </JsFile2Re>
//         <ReFile2
//           inner=1
//           something=(Js.Null_undefined.return "duckyou")>
//           state##name state##count props##children
//         </ReFile2>
//         <ReFile2
//           inner=1
//           something=(Js.Null_undefined.return "asd")>
//           state##name state##count props##children
//         </ReFile2>
//         <ReFile2 inner=1> state##name state##count </ReFile2>
//         <ReFile2 inner=1 something=foo />
//         <div> props##name </div>
//         [|<div key=1> (props##name ^ "hello") </div>, <div key=2> props##name </div>|]
//       </div>
//     }
//   }
//   [@bs]
// );
