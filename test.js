let asd = 1;
let asd2;
5;
"hi";
a;

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
//
// let a1 = React.createClass({});
//
// let a2 = React.createClass({
//   foo: 1.5,
//   getInitialState() {
//     return {a: 1};
//   },
//   getInitialState2: function() {
//     function asd2() {return 1};
//   },
//   getInitialState2dotFive: function() {
//     function asd2(a) {return 1};
//   },
//   getInitialState3: function() {
//     function asd2(a, b) {return 1};
//     let asd3 = function foo (a, b) {a; return 1};
//   },
//   getInitialState4: function() {
//     (function(a) {return 1});
//     let asd3 = function foo (a, b) {a; return 1};
//     return {a: 1};
//   },
//   getInitialState7 = (a, b) => 1,
//   getInitialState8 = () => 2,
//   getInitialState9() {},
//   shouldComponentUpdate: function(nextProps, nextState) {
//     let nothing;
//     let a = "hello world";
//     let b = 6;
//     foo();
//     foo(a);
//     (a.baz ()).bar(a, b);
//     var c = 7;
//     const d = 8;
//     return b;
//   },
//   handleClick: (e) => {
//     return 1;
//     this.props.handleClick();
//   },
//   handleClick2: (e) => {
//     this.props.handleClick(this.foo.bar);
//     this.props.handleClick2();
//   },
//   render: function() {
//     if (a && b() || c) {
//       b;
//       c;
//       null;
//     }
//     return true
//   }
// });

// source:
class comp extends React.Component {
  // the following propTypes should generate some externals
  static propTypes = {
    // name: React.PropTypes.string.isRequired,
    // name2: React.PropTypes.string,
    // className: React.PropTypes.oneOfType([React.PropTypes.number.isRequired]).isRequired,
    // className2: React.PropTypes.oneOfType([React.PropTypes.number.isRequired]).isRequired,
    // onClick: React.PropTypes.oneOf(["foo"]),
    // foo: React.PropTypes.shape({
    //   foo: React.PropTypes.arrayOf(React.PropTypes.number.isRequired).isRequired,
    // }),


    "string" : React.PropTypes.string,
    "bool" : React.PropTypes.bool,
    "number" : React.PropTypes.number,
    "object_" : React.PropTypes.object,
    "symbol" : React.PropTypes.symbol,
    "any" : React.PropTypes.any,
    "element" : PropTypes.element,
    "func" : React.PropTypes.func,

    "stringRequired" : React.PropTypes.string.isRequired,
    "boolRequired" : React.PropTypes.bool.isRequired,
    "numberRequired" : React.PropTypes.number.isRequired,
    "object_Required" : React.PropTypes.object.isRequired,
    "symbolRequired" : React.PropTypes.symbol.isRequired,
    "anyRequired" : React.PropTypes.any.isRequired,
    "elementRequired" : React.PropTypes.element.isRequired,
    "funcRequired" : PropTypes.func.isRequired,

    "oneOfType" : React.PropTypes.oneOfType([]),
    "oneOf" : React.PropTypes.oneOf([]),
    "objectOf" : React.PropTypes.objectOf([]),
    "instanceOf" : React.PropTypes.instanceOf([]),
    "arrayOf" : React.PropTypes.arrayOf(React.PropTypes.string),
    "shape" : React.PropTypes.shape({
      foo: bar,
      "bar": React.PropTypes.arrayOf(React.PropTypes.string).isRequired
    }),
    "shape2" : React.PropTypes.shape({
      foo: bar,
      "bar": React.PropTypes.arrayOf(React.PropTypes.string.isRequired)
    }).isRequired

  };

  state = {
    count: 0,
    name: 1
  };

  id = null;

  componentDidMount() {
    console.log("mounted!");
    this.id && setInterval(() => {
      console.log('asd');
    }, 1000);
  }

  componentWillUnmount() {
    this.id && clearInterval(this.id);
  }

  handleClick = () => {
    this.setState(function(state) {
      return {
        count: state.count + 1
      };
    });
  };

  handleClick2 = () => {
    var check = React.PropTypes.string.isRequired;
    return this.setState({
      count: this.state.count + 22
    });
  };

  render() {
    return <div onClick={this.handleClick}>
             <div>
               {this.state.count}
             </div>
             <Comp onClick={this.handleClick} style={{ border: "1px solid black" }}>
               {this.state.count}
             </Comp>
             <ReFile2 inner={1} something="duckyou">
               {this.state.name}
               {this.state.count}
               {this.props.children}
             </ReFile2>
             <ReFile2 inner={1} something="asd" cb={() => this.handleClick} cb2={this.handleClick}>
               {this.state.name}
               {this.state.count}
               {this.props.children}
             </ReFile2>
             <ReFile2 inner={1}>
               {this.state.name}
               {this.state.count}
             </ReFile2>
             <ReFile2 inner={1} something={foo} />
             <div>
               {this.props.name}
             </div>
             {[<div key={1}> {this.props.name + "hello"} </div>, <div key={2}> {this.props.name} </div>]}
           </div>;
  }
}

// var comp = React.createClass({
//   // the following propTypes should generate some externals
//   propTypes: {
//     name: React.PropTypes.string.isRequired,
//     asd: React.PropTypes.oneOfType([React.PropTypes.number.isRequired]),
//     asd2: React.PropTypes.oneOf(["foo"]),
//     asd3: React.PropTypes.shape({
//       foo: React.PropTypes.oneOf(["foo"]).isRequired,
//     }),
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
//     var check = React.PropTypes.string.isRequired;
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
//              <ReFile2 inner={1} something="duckyou">
//                {this.state.name}
//                {this.state.count}
//                {this.props.children}
//              </ReFile2>
//              <ReFile2 inner={1} something="asd" cb={() => this.handleClick} cb2={this.handleClick}>
//                {this.state.name}
//                {this.state.count}
//                {this.props.children}
//              </ReFile2>
//              <ReFile2 inner={1}>
//                {this.state.name}
//                {this.state.count}
//              </ReFile2>
//              <ReFile2 inner={1} something={foo} />
//              <div>
//                {this.props.name}
//              </div>
//              {[<div key={1}> {this.props.name + "hello"} </div>, <div key={2}> {this.props.name} </div>]}
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
