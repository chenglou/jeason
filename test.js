let asd = 1

let a1 = React.createClass({});

let a2 = React.createClass({
  foo: 1,
  getInitialState() {
    return {a: 1};
  },
  shouldComponentUpdate: function(nextProps, nextState) {
    return true;
  },
  handleClick: (e) => {
    this.props.handleClick();
  },
  render: function() {
    return 1
  }
});
