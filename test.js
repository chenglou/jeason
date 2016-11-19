'use strict';

const React = require('React');
const {PropTypes} = React;

const MyComponent = React.createClass({
  propTypes: {
    name: PropTypes.string,
    age: PropTypes.number.isRequired,
  },

  getInitialState() {
    return {show: true};
  },

  handleClick(event) {
    event.preventDefault();
    this.setState({
      show: !this.state.show,
    });
  },

  render() {
    const msg = "Hello, my name is " + name + " and my age is " + age;
    return (
      <div className="foo" onClick={this.handleClick}>
        {this.state.show ? <div>{msg}</div> : null}
      </div>
    );
  },
});

module.exports = MyComponent;
