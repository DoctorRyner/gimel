const React = require("react")

exports.useEffectTest = () => React.useEffect(() => console.log("USE TESSA"))

exports.inFC = f => () => f()

exports.testComp = () => {
    console.log("EVAL COMP")
    React.useEffect(() => console.log("USE TESSA"))
    return React.createElement("div", null, "Hello World")
}
