# Gimel

## What is this?

This is an Elm-like UI library with power of React. Elm is great but it doesn't have that much widgets and ready-to-use solutions as React. Also PureScript is just much more powerful than Elm. I wanted to make this library as easy-to-use as Elm but with much greater potential

## How to run the examples?

If you don't have `npm`, you'll need to install it. For example, you can download it from here https://nodejs.org/en/download/

Just enter the examples folder and type `make init def`. Next time you can just type `make` because everything is already initialized

## Tools

We use:
* `PureScript` as a programming language because it's much more powerful and less error-prone than `JavaScript`, you can install it using `npm i -g purescript`
* `spago` as a PureScript build tool, it's much more flexible and reliable than pulp, you can install it using `npm i -g spago`
* `parcel` as a web bundler, we use it because it doesn't require ANY configuration in opposite to `webpack`

# DEPRECATED

## Templates

You can grab one of these for your project that will help you to start fast and without any pain

Any template can be treated just like `examples/`

### EMPTY

Doesn't contain anything and just logs `Hello, world!` in the browser console

This template is good for your own code structuring

### BASE

Contains basic project structure to develop in a standard way

### Quick Start

Contains some basic examples to play around with (we also show how to use react widgets like material-ui)
