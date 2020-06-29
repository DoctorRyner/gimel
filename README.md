# Gimel

# ALPHA RELEASE IS OUT!
Now I'm goint to build some infrastructure, the first step is to make a website with documentation, you can follow my progress here https://github.com/DoctorRyner/gimel.io

Also, it can be a good example of Gimel's usage

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
