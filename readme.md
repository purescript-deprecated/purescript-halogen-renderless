# Renderless

[![Latest release](http://img.shields.io/github/release/thomashoneyman/purescript-halogen-renderless.svg)](https://github.com/thomashoneyman/purescript-halogen-renderless/releases)
[![Maintainer: thomashoneyman](https://img.shields.io/badge/maintainer-thomashoneyman-lightgrey.svg)](http://github.com/thomashoneyman)

Renderless components are one approach for code reuse among Halogen components. By abstracting out the render function altogether, you can implement the component's behaviors and state without making any rendering decisions as a library author. As a library user, you can implement your rendering however you see fit and you can selectively attach the behaviors that have been made available to you.

This repository contains two things:

- the minimal implementation for a renderless component you can use as a template for your own components in `/example`
- a library of helper functions for working with the `Store` comonad for your component state in `/src`.

The component implemented in this project is a working renderless component. Either take it and use it as a scaffold for a new component you're building and would like to share, or use it to update an existing component to be renderless.

## Getting started

``` purescript
# psc-package
psc-package install halogen-renderless

# bower
bower install purescript-halogen-renderless
```

## Module documentation

Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-halogen-renderless).

## Renderless Components Built With This Library

- [purescript-halogen-select](https://github.com/citizennet/purescript-halogen-select)
- [purescript-halogen-formless](https://github.com/thomashoneyman/purescript-halogen-formless)

# Examples

For demonstration purposes the renderless components in this repository are a fully-functioning Halogen app. The `/example` folder contains scaffolds for renderless components you can use to get started, and demonstrate the library helper functions in use.

- `yarn` will install everything you need
- `yarn build` will build the project, which you can view at `dist/index.html`

## Contributing

Check out the [contribution guidelines](https://github.com/thomashoneyman/purescript-halogen-renderless/blob/master/.github/contributing.md) to get started and see helpful related resources.

