# BeetleStudio

This project was generated with [Angular CLI](https://github.com/angular/angular-cli) version 1.3.2.

## Code scaffolding

Run `ng generate component component-name` to generate a new component. You can also use `ng generate directive|pipe|service|class|guard|interface|enum|module`.

## Build

If you have maven installed  run `mvn clean install`, this will install the YARN and dependency resolve your modules and build and test your project. The build artifacts will be stored in the `dist/` directory. Use the `-prod` flag for a production build.

You can also run `ng build` locally to build.

## Running unit tests

In addtion to above maven test, you can also Run `ng test` to execute the unit tests via [Karma](https://karma-runner.github.io).

## Running end-to-end tests

Run `ng e2e` to execute the end-to-end tests via [Protractor](http://www.protractortest.org/).
Before running the tests make sure you are serving the app via `ng serve`.

## Building the "Data Services NPM Component" locally

Run `ng build beetle-studio-lib`, this will generate a `dist/lib` folder which will have the required files for publishing.

## Publishing the "Data Services NPM Component" to NPM.JS
```
ng build beetle-studio-lib
cd dist/lib
npm login
npm publish
```

## Further help

To get more help on the Angular CLI use `ng help` or go check out the [Angular CLI README](https://github.com/angular/angular-cli/blob/master/README.md).
