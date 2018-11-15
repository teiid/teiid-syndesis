// Karma configuration file, see link for more information
// https://karma-runner.github.io/1.0/config/configuration-file.html

module.exports = function (config) {
  config.set({
    basePath: '',
    frameworks: ['jasmine', '@angular-devkit/build-angular'],
    plugins: [
      require('karma-jasmine'),
      require('karma-chrome-launcher'),
      require('karma-jasmine-html-reporter'),
      require('karma-junit-reporter'),
      require('karma-coverage-istanbul-reporter'),
      require('@angular-devkit/build-angular/plugins/karma')
    ],
    junitReporter : {
      // .../beetle-studio/target/karma-reports/*.xml
      outputDir : './target/karma-reports/'
    },
    angularCli: {
      environment: 'dev'
    },
    customLaunchers: {
      ChromeHeadless: {
        base: 'Chrome',
        flags: [
          '--headless',
          '--disable-gpu',
          '--remote-debugging-port=9222',
        ]
      }
    },
    reporters: ['junit', 'progress'],
    autoWatch: false,
    browsers: [ 'ChromeHeadless' ],
    singleRun: true
  });
};
