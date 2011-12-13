// # Testing
//
// To test this project ensure that [phantomjs] is installed, then compile the
// clojurescript to a single file `out/all.js`, then run the tests with
// `phantomjs test.js`.
//
// [phantomjs]: http://phantomjs.org

phantom.injectJs('jasmine/jasmine.js');
phantom.injectJs('jasmine/ConsoleReporter.js');
phantom.injectJs('out/all.js');

var fs = require('fs');

jasmine.getEnv().addReporter(
    new jasmine.ConsoleReporter(
      function(msg) { fs.write('/dev/stdout', msg, 'w'); },
      function(runner) {
        try {
          if (runner.results().passed()) phantom.exit(0);
          else phantom.exit(1);
        } catch (e) {
          phantom.exit(1);
        }
      }));
jasmine.getEnv().execute();
