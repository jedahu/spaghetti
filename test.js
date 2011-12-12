phantom.injectJs('jasmine/jasmine.js');
phantom.injectJs('jasmine/ConsoleReporter.js');
phantom.injectJs('out/all.js');

var fs = require('fs');

jasmine.getEnv().addReporter(
    new jasmine.ConsoleReporter(
      function(msg) { fs.write('/dev/stdout', msg, 'w'); },
      function(runner) {
        if (runner.results().passed()) phantom.exit(0);
        else phantom.exit(1);
      }));
jasmine.getEnv().execute();
