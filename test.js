phantom.injectJs('jasmine/jasmine.js');
phantom.injectJs('jasmine/ConsoleReporter.js');
phantom.injectJs('out/all.js');

jasmine.getEnv().addReporter(
    new jasmine.ConsoleReporter(
      function(msg) { console.log(msg); },
      function(runner) {
        if (runner.results().passed()) phantom.exit(0);
        else phantom.exit(1);
      }));
jasmine.getEnv().execute();
