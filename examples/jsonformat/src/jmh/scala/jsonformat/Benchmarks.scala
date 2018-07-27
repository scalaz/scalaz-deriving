// Copyright: 2010 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package jsonformat

import org.openjdk.jmh.annotations.{ State => Input, _ }

// jsonformat/jmh:run -i 5 -wi 5 -f1 -t2 -w2 -r2 .*Benchmarks
//
// see org.openjdk.jmh.runner.options.CommandLineOptions
class Benchmarks {}

@Input(Scope.Benchmark)
class Data {}
