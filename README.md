passage
=======

[![hex.pm version](https://img.shields.io/hexpm/v/passage.svg)](https://hex.pm/packages/passage)
[![Build Status](https://travis-ci.org/sile/passage.svg?branch=master)](https://travis-ci.org/sile/passage)
[![Code Coverage](https://codecov.io/gh/sile/passage/branch/master/graph/badge.svg)](https://codecov.io/gh/sile/passage/branch/master)
[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

[OpenTracing](http://opentracing.io/) API for Erlang

[Documentation](https://hexdocs.pm/passage/)

Examples
---------

```erlang
%% Registers `tracer`
Context = passage_span_context_null,
Sampler = passage_sampler_all:new(),
Reporter = passage_reporter_process:new(self(), span),
ok = passage_tracer_registry:register(tracer, Context, Sampler, Reporter),

%% Starts a root span
RootSpan = passage:start_span(example_root, [{tracer, tracer}]),

%% Starts a child span
ChildSpan = passage:start_span(example_child, [{child_of, RootSpan}]),

%% Finishes spans
passage:finish_span(ChildSpan),
passage:finish_span(RootSpan),

%% Receives the finished spans
receive {span, FinishedChildSpan} -> ok end,
receive {span, FinishedRootSpan} -> ok end.
```

### Process Dictionary Version

```erlang
%% Registers `tracer`
Context = passage_span_context_null,
Sampler = passage_sampler_all:new(),
Reporter = passage_reporter_process:new(self(), span),
ok = passage_tracer_registry:register(tracer, Context, Sampler, Reporter),

%% Starts a root span
ok = passage_pd:start_span(example_root, [{tracer, tracer}]),

%% Starts a child span
ok = passage_pd:start_span(example_child),

%% Finishes spans
passage_pd:finish_span(),  % child
passage_pd:finish_span(),  % root

%% Receives the finished spans
receive {span, FinishedChildSpan} -> ok end,
receive {span, FinishedRootSpan} -> ok end.
```

### Parse Transform

```erlang
-module(example).

-compile({parse_transform, passage_transform}). % Enables `passage_transform'

-passage_trace([{tags, #{foo => "bar", size => "byte_size(Bin)"}}]).
-spec foo(binary()) -> binary().
foo(Bin) ->
  <<"foo", Bin/binary>>.
```

The above `foo` function will be transformed as follows:
```erlang
foo(Bin) ->
  passage_pd:start_span('example:foo/1', []),
  try
    passage_pd:set_tags(
        fun () ->
            #{
                'location.pid' => self(),
                'location.application' => example,
                'location.module' => example,
                'location.line' => 7
                foo => bar,
                size => byte_size(Bin)
             }
        end),
    <<"foo", Bin/binary>>
  after
    passage_pd:finish_span()
  end.
```

References
-----------

- [The OpenTracing Semantic Specification(v1.1)](https://github.com/opentracing/specification/blob/1.1/specification.md)
