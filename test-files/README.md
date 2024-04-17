## What this is

A lua-based templating engine, intended for a Haskell host program. Templates are loaded at runtime
and have access to the full capabilities of the lua language.

## Templates

A template is any text file. The result of running a template is just the content of the file, unless
it contains one of the special sequences `<%` or `<%=`. This library assumes all text is encoded in UTF-8,
but makes no attempt to check or validate this; you can use binary files as templates too, if you really
want to.

`<%` begins a lua section. This can contain any lua code you like, and ends when a `%>` is encountered—even
in a string or comment! `<%=` begins a lua expression segment, and also ends when a `%>` is encountered.
This should enclose a single lua expression, the result of which will be output when the `<%= expr %>` segment
is processed. Unpaired delimiters are allowed (and ignored), however the file MUST end in text mode,
i.e. with `%>` optionally followed by some text if it has any `<%`/`<%=` in it.

Operationally, each template file is translated into a single lua chunk, as follows:
- Text between `%>` and `<%` / `<%=` is translated into `_EMIT_CHUNK_N()`, where `_EMIT_CHUNK_N` is a magic function
  that just appends the text in that section to the output buffer. The same is true for text before the first `<%`/`<%=`
  in the file, and 
- Text between `<%` and `%>` is left as-is and becomes part of the lua chunk.
- Text between `<%=` and `%>` is wrapped in `emit( ... )` and then becomes part of the lua chunk.

The resulting lua chunk is executed in an environment modified as follows:

### Lua environment
The following extra functions are available in the lua environment:

- `emit(str)`, which takes exactly one string argument and appends it to the output buffer.
  This is the main way to programmatically work with the buffer.
- `with_buffer(fn)`, which calls `fn` with the current buffer's contents.
  - if `fn(current_buffer)` returns a string, that string *replaces* the buffer entirely
  - if `fn(current_buffer)` returns nil, nothing else happens (this is useful if you merely want to append to the buffer)
  - if `fn(current_buffer)` exits exceptionally, the buffer is left unmodified except for any calls to `emit` or equivalent that occured before the exit.
- TODO `include()` runs another file as a template

## Other Concerns

### Streaming

Streaming is not supported. The entire template file must fit in memory, and more importantly so must the output.

### Security

Templates should be considered trusted code: they can do anything that an arbitrary lua file can do. Don't splice untrusted
user input into templates, don't `eval()` user input, etc.

## Licensing

This project is licensed under the GNU GPL, version 3.0 or later.