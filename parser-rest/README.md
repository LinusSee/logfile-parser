# parser-rest
Here I write the rest service that will allow a user to create parsers and to run those parsers on a logfile.
For this it needs the following components:
1. Have a few simple parsers like "matches <some_string>". The user can then create parsers specifying <some_string>.
2. Be able to use the parsers created in step 1 to build parsers for a single "logline".
This means building a parser for what would be logged by a single `log("someStuff")` command.
Keep in mind that this could span a single line like `log("This is in a single line.")` or multiple lines, e.g. like `log("This \\nwill \\nspan \\nmultiple \\nlines")`. Therefore the distinction between "logline" and "line". See [here](#creating-a-logline-parser) for a detailed example.
3. Apply a parser created in step one repeatedly to a file until there is no input left to parse.

## Api
- /api
   - /parsers/building-blocks/complex
     <br>GET:&nbsp;&nbsp;&nbsp;&nbsp; Returns a list of applied elementary parsers (e.g. "matching <some_string>").
     <br>POST:&nbsp;&nbsp;      Apply an argument to a simple parser. Creates a new applied parser.

## Creating a logline parser
A logline parser is supposed to parse what is written by a single `log()` command. Let's look at two examples.
Example 1:
```
2020-11-02 17:08:00,889 DEBUG Reconfiguration started for context sun.misc.Launcher$AppClassLoader@37b90b39
```
Example 2:
```xml
2020-11-02 17:08:00,889 DEBUG Couldn't process queue message: <?xml version="1.0"?>
<catalog>
   <book id="1">
      <author>Schwartz, Richard</author>
      <title>Askir</title>
      <genre>Fantasy</genre>
      <price>20.00</price>
      <publishing_date>2006</publishing_date>
      <description>Among the best fantasy literature until today.</description>
   </book>
</catalog>
```
Both of those could be logged by a simple command, but one is single line and one is multiline. A logline parser should parse both of them in a single iteration. This means that every time the parser is finished we start again at a completely new message, logged by a different `log()` command.
