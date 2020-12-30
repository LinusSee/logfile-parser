# parser-rest
Here I write the rest service that will allow a user to create parsers and to run those parsers on a logfile.
For this it needs the following components:
1. Have a few simple parsers like "matches <some_string>". The user can then create parsers specifying <some_string>.
2. Be able to use the parsers created in step 1 to build parsers for a single "logline".
This means building a parser for what would be logged by a single `log("someStuff")` command.
Keep in mind that this could span a single line like `log("This is in a single line.")` or multiple lines, e.g. like `log("This \\nwill \\nspan \\nmultiple \\nlines")`. Therefore the distinction between "logline" and "line". See [here](#creating-a-logline-parser) for a detailed example.
3. Apply a parser created in step one repeatedly to a file until there is no input left to parse.

## Api
- /api/parsers
    - /logfile
      <br>**GET:**&nbsp;&nbsp;&nbsp; Returns a list of all logfile parser names (a name can be used to apply a specific parser)
      <br>**POST:** Create the logfile parser send in the body
      - /apply/:parserName?target=foobar
        <br> **GET:**&nbsp;&nbsp;&nbsp; Send a `parserName` and a `target` to apply the logfileParser saved under the name `parserName` to the specified target
      - /apply
        <br> **POST:** Send a full logfile parser and target string in the body to apply the parser to a target
    - /building-blocks/complex
     <br>**GET:**&nbsp;&nbsp;&nbsp;&nbsp; Returns a list of applied elementary parsers (e.g. "matching <some_string>").
     <br>**POST:**&nbsp;&nbsp;      Apply an argument to a simple parser. Creates a new applied parser.
     - /apply/:parserName?target=foobar
       <br> **GET:**&nbsp;&nbsp;&nbsp; Send a `parserName` and a `target` to apply the parser saved under the name `parserName` to the specified target
     - /apply
        <br>**POST:** Send an applied parser and a target string and return the result of applying that parser to the target

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


## How to build it
### For development
During development all you need to run the project is execute the `stack build` command and then the `stack exec parser-rest-exe` command.
<br>
This will host a server on port `8080`, which is configured in the `assets/project-environment.ini` file. For testing purposes feel free to use the [postman collection](./assets/postman).

### For production
During production I wanted to run the app behind a proxy server, following this [yesod tutorial](https://www.yesodweb.com/book/deploying-your-webapp), which works just as well for `haskell-servant`.
<br>
To achieve this with minimum effort, I added a config file for production (`assets/project-environtment.ini`) where the port is configured to be `4321`.
<br>
The nginx server will then listen to port `8080` and pass all calls on to our app listening on port `4321` using this config.
```
server {
		listen			8080;
		server_name localhost;
		location / {
			proxy_pass http://127.0.0.1:4321; # Reverse proxy for parser-rest
		}
	}
```

While this is all well and good, first we need to build our app and run it.
For this I wrote a script `build_for_production.cmd`.
<br>
It starts with `stack clean` and `stack build` and then copies the resulting `.exe` as well as all necessary assets and config files into an archive called `parser-rest.zip`. During this process the project config `project-environment.prod.ini` is renamed to `project-environment.ini` so the correct config is provided to our production `.exe`.

So our steps are:
1. stack clean
2. stack build
3. copy all assets and configs
4. rename project-environment.prod.ini to project-environment.ini
5. zip it so the result is a standalone archive
6. copy the archive wherever you want and run it

If I understood the [yesod tutorial](https://www.yesodweb.com/book/deploying-your-webapp) correctly, all you have to do is run the exe. Since `servant`, like `yesod`, uses warp and warp is highly efficient, it is suitable for production (see this [github issue](https://github.com/haskell-servant/servant/issues/469) for more infos).
<br>
Our proxy will then reroute the calls to our app. Of course just clicking on the exe is not a great way to run it in production, we would normally use some kind of server process, e.g. via `systemd/systemctl`.
