# Home - Create Parser
On this page you can create a rudimentary parser. This means you take a predefined parser
that matches a certain type of data and apply it to data of your choice.
<br>
For example you could choose the `OneOf` parser that you apply to the input `'DEBUG', 'INFO', 'ERROR'`. This will create a parser that matches either 'DEBUG' or 'INFO' or 'ERROR'.


Currently the page looks like this.

![Image of the full create parser page](./full_page.PNG)

Now let's look at the different parts of the page.

## Create specialized parsers
In this section you can create a parser that matches the string or pattern you give it.
<br>
You select a type of parser (via the dropdown) and the value or values it should match and give it a name. Then you can submit it and a parser will be created. Here a few examples to make this clearer.
<br>
First if we wish to create a loglevel parser as described in the first section, we would do so as follows:
<br>
![Image of an example of how to create a loglevel parser](./create_loglevel_parser.PNG)

And if we wanted to create a parser matching a time format like this 'HH-MM' we could do so like this:
<br>
![Image of an example of how to create a time parser](./create_time_parser.PNG)

## Test existing parsers
Since it would be helpful to test if a created parser actually works, this section allows you to select an existing parser by name via the dropdown. You can then provide a target string which the parser will attempt to parse. The result is then displayed as a string below the input fields.

For example with our previously created time parser.
<br>
![Image of how to test our previously created time parser](./match_time.PNG)

If we provide a string that is too long, it still works, as long as it can match the start.
<br>
![Image of how to test our previously created time parser](./match_time_with_longer_string.PNG)
