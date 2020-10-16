# logfile-parser
This will become a project that allows you to combine parsers in the frontend, to build more complex parsers.
These complex parsers can then be used to parse stuff like logfiles and extract data from it.

With this I want to:
1. Make it easy to search logfiles
2. Be able to easily parse logfiles with different formats
3. Enable a search easier than just searching in the logfile itself
4. Be able to extract content

<br>
For this I need to build the following components:

In the backend:
1. Provide basic parser building blocks
2. Allow saving configurations
3. Allow querying a file with a certain parser
4. Allow indexing a file

In the frontend:
1. A page to build and save more complex parsers
2. A page to build final logfileparsers
3. A page to query an indexed file
4. A page to upload and index a file
