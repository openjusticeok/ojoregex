# ojoRegex

This will be a package inside of the `{ojoverse}` family. It will only contain a few things:

* Datasets of regex patterns that we've defined for cleaning charge descriptions, etc.
* A function for applying / joining those categories to a table from `{ojodb}`
  * This will have a few layers to it

The idea is that we want to be able to tweak / update our regexes whenever we want, without having to do a breaking change to `{ojodb}`. We want people to be able to use any version of the package with any version of the regex, and you should be able to stay with "version 1.3" of the regex or whatever if you need to.
