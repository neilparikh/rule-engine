# A rule engine for allowing users to rules over data in your application

## Running the example

```
$ stack build
$ cat example.txt | stack exec rules-engine-exe
```

## Usage

You can find an example of how use this in `app/Main.hs`.

The flow is:
1. Use `parseRule` to receive an `Either ParseError Rule`.
2. If you have a `Right rule`, pass that to `validateRule`, along with a list of all the valid actions, and a list of all the valid variables.
    - This is a separate function call, so that you can surface a failure to the user.
3. If `validateRule` returns `True`, pass the rule, along with `Map` from variable names to their value, a `Map` from actions, to their callback functions, and a context value to pass to the callback if it is triggered.
    - This context value is useful if you are running the rule against multiple sets of data, so that you can distinguish which data set triggered the rule.
