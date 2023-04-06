## How to write negative tests

Often we need to check for errors also. We have to be sure that some 
TX fails. Maybe this TX is malicious one, and we have to be sure that it can not pass.
For that we have useful function:

```haskell
mustFail :: Run a -> Run a
mustFail action = ...
```

It saves the current state of blockchain and 
tries to run the `action` and if the action succeeds it logs an error
but if action fails it does not log error and retrieves the stored previous
blockchain state. 

This way we can ensure that some scenario fails, and we can proceed
the execution of blockchain actions.


