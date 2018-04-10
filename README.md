# haskell-todo-mvc

A haskell backend for a [todo MVC](http://todomvc.com) app

```bash
# create a new todo
$ curl --verbose --request POST --header "Content-Type: application/json" \
    --data '{"name": "do stuff" }' \
	http://localhost:8080/todos

# get all todo in database
$ curl --verbose --request GET --header "Content-Type: application/json" \
	http://localhost:8080/todos

# get certain todo in database
$ curl --verbose --request GET --header "Content-Type: application/json" \
	http://localhost:8080/todos/1
```
