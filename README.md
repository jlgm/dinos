# Dinos x Robots Simulator

This project provides an API for simulating robots walking on a grid and fighting some dinosaurs - even though these poor creatures are already extinct. 

## Requirements

* [Java JDK 8](http://openjdk.java.net/install)
* [Leiningen 2.8.1](https://leiningen.org)

## Running

Run with command: `lein run`

The API will then be listening on port 8080.

## Features

* Create an empty grid (default 50 x 50)
* Place a robot in a certain position (robots by default are facing to the right)
* Place a dinosaur in a certain position (dinasours don't move)
* Issue commands to a robot, such as:
	- turn left/right
	- move forward/backwards
	- attack
* Display current board state

### Routes

* **GET /show-state**
	- Shows board current state.
	- Example: `curl http://localhost:8080/show-state`

* **POST /place-dino**
	- Places a dinosaur on a given position (passed on body of request)
	- Example: `curl --data "{\"x\":45,\"y\":0}" http://localhost:8080/place-dino`

* **POST /place-robot**
	- Places a robot in a given position (passed on body of request)
	- Example: `curl --data "{\"x\":45,\"y\":0}" http://localhost:8080/place-dino`

* **POST /robot-cmd/:op**
	- Execute a robot command
	- Valids operations (:op) are: `fwd-move` `rev-move` `rotate-left` `rotate-right` `attack`
	- Example: `curl --data "{\"x\":45,\"y\":0}" http://localhost:8080/robot-cmd/attack`

* **POST /reset-board**
	- Resets the board state to an empty grid
	- Example: `curl --data "{}" http://localhost:8080/reset-board`

## Illustration

This is how a **GET /show-state** would print the grid on a browser:

```
#####
#####
#####
#####
#####
```

Now, if we add a dinosaur on the [1 1] position: `curl --data "{\"x\":1,\"y\":1}" http://localhost:8080/place-dino`

This is how the board will look like:

```
#####
#D###
#####
#####
#####
```

Please notice this board is 5x5 for illustration purpose. The real board will be 50x50.

## Testing

Run all unit tests with command: `lein test`

## Documentation

An automatically generated documentation are available on the `doc` folder.

## Final Observations

* Issuing an invalid command will either return a 400 code or just ignore the command
* Attempting to place a robot (or dinosaur) into an occupied cell will replace it
* `/show-state` will refresh automatically after 2 seconds

## License

Copyright © 2018

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
