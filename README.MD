# Genetic Programming
This project contains a genetic algorithm.
Specifically an implementation for a genetic programming algorithm.

## Using the project
Make sure you have a Java 8 SDK installed. This project is a Scala project, using SBT as build tool.

Run

    > activator

in a shell to run an SBT console. In the SBT console you can do the following:

`test` runs all the unit tests.

`eclipse` generates Eclipse (with Scala IDE plugin) project files

After installing the Scala plugin in IntelliJ IDEA, you can can use the project import wizard.

## Train an AI player for Three-in-a-row

In the SBT console, you can run `console` to start a Scala REPL.
Run the following commands to train an AI player and play the game:

    import com.devdiscoveries.examples._
    import scalaz._
    import Scalaz._
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.Await
    import scala.concurrent.duration._
    val gp = new ConcurrentThreeInARowGPAlgorithm

Now you can start training the algorithm with the following command:

    val futureResults = gp.evolve(1000,100).run()

Training will now start in the background. To check whether the calculation is complete, you run:

    futureResults.isCompleted

Or you just wait until CPU gets low again.

To play:

    val results = Await.result(futureResults, 2 seconds)
    new Game(ConsolePlayer(X), GPPlayer(O, results.head)).play

With some luck, you will see that your AI player has a tiny bit of 'intelligence'. It might try to complete a row
or at least try to not play an occupied square.

## Game rules for Three-in-a-row
The player who gets three squares in a row / column or diagonal wins.
You will lose when playing an already occupied square.
The input for the game is a number (1 - 9), to select a square to play.

    -------
    |1|2|3|
    |4|5|6|
    |7|8|9|
    -------

Note: when playing in a Scala REPL you might not see the number you type. But just
press enter. It will work.