package com.devdiscoveries.genalg

import akka.actor.FSM



case class Evolve(populationSize: Int, numberOfGenerations: Int)
case class Initialize(populationSize: Int, numberOfGenerations: Int)
case object Status
case class CurrentStatus[State](state: State,
                                numberOfGenerations: Int = 0,
                                currentGeneration: Int = 0)

sealed trait State
case object Idle extends State
case object Initialized extends State
case object Evolving extends State
case object Done extends State

sealed trait Data
case object Uninitialized extends Data
case class GenAlgData[Individual](population: Seq[Individual],
                         numberOfGenerations: Int,
                         currentGeneration: Int)

/**
  * Created by hv01016 on 7-6-2016.
  */
class GeneticAlgorithmFSM extends FSM[State, Data] {

  startWith(Idle, Uninitialized)

  when(Idle) {
    case Event(Initialize(popSize, numGens), Uninitialized) =>
      goto(Initialized)
  }

  when(Initialized) {
    case Event(Evolve, data: GenAlgData[_]) =>
      goto(Evolving)
  }

  when(Evolving) {
    case Event(Done, data: GenAlgData[_]) =>
      goto(Idle)
  }

  whenUnhandled{
    case Event(Status, s) =>
      stay replying CurrentStatus(state = stateName)
    case Event(e, s) =>
      log.warning("received request {} is unhandled in state {}/{}", e, stateName, s)
      stay
  }

  initialize()
}
