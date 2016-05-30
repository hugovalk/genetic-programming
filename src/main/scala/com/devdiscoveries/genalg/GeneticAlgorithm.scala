package com.devdiscoveries.genalg

import scala.util.Random
import scalaz.Kleisli._
import scalaz._

trait GeneticAlgorithm[Individual, Response[_]] {
  type GAOperation[A, B] = Kleisli[Response, A, B]

  type Population = Seq[Individual]

  /** Absolute number. 0 is perfect fitness */
  type Fitness = Double
  type RankedPopulation = Seq[(Individual, Fitness)]

  implicit def lift[A](a: A): Response[A]

  def withProbability[A](p: Double)(operation: GAOperation[A,A]) =
    if (Random.nextDouble() <= p)
      operation
    else
      kleisli[Response, A, A] { a => a}

  def generateInitialPopulation(populationSize: Int): GAOperation[Unit, Population]

  def rankPopulation: RankPopulationFunction

  type RankPopulationFunction = GAOperation[Population, RankedPopulation]

  def selectFittest: GAOperation[RankedPopulation, RankedPopulation] =
    kleisli { rankedPopulation =>
      rankedPopulation.sortBy(_._2).take(rankedPopulation.size / 3)
    }

  def selectElite: GAOperation[RankedPopulation, Population]

  def selectParents: GAOperation[Population, (Individual, Individual)]

  def breed(populationSize: Int): GAOperation[RankedPopulation, Population]

  final def breedNewIndividual()(implicit b: Bind[Response]): GAOperation[Population, Individual] =
    selectParents andThen
      crossOver andThen
      withProbability(0.01){mutate}

  def mutate: GAOperation[Individual, Individual]

  def crossOver: GAOperation[(Individual, Individual), Individual]

  final def evolve(populationSize: Int = 100, numberOfGenerations: Int = 100)(implicit b: Bind[Response]) =
    generateInitialPopulation(populationSize) andThen
      evolveGenerations(populationSize, numberOfGenerations)

  final def evolveGenerations(populationSize: Int, numberOfGenerations: Int)(implicit b: Bind[Response]): GAOperation[Population, Population] = {
    val generations = for {
      _ <- Range(0, numberOfGenerations)
    } yield evolveNextGeneration(populationSize)
    generations.tail.foldLeft(generations.head)(_ andThen _)
  }

  final def evolveNextGeneration(populationSize: Int)(implicit b: Bind[Response]): GAOperation[Population, Population] =
    rankPopulation andThen
      selectFittest andThen
      breed(populationSize)

  def generateRandomIndividual: Response[Individual]
}

