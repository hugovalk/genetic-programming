package com.devdiscoveries.genalg

import scala.util.Random
import scalaz.Kleisli._
import scalaz._
import Scalaz._

trait SimpleGeneticAlgorithmInterpreter[Individual] extends GeneticAlgorithm[Individual, Option] {

  override implicit def lift[A](a: A): Option[A] = Some(a)

  override def selectParents: GAOperation[Population, (Individual, Individual)] =
    kleisli { population =>
      val parent1 = population(Random.nextInt(population.size))
      val parent2 = population(Random.nextInt(population.size))
      Some(parent1, parent2)
    }

  override def selectElite: GAOperation[RankedPopulation, Population] = {
    kleisli { population =>
      Some(population.sortBy(_._2).take(3).map(_._1))
    }
  }

  override def breed(populationSize: Int): GAOperation[RankedPopulation, Population] =
    kleisli { rankedSelectedParents =>
      val selectedParents = rankedSelectedParents.map(_._1)
      val elite = selectElite.run(rankedSelectedParents)
      val children = for {
        i <- Range(elite.size, populationSize)
      } yield breedNewIndividual.run(selectedParents).get
      elite.map(_ ++ children)
    }

  override def generateInitialPopulation(populationSize: Int): GAOperation[Unit, Population] =
    kleisli[Option, Unit, Population] { (x: Unit) =>
      Some(Range(0, populationSize).flatMap(_ => generateRandomIndividual).toSeq)
    }

}