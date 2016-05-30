package com.devdiscoveries.genalg

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random
import scalaz._
import Scalaz._
import Kleisli._

/**
  * Created by hv01016 on 30-5-2016.
  */
trait ConcurrentGeneticAlgorithm[Individual] extends GeneticAlgorithm[Individual, Future] {

  override implicit def lift[A](a: A): Future[A] = Future.successful(a)

  override def selectParents: GAOperation[Population, (Individual, Individual)] =
    kleisli { population =>
      val parent1 = population(Random.nextInt(population.size))
      val parent2 = population(Random.nextInt(population.size))
      Future.successful(parent1, parent2)
    }

  override def selectElite: GAOperation[RankedPopulation, Population] = {
    kleisli { population =>
      Future {
        population.sortBy(_._2).take(3).map(_._1)
      }
    }
  }

  override def breed(populationSize: Int): GAOperation[RankedPopulation, Population] =
    kleisli { rankedSelectedParents =>
      val selectedParents = rankedSelectedParents.map(_._1)
      val elite = selectElite.run(rankedSelectedParents)
      elite.flatMap { e =>
        val children = Future.sequence(for {
          i <- Range(e.size, populationSize)
        } yield breedNewIndividual.run(selectedParents))
        children.map(e ++ _)
      }
    }

  override def generateInitialPopulation(populationSize: Int): GAOperation[Unit, Population] =
    kleisli[Future, Unit, Population] { (x: Unit) =>
      Future.sequence(Range(0, populationSize).map(_ => generateRandomIndividual).toSeq)
    }

}
