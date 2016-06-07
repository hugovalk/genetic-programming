package com.devdiscoveries.genalg

import akka.actor.{Props, ActorSystem}
import akka.testkit.{ImplicitSender, TestKit}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

/**
  * Created by hv01016 on 7-6-2016.
  */
class GeneticAlgorithmFSMSpec extends TestKit(ActorSystem("GeneticAlgorithmSpec")) with ImplicitSender
  with WordSpecLike with Matchers with BeforeAndAfterAll {

  override def afterAll = {
    TestKit.shutdownActorSystem(system)
  }

  "The Genetic Algorithm" must {
    val gaActor = system.actorOf(Props(classOf[GeneticAlgorithmFSM]))
    "should start out in status Idle" in {
      gaActor ! Status
      expectMsg(CurrentStatus(Idle, 0, 0))
    }
    "should transition to Initialized with Initialize message" in {
      gaActor ! Initialize(1,1)
      gaActor ! Status
      expectMsg(CurrentStatus(Initialized, 0, 0))
    }
  }

}
