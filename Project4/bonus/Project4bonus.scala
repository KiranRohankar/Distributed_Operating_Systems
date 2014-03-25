import akka.actor._
import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props
import akka.actor.ActorRefProvider
import akka.routing.RoundRobinRouter
import akka.actor.ActorRef
import scala.util.Random
import java.io.File
import java.io.PrintWriter
import java.io.BufferedWriter
import java.io.FileWriter

trait Msg
case object Listen extends Msg
case object ActionTerminate extends Msg
case object CONVERGE extends Msg

object Project4bonus extends App {
	val mainSystem= ActorSystem("mainSystem")
	val bossSystem= ActorSystem("bossSystem")
	val refofWorkers:Array[ActorRef]= new Array[ActorRef](10)
	var main:ActorRef=null 
	for(i<-0 to 9) {
		val file= new File("Log"+i+".txt")
		val pen= new PrintWriter(new BufferedWriter(new FileWriter(file,true)))
		pen.println("Source Actor | Destination Actor | Time   | Message")
		pen.close()
	}

	val file= new File("LogBoss"+".txt")
	val pen= new PrintWriter(new BufferedWriter(new FileWriter(file,true)))
	pen.println("Source Actor | Destination Actor | Time   | Message")
	pen.close()
	main = mainSystem.actorOf(Props(new boss with Trait2))

	class boss extends Actor with Trait1 {
		var count=0 
		var count2=0
		for(i<-0 to 9) {
			val prop2 = Props(new worker(i))
			refofWorkers(i)= bossSystem.actorOf(prop2)
		}
		val random= Random.nextInt(10)
		var time= System.currentTimeMillis()
		// refofWorkers(random)! Listen
		send(refofWorkers(random),"boss",random.toString,time, Listen)
		def receive = {
			case ActionTerminate => 
				count=count+1
				if(count==10) {
					println("***********************************")
					println("System is terminating..")
					bossSystem.shutdown
					mainSystem.shutdown
				}
			case CONVERGE=> {
				count2=count2+1
				if(count2==10) {
					println("System is Converged!!!")
				}         
			}
		}
	}

	class worker(id:Int) extends Actor with Trait1  {
		var count= 0
		def receive = {
			case "Hi" => 
				println("I came in childActor and so Second boss")
			case Listen => 
				// println("I am worker with id:"+id)
				count=count+1
				if(count==1) {
					var time= System.currentTimeMillis()
					send(main,id.toString,"boss",time, CONVERGE)
					// main ! CONVERGE
				}
				if(count==10) {
					var time= System.currentTimeMillis()
					send(main,id.toString,"boss",time, ActionTerminate)
					// main ! ActionTerminate
				}
				else {
					var random= Random.nextInt(10)
					while(random==id) {
						random=Random.nextInt(10)
					}
					var time= System.currentTimeMillis()
					send(refofWorkers(random),id.toString,random.toString,time, Listen)
					// refofWorkers(random)!Listen 
				}
		}
	}//end of worker
}//end of object