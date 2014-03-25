import akka.actor._
import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props
import akka.actor.ActorRefProvider
import akka.routing.RoundRobinRouter
import akka.actor.ActorRef
import scala.util.Random

trait Msg
case object Listen extends Msg
case object ActionTerminate extends Msg
case object CONVERGE extends Msg

object Project4 extends App {
	val mainSystem= ActorSystem("mainSystem")
	val bossSystem= ActorSystem("bossSystem")
	val refofWorkers:Array[ActorRef]= new Array[ActorRef](10)
	var main:ActorRef=null 
	main = mainSystem.actorOf(Props(new boss))
	class boss extends Actor {
		var myfile=new MyLogger("boss")
		var count=0 
		var count2=0
		for(i<-0 to 9) {
			val prop2 = Props(new worker(i))
			refofWorkers(i)= bossSystem.actorOf(prop2)
		}
		val random= Random.nextInt(10)
		var time= System.currentTimeMillis()
		myfile.WriteMessage("boss", random.toString, time,"Hello")
		refofWorkers(random)! Listen
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
				// println("I am in converge")
				count2=count2+1
				if(count2==10)
					println("System is Converged!!!")
			}
		}
	}

	class worker(id:Int) extends Actor {
		var count= 0
		var myfile=new MyLogger(id.toString)
		def receive = {
			case Listen => 
				// println("I am worker with id:"+id)
				count=count+1
				if(count==1) {
					var time= System.currentTimeMillis()
					myfile.WriteMessage(id.toString, "boss", time,"Converge")
					main ! CONVERGE
				}
				if(count==10) {
					var time= System.currentTimeMillis()
					myfile.WriteMessage(id.toString, "boss", time,"Terminate")
					main ! ActionTerminate
				}
				else {
					var random= Random.nextInt(10)
					while(random==id) {
						random=Random.nextInt(10)
					}
				var time= System.currentTimeMillis()
				myfile.WriteMessage(id.toString, random.toString, time,"Hello")
				refofWorkers(random) ! Listen 
				}
		}
	}//end of worker
}