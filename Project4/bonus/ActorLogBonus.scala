import akka.actor._
import java.io.File
import java.io.PrintWriter
import java.io.BufferedWriter
import java.io.FileWriter

trait Trait1 extends Actor {
	def send(aRef: ActorRef ,actor1:String,actor2:String,time:Long, msg : Any){
		// println("I can write my log here--" + msg)
		val file= new File("Log"+actor1+".txt")
		val outObject= new PrintWriter(new BufferedWriter(new FileWriter(file,true)));
		outObject.println(actor1+"\t\t| "+actor2+"\t| "+time+"\t| "+msg);
		outObject.close();
		// println(actor1+"  "+actor2+"  "+time+"  "+msg)
		aRef ! msg
	}
}

trait Trait2 extends Actor with Trait1 {
	abstract override def receive = {
		case(msg : Any) =>
			// println("I am first" + msg)
			super.receive(msg);
	}
}

object ActorLog { }