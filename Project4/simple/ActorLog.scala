import java.io.File
import java.io.PrintWriter
import java.io.BufferedWriter
import java.io.FileWriter

object ActorLog { }

class MyLogger(id:String) {
	val file= new File(""+id+".txt")
	val pen= new PrintWriter(new BufferedWriter(new FileWriter(file,true)))
	pen.println("Source Actor\t| Destination Actor\t| Time\t\t| Message")
	pen.close()
	def WriteMessage(actor1:String,actor2:String,time:Long,message:String) {
		try {
			if(file.createNewFile()) {
				println("File with id "+id+" is created")
			}
		}
		val outObject= new PrintWriter(new BufferedWriter(new FileWriter(file,true)));
		outObject.println(actor1+"\t\t| "+actor2+"\t| "+time+"\t| "+message);
		outObject.close();
	}
}//end of MyLogger