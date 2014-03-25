package abcd

import akka.actor._
import akka.routing.RoundRobinRouter

case class Work(number:Long,number1:Long,sequence:Long)
case class output(Out:Boolean)

object Abc {

  def main(args: Array[String]) {
    
         val n:Long= args(0).toLong
   
	  
   
   
	  val seq:Long= args(1).toLong
	 
   
  val s= ActorSystem("s")
  val boss= s.actorOf(Props(new Boss(n,seq)),name = "boss")
 
  } 
}

	class Employee extends Actor {
		
	def receive= { 
		case Work(number:Long,number1:Long,sequence:Long) => 
		
		
		  var i:Long=number;
		 
   for(i <-number to number1)
   {
     var ans : Long= compute(i,sequence);
     var d:Double= Math.sqrt(ans);
     if((d%1==0)==true)
			{
       if(i==0)
       {}
       else{
       println(i)}
			
			}	
     
   }
   sender ! output(true) 
   def compute(num:Long , seq:Long) : Long = {
     
     var result=0:Long;
     var j=0:Long;
     while(j<seq)
     {
       result+=((num+j)*(num+j));
			j=j+1;
     }
     return result;
  
}
			
			
		
			
			
		
			//context.system.shutdown()
			
			
		
			
		}
    }
  class Boss(n:Long,seq:Long) extends Actor{
    
    var noOfElements = 1000L
   
    var n1=(n/noOfElements)
   
    
    var nrOfWorkers = 16
 
   
  
   
		val system = ActorSystem("1")
		//val Employee1 = system.actorOf(Props(new Employee),name = "Employee1")
		val router = system.actorOf(Props[Employee].withRouter(RoundRobinRouter(nrOfWorkers)),name="router")
		//val Employee2 = system.actorOf(Props(new Employee2),name = "Employee2")
		//val Employee3 = system.actorOf(Props(new Employee3),name = "Employee3")
		
		 
		val k=0L
         for (k<-0 to n1.toInt)
         {
           
           router ! Work((k*noOfElements),((k+1)*noOfElements),seq)
           
         }
    
  
		
		var i=0L;
	def receive= { 
		case output(out:Boolean) =>
		  i=i+1
	//	println("n1 is  "+n+"value of i is "+i)
		 if(i==n1)
		 {
		   
		   println("Good Bye)
		   context.system.shutdown()
		   
		 }
		  
	}
	

}