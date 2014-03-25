import akka.actor.Actor
import akka.actor.Props
import akka.actor.ActorSystem
import scala.util.Random
import akka.actor.ActorRef
import akka.actor._

	




    



object project2 {
  var master:ActorRef=null
  var debyg:Int=0
  case class MSG
  case class CONVERGE
  case class push(sid:Double,w:Double)
 val sys=  ActorSystem("MySystem")
  val s= Array
  def main(args: Array[String]) {
	  	val noOfActors= args(0).toInt
	  			val topology= args(1)
	  			val algorithm= args(2)
	  			
	  			
	  		val	prop1=Props(new Master(noOfActors,topology,algorithm))
	  		
	  
	  			 master= sys.actorOf(prop1,"Master")
	  			
}
  
  class Master(noOfActors:Int,topology:String,Algorithm:String) extends Actor
  {
	  var count:Int=0;
	  var count1:Int=0
    val masterSys=  ActorSystem("MasterSys")
    
    		
	  			val s:Array[ActorRef]= new Array[ActorRef](noOfActors)
	  			val i:Int=0
	  			for(i<-0 to noOfActors-1)
	  			{
	  			  
	  			  
	  			 var prop1= Props(new MyActor(i.toString,topology,noOfActors,s))
	  		   
	  			    s(i)=masterSys.actorOf(prop1,i.toString)
	  			   
	  			    
	  			    
	  			  
	  			}
    
	  	val random = Random.nextInt(9)
     
	  	
	  	var startTime=System.currentTimeMillis()
	  	println("************SETTING UP THE NETWORK***********")
	 if(Algorithm.equals("gossip")) 
	   
    s(random) ! MSG
    else
      s(random) ! push(random,1)
      
    
    
    def receive =  {
	  	  
      case MSG =>
      
        count=count+1
       // println("count is:"+count)
      if(count==noOfActors)
      {
        println("Terminating sysytem")
       // var endTime=System.currentTimeMillis()
       // println("Time required to terminate:"+(endTime-startTime))
        masterSys.shutdown
        sys.shutdown
        
        
      }
        
      case CONVERGE=>
        {
          count1= count1+1
       // print("  Position:"+count1)
          if(count1==noOfActors)
          {
             var endTime=System.currentTimeMillis()
        println("Time required to Converge:"+(endTime-startTime)+" milliseconds")
            println("**********PROGRAM IS CONVEREGED************")
              
          }
        }
      case push(sid:Double,w:Double)=>
        {
            var endTime=System.currentTimeMillis()
        println("Time required to Terminate:"+(endTime-startTime)+" milliseconds")
          
          println("Program is terminated:"+(sid/w))
         masterSys.shutdown
         sys.shutdown
        }
  
	  	}
  }
  
 
  

class MyActor(name:String,topology:String,noOfActors:Int,s:Array[ActorRef]) extends Actor {



var a:Array[Boolean]= new Array[Boolean](noOfActors) 
var x:Int=0
var flag:Int=0
var sOfWorker:Double=name.toInt
var wOfWorker:Double=1
var pratio:Double=0
var ppratio:Double=0
var rcount:Int=0
var sw:Array[Double]=new Array[Double](3) 
var count_diff:Int=0
def receive= {
   
  
  
case MSG =>
 
  x=x+1
 

  if(x==1)
  {
    println("Actor id:"+name+" heard the rumor")
    master ! CONVERGE
  }
  if(x==10)
  {
   flag=1
  
     master ! MSG

  }
if(topology.equals("full"))
    {
		var rad=full(name.toInt, noOfActors)
		
		
		
		if(flag==0)
		{
		s(rad) ! MSG
		for(z<-0 to 2)
		  rad=full(name.toInt, noOfActors)
		 s(rad) ! MSG
		
		
		}
		//context.system.scheduler.scheduleOnce(50 milliseconds)()
		{
		  //s(rad) ! MSG
		}
     
    }
    else if(topology.equals("2D"))
    {
      var rec= twoD(name.toInt, noOfActors)
     var random= Random.nextInt(rec.length)
      if(flag==0)
        
      {
        s(rec(random))! MSG  
        for(z<-0 to 2)
        {
		  random=Random.nextInt(rec.length)
		 s(rec(random)) ! MSG
        }
      }
    	
    }
    else if(topology.equals("line"))
    {
    var rec=Line(name.toInt, noOfActors)
  var random= Random.nextInt(rec.length)
      if(flag==0)
      {
        s(rec(random))! MSG  
        for(z<-0 to 2)
        {
		  random= Random.nextInt(rec.length)
		 s(rec(random)) ! MSG
        }
      }
      }
    else
    {
      //println("i am in ln")
      var rec= twoD(name.toInt, noOfActors)
        var random= Random.nextInt(rec.length)
        var random2=full(name.toInt, noOfActors) 
      if(flag==0)
      {
        for(z<-0 to 2)
        {
            var random= Random.nextInt(rec.length)
        var random2=full(name.toInt, noOfActors) 
        s(random) !MSG
        s(random2) ! MSG
        }
      }
    }

case push(sid:Double,w:Double)=>
   
	if(topology.equals("full"))
    {
	 
      var rad=Random.nextInt(noOfActors)
      if(rad==name.toInt && rad==noOfActors-1)
      {
        rad=rad-1
      }
      else if(rad==name.toInt && rad==0)
      {
        rad=rad+1
      }
      else if(rad==name.toInt)
      {
        rad=rad+1
      }
      sw=sw:+(sOfWorker/wOfWorker)
       if(rcount>5)
       {
         for(zz<-1 to 3)
         {
           if(math.abs(sw(rcount)-sw(rcount-zz))<1*math.exp(-10))
             count_diff += 1
             
         }
       }
               print(name.toInt+" "+sOfWorker+" "+wOfWorker+" "+sw(rcount)+"\n")
           if(count_diff<3)
           {
             sOfWorker=sOfWorker-(sOfWorker/2)
             wOfWorker=wOfWorker-(wOfWorker/2)
             s(rad)! push(sOfWorker,wOfWorker)
             sOfWorker=sOfWorker+sid
             wOfWorker=wOfWorker+w
             count_diff=0
             
             
             
           }
           else{
             
             master ! push(sOfWorker,wOfWorker)
           }
           rcount=rcount+1
         
           
        
      
       
    }
    else if(topology.equals("2D"))
    {
       var rec= twoD(name.toInt, noOfActors)
     var random= Random.nextInt(rec.length)
      sw=sw:+(sOfWorker/wOfWorker)
       if(rcount>5)
       {
         for(zz<-1 to 3)
         {
           if(math.abs(sw(rcount)-sw(rcount-zz))<1*math.exp(-10))
             count_diff += 1
             
         }
       }
               print(name.toInt+" "+sOfWorker+"    "   +sw(rcount)+"\n")
           if(count_diff<3)
           {
             sOfWorker=sOfWorker-(sOfWorker/2)
             wOfWorker=wOfWorker-(wOfWorker/2)
             s(rec(random))! push(sOfWorker,wOfWorker)
             sOfWorker=sOfWorker+sid
             wOfWorker=wOfWorker+w
             count_diff=0
             
             
             
           }
           else{
             
             master ! push(sOfWorker,wOfWorker)
           }
           rcount=rcount+1
         
     
     
      
    }
    else if(topology.equals("line"))
    {
         var rec=Line(name.toInt, noOfActors)
  var random= Random.nextInt(rec.length)
  sw=sw:+(sOfWorker/wOfWorker)
       if(rcount>5)
       {
         for(zz<-1 to 3)
         {
           if(math.abs(sw(rcount)-sw(rcount-zz))<1*math.exp(-10))
             count_diff += 1
             
         }
       }
               print(name.toInt+" "+sOfWorker+"    "   +sw(rcount)+"\n")
           if(count_diff<3)
           {
             sOfWorker=sOfWorker-(sOfWorker/2)
             wOfWorker=wOfWorker-(wOfWorker/2)
             s(rec(random))! push(sOfWorker,wOfWorker)
             sOfWorker=sOfWorker+sid
             wOfWorker=wOfWorker+w
             count_diff=0
             
             
             
           }
           else{
             
             master ! push(sOfWorker,wOfWorker)
           }
           rcount=rcount+1
         
     
      
    }
    else
    {
      var rec= twoD(name.toInt, noOfActors)
        var random= Random.nextInt(rec.length)
        var random2=full(name.toInt, noOfActors) 
        
        var ttt=Random.nextInt(2)
        if(ttt==0){
          ttt=random
        
        }
        else
        {
          ttt=random2
        }
           sw=sw:+(sOfWorker/wOfWorker)
       if(rcount>5)
       {
         for(zz<-1 to 3)
         {
           if(math.abs(sw(rcount)-sw(rcount-zz))<1*math.exp(-10))
             count_diff += 1
             
         }
       }
               print(name.toInt+" "+sOfWorker+" "+wOfWorker+" "+sw(rcount)+"\n")
           if(count_diff<3)
           {
             sOfWorker=sOfWorker-(sOfWorker/2)
             wOfWorker=wOfWorker-(wOfWorker/2)
             if(ttt==random)
             {
             s(rec(random))! push(sOfWorker,wOfWorker)
             }else
             {s(random2)! push(sOfWorker,wOfWorker)}
             
             sOfWorker=sOfWorker+sid
             wOfWorker=wOfWorker+w
             count_diff=0
             
             
             
           }
           else{
             
             master ! push(sOfWorker,wOfWorker)
           }
           rcount=rcount+1
   
     
    }



}

def twoDimp(index:Int,noOfActors:Int) : Array[Int]= {
  var rec1 = twoD(index, noOfActors)
  //var rec2= Random.nextInt(noOfActors-1)
  var arr:Array[Int]= new Array[Int](5)
 
  
  return arr
}

 def twoD(index:Int,noOfActors:Int):Array[Int]  = {
    var i:Int=0
    var sqroot = (Math.sqrt(noOfActors))
    val arr:Array[Int]= new Array[Int](noOfActors)
    val choose:Array[Int]= new Array[Int](4)
    if(index== 0)
    {
      choose(0)= index+1
      choose(1)=(index+sqroot.intValue)
       choose(2)= index+1
      choose(3)=(index+sqroot.intValue)
     
      //	val random = Random.nextInt(2)
      	
      	return choose
      	
    }
    else if(index==((noOfActors/sqroot)-1))
    {
       choose(0)= index-1
      choose(1)=(index+sqroot.intValue)
       choose(2)= index-1
      choose(3)=(index+sqroot.intValue)
      
      //val random = Random.nextInt(2)
      	
      	
      	return choose
    }
    else if(index==(noOfActors-sqroot))
    {
      choose(0)= index+1
      choose(1)=(index-sqroot.intValue)
       choose(2)= index+1
      choose(3)=(index-sqroot.intValue)
     	
      	return choose
    }
    else if(index==(noOfActors-1))
    {   
      choose(0)= index-1
      choose(1)=(index-sqroot.intValue)
      choose(3)=index-1
       choose(2)=(index-sqroot.intValue)
     
      	return choose
    }
    
    else if(index<((noOfActors/sqroot)-1))
    {
      choose(0)= index+1
      choose(1)=index-1
      choose(2)=(index+sqroot.intValue)
      choose(3)=index+1
      	//val random = Random.nextInt(3)
      	
      	return choose
      
    }
    else if(index<(noOfActors-1) && index>(noOfActors-sqroot))
    {
      choose(0)= index+1
      choose(1)=index-1
      choose(2)=(index-sqroot.intValue)
      choose(3)=index+1
      	val random = Random.nextInt(3)
      	//return choose(random)
      	return choose
    }
    else if((index %sqroot)==0)
    {
      choose(0)= index+1
      choose(1)=(index-sqroot.intValue)
      choose(2)=(index+sqroot.intValue)
      choose(3)=index+1
      	val random = Random.nextInt(3)
      	//return choose(random)
      	return choose
    }
    else if(((index+1)%sqroot)==0)
    {
      choose(0)= index-1
      choose(1)=(index-sqroot.intValue)
      choose(2)=(index+sqroot.intValue)
      choose(3)=index-1
      //	val random = Random.nextInt(3)
      	
      	return choose
    }
    else
    {
        choose(0)= index+1
      choose(1)=index-1
      choose(2)=(index-sqroot.intValue)
      choose(3)=(index+sqroot.intValue)
      
      	val random = Random.nextInt(4)
      	return choose
    
    }
    
     
 
}


}
 def Line(index:Int,noOfActors:Int) : Array[Int] = {
		  var choose:Array[Int]=new Array[Int](2)
    if(index==0)
    {
      choose(0)=index+1
      choose(1)=index+1
     return choose
      
    }
    else if(index==(noOfActors-1))
    {
      choose(0)=index-1
      choose(1)=index-1
      return choose
     
    }
    else
    {
       choose(0)=index+1
      choose(1)=index-1
      return choose
     
        
      
    
    }
    
  
}

def full(index:Int,noOfActors:Int) : Int = {
  var rad= Random.nextInt(noOfActors)
  if(rad==index && rad==noOfActors-1)
      {
        rad=rad-1
      }
      else if(rad==index && rad==0)
      {
        rad=rad+1
      }
      else if(rad==index) 
      {
        rad=rad+1
      }
  return rad
}



}