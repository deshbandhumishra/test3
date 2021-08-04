import scala.collection.mutable.ListBuffer
object MultipeValue extends App {

  def parseString[A](str: String,data_type:String):ListBuffer[String] = {

    val regex=  "([0-9]{4})-([0-9]{2})-([0-9]{2}) ([0-9]{2}):([0-9]{2})|([0-9]+).([0-9]+)|([0-9]+)|([A-Za-z]+)".r

    var result:Any=0

    var result1=new ListBuffer[String]()

    val str1 = regex.findAllIn(str)//.get.group(1)

    str1.foreach{ x=>
      data_type match {
        case "integer"=>  x.split(",").foreach{x=>
          result =x.toInt
          result1 +=result.toString
        }
        case "float" =>
          result=x.toFloat
          result1 +=result.toString

        case "list" => x.split(",").foreach{x=>
          result =x
          result1 +=result.toString

        }
        case "string"=>  x.split(",").foreach{x=>
          result =x
          result1 +=result.toString
        }
        case "timestamp"=>  x.split(",").foreach{x=>
          result =x
          result1 +=result.toString
        }

      }

    }
    result1

  }

  //String
  val data0 = """"["i","am","good"]"""
  val data01 = """"I, am""""
  val dd0 = parseString(data01,"string").toList
  println(dd0)
  dd0.foreach(x=> println("=====String=="+x))

  /*
    // Integer
    val data = """10,20"""
    val data1 = "10"
    val data12= """[10,900,20,50]"""
    val data13="""["10","900","20","50"]"""
    val dd1 = parseString(data,"integer").toList
    val result = dd1(0).toString
    println("==Integer Normal==="+result.toInt)
    println(dd1)
    dd1.foreach(x=> println("=====Integer=="+x.toInt))
  */
  /*
  //Float
  val data2 = """"10.5,3.5,77.9""""
  val data21 = "10.8"
  val dd2 = parseString(data2,"float").toList
  println("==Float Normal==="+dd2(0).toFloat)
  dd2.foreach(x=> println("=====Float=="+x.toFloat))
  //Braces
  val data3 = """"[10,8,9]""""
  val data31 = """[10.8,8.9,9.0]"""
  val data32 = """""["ds","ms","cs"]"""""
  val dd3 = parseString(data3,"list").toList
  dd3.foreach(x=> println("=====braces=="+x))
  //time stamp
  val data4 = """"2020-11-26 10:10,2020-11-13 10:10""""
  val data41 = """[2020-11-26 10:10,2020-11-13 10:10]"""
  val data42 = """""["ds","ms","cs"]"""""
  val dd4 = parseString(data41,"timestamp").toList
  dd4.foreach(x=> println("=====time Stamp=="+x))
*/
}