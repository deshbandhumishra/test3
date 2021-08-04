object SingleValue extends App {
  println("============")

  def parseString[A](str: String,data_type:String):Any = {
    val regex=  "([0-9]{4})-([0-9]{2})-([0-9]{2}) ([0-9]{2}):([0-9]{2})|([0-9]+).([0-9]+)|([0-9]+)|([A-Za-z]+)".r
    var result:Any=0
    var result1=List()
    val str1 = regex.findAllIn(str)//.get.group(1)
    str1.foreach{ x=>
      data_type match {

        case "integer"=>  x.split(",").foreach{x=>

          println("====inter======="+x)
          result =x.toString.toInt
        }


        case "float" =>
          result=x.toFloat
          println("=========inter==="+result)

        case "list" => x.split(",").foreach{x=>
          println("====inter======="+x)
          result =x.toString
          result1.+(result.toString)//:::()result
        }

        case "string"=>  x.split(",").foreach{x=>
          println("====inter======="+x)
          result =x
        }

      }

    }
    result

  }
  //String
  //Braces
  val data0 = """""i","am","good"""""
  val data01 = """"I""""
  val dd0 = parseString(data01,"list")
  println("final==="+dd0)

  // Integer
  val data = """"10,9000,20,50,70""""
  val data1 = "10"
  val dd1:Int = parseString(data1,"integer").toString.toInt
  println("final==="+dd1.toInt)

  //Float
  val data2 = """"10.5,3.5,77.9""""
  val data21 = "10.8"
  val dd2:Float = parseString(data21,"float").toString.toFloat
  println("final==="+dd2)

  //Braces ([0-9]{4})-([0-9]{2})-([0-9]{2}) ([0-9]{2}):([0-9]{2})
  //2020-11-26 10:10
  val data3 = """"[10,8,9]""""
  val data31 = """[10.8,8.9,9.0]"""
  val data32 = """""["ds","ms","cs"]"""""
  val dd3 = parseString(data31,"list")
  println("final==="+dd3.toString.toFloat)

}