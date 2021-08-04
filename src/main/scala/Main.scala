object Main extends App {
  def parseString(str: String):Either[String, List[String]] = {

    val regex = "([0-9]{4})-([0-9]{2})-([0-9]{2}) ([0-9]{2}):([0-9]{2})|([0-9]+).([0-9]+)|([0-9]+)|([A-Za-z]+)".r


    val result = regex.findAllIn(str).toList.flatMap(x => x.split(","))

    if(str.contains("[")) Right(result)
    else {
      result.reduceLeft((x,y) => x +","+ y)
      Left(result.reduceLeft((x,y) => x +","+ y))
    }

  }

  //String
  val data01 = """"I, am""""
  val dd0 = parseString(data01).left.getOrElse("No value")
  println("=====String=="+dd0)

  // Integer
  val data = """10,20"""
  val data1 = "10"
  val dd1 = parseString(data1).left.get//.right.get
  println("==Integer Normal==="+dd1)
  println("=====Integer=="+dd1.toInt)

  //Float
  val data2 = """"10.5,3.5,77.9""""
  val data21 = "10.8"
  val dd2 = parseString(data21).left.get//.toList
  println("==Float Normal==="+dd2)
  println("=====Float=="+dd2.toFloat)

  //time stamp
  val data4 = """"2020-11-13 10:10""""
  val data41 = """"2020-11-26 10:10,2020-11-13 10:10""""
  val dd4 = parseString(data4).left.get
  println("=====time Stamp=="+dd4)


  //Braces
  val data3 = """"[10,8,9]""""
  val data31 = """[10.8,8.9,9.0]"""
  val data32 = """""["ds","ms","cs"]"""""
  val data33 = """[2020-11-26 10:10,2020-11-13 10:10]"""
  val dd3 = parseString(data31).right.get
  println("=====braces=="+dd3(1).toFloat)

}
