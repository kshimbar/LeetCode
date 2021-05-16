package cs2finalq5

object Main extends App{
    def lowestCost(a:Array[Array[Int]],x:Int,y:Int):Int = {
        var results = List[Int]()
        var count = 0
        def helper(xh:Int, yh:Int,pathh:Int):List[Int] = {
            if(x == 0 && y == 0 && count < 30){
                results = pathh :: results
                count = count + 1
                results
            }else if(count < 30){
                var pathv = pathh + a(x)(y)
                helper(xh + 1, yh,pathv)
                helper(xh, yh + 1,pathv)
                helper(xh - 1, yh,pathv)
                helper(xh, yh - 1,pathv)
            }
        }
        helper(x,y,0)
        val ret:Int = results.min()
        ret
    }
    val b = Array(Array(1,2,3,4,5),Array(2,3,4,5,6),Array(8,7,8,9,10))
    println(lowestCost(b,2,4))
}