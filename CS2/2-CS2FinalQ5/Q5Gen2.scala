object Main extends App{
    def lowestCost(a:Array[Array[Int]],x:Int,y:Int):Int = {
        if(x < 0 || y < 0){
            return 1000
        }else if(x > a.length - 1 || y > a(x).length - 1){
            return 1000
        }else if(x == 0 && y == 0){
            return a(x)(y)
        }else{
            return a(x)(y) + Math.min(lowestCost(a,x-1,y),lowestCost(a,x,y-1))
        }
    }
    val b = Array(Array(1,2,3),Array(2,3,4),Array(8,7,8))
    println(lowestCost(b,1,2))
}