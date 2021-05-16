object Main extends App{
    def lowestCost(a:Array[Array[Int]],x:Int,y:Int):Int={
        var cost = Array.fill(a.length)(Array.fill(a.length)(100))
        cost(0)(0) = a(0)(0)
        var i = 1
        while(i <= x){
            cost(i)(0) = a(i)(0) + cost(i - 1)(0)
            i = i + 1
        }
        i = 1
        while(i <= y){
            cost(0)(i) = cost(0)(i - 1) + a(0)(i)
            i = i + 1
        }
        i = 1
        while(i <= x){
            var n = 1
            while(n <= y){
                cost(i)(n) = math.min(cost(i - 1)(n),cost(i)(n - 1)) + a(i)(n)
                n = n + 1
            }
            i = i  + 1
        }
        cost(x)(y)
    }
    val b = Array(Array(1,2,3),Array(2,3,4),Array(8,7,8))
    println(lowestCost(b,1,2))
}